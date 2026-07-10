# cert_funnel.R ----------------------------------------------------------------
# The Cert Funnel: classify every petition's path through the certiorari
# machine from its docket entries, compute funnel statistics, and render a
# public explainer page.
#
# Stage model (empirically grounded):
#   Docketed -> Considered at conference (>=1 distribution) -> Relisted (>=1
#   true relist) -> outcome {granted, gvr, denied, dismissed} or still pending.
#
# A "true relist" is a redistribution with NO intervening "Rescheduled." entry
# (the case was moved before its conference happened) and NO intervening
# "Response Requested" entry (redistribution after a called-for response
# arrives is mechanical, not a signal). Pooled OT17-22, counting naively would
# overstate relists by ~55%.
#
# "Response Requested" is deliberately NOT a funnel stage: in OT2019, all 339
# CFRs happened on/after first distribution. It is an engagement statistic.
#
# Outcome buckets:
#   granted   = full merits grant (incl. cert before judgment, split IFP-motion
#               grants, lowercase per-curiam grant wording, "treated as a
#               petition" grants, and appeal equivalents: probable jurisdiction
#               noted / jurisdiction postponed)
#   gvr       = summary disposition in the same order that grants: GVR
#               (grant-vacate-remand), summary reversal, Munsingwear vacatur,
#               standalone appeal GVRs, summary affirmance of an appeal
#   denied    = petition denied (incl. mandamus/habeas/prohibition and
#               lowercase denials embedded in longer orders)
#   dismissed = Rule 46 settlement, Rule 39.8, appeal dismissed for want of
#               jurisdiction / substantial federal question, unpaid-fee and
#               administrative closures ("Case considered closed", "Case
#               removed from Docket"), lack-of-quorum dispositions
#   pending   = no recognized disposition
# Earliest classified entry wins (a granted case later DIG'd stays "granted":
# the funnel measures what the Court agreed to hear).
#
# Wording families were verified against OT2017-OT2024 (~40,500 petitions) by
# an adversarial audit; the classifier reproduces known grant counts and the
# closed-term pending share is the audit's canary (should be ~0.1-0.5%).
#
# Works on both the historical archive tibbles (data-raw/ot_*.rds) and the
# live JSON-pipeline tibbles (scotus_dash_new.R): both carry events with
# `Date` + `Proceedings and Orders`.

suppressPackageStartupMessages({
  library(gt)
  library(gtExtras)
  library(tidyverse)
  library(htmltools)
  library(jsonlite)
})

# ---- entry patterns -----------------------------------------------------------

FUNNEL_PATTERNS <- list(
  dist = "^DISTRIBUTED for Conference",
  resched = "^Rescheduled",
  cfr = "^Response Requested",
  resp_filed = "in opposition filed",
  amicus = "^Brief (amicus|amici) curiae"
)

# Grant-entry forms (an entry that grants review). Verified families:
GRANT_FORMS <- c(
  "^Petition GRANTED",
  "^Petition for a writ of certiorari( before judgment)? GRANTED",
  "^Petition for certiorari GRANTED",
  "^Motion to proceed in forma pauperis and petition for a writ of certiorari GRANTED",
  # split-motion form: "Motion to proceed in forma pauperis GRANTED. Petition
  # for a writ of certiorari GRANTED limited to ..."
  "^Motion (of petitioner )?(for leave )?to proceed in forma pauperis (is )?GRANTED[.,]?\\s+(and )?[Tt]he [Pp]etition[^.]{0,200}GRANTED",
  "^Motion (of petitioner )?(for leave )?to proceed in forma pauperis (is )?GRANTED[.,]?\\s+[Pp]etition[^.]{0,200}GRANTED",
  # lowercase per-curiam prose ("The petition for a writ of certiorari is
  # granted[, the judgment ... is vacated ...]")
  "(?i)^the (motion[^.]{0,160} and the )?petition for a writ of certiorari( before judgment)? (is|are) granted",
  # applications treated as cert petitions and granted; plain "." span because
  # sentence-scoping with [^.] is defeated by docket citations ("No. 21-588")
  "(?i)treated as a petition for a writ of certiorari.{0,250}granted",
  # appeal equivalents of a grant (often mid-entry inside a stay order, so
  # unanchored)
  "(?i)probable jurisdiction (is )?noted",
  "(?i)consideration of the question of jurisdiction is postponed"
)

# Within a grant entry, a summary disposition (GVR / summary reversal /
# Munsingwear vacatur) also disposes of the case in the same order.
SUMMARY_RX <- paste0(
  "(?i)judgments?[^.]{0,120}(vacated|reversed)",
  "|(?i)order[^.]{0,100}is vacated",
  "|(?i)vacated as moot"
)

DENY_FORMS <- c(
  "^Petition DENIED",
  "^Petition for writ of (mandamus|habeas corpus|prohibition)[^.]{0,80}DENIED",
  # lowercase denials embedded in longer orders (capital cases, sealed filings);
  # sentence-anchored so "consideration of the petition ... is denied" (a
  # motion ruling) cannot match
  "(?i)(^|\\.\\s+)(the )?petition for a writ of certiorari( before judgment)? is denied"
)

DISMISS_FORMS <- c(
  "(?i)^petition dismissed",
  # Rule 39.8: IFP motion denied and petition dismissed outright
  "^The motion for leave to proceed in forma pauperis is denied, and the petition",
  # appeals declined
  "(?i)^(the )?appeal is dismissed",
  "(?i)appeal dismissed for want",
  "(?i)dismissed for want of (jurisdiction|a substantial federal question)",
  # administrative closures (dominantly: conditional IFP denial under Rule
  # 38(a), docketing fee never paid)
  "^Case considered closed",
  "^Case removed from Docket",
  "(?i)lacks? a quorum"
)

rx_any <- function(txt, forms) {
  out <- rep(FALSE, length(txt))
  for (f in forms) out <- out | str_detect(txt, f)
  out
}

# Case type from the docket number (authoritative across all eras).
funnel_case_type <- function(dkt) {
  n <- suppressWarnings(as.integer(str_extract(dkt, "\\d+$")))
  case_when(
    str_detect(dkt, "A\\d+$") ~ "app",
    !is.na(n) & n >= 5001 ~ "ifp",
    TRUE ~ "paid"
  )
}

# ---- per-case classification --------------------------------------------------

# Classify one case's events. Returns a one-row tibble.
classify_petition_events <- function(events) {
  empty <- tibble(
    n_dist = 0L, n_relists = 0L,
    first_dist = as.Date(NA), dist_dates = list(as.Date(character())),
    relist_dates = list(as.Date(character())),
    outcome = "pending", outcome_date = as.Date(NA),
    has_cfr = FALSE, has_resp = FALSE, has_amicus = FALSE
  )
  if (!is.data.frame(events) || !("Proceedings and Orders" %in% names(events)) ||
      nrow(events) == 0) {
    return(empty)
  }
  txt <- events[["Proceedings and Orders"]]
  txt[is.na(txt)] <- ""
  edate <- suppressWarnings(lubridate::mdy(events$Date))

  # Drop exact duplicate rows (same date + text): duplicated DISTRIBUTED rows
  # occur in the source data and would inflate relist counts.
  keep <- !duplicated(paste(edate, txt))
  txt <- txt[keep]; edate <- edate[keep]

  # Entries are scraped in docket order; sort by date with a STABLE sort so
  # same-date entries keep their docket order. This ordering is load-bearing
  # for same-date DISTRIBUTED/Rescheduled pairs (audited: the docket order is
  # semantically correct in both observed same-date patterns).
  ord <- order(edate)
  txt <- txt[ord]; edate <- edate[ord]

  is_dist <- str_detect(txt, FUNNEL_PATTERNS$dist)
  is_resched <- str_detect(txt, FUNNEL_PATTERNS$resched)
  is_cfr <- str_detect(txt, FUNNEL_PATTERNS$cfr)

  # True relists: for each distribution after the first, it is a relist unless
  # a Rescheduled or Response Requested entry intervened since the previous
  # distribution.
  di <- which(is_dist)
  relist_flags <- logical(0)
  if (length(di) > 1) {
    relist_flags <- vapply(seq_len(length(di) - 1), function(i) {
      lo <- di[i] + 1L; hi <- di[i + 1L] - 1L
      if (lo > hi) return(TRUE) # adjacent distributions: nothing intervened
      !any(is_resched[lo:hi] | is_cfr[lo:hi])
    }, logical(1))
  }

  # Outcome: earliest entry matching a terminal form; within an entry,
  # grant/summary takes precedence over deny/dismiss.
  is_grant <- rx_any(txt, GRANT_FORMS)
  is_summary <- str_detect(txt, SUMMARY_RX)
  term_kind <- case_when(
    is_grant & is_summary ~ "gvr",
    is_grant ~ "granted",
    # standalone appeal GVRs and summary affirmances (in argued cases the
    # grant entry is earlier, so earliest-wins keeps those "granted")
    str_detect(txt, "^Judgments? VACATED and cases? REMANDED") ~ "gvr",
    str_detect(txt, "^Adjudged to be AFFIRMED") ~ "gvr",
    rx_any(txt, DENY_FORMS) ~ "denied",
    rx_any(txt, DISMISS_FORMS) ~ "dismissed",
    TRUE ~ NA_character_
  )
  outcome <- "pending"; outcome_date <- as.Date(NA)
  hit <- which(!is.na(term_kind))
  if (length(hit) > 0) {
    first <- hit[1]
    outcome <- term_kind[first]
    outcome_date <- edate[first]
  }

  tibble(
    n_dist = length(di),
    n_relists = sum(relist_flags),
    first_dist = if (length(di) > 0) edate[di[1]] else as.Date(NA),
    dist_dates = list(edate[di]),
    relist_dates = list(if (length(di) > 1) edate[di[-1]][relist_flags] else as.Date(character())),
    outcome = outcome,
    outcome_date = outcome_date,
    has_cfr = any(is_cfr),
    has_resp = any(str_detect(txt, FUNNEL_PATTERNS$resp_filed)),
    has_amicus = any(str_detect(txt, FUNNEL_PATTERNS$amicus))
  )
}

# Classify every petition (paid + IFP; applications excluded) in a case tibble.
classify_petitions <- function(cases) {
  stopifnot(all(c("dkt", "events") %in% names(cases)))
  cases |>
    mutate(type = funnel_case_type(dkt)) |>
    filter(type != "app") |>
    mutate(cls = map(events, classify_petition_events)) |>
    select(dkt, type, date, any_of("caption"), cls) |>
    unnest(cls) |>
    mutate(term = str_extract(dkt, "^\\d{2}"))
}

# ---- funnel statistics ---------------------------------------------------------

# Aggregate classified petitions into funnel stats, optionally as of a date
# (events after `as_of` are masked, so mid-term views stay honest).
funnel_stats <- function(cls, as_of = NULL) {
  if (!is.null(as_of)) {
    as_of <- as.Date(as_of)
    cls <- cls |>
      filter(!is.na(date), date <= as_of) |>
      mutate(
        n_dist = map_int(dist_dates, \(d) sum(!is.na(d) & d <= as_of)),
        n_relists = map_int(relist_dates, \(d) sum(!is.na(d) & d <= as_of)),
        outcome = if_else(!is.na(outcome_date) & outcome_date <= as_of,
                          outcome, "pending"),
        first_dist = as.Date(if_else(!is.na(first_dist) & first_dist <= as_of,
                                     first_dist, as.Date(NA)))
      )
  }

  stage_block <- function(d) {
    tibble(
      docketed = nrow(d),
      considered = sum(d$n_dist > 0),
      relisted = sum(d$n_relists > 0),
      granted_or_gvr = sum(d$outcome %in% c("granted", "gvr")),
      granted = sum(d$outcome == "granted"),
      gvr = sum(d$outcome == "gvr"),
      denied = sum(d$outcome == "denied"),
      dismissed = sum(d$outcome == "dismissed"),
      pending = sum(d$outcome == "pending"),
      # denied at the case's first and only conference (exactly one
      # distribution, then denial)
      died_first_conf = sum(d$outcome == "denied" & d$n_dist == 1),
      has_cfr = sum(d$has_cfr),
      has_resp = sum(d$has_resp),
      has_amicus = sum(d$has_amicus)
    )
  }

  relist_table <- cls |>
    filter(outcome != "pending", n_dist > 0) |>
    mutate(bucket = if_else(n_relists >= 5, "5+", as.character(n_relists))) |>
    count(bucket, outcome) |>
    pivot_wider(names_from = outcome, values_from = n, values_fill = 0L)
  for (col in c("granted", "gvr", "denied", "dismissed")) {
    if (!col %in% names(relist_table)) relist_table[[col]] <- 0L
  }
  relist_table <- relist_table |>
    mutate(
      n = granted + gvr + denied + dismissed,
      bucket_ord = if_else(bucket == "5+", 5L, suppressWarnings(as.integer(bucket)))
    ) |>
    arrange(bucket_ord) |>
    select(bucket, n, granted, gvr, denied, dismissed)

  list(
    total = stage_block(cls),
    by_type = cls |> group_by(type) |> group_modify(\(d, g) stage_block(d)) |> ungroup(),
    relist_table = relist_table,
    as_of = if (is.null(as_of)) NA_character_ else as.character(as_of),
    n_cases = nrow(cls)
  )
}

# ---- baselines from the historical archive -------------------------------------

# Compute per-term funnel stats for archive terms. Terms whose pending share
# exceeds `max_pending` are flagged incomplete and excluded from the pooled
# relist-odds table (deterministic rule, disclosed on the page).
compute_funnel_baselines <- function(paths, max_pending = 0.015) {
  per_term <- imap(paths, function(path, term) {
    cases <- readRDS(path)
    cls <- classify_petitions(cases)
    st <- funnel_stats(cls)
    st$term <- term
    st$pending_share <- st$total$pending / st$total$docketed
    st$complete <- st$pending_share <= max_pending
    st$cls <- cls
    st
  })

  pooled_cls <- per_term |>
    keep(\(s) s$complete) |>
    map(\(s) s$cls) |>
    bind_rows()
  pooled <- funnel_stats(pooled_cls)

  list(
    per_term = map(per_term, \(s) s[setdiff(names(s), "cls")]),
    pooled = pooled,
    pooled_terms = per_term |> keep(\(s) s$complete) |> map_chr(\(s) s$term),
    excluded_terms = per_term |> discard(\(s) s$complete) |> map_chr(\(s) s$term)
  )
}

# ---- rendering -----------------------------------------------------------------

fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE)
pct <- function(num, den, digits = 1) {
  if (is.na(den) || den == 0) return("—")
  paste0(format(round(100 * num / den, digits), nsmall = digits, trim = TRUE), "%")
}
one_in <- function(num, den) {
  if (is.na(num) || num == 0 || is.na(den)) return("—")
  paste0("1 in ", fmt_n(round(den / num)))
}

# A proportional horizontal-bar funnel as inline SVG. Count + note render
# BELOW each bar (never clipped by the viewBox, whatever the bar width).
funnel_svg <- function(rows, total, width = 680, bar_h = 34, gap = 34,
                       fill = "#0b3d91") {
  if (is.na(total) || total <= 0 || nrow(rows) == 0) {
    return(tags$p(class = "note", "No petitions yet."))
  }
  n_rows <- nrow(rows)
  height <- n_rows * (bar_h + gap) + 4
  label_w <- max(150, round(width * 0.26))
  usable <- width - label_w - 10
  bars <- map_chr(seq_len(n_rows), function(i) {
    w <- max(2, round(usable * rows$n[i] / total))
    y <- (i - 1) * (bar_h + gap)
    opacity <- 1 - 0.13 * (i - 1)
    sprintf(paste0(
      '<text x="%d" y="%d" text-anchor="end" class="fl">%s</text>',
      '<rect x="%d" y="%d" width="%d" height="%d" rx="4" fill="%s" fill-opacity="%.2f"/>',
      '<text x="%d" y="%d" class="fn">%s</text>',
      '<text x="%d" y="%d" class="fp">%s</text>'
    ),
      label_w - 8, y + bar_h / 2 + 5, htmlEscape(rows$label[i]),
      label_w, y, w, bar_h, fill, opacity,
      label_w, y + bar_h + 15, fmt_n(rows$n[i]),
      label_w + 12 + nchar(fmt_n(rows$n[i])) * 9, y + bar_h + 15,
      htmlEscape(rows$note[i])
    )
  })
  HTML(sprintf(
    '<svg viewBox="0 0 %d %d" width="100%%" style="max-width:%dpx" role="img" aria-label="Certiorari funnel">%s</svg>',
    width, height, width, paste(bars, collapse = "")
  ))
}

funnel_rows <- function(tot, live = FALSE) {
  tibble(
    label = c("Docketed", "Considered at conference", "Relisted at least once",
              "Granted or GVR'd", "Granted outright"),
    n = c(tot$docketed, tot$considered, tot$relisted, tot$granted_or_gvr, tot$granted),
    note = c(
      "every petition filed",
      paste0(pct(tot$considered, tot$docketed), " reached the Justices' private conference"),
      paste0(pct(tot$relisted, tot$docketed), " were relisted at least once"),
      paste0(pct(tot$granted_or_gvr, tot$docketed), " got any form of yes"),
      if (live) {
        paste0(pct(tot$granted, tot$docketed), " granted so far, headed for briefing and argument")
      } else {
        paste0(pct(tot$granted, tot$docketed), " went on to full briefing, argument, and decision")
      }
    )
  )
}

relist_gt <- function(rt) {
  rt |>
    filter(n >= 25) |> # minimum-N row suppression
    mutate(
      relists = if_else(bucket == "0", "Never relisted",
                        paste0("Relisted ", bucket, "×")),
      p_granted = 100 * granted / n,
      p_gvr = 100 * gvr / n,
      p_denied = 100 * denied / n,
      p_dismissed = 100 * dismissed / n
    ) |>
    select(relists, n, p_granted, p_gvr, p_denied, p_dismissed) |>
    gt() |>
    fmt_number(columns = starts_with("p_"), decimals = 1, pattern = "{x}%") |>
    fmt_number(columns = n, sep_mark = ",", decimals = 0) |>
    cols_label(
      relists = "", n = "Petitions",
      p_granted = "Granted", p_gvr = "GVR'd",
      p_denied = "Denied", p_dismissed = "Dismissed"
    ) |>
    data_color(columns = p_granted, palette = c("white", "#B2DF8A"),
               domain = c(0, 60)) |>
    gt_theme_nytimes() |>
    tab_options(table.font.size = px(15))
}

term_label <- function(term) paste0("OT", 2000 + as.integer(term))

# Render the funnel page. `live` is a named list of funnel_stats() for terms to
# show live (e.g. list(`26` = ..., `25` = ...)); `baselines` is
# compute_funnel_baselines() output (without cls); `live_cls` optionally maps
# term -> classified tibble for the relisted-now drill-down.
render_funnel_page <- function(live, baselines, out_dir,
                               live_cls = list(),
                               data_dates = list()) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  css <- HTML("
    body{font-family:'Source Sans Pro',system-ui,sans-serif;max-width:860px;
      margin:2rem auto;padding:0 1rem;color:#1a1a1a;line-height:1.5}
    h1{font-weight:600;border-bottom:2px solid #1a1a1a;padding-bottom:.4rem}
    h2{font-weight:600;margin-top:2.2rem}
    .headline{font-size:1.35rem;background:#f5f7fb;border-left:5px solid #0b3d91;
      padding:1rem 1.2rem;margin:1.4rem 0}
    .headline b{color:#0b3d91}
    .defs{background:#fbf8f2;border:1px solid #e8e0d0;border-radius:6px;
      padding:.8rem 1.2rem;font-size:.95rem}
    .defs dt{font-weight:700;float:left;clear:left;margin-right:.5rem}
    .defs dd{margin:0 0 .35rem 7.5rem}
    .fl{font:600 14px 'Source Sans Pro',sans-serif;fill:#1a1a1a}
    .fn{font:700 14px 'Source Sans Pro',sans-serif;fill:#1a1a1a}
    .fp{font:13px 'Source Sans Pro',sans-serif;fill:#666}
    .cols{display:flex;gap:2rem;flex-wrap:wrap}
    .cols>div{flex:1;min-width:300px}
    .note{color:#555;font-size:.92rem}
    .methods{font-size:.88rem;color:#444;border-top:1px solid #ddd;
      margin-top:3rem;padding-top:1rem}
    .methods li{margin-bottom:.4rem}
    a{color:#0b3d91}
    .exitbar{color:#666;font-size:.92rem;margin:.3rem 0 1.2rem}
  ")

  pooled <- baselines$pooled$total
  pooled_terms <- baselines$pooled_terms
  pooled_span <- paste0(term_label(min(pooled_terms)), "–", term_label(max(pooled_terms)))

  # -- live-term sections (newest first) --
  live_sections <- imap(rev(live), function(st, term) {
    tot <- st$total
    asof <- data_dates[[term]] %||% st$as_of
    relisted_now <- NULL
    if (!is.null(live_cls[[term]])) {
      rn_all <- live_cls[[term]] |>
        filter(outcome == "pending", n_relists >= 1) |>
        arrange(desc(n_relists))
      n_rn <- nrow(rn_all)
      rn <- head(rn_all, 25)
      if (n_rn > 0 && "caption" %in% names(rn)) {
        label <- if (n_rn == 1) {
          "1 pending petition is currently relisted — one to watch"
        } else if (n_rn <= 25) {
          sprintf("%d pending petitions are currently relisted — the ones to watch", n_rn)
        } else {
          sprintf("%d pending petitions are currently relisted — showing the 25 most-relisted", n_rn)
        }
        relisted_now <- tags$details(
          tags$summary(label),
          rn |>
            transmute(
              Case = str_trunc(str_squish(caption), 90),
              `Docket` = sprintf(
                '<a href="https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/%s.html">%s</a>',
                dkt, dkt),
              Relists = n_relists
            ) |>
            gt() |> fmt_markdown(columns = Docket) |>
            gt_theme_nytimes() |> as_raw_html() |> HTML()
        )
      }
    }
    tagList(
      h2(sprintf("%s — the term in progress", term_label(term))),
      p(class = "note", sprintf(
        "Data as of %s. Petitions from this term are still being docketed and decided; %s are still waiting on an outcome.",
        asof %||% "the last weekly refresh", fmt_n(tot$pending))),
      funnel_svg(funnel_rows(tot, live = TRUE), tot$docketed),
      div(class = "exitbar", sprintf(
        "So far: %s denied · %s GVR'd · %s dismissed · %s still pending (%s of the term)",
        fmt_n(tot$denied), fmt_n(tot$gvr), fmt_n(tot$dismissed),
        fmt_n(tot$pending), pct(tot$pending, tot$docketed))),
      if (!is.null(relisted_now)) relisted_now
    )
  })

  # -- baseline funnel + paid/IFP split --
  ptot <- baselines$pooled$by_type |> filter(type == "paid")
  itot <- baselines$pooled$by_type |> filter(type == "ifp")

  doc <- tags$html(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$title("The Cert Funnel — how Supreme Court petitions live and die"),
      tags$style(css)
    ),
    tags$body(
      h1("The Cert Funnel"),
      p("Nearly everything the Supreme Court is asked to do, it declines to do — silently. ",
        "Each Term roughly five thousand parties petition the Court to hear their case; ",
        "the Justices agree to hear fewer than a hundred. This page tracks every petition ",
        "through the machine: from docketing, to the Justices' private conference, through ",
        "relists, to the quiet one-line order that ends almost all of them."),
      tags$dl(class = "defs",
        tags$dt("Docketed"), tags$dd("A petition arrives and gets a case number. “OT2025” is the Term that opened in October 2025."),
        tags$dt("Conference"), tags$dd("The Justices' private meeting (most Fridays in season) where they vote on which cases to hear. No outsiders attend; no reasons are given."),
        tags$dt("Relist"), tags$dd("The Justices looked at a petition at conference and, instead of granting or denying, rolled it to the next conference for another look."),
        tags$dt("GVR"), tags$dd("Granted, Vacated, Remanded: a one-line order sending a case back to the lower court for a fresh look — usually in light of a new decision — without briefing or argument."),
        tags$dt("IFP"), tags$dd(HTML("<em>In forma pauperis</em> — filed by someone who cannot afford the $300 filing fee, most often a prisoner without a lawyer."))
      ),
      div(class = "headline", HTML(sprintf(
        "Over %s, the Court received <b>%s</b> petitions and granted <b>%s</b> — <b>%s</b>.",
        pooled_span, fmt_n(pooled$docketed), fmt_n(pooled$granted),
        one_in(pooled$granted, pooled$docketed)))),
      live_sections,
      h2(sprintf("The complete picture: %s", pooled_span)),
      p(class = "note", sprintf(
        "%s petitions across %d completed Terms. This is what normally happens.",
        fmt_n(pooled$docketed), length(pooled_terms))),
      funnel_svg(funnel_rows(pooled), pooled$docketed),
      div(class = "exitbar", sprintf(
        "Exits: %s denied (%s of everything filed) · %s GVR'd · %s dismissed or otherwise closed. %s%% of all denials came at the case's first and only conference.",
        fmt_n(pooled$denied), pct(pooled$denied, pooled$docketed),
        fmt_n(pooled$gvr), fmt_n(pooled$dismissed),
        round(100 * pooled$died_first_conf / max(pooled$denied, 1)))),
      h2("Two very different doors"),
      p("Petitioners who can pay the $300 filing fee and printing costs file on the ",
        HTML("&ldquo;paid&rdquo;"), " docket. Prisoners and others who cannot afford it file ",
        HTML("<em>in forma pauperis</em>"), " (IFP). The machine treats the two piles very differently:"),
      div(class = "cols",
        div(h3(sprintf("Paid (%s petitions)", fmt_n(ptot$docketed))),
            funnel_svg(funnel_rows(ptot), ptot$docketed, width = 460, bar_h = 30, gap = 32),
            p(class = "note", sprintf("Granted: %s — %s", pct(ptot$granted, ptot$docketed),
                                      one_in(ptot$granted, ptot$docketed)))),
        div(h3(sprintf("In forma pauperis (%s petitions)", fmt_n(itot$docketed))),
            funnel_svg(funnel_rows(itot), itot$docketed, width = 460, bar_h = 30, gap = 32,
                       fill = "#c05a00"),
            p(class = "note", sprintf("Granted: %s — %s", pct(itot$granted, itot$docketed),
                                      one_in(itot$granted, itot$docketed))))
      ),
      p(class = "note", "These are descriptive base rates, not causes: the two dockets ",
        "carry very different kinds of cases, not just different filing fees."),
      h2("What a relist is worth"),
      p("Court-watchers treat the relist as the strongest public signal at this stage: ",
        "it means at least someone in the building gave the petition a second look. ",
        sprintf(paste0("Across %s, here is what happened to decided petitions that ",
                       "reached at least one conference, grouped by how many times ",
                       "they were relisted:"), pooled_span)),
      HTML(as_raw_html(relist_gt(baselines$pooled$relist_table))),
      p(class = "note", "Petitions relisted once or twice were granted far more often ",
        "than never-relisted ones. Petitions relisted many times usually met a ",
        "different fate: most were being held for another case already under review, ",
        "and usually ended in a GVR or a denial once that case came down. ",
        "History, not prophecy: none of this predicts any particular case."),
      h2("How rarely anyone even responds"),
      p(sprintf(
        paste0("The other side often does not bother: across %s, a brief in ",
               "opposition was filed in only %s of cases (%s). The Court itself asked ",
               "for a response — a signal someone took interest — in %s (%s). And ",
               "outside groups filed friend-of-the-court briefs supporting or opposing ",
               "review in just %s (%s)."),
        pooled_span,
        pct(pooled$has_resp, pooled$docketed), fmt_n(pooled$has_resp),
        pct(pooled$has_cfr, pooled$docketed), fmt_n(pooled$has_cfr),
        pct(pooled$has_amicus, pooled$docketed), fmt_n(pooled$has_amicus))),
      h2("Follow the machine"),
      tags$ul(
        tags$li(a(href = "../dashboards/", "The daily docket"),
                " — every new petition and application, as it arrives."),
        tags$li(a(href = "../conferences/", "Conference reports"),
                " — what the Justices consider at each private conference, sorted by relists.")
      ),
      div(class = "methods",
        tags$b("Methods & honest limits"),
        tags$ul(
          tags$li("Built entirely from the public docket entries on supremecourt.gov. ",
                  "Docket data shows process, not reasons: the Court explains almost ",
                  "nothing at this stage, and neither do we."),
          tags$li("A petition belongs to the Term of its docket number; outcomes are ",
                  "attributed to that Term even when they arrive in the next one. ",
                  "A granted case that is later dismissed or dropped still counts as granted."),
          tags$li(HTML(paste0(
            "A <em>relist</em> is a redistribution with no intervening ",
            "&ldquo;Rescheduled&rdquo; entry and no intervening call for a response ",
            "(those redistributions are mechanical). Counting naively would ",
            "overstate relists by more than half."))),
          tags$li(HTML(paste0(
            "The &ldquo;GVR'd&rdquo; bucket is summary dispositions: overwhelmingly ",
            "grant-vacate-remand orders, plus a handful of summary reversals and ",
            "summary affirmances. &ldquo;Dismissed&rdquo; includes settlements, ",
            "procedural dismissals, and administrative closures (most often an ",
            "unpaid filing fee)."))),
          tags$li("Emergency applications (the ", HTML("&ldquo;A&rdquo;"),
                  " docket) are excluded here; they are a different machine."),
          tags$li(sprintf(
            paste0("Baseline Terms pooled: %s. Terms are pooled only when fewer than ",
                   "1.5%% of petitions lack a recognized outcome%s."),
            paste(map_chr(pooled_terms, term_label), collapse = ", "),
            if (length(baselines$excluded_terms) > 0) {
              paste0(" (excluded as incomplete: ",
                     paste(map_chr(baselines$excluded_terms, term_label), collapse = ", "), ")")
            } else "")),
          tags$li("Rows with fewer than 25 petitions are suppressed in the relist table; ",
                  "small percentages inside surviving rows can reflect a handful of ",
                  "cases. All rates are descriptive history, not predictions about any ",
                  "pending case."),
          tags$li(a(href = "https://github.com/baldrige/ceRt", "Code and data"), ".")
        )
      )
    )
  )
  # Write directly (save_html would wrap the document in a second <html>).
  writeLines(paste0("<!DOCTYPE html>\n", as.character(doc)),
             file.path(out_dir, "index.html"), useBytes = TRUE)
  invisible(file.path(out_dir, "index.html"))
}
