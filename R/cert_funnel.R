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

# A proportional funnel as animated HTML bars: each stage a labelled band whose
# bar width is proportional to its count (last/granted bar in the accent).
# `bar_color` is the CSS colour for the non-final bars.
funnel_svg <- function(rows, total, bar_color = "var(--ink)", ...) {
  if (is.na(total) || total <= 0 || nrow(rows) == 0) {
    return(tags$p(class = "note", "No petitions yet."))
  }
  n_rows <- nrow(rows)
  items <- map_chr(seq_len(n_rows), function(i) {
    w <- max(0.7, 100 * rows$n[i] / total)
    is_last <- i == n_rows
    color <- if (is_last) "var(--oxblood)" else bar_color
    sprintf(paste0(
      '<div class="frow" style="--i:%d">',
      '<div class="flabel">%s</div>',
      '<div class="ftrack">',
      '<div class="frail"><span class="fbar%s" style="width:%.2f%%;background:%s"></span></div>',
      '<span class="fcount">%s</span></div>',
      '<div class="fnote">%s</div></div>'
    ),
      i - 1L, htmlEscape(rows$label[i]),
      if (is_last) " fbar-key" else "", w, color,
      fmt_n(rows$n[i]), htmlEscape(rows$note[i])
    )
  })
  HTML(sprintf('<div class="funnel" role="img" aria-label="Certiorari funnel">%s</div>',
               paste(items, collapse = "")))
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

# Hand-rolled relist table with an oxblood heat on the Granted column so the
# eye lands on where a relist actually pays off. Rows with fewer than 25
# decided petitions are suppressed (small-N noise).
relist_html <- function(rt) {
  d <- rt |>
    filter(n >= 25) |>
    mutate(
      relists = if_else(bucket == "0", "Never relisted",
                        paste0("Relisted ", bucket, "×")),
      p_granted = 100 * granted / n,
      p_gvr = 100 * gvr / n,
      p_denied = 100 * denied / n,
      p_dismissed = 100 * dismissed / n
    )
  # Heat scaled to the strongest granted rate in the table so contrast is real.
  gmax <- max(d$p_granted, 1)
  cell_pct <- function(v) paste0(format(round(v, 1), nsmall = 1, trim = TRUE), "%")
  rows <- pmap_chr(d, function(relists, n, p_granted, p_gvr, p_denied, p_dismissed, ...) {
    a <- max(0, min(0.9, p_granted / gmax))
    on_dark <- a > 0.55
    heat <- sprintf('style="background:rgba(138,43,43,%.3f)%s"',
                    a, if (on_dark) ";color:var(--paper);font-weight:600" else "")
    sprintf(paste0(
      '<tr><th scope="row">%s</th><td class="num">%s</td>',
      '<td class="num heat" %s>%s</td><td class="num">%s</td>',
      '<td class="num">%s</td><td class="num">%s</td></tr>'),
      htmlEscape(relists), fmt_n(n),
      heat, cell_pct(p_granted), cell_pct(p_gvr),
      cell_pct(p_denied), cell_pct(p_dismissed))
  })
  HTML(sprintf(paste0(
    '<table class="rtable"><thead><tr>',
    '<th class="lead"></th><th class="num">Petitions</th>',
    '<th class="num">Granted</th><th class="num">GVR’d</th>',
    '<th class="num">Denied</th><th class="num">Dismissed</th>',
    '</tr></thead><tbody>%s</tbody></table>'),
    paste(rows, collapse = "")))
}

# The "ones to watch" drill-down: pending petitions currently relisted, most
# relists first. Hand-rolled to match the page (no gt theme).
watch_table <- function(rn) {
  rows <- pmap_chr(rn, function(caption, dkt, n_relists, ...) {
    sprintf(paste0(
      '<tr><td class="wcase">%s</td>',
      '<td class="num"><a href="https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/%s.html">%s</a></td>',
      '<td class="num">%s</td></tr>'),
      htmlEscape(str_trunc(str_squish(caption), 90)),
      htmlEscape(dkt), htmlEscape(dkt), n_relists)
  })
  HTML(sprintf(paste0(
    '<table class="rtable wtable"><thead><tr>',
    '<th class="lead">Case</th><th class="num">Docket</th>',
    '<th class="num">Relists</th></tr></thead><tbody>%s</tbody></table>'),
    paste(rows, collapse = "")))
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
    :root{
      --paper:#f3ecdd;--paper-2:#ede4d0;--panel:#f7f1e4;
      --ink:#23262d;--ink-soft:#5f5847;--faint:#8a8271;
      --oxblood:#8a2b2b;--sienna:#b5651d;--gold:#a8862c;
      --rule:#d8cdb4;--rail:#e4dac2;
    }
    *{box-sizing:border-box}
    html{-webkit-text-size-adjust:100%}
    body{
      font-family:'Newsreader',Georgia,serif;font-optical-sizing:auto;
      font-size:19px;line-height:1.62;color:var(--ink);
      background:var(--paper);margin:0;
      font-feature-settings:'onum' 1;
    }
    /* faint paper grain */
    body::before{content:'';position:fixed;inset:0;z-index:-1;pointer-events:none;
      opacity:.5;mix-blend-mode:multiply;
      background-image:url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='140' height='140'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.9' numOctaves='2' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)' opacity='0.045'/%3E%3C/svg%3E\");}
    .wrap{max-width:44rem;margin:0 auto;padding:3.2rem 1.4rem 5rem}
    p{margin:0 0 1.05rem}
    a{color:var(--oxblood);text-decoration:none;
      border-bottom:1px solid rgba(138,43,43,.32);transition:border-color .15s}
    a:hover{border-color:var(--oxblood)}
    /* -- masthead -- */
    .kicker{font:600 .78rem/1 'Newsreader';letter-spacing:.22em;
      text-transform:uppercase;color:var(--oxblood);margin:0 0 1rem}
    h1{font-family:'Fraunces',Georgia,serif;font-weight:600;
      font-size:clamp(2.9rem,8vw,4.6rem);line-height:.98;letter-spacing:-.015em;
      margin:0 0 1rem;color:var(--ink);font-optical-sizing:auto}
    h1 em{font-style:italic;font-weight:500;color:var(--oxblood)}
    .dek{font-size:1.28rem;line-height:1.5;color:var(--ink-soft);
      font-style:italic;max-width:34rem;margin:0 0 1.6rem}
    .brule{border:0;height:0;border-top:2px solid var(--ink);
      margin:1.6rem 0 .4rem;position:relative}
    .brule::after{content:'';position:absolute;left:0;top:4px;width:100%;
      border-top:1px solid var(--rule)}
    /* -- section headers -- */
    .over{font:600 .74rem/1 'Newsreader';letter-spacing:.2em;
      text-transform:uppercase;color:var(--sienna);
      margin:3.2rem 0 .55rem;display:flex;align-items:center;gap:.7rem}
    .over::after{content:'';flex:1;border-top:1px solid var(--rule)}
    h2{font-family:'Fraunces',Georgia,serif;font-weight:600;
      font-size:1.85rem;line-height:1.08;letter-spacing:-.01em;
      margin:0 0 .9rem;color:var(--ink)}
    h3{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.18rem;
      margin:0 0 .3rem;color:var(--ink)}
    /* -- hero stat callout -- */
    .hero{margin:2.2rem 0 2.6rem;padding:1.7rem 1.8rem;background:var(--panel);
      border:1px solid var(--rule);border-left:4px solid var(--oxblood);
      box-shadow:0 1px 0 rgba(0,0,0,.03),0 14px 30px -22px rgba(60,40,20,.5)}
    .hero .big{font-family:'Fraunces',Georgia,serif;font-weight:600;
      font-size:clamp(2.6rem,9vw,3.9rem);line-height:1;color:var(--oxblood);
      letter-spacing:-.02em;font-feature-settings:'lnum' 1;margin-bottom:.35rem}
    .hero .sub{font-size:1.05rem;color:var(--ink-soft)}
    .hero .sub b{color:var(--ink);font-weight:600}
    /* -- definitions -- */
    .defs{margin:1.6rem 0;padding:0;list-style:none;border-top:1px solid var(--rule)}
    .defs .row{display:grid;grid-template-columns:8.5rem 1fr;gap:.2rem 1.1rem;
      padding:.7rem 0;border-bottom:1px solid var(--rule)}
    .defs dt{font-family:'Fraunces',Georgia,serif;font-weight:600;
      font-size:1.02rem;color:var(--ink)}
    .defs dd{margin:0;font-size:.98rem;color:var(--ink-soft);line-height:1.45}
    @media(max-width:33rem){.defs .row{grid-template-columns:1fr}}
    /* -- funnel bars -- */
    .funnel{margin:1.8rem 0 .6rem}
    .frow{margin:0 0 1.2rem;opacity:0;transform:translateY(7px);
      animation:frise .6s cubic-bezier(.2,.7,.2,1) forwards;
      animation-delay:calc(var(--i)*130ms)}
    .flabel{font:600 .72rem/1 'Newsreader';letter-spacing:.13em;
      text-transform:uppercase;color:var(--ink-soft);margin-bottom:.42rem}
    .ftrack{display:grid;grid-template-columns:1fr auto;align-items:center;gap:.85rem}
    .frail{position:relative;height:30px;background:var(--rail);border-radius:2px;
      overflow:hidden}
    .fbar{display:block;height:100%;border-radius:2px;transform:scaleX(0);
      transform-origin:left center;
      animation:fgrow .9s cubic-bezier(.22,.61,.36,1) forwards;
      animation-delay:calc(var(--i)*130ms + 90ms)}
    .fbar-key{box-shadow:inset 0 0 0 999px rgba(0,0,0,.04)}
    .fcount{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.18rem;
      color:var(--ink);font-feature-settings:'tnum' 1,'lnum' 1;
      min-width:3.4rem;text-align:right;opacity:0;
      animation:ffade .5s ease forwards;animation-delay:calc(var(--i)*130ms + .5s)}
    .fnote{font-size:.92rem;color:var(--faint);margin-top:.35rem;font-style:italic}
    @keyframes frise{to{opacity:1;transform:none}}
    @keyframes fgrow{to{transform:scaleX(1)}}
    @keyframes ffade{to{opacity:1}}
    @media(prefers-reduced-motion:reduce){
      .frow,.fbar,.fcount{animation:none;opacity:1;transform:none}}
    .exitbar{color:var(--ink-soft);font-size:.96rem;font-style:italic;
      margin:.2rem 0 1.4rem;padding-left:.9rem;border-left:2px solid var(--rule)}
    /* -- two doors -- */
    .cols{display:grid;grid-template-columns:1fr 1fr;gap:2.2rem;margin:1.4rem 0}
    @media(max-width:38rem){.cols{grid-template-columns:1fr}}
    .cols .note{margin-top:.2rem}
    /* -- tables -- */
    .rtable{width:100%;border-collapse:collapse;margin:1.3rem 0 .5rem;
      font-size:.98rem}
    .rtable th,.rtable td{padding:.55rem .7rem;text-align:left;
      border-bottom:1px solid var(--rule)}
    .rtable thead th{font:600 .7rem/1.2 'Newsreader';letter-spacing:.09em;
      text-transform:uppercase;color:var(--faint);border-bottom:1.5px solid var(--ink);
      vertical-align:bottom}
    .rtable .num{text-align:right;font-feature-settings:'tnum' 1,'lnum' 1;
      font-variant-numeric:tabular-nums}
    .rtable tbody th[scope=row]{font-weight:600;color:var(--ink)}
    .rtable .wcase{font-size:.95rem;line-height:1.35}
    .rtable tbody tr:hover{background:rgba(138,43,43,.04)}
    .rtable .heat{border-radius:2px}
    details{margin:1rem 0 1.6rem}
    summary{cursor:pointer;font-family:'Fraunces',Georgia,serif;font-weight:600;
      font-size:1.05rem;color:var(--oxblood);list-style:none;
      padding:.55rem .8rem;background:var(--panel);border:1px solid var(--rule);
      border-radius:3px}
    summary::-webkit-details-marker{display:none}
    summary::before{content:'▸ ';color:var(--sienna)}
    details[open] summary::before{content:'▾ '}
    .note{color:var(--ink-soft);font-size:.96rem;line-height:1.5}
    /* -- follow / methods -- */
    .follow{list-style:none;padding:0;margin:1rem 0}
    .follow li{padding:.75rem 0;border-bottom:1px solid var(--rule)}
    .follow a{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.1rem}
    .methods{font-size:.9rem;color:var(--ink-soft);line-height:1.5;
      border-top:2px solid var(--ink);margin-top:3.4rem;padding-top:1.1rem}
    .methods b{font-family:'Fraunces',Georgia,serif;font-size:1rem;color:var(--ink)}
    .methods ul{padding-left:1.1rem;margin:.7rem 0 0}
    .methods li{margin-bottom:.55rem}
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
          watch_table(rn)
        )
      }
    }
    tagList(
      div(class = "over", "This term, so far"),
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

  font_url <- paste0(
    "https://fonts.googleapis.com/css2?",
    "family=Fraunces:ital,opsz,wght@0,9..144,500;0,9..144,600;1,9..144,500&",
    "family=Newsreader:ital,opsz,wght@0,6..72,400;0,6..72,500;0,6..72,600;1,6..72,400&",
    "display=swap")
  # htmltools treats <head> as a singleton collection that as.character() drops
  # (it is only emitted by save_html/renderDocument). Build the head as raw HTML
  # and use tags only for the body, which serializes reliably.
  head_html <- paste0(
    "<head>",
    "<script async src='/analytics.js'></script>",
    '<meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    '<link rel="icon" href="/favicon.svg" type="image/svg+xml"><link rel="alternate icon" href="/favicon.ico" sizes="any">',
    "<title>The Cert Funnel &mdash; how Supreme Court petitions live and die</title>",
    '<link rel="preconnect" href="https://fonts.googleapis.com">',
    '<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>',
    '<link rel="stylesheet" href="', font_url, '">',
    "<style>", as.character(css), "</style>",
    "</head>")

  body_tag <- tags$body(
      tags$main(class = "wrap",
      p(class = "kicker", "A field guide to the shadow docket’s daylight cousin"),
      h1(HTML("The <em>Cert</em> Funnel")),
      p(class = "dek", "Nearly everything the Supreme Court is asked to do, it declines ",
        "to do — silently. This page follows every petition through the machine."),
      tags$hr(class = "brule"),
      p("Each Term roughly five thousand parties petition the Court to hear their case; ",
        "the Justices agree to hear fewer than a hundred. What follows traces that ",
        "attrition end to end: from docketing, to the Justices' private conference, through ",
        "relists, to the quiet one-line order that ends almost all of them."),
      div(class = "hero",
        div(class = "big", one_in(pooled$granted, pooled$docketed)),
        div(class = "sub", HTML(sprintf(
          "Over %s the Court received <b>%s</b> petitions and granted <b>%s</b>.",
          pooled_span, fmt_n(pooled$docketed), fmt_n(pooled$granted))))),
      tags$dl(class = "defs",
        div(class = "row", tags$dt("Docketed"), tags$dd("A petition arrives and gets a case number. “OT2025” is the Term that opened in October 2025.")),
        div(class = "row", tags$dt("Conference"), tags$dd("The Justices' private meeting (most Fridays in season) where they vote on which cases to hear. No outsiders attend; no reasons are given.")),
        div(class = "row", tags$dt("Relist"), tags$dd("The Justices looked at a petition at conference and, instead of granting or denying, rolled it to the next conference for another look.")),
        div(class = "row", tags$dt("GVR"), tags$dd("Granted, Vacated, Remanded: a one-line order sending a case back to the lower court for a fresh look — usually in light of a new decision — without briefing or argument.")),
        div(class = "row", tags$dt("IFP"), tags$dd(HTML("<em>In forma pauperis</em> — filed by someone who cannot afford the $300 filing fee, most often a prisoner without a lawyer.")))
      ),
      live_sections,
      div(class = "over", "The base rate"),
      h2(sprintf("The complete picture: %s", pooled_span)),
      p(class = "note", sprintf(
        "%s petitions across %d completed Terms. This is what normally happens.",
        fmt_n(pooled$docketed), length(pooled_terms))),
      funnel_svg(funnel_rows(pooled), pooled$docketed),
      div(class = "exitbar", HTML(sprintf(
        "Exits: %s denied (%s of everything filed) · %s GVR'd · %s dismissed or otherwise closed. %s%% of all denials came at the case's first and only conference.",
        fmt_n(pooled$denied), pct(pooled$denied, pooled$docketed),
        fmt_n(pooled$gvr), fmt_n(pooled$dismissed),
        round(100 * pooled$died_first_conf / max(pooled$denied, 1))))),
      div(class = "over", "Two doors"),
      h2("Two very different doors"),
      p("Petitioners who can pay the $300 filing fee and printing costs file on the ",
        HTML("&ldquo;paid&rdquo;"), " docket. Prisoners and others who cannot afford it file ",
        HTML("<em>in forma pauperis</em>"), " (IFP). The machine treats the two piles very differently:"),
      div(class = "cols",
        div(h3(sprintf("Paid (%s petitions)", fmt_n(ptot$docketed))),
            funnel_svg(funnel_rows(ptot), ptot$docketed),
            p(class = "note", sprintf("Granted: %s — %s", pct(ptot$granted, ptot$docketed),
                                      one_in(ptot$granted, ptot$docketed)))),
        div(h3(sprintf("In forma pauperis (%s petitions)", fmt_n(itot$docketed))),
            funnel_svg(funnel_rows(itot), itot$docketed, bar_color = "var(--sienna)"),
            p(class = "note", sprintf("Granted: %s — %s", pct(itot$granted, itot$docketed),
                                      one_in(itot$granted, itot$docketed))))
      ),
      p(class = "note", "These are descriptive base rates, not causes: the two dockets ",
        "carry very different kinds of cases, not just different filing fees."),
      div(class = "over", "The relist signal"),
      h2("What a relist is worth"),
      p("Court-watchers treat the relist as the strongest public signal at this stage: ",
        "it means at least someone in the building gave the petition a second look. ",
        sprintf(paste0("Across %s, here is what happened to decided petitions that ",
                       "reached at least one conference, grouped by how many times ",
                       "they were relisted:"), pooled_span)),
      relist_html(baselines$pooled$relist_table),
      p(class = "note", "Petitions relisted once or twice were granted far more often ",
        "than never-relisted ones. Petitions relisted many times usually met a ",
        "different fate: most were being held for another case already under review, ",
        "and usually ended in a GVR or a denial once that case came down. ",
        "History, not prophecy: none of this predicts any particular case."),
      div(class = "over", "Silence"),
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
      div(class = "over", "Keep watching"),
      h2("Follow the machine"),
      tags$ul(class = "follow",
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
      ) # /main.wrap
    ) # /body
  html <- paste0("<!DOCTYPE html>\n<html lang=\"en\">\n",
                 head_html, "\n", as.character(body_tag), "\n</html>\n")
  writeLines(html, file.path(out_dir, "index.html"), useBytes = TRUE)
  invisible(file.path(out_dir, "index.html"))
}
