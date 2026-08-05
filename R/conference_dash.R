# conference_dash.R -----------------------------------------------------------
# Supreme Court conference reports. SCOTUS records each case's distribution for a
# Conference as a docket entry: "DISTRIBUTED for Conference of M/D/YYYY." These
# functions scan a case tibble (as produced by get_scotus_update() in
# scotus_dash_new.R, or the historical data in data-raw/ot_*.rds) for those
# notations and render one dashboard per conference date.
#
# Only the `events` proceedings text is needed, so this works on both the live
# JSON cases and the historical scrape, and requires no network access.

suppressPackageStartupMessages({
  library(gt)
  library(gtExtras)
  library(tidyverse)
  library(htmltools)
})

# Shared helpers: page-presentation (gtsave_titled, styled_index_page) and the
# interactive editorial theme (scr_interactive/scr_write_page, case_documents),
# sourced relative to this file's location so it works from the repo root or not.
local({
  here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA)
  find <- function(f) {
    if (!is.na(here) && file.exists(file.path(here, f))) file.path(here, f)
    else if (file.exists(file.path("R", f))) file.path("R", f)
    else f
  }
  sys.source(find("page_style.R"), envir = globalenv())
  sys.source(find("interactive_theme.R"), envir = globalenv())
})

CONF_PATTERN <- regex(
  "DISTRIBUTED for Conference of (\\d{1,2}/\\d{1,2}/\\d{4})",
  ignore_case = TRUE
)

# Case type derived from the docket number itself, which is authoritative even
# when the source data's `type` column is missing (older terms in scotus.rds
# predate that column): NNA# = application, NN-5001+ = IFP, otherwise paid.
derive_case_type <- function(dkt) {
  n <- suppressWarnings(as.integer(str_extract(dkt, "\\d+$")))
  case_when(
    str_detect(dkt, "A\\d+$") ~ "app",
    !is.na(n) & n >= 5001 ~ "ifp",
    TRUE ~ "paid"
  )
}

# URL of the petition-for-certiorari PDF from a case's events. Handles both the
# historical scrape (Document_*/links_* columns) and the JSON build_events()
# layout (docs_*/links_*), where description column N pairs with link column N.
find_petition_url <- function(events) {
  if (!is.data.frame(events) || nrow(events) == 0) return(NA_character_)
  desc_cols <- str_subset(names(events), "^(docs_|Document_)")
  link_cols <- str_subset(names(events), "^links_")
  if (length(desc_cols) == 0 || length(link_cols) == 0) return(NA_character_)
  for (i in seq_len(nrow(events))) {
    descs <- unlist(events[i, desc_cols], use.names = FALSE)
    links <- unlist(events[i, link_cols], use.names = FALSE)
    hit <- which(!is.na(descs) & str_detect(descs, regex("^Petition", ignore_case = TRUE)))
    hit <- hit[hit <= length(links)]
    if (length(hit) > 0 && !is.na(links[hit[1]])) return(links[hit[1]])
  }
  NA_character_
}

# The conference dates one case's events were distributed for (sorted, unique).
# Thin alias. The canonical implementation lives in cert_funnel.R so that
# cert_model.R and docket_page.R can reach it without sourcing this file -- three
# of the four render entry points never did, and silently lost the GVR line.
case_conference_dates <- function(events) {
  if (exists("conference_dates_from_events")) return(conference_dates_from_events(events))
  if (!is.data.frame(events) || !("Proceedings and Orders" %in% names(events))) {
    return(as.Date(character()))
  }
  m <- str_match(events[["Proceedings and Orders"]], CONF_PATTERN)[, 2]
  sort(unique(lubridate::mdy(m[!is.na(m)])))
}

# Filter the case database to distributions and expand to one row per
# (case, conference date). Adds:
#   conf_date        the Conference the case was distributed for
#   n_distributions  total conferences this case was distributed for
#   distribution_no  1 = first conference, 2+ = a relist
conference_distributions <- function(cases) {
  stopifnot("events" %in% names(cases))
  cases |>
    mutate(
      .cid = row_number(),
      conf_date = map(events, case_conference_dates)
    ) |>
    filter(lengths(conf_date) > 0) |>
    mutate(
      n_distributions = lengths(conf_date),
      petition_url = map_chr(events, find_petition_url),
      # Disposition per docket. Needed for two things the renderer got wrong:
      # already-decided petitions were reappearing on later conference pages with
      # a live forecast (798 of 17,540 rows, on 186 of 233 published pages), and
      # hold_signal()'s companion tier needs the set of dockets granted BEFORE a
      # given conference -- it was always empty because no case tibble in the
      # pipeline carries an `outcome` column.
      .cls = map(events, function(e)
        tryCatch(classify_petition_events(e), error = function(err) NULL)),
      outcome = map_chr(.cls, ~ if (is.null(.x)) NA_character_ else .x$outcome[[1]]),
      outcome_date = as.Date(map_dbl(.cls,
        ~ if (is.null(.x)) NA_real_ else as.numeric(.x$outcome_date[[1]])),
        origin = "1970-01-01")
    ) |>
    select(-.cls) |>
    unnest_longer(conf_date) |>
    group_by(.cid) |>
    mutate(distribution_no = row_number()) |>
    ungroup() |>
    mutate(type = derive_case_type(dkt)) |> # authoritative; source `type` may be NA
    select(-.cid)
}

# Petitioner's counsel of record as "Name\nFirm", from the JSON parties
# structure (build_parties() in scotus_dash_new.R). Returns NA for the
# historical scraper structure (no firm / counsel-of-record; not re-rendered by
# CI) so the column can be dropped when empty.
counsel_cell <- function(parties) {
  if (!is.data.frame(parties) || nrow(parties) == 0) return(NA_character_)
  if (!all(c("attys", "firm", "counsel_of_record", "type") %in% names(parties))) {
    return(NA_character_)
  }
  pet <- parties |> filter(str_detect(type, "Petitioner|Applicant|Appellant"))
  if (nrow(pet) == 0) return(NA_character_)
  cor <- pet |> filter(counsel_of_record %in% TRUE)
  row <- if (nrow(cor) > 0) cor[1, ] else pet[1, ]
  nm <- row$attys
  fm <- row$firm
  if (is.na(nm) || nm == "") return(NA_character_)
  if (!is.na(fm) && fm != "") str_c(nm, "  \n", fm) else nm
}

# A "Forecast" markdown cell per row of a conference tibble `d`, scored as of the
# conference date with the enhanced grant model + the companion GVR-risk model
# (both in `models`, from load_cert_models()). Paid petitions only; everything
# else -- non-paid, absent models, a scoring error, or the scoring functions not
# being sourced -- yields an em dash, so this never breaks a render.
conference_forecast <- function(d, conf_date, models) {
  out <- rep("—", nrow(d))
  if (is.null(models) || is.null(models$enhanced) || is.null(models$gvr) ||
      !exists("score_disposition")) return(out)
  getcol <- function(nm, def) if (nm %in% names(d)) d[[nm]] else rep(def, nrow(d))
  ld <- getcol("lower_date", as.Date(NA)); rel <- getcol("related", NA_character_)
  dt <- getcol("date", as.Date(NA))
  # Dockets granted before this conference enable the companion "Vide" hold tier.
  gd <- if ("outcome" %in% names(d))
    d$dkt[d$outcome %in% "granted"] else character()
  for (i in seq_len(nrow(d))) {
    if (!identical(d$type[i], "paid")) next
    s <- tryCatch(score_disposition(
      models$enhanced, models$gvr, d$caption[i], d$lower[i], d$parties[[i]],
      dt[i], ld[i], rel[i], events = d$events[[i]], as_of = conf_date,
      granted_dockets = gd, counsel_index = models$counsel_index),
      error = function(e) NULL)
    if (is.null(s) || is.na(s$p_grant)) next
    out[i] <- if (isTRUE(s$held)) {
      sprintf("**%d%%** grant  \n`held` · %d%% GVR", round(100*s$p_grant), round(100*s$p_gvr))
    } else {
      sprintf("**%d%%**", round(100*s$p_grant))
    }
  }
  out
}

# Render one conference's dashboard from a conference_distributions() tibble.
# `qp_map` (optional) is a named vector: raw docket -> <details> QP HTML.
# Counsel and QP columns are included only when the data provides them (so
# historical pages stay clean). Returns the output path (invisibly), or NULL.
conference_dash <- function(dist, conf_date,
                            out_dir = path.expand("~/public_html/conferences"),
                            qp_map = NULL, models = NULL, pnav = "") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  conf_date <- as.Date(conf_date)

  d <- dist |> filter(conf_date == !!conf_date)
  # A distribution dated after the petition's own disposition is a rehearing
  # redistribution, not a live cert petition -- rendering it put a grant forecast
  # on an already-decided case.
  if ("outcome_date" %in% names(d))
    d <- d |> filter(is.na(outcome_date) | conf_date <= outcome_date)
  if (nrow(d) == 0) return(invisible(NULL))

  # Numeric grant / GVR-risk forecasts, scored as of this conference with the
  # enhanced + companion GVR models. Paid petitions only; everything else --
  # non-paid, absent models, or a scoring error -- stays NA (an em dash), so a
  # render never breaks. Kept numeric so the columns sort by value. Defensive
  # column access (gc) keeps the historical scrape, which lacks some fields,
  # from erroring.
  gc <- function(nm, def) if (nm %in% names(d)) d[[nm]] else rep(def, nrow(d))
  dt <- gc("date", as.Date(NA)); ld <- gc("lower_date", as.Date(NA))
  rel <- gc("related", NA_character_)
  has_parties <- "parties" %in% names(d)
  # Two published quantities, each from the model that wins that target on a
  # like-for-like rolling-origin comparison (see score_conference()):
  #   Grant  = P(granted AT THIS conference)  -- competing-risks hazard
  #   Ever   = P(granted eventually)          -- at-risk binary
  # A single number could only be one of these, and the old column was neither:
  # it read 7.8% where 2.9% of petitions are granted at the conference in front
  # of them, and understated the eventual figure 2.8x at a first conference.
  p_grant <- rep(NA_real_, nrow(d)); p_gvr <- rep(NA_real_, nrow(d))
  p_ever  <- rep(NA_real_, nrow(d))
  if (!is.null(models) && exists("score_conference") &&
      (!is.null(models$conference) || !is.null(models$enhanced))) {
    # Dockets already GRANTED as of this conference, across the whole database --
    # not just this page's rows, and not using a final outcome that would be
    # future information. This is what makes hold_signal()'s companion-linkage
    # tier reachable; it never fired before because the set was always empty.
    gd <- if (all(c("outcome", "outcome_date") %in% names(dist)))
      unique(dist$dkt[dist$outcome %in% "granted" &
                      !is.na(dist$outcome_date) & dist$outcome_date < conf_date])
      else character()
    for (i in seq_len(nrow(d))) {
      if (!identical(d$type[i], "paid")) next
      par <- if (has_parties) d$parties[[i]] else NULL
      s <- tryCatch(score_conference(
        models, d$caption[i], d$lower[i], par, dt[i], ld[i], rel[i],
        events = d$events[[i]], as_of = conf_date,
        conf_idx = d$distribution_no[i], granted_dockets = gd),
        error = function(e) NULL)
      if (!is.null(s)) {
        p_grant[i] <- s$p_grant_now; p_gvr[i] <- s$p_gvr_now
        p_ever[i]  <- s$p_grant_ever
      }
    }
  }

  # Shade a probability the way gt's data_color() did when Granted here / ever /
  # GVR were three separate columns. Merging them into one cell means gt can no
  # longer colour by value -- a cell holds markup, not a number -- so the ramp is
  # applied inline instead. Keeping it matters: the heat map is how you find the
  # live petitions by eye, and it is doing more work now that the merged column
  # cannot be sorted numerically.
  fc_shade <- function(p, hi, cols) {
    out <- rep(GRANT_NA, length(p))                     # na_color, as before
    ok <- !is.na(p)
    if (any(ok)) {
      m <- grDevices::colorRamp(cols)(pmin(pmax(p[ok] / hi, 0), 1))
      out[ok] <- grDevices::rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255)
    }
    out
  }
  # Significant figures, not a fixed precision. A GVR probability is almost
  # always under half a percent, so a flat "%.0f%%" rendered 42 of 44 cells as
  # "GVR 0%" -- a false zero, asserting a probability of nothing where the value
  # was 0.2%.
  #
  # The first fix used "<0.1%" below a threshold, and that rendered as a blank:
  # "GVR" followed by nothing. The payload carries a correct single-escaped
  # &lt;, and by the HTML spec a "<" before a digit should emit as a literal
  # character -- so I could not pin down where it is lost between gt's markdown,
  # reactable's JSON and the innerHTML the browser finally parses. It buys
  # nothing over showing the number, so it is gone rather than debugged.
  #
  # Two decimals appear only below 1%, where the reader's question is just
  # "is this essentially zero" and 0.04% answers it better than a threshold.
  # The floor is 0.01% (p = 1e-4), the smallest value two decimals can state
  # without printing "0.00%" -- which would be the same false zero in a longer
  # coat. Below that, "0%" is honest: 0.003% is zero to any reader, whereas the
  # 0.2% the original bug rounded away sits in a range that means something.
  pct <- function(p) ifelse(
    is.na(p), "—",
    ifelse(p >= 0.10,   sprintf("%.0f%%", 100 * p),
    ifelse(p >= 0.01,   sprintf("%.1f%%", 100 * p),
    ifelse(p >= 0.0001, sprintf("%.2f%%", 100 * p), "0%"))))

  # "Granted ever" leads and carries the shading: it is the question a reader
  # scanning a conference actually has -- will this petition be granted at all --
  # and the one that survives past today. "Granted here" and GVR follow, muted:
  # the two outcomes of this conference in particular.
  # here and GVR go on their own lines, not separated by a middot on one.
  # "here 8.8% &middot; GVR 4.0%" is wider than the 120px column, and .fc-sub
  # sets white-space:nowrap, so it overflowed and was clipped at the column edge
  # -- the row rendered as "here 8.8% &middot; GVR" with the number sliced off.
  # It was never the value: the markup was right all along and the cell was too
  # narrow to show it. A <br> is what nowrap is for.
  fc_cell <- function(g, e, v) {
    sub <- paste0(ifelse(is.na(g), "", paste0("here ", pct(g))),
                  ifelse(!is.na(g) & !is.na(v), "<br>", ""),
                  ifelse(is.na(v), "", paste0("GVR ", pct(v))))
    ifelse(is.na(g) & is.na(e) & is.na(v), "—",
      paste0("<span class='fc-here' style='background:",
             # Domain 0-1, as gt's data_color() used for Ever. Granted-here had
             # 0-0.5; keeping each number on the scale it was published with
             # matters more than the extra contrast a tighter domain would give.
             fc_shade(e, 1, GRANT_RAMP), "'>",
             pct(e), "</span>",
             ifelse(nzchar(sub), paste0("<span class='fc-sub'>", sub, "</span>"), "")))
  }

  # One editorial row per distributed case. Relists = prior distributions.
  qp_get <- function(dk) if (is.null(qp_map)) NA_character_ else unname(qp_map[dk])
  tbl <- tibble(
    Type = factor(d$type, levels = c("paid", "ifp", "app"),
                  labels = c("Paid", "IFP", "Application")),
    # The docket number now sits under the caption instead of holding a column
    # of its own. It stays searchable -- the search box matches rendered cell
    # text -- and the case page it links to is the same one the caption links to.
    Case = sprintf(
      "<a href='../cases/%s.html' target='_blank'>%s</a><span class='cdk'>No. %s</span>",
      d$dkt,
      strip_caption_roles(d$caption),
      d$dkt),
    Relists = d$distribution_no - 1L,
    Forecast = fc_cell(p_grant, p_ever, p_gvr),
    # Sort key only: `Forecast` is markup, so arranging on it would sort "4%"
    # after "12%". Dropped before the table is built.
    # Sorts by the number the cell now leads with.
    .fc_sort = p_ever,
    # No str_trunc(28). A truncated court name loses exactly the part that
    # distinguishes it -- "Supreme Court of the State of New York, App..." --
    # and the ellipsis is not recoverable by hovering, sorting or searching,
    # because the truncation happens before the value ever reaches the page.
    # The column wraps instead; two lines cost less than a lost name.
    Court = str_replace(coalesce(d$lower, "—"),
              "^United States Court of Appeals for the (.+?Circuit)$", "\\1"),
    # Petitioner's counsel of record + firm, as on the daily dashboards. Only the
    # JSON pipeline carries a parties structure; the historical scrape does not,
    # so this is "—" (and the column is dropped) on the pre-JSON archive.
    Counsel = if ("parties" %in% names(d)) map_chr(d$parties, petitioner_counsel_html)
              else rep("—", nrow(d)),
    Documents = map_chr(d$events, function(e)
                  case_documents(e, c("Petition", "Appendix", "BIO", "Reply"))),
    QP = { q <- map_chr(d$dkt, qp_get); ifelse(is.na(q) | q == "", "—", q) }
  ) |> arrange(desc(Relists), desc(.fc_sort)) |> select(-.fc_sort)

  # Drop the forecast column on conferences with no paid petitions, and any
  # column that is entirely empty -- e.g. QP and Counsel on the pre-JSON
  # historical archive, which has neither source (matches the old renderer, which
  # omitted the column rather than showing a wall of em dashes).
  #
  # One column to drop instead of three, and the per-value drops are gone with
  # it: when only one of the two models scores a conference, fc_cell() simply
  # omits that number from the cell. That also retires the failure mode noted
  # below -- naming a dropped column in cols_label() aborted a whole render.
  has_grant <- any(!is.na(p_grant)) || any(!is.na(p_ever))
  if (!has_grant) tbl <- select(tbl, -Forecast)
  for (col in c("Counsel", "QP", "Documents")) {
    if (col %in% names(tbl) && all(tbl[[col]] == "—")) tbl <- select(tbl, -all_of(col))
  }
  has_qp <- "QP" %in% names(tbl)

  left_cols <- match(intersect(c("Case", "Court", "Counsel", "Documents", "QP"), names(tbl)),
                     names(tbl))

  t <- tbl |>
    gt() |>
    fmt_markdown(columns = any_of(c("Case", "Counsel", "Documents", "QP", "Forecast"))) |>
    data_color(columns = Type, method = "factor",
      palette = TYPE_CHIPS) |>
    cols_align("center", columns = everything()) |>
    # Type holds "Paid" / "IFP" / "Application" -- the longest is 11 characters,
    # and reactable was giving the column far more than that. Court gets a real
    # allowance instead, since it now wraps rather than truncating.
    cols_width(Case ~ px(230), Type ~ px(76), Court ~ px(160))
  if (has_qp) t <- t |> cols_label(QP = "Questions Presented") |> cols_width(QP ~ px(190))
  # Formatting, shading and the em dash for a non-paid row all happen inside
  # fc_cell() now, so nothing is needed here beyond the header and a width. The
  # header is short on purpose: "Granted here" was ~12 characters of uppercase
  # tracked caps holding open a column whose data is three, and across three such
  # columns the labels, not the numbers, were setting the table's width.
  if (has_grant) t <- t |> cols_label(Forecast = "Grant forecast") |>
    cols_width(Forecast ~ px(120))

  footer <- if (has_grant) paste0(
    "<em>Grant forecast</em> leads with the model's estimate that this petition is ",
    "granted at <em>any</em> conference, now or after further relists, and is shaded ",
    "by that value. Beneath it, <em>here</em> is the estimate that it is granted at ",
    "<em>this</em> conference and <em>GVR</em> the companion estimate of a grant, ",
    "vacate &amp; remand at this one. The leading number and <em>here</em> differ most ",
    "for a petition at its first conference, which is usually relisted or denied ",
    "rather than granted on the spot. Paid petitions only. ",
    "Estimates, not predictions about any case."
  ) else ""

  n_case <- nrow(tbl)
  # No longer "sort by Granted ever": the three forecast columns are one cell of
  # markup now, so it sorts as text rather than by value and pointing readers at
  # it would be pointing them at nonsense. Relists alone does what that sentence
  # was for, and the shading finds the live petitions by eye without sorting.
  dek <- paste0(n_case, if (n_case == 1) " case" else " cases",
    " distributed for this conference &mdash; sortable and filterable. Sort by ",
    "<em>Relists</em> to surface the serially-relisted cases; the darkest ",
    "<em>Grant forecast</em> cells are the likeliest grants.")

  scr_interactive(t, n_rows = nrow(tbl)) |>
    scr_write_page(
      file.path(out_dir, str_c("conf_", conf_date, ".html")),
      kicker = "Supreme Court of the United States",
      title = paste0("Conference of ", format(conf_date, "%B %d, %Y")),
      dek = dek, n_rows = nrow(tbl), left_cols = left_cols, footer = footer,
      # Eight columns now, not eleven: Type, Case, Relists, Grant forecast,
      # Court, Counsel, Documents, QP. Their natural widths total ~1220px, so
      # 78rem holds them without the table stretching to fill dead space -- which
      # is what made a row a 1470px scan from case name to number.
      leaf_max = 78,
      active = "/conferences/", pnav = pnav,
      crumb = list(label = format(conf_date, "%B %d, %Y"),
                   section = list(href = "/conferences/", label = "Conferences")),
      back = list(href = "index.html", label = "&larr; All conference reports"))

  invisible(file.path(out_dir, str_c("conf_", conf_date, ".html")))
}

# Regenerate index.html for the conference directory, listing every conf_*.html
# newest-first with its case count. The count is read back from each page's
# subtitle ("N case(s)"), so the index is always correct across all terms --
# including ones backfilled in a separate run.
conference_index <- function(out_dir = path.expand("~/public_html/conferences")) {
  files <- list.files(out_dir, pattern = "^conf_\\d{4}-\\d{2}-\\d{2}\\.html$")
  if (length(files) == 0) return(invisible(NULL))
  dates <- as.Date(str_extract(files, "\\d{4}-\\d{2}-\\d{2}"))
  ord <- order(dates, decreasing = TRUE)
  files <- files[ord]
  dates <- dates[ord]

  read_count <- function(f) {
    h <- paste(readLines(file.path(out_dir, f), warn = FALSE), collapse = " ")
    m <- str_match(h, "([0-9,]+)\\s+case")[, 2]
    if (is.na(m)) NA_integer_ else as.integer(str_remove_all(m, ","))
  }

  items <- purrr::map2(files, dates, function(f, d) {
    n <- read_count(f)
    count_txt <- if (!is.na(n)) paste0(n, if (n == 1) " case" else " cases") else ""
    list(href = f, label = format(d, "%B %d, %Y"), meta = count_txt)
  })

  styled_index_page(
    file.path(out_dir, "index.html"),
    title = "Conference Reports — SCOTUS",
    kicker = "Supreme Court of the United States",
    heading = "Conference Reports",
    dek = "What the Justices consider at each private conference, sorted by relists.",
    items = items,
    active = "/conferences/"   # was back = "← All dashboards" -> "/" (wrong both ways)
  )
  patch_prev_next(out_dir, "^conf_\\d{4}-\\d{2}-\\d{2}\\.html$", "conference",
                  key   = function(f) as.Date(str_extract(f, "\\d{4}-\\d{2}-\\d{2}")),
                  label = function(f) format(as.Date(str_extract(f, "\\d{4}-\\d{2}-\\d{2}")),
                                             "%B %d, %Y"))
  invisible(file.path(out_dir, "index.html"))
}

# Compute distributions once, render a dashboard for every conference date in
# `cases`, and (re)build the index. Returns the conference dates (invisibly).
conference_dashboards <- function(cases,
                                  out_dir = path.expand("~/public_html/conferences")) {
  dist <- conference_distributions(cases)
  dates <- dist |> distinct(conf_date) |> arrange(conf_date) |> pull(conf_date)
  message("Rendering ", length(dates), " conference date(s) to ", out_dir)
  for (i in seq_along(dates)) {
    conference_dash(dist, dates[i], out_dir = out_dir)
  }
  conference_index(out_dir)
  invisible(dates)
}
