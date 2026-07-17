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
case_conference_dates <- function(events) {
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
      petition_url = map_chr(events, find_petition_url)
    ) |>
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
      granted_dockets = gd), error = function(e) NULL)
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
                            qp_map = NULL, models = NULL) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  conf_date <- as.Date(conf_date)

  d <- dist |> filter(conf_date == !!conf_date)
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
  p_grant <- rep(NA_real_, nrow(d)); p_gvr <- rep(NA_real_, nrow(d))
  if (!is.null(models) && !is.null(models$enhanced) && !is.null(models$gvr) &&
      exists("score_disposition")) {
    gd <- if ("outcome" %in% names(d)) d$dkt[d$outcome %in% "granted"] else character()
    for (i in seq_len(nrow(d))) {
      if (!identical(d$type[i], "paid")) next
      par <- if (has_parties) d$parties[[i]] else NULL
      s <- tryCatch(score_disposition(
        models$enhanced, models$gvr, d$caption[i], d$lower[i], par,
        dt[i], ld[i], rel[i], events = d$events[[i]], as_of = conf_date,
        granted_dockets = gd), error = function(e) NULL)
      if (!is.null(s)) { p_grant[i] <- s$p_grant; p_gvr[i] <- s$p_gvr }
    }
  }

  # One editorial row per distributed case. Relists = prior distributions.
  qp_get <- function(dk) if (is.null(qp_map)) NA_character_ else unname(qp_map[dk])
  tbl <- tibble(
    Type = factor(d$type, levels = c("paid", "ifp", "app"),
                  labels = c("Paid", "IFP", "Application")),
    Case = sprintf(
      "<a href='https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/%s.html' target='_blank'>%s</a>",
      d$dkt,
      str_squish(str_remove_all(d$caption, ", Petitioners?|, Respondents?|, et al\\."))),
    Docket = d$dkt,
    Relists = d$distribution_no - 1L,
    Grant = p_grant,
    GVR = p_gvr,
    Court = str_replace(coalesce(d$lower, "—"),
              "^United States Court of Appeals for the (.+?Circuit)$", "\\1") |>
              str_trunc(28),
    # Petitioner's counsel of record + firm, as on the daily dashboards. Only the
    # JSON pipeline carries a parties structure; the historical scrape does not,
    # so this is "—" (and the column is dropped) on the pre-JSON archive.
    Counsel = if ("parties" %in% names(d)) map_chr(d$parties, petitioner_counsel_html)
              else rep("—", nrow(d)),
    Documents = map_chr(d$events, function(e)
                  case_documents(e, c("Petition", "Appendix", "BIO", "Reply"))),
    QP = { q <- map_chr(d$dkt, qp_get); ifelse(is.na(q) | q == "", "—", q) }
  ) |> arrange(desc(Relists), desc(Grant))

  # Drop the forecast columns on conferences with no paid petitions (all NA),
  # and any column that is entirely empty -- e.g. QP and Counsel on the pre-JSON
  # historical archive, which has neither source (matches the old renderer, which
  # omitted the column rather than showing a wall of em dashes).
  has_grant <- any(!is.na(tbl$Grant))
  if (!has_grant) tbl <- select(tbl, -Grant, -GVR)
  for (col in c("Counsel", "QP", "Documents")) {
    if (col %in% names(tbl) && all(tbl[[col]] == "—")) tbl <- select(tbl, -all_of(col))
  }
  has_qp <- "QP" %in% names(tbl)

  left_cols <- match(intersect(c("Case", "Court", "Counsel", "Documents", "QP"), names(tbl)),
                     names(tbl))

  t <- tbl |>
    gt() |>
    fmt_markdown(columns = any_of(c("Case", "Counsel", "Documents", "QP"))) |>
    data_color(columns = Type, method = "factor",
      palette = c("Paid" = "#e4e7d8", "IFP" = "#efe1cd", "Application" = "#dfe4ea")) |>
    cols_align("center", columns = everything()) |>
    cols_width(Case ~ px(230))
  if (has_qp) t <- t |> cols_label(QP = "Questions Presented") |> cols_width(QP ~ px(190))
  if (has_grant) {
    t <- t |>
      fmt_percent(columns = c(Grant, GVR), decimals = 0) |>
      # NA (non-paid) forecasts show as an em dash; raw values stay numeric so
      # the columns still sort by value.
      sub_missing(columns = c(Grant, GVR), missing_text = "—") |>
      data_color(columns = Grant, palette = c("#f3ecdd", "#e8c9a0", "#c8794f", "#8a2b2b"),
                 domain = c(0, 1), na_color = "#f7f1e4") |>
      data_color(columns = GVR, palette = c("#f3ecdd", "#dfe0cf", "#b9b98f"),
                 domain = c(0, 0.4), na_color = "#f7f1e4") |>
      cols_label(Grant = "Grant forecast", GVR = "GVR risk")
  }

  footer <- if (has_grant) paste0(
    "<em>Grant forecast</em> is a calibrated model estimate of plenary certiorari ",
    "for paid petitions (base rate ~4%), scored as of this conference; <em>GVR ",
    "risk</em> is the companion estimate of a grant, vacate &amp; remand. ",
    "Estimates, not predictions about any case."
  ) else ""

  n_case <- nrow(tbl)
  dek <- paste0(n_case, if (n_case == 1) " case" else " cases",
    " distributed for this conference &mdash; sortable and filterable. Sort by ",
    "<em>Relists</em> or <em>Grant forecast</em> to surface the serially-relisted cases.")

  scr_interactive(t, n_rows = nrow(tbl)) |>
    scr_write_page(
      file.path(out_dir, str_c("conf_", conf_date, ".html")),
      kicker = "Supreme Court of the United States",
      title = paste0("Conference of ", format(conf_date, "%B %d, %Y")),
      dek = dek, n_rows = nrow(tbl), left_cols = left_cols, footer = footer,
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
    back = list(href = "../", label = "← All dashboards")
  )
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
