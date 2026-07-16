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

# Shared page-presentation helpers (gtsave_titled, styled_index_page), sourced
# relative to this file's location so it works from the repo root or elsewhere.
local({
  here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA)
  ps <- if (!is.na(here) && file.exists(file.path(here, "page_style.R"))) {
    file.path(here, "page_style.R")
  } else if (file.exists("R/page_style.R")) {
    "R/page_style.R"
  } else {
    "page_style.R"
  }
  sys.source(ps, envir = globalenv())
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

  # Model forecast (computed on the RAW fields, before the display transforms).
  # A paid petition gets P(grant); a held one also shows its GVR risk. Everything
  # is defensive: absent models or a scoring error just leaves the em-dash.
  d$forecast <- conference_forecast(d, conf_date, models)

  d <- d |>
    # Keep the case name on one line (no v. break).
    mutate(caption = caption |>
      str_replace_all("\\s+", " ") |>
      str_remove(", Petitioners?") |>
      str_remove(", Respondents?") |>
      str_remove_all(", et al.") |>
      str_trim()) |>
    mutate(lower = str_replace(
      lower, "^United States Court of Appeals for the (.+?Circuit)", "\\1"
    )) |>
    mutate(status = if_else(
      distribution_no == 1, "Initial",
      paste0("Relisted (", distribution_no - 1, "×)")
    )) |>
    mutate(petition = if_else(
      !is.na(petition_url) & petition_url != "",
      str_c("[Petition](", petition_url, ")"), "—"
    )) |>
    mutate(counsel = if ("parties" %in% names(dist)) {
      map_chr(parties, counsel_cell)
    } else {
      NA_character_
    }) |>
    mutate(qp = if (is.null(qp_map)) NA_character_ else unname(qp_map[dkt])) |>
    # Most-relisted cases first (the interesting ones), then paid -> ifp -> app
    # and docket number, while dkt is still raw.
    arrange(
      desc(distribution_no),
      factor(type, levels = c("paid", "ifp", "app")),
      as.integer(str_extract(dkt, "\\d+$"))
    ) |>
    mutate(dkt = str_c(
      "[", dkt,
      "](https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/",
      dkt, ".html)"
    ))

  has_counsel <- any(!is.na(d$counsel))
  has_qp <- any(!is.na(d$qp))
  has_forecast <- any(d$forecast != "—")
  d <- d |>
    mutate(counsel = if_else(is.na(counsel), "—", counsel),
           qp = if_else(is.na(qp), "—", qp)) |>
    select(type, caption, dkt, petition,
           any_of(if (has_counsel) "counsel" else character()),
           lower, status,
           any_of(if (has_forecast) "forecast" else character()),
           any_of(if (has_qp) "qp" else character()))

  md_cols <- intersect(c("caption", "dkt", "petition", "counsel", "forecast", "qp"), names(d))
  labels <- list(
    type = "Type", caption = "Caption", dkt = "Docket No", petition = "Petition",
    counsel = "Petitioner's Counsel", lower = "Court Below", status = "Status",
    forecast = "Grant forecast", qp = "QP"
  )
  labels <- labels[names(labels) %in% names(d)]

  tbl <- d |>
    gt() |>
    fmt_markdown(columns = all_of(md_cols)) |>
    tab_header(
      title = paste0("Cases distributed for the Conference of ",
                     format(conf_date, "%B %d, %Y")),
      subtitle = paste0(nrow(d), " case(s)")
    ) |>
    gt_theme_nytimes() |>
    cols_label(.list = labels) |>
    # Center every column to match the daily dashboards; the QP <details> block
    # reads better left-aligned (as it is on the daily).
    cols_align(align = "center", columns = everything()) |>
    cols_align(align = "left", columns = any_of("qp")) |>
    # Fixed per-type fills: paid = green, ifp = orange, app = blue.
    tab_style(cell_fill(color = "#B2DF8A"),
              cells_body(columns = type, rows = type == "paid")) |>
    tab_style(cell_fill(color = "#FDBF6F"),
              cells_body(columns = type, rows = type == "ifp")) |>
    tab_style(cell_fill(color = "#A6CEE3"),
              cells_body(columns = type, rows = type == "app"))
  if (has_forecast) {
    tbl <- tbl |> tab_source_note(gt::md(paste0(
      "*Grant forecast* is a calibrated model estimate of plenary certiorari for ",
      "paid petitions (base rate ~4%), scored as of this conference. A **held** ",
      "petition — one relisted well beyond the norm — carries low grant odds but ",
      "elevated GVR risk. Estimates, not predictions about any case.")))
  }
  gtsave_titled(tbl, str_c("conf_", conf_date, ".html"), path = out_dir,
                title = paste0("Conference of ", format(conf_date, "%B %d, %Y"),
                               " — SCOTUS"))

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
