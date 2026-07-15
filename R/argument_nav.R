# argument_nav.R ---------------------------------------------------------------
# The Oral Argument Navigator: for every merits grant, extract its argument
# lifecycle from the docket events and present each Term's argument calendar,
# grouped by sitting (October, November, ... April sessions).
#
# A granted case's docket records the schedule directly:
#   "SET FOR ARGUMENT on Wednesday, April 23, 2025."   -> scheduled date
#   "Argued. For petitioners: ...  For respondents: ..." -> argued date + advocates
#   "Writ of certiorari DISMISSED as improvidently granted." -> DIG'd
#   a merits judgment / opinion entry                    -> decided
#
# A sitting draws cases from MULTIPLE docket-number terms (a 22-, 23- and 24-
# docket can all be argued in OT2024), so cases are grouped by the term they are
# ARGUED in, not their docket term. Grant detection is reused from cert_funnel.R
# (classify_petitions, adversarially verified); this module only adds the
# argument-stage extraction and rendering.

suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse); library(lubridate); library(htmltools)
})

# Shared modules: classify_petitions()/term_label() from cert_funnel.R and the
# page helpers from page_style.R, sourced relative to this file.
local({
  here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA)
  find <- function(f) {
    if (!is.na(here) && file.exists(file.path(here, f))) file.path(here, f)
    else if (file.exists(file.path("R", f))) file.path("R", f)
    else f
  }
  sys.source(find("cert_funnel.R"), envir = globalenv())
  sys.source(find("page_style.R"), envir = globalenv())
})

# ---- argument-stage classification --------------------------------------------

# Classify one granted case's argument lifecycle from its events. One-row tibble.
classify_argument <- function(events) {
  empty <- tibble(scheduled_date = as.Date(NA), argued_date = as.Date(NA),
                  decided_date = as.Date(NA), n_settings = 0L,
                  vided = FALSE, dig = FALSE, argued_text = NA_character_,
                  status = "granted")
  if (!is.data.frame(events) || !("Proceedings and Orders" %in% names(events)) ||
      nrow(events) == 0) return(empty)
  txt <- events[["Proceedings and Orders"]]; txt[is.na(txt)] <- ""
  edate <- suppressWarnings(mdy(events$Date))
  ord <- order(edate); txt <- txt[ord]; edate <- edate[ord]

  # SET FOR ARGUMENT on [Weekday, ]Month D, YYYY.  The LAST setting is effective
  # (a reschedule re-sets it), so a "Reset for argument" is handled implicitly.
  set_hits <- str_match(txt, regex(
    "SET FOR ARGUMENT on (?:[A-Za-z]+,\\s+)?([A-Za-z]+ \\d{1,2}, \\d{4})",
    ignore_case = TRUE))[, 2]
  set_idx <- which(!is.na(set_hits))
  scheduled <- if (length(set_idx)) mdy(tail(set_hits[set_idx], 1)) else as.Date(NA)

  arg_idx <- which(str_detect(txt, regex("^Argued\\.", ignore_case = TRUE)))
  argued_date <- if (length(arg_idx)) edate[arg_idx[1]] else as.Date(NA)
  argued_text <- if (length(arg_idx)) txt[arg_idx[1]] else NA_character_

  dig <- any(str_detect(txt, regex("DISMISSED as improvidently granted", ignore_case = TRUE)))
  dec_idx <- which(str_detect(txt, regex(
    paste0("Judgment (AFFIRMED|REVERSED|VACATED|entered)",
           "|Adjudged to be (AFFIRMED|REVERSED)",
           "|delivered the opinion of the Court"),
    ignore_case = TRUE)))
  decided_date <- if (length(dec_idx)) edate[dec_idx[1]] else as.Date(NA)

  status <- if (dig) "DIG'd"
    else if (!is.na(decided_date)) "Decided"
    else if (!is.na(argued_date)) "Argued"
    else if (!is.na(scheduled)) "Scheduled"
    else "Granted"

  tibble(scheduled_date = scheduled, argued_date = argued_date,
         decided_date = decided_date, n_settings = length(set_idx),
         vided = any(str_detect(txt, regex("SET FOR ARGUMENT.*VIDED", ignore_case = TRUE))),
         dig = dig, argued_text = argued_text, status = status)
}

# Compact "argued by" from an "Argued. For <side>: <Name>, <City>. ..." entry:
# the lead advocate name from each side, joined.
extract_advocates <- function(argued_text) {
  if (is.na(argued_text) || argued_text == "") return(NA_character_)
  t <- str_remove(argued_text, regex("^Argued\\.\\s*", ignore_case = TRUE))
  segs <- str_match_all(t, "For [^:]+:\\s*([A-Z][^,;]+)")[[1]]
  if (nrow(segs) == 0) return(str_trunc(str_squish(t), 70))
  str_trunc(paste(unique(str_squish(segs[, 2])), collapse = " · "), 70)
}

# Petition-for-certiorari URL from a case's events (handles both the historical
# scrape's Document_*/links_* and the JSON build_events() docs_*/links_* layouts).
arg_petition_url <- function(events) {
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

# ---- assemble the argument table ----------------------------------------------

# The Term a case is ARGUED in: Oct-Dec -> that year's Term; Jan-Jun -> prior.
argument_term <- function(d) if_else(is.na(d), NA_integer_,
                                     if_else(month(d) >= 9L, year(d), year(d) - 1L))

# Build a tidy table of every merits grant with an argument date (scheduled or
# argued), across all docket terms present in `cases`. `qp_map` (optional) maps
# raw docket -> QP <details> HTML.
build_argument_table <- function(cases, qp_map = NULL) {
  cls <- classify_petitions(cases)
  granted <- cls |> filter(outcome == "granted") |> distinct(dkt)
  g <- cases |> filter(dkt %in% granted$dkt) |> distinct(dkt, .keep_all = TRUE)
  if (nrow(g) == 0) return(tibble())

  arg <- bind_cols(
    g |> transmute(dkt, caption = str_squish(caption %||% dkt)),
    map_dfr(g$events, classify_argument)
  ) |>
    mutate(
      petition_url = map_chr(g$events, arg_petition_url),
      advocates = map_chr(argued_text, extract_advocates),
      arg_ref = coalesce(argued_date, scheduled_date),
      term = argument_term(arg_ref),
      sitting_date = if_else(is.na(arg_ref), as.Date(NA),
                             floor_date(arg_ref, "month")),
      sitting = if_else(is.na(arg_ref), NA_character_, format(arg_ref, "%B %Y")),
      qp = if (is.null(qp_map)) NA_character_ else unname(qp_map[dkt])
    ) |>
    filter(!is.na(arg_ref))                      # only cases with an argument date
  arg
}

# ---- rendering ----------------------------------------------------------------

STATUS_FILL <- c(Scheduled = "#A6CEE3", Argued = "#B2DF8A",
                 Decided = "#E4DAC2", `DIG'd` = "#FB9A99")

# Render one Term's argument calendar (a gt table grouped by sitting).
argument_term_page <- function(tbl, term, out_dir) {
  d <- tbl |>
    filter(term == !!term) |>
    arrange(sitting_date, arg_ref, as.integer(str_extract(dkt, "\\d+$")))
  if (nrow(d) == 0) return(invisible(NULL))

  has_qp <- any(!is.na(d$qp))
  d <- d |>
    mutate(
      when = format(arg_ref, "%a %b %d"),
      case = caption,
      docket = if_else(!is.na(petition_url) & petition_url != "",
        str_c("[", dkt, "](", petition_url, ")"),
        str_c("[", dkt,
              "](https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/",
              dkt, ".html)")),
      argued_by = if_else(is.na(advocates), "—", advocates),
      qp = if_else(is.na(qp), "—", qp)
    ) |>
    select(sitting, when, case, docket, status, argued_by,
           any_of(if (has_qp) "qp" else character()))

  labels <- list(when = "", case = "Case", docket = "Docket", status = "Status",
                 argued_by = "Argued by", qp = "QP")
  labels <- labels[names(labels) %in% names(d)]
  md_cols <- intersect(c("docket", "qp"), names(d))

  tbl_gt <- d |>
    gt(groupname_col = "sitting") |>
    fmt_markdown(columns = all_of(md_cols)) |>
    tab_header(
      title = paste0(term_label(term - 2000L), " — Oral Argument Calendar"),
      subtitle = paste0(nrow(d), " case(s) argued or scheduled")
    ) |>
    gt_theme_nytimes() |>
    cols_label(.list = labels) |>
    cols_align(align = "center", columns = everything()) |>
    cols_align(align = "left", columns = any_of("qp"))
  for (s in names(STATUS_FILL)) {
    tbl_gt <- tbl_gt |>
      tab_style(cell_fill(color = STATUS_FILL[[s]]),
                cells_body(columns = status, rows = status == s))
  }
  gtsave_titled(tbl_gt, str_c("arg_", term, ".html"), path = out_dir,
                title = paste0(term_label(term - 2000L), " oral arguments — SCOTUS"))
  invisible(file.path(out_dir, str_c("arg_", term, ".html")))
}

# Styled Term index over the arg_*.html pages (case counts read back from each).
argument_index <- function(out_dir) {
  files <- list.files(out_dir, pattern = "^arg_\\d{4}\\.html$")
  if (length(files) == 0) return(invisible(NULL))
  terms <- as.integer(str_extract(files, "\\d{4}"))
  ord <- order(terms, decreasing = TRUE); files <- files[ord]; terms <- terms[ord]

  read_count <- function(f) {
    h <- paste(readLines(file.path(out_dir, f), warn = FALSE, encoding = "UTF-8"),
               collapse = " ")
    m <- str_match(h, "([0-9,]+)\\s+case")[, 2]
    if (is.na(m)) NA_integer_ else as.integer(str_remove_all(m, ","))
  }
  items <- purrr::map2(files, terms, function(f, t) {
    n <- read_count(f)
    list(href = f, label = term_label(t - 2000L),
         meta = if (!is.na(n)) paste0(n, if (n == 1) " case" else " cases") else "")
  })
  styled_index_page(
    file.path(out_dir, "index.html"),
    title = "Oral Argument Navigator — SCOTUS",
    kicker = "Supreme Court of the United States",
    heading = "Oral Argument Navigator",
    dek = "Every granted case and when it is heard, Term by Term and sitting by sitting.",
    items = items,
    back = list(href = "../", label = "← All dashboards")
  )
  invisible(file.path(out_dir, "index.html"))
}

# Build the table (or use a prebuilt `tbl`), render a page per argued Term, and
# (re)build the index. Passing `tbl` avoids re-running the classifier when the
# caller has already built the table (e.g. to attach QP from a cache).
render_argument_nav <- function(cases = NULL, out_dir, qp_map = NULL, tbl = NULL) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (is.null(tbl)) tbl <- build_argument_table(cases, qp_map = qp_map)
  if (nrow(tbl) == 0) { message("No argued/scheduled grants found."); return(invisible(NULL)) }
  terms <- tbl |> distinct(term) |> filter(!is.na(term)) |> arrange(term) |> pull(term)
  # A Term's fall sitting is argued from the PRIOR docket term, so the earliest
  # Term in the archive is incomplete (we lack the term before it). Drop it so it
  # doesn't read as a data error; every later Term has both docket terms present.
  min_dkt_year <- 2000L + suppressWarnings(min(as.integer(str_sub(tbl$dkt, 1, 2)), na.rm = TRUE))
  if (length(terms) > 1) terms <- terms[terms > min_dkt_year]
  for (t in terms) argument_term_page(tbl, t, out_dir)
  argument_index(out_dir)
  invisible(terms)
}
