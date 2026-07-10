# qp_extract.R -----------------------------------------------------------------
# Extract the Question(s) Presented from petition PDFs, with a persistent,
# incremental on-disk cache so a large corpus (e.g. all cases distributed for a
# term's conferences) is only fetched once. Shared by the daily dashboard
# (scotus_dash_new.R) and the conference reports (render_conferences.R).

suppressPackageStartupMessages({
  library(tidyverse)
  library(pdftools)
  library(jsonlite)
})

# Drop the boilerplate that opens a petition's Question-Presented page: an
# optional page number (usually "i") and the "Question(s) Presented" heading.
# Finds the heading within the first stretch of text and cuts everything up to
# and including it, so a leading page number or running header goes too. If no
# heading is found near the top, the text is returned unchanged.
strip_qp_heading <- function(txt) {
  if (length(txt) == 0 || is.na(txt) || identical(txt, "-")) return(txt)
  m <- str_locate(
    str_sub(txt, 1, 300),
    regex("QUESTION\\(?[Ss]?\\)?\\s+PRESENTED(\\s+FOR\\s+REVIEW)?\\s*[:.]?",
          ignore_case = TRUE)
  )
  if (is.na(m[1, "end"])) return(str_trim(txt))
  str_trim(str_sub(txt, m[1, "end"] + 1L))
}

# Extract the QP text from page 2 of a petition PDF (a URL or a local path).
# Tries the text layer first (fast); falls back to OCR only when there's none.
extract_qp_page2 <- function(src) {
  page2 <- function(fn) {
    t <- fn(src)
    t <- if (length(t) >= 2) t[[2]] else t[[1]]
    if (is.null(t) || str_squish(t) == "") stop("empty page")
    t
  }
  txt <- tryCatch(
    page2(pdftools::pdf_text),
    error = function(e) tryCatch(
      page2(function(s) pdftools::pdf_ocr_text(s, pages = 2)),
      error = function(e2) "-"
    )
  )
  if (identical(txt, "-")) return(txt)
  # Petition pages are indented, and markdown renders any line starting with 4+
  # spaces as a code block (monospace). Strip per-line leading whitespace and
  # collapse blank-line runs so the QP renders as normal prose.
  txt <- str_replace_all(txt, regex("^[ \\t]+", multiline = TRUE), "")
  txt <- str_replace_all(txt, "\n{3,}", "\n\n")
  strip_qp_heading(str_trim(txt))
}

# Single petition QP (pdftools downloads the URL itself).
get_qp <- function(url) {
  if (is.na(url) || url == "") return("-")
  extract_qp_page2(url)
}

# Cache-backed batch resolver. Returns a named character vector docket -> QP for
# every docket in `dockets` (aligned; "-" where unavailable). The cache is a
# JSON object keyed by docket, storing the petition URL and extracted text so a
# stale URL (petition re-filed) is re-fetched. Only up to `max_new` uncached
# petitions are fetched per run to bound per-run PDF load; the rest stay "-"
# until a later run, and the cache fills in incrementally.
resolve_qps <- function(dockets, urls, cache_path = NULL, max_new = Inf) {
  stopifnot(length(dockets) == length(urls))
  cache <- list()
  if (!is.null(cache_path) && file.exists(cache_path)) {
    cache <- tryCatch(fromJSON(cache_path, simplifyVector = FALSE),
                      error = function(e) list())
  }

  # Unique dockets with a usable URL and no fresh cache hit.
  uniq <- tibble(dkt = dockets, url = urls) |>
    filter(!is.na(url), url != "") |>
    distinct(dkt, .keep_all = TRUE)
  needs <- uniq |>
    rowwise() |>
    mutate(cached = {
      c <- cache[[dkt]]
      !is.null(c) && identical(c$url, url)
    }) |>
    ungroup() |>
    filter(!cached)

  n_fetch <- min(nrow(needs), max_new)
  if (n_fetch > 0) {
    fetch <- needs |> slice_head(n = n_fetch)
    message("QP: fetching ", n_fetch, " of ", nrow(needs), " uncached petition(s)",
            if (is.finite(max_new)) paste0(" (cap ", max_new, ")") else "")
    for (i in seq_len(nrow(fetch))) {
      qp <- tryCatch(get_qp(fetch$url[i]), error = function(e) "-")
      # Cache only successful extractions; a "-" (empty page, OCR miss, or a
      # transient throttle) is left uncached so it retries on a later run.
      if (!identical(qp, "-")) cache[[fetch$dkt[i]]] <- list(url = fetch$url[i], qp = qp)
    }
    if (!is.null(cache_path)) {
      dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
      write_json(cache, cache_path, auto_unbox = TRUE)
    }
  }

  vapply(dockets, function(d) {
    c <- cache[[d]]
    if (is.null(c) || is.null(c$qp)) "-" else c$qp
  }, character(1), USE.NAMES = FALSE)
}

# Wrap raw QP text as a collapsible <details> block (markdown-rendered).
qp_details <- function(qp) {
  qp <- if_else(is.na(qp) | qp == "", "-", qp)
  qp <- vapply(qp, strip_qp_heading, character(1), USE.NAMES = FALSE) # idempotent
  qp <- str_replace_all(qp, "\\$", "&#36;")
  str_c("<details><summary>Question(s) presented</summary>", qp, "</details>")
}
