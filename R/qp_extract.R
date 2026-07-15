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
  # Petition pages use layout whitespace that markdown misreads as code blocks
  # (monospace): 4+ leading spaces, or several spaces after a list marker like
  # "2.    Whether". Normalize all horizontal whitespace to single spaces (with
  # no leading indent) so the QP renders as prose / a clean numbered list.
  txt <- str_replace_all(txt, "\t", " ")                             # tabs -> space
  txt <- str_replace_all(txt, regex("^ +", multiline = TRUE), "")    # drop leading indent
  txt <- str_replace_all(txt, " {2,}", " ")                          # collapse layout spacing
  txt <- str_replace_all(txt, "\n{3,}", "\n\n")                      # collapse blank runs
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

# ---- granted-case Question Presented (the Court's dedicated QP PDF) -----------
# For a GRANTED case the Court posts a clean, typeset "Questions Presented" PDF
# (the JSON docket's QPLink). It is far more reliable than page 2 of the petition
# -- machine-readable text (no OCR), present even when the petition is a scan or
# the case came up on cert-before-judgment, and it is the QP *as granted*. The
# URL is derivable from the docket number (same form as QPLink).

qp_pdf_url <- function(dkt) {
  m <- str_match(dkt, "^(\\d{2})-(\\d+)$")             # paid/IFP dockets only
  if (is.na(m[1, 1])) return(NA_character_)
  sprintf("https://www.supremecourt.gov/qp/%s-%05dqp.pdf", m[1, 2], as.integer(m[1, 3]))
}

# QP text from the dedicated PDF: read all pages, cut to after the
# "Question(s) Presented" heading, drop the trailing "CERT. GRANTED ..." note.
get_granted_qp <- function(dkt) {
  url <- qp_pdf_url(dkt)
  if (is.na(url)) return("-")
  txt <- tryCatch(paste(pdftools::pdf_text(url), collapse = "\n"),
                  error = function(e) NA_character_)
  if (is.na(txt) || str_squish(txt) == "") return("-")
  txt <- txt |>
    str_replace_all("\t", " ") |>
    str_replace_all(regex("^ +", multiline = TRUE), "") |>
    str_replace_all(" {2,}", " ") |>
    str_replace_all("\n{3,}", "\n\n")
  m <- str_locate(txt, regex("QUESTIONS?\\s+PRESENTED\\s*[:.]?", ignore_case = TRUE))
  if (is.na(m[1, "end"])) return("-")
  qp <- str_trim(str_sub(txt, m[1, "end"] + 1L))
  qp <- str_trim(str_replace(qp, regex("\\s*CERT\\.?\\s+GRANTED\\b.*$", ignore_case = TRUE), ""))
  if (qp == "") "-" else qp
}

# Cache-backed resolver keyed by docket -> {qp}. The URL is derived, so none is
# stored. Fetches at most `max_new` uncached QP PDFs per run (they are small,
# clean text -- no OCR -- so this is far lighter than the petition path).
resolve_granted_qps <- function(dockets, cache_path = NULL, max_new = Inf) {
  cache <- list()
  if (!is.null(cache_path) && file.exists(cache_path)) {
    cache <- tryCatch(fromJSON(cache_path, simplifyVector = FALSE),
                      error = function(e) list())
  }
  uniq <- unique(dockets[!is.na(dockets)])
  uniq <- uniq[!is.na(vapply(uniq, qp_pdf_url, character(1)))]   # paid/IFP only
  needs <- uniq[vapply(uniq, function(d) is.null(cache[[d]]), logical(1))]
  n_fetch <- min(length(needs), max_new)
  if (n_fetch > 0) {
    message("granted QP: fetching ", n_fetch, " of ", length(needs), " uncached PDF(s)",
            if (is.finite(max_new)) paste0(" (cap ", max_new, ")") else "")
    for (d in head(needs, n_fetch)) {
      qp <- tryCatch(get_granted_qp(d), error = function(e) "-")
      if (!identical(qp, "-")) cache[[d]] <- list(qp = qp)
    }
    if (!is.null(cache_path)) {
      dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
      write_json(cache, cache_path, auto_unbox = TRUE)
    }
  }
  vapply(dockets, function(d) {
    c <- cache[[d]]; if (is.null(c) || is.null(c$qp)) "-" else c$qp
  }, character(1), USE.NAMES = FALSE)
}

# Reflow an extracted QP for clean markdown rendering. Petition PDFs wrap each
# question across lines and separate questions with blank lines, which markdown
# fragments into stray one-item lists and loose paragraphs. This rebuilds a
# multi-question QP as a single tight ordered list (wrapped lines joined,
# soft-hyphen line breaks repaired, items renumbered) and collapses a
# single-question QP to flowing prose. Idempotent.
reflow_qp <- function(txt) {
  if (length(txt) == 0 || is.na(txt) || identical(txt, "-") || txt == "") return(txt)
  lines <- str_trim(str_split(txt, "\n")[[1]])
  lines <- lines[lines != ""]
  if (length(lines) == 0) return("-")
  # Drop a trailing lone page number (some petitions number at the bottom).
  if (length(lines) > 1 &&
      str_detect(lines[length(lines)], "^[ivxlcdmIVXLCDM0-9]{1,4}$")) {
    lines <- lines[-length(lines)]
  }
  # Join a run of lines into one string, repairing soft-hyphen line breaks
  # ("harm-" + "lessness" -> "harmlessness").
  join_lines <- function(ls) {
    out <- ls[1]
    for (k in seq_len(length(ls) - 1L) + 1L) {
      out <- if (str_detect(out, "[A-Za-z]-$")) {
        paste0(str_sub(out, 1, -2), ls[k])
      } else {
        paste(out, ls[k])
      }
    }
    str_squish(out)
  }

  marker <- str_detect(lines, "^\\(?\\d+[.)]\\s")
  if (sum(marker) < 2) return(join_lines(lines)) # not a numbered list -> prose

  grp <- cumsum(marker)
  preamble <- if (any(grp == 0)) join_lines(lines[grp == 0]) else ""
  items <- tapply(seq_along(lines)[grp > 0], grp[grp > 0], function(idx) {
    str_replace(join_lines(lines[idx]), "^\\(?\\d+[.)]\\s*", "")
  })
  # Blank line between items: gt's markdown renders a loose list; a single
  # newline is NOT recognized as a list and falls back to prose.
  md_list <- paste0(seq_along(items), ". ", items, collapse = "\n\n")
  if (nzchar(preamble)) paste0(preamble, "\n\n", md_list) else md_list
}

# Wrap raw QP text as a collapsible <details> block (markdown-rendered).
qp_details <- function(qp) {
  qp <- vapply(qp, function(q) {
    if (is.na(q) || q == "" || q == "-") return("—") # em-dash, not a list marker
    reflow_qp(strip_qp_heading(q))
  }, character(1), USE.NAMES = FALSE)
  qp <- str_replace_all(qp, "\\$", "&#36;")
  # Blank lines around the content so a leading "1." sits at a line boundary and
  # markdown parses it as a list item (glued to </summary> it would not).
  str_c("<details><summary>Question(s) presented</summary>\n\n", qp, "\n\n</details>")
}
