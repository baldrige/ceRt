# Build the opinion pages: /opinions/<yyyy>/<docket>.{html,md,epub,json}.
# R/opinion_page.R says what is built and why; docs/opinions.md the rest.
#
# For each merits decision whose PDF is not yet built at the current versions:
# download the PDF (paced, and stopping on a run of failures, which is
# throttling), extract it to JSON, render the JSON to Markdown, EPUB and an
# HTML body (both Python, .github/scripts/opinions/), and set the body in the
# site's page. A decision whose JSON is already published at the extractor's
# version is re-rendered from it without a download.
#
# Env: SITE_DIR (default "site"), OPINION_MAX_NEW (PDFs to download this run,
#      default 40; 0 = re-render from the published JSON only), OPINION_PACE
#      (seconds between downloads, default 1), OPINION_ONLY (comma-separated
#      dockets, to build just those), PYTHON (default "python3").

suppressPackageStartupMessages({ library(tidyverse); library(httr2); library(jsonlite); library(htmltools) })
source("R/palette.R"); source("R/site_nav.R"); source("R/site_meta.R"); source("R/page_style.R")
source("R/opinion_page.R")

site_dir <- Sys.getenv("SITE_DIR", unset = "site")
max_new  <- as.integer(Sys.getenv("OPINION_MAX_NEW", unset = "40"))
pace     <- as.numeric(Sys.getenv("OPINION_PACE", unset = "1"))
only     <- setdiff(strsplit(Sys.getenv("OPINION_ONLY", unset = ""), "[, ]+")[[1]], "")
python   <- Sys.getenv("PYTHON", unset = "python3")
py_dir   <- ".github/scripts/opinions"

py <- function(script, args) {
  out <- suppressWarnings(system2(python, c(file.path(py_dir, script), shQuote(args)), stdout = TRUE, stderr = TRUE))
  list(ok = is.null(attr(out, "status")) || identical(attr(out, "status"), 0L), out = out)
}
versions <- function() {
  v <- system2(python, c("-c", shQuote(paste0(
    "import sys; sys.path.insert(0, '", py_dir, "'); import extract_opinion as e, render_opinion as r; ",
    "print(e.EXTRACT_VERSION, r.RENDER_VERSION)"))), stdout = TRUE, stderr = FALSE)
  v <- strsplit(trimws(v[length(v)]), " ")[[1]]
  if (length(v) != 2) stop("could not read the Python scripts' versions -- is pymupdf installed?")
  list(x = v[1], o = paste0(v[2], OPINION_PAGE_VERSION))
}
ver <- versions()
cat("opinions: extractor", ver$x, "| page", ver$o, "\n")

work <- opinion_worklist(site_dir)
if (length(only)) work <- work[work$dkt %in% only | vapply(work$dkts, function(d) any(d %in% only), logical(1)), , drop = FALSE]
idx <- read_opinion_index(site_dir)
cat("opinions:", nrow(work), "decision(s) with a file of their own;", sum(vapply(idx, function(e) isTRUE(e$lead), logical(1))), "built so far\n")

fetched <- 0L; down_run <- 0L; built <- 0L; failed <- character(); stopped <- ""
for (i in seq_len(nrow(work))) {
  o <- work[i, ]; o$dkts <- work$dkts[i]
  dkt <- o$dkt; term <- o$term
  have <- idx[[dkt]]
  stem <- file.path(site_dir, OPINION_DIR, term, dkt)
  json <- paste0(stem, ".json")
  need_x <- is.null(have) || !identical(have$url, o$url) || !identical(have$xv, ver$x) || !file.exists(json)
  need_o <- need_x || !identical(have$ov, ver$o) || !file.exists(paste0(stem, ".html"))
  if (!need_o) next
  dir.create(dirname(stem), recursive = TRUE, showWarnings = FALSE)
  if (need_x) {
    if (fetched >= max_new || nzchar(stopped)) next
    if (fetched > 0) Sys.sleep(pace * stats::runif(1, 0.75, 1.25))
    tf <- tempfile(fileext = ".pdf")
    got <- tryCatch({
      request(sub("#.*$", "", o$url)) |> req_user_agent("ceRt SCOTUS docketing dashboard (httr2)") |>
        req_retry(max_tries = 3) |> req_timeout(180) |> req_error(is_error = \(r) FALSE) |> req_perform(path = tf)
      file.exists(tf) && file.size(tf) > 1000 && identical(readBin(tf, "raw", 4L), charToRaw("%PDF"))
    }, error = function(e) FALSE)
    fetched <- fetched + 1L
    if (!got) {
      down_run <- down_run + 1L; unlink(tf)
      if (down_run >= 6L) stopped <- "6 downloads in a row failed -- throttled"
      next
    }
    down_run <- 0L
    r <- py("extract_opinion.py", c(tf, "-o", json))
    unlink(tf)
    if (!r$ok || !file.exists(json)) { failed <- c(failed, dkt); message("  ", dkt, ": extraction failed: ", paste(tail(r$out, 2), collapse = " ")); next }
  }
  doc <- tryCatch(fromJSON(json, simplifyVector = FALSE), error = function(e) NULL)
  # No opinion found in the file is a failure to read it, not an empty opinion.
  kinds <- if (is.null(doc)) character() else vapply(doc$sections, function(s) s$kind, character(1))
  if (!any(kinds %in% c("court", "percuriam", "plurality"))) {
    failed <- c(failed, dkt); message("  ", dkt, ": no opinion of the Court found in the PDF; not published")
    unlink(json); next
  }
  # The opinion's own short caption where its running head gave one.
  o$long <- o$caption
  o$caption <- opinion_short_caption(doc$short, o$long) %||% o$long
  meta <- list(docket = paste(unlist(o$dkts), collapse = ", "), docket_id = dkt, term = as.character(term),
               caption = o$caption, decided = o$decided, decided_long = .op_long_date(o$decided),
               cite = doc$cite, pdf_url = o$url)
  mf <- tempfile(fileext = ".json"); write_json(meta, mf, auto_unbox = TRUE, null = "null")
  r <- py("render_opinion.py", c(json, mf, stem)); unlink(mf)
  bodyf <- paste0(stem, ".body.html")
  if (!r$ok || !file.exists(bodyf)) { failed <- c(failed, dkt); message("  ", dkt, ": render failed: ", paste(tail(r$out, 2), collapse = " ")); next }
  body <- paste(readLines(bodyf, warn = FALSE, encoding = "UTF-8"), collapse = "\n"); unlink(bodyf)
  render_opinion_page(site_dir, o, body, doc)
  entry <- list(href = opinion_href(term, dkt), url = o$url, decided = o$decided, term = term, caption = o$caption,
                xv = ver$x, ov = ver$o, cite = doc$cite, layout = doc$layout, words = doc$qa$words, notes = doc$qa$notes,
                unmatched = length(doc$qa$notes_unmatched), lead = TRUE)
  idx[[dkt]] <- entry
  for (d in setdiff(unlist(o$dkts), dkt)) idx[[d]] <- list(href = entry$href, lead = FALSE, of = dkt)
  built <- built + 1L
  if (built %% 20 == 0) { write_opinion_index(site_dir, idx); cat("  ", built, "built\n") }
}
write_opinion_index(site_dir, idx)
n <- write_opinion_index_page(site_dir, idx)
n_opds <- write_opinion_opds(site_dir, idx)
cat("opinions: OPDS catalogue of", n_opds %||% 0, "EPUB(s)\n")
left <- sum(!work$dkt %in% names(idx))
cat("opinions:", built, "built this run (", fetched, "download(s));", length(failed), "failed;", left, "still to build;",
    n %||% 0, "on the index\n")
if (length(failed)) cat("  failed:", paste(failed, collapse = ", "), "\n")
if (nzchar(stopped)) cat("  stopped early:", stopped, "\n")
gh <- Sys.getenv("GITHUB_STEP_SUMMARY", unset = "")
if (nzchar(gh)) cat(sprintf("Opinions: %d built, %d failed, %d still to build%s\n\n", built, length(failed), left,
                            if (nzchar(stopped)) paste0(" — stopped: ", stopped) else ""), file = gh, append = TRUE)
