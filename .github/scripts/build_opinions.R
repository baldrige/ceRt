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
source("R/site_decisions.R")      # .parse_opinion_listing(), for the orders listing
source("R/opinion_page.R")

site_dir <- Sys.getenv("SITE_DIR", unset = "site")
max_new  <- as.integer(Sys.getenv("OPINION_MAX_NEW", unset = "40"))
pace     <- as.numeric(Sys.getenv("OPINION_PACE", unset = "1"))
only     <- setdiff(strsplit(Sys.getenv("OPINION_ONLY", unset = ""), "[, ]+")[[1]], "")
# OPINION_FORCE: dockets to read again from the PDF whatever the index says --
# to repair one opinion after an extractor fix without re-reading all of them.
force    <- setdiff(strsplit(Sys.getenv("OPINION_FORCE", unset = ""), "[, ]+")[[1]], "")
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
# Opinions relating to orders (R/opinion_page.R): the Court's own listing, the
# two newest Terms re-read each run; OPINION_ORDER_TERMS (two-digit, comma-
# separated) reads others, for the back-catalogue.
now_term <- opinion_term(Sys.Date()) %% 100L
order_terms <- setdiff(strsplit(Sys.getenv("OPINION_ORDER_TERMS", unset = ""), "[, ]+")[[1]], "")
if (!length(order_terms)) order_terms <- sprintf("%02d", c(now_term, now_term - 1L))
orders <- tryCatch(opinion_orders_worklist(site_dir, refresh_orders_cache(site_dir, order_terms)),
                   error = function(e) { message("orders listing skipped: ", conditionMessage(e)); tibble::tibble() })
# A docket with a merits opinion keeps that page.
if (nrow(orders)) work <- dplyr::bind_rows(work, orders[!orders$dkt %in% unlist(work$dkts), , drop = FALSE])
skipped <- .op_read_json(opinion_skipped_path(site_dir))
if (length(only)) work <- work[work$dkt %in% only | vapply(work$dkts, function(d) any(d %in% only), logical(1)), , drop = FALSE]
idx <- read_opinion_index(site_dir)
cat("opinions:", sum(work$class == "orders"), "relating to orders on the list;", length(skipped), "of those already found short\n")
cat("opinions:", nrow(work), "decision(s):", sum(is.na(work$volume)), "with a file of their own,", sum(!is.na(work$volume)),
    "inside", length(unique(stats::na.omit(work$volume))), "volume(s);",
    sum(vapply(idx, function(e) isTRUE(e$lead), logical(1))), "built so far\n")

forced <- character()
fetched <- 0L; down_run <- 0L; built <- 0L; failed <- character(); stopped <- ""; n_short <- 0L

# A PDF to a temp file, or NULL. Counts toward the run's cap and its
# throttling stop whichever kind of file it is.
download <- function(url, timeout = 180) {
  if (fetched > 0) Sys.sleep(pace * stats::runif(1, 0.75, 1.25))
  tf <- tempfile(fileext = ".pdf")
  got <- tryCatch({
    request(sub("#.*$", "", url)) |> req_user_agent("ceRt SCOTUS docketing dashboard (httr2)") |>
      req_retry(max_tries = 3) |> req_timeout(timeout) |> req_error(is_error = \(r) FALSE) |> req_perform(path = tf)
    file.exists(tf) && file.size(tf) > 1000 && identical(readBin(tf, "raw", 4L), charToRaw("%PDF"))
  }, error = function(e) FALSE)
  fetched <<- fetched + 1L
  if (!got) {
    unlink(tf); down_run <<- down_run + 1L
    if (down_run >= 6L) stopped <<- "6 downloads in a row failed -- throttled"
    return(NULL)
  }
  down_run <<- 0L
  tf
}

stem_of <- function(o) file.path(site_dir, OPINION_DIR, o$term, o$dkt)
# What a decision still needs: "x" (read its PDF), "o" (render what is read), or "".
needs <- function(o) {
  have <- idx[[o$dkt]]; stem <- stem_of(o)
  if (o$dkt %in% force && !o$dkt %in% forced) { forced <<- c(forced, o$dkt); return("x") }
  # Read once, found short, and nothing about it has changed since.
  sk <- skipped[[o$dkt]]
  if (!is.null(sk) && identical(sk$url, o$url) && identical(sk$xv, ver$x) && identical(as.integer(sk$min), OPINION_MIN_WORDS)) return("")
  if (is.null(have) || !identical(have$url, o$url) || !identical(have$xv, ver$x) || !file.exists(paste0(stem, ".json"))) return("x")
  if (!identical(have$ov, ver$o) || !file.exists(paste0(stem, ".html"))) return("o")
  ""
}

# From the extracted JSON to the published page and its index entry.
# `pdf_url` is the link the page offers: for a case in a volume, the file that
# was actually read, at the page the case was found on.
finish <- function(o, pdf_url = o$url) {
  dkt <- o$dkt; term <- o$term; stem <- stem_of(o); json <- paste0(stem, ".json")
  doc <- tryCatch(fromJSON(json, simplifyVector = FALSE), error = function(e) NULL)
  # No opinion found in the file is a failure to read it, not an empty opinion.
  kinds <- if (is.null(doc)) character() else vapply(doc$sections, function(s) s$kind, character(1))
  if (identical(o$class, "orders")) {
    # A dissent from a denial has no opinion of the Court; any writing will do.
    if (!length(kinds)) {
      failed <<- c(failed, dkt); message("  ", dkt, ": no writing found in the PDF; not published")
      unlink(json); return(invisible(FALSE))
    }
    if ((doc$qa$words %||% 0L) < OPINION_MIN_WORDS) {
      skipped[[dkt]] <<- list(url = o$listed, xv = ver$x, words = doc$qa$words, min = OPINION_MIN_WORDS)
      n_short <<- n_short + 1L
      unlink(json); return(invisible(FALSE))
    }
  } else if (!any(kinds %in% c("court", "percuriam", "plurality"))) {
    failed <<- c(failed, dkt); message("  ", dkt, ": no opinion of the Court found in the PDF; not published")
    unlink(json); return(invisible(FALSE))
  }
  skipped[[dkt]] <<- NULL
  # The opinion's own short caption where its running head gave one.
  o$long <- o$caption
  o$caption <- opinion_short_caption(doc$short, o$long) %||% o$long
  o$url <- pdf_url
  meta <- list(docket = paste(unlist(o$dkts), collapse = ", "), docket_id = dkt, term = as.character(term),
               caption = o$caption, decided = o$decided, decided_long = .op_long_date(o$decided),
               cite = doc$cite, pdf_url = pdf_url)
  mf <- tempfile(fileext = ".json"); write_json(meta, mf, auto_unbox = TRUE, null = "null")
  r <- py("render_opinion.py", c(json, mf, stem)); unlink(mf)
  bodyf <- paste0(stem, ".body.html")
  if (!r$ok || !file.exists(bodyf)) {
    failed <<- c(failed, dkt); message("  ", dkt, ": render failed: ", paste(tail(r$out, 2), collapse = " "))
    return(invisible(FALSE))
  }
  body <- paste(readLines(bodyf, warn = FALSE, encoding = "UTF-8"), collapse = "\n"); unlink(bodyf)
  render_opinion_page(site_dir, o, body, doc)
  entry <- list(href = opinion_href(term, dkt), url = o$listed, pdf = pdf_url, decided = o$decided, term = term, caption = o$caption,
                xv = ver$x, ov = ver$o, cite = doc$cite, layout = doc$layout, words = doc$qa$words, notes = doc$qa$notes,
                unmatched = length(doc$qa$notes_unmatched), lead = TRUE, class = o$class %||% "merits")
  idx[[dkt]] <<- entry
  for (d in setdiff(unlist(o$dkts), dkt)) idx[[d]] <<- list(href = entry$href, lead = FALSE, of = dkt)
  built <<- built + 1L
  if (built %% 20 == 0) { write_opinion_index(site_dir, idx); cat("  ", built, "built\n") }
  invisible(TRUE)
}
row_of <- function(i) { o <- work[i, ]; o$dkts <- work$dkts[i]; o$listed <- o$url; o }

# ---- decisions with a file of their own ---------------------------------------------
for (i in which(is.na(work$volume))) {
  o <- row_of(i); what <- needs(o)
  if (!nzchar(what)) next
  dir.create(dirname(stem_of(o)), recursive = TRUE, showWarnings = FALSE)
  if (what == "x") {
    if (fetched >= max_new || nzchar(stopped)) next
    tf <- download(o$url)
    if (is.null(tf)) next
    r <- py("extract_opinion.py", c(tf, "-o", paste0(stem_of(o), ".json")))
    unlink(tf)
    if (!r$ok || !file.exists(paste0(stem_of(o), ".json"))) {
      failed <- c(failed, o$dkt); message("  ", o$dkt, ": extraction failed: ", paste(tail(r$out, 2), collapse = " ")); next
    }
  }
  finish(o, idx[[o$dkt]]$pdf %||% o$url)
}

# ---- decisions inside a volume --------------------------------------------------------
# One download per volume, however many of its cases are wanted; each case is
# found in it by its docket number (extract_opinion.py --batch). A preliminary
# print the Court no longer serves falls back to the bound volume of the same
# number, as R/justices.R does for the lineups.
vol_rows <- which(!is.na(work$volume))
for (v in unique(work$volume[vol_rows])) {
  rows <- vol_rows[work$volume[vol_rows] == v]
  what <- vapply(rows, function(i) needs(row_of(i)), character(1))
  for (i in rows[what == "o"]) { o <- row_of(i); finish(o, idx[[o$dkt]]$pdf %||% o$url) }
  rows <- rows[what == "x"]
  if (!length(rows) || fetched >= max_new || nzchar(stopped)) next
  vol_no <- str_match(v, "preliminaryprint/(\\d{3})US")[1, 2]
  cands <- c(v, if (!is.na(vol_no)) sprintf("https://www.supremecourt.gov/opinions/boundvolumes/%sBV.pdf", vol_no))
  tf <- NULL; used <- NA_character_
  for (u in cands) { tf <- download(u, timeout = 600); if (!is.null(tf)) { used <- u; break } }
  if (is.null(tf)) { message("  volume ", basename(v), ": not served"); failed <- c(failed, work$dkt[rows]); next }
  jobs <- lapply(rows, function(i) {
    o <- row_of(i); dir.create(dirname(stem_of(o)), recursive = TRUE, showWarnings = FALSE)
    list(dockets = as.list(unlist(o$dkts)), hint = if (identical(used, v)) o$anchor else NULL, out = paste0(stem_of(o), ".json"))
  })
  jf <- tempfile(fileext = ".json"); write_json(jobs, jf, auto_unbox = TRUE, null = "null")
  r <- py("extract_opinion.py", c(tf, "--batch", jf)); unlink(c(tf, jf))
  res <- lapply(grep("^\\{", r$out, value = TRUE), function(l) tryCatch(fromJSON(l, simplifyVector = FALSE), error = function(e) NULL))
  ok_at <- list()
  for (x in res) if (!is.null(x) && isTRUE(x$ok)) ok_at[[x$out]] <- x$pages[[1]]
  cat(sprintf("  volume %s: %d of %d case(s) read\n", basename(used), length(ok_at), length(rows)))
  for (i in rows) {
    o <- row_of(i); json <- paste0(stem_of(o), ".json")
    if (is.null(ok_at[[json]])) {
      why <- Filter(function(x) !is.null(x) && identical(x$out, json), res)
      failed <- c(failed, o$dkt); message("  ", o$dkt, ": not read from ", basename(used), ": ", if (length(why)) why[[1]]$why %||% "" else "no result")
      next
    }
    finish(o, paste0(used, "#page=", ok_at[[json]]))
  }
}
write_opinion_index(site_dir, idx)
.op_write_json(skipped, opinion_skipped_path(site_dir))
n <- write_opinion_index_page(site_dir, idx)
n_opds <- write_opinion_opds(site_dir, idx)
cat("opinions: OPDS catalogue of", n_opds %||% 0, "EPUB(s)\n")
left <- sum(!work$dkt %in% c(names(idx), names(skipped)))
cat("opinions:", n_short, "opinion(s) relating to orders read and found under", OPINION_MIN_WORDS, "words; not published\n")
cat("opinions:", built, "built this run (", fetched, "download(s));", length(failed), "failed;", left, "still to build;",
    n %||% 0, "on the index\n")
if (length(failed)) cat("  failed:", paste(failed, collapse = ", "), "\n")
if (nzchar(stopped)) cat("  stopped early:", stopped, "\n")
gh <- Sys.getenv("GITHUB_STEP_SUMMARY", unset = "")
if (nzchar(gh)) cat(sprintf("Opinions: %d built, %d failed, %d still to build%s\n\n", built, length(failed), left,
                            if (nzchar(stopped)) paste0(" — stopped: ", stopped) else ""), file = gh, append = TRUE)
