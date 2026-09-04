# Fetch the original-jurisdiction docket (22O###) by name: every docket
# cases/original.json knows, plus a short probe above the highest number so a new
# filing is found within a week. With no manifest yet -- the first run -- it scans
# the number space 1..ORIG_FIRST_SCAN_TO once (200 requests, ~2 min at the fleet's
# pace); after that it is ~70 requests a week. The numbering is sparse (44 of the
# first 200 numbers exist), which is why this is a named list and not a range
# fetch. See R/original_dockets.R and docs/original-jurisdiction.md.
#
# Writes $OUT_FILE (default cases-original.rds), which render_conferences.R and
# render_arguments.R pick up alongside the per-Term artifacts. Deliberately named
# so it does NOT match `^cases-\d{2}\.rds$`: the funnel and the counsel table load
# Terms, and an original action is not a petition.
#
# Never fails the run: a throttled fetch here must not cost the Term artifacts.
#
# Env: SITE_DIR (gh-pages checkout, default "site"), OUT_FILE.

suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse)
  library(httr2); library(jsonlite); library(pdftools); library(htmltools)
})

site <- Sys.getenv("SITE_DIR", unset = "site")
out <- Sys.getenv("OUT_FILE", unset = "cases-original.rds")

src <- readLines("R/scotus_dash_new.R"); src <- src[-grep("^scotus_dash\\(", src)]
eval(parse(text = paste(src, collapse = "\n")))
source("R/original_dockets.R")

dkts <- tryCatch(originals_to_fetch(site, live_only = FALSE), error = function(e) {
  cat("originals_to_fetch() failed:", conditionMessage(e), "-- nothing to fetch\n")
  character()
})
if (!length(dkts)) { cat("No original dockets to fetch.\n"); quit(status = 0) }

known <- length(read_originals(site))
cat("Naming", length(dkts), "original docket(s)",
    if (known) paste0(" (", known, " known, ", length(dkts) - known, " probed above the highest)")
    else " (no manifest yet: first scan of the number space)", "\n", sep = "")

ot <- tryCatch(fetch_cases(dkts), error = function(e) {
  cat("fetch failed:", conditionMessage(e), "\n"); NULL
})
if (is.null(ot) || !nrow(ot)) {
  cat("nothing resolved; no artifact this run\n")
  quit(status = 0)
}
# A probe past the highest docket is expected to 404; only a request that failed
# for another reason counts against the fetch.
cat("fetched:", nrow(ot), "of", length(dkts),
    "| unresolved:", attr(ot, "n_failed") %||% 0, "\n")
saveRDS(ot, out)
cat("Saved", nrow(ot), "case(s) to", out, "\n")
