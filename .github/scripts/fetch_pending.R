# Fetch the live stragglers by name: dockets that cases/pending.json says are
# still undisposed and that this run's Term matrix will NOT range-fetch.
#
# Measured median 8 dockets, max 13 (OT2017-OT2024), against ~5,000 for the extra
# Term that would otherwise be needed to see them. See R/pending_dockets.R for why
# targeting is exact for old Terms and impossible for the current one.
#
# Writes $OUT_FILE (default cases-pending.rds), which render_conferences.R picks
# up alongside the per-Term artifacts. Deliberately named so it does NOT match
# `^cases-\d{2}\.rds$`: the funnel's live-Term statistics must not be perturbed by
# a handful of old stragglers.
#
# Never fails the run. An empty or absent cache means there is nothing to name,
# which is the normal state on a fresh site, and a throttled fetch here must not
# cost the Term artifacts that succeeded.
#
# Env: SITE_DIR (gh-pages checkout, default "site"), WINDOW_TERMS (comma-separated
#      two-digit Terms being range-fetched), OUT_FILE.

suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse)
  library(httr2); library(jsonlite); library(pdftools); library(htmltools)
})

site <- Sys.getenv("SITE_DIR", unset = "site")
out <- Sys.getenv("OUT_FILE", unset = "cases-pending.rds")
window <- Sys.getenv("WINDOW_TERMS", unset = "")

src <- readLines("R/scotus_dash_new.R"); src <- src[-grep("^scotus_dash\\(", src)]
eval(parse(text = paste(src, collapse = "\n")))
source("R/pending_dockets.R")

wt <- trimws(unlist(strsplit(window, "[,[:space:]]+")))
wt <- wt[nzchar(wt)]
cat("Range-fetched Terms this run:", if (length(wt)) paste(wt, collapse = ", ")
    else "(none declared)", "\n")

dkts <- tryCatch(pending_to_fetch(site, wt), error = function(e) {
  cat("pending_to_fetch() failed:", conditionMessage(e), "-- nothing to fetch\n")
  character()
})

if (!length(dkts)) {
  cat("No out-of-window pending dockets to fetch",
      if (!file.exists(file.path(site, PENDING_CACHE)))
        " (no cases/pending.json yet -- it is written by the render step, so the\n",
      "  first run after this ships is expected to be empty).\n", sep = "")
  quit(status = 0)
}

cat("Naming", length(dkts), "out-of-window pending docket(s):\n  ",
    paste(dkts, collapse = ", "), "\n", sep = "")

ot <- tryCatch(fetch_cases(dkts), error = function(e) {
  cat("fetch failed:", conditionMessage(e), "\n"); NULL
})

if (is.null(ot) || !nrow(ot)) {
  cat("nothing resolved; no artifact this run (the Term artifacts are unaffected)\n")
  quit(status = 0)
}
cat("fetched:", nrow(ot), "of", length(dkts),
    "| unresolved:", attr(ot, "n_failed") %||% 0, "\n")
saveRDS(ot, out)
cat("Saved", nrow(ot), "case(s) to", out, "\n")
