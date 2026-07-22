# Fetch only the dockets of one term whose PUBLISHED page still predates the
# brief-cover template -- the throttle-casualties of the full backfill, which the
# per-term full fetch never resolved. A brief-cover (v8+) page renders every
# timeline entry as "<li class='proc'>" or "<li style='--dot:...'>", so a bare
# "<li>" marks a stale page. Scans $SITE_DIR/cases for that term's stale pages and
# fetches just those docket numbers (far fewer than a whole term, so far less
# throttling), saving them to $OUT_FILE for the shared render step.
#
# Env: TERM_YEAR (e.g. "17"), SITE_DIR (gh-pages checkout, default "site"),
#      OUT_FILE (default "cases-<term>.rds").

suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse)
  library(httr2); library(jsonlite); library(pdftools); library(htmltools)
})

term <- Sys.getenv("TERM_YEAR"); if (term == "") stop("TERM_YEAR not set")
site <- Sys.getenv("SITE_DIR", unset = "site")
out  <- Sys.getenv("OUT_FILE", unset = paste0("cases-", term, ".rds"))

src <- readLines("R/scotus_dash_new.R"); src <- src[-grep("^scotus_dash\\(", src)]
eval(parse(text = paste(src, collapse = "\n")))

cases_dir <- file.path(site, "cases")
# Match paid/IFP ("17-123") AND application ("17A191") docket pages -- the "A"
# applications were the throttle-casualties a dash-only pattern silently skipped.
files <- list.files(cases_dir, pattern = paste0("^", term, "[-A][0-9]+\\.html$"), full.names = TRUE)
is_stale <- function(f) {
  x <- paste(readLines(f, warn = FALSE), collapse = " ")
  # A pre-template page renders the FIRST timeline entry as a bare "<li>"; the
  # template always tags it (proc/--dot). Anchor to the timeline so a multi-
  # question QP's bare list items don't read as a stale page.
  grepl("class='timeline'><li>", x, fixed = TRUE)
}
stale <- files[vapply(files, is_stale, logical(1))]
dkts  <- str_match(basename(stale), "^([0-9]+[-A][0-9]+)\\.html$")[, 2]
dkts  <- dkts[!is.na(dkts)]
cat("OT", term, ": ", length(dkts), " stale docket page(s) to refetch\n", sep = "")
if (!length(dkts)) { cat("nothing to fill for OT", term, "\n"); quit(status = 0) }

ot <- fetch_cases(dkts)
cat("OT", term, "fetched:", nrow(ot), "| unresolved:",
    attr(ot, "n_failed") %||% 0, "/", length(dkts), "\n")
# Save whatever resolved -- a partial fill is fine (the render is incremental and
# the workflow is re-dispatchable, so remaining stragglers converge on a re-run).
if (nrow(ot)) { saveRDS(ot, out); cat("Saved", nrow(ot), "cases to", out, "\n") } else
  cat("no cases resolved (throttled); no artifact this run\n")
