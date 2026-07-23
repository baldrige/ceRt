# Fetch only the dockets whose PUBLISHED page is behind the current template --
# the throttle-casualties of a full backfill, which the per-term full fetch never
# resolved. Two ways in:
#
#   * SCAN mode (default): scan $SITE_DIR/cases for one term's stale pages. A page
#     is stale if it PREDATES the brief-cover template (a bare "<li>" -- pre-v8) OR
#     carries a template-version stamp OLDER than the current PAGE_TEMPLATE_VERSION
#     (a page a version bump left behind: it keeps its old <meta name='tv'> stamp).
#     Unstamped pages that DO have dots are grandfathered (every pre-v12 page is
#     unstamped) -- they are only reachable via the bare-<li> rule or DOCKETS.
#   * EXPLICIT mode: if $DOCKETS is set (comma/space-separated), fetch exactly
#     those docket numbers, across any term, bypassing the scan. This is the
#     surgical targeting path -- e.g. a handful of dockets lost to throttling in
#     every prior pass -- and is also what a future version-aware scan can feed in.
#
# Fetches just the resulting docket numbers (far fewer than a whole term, so far
# less throttling) into $OUT_FILE for the shared render step.
#
# Env: TERM_YEAR (e.g. "17"; required for scan mode), DOCKETS (explicit list;
#      when set, TERM_YEAR is ignored), SITE_DIR (gh-pages checkout, default
#      "site"), OUT_FILE (default "cases-<term>.rds").

suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse)
  library(httr2); library(jsonlite); library(pdftools); library(htmltools)
})

site <- Sys.getenv("SITE_DIR", unset = "site")

src <- readLines("R/scotus_dash_new.R"); src <- src[-grep("^scotus_dash\\(", src)]
eval(parse(text = paste(src, collapse = "\n")))

# Save whatever resolved -- a partial fill is fine (the render is incremental and
# the workflow is re-dispatchable, so remaining stragglers converge on a re-run).
fetch_and_save <- function(dkts, out, label) {
  ot <- fetch_cases(dkts)
  cat(label, "fetched:", nrow(ot), "| unresolved:",
      attr(ot, "n_failed") %||% 0, "/", length(dkts), "\n")
  if (nrow(ot)) { saveRDS(ot, out); cat("Saved", nrow(ot), "cases to", out, "\n") } else
    cat("no cases resolved (throttled); no artifact this run\n")
}

# ---- EXPLICIT mode -----------------------------------------------------------
dockets_env <- Sys.getenv("DOCKETS")
if (nzchar(dockets_env)) {
  out  <- Sys.getenv("OUT_FILE", unset = "cases-manual.rds")
  dkts <- str_squish(unlist(str_split(dockets_env, "[,[:space:]]+")))
  dkts <- unique(dkts[nzchar(dkts)])
  cat("Explicit docket list: ", length(dkts), " docket(s): ",
      paste(dkts, collapse = ", "), "\n", sep = "")
  if (!length(dkts)) { cat("DOCKETS set but empty after parsing; nothing to do\n"); quit(status = 0) }
  fetch_and_save(dkts, out, "explicit")
  quit(status = 0)
}

# ---- SCAN mode ---------------------------------------------------------------
term <- Sys.getenv("TERM_YEAR"); if (term == "") stop("TERM_YEAR not set (and no DOCKETS given)")
out  <- Sys.getenv("OUT_FILE", unset = paste0("cases-", term, ".rds"))

# Current template version (the integer in PAGE_TEMPLATE_VERSION <- "vNN"), read
# straight from the source so the scanner and the renderer can never disagree.
cur_tv <- as.integer(str_match(
  grep("^PAGE_TEMPLATE_VERSION <-", readLines("R/docket_page.R"), value = TRUE)[1],
  "v([0-9]+)")[, 2])
if (is.na(cur_tv)) stop("could not read PAGE_TEMPLATE_VERSION from R/docket_page.R")

cases_dir <- file.path(site, "cases")
# Match paid/IFP ("17-123") AND application ("17A191") docket pages -- the "A"
# applications were the throttle-casualties a dash-only pattern silently skipped.
files <- list.files(cases_dir, pattern = paste0("^", term, "[-A][0-9]+\\.html$"), full.names = TRUE)
is_stale <- function(f) {
  x <- paste(readLines(f, warn = FALSE), collapse = " ")
  # (1) Pre-template page: the FIRST timeline entry is a bare "<li>"; the template
  # always tags it (proc/--dot). Anchor to the timeline so a multi-question QP's
  # bare list items don't read as a stale page.
  if (grepl("class='timeline'><li>", x, fixed = TRUE)) return(TRUE)
  # (2) Version-behind page: a stamp older than the current template. A page a
  # bump left behind keeps its old <meta name='tv' content='vNN'>. Unstamped
  # pages (NA) are grandfathered -- not flagged here (every pre-v12 page lacks the
  # stamp; flagging them would re-render the whole back-catalog).
  tv <- str_match(x, "name='tv' content='v([0-9]+)'")[, 2]
  !is.na(tv) && as.integer(tv) < cur_tv
}
stale <- files[vapply(files, is_stale, logical(1))]
dkts  <- str_match(basename(stale), "^([0-9]+[-A][0-9]+)\\.html$")[, 2]
dkts  <- dkts[!is.na(dkts)]
cat("OT", term, ": ", length(dkts), " stale docket page(s) to refetch (current template v",
    cur_tv, ")\n", sep = "")
if (!length(dkts)) { cat("nothing to fill for OT", term, "\n"); quit(status = 0) }

fetch_and_save(dkts, out, paste0("OT", term))
