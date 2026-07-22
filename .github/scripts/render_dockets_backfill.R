# Re-render docket pages for whole terms from pre-fetched case snapshots, applying
# the CURRENT page template (v8 brief-cover dots and every later page improvement).
# Reads the cases-*.rds produced by fetch_term.R (one per term), renders each into
# $SITE_DIR/cases via render_dockets_for(), and lets that helper maintain the
# shared manifest, search index, and QP/Rule-10 embedding. Rendering is
# incremental: the PAGE_TEMPLATE_VERSION bump makes every stale page rewrite while
# already-current pages skip, so a re-dispatch resumes cheaply. The gh-pages commit
# is done by the workflow, not here.
#
# Env: SITE_DIR (default "site"), RDS_DIR (default "." -- dir holding cases-*.rds).

suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse)
  library(httr2); library(jsonlite); library(pdftools); library(htmltools)
})

site_dir <- Sys.getenv("SITE_DIR", unset = "site")
rds_dir  <- Sys.getenv("RDS_DIR",  unset = ".")

# Same setup as build_dashboards.R: load the dashboard functions without tripping
# the script's bottom scotus_dash() call, then the model + page modules.
src <- readLines("R/scotus_dash_new.R")
src <- src[-grep("^scotus_dash\\(", src)]
eval(parse(text = paste(src, collapse = "\n")))
source("R/cert_model.R")
source("R/petition_signals.R")   # resolve_petition_signals (unused here, sourced for deps)
source("R/argument_nav.R")       # classify_argument (docket-page lifecycle)
source("R/docket_page.R")        # render_dockets_for

files <- sort(list.files(rds_dir, pattern = "^cases-.*\\.rds$", full.names = TRUE))
if (length(files) == 0) {
  cat("No cases-*.rds snapshots found in", rds_dir, "-- nothing to render.\n")
  quit(status = 0)
}
cat("Re-rendering", length(files), "term snapshot(s):",
    paste(basename(files), collapse = ", "), "\n")

total_cases <- 0L
for (f in files) {
  ot <- tryCatch(readRDS(f), error = function(e) { message("read failed: ", f); NULL })
  if (is.null(ot) || !nrow(ot)) { cat("  skip (empty):", basename(f), "\n"); next }
  cat("== ", basename(f), ":", nrow(ot), "cases ==\n")
  render_dockets_for(ot, site_dir)     # v8 render into site/cases (+ manifest/search)
  total_cases <- total_cases + nrow(ot)
}
cat("Done. Processed", total_cases, "cases across", length(files), "term(s).\n")
