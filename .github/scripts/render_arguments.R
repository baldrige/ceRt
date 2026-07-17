# CI render: build the Oral Argument Navigator into $SITE_DIR/arguments.
# Combines the historical archive (data-raw/ot_*.rds) with the freshly fetched
# per-term artifacts ($CASES_DIR/cases-NN.rds) so every argument Term is complete
# -- a sitting draws cases from multiple docket-number terms. Question Presented
# is attached read-only from the conference QP cache (no petition fetching here).
#
# Env: SITE_DIR (default "site"), CASES_DIR (default "cases").

suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse); library(lubridate)
  library(jsonlite); library(htmltools); library(pdftools)
})

site_dir <- Sys.getenv("SITE_DIR", unset = "site")
cases_dir <- Sys.getenv("CASES_DIR", unset = "cases")
arg_dir <- file.path(site_dir, "arguments")
dir.create(arg_dir, recursive = TRUE, showWarnings = FALSE)

source("R/qp_extract.R")
source("R/argument_nav.R")   # sources cert_funnel.R + page_style.R
source("R/cert_model.R")     # score_case (docket-page forecast)
source("R/docket_page.R")    # render_dockets_for

# Combine, deduping by docket with a freshness preference:
#   arg_refresh.rds (re-fetched historical grants, freshest for OT17-23)
#   > cases-NN.rds  (JSON artifacts, freshest for the fetched terms)
#   > ot_*.rds      (historical snapshots, may predate a Term's decisions)
refresh <- list.files("data-raw", pattern = "^arg_refresh\\.rds$", full.names = TRUE)
art <- list.files(cases_dir, pattern = "^cases-\\d{2}\\.rds$",
                  full.names = TRUE, recursive = TRUE)
hist <- list.files("data-raw", pattern = "^ot_\\d+\\.rds$", full.names = TRUE)
files <- c(refresh, art, hist)
if (length(files) == 0) stop("no case data found (artifacts or data-raw)")
cat("Loading", length(files), "file(s):", paste(basename(files), collapse = ", "), "\n")
combined <- files |> map(readRDS) |> bind_rows() |> distinct(dkt, .keep_all = TRUE)
cat("Combined cases:", nrow(combined), "\n")

# Build the argument table once; attach QP from the Court's dedicated
# Questions-Presented PDFs (clean text, present for granted cases across all
# Terms). Cached in the argument section's own qp_cache.json. QP_MAX_NEW>0 fetches
# uncached PDFs this run (they are small text PDFs, no OCR); default 0 stays
# read-only. The publish runner is fresh, so a full backfill is safe there.
tbl <- build_argument_table(combined)
cat("Argued/scheduled grants:", nrow(tbl), "\n")
qp_max <- as.integer(Sys.getenv("QP_MAX_NEW", unset = "0"))
cache <- file.path(arg_dir, "qp_cache.json")
if (nrow(tbl) > 0) {
  qp_raw <- resolve_granted_qps(tbl$dkt, cache_path = cache, max_new = qp_max)
  tbl <- tbl |> mutate(qp = ifelse(qp_raw == "-", NA_character_, qp_details(qp_raw)))
  cat("QP attached for", sum(!is.na(tbl$qp)), "of", nrow(tbl), "cases",
      if (qp_max > 0) paste0(" (fetched up to ", qp_max, " new)") else " (cache-only)", "\n")
}

terms <- render_argument_nav(out_dir = arg_dir, tbl = tbl)
# Typographic (smart) quotes across the per-Term argument pages (the index is
# already smartened by styled_index_page). smarten_html skips <style>/<script>/
# tags, so the static gt tables' data and CSS are untouched but prose is fixed.
for (f in list.files(arg_dir, pattern = "^arg_\\d+\\.html$", full.names = TRUE)) {
  writeLines(enc2utf8(smarten_html(paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                                          collapse = "\n"))), f, useBytes = TRUE)
}
cat("Rendered argument Terms:", paste(map_chr(terms, ~ term_label(.x - 2000L)), collapse = ", "), "\n")

# Docket pages for the granted cases (freshest data -> carry the opinion links).
render_dockets_for(combined |> filter(dkt %in% tbl$dkt), site_dir)
cat("Done.\n")
