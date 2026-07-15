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

# Combine artifacts (JSON, richer) + historical archive, deduping by docket and
# preferring the JSON record where a docket appears in both.
art <- list.files(cases_dir, pattern = "^cases-\\d{2}\\.rds$",
                  full.names = TRUE, recursive = TRUE)
hist <- list.files("data-raw", pattern = "^ot_\\d+\\.rds$", full.names = TRUE)
files <- c(art, hist)
if (length(files) == 0) stop("no case data found (artifacts or data-raw)")
cat("Loading", length(files), "file(s):", paste(basename(files), collapse = ", "), "\n")
combined <- files |> map(readRDS) |> bind_rows() |> distinct(dkt, .keep_all = TRUE)
cat("Combined cases:", nrow(combined), "\n")

# Build the argument table once; attach QP from the conference cache if present.
tbl <- build_argument_table(combined)
cat("Argued/scheduled grants:", nrow(tbl), "\n")
cache <- file.path(site_dir, "conferences", "qp_cache.json")
if (file.exists(cache) && nrow(tbl) > 0) {
  qp_raw <- resolve_qps(tbl$dkt, tbl$petition_url, cache_path = cache, max_new = 0)
  tbl <- tbl |> mutate(qp = ifelse(qp_raw == "-", NA_character_, qp_details(qp_raw)))
  cat("QP attached for", sum(!is.na(tbl$qp)), "of", nrow(tbl), "cases (cache-only)\n")
}

terms <- render_argument_nav(out_dir = arg_dir, tbl = tbl)
cat("Rendered argument Terms:", paste(map_chr(terms, ~ term_label(.x - 2000L)), collapse = ", "), "\n")
cat("Done.\n")
