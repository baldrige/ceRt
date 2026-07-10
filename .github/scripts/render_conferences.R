# Combine the per-term case artifacts in $CASES_DIR, then render the conferences
# on/after $MIN_CONF_DATE into $SITE_DIR/conferences and rebuild the index over
# the whole directory (so the refreshed conferences merge with the archive).
# Each case's Question Presented is resolved through a persistent, incremental
# cache (site/conferences/qp_cache.json) so the petition PDFs are only ever
# fetched once; QP_MAX_NEW caps how many uncached petitions are pulled per run.
#
# Env: SITE_DIR (default "site"), CASES_DIR (default "cases"), MIN_CONF_DATE,
#      QP_MAX_NEW (default 600).

suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse)
  library(jsonlite); library(htmltools); library(pdftools)
})

site_dir <- Sys.getenv("SITE_DIR", unset = "site")
cases_dir <- Sys.getenv("CASES_DIR", unset = "cases")
min_conf <- as.Date(Sys.getenv("MIN_CONF_DATE"))
if (is.na(min_conf)) stop("MIN_CONF_DATE not set / invalid")
qp_max_new <- as.integer(Sys.getenv("QP_MAX_NEW", unset = "600"))
conf_dir <- file.path(site_dir, "conferences")
dir.create(conf_dir, recursive = TRUE, showWarnings = FALSE)

source("R/qp_extract.R")
source("R/conference_dash.R")

files <- list.files(cases_dir, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE)
if (length(files) == 0) stop("no case artifacts found in ", cases_dir)
cat("Loading", length(files), "term file(s):", paste(basename(files), collapse = ", "), "\n")
combined <- files |> map(readRDS) |> bind_rows()
cat("Combined cases:", nrow(combined), "\n")

# distribution_no is computed across the full combined history, then we keep only
# the target conference-date range to render.
dist <- conference_distributions(combined) |> filter(conf_date >= min_conf)
dates <- dist |> distinct(conf_date) |> arrange(conf_date) |> pull(conf_date)

# Resolve the QP for each distinct distributed docket (cache-backed).
uniq <- dist |> distinct(dkt, .keep_all = TRUE)
qp_raw <- resolve_qps(uniq$dkt, uniq$petition_url,
                      cache_path = file.path(conf_dir, "qp_cache.json"),
                      max_new = qp_max_new)
qp_map <- setNames(
  ifelse(qp_raw == "-", NA_character_, qp_details(qp_raw)),
  uniq$dkt
)
cat("QP resolved:", sum(!is.na(qp_map)), "of", length(qp_map), "distinct dockets\n")

cat("Rendering", length(dates), "conference(s) on/after", format(min_conf), "\n")
for (i in seq_along(dates)) {
  conference_dash(dist, dates[i], out_dir = conf_dir, qp_map = qp_map)
}
conference_index(conf_dir)
cat("Done.\n")
