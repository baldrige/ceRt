# Combine the per-term case artifacts in $CASES_DIR, then render the conferences
# on/after $MIN_CONF_DATE into $SITE_DIR/conferences and rebuild the index over
# the whole directory (so the refreshed conferences merge with the archive).
#
# Env: SITE_DIR (default "site"), CASES_DIR (default "cases"), MIN_CONF_DATE.

suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse)
  library(jsonlite); library(htmltools)
})

site_dir <- Sys.getenv("SITE_DIR", unset = "site")
cases_dir <- Sys.getenv("CASES_DIR", unset = "cases")
min_conf <- as.Date(Sys.getenv("MIN_CONF_DATE"))
if (is.na(min_conf)) stop("MIN_CONF_DATE not set / invalid")
conf_dir <- file.path(site_dir, "conferences")
dir.create(conf_dir, recursive = TRUE, showWarnings = FALSE)

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
cat("Rendering", length(dates), "conference(s) on/after", format(min_conf), "\n")
for (i in seq_along(dates)) conference_dash(dist, dates[i], out_dir = conf_dir)
conference_index(conf_dir)
cat("Done.\n")
