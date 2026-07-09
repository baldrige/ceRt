suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse); library(htmltools)
})
source("R/conference_dash.R")

# Build conference reports for every term in the combined case database.
cat("Loading case database (all terms)...\n")
cases <- readRDS("data/scotus.rds")
cat("cases in DB:", nrow(cases), "\n")

dates <- conference_dashboards(cases) # -> ~/public_html/conferences/
cat("Done. Rendered", length(dates), "conference date(s), from",
    format(min(dates)), "to", format(max(dates)), "\n")
