# Backfill: the historical data-raw/ot_*.rds snapshots were captured before their
# Terms' decisions came down, so argued cases show as "Argued" rather than
# "Decided". Re-fetch the granted (merits) cases in the stale docket terms
# (17-24) from the live JSON API so their decisions + opinions appear, and save a
# refresh layer that render_arguments.R prefers over the stale snapshots.
#
# Run this on a FRESH IP (a CI runner) -- ~500 sequential fetches trip the
# supremecourt.gov cumulative rate limit from an already-warmed local IP.
suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse)
  library(httr2); library(jsonlite); library(pdftools); library(htmltools)
})
# Load scotus_dash_new.R's fetch functions without its bottom render() call.
src <- readLines("R/scotus_dash_new.R"); src <- src[-grep("^scotus_dash\\(", src)]
eval(parse(text = paste(src, collapse = "\n")))
source("R/cert_funnel.R")   # classify_petitions

hist <- list.files("data-raw", "^ot_20(1[7-9]|2[0-4])\\.rds$", full.names = TRUE) |>
  map(readRDS) |> bind_rows()
gr <- classify_petitions(hist) |> filter(outcome == "granted") |>
  distinct(dkt) |> pull(dkt)
cat("granted (merits) dockets in OT17-24 to refresh:", length(gr), "\n")

fresh <- fetch_cases(gr)
cat("fetched:", nrow(fresh), "of", length(gr),
    "| failed:", attr(fresh, "n_failed") %||% 0, "\n")
saveRDS(fresh, "data-raw/arg_refresh.rds")
cat("saved data-raw/arg_refresh.rds\n")
