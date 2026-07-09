suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse)
  library(httr2); library(jsonlite); library(pdftools)
})

# Load the JSON-based dashboard functions without triggering the bottom call.
src <- readLines("R/scotus_dash_new.R")
last_call <- grep("^scotus_dash\\(", src)
if (length(last_call) > 0) src <- src[-last_call]
eval(parse(text = paste(src, collapse = "\n")))

# Fetch the full recent OT2026 docket once, then render every docketed date.
cat("Fetching OT 2026 docket via JSON API...\n")
ot26 <- get_scotus_update("26")
cat("Cases fetched:", nrow(ot26),
    "| unresolved:", attr(ot26, "n_failed") %||% 0,
    "/", attr(ot26, "n_attempted") %||% nrow(ot26), "\n")

# Abort before touching any file if the fetch was throttle-degraded, so we never
# publish incomplete dashboards over good ones.
if (fetch_is_degraded(ot26)) {
  stop("Fetch degraded by throttling (", attr(ot26, "n_failed"), "/",
       attr(ot26, "n_attempted"), " dockets unresolved). Not rendering. ",
       "Re-run later from a cold state.")
}

# Serve the cached fetch to scotus_dash so we don't re-hit the API per date.
get_scotus_update <- function(year) ot26

dates <- ot26 |> filter(!is.na(date)) |> distinct(date) |> arrange(date) |> pull(date)
cat("Rendering", length(dates), "date(s):", paste(format(dates), collapse = ", "), "\n")
for (d in dates) {
  d <- as.Date(d, origin = "1970-01-01")
  cat("  ->", format(d), "\n")
  scotus_dash(range = d, year = "26") # default out_dir = ~/public_html/dashboards
}
cat("Done.\n")
