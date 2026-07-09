# CI backfill: full-fetch an entire term and render its conference reports into
# $SITE_DIR/conferences, then rebuild the conference index over the whole
# directory so a backfilled term merges with the existing archive.
#
# Env: SITE_DIR (default "site"), TERM_YEAR (default "25").

suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse)
  library(httr2); library(jsonlite); library(pdftools); library(htmltools)
})

site_dir <- Sys.getenv("SITE_DIR", unset = "site")
term <- Sys.getenv("TERM_YEAR", unset = "25")
conf_dir <- file.path(site_dir, "conferences")
dir.create(conf_dir, recursive = TRUE, showWarnings = FALSE)

src <- readLines("R/scotus_dash_new.R")
src <- src[-grep("^scotus_dash\\(", src)]
eval(parse(text = paste(src, collapse = "\n")))
source("R/conference_dash.R")

cat("Full-fetching OT", term, "term (thousands of dockets -- this is slow)...\n")
ot <- get_scotus_term(term)
cat("Cases:", nrow(ot),
    "| unresolved:", attr(ot, "n_failed") %||% 0,
    "/", attr(ot, "n_attempted") %||% nrow(ot), "\n")

# Refuse to publish a partial term (throttled fetch) as conference data.
if (fetch_is_degraded(ot)) {
  cat("Fetch degraded by throttling; not publishing partial conference data.\n")
  quit(status = 1)
}

dist <- conference_distributions(ot)
dates <- dist |> distinct(conf_date) |> arrange(conf_date) |> pull(conf_date)
cat("Rendering", length(dates), "conference(s) for OT", term, "to", conf_dir, "\n")
for (i in seq_along(dates)) conference_dash(dist, dates[i], out_dir = conf_dir)
conference_index(conf_dir)
cat("Done.\n")
