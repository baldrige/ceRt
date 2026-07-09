# CI backfill: full-fetch one or more terms at their CURRENT docket state,
# combine them, and render the conferences on/after MIN_CONF_DATE into
# $SITE_DIR/conferences -- then rebuild the index over the whole directory so the
# backfilled conferences merge with the existing archive.
#
# A conference's case list spans the term AND the preceding term(s), because
# older petitions still pending get relisted. So OT2025 conferences need fresh
# OT2024 + OT2025 data (scotus.rds is too stale to supply the 24- relists).
#
# Env: SITE_DIR (default "site"), TERM_YEARS (comma-separated, default "24,25"),
#      MIN_CONF_DATE (default "2025-08-01" -- render OT2025-term conferences only,
#      so we don't overwrite the archive's older, more-complete conferences).

suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse)
  library(httr2); library(jsonlite); library(pdftools); library(htmltools)
})

site_dir <- Sys.getenv("SITE_DIR", unset = "site")
terms <- str_split(Sys.getenv("TERM_YEARS", unset = "24,25"), ",")[[1]] |> str_trim()
terms <- terms[terms != ""]
min_conf <- as.Date(Sys.getenv("MIN_CONF_DATE", unset = "2025-08-01"))
conf_dir <- file.path(site_dir, "conferences")
dir.create(conf_dir, recursive = TRUE, showWarnings = FALSE)

src <- readLines("R/scotus_dash_new.R")
src <- src[-grep("^scotus_dash\\(", src)]
eval(parse(text = paste(src, collapse = "\n")))
source("R/conference_dash.R")

# Fetch each term full, tracking degradation across the whole set.
all_cases <- list()
tot_failed <- 0L
tot_attempted <- 0L
for (t in terms) {
  cat("Full-fetching OT", t, "term...\n")
  ot <- get_scotus_term(t)
  tot_failed <- tot_failed + (attr(ot, "n_failed") %||% 0L)
  tot_attempted <- tot_attempted + (attr(ot, "n_attempted") %||% nrow(ot))
  all_cases[[t]] <- ot
  cat("  OT", t, ":", nrow(ot), "cases\n")
}
cat("Total:", sum(map_int(all_cases, nrow)), "cases | unresolved:",
    tot_failed, "/", tot_attempted, "\n")

# Refuse to publish a throttle-degraded fetch as conference data.
if (tot_failed > 0.1 * max(tot_attempted, 1L)) {
  cat("Fetch degraded by throttling; not publishing partial conference data.\n")
  quit(status = 1)
}

combined <- bind_rows(all_cases)
# distribution_no is computed across the full combined history (so a case
# relisted from an earlier term shows the correct relist count), then we keep
# only the OT2025-range conferences to render.
dist <- conference_distributions(combined) |> filter(conf_date >= min_conf)
dates <- dist |> distinct(conf_date) |> arrange(conf_date) |> pull(conf_date)
cat("Rendering", length(dates), "conference(s) on/after", format(min_conf),
    "to", conf_dir, "\n")
for (i in seq_along(dates)) conference_dash(dist, dates[i], out_dir = conf_dir)
conference_index(conf_dir)
cat("Done.\n")
