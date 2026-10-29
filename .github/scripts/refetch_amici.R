# Backfill for the amicus page (R/amicus.R): re-fetch every docket in the
# committed archives that carries an amicus brief, and save the result as
# data-raw/amici_refresh.rds.
#
# Two things the archives cannot give, and one fetch gives both:
#
#   * WHO WROTE THE BRIEF. The docket JSON's "Other" block names each amicus
#     party with its counsel of record and firm. The fetcher dropped that block
#     until build_amici() (R/scotus_dash_new.R); the HTML-era archives kept the
#     party and the attorney but never the firm.
#   * THE MERITS STAGE. A Term archive is taken before its granted cases are
#     briefed, so ot_2024.rds holds 111 respondent-side merits amici where a
#     normal Term has ~400. A fresh fetch has every brief and the judgment.
#
# ~3,100 dockets at SCOTUS_FETCH_RPS (default 2/s): about half an hour. Run it on
# a CI runner (refetch-amici.yml), not locally -- see refetch_argued.R.
#
# Refuses to save a degraded fetch (>10% of dockets lost to throttling), and
# keeps the previous refresh's copy of any docket this run failed to fetch, so a
# re-dispatch only ever adds.
suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse)
  library(httr2); library(jsonlite); library(pdftools); library(htmltools)
})
source("R/scotus_dash_new.R")   # fetch_cases, build_amici
source("R/amicus.R")            # amicus_input_paths, AMICUS_ENTRY_RX

out <- AMICUS_REFRESH
inputs <- setdiff(amicus_input_paths(), out)
years <- amicus_docket_years(counsel_archive_paths())
cat("inputs:", paste(basename(inputs), collapse = ", "),
    "| docket years", min(years), "-", max(years), "\n")

want <- map(inputs, function(p) {
  x <- readRDS(p)
  yr <- 2000L + suppressWarnings(as.integer(str_match(x$dkt, "^(\\d{2})-")[, 2]))
  x <- x[funnel_case_type(x$dkt) %in% PETITION_TYPES & yr %in% years, , drop = FALSE]
  has <- map_lgl(x$events, function(e) is.data.frame(e) &&
    any(str_detect(coalesce(e[["Proceedings and Orders"]], ""), AMICUS_ENTRY_RX)))
  x$dkt[has]
}) |> unlist() |> unique() |> sort()
cat("dockets carrying an amicus brief:", length(want), "\n")

# Optional cap for a trial run: AMICI_LIMIT=50.
lim <- suppressWarnings(as.integer(Sys.getenv("AMICI_LIMIT", "")))
if (!is.na(lim) && lim > 0) { want <- head(want, lim); cat("limited to", lim, "\n") }

fresh <- fetch_cases(want)
n_failed <- attr(fresh, "n_failed") %||% 0L
cat("fetched:", nrow(fresh), "of", length(want), "| failed:", n_failed, "\n")
if (length(want) && n_failed / length(want) > 0.1)
  stop("degraded fetch: ", n_failed, " of ", length(want),
       " dockets failed -- not saving. Re-dispatch from a fresh runner.")

# Keep the previous refresh's copy of anything this run did not fetch.
if (file.exists(out)) {
  old <- readRDS(out)
  keep <- old[!old$dkt %in% fresh$dkt, , drop = FALSE]
  if (nrow(keep)) cat("kept", nrow(keep), "docket(s) from the previous refresh\n")
  fresh <- bind_rows(fresh, keep)
}
fresh <- arrange(fresh, dkt)
n_amici <- sum(map_int(fresh$amici, ~ if (is.data.frame(.x)) nrow(.x) else 0L))
cat("amicus attorney rows:", n_amici, "\n")
saveRDS(fresh, out)
cat("saved", out, "\n")
