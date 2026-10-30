# CI render: every Term-range page of the Counsel section, under
# $SITE_DIR/counsel/terms/<OTxxxx-OTyyyy>/ -- index.html (the Counsel Table) and
# amicus.html (Friends of the Court) for each contiguous range of Terms short of
# the full span, which is the section's own /counsel/ pages (render_counsel.R,
# render_amicus.R). See R/term_range.R for which Term a fact belongs to and how
# the floors scale.
#
# Computed from the committed extractions, not the archives: data/counsel_inputs.rds
# and data/amicus_briefs.rds, written by make_counsel_stats.R and
# make_amicus_stats.R under the same fingerprints as the stats. A stale or
# missing one is re-extracted in-process (~12 and ~13 minutes) rather than
# failing, for render_counsel.R's reason: a hard stop here would leave the
# previous, wrong pages live.
#
# Env: SITE_DIR (default "site").

suppressPackageStartupMessages({library(tidyverse); library(jsonlite)})
source("R/amicus.R")

site_dir <- Sys.getenv("SITE_DIR", unset = "site")
t0 <- Sys.time()
elapsed <- function() sprintf("%.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))

# A committed extraction if it is what this code would produce, else a fresh one.
load_or_extract <- function(path, want, extract, what) {
  have <- if (file.exists(path)) readRDS(path) else NULL
  fp <- attr(have, "fingerprint") %||% NA_character_
  if (!is.null(have) && identical(as.character(fp), want)) {
    cat(what, ": committed extraction is current (", substr(want, 1, 16), ")\n", sep = "")
    return(have)
  }
  cat("!! ", path, if (is.null(have)) " is missing" else " is STALE",
      ". Extracting in-process so the published pages are correct.\n", sep = "")
  cat("   Refresh the committed file with the matching make_*_stats.R script.\n")
  extract()
}

arch <- counsel_archive_paths()
apaths <- amicus_input_paths()
cin <- load_or_extract(COUNSEL_INPUTS_PATH, counsel_stats_fingerprint(arch),
                       function() counsel_inputs(arch), "Counsel Table")
bin <- load_or_extract(AMICUS_BRIEFS_PATH, amicus_stats_fingerprint(apaths),
                       function() amicus_briefs(apaths), "Amicus page")
expected <- load_counsel_expected()

cspan <- counsel_term_span(cin)
aspan <- amicus_term_span(bin)
cat(sprintf("spans: Counsel Table OT%d-OT%d, amicus page OT%d-OT%d\n",
            cspan[1], cspan[2], aspan[1], aspan[2]))

# Each range short of the page's own span; the span itself is the canonical page.
partial <- function(span) term_ranges(span[1], span[2]) |>
  filter(!(from == span[1] & to == span[2]))
dir_of <- function(from, to) file.path(site_dir, "counsel", "terms", term_range_slug(from, to))

cr <- partial(cspan)
for (i in seq_len(nrow(cr))) {
  st <- counsel_stats_range(cin, cr$from[i], cr$to[i], expected = expected)
  st$totals$other_from <- aspan[1]; st$totals$other_to <- aspan[2]
  render_counsel_page(st, dir_of(cr$from[i], cr$to[i]))
}
cat(sprintf("Counsel Table: %d range pages (%s)\n", nrow(cr), elapsed()))

ar <- partial(aspan)
for (i in seq_len(nrow(ar))) {
  st <- amicus_stats_range(bin, ar$from[i], ar$to[i])
  st$totals$other_from <- cspan[1]; st$totals$other_to <- cspan[2]
  render_amicus_page(st, dir_of(ar$from[i], ar$to[i]))
}
cat(sprintf("Amicus page: %d range pages (%s)\n", nrow(ar), elapsed()))
