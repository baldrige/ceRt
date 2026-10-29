# CI render: build the amicus page from the committed data/amicus_stats.json,
# writing $SITE_DIR/counsel/amicus.html.
#
# No fetch and no network: everything comes from the committed archives and the
# amicus refresh layer, so the page changes only when they or the classifier do.
# A stale or missing stats file is recomputed in-process rather than failing --
# render_counsel.R gives the reason: this step is continue-on-error, so a hard
# stop would be silent and leave the previous, wrong page live.
#
# Env: SITE_DIR (default "site"), AMICUS_STATS (default data/amicus_stats.json).

suppressPackageStartupMessages({library(tidyverse); library(jsonlite)})
source("R/amicus.R")

site_dir <- Sys.getenv("SITE_DIR", unset = "site")
stats_path <- Sys.getenv("AMICUS_STATS", unset = "data/amicus_stats.json")

paths <- amicus_input_paths()
stats <- if (file.exists(stats_path)) fromJSON(stats_path) else NULL
want <- if (length(paths)) tryCatch(amicus_stats_fingerprint(paths),
                                    error = function(e) NA_character_) else NA_character_
have <- if (is.null(stats)) NA_character_ else (stats$fingerprint %||% NA_character_)
if (is.null(stats)) {
  if (!length(paths)) stop("no data/amicus_stats.json and no archives to build it from")
  cat("!! ", stats_path, " is missing. Computing in-process.\n", sep = "")
  stats <- compute_amicus_stats(paths)
} else if (!is.na(want) && !identical(as.character(have), want)) {
  cat("!! ", stats_path, " is STALE.\n", sep = "")
  cat("   committed fingerprint:", if (is.na(have)) "(none)" else substr(have, 1, 16), "\n")
  cat("   this code would produce:", substr(want, 1, 16), "\n")
  cat("   Recomputing in-process so the published page is correct.\n")
  cat("   Refresh the committed file: Rscript .github/scripts/make_amicus_stats.R\n")
  stats <- compute_amicus_stats(paths)
} else if (!is.na(want)) {
  cat("Amicus fingerprint matches (", substr(want, 1, 16), ")\n", sep = "")
}

# jsonlite round-trips a data frame as a data frame; `totals` and `us` come back
# as one-row frames and the renderer wants plain lists.
for (b in c("totals", "us"))
  if (is.data.frame(stats[[b]])) stats[[b]] <- as.list(stats[[b]][1, ])

p <- render_amicus_page(stats, file.path(site_dir, "counsel"))
cat("Rendered", p, "\n")
cat(sprintf("  %s briefs in %s cases from %s filers | counsel matched on %s of %s\n",
            format(stats$totals$briefs, big.mark = ","),
            format(stats$totals$cases, big.mark = ","),
            format(stats$totals$filers, big.mark = ","),
            format(stats$totals$matched %||% 0, big.mark = ","),
            format(stats$totals$block_briefs %||% 0, big.mark = ",")))
if (!file.exists(AMICUS_REFRESH))
  cat("!! ", AMICUS_REFRESH, " is missing: the counsel boards are omitted. ",
      "Run the refetch-amici.yml workflow.\n", sep = "")
