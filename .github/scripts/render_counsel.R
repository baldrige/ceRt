# CI render: build The Counsel Table from the committed data/counsel_stats.json,
# writing $SITE_DIR/counsel/index.html.
#
# No fetch and no network. Everything on this page comes from the committed term
# archives, so the page only changes when the archives or the classifier do.
#
# Env: SITE_DIR (default "site"), COUNSEL_STATS (default data/counsel_stats.json).

suppressPackageStartupMessages({library(tidyverse); library(jsonlite)})
source("R/counsel_table.R")

site_dir <- Sys.getenv("SITE_DIR", unset = "site")
stats_path <- Sys.getenv("COUNSEL_STATS", unset = "data/counsel_stats.json")

arch <- sort(Sys.glob("data-raw/ot_*.rds"))
stats <- if (file.exists(stats_path)) fromJSON(stats_path) else NULL

# Is the committed file still what this code would produce?
#
# Recomputes on a mismatch rather than failing, for the reason render_funnel.R
# spells out: this step is continue-on-error so a counsel problem cannot block a
# publish, which means a hard stop here would be SILENT in exactly the case that
# matters -- and would leave the previous, wrong page live. Four minutes buys a
# correct page; the loud log line is what gets the committed file refreshed.
want <- if (length(arch)) tryCatch(counsel_stats_fingerprint(arch),
                                   error = function(e) NA_character_) else NA_character_
have <- if (is.null(stats)) NA_character_ else (stats$fingerprint %||% NA_character_)
if (is.null(stats)) {
  if (!length(arch)) stop("no data/counsel_stats.json and no archives to build it from")
  cat("!! ", stats_path, " is missing. Computing in-process (~4 min).\n", sep = "")
  stats <- compute_counsel_stats(arch)
} else if (!is.na(want) && !identical(as.character(have), want)) {
  cat("!! ", stats_path, " is STALE.\n", sep = "")
  cat("   committed fingerprint:", if (is.na(have)) "(none -- predates the check)"
      else substr(have, 1, 16), "\n")
  cat("   this code would produce:", substr(want, 1, 16), "\n")
  cat("   Recomputing in-process so the published page is correct (~4 min).\n")
  cat("   Refresh the committed file: Rscript .github/scripts/make_counsel_stats.R\n")
  stats <- compute_counsel_stats(arch)
} else if (!is.na(want)) {
  cat("Counsel fingerprint matches (", substr(want, 1, 16), ")\n", sep = "")
}

# jsonlite round-trips a data frame as a data frame; the renderer wants tibbles,
# and `totals` back as a plain list rather than a one-row frame.
for (b in c("filings", "relists", "grants_private", "grants_government", "by_side"))
  if (!is.null(stats[[b]])) stats[[b]] <- as_tibble(stats[[b]])
if (is.data.frame(stats$totals)) stats$totals <- as.list(stats$totals[1, ])

p <- render_counsel_page(stats, file.path(site_dir, "counsel"))
cat("Rendered", p, "\n")
cat(sprintf("  %d advocates over %s cases | boards: %d private, %d government\n",
            stats$totals$qualifying, format(stats$totals$cases, big.mark = ","),
            stats$totals$board_private, stats$totals$board_government))
