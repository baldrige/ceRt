# Regenerate data/counsel_stats.json from data-raw/ot_*.rds, and print what moved.
# Also writes data/counsel_inputs.rds, the petition and argument rows the
# Term-range pages are computed from.
#
# Generated, not hand-maintained, for the reason make_baselines.R carries in full:
# a committed summary of a classifier's output is indistinguishable from a fresh
# one by inspection, and data/funnel_baselines.json went stale for two weeks
# publishing a relist count 2.5x too high. render_counsel.R checks the same
# fingerprint and recomputes in-process on a mismatch, so a stale file cannot
# reach the page; this script is how you refresh the committed copy so the render
# does not pay the ~4 minutes.
#
#   Rscript .github/scripts/make_counsel_stats.R            # rewrite + diff
#   CHECK_ONLY=1 Rscript .github/scripts/make_counsel_stats.R   # exit 1 if stale
#
# Env: COUNSEL_STATS (default data/counsel_stats.json), CHECK_ONLY.

suppressPackageStartupMessages({library(tidyverse); library(jsonlite)})
source("R/counsel_table.R")

out <- Sys.getenv("COUNSEL_STATS", unset = "data/counsel_stats.json")
check_only <- nzchar(Sys.getenv("CHECK_ONLY", ""))

arch <- counsel_archive_paths()
if (!length(arch)) stop("no archives matched counsel_archive_paths()")
cat("inputs:", paste(basename(arch), collapse = ", "), "->", counsel_terms(arch), "\n")

old <- if (file.exists(out)) fromJSON(out, simplifyVector = FALSE) else list()
want <- counsel_stats_fingerprint(arch)
have <- old$fingerprint %||% NA_character_
fresh <- identical(as.character(have), want)
cat("fingerprint  committed:", if (is.na(have)) "(none)" else substr(have, 1, 16),
    "| current:", substr(want, 1, 16), "|", if (fresh) "FRESH" else "STALE", "\n")

if (check_only) {
  if (fresh) { cat("Counsel stats are current.\n"); quit(status = 0) }
  cat("Counsel stats are STALE. Run: Rscript .github/scripts/make_counsel_stats.R\n")
  quit(status = 1)
}

cat("\nRecomputing (~12 min)...\n")
inputs <- counsel_inputs(arch)
st <- compute_counsel_stats(arch, inputs)
st$generated <- format(Sys.Date())
# The petition and argument rows every Term-range page is computed from
# (render_counsel_ranges.R), under the same fingerprint as the stats.
saveRDS(structure(inputs, fingerprint = want), COUNSEL_INPUTS_PATH, compress = "xz")
cat("wrote", COUNSEL_INPUTS_PATH, "\n")

cat("\ntotals:\n")
for (k in names(st$totals)) cat(sprintf("  %-18s %s\n", k, st$totals[[k]]))
cat("\ngrant rate by petitioner:\n")
print(as.data.frame(st$by_side |>
  mutate(rate = sprintf("%.1f%%", 100 * rate))), row.names = FALSE)
if (nrow(st$arg_rates)) {
  cat("\nargument win rate by side (the reason those boards are split):\n")
  print(as.data.frame(st$arg_rates |>
    mutate(rate = sprintf("%.1f%%", 100 * rate))), row.names = FALSE)
}

if (nrow(st$grants_expected)) {
  cat(sprintf("\nagainst expectation (%d qualifying; %d granted vs %.1f expected; spread %.2f):\n",
              st$totals$exp_qualifying, st$totals$exp_granted, st$totals$exp_expected,
              st$totals$exp_spread))
  print(as.data.frame(head(st$grants_expected, 15) |>
    transmute(name, cases = exp_n, granted = exp_granted,
              expected = round(exp_expected, 1), ratio = round(exp_ratio, 2),
              shrunk = round(exp_shrunk, 2), z = round(exp_z, 1))), row.names = FALSE)
} else cat("\n(no", COUNSEL_EXPECTED_PATH, "-- the against-expectation board is omitted)\n")

# What actually moved at the top of each board -- the part a reader would notice.
if (length(old$filings)) {
  cat("\n=== leaders: committed -> recomputed ===\n")
  for (b in c("filings", "relists", "grants_private", "grants_government", "grants_expected",
              "arguments", "arg_petitioner", "arg_respondent")) {
    o <- if (length(old[[b]])) old[[b]][[1]]$name else "(none)"
    n <- if (nrow(st[[b]])) st[[b]]$name[[1]] else "(none)"
    cat(sprintf("  %-18s %-24s %s %s\n", b, o, if (identical(o, n)) "==" else "->",
                if (identical(o, n)) "" else n))
  }
}

write_json(st, out, auto_unbox = TRUE, digits = NA, pretty = TRUE)
cat("\nwrote", out, "\n")
cat("Commit both, or the render will pay ~12 min recomputing the same thing.\n")
