# Regenerate data/amicus_stats.json from the committed archives, and print what
# moved. Same contract as make_counsel_stats.R: render_amicus.R checks the
# fingerprint and recomputes in-process on a mismatch, so a stale file cannot
# reach the page; this script refreshes the committed copy so the render does
# not pay for it.
#
#   Rscript .github/scripts/make_amicus_stats.R              # rewrite + diff
#   CHECK_ONLY=1 Rscript .github/scripts/make_amicus_stats.R # exit 1 if stale
#
# Env: AMICUS_STATS (default data/amicus_stats.json), CHECK_ONLY.

suppressPackageStartupMessages({library(tidyverse); library(jsonlite)})
source("R/amicus.R")

out <- Sys.getenv("AMICUS_STATS", unset = "data/amicus_stats.json")
check_only <- nzchar(Sys.getenv("CHECK_ONLY", ""))

paths <- amicus_input_paths()
if (!length(paths)) stop("no archives matched amicus_input_paths()")
cat("inputs:", paste(basename(paths), collapse = ", "), "\n")
if (!file.exists(AMICUS_REFRESH))
  cat("!! ", AMICUS_REFRESH, " is missing: no counsel boards, and the merits stage ",
      "of recent docket years is thin. Run refetch-amici.yml.\n", sep = "")

old <- if (file.exists(out)) fromJSON(out, simplifyVector = FALSE) else list()
want <- amicus_stats_fingerprint(paths)
have <- old$fingerprint %||% NA_character_
fresh <- identical(as.character(have), want)
cat("fingerprint  committed:", if (is.na(have)) "(none)" else substr(have, 1, 16),
    "| current:", substr(want, 1, 16), "|", if (fresh) "FRESH" else "STALE", "\n")

if (check_only) {
  if (fresh) { cat("Amicus stats are current.\n"); quit(status = 0) }
  cat("Amicus stats are STALE. Run: Rscript .github/scripts/make_amicus_stats.R\n")
  quit(status = 1)
}

cat("\nRecomputing...\n")
st <- compute_amicus_stats(paths)
st$generated <- format(Sys.Date())

cat("\ntotals:\n")
for (k in names(st$totals)) cat(sprintf("  %-18s %s\n", k, st$totals[[k]]))
cat("\nby docket year:\n")
print(as.data.frame(st$by_term), row.names = FALSE)

if (length(old$filers)) {
  cat("\n=== leaders: committed -> recomputed ===\n")
  for (b in c("filers", "door", "record", "states", "firms", "counsel")) {
    nm <- if (b == "states") "state" else if (b == "firms") "firm" else "name"
    o <- if (length(old[[b]])) old[[b]][[1]][[nm]] else "(none)"
    n <- if (nrow(st[[b]])) st[[b]][[nm]][[1]] else "(none)"
    cat(sprintf("  %-10s %-36s %s %s\n", b, o, if (identical(o, n)) "==" else "->",
                if (identical(o, n)) "" else n))
  }
}

write_json(st, out, auto_unbox = TRUE, digits = NA, pretty = TRUE)
cat("\nwrote", out, "\n")
cat("Commit it, or the render will pay for recomputing the same thing.\n")
