# Regenerate data/funnel_baselines.json from data-raw/ot_*.rds, and print exactly
# what moved.
#
# Generated, not hand-maintained. train_cert_model.R already carries this lesson
# for the model metrics table -- "the published table drifted out of sync with the
# shipped artifacts and stayed wrong for months" -- and the funnel baselines then
# went and did the same thing: generated 2026-07-10, invalidated 2026-07-25 by the
# CVSG relist fix, publishing a relist count 2.5x too high until 2026-08.
#
# render_funnel.R now checks a fingerprint of the classifier's own function bodies
# plus the archives, and recomputes in-process on a mismatch, so a stale file can
# no longer reach the page. This script is how you refresh the committed copy so
# the weekly render does not pay the ~4 minutes.
#
#   Rscript .github/scripts/make_baselines.R          # rewrite + diff
#   CHECK_ONLY=1 Rscript .github/scripts/make_baselines.R   # exit 1 if stale
#
# Env: BASELINES (default data/funnel_baselines.json), CHECK_ONLY.

suppressPackageStartupMessages({library(tidyverse); library(jsonlite)})
source("R/cert_funnel.R")

out <- Sys.getenv("BASELINES", unset = "data/funnel_baselines.json")
check_only <- nzchar(Sys.getenv("CHECK_ONLY", ""))

arch <- sort(Sys.glob("data-raw/ot_*.rds"))
if (!length(arch)) stop("no archives matched data-raw/ot_*.rds")
named <- setNames(arch, str_extract(basename(arch), "\\d{2}(?=\\.rds)"))
cat("archives:", paste(names(named), collapse = ", "), "\n")

old <- if (file.exists(out)) fromJSON(out, simplifyVector = FALSE) else list()
want <- funnel_baseline_fingerprint(arch)
have <- old$fingerprint %||% NA_character_
fresh <- identical(as.character(have), want)

cat("fingerprint  committed:", if (is.na(have)) "(none)" else substr(have, 1, 16),
    "| current:", substr(want, 1, 16), "|", if (fresh) "FRESH" else "STALE", "\n")

if (check_only) {
  if (fresh) { cat("Baselines are current.\n"); quit(status = 0) }
  cat("Baselines are STALE. Run: Rscript .github/scripts/make_baselines.R\n")
  quit(status = 1)
}

cat("\nRecomputing (~4 min)...\n")
b <- compute_funnel_baselines(named)
b$generated <- format(Sys.Date())

cat("\npooled_terms:", paste(b$pooled_terms, collapse = ", "),
    "| excluded:", paste(b$excluded_terms, collapse = ", "), "\n")

if (length(old$pooled)) {
  o <- old$pooled$total[[1]]; n <- as.list(b$pooled$total[1, ])
  cat("\n=== pooled totals: committed -> recomputed ===\n")
  moved <- FALSE
  for (k in intersect(names(o), names(n))) {
    ov <- o[[k]]; nv <- n[[k]]
    if (is.numeric(ov) && is.numeric(nv) && !identical(as.numeric(ov), as.numeric(nv))) {
      moved <- TRUE
      cat(sprintf("  %-18s %9s -> %9s  (%+d, %+.1f%%)\n", k,
                  format(ov, big.mark = ","), format(nv, big.mark = ","),
                  as.integer(nv - ov), 100 * (nv - ov) / max(ov, 1)))
    }
  }
  if (!moved) cat("  (nothing moved)\n")
}

write_json(b, out, auto_unbox = TRUE, digits = NA, pretty = TRUE)
cat("\nwrote", out, "\n")
cat("Commit it, or the weekly render will pay ~4 min recomputing the same thing.\n")
