# Train the certiorari-grant models from the historical term archives.
#
# Produces three calibrated logistic models:
#   data/cert_model_baseline.rds  -- P(grant), petition-stage (structural)
#                                    features only, for the daily dashboards.
#   data/cert_model_enhanced.rds  -- P(grant), adds the conference-stage process
#                                    signals (relists, cert-stage amicus, CVSG,
#                                    response events), for the conference reports.
#   data/cert_model_gvr.rds       -- P(GVR) companion (GVR vs granted|denied),
#                                    same enhanced features. Held petitions carry
#                                    low grant odds but sustained GVR risk, so the
#                                    conference reports pair this with the grant
#                                    model to show an honest disposition.
#
# The labeled corpus is assembled from data-raw/ot_*.rds (full terms, incl.
# denials). Assembly is the slow step (~minutes over ~49k petitions), so it is
# cached; delete data-raw/cert_corpus.rds to force a rebuild.
#
# Env: TERMS_GLOB (default "data-raw/ot_*.rds"), CORPUS_CACHE, OUT_DIR (default
# "data"). Run from a machine that has the term archives; no network needed.

suppressPackageStartupMessages({library(tidyverse)})
source("R/cert_funnel.R")   # classify_petitions + label machinery
source("R/cert_model.R")

terms_glob <- Sys.getenv("TERMS_GLOB", "data-raw/ot_*.rds")
cache      <- Sys.getenv("CORPUS_CACHE", "data-raw/cert_corpus.rds")
out_dir    <- Sys.getenv("OUT_DIR", "data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

paths <- Sys.glob(terms_glob)
if (length(paths) == 0) stop("No term archives matched ", terms_glob)

if (file.exists(cache)) {
  corpus <- readRDS(cache)
  message("Loaded cached corpus (", nrow(corpus), " rows) from ", cache)
} else {
  corpus <- assemble_corpus(paths)
  saveRDS(corpus, cache)
  message("Cached corpus to ", cache)
}

report <- function(tag, m) {
  mc <- m$metrics_calibrated
  cat(sprintf("%-9s  n=%d pos=%d (%.2f%%)  |  out-of-time AUC=%.3f  AP=%.3f  Brier=%.4f\n",
              tag, m$metrics$n, m$metrics$n_pos, 100 * m$base_rate,
              mc$auc, mc$ap, mc$brier))
}

cat("=== training disposition models ===\n")
baseline <- fit_cert_model(corpus, BASELINE_FEATURES, target = "grant")
enhanced <- fit_cert_model(corpus, ENHANCED_FEATURES, target = "grant")
# Companion GVR-risk model: relabel the same corpus (GVR vs granted|denied).
gvr <- fit_cert_model(set_target(corpus, "gvr"), ENHANCED_FEATURES, target = "gvr")
report("baseline", baseline); report("enhanced", enhanced); report("gvr", gvr)

# Save slim artifacts (drop the per-row LOTO frame kept for evaluation).
slim <- function(m) { m$loto <- NULL; m }
files <- c(cert_model_baseline.rds = list(baseline),
           cert_model_enhanced.rds = list(enhanced),
           cert_model_gvr.rds      = list(gvr))
for (nm in names(files)) saveRDS(slim(files[[nm]]), file.path(out_dir, nm))
cat("Saved models to ", out_dir, "/cert_model_{baseline,enhanced,gvr}.rds\n", sep = "")

# Sizes, so a bloated artifact (embedded training data) is caught early.
for (f in names(files))
  cat(sprintf("  %-28s %6.1f KB\n", f,
              file.size(file.path(out_dir, f)) / 1024))
