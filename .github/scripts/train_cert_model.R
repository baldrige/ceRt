# Train the certiorari models from the historical term archives.
#
# Produces four artifacts in $OUT_DIR:
#   cert_model_baseline.rds    P(granted), petition-stage features only. Daily
#                              dashboards and case pages.
#   cert_model_conference.rds  Four-way competing risks over {granted, gvr,
#                              denied, relisted} AT a conference. Conference
#                              reports, "Granted here" / "GVR here".
#   cert_model_enhanced.rds    P(granted EVER), fitted on the at-risk panel.
#                              Conference reports, "Granted ever".
#   cert_model_gvr.rds         P(GVR ever), same panel.
#   counsel_index.rds          Advocate track record. NOT optional: the baseline
#                              model cannot be scored without it.
#
# Two training frames, and the difference matters:
#   * the DISPOSITION corpus  -- one row per petition, features at its outcome.
#     Right for the petition-stage model.
#   * the AT-RISK panel       -- one row per (petition, conference), features as
#     of that conference. Right for anything served at a conference, because it
#     is the unit the conference renderer actually produces. Fitting the
#     conference tier on the disposition corpus made it answer "given the Court
#     acts today, is it a grant?", which is not what the page claimed, and left
#     the published calibration slope at 0.58.
#
# Both are cached (assembly is the slow step, ~minutes over ~49k petitions).
# Delete the caches to force a rebuild.
#
# Env: TERMS_GLOB (default "data-raw/ot_*.rds"), CORPUS_CACHE, PANEL_CACHE,
# OUT_DIR (default "data"). No network needed.

suppressPackageStartupMessages({library(tidyverse); library(nnet)})
source("R/cert_funnel.R")   # classify_petitions + label machinery
source("R/cert_model.R")

terms_glob  <- Sys.getenv("TERMS_GLOB", "data-raw/ot_*.rds")
cache       <- Sys.getenv("CORPUS_CACHE", "data-raw/cert_corpus.rds")
panel_cache <- Sys.getenv("PANEL_CACHE", "data-raw/cert_panel.rds")
out_dir     <- Sys.getenv("OUT_DIR", "data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

paths <- Sys.glob(terms_glob)
if (length(paths) == 0) stop("No term archives matched ", terms_glob)

cached <- function(path, build) {
  if (file.exists(path)) { message("Loaded cache ", path); readRDS(path) }
  else { x <- build(); saveRDS(x, path); message("Cached ", path); x }
}
corpus <- cached(cache,       function() assemble_corpus(paths))
panel  <- cached(panel_cache, function() assemble_at_risk(paths))
message("Corpus ", nrow(corpus), " rows | panel ", nrow(panel), " rows")

# Term completeness is measured on the corpus and passed explicitly: the panel
# drops pending petitions by construction, so every term looks complete there and
# the censoring guard would silently pass.
complete <- complete_terms(corpus)
message("Complete terms: ", paste(complete, collapse = ", "))

# ---- counsel index (a deployed artifact, built before anything is fitted) -----
# A silently empty index would rate every advocate first-time and quietly undo
# the feature, with no error anywhere -- exactly how elite_counsel shipped dead.
counsel_ix <- build_counsel_index(corpus)
stopifnot(nrow(counsel_ix) > 0)
corpus$counsel_tier <- counsel_tier(corpus$counsel_key, corpus$date, counsel_ix)
panel$counsel_tier  <- counsel_tier(panel$counsel_key,  panel$date,  counsel_ix)
stopifnot(dplyr::n_distinct(corpus$counsel_tier) > 1)
saveRDS(counsel_ix, file.path(out_dir, "counsel_index.rds"))
cat("Counsel index:", nrow(counsel_ix), "advocates\n")
print(table(corpus$counsel_tier[corpus$type == "paid"]))

# ---- relist odds (authoritative; a hand-maintained copy went stale twice) -----
cat("\n=== relist odds (paid, resolved) ===\n")
print(as.data.frame(
  corpus |>
    dplyr::filter(type == "paid", !is.na(label)) |>
    dplyr::mutate(bucket = relist_bucket(n_relists)) |>
    dplyr::group_by(bucket) |>
    dplyr::summarise(n = dplyr::n(), granted = sum(label),
                     rate = sprintf("%.1f%%", 100 * mean(label)), .groups = "drop")),
  row.names = FALSE)

# ---- fit ---------------------------------------------------------------------
cat("\n=== fitting ===\n")
baseline <- fit_cert_model(corpus, BASELINE_FEATURES, target = "grant", complete = complete)
enhanced <- fit_cert_model(panel,  ENHANCED_FEATURES, target = "grant", complete = complete)
gvr      <- fit_cert_model(set_target(panel, "gvr"), ENHANCED_FEATURES,
                           target = "gvr", complete = complete)
conference <- fit_conference_model(panel)

slim <- function(m) { m$loto <- NULL; m }
files <- list(cert_model_baseline.rds   = slim(baseline),
              cert_model_enhanced.rds   = slim(enhanced),
              cert_model_gvr.rds        = slim(gvr),
              cert_model_conference.rds = conference)
for (nm in names(files)) saveRDS(files[[nm]], file.path(out_dir, nm))

# ---- report ------------------------------------------------------------------
# Generated, not hand-maintained: the published table drifted out of sync with
# the shipped artifacts and stayed wrong for months.
cat("\n=== paste into docs/cert_model.md ===\n")
cat("| Model | Target | AUC | AP | Brier | Base rate |\n|---|---|---|---|---|---|\n")
for (x in list(list(baseline, "Baseline"), list(enhanced, "At-risk"), list(gvr, "GVR"))) {
  m <- x[[1]]; mc <- m$metrics_calibrated
  cat(sprintf("| %s | %s | %.3f | %.3f | %.4f | %.2f%% |\n",
              x[[2]], m$target, mc$auc, mc$ap, mc$brier, 100 * m$base_rate))
}
cat("\nConference model: ", conference$n, " rows; per-conference outcome mix\n", sep = "")
print(round(conference$rates, 4))

cat("\nArtifacts in ", out_dir, ":\n", sep = "")
for (nm in c(names(files), "counsel_index.rds"))
  cat(sprintf("  %-30s %6.1f KB\n", nm, file.size(file.path(out_dir, nm)) / 1024))

# Final gate: the models must survive the same validation the renderers apply, or
# CI has produced artifacts the site will refuse to load.
loaded <- load_cert_models(out_dir)
missing <- setdiff(c("baseline", "enhanced", "gvr", "conference", "counsel_index"),
                   names(loaded))
if (length(missing))
  stop("load_cert_models() rejected: ", paste(missing, collapse = ", "))
cat("\nload_cert_models() accepts all artifacts.\n")
