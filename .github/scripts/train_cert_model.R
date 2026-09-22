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

# No key may END in a generational suffix. counsel_key() took the last token
# blindly until 2026-08-13, so "Robert L. Sirianni Jr." and 14 other Roberts all
# keyed as "robert jr" -- 75 petitions, 0 grants, read by counsel_tier() as one
# veteran who had never won. This fails the build rather than shipping that
# again, and it also catches a stale CORPUS CACHE: counsel_key is computed into
# data-raw/cert_corpus.rds, so a retrain that reuses the cache would quietly
# refit on the old keys.
.suffix_keys <- grep(paste0(" (", paste(COUNSEL_SUFFIXES, collapse = "|"), ")$"),
                     counsel_ix$counsel_key, value = TRUE)
if (length(.suffix_keys))
  stop("counsel index holds ", length(.suffix_keys), " key(s) ending in a ",
       "generational suffix, e.g. ", paste(head(.suffix_keys, 5), collapse = ", "),
       ". counsel_key() should strip these -- if the code is current, delete ",
       "the corpus cache (", cache, ") and retrain: it stores counsel_key.",
       call. = FALSE)
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
# The at-risk grant model carries the Rule 10 cues (ATRISK_FEATURES); the GVR
# model carries gvr_ask (GVR_FEATURES) -- see the note by their definitions.
enhanced <- fit_cert_model(panel,  ATRISK_FEATURES, target = "grant", complete = complete)
gvr      <- fit_cert_model(set_target(panel, "gvr"), GVR_FEATURES,
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
# the shipped artifacts and stayed wrong for months. Then it was printed for
# someone to paste, the paste was skipped, and the doc sat 0.026 AP off the
# shipped artifact (2026-09-22). Now it is written into docs/cert_model.md,
# between its metrics:begin / metrics:end markers.
metrics_rows <- vapply(list(list(baseline, "Baseline", "P(grant)"),
                            list(enhanced, "At-risk", "P(grant ever)"),
                            list(gvr, "GVR", "P(GVR ever)")), function(x) {
  m <- x[[1]]; mc <- m$metrics_calibrated
  sprintf("| %s | %s | %.3f | %.3f | %.4f | %.2f%% |",
          x[[2]], x[[3]], mc$auc, mc$ap, mc$brier, 100 * m$base_rate)
}, character(1))
terms <- sort(unique(corpus$term))
metrics_block <- c(
  "| Model | Target | AUC | AP | Brier | Base rate |", "|---|---|---|---|---|---|",
  metrics_rows, "",
  sprintf("Retrained %s on OT20%s–20%s; base rates from the complete Terms (%s–%s).",
          format(Sys.Date()), min(terms), max(terms), min(complete), max(complete)))
cat("\n=== docs/cert_model.md metrics ===\n", paste(metrics_block, collapse = "\n"), "\n", sep = "")
doc_path <- "docs/cert_model.md"
if (file.exists(doc_path)) {
  doc <- readLines(doc_path, encoding = "UTF-8", warn = FALSE)
  b <- grep("^<!-- metrics:begin -->", doc); e <- grep("^<!-- metrics:end -->", doc)
  if (length(b) != 1L || length(e) != 1L || e <= b)
    stop("docs/cert_model.md: expected one metrics:begin / metrics:end marker pair")
  writeLines(enc2utf8(c(doc[seq_len(b)], metrics_block, doc[e:length(doc)])), doc_path, useBytes = TRUE)
  cat("Rewrote the metrics block in", doc_path, "\n")
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

# ---- regenerate the documents that describe these artifacts ------------------
# The methods note and the coefficient reference are built FROM the artifacts, so
# retraining without rebuilding them leaves the site describing models that are
# no longer serving. That is not hypothetical: the reference sat in the repo
# built from 2026-07-25 artifacts while 2026-07-30 models served forecasts, and
# nothing caught it -- both documents were hand-run, and the hand stopped.
#
# Each runs in its own Rscript process rather than source()-d here. They define
# dozens of names between them (esc, fnum, b, e, g, cm ...) that would collide
# with each other and with this script, and a failure in one should be a
# reported non-zero exit, not a half-populated environment.
#
# Skipped unless the artifacts landed in the deployed location: a training run
# pointed at a scratch OUT_DIR is an experiment, and it must not rewrite the
# committed documents to describe models nobody is serving.
docs_ok <- tryCatch(
  normalizePath(out_dir, mustWork = TRUE) == normalizePath("data", mustWork = TRUE),
  error = function(e) FALSE)

if (!nzchar(Sys.getenv("SKIP_MODEL_DOCS", "")) && docs_ok) {
  for (script in c("docs/make_methods_note.R", "docs/make_model_reference.R")) {
    cat("\nRegenerating from", script, "\n")
    # Inherited, not passed through system2(env=): on Linux that argument is a
    # shell prefix and shQuote() made the path literally "'data'"; on Windows it
    # is spliced in as a command argument and Rscript tried to run a file named
    # "MODEL_DIR=data". Either way this step had failed on every retrain since
    # it was added, and the docs it guards went stale anyway (found 2026-09-22).
    Sys.setenv(MODEL_DIR = out_dir)
    st <- system2("Rscript", script)
    if (!identical(st, 0L))
      stop(script, " failed (exit ", st, "); the artifacts are written but the ",
           "documents describing them are now stale.")
  }
  cat("\nMethods note and coefficient reference regenerated from ", out_dir, ".\n", sep = "")
} else if (!docs_ok) {
  cat("\nOUT_DIR is ", out_dir, ", not data/ -- documents NOT regenerated.\n",
      "They describe the deployed artifacts; rerun with OUT_DIR=data to refresh them.\n", sep = "")
} else {
  cat("\nSKIP_MODEL_DOCS set -- methods note and reference not regenerated.\n")
}
