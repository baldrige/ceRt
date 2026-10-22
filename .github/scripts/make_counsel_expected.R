# Regenerate data/counsel_expected.json: a grant probability for every resolved
# paid petition in the training corpus, from a model that knows nothing about the
# petitioner's counsel. The Counsel Table's "against expectation" board compares
# each advocate's grants with the sum of these.
#
# The model is the petition-stage baseline (BASELINE_FEATURES) with the counsel
# track record removed, and nothing else changed. Every petition is scored OUT OF
# TERM -- by a model fitted on the other Terms, then Platt-calibrated by a map
# fitted on the other Terms (loto_predict / loto_platt) -- so an advocate's own
# grants never taught the model what their cases were worth. Scored in-sample,
# a docket's own label is in the fit, and a prolific winner's expectation is
# pulled up toward what they actually got.
#
# Committed, not computed by the render: it needs the assembled corpus
# (data-raw/cert_corpus.rds, ~minutes to build and not tracked) and eight model
# fits. render_counsel.R reads the file, and the counsel fingerprint digests it,
# so refreshing it here makes data/counsel_stats.json stale until
# make_counsel_stats.R is re-run.
#
#   Rscript .github/scripts/make_counsel_expected.R
#
# Env: TERMS_GLOB (default data-raw/ot_*.rds), CORPUS_CACHE (default
# data-raw/cert_corpus.rds), COUNSEL_EXPECTED (default data/counsel_expected.json).

suppressPackageStartupMessages({library(tidyverse); library(jsonlite)})
source("R/counsel_table.R")

out   <- Sys.getenv("COUNSEL_EXPECTED", unset = COUNSEL_EXPECTED_PATH)
cache <- Sys.getenv("CORPUS_CACHE", unset = "data-raw/cert_corpus.rds")
paths <- Sys.glob(Sys.getenv("TERMS_GLOB", unset = "data-raw/ot_*.rds"))
if (!length(paths)) stop("no term archives matched")

corpus <- if (file.exists(cache)) { message("Loaded cache ", cache); readRDS(cache) } else {
  x <- assemble_corpus(paths); saveRDS(x, cache); message("Cached ", cache); x }

stopifnot(!any(COUNSEL_FEATURES %in% COUNSEL_BLIND_FEATURES))
mf <- model_frame(corpus, COUNSEL_BLIND_FEATURES)
raw <- loto_predict(mf, COUNSEL_BLIND_FEATURES)
p <- loto_platt(mf$label, raw, mf$term_year)
stopifnot(!anyNA(p))

# For the record: what dropping counsel cost the model. Measured on the same
# frame, the same way.
full_mf <- model_frame(mutate(corpus, counsel_tier = counsel_tier(
  counsel_key, date, build_counsel_index(corpus))), BASELINE_FEATURES)
full_p <- loto_platt(full_mf$label, loto_predict(full_mf, BASELINE_FEATURES),
                     full_mf$term_year)

res <- list(
  generated = format(Sys.Date()),
  features = COUNSEL_BLIND_FEATURES,
  terms = sort(unique(mf$term_year)),
  n = nrow(mf),
  granted = sum(mf$label),
  expected = round(sum(p), 1),
  auc = round(auc_roc(mf$label, p), 4),
  auc_with_counsel = round(auc_roc(full_mf$label, full_p), 4),
  p = as.list(setNames(signif(p, 4), mf$dkt)))

cat(sprintf("scored %s petitions, OT%d-OT%d | %d granted, %.1f expected\n",
            format(res$n, big.mark = ","), min(res$terms), max(res$terms),
            res$granted, res$expected))
cat(sprintf("AUC counsel-blind %.3f | with counsel %.3f\n", res$auc, res$auc_with_counsel))
print(mf |> mutate(p = p) |> group_by(term_year) |>
        summarise(n = n(), granted = sum(label), expected = round(sum(p), 1)))

write_json(res, out, auto_unbox = TRUE, digits = NA)
cat("wrote", out, sprintf("(%.0f KB)\n", file.size(out) / 1024))
cat("Now re-run: Rscript .github/scripts/make_counsel_stats.R\n")
