# Regenerate data/counsel_expected.json: a grant probability for every resolved
# paid petition in the training corpus, and in the two snapshots the Counsel
# Table also reads (OT2025, and the OT2024 petitions decided after their archive
# was taken), from a model that knows nothing about the petitioner's counsel. The Counsel Table's "against expectation" board compares
# each advocate's grants with the sum of these.
#
# The model is the petition-stage baseline (BASELINE_FEATURES) with the counsel
# track record removed, and nothing else changed. Every corpus petition is scored
# OUT OF TERM -- by a model fitted on the other Terms, then Platt-calibrated by a map
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

# ---- the petitions the training corpus cannot label --------------------------
# OT2025 lives in snapshot_25.rds, outside the ot_* glob on purpose (it would
# re-base the Funnel and the cert model), and ~300 OT2024 petitions were still
# pending when ot_2024.rds was taken -- their outcomes are in
# snapshot_refresh.rds. Neither is in `mf`, so both are scored by the model
# fitted on EVERY corpus Term, calibrated by a map fitted on all the
# out-of-term predictions above. They are outside the fit, so this is as
# out-of-sample as the LOTO scores. Refresh first: a docket in both files keeps
# its re-fetched copy.
#
# Their petition-text cues come from the same enrichment layer, extended to
# these archives (enrich-petitions.yml terms "2025" and "refresh"). A petition
# the enricher has not reached would read as "no dissent, no split" and score
# low, so the run refuses a set whose coverage falls short of the corpus's own.
EXTRA_ARCHIVES <- c("data-raw/snapshot_refresh.rds", "data-raw/snapshot_25.rds")
extra_paths <- EXTRA_ARCHIVES[file.exists(EXTRA_ARCHIVES)]
p_extra <- numeric(0); n_extra <- 0L; extra_cov <- NA_real_
if (length(extra_paths)) {
  ex <- assemble_corpus(extra_paths) |>
    distinct(dkt, .keep_all = TRUE) |>
    filter(!dkt %in% mf$dkt)
  sig_ok <- function(d) mean(coalesce(d$sig_v, 0L) >= MODEL_SIGNALS_VERSION)
  corpus_cov <- sig_ok(filter(corpus, type == "paid", !is.na(label), dkt %in% mf$dkt))
  ex_res <- filter(ex, type == "paid", !is.na(label))
  extra_cov <- sig_ok(ex_res)
  cat(sprintf("extra archives: %d resolved paid petitions | signals at v%d: %.1f%% (corpus %.1f%%)\n",
              nrow(ex_res), MODEL_SIGNALS_VERSION, 100 * extra_cov, 100 * corpus_cov))
  if (extra_cov < corpus_cov - 0.02)
    stop(sprintf(paste0("only %.1f%% of the extra archives' petitions carry signals at v%d, ",
                        "against %.1f%% of the corpus. Re-dispatch enrich-petitions.yml ",
                        "with terms=2025,refresh until it closes the gap."),
                 100 * extra_cov, MODEL_SIGNALS_VERSION, 100 * corpus_cov), call. = FALSE)
  mx <- model_frame(ex, COUNSEL_BLIND_FEATURES)
  # A level the corpus never saw (a lower court that first appears in OT2025)
  # has no coefficient; such a row cannot be scored and is left out on both
  # sides of the board, like any unscored petition.
  for (v in COUNSEL_BLIND_FEATURES) if (is.factor(mf[[v]]) || is.character(mf[[v]])) {
    bad <- !(as.character(mx[[v]]) %in% as.character(unique(mf[[v]])))
    if (any(bad)) cat("  ", sum(bad), "row(s) with an unseen", v, "level dropped\n")
    mx <- mx[!bad, ]
    if (is.factor(mf[[v]])) mx[[v]] <- factor(as.character(mx[[v]]), levels = levels(mf[[v]]))
  }
  final <- fit_logit(mf, COUNSEL_BLIND_FEATURES)
  cal <- fit_platt(mf$label, raw)
  p_extra <- setNames(apply_platt(cal, predict(final, newdata = mx, type = "response")),
                      mx$dkt)
  stopifnot(!anyNA(p_extra))
  n_extra <- length(p_extra)
  cat(sprintf("scored %d of them | %d granted, %.1f expected\n",
              n_extra, sum(mx$label), sum(p_extra)))
  print(mx |> mutate(p = p_extra) |> group_by(term_year) |>
          summarise(n = n(), granted = sum(label), expected = round(sum(p), 1)))
}

# ---- recentre each Term on its own grant count --------------------------------
# How many petitions the Court grants moves from Term to Term for reasons that
# have nothing to do with any advocate, and the petitions themselves drift: OT2025
# reports dissents, splits and dissent citations more often than any earlier Term
# while the Court granted 3.1% of its paid petitions, against 3.8-5.0% before. Left
# alone, the model expected 63.5 OT2025 grants and there were 37 -- a deficit that
# would land on whoever filed most that Term. (Smaller versions of the same thing
# run through the LOTO scores: OT2020 70 vs 84.7, OT2017 67 vs 57.9.)
#
# So every petition's log-odds in a Term is shifted by one constant, chosen so the
# Term's expected total equals its grants. The board then asks "better than other
# petitions THAT Term", which is the comparison a reader means. It reads each
# Term's outcomes only in aggregate: an advocate's own grants are a few of the
# ~60 in their Term, so the shift they induce on their own expectation is small
# and runs against them -- conservative, not flattering.
recentre <- function(p, label, term) {
  shift <- tapply(seq_along(p), term, function(i) {
    target <- sum(label[i]); z <- qlogis(p[i])
    uniroot(function(d) sum(plogis(z + d)) - target, c(-5, 5))$root
  })
  # Named explicitly: the arithmetic would otherwise hand back the Term labels
  # from `shift` as names, keying every probability to "2017", "2017.1", ...
  list(p = setNames(plogis(qlogis(unname(p)) + unname(shift[as.character(term)])), names(p)),
       shift = shift)
}
all_p <- c(setNames(p, mf$dkt), p_extra)
all_term <- c(mf$term_year, if (n_extra) mx$term_year)
all_label <- c(mf$label, if (n_extra) mx$label)
rc <- recentre(all_p, all_label, all_term)
# Keyed by docket or useless: a mis-keyed file joins to nothing, and the board
# then simply vanishes from the page with no error anywhere (it did, once,
# during development).
stopifnot(all(grepl("^[0-9]{2}-[0-9]+$", names(rc$p))), !anyDuplicated(names(rc$p)))
cat("\nper-Term log-odds shift (granted vs expected before it):\n")
print(tibble(term = names(rc$shift), shift = round(as.numeric(rc$shift), 3)) |>
  left_join(tibble(term = as.character(all_term), y = all_label, p0 = all_p) |>
              group_by(term) |> summarise(n = n(), granted = sum(y), expected = round(sum(p0), 1)),
            by = "term"))

# For the record: what dropping counsel cost the model. Measured on the same
# frame, the same way.
full_mf <- model_frame(mutate(corpus, counsel_tier = counsel_tier(
  counsel_key, date, build_counsel_index(corpus))), BASELINE_FEATURES)
full_p <- loto_platt(full_mf$label, loto_predict(full_mf, BASELINE_FEATURES),
                     full_mf$term_year)

res <- list(
  generated = format(Sys.Date()),
  features = COUNSEL_BLIND_FEATURES,
  terms = sort(unique(c(mf$term_year, if (n_extra) mx$term_year))),
  n = nrow(mf),
  granted = sum(mf$label),
  expected = round(sum(p), 1),
  auc = round(auc_roc(mf$label, p), 4),
  auc_with_counsel = round(auc_roc(full_mf$label, full_p), 4),
  # The snapshot petitions, scored by the all-Terms model (above).
  n_extra = n_extra,
  extra_signal_coverage = round(extra_cov, 4),
  term_shift = as.list(round(rc$shift, 4)),
  p = as.list(signif(rc$p, 4)))

cat(sprintf("scored %s petitions, OT%d-OT%d | %d granted, %.1f expected\n",
            format(res$n, big.mark = ","), min(res$terms), max(res$terms),
            res$granted, res$expected))
cat(sprintf("AUC counsel-blind %.3f | with counsel %.3f\n", res$auc, res$auc_with_counsel))
print(mf |> mutate(p = p) |> group_by(term_year) |>
        summarise(n = n(), granted = sum(label), expected = round(sum(p), 1)))

write_json(res, out, auto_unbox = TRUE, digits = NA)
cat("wrote", out, sprintf("(%.0f KB)\n", file.size(out) / 1024))
cat("Now re-run: Rscript .github/scripts/make_counsel_stats.R\n")
