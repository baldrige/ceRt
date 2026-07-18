# cert_model.R -----------------------------------------------------------------
# A calibrated model of the probability that a paid petition for certiorari is
# GRANTED plenary review. Phase 0 assembles a labeled corpus and computes
# leakage-safe features; Phase 1 fits an interpretable logistic model, evaluates
# it out-of-time, calibrates it, and explains each prediction as a set of cues.
#
# Design decisions (see the feature-notes README for the rationale):
#   * Target      : outcome == "granted" (plenary merits grant). GVRs, Rule 46
#                   dismissals, and still-pending petitions are EXCLUDED from
#                   training -- the model contrasts clean grants against clean
#                   denials, which is what "likelihood of cert" means to a
#                   reader. At inference it scores any petition.
#   * Segment     : PAID petitions only. IFP grant rates are ~0.1%, a different
#                   regime; pooling lets the easy IFP negatives wash out the
#                   paid signal. IFP is modeled separately (or not surfaced).
#   * Leakage     : every process feature (relists, cert-stage amicus, CVSG,
#                   response events) is snapshotted STRICTLY BEFORE the decision
#                   date. This is essential: merits-stage amicus briefs are
#                   filed AFTER a grant, so counting them over the whole docket
#                   would let the model "learn" that grants cause amicus briefs.
#   * Two tiers   : the BASELINE model (daily dashboards) uses only structural
#                   features known at the petition stage; the ENHANCED model
#                   (conference reports, Phase 2) adds the process features.
#
# Labels and the relist/amicus/CFR machinery come from classify_petitions() in
# cert_funnel.R, which must be sourced before this file is used.

suppressPackageStartupMessages({
  library(tidyverse)
})

# ---- A. party / entity typing -------------------------------------------------

# The petitioner and respondent captions carry most of the entity signal. We
# split the caption on " v. " and classify each side into a coarse type; raw
# party names are far too high-cardinality to use directly.
US_FED_RX <- str_c(
  "\\bUnited States\\b",
  "|\\b(Secretary|Commissioner|Administrator|Comptroller|Postmaster General) ",
  "|\\bAttorney General of the United States\\b",
  "|\\b(National Labor Relations Board|NLRB|Securities and Exchange Commission|SEC",
  "|Environmental Protection Agency|EPA|Federal (Trade|Communications|Energy|Election|Maritime|Deposit) ",
  "|FTC|FCC|FDIC|Internal Revenue Service|Commissioner of Internal Revenue",
  "|Food and Drug Administration|Immigration and Naturalization|Citizenship and Immigration",
  "|Federal Bureau|Department of (Justice|Labor|State|Defense|Homeland Security|Health",
  "|Education|Agriculture|Commerce|Treasury|the Interior|Transportation|Veterans|Energy|Housing))\\b"
)
STATE_RX <- str_c(
  "\\b(State|Commonwealth) of [A-Z]",
  "|\\bPeople of the State\\b",
  "|^(State|Commonwealth|People) (of|ex rel)\\b",
  "|^(City|County|Town|Village|Borough|Township|Parish|Board of Education) of\\b",
  "|\\bGovernor of\\b|\\bAttorney General of [A-Z]",
  "|\\bDepartment of Corrections\\b|\\bWarden\\b|\\bSheriff\\b",
  "|\\b(School District|Board of Education|Housing Authority|Transit Authority)\\b"
)
BUSINESS_RX <- str_c(
  "\\b(Inc|L\\.?L\\.?C|Corp|Corporation|Co|Company|Ltd|L\\.?P|LLP|N\\.A|PLLC",
  "|Bank|Bancorp|Group|Holdings|Industries|Systems|Technologies|Pharmaceuticals",
  "|Laboratories|Airlines|Insurance|Mutual|Partners|Associates|Enterprises",
  "|Motors|Financial|Capital|Services|Communications|Networks|Solutions)\\b\\.?"
)

classify_entity <- function(name) {
  name <- name %||% ""
  name[is.na(name)] <- ""
  dplyr::case_when(
    str_detect(name, US_FED_RX)   ~ "us_fed",
    str_detect(name, STATE_RX)    ~ "state_local",
    str_detect(name, BUSINESS_RX) ~ "business",
    nzchar(str_squish(name))      ~ "individual",
    TRUE                          ~ "other"
  )
}

# petitioner / respondent name strings from a caption ("A, B v. C").
caption_sides <- function(caption) {
  caption <- caption %||% ""
  parts <- str_split_fixed(caption, "\\s+v\\.?\\s+", 2)
  list(pet = str_squish(parts[, 1]), resp = str_squish(parts[, 2]))
}

# ---- B. court below -----------------------------------------------------------

CIRCUIT_WORDS <- c(First="CA1", Second="CA2", Third="CA3", Fourth="CA4",
                   Fifth="CA5", Sixth="CA6", Seventh="CA7", Eighth="CA8",
                   Ninth="CA9", Tenth="CA10", Eleventh="CA11")

# Bucket the LowerCourt string into a modest set of levels: the 13 federal
# courts of appeals, a pooled STATE bucket, other federal courts, and OTHER.
court_bucket <- function(lower) {
  lower <- lower %||% ""
  lower[is.na(lower)] <- ""
  out <- rep("OTHER", length(lower))
  for (w in names(CIRCUIT_WORDS)) {
    hit <- str_detect(lower, str_c("\\b", w, " Circuit\\b"))
    out[hit] <- CIRCUIT_WORDS[[w]]
  }
  out[str_detect(lower, "District of Columbia Circuit")] <- "CADC"
  out[str_detect(lower, "Federal Circuit")]              <- "CAFED"
  # state courts: "Supreme Court of X", "Court of Appeals of X", "X Court of
  # Appeal(s)", "Superior Court", "Court of Criminal Appeals", etc. Anything not
  # a federal court of appeals but clearly a state tribunal.
  is_state <- str_detect(lower, str_c(
    "Supreme (Court|Judicial Court) of |Court of Appeals? of |Court of Criminal Appeals",
    "|Superior Court|Appellate (Court|Division)|Supreme Court, Appellate",
    "|Court of Special Appeals|Commonwealth Court|District Court of Appeal"))
  out[out == "OTHER" & is_state] <- "STATE"
  is_fed_other <- str_detect(lower, "District Court|Court of Federal Claims|Tax Court|Court of Appeals for the Armed Forces|Court of International Trade")
  out[out == "OTHER" & is_fed_other] <- "FED_OTHER"
  out
}

# ---- C. counsel (elite Supreme Court bar) -------------------------------------

# A curated set of firms and named advocates with well-above-baseline cert
# success. Matched against the petitioner's counsel-of-record firm and attorney
# name. Not exhaustive; extend as the data warrants.
ELITE_COUNSEL_RX <- str_c(
  "Clement & Murphy|Gibson,? Dunn|Jones Day|Williams & Connolly|Mayer Brown",
  "|Hogan Lovells|Kirkland & Ellis|Latham & Watkins|Sidley Austin|Munger,? Tolles",
  "|WilmerHale|Wilmer Cutler|O'Melveny|Arnold & Porter|MoloLamken|Paul,? Weiss",
  "|Sullivan & Cromwell|Cravath|Covington|Susman Godfrey|Orrick|Morrison & Foerster",
  "|Vinson & Elkins|King & Spalding|Quinn Emanuel|Skadden|Dechert|Gupta Wessler",
  "|Consovoy|Cooper & Kirk|Boies Schiller|Bancroft",
  # named advocates who move between firms
  "|Goldstein|Katyal|Clement|Waxman|Phillips|Shanmugam|Gornstein|Fisher|Dreeben|Gupta")

# Pull the petitioner's counsel-of-record firm + attorney from the parties
# tibble (may be empty). Returns "" when unknown.
petitioner_counsel <- function(parties) {
  if (!is.data.frame(parties) || nrow(parties) == 0) return("")
  p <- parties[str_detect(parties$type %||% "", regex("petition", ignore_case = TRUE)), , drop = FALSE]
  if (nrow(p) == 0) return("")
  cor <- p[which(p$counsel_of_record %in% TRUE), , drop = FALSE]
  if (nrow(cor) == 0) cor <- p[1, , drop = FALSE]
  str_squish(str_c(cor$firm[1] %||% "", " ", cor$attys[1] %||% ""))
}

# ---- D. feature extraction ----------------------------------------------------

# Structural features known at the petition stage (no docket-development
# signal). One row of predictors from one case record.
petition_features <- function(caption, lower, parties, date, lower_date, related) {
  sides <- caption_sides(caption)
  counsel <- petitioner_counsel(parties)
  # NA-safe: missing/blank counsel makes str_detect() return NA, which drops the
  # whole row in model.matrix (score_case then fails with "subscript out of
  # bounds"). Absence of counsel data means "not elite", not unknown.
  elite <- str_detect(counsel, ELITE_COUNSEL_RX); elite <- !is.na(elite) & elite
  pet_type <- classify_entity(sides$pet)
  resp_type <- classify_entity(sides$resp)
  d_gap <- suppressWarnings(as.integer(as.Date(date) - as.Date(lower_date)))
  tibble(
    pet_type       = pet_type,
    resp_type      = resp_type,
    us_petitioner  = pet_type == "us_fed",
    us_respondent  = resp_type == "us_fed",
    business_pet   = pet_type == "business",
    court_below    = court_bucket(lower),
    elite_counsel  = elite,
    days_lower_gap = if (length(d_gap) == 0 || is.na(d_gap)) NA_integer_ else d_gap,
    # NA-safe: nzchar(NA) is TRUE, which silently made this a constant on the
    # historical archives (they carry no `related` column).
    related_present = !is.na(related) && nzchar(str_squish(related %||% ""))
  )
}

# Process features, snapshotted STRICTLY BEFORE `as_of` (the decision date in
# training; a conference date at inference). Counts are leakage-safe by date.
CVSG_RX  <- "Solicitor General is invited"
AMICUS_RX <- "^Brief (amicus|amici) curiae"
process_features <- function(events, as_of, n_relists_full = NA_integer_) {
  empty <- tibble(n_amicus_cert = 0L, cvsg = FALSE,
                  response_requested = FALSE, response_filed = FALSE)
  if (!is.data.frame(events) || !("Proceedings and Orders" %in% names(events)) ||
      nrow(events) == 0) return(empty)
  txt <- events[["Proceedings and Orders"]]; txt[is.na(txt)] <- ""
  edate <- suppressWarnings(lubridate::mdy(events$Date))
  keep <- !is.na(edate) & edate < as_of              # strictly before the decision
  txt <- txt[keep]
  tibble(
    n_amicus_cert      = sum(str_detect(txt, AMICUS_RX)),
    cvsg               = any(str_detect(txt, CVSG_RX)),
    response_requested = any(str_detect(txt, "^Response Requested")),
    response_filed     = any(str_detect(txt, "in opposition filed"))
  )
}

# Relists enter the model as a BUCKET, not a linear count: the grant rate is
# non-monotonic in relists (0: 1%, 1: 20%, 2: 44%, 3-4: 36%, 5+: 19% over
# OT2017-24), and a linear term would extrapolate a 20-relist hold to ~99%.
# Bucketing gives each level its own effect and caps the ambiguous tail.
relist_bucket <- function(n) {
  as.character(cut(n, breaks = c(-1, 0, 1, 2, 4, Inf),
                   labels = c("0", "1", "2", "3-4", "5+")))
}

# "Hold" detector. A petition deferred conference after conference -- far beyond
# the 1-3 relists of a case under active grant consideration -- is being HELD,
# typically pending a lead case on the same question. Empirically (OT2017-24,
# paid, >=6 relists) a held petition resolves ~15% granted, ~20% GVR'd, ~65%
# denied: a hold predicts DEFERRAL, most often toward a GVR, not a plenary grant.
# So `held` is exposed as a status flag and a GVR-risk signal, NOT as something
# that raises the grant probability. Leakage-safe: pass relists counted < as_of.
#
# Two tiers: (1) serial relisting >= threshold; (2) the definitional signal --
# the petition is expressly linked (docket "Vide, NN-NNN" companion references)
# to a case already GRANTED as of the conference. Tier 2 is the sharper signal
# for companion grants; it needs the `related` field (live JSON pipeline only --
# the training archives lack it) and the set of dockets granted so far, which a
# conference renderer has on hand.
hold_signal <- function(n_relists, related = NULL, granted_dockets = character(),
                        threshold = 6L) {
  linked <- related_companions(related)
  linked_grant <- length(intersect(linked, granted_dockets)) > 0
  (n_relists >= threshold) | linked_grant
}

# Companion docket numbers referenced in a petition's `related` field (e.g.
# "Vide, 25-566" -> "25-566"). The Court uses "Vide" (Latin: see) to cross-link
# companion petitions processed together.
related_companions <- function(related) {
  if (is.null(related) || length(related) == 0 || is.na(related[1]) ||
      !nzchar(related[1])) return(character())
  unique(str_extract_all(related[1], "\\b\\d{2}-\\d{1,5}\\b")[[1]])
}

# ---- E. corpus assembly -------------------------------------------------------

# Read one term's case tibble, classify every petition, recover the party/court
# columns the classifier drops, and attach structural + process features. The
# process features are snapshotted as of each petition's decision date so the
# same corpus serves both the baseline (structural) and enhanced (all) models.
assemble_term <- function(path) {
  cases <- readRDS(path)
  cls <- classify_petitions(cases)                    # dkt,type,date,caption,outcome,...
  # recover parties / lower / lower_date / related / events by docket
  extra <- cases |>
    transmute(dkt,
              lower = if ("lower" %in% names(cases)) lower else NA_character_,
              lower_date = if ("lower_date" %in% names(cases)) lower_date else as.Date(NA),
              related = if ("related" %in% names(cases)) related else NA_character_,
              parties = if ("parties" %in% names(cases)) parties else vector("list", n()),
              events = events)
  df <- cls |> left_join(extra, by = "dkt")

  feats <- purrr::pmap_dfr(
    list(df$caption, df$lower, df$parties, df$date, df$lower_date, df$related),
    petition_features)
  proc <- purrr::pmap_dfr(
    list(df$events, df$outcome_date, df$n_relists),
    function(ev, od, nr) process_features(ev, if (is.na(od)) as.Date("2999-01-01") else od, nr))

  bind_cols(
    df |> select(dkt, type, term, caption, date, outcome, outcome_date,
                 n_dist, n_relists, has_cfr, has_resp, has_amicus),
    feats, proc)
}

# The petition-derived Rule 10 signals (data-raw/petition_signals.json), keyed by
# docket, produced by the enrich-petitions workflow. Returns an empty (but typed)
# tibble when the layer is absent, so a join still creates the columns.
PETITION_SIGNALS_PATH <- "data-raw/petition_signals.json"
load_petition_signals <- function(path = PETITION_SIGNALS_PATH) {
  empty <- tibble(dkt = character(), dissent_below = logical(),
                  dissent_argued = logical(), enbanc_dissent = logical(),
                  split_argued = logical())
  if (!file.exists(path)) return(empty)
  j <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
  if (length(j) == 0) return(empty)
  purrr::imap_dfr(j, function(s, dk) tibble(
    dkt = dk, dissent_below = isTRUE(s$dissent_below),
    dissent_argued = isTRUE(s$dissent_argued),
    enbanc_dissent = isTRUE(s$enbanc_dissent),
    split_argued = isTRUE(s$split_argued)))
}

# Assemble the full labeled corpus across term files. Adds the binary label,
# term-year, and the petition-derived Rule 10 signals (missing/unresolved ->
# FALSE, so no rows drop at fit time). Keeps every decided-or-pending row; model
# fitting does the grant/deny filtering so callers can inspect the distribution.
assemble_corpus <- function(paths) {
  message("Assembling corpus from ", length(paths), " term file(s)...")
  corpus <- purrr::map_dfr(paths, function(p) {
    message("  ", basename(p)); assemble_term(p)
  })
  corpus |>
    left_join(load_petition_signals(), by = "dkt") |>
    mutate(across(c(dissent_below, dissent_argued, enbanc_dissent, split_argued),
                  ~ coalesce(.x, FALSE)),
           term_year = 2000L + as.integer(term), granted = outcome == "granted") |>
    set_target("grant")
}

# Set the binary `label` for a modeling target, leaving all other rows NA so
# model fitting drops them:
#   "grant" -> granted (1) vs denied (0)               [the cert model]
#   "gvr"   -> GVR (1) vs granted|denied (0)           [the companion GVR-risk model]
set_target <- function(corpus, target = c("grant", "gvr")) {
  target <- match.arg(target)
  corpus$label <- if (target == "grant")
    dplyr::case_when(corpus$outcome == "granted" ~ 1L,
                     corpus$outcome == "denied"  ~ 0L, TRUE ~ NA_integer_)
  else
    dplyr::case_when(corpus$outcome == "gvr" ~ 1L,
                     corpus$outcome %in% c("granted", "denied") ~ 0L, TRUE ~ NA_integer_)
  corpus
}

# ---- F. metrics (no external deps) --------------------------------------------

# Rank-based ROC-AUC (Mann-Whitney). NA if a class is empty.
auc_roc <- function(y, p) {
  pos <- p[y == 1]; neg <- p[y == 0]
  if (length(pos) == 0 || length(neg) == 0) return(NA_real_)
  r <- rank(c(pos, neg))
  (sum(r[seq_along(pos)]) - length(pos) * (length(pos) + 1) / 2) /
    (length(pos) * length(neg))
}

# Average precision (area under precision-recall), the imbalance-aware metric.
average_precision <- function(y, p) {
  if (sum(y == 1) == 0) return(NA_real_)
  o <- order(p, decreasing = TRUE); y <- y[o]
  tp <- cumsum(y == 1); fp <- cumsum(y == 0)
  prec <- tp / (tp + fp); rec <- tp / sum(y == 1)
  drec <- diff(c(0, rec))
  sum(prec * drec)
}

binary_metrics <- function(y, p) {
  ok <- !is.na(y) & !is.na(p)
  y <- y[ok]; p <- p[ok]
  list(n = length(y), n_pos = sum(y == 1), base_rate = mean(y == 1),
       auc = auc_roc(y, p), ap = average_precision(y, p),
       brier = mean((p - y)^2))
}

# Calibration table: bin predictions and compare mean predicted vs observed.
calibration_table <- function(y, p, bins = 10) {
  ok <- !is.na(y) & !is.na(p); y <- y[ok]; p <- p[ok]
  br <- unique(quantile(p, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE))
  g <- cut(p, breaks = br, include.lowest = TRUE)
  tibble(y, p, g) |> group_by(g) |>
    summarise(n = n(), pred = mean(p), obs = mean(y), .groups = "drop")
}

# ---- G. model fit, out-of-time evaluation, calibration ------------------------

# Feature groups. Structural = known from case identity (entity type is carried
# by the pet_type/resp_type factors, which include a "us_fed" level, so the
# standalone us_petitioner/business_pet logicals are omitted -- they duplicate a
# level and would alias a coefficient to NA; related_present is omitted until the
# archives carry a `related` column). Petition-signal = the Rule 10 cues parsed
# from the petition PDF (petition_signals.R). Process = the leakage-safe docket-
# development signals (relists bucketed, not linear).
STRUCTURAL_FEATURES <- c("pet_type", "resp_type", "court_below", "elite_counsel")
PETITION_SIGNAL_FEATURES <- c("dissent_below", "split_argued")
PROCESS_FEATURES <- c("relist_bucket", "n_amicus_cert", "cvsg",
                      "response_requested", "response_filed")

# The BASELINE (daily / petition-stage) model adds the Rule 10 signals to the
# structural set -- both are knowable the day a petition is docketed, and the
# dissent/split cues are the biggest lever a structural model has. The ENHANCED
# (conference-stage) model instead adds the process signals; it deliberately
# does NOT use the petition signals (they add ~no lift once relists/amicus exist,
# and the conference renderer doesn't parse petition PDFs, so leaving them out
# avoids a train/serve mismatch).
BASELINE_FEATURES <- c(STRUCTURAL_FEATURES, PETITION_SIGNAL_FEATURES)
ENHANCED_FEATURES <- c(STRUCTURAL_FEATURES, PROCESS_FEATURES)

# Reference levels for the categorical predictors, chosen so a cue's log-odds
# reads against an intuitive baseline: a private individual party, a state
# court below, a petition not yet relisted.
FACTOR_REFERENCES <- list(pet_type = "individual", resp_type = "individual",
                          court_below = "STATE", relist_bucket = "0")

# Training frame: paid, decided as grant or deny, complete predictors. Residual
# levels that would separate the likelihood (the "OTHER" court bucket has zero
# grants; the empty-caption entity "other" is a handful of cases) are lumped
# into a neighboring low-rate level so glm coefficients stay finite. The raw
# extractors keep the granular levels; only model fitting lumps.
lump_model_levels <- function(df) {
  df |>
    mutate(
      court_below = if ("court_below" %in% names(df))
        if_else(court_below == "OTHER", "STATE", court_below) else NULL,
      pet_type  = if ("pet_type"  %in% names(df)) if_else(pet_type  == "other", "individual", pet_type)  else NULL,
      resp_type = if ("resp_type" %in% names(df)) if_else(resp_type == "other", "individual", resp_type) else NULL
    )
}
model_frame <- function(corpus, features) {
  df <- corpus |>
    filter(type == "paid", !is.na(label)) |>
    lump_model_levels() |>
    mutate(relist_bucket = relist_bucket(n_relists))   # bucketed from the raw count
  for (v in names(FACTOR_REFERENCES)) if (v %in% features)
    df[[v]] <- relevel(factor(df[[v]]), ref = FACTOR_REFERENCES[[v]])
  df |>
    select(dkt, term_year, label, all_of(features)) |>
    tidyr::drop_na()
}

fit_logit <- function(train, features) {
  form <- reformulate(features, response = "label")
  suppressWarnings(glm(form, data = train, family = binomial()))
}

# Drop the n-sized components (model frame, residuals, fitted values) and the
# captured environments a glm carries, so a saved model stays small and doesn't
# serialize the training data. Point prediction + model.matrix (for the cue
# explainer) still work from coefficients / terms / xlevels.
strip_glm <- function(m) {
  m$model <- NULL; m$residuals <- NULL; m$fitted.values <- NULL
  m$effects <- NULL; m$weights <- NULL; m$prior.weights <- NULL
  m$y <- NULL; m$linear.predictors <- NULL; m$na.action <- NULL; m$data <- NULL
  # The deployed scoring path computes the linear predictor from the design
  # matrix and coefficients directly (see score_features), never predict.glm,
  # so the n-by-p qr factorization is dead weight -- drop it. This is the bulk
  # of a fitted glm's size.
  m$qr <- NULL
  # Re-root (not empty) the formula/terms environments: model.frame() evaluates
  # in them and needs base functions, but we don't want to retain the original
  # (data-carrying) calling environment. baseenv() gives both.
  attr(m$terms, ".Environment") <- baseenv()
  if (!is.null(m$formula)) attr(m$formula, ".Environment") <- baseenv()
  m
}

# Leave-one-term-out predictions: each term is scored by a model trained on the
# OTHER terms, giving an out-of-time prediction for every petition. Used both to
# report honest performance and to fit the calibrator.
loto_predict <- function(mf, features) {
  terms <- sort(unique(mf$term_year))
  preds <- rep(NA_real_, nrow(mf))
  for (t in terms) {
    tr <- mf[mf$term_year != t, ]; te <- which(mf$term_year == t)
    m <- fit_logit(tr, features)
    preds[te] <- predict(m, newdata = mf[te, ], type = "response")
  }
  preds
}

# Platt scaling: recalibrate raw probabilities via a 1-D logistic on the logit.
# The logit is precomputed into a plain column `z` so the fitted formula is
# `y ~ z` (no namespaced function), which keeps it safe to strip_glm().
fit_platt <- function(y, p) {
  z <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  glm(y ~ z, family = binomial(), data = data.frame(y = y, z = z))
}
apply_platt <- function(cal, p) {
  z <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  co <- coef(cal)                       # intercept + slope on z; computed
  as.numeric(plogis(co[[1]] + co[[2]] * z))   # directly, so no qr needed
}

# Fit the deployable model: refit on ALL terms, and fit a Platt calibrator on
# out-of-term predictions so the stored probabilities are honest. Returns the
# glm, the calibrator, the feature set, the training base rate, and the
# out-of-time metrics.
fit_cert_model <- function(corpus, features = BASELINE_FEATURES, target = "grant") {
  mf <- model_frame(corpus, features)
  message("Model frame [", target, "]: ", nrow(mf), " paid petitions, ",
          sum(mf$label), " positives (", round(100 * mean(mf$label), 2), "%)")
  raw <- loto_predict(mf, features)
  cal <- fit_platt(mf$label, raw)
  metrics_raw <- binary_metrics(mf$label, raw)
  metrics_cal <- binary_metrics(mf$label, apply_platt(cal, raw))
  final <- fit_logit(mf, features)
  structure(list(
    glm = strip_glm(final), calibrator = strip_glm(cal), features = features,
    target = target, xlevels = final$xlevels, base_rate = mean(mf$label),
    metrics = metrics_raw, metrics_calibrated = metrics_cal,
    calibration = calibration_table(mf$label, apply_platt(cal, raw)),
    loto = tibble(dkt = mf$dkt, term_year = mf$term_year,
                  label = mf$label, pred = apply_platt(cal, raw))
  ), class = "cert_model")
}

# A strictly forward-looking check: train on all terms before the last, test on
# the last term (the closest thing to real deployment).
forward_eval <- function(corpus, features = BASELINE_FEATURES) {
  mf <- model_frame(corpus, features)
  last <- max(mf$term_year)
  tr <- mf[mf$term_year < last, ]; te <- mf[mf$term_year == last, ]
  m <- fit_logit(tr, features)
  p <- predict(m, newdata = te, type = "response")
  c(list(test_term = last), binary_metrics(te$label, p))
}

# ---- H. scoring + cue-breakdown explainer -------------------------------------

# Score a feature row with a fitted cert_model, returning the calibrated
# probability and the raw log-odds contribution of each predictor (relative to
# the model's reference level), sorted by magnitude -- the "which cues fired"
# explanation shown to the reader.
score_features <- function(model, newrow) {
  beta <- coef(model$glm)
  beta[is.na(beta)] <- 0            # aliased terms contribute nothing (as predict.glm)
  # Build the one-row design matrix once; the linear predictor and the per-cue
  # contributions both come from it, so we never call predict.glm (and the
  # stored model needs no qr).
  mm <- model.matrix(delete.response(terms(model$glm)), data = newrow,
                     xlev = model$xlevels, contrasts.arg = model$glm$contrasts)
  x <- mm[1, names(beta), drop = TRUE]
  raw <- plogis(sum(x * beta))                       # logit link inverse
  prob <- apply_platt(model$calibrator, raw)
  nonint <- names(beta) != "(Intercept)"
  contrib <- x[nonint] * beta[nonint]
  contrib <- contrib[is.finite(contrib) & contrib != 0]
  cues <- tibble(term = names(contrib), log_odds = as.numeric(contrib)) |>
    arrange(desc(abs(log_odds)))
  list(prob = as.numeric(prob), raw = as.numeric(raw),
       base_rate = model$base_rate, lift = as.numeric(prob) / model$base_rate,
       cues = cues)
}

# ---- forecast description (plain-English cue read) ----------------------------
# Human phrase for each model cue term. model.matrix names a factor level as
# "<var><level>" (reference level omitted), a logical as "<var>TRUE", a numeric
# as "<var>". Each phrase names ONLY the factor; the direction (raises vs lowers
# the forecast) comes from the sign of the cue's log-odds, so one phrase serves
# both. Covers the BASELINE cues (structural + petition signals) and the ENHANCED
# process cues, so the describer works for either model.
FORECAST_CUE_PHRASES <- c(
  "pet_typeus_fed"       = "a federal-government petitioner",
  "pet_typestate_local"  = "a state or local-government petitioner",
  "pet_typebusiness"     = "a business petitioner",
  "resp_typeus_fed"      = "a federal-government respondent",
  "resp_typestate_local" = "a state or local-government respondent",
  "resp_typebusiness"    = "a business respondent",
  "court_belowCA1"  = "a First Circuit decision below",
  "court_belowCA2"  = "a Second Circuit decision below",
  "court_belowCA3"  = "a Third Circuit decision below",
  "court_belowCA4"  = "a Fourth Circuit decision below",
  "court_belowCA5"  = "a Fifth Circuit decision below",
  "court_belowCA6"  = "a Sixth Circuit decision below",
  "court_belowCA7"  = "a Seventh Circuit decision below",
  "court_belowCA8"  = "an Eighth Circuit decision below",
  "court_belowCA9"  = "a Ninth Circuit decision below",
  "court_belowCA10" = "a Tenth Circuit decision below",
  "court_belowCA11" = "an Eleventh Circuit decision below",
  "court_belowCADC"  = "a D.C. Circuit decision below",
  "court_belowCAFED" = "a Federal Circuit decision below",
  "court_belowFED_OTHER" = "another federal court below",
  "elite_counselTRUE" = "experienced Supreme Court counsel",
  "dissent_belowTRUE" = "a dissent in the court below (flagged in the petition)",
  "split_arguedTRUE"  = "a circuit split argued in the petition",
  "relist_bucket1"   = "one relist",
  "relist_bucket2"   = "two relists",
  "relist_bucket3-4" = "three or four relists",
  "relist_bucket5+"  = "five or more relists",
  "n_amicus_cert"          = "cert-stage amicus briefs",
  "cvsgTRUE"               = "a call for the Solicitor General's views (CVSG)",
  "response_requestedTRUE" = "a requested response",
  "response_filedTRUE"     = "a brief in opposition filed"
)

# Turn a score_features() result into one model-faithful sentence: the forecast's
# lift over the base rate, then the factors the model weights up and down (biggest
# |log-odds| first, up to `top` each; cues below `eps` are dropped as negligible).
# Deterministic -- no model call, no network. Guardrails baked into the wording:
# probability + lift, never yes/no; "the model weights this up/down" (the cues are
# correlational weights, not causes); a lean, not a verdict. `include_prob` adds a
# leading "an N% forecast" for standalone use (off by default, since on the docket
# page the number is shown right above it).
describe_forecast <- function(score, top = 3L, eps = 0.05, include_prob = FALSE,
                              retrospective = FALSE) {
  if (is.null(score) || is.null(score$cues) || is.na(score$prob %||% NA_real_)) return("")
  pctd <- function(p) sprintf("%.1f%%", 100 * p)        # base rate (1 dp)
  pcti <- function(p) sprintf("%d%%", round(100 * p))   # forecast (integer)
  mult <- function(l) { r <- round(l, 1)
    if (abs(r - round(r)) < .05) sprintf("%d", round(r)) else sprintf("%.1f", r) }
  join <- function(x) { n <- length(x)
    if (n == 0) "" else if (n == 1) x[1]
    else if (n == 2) paste(x, collapse = " and ")
    else paste0(paste(x[-n], collapse = ", "), ", and ", x[n]) }
  cap1 <- function(s) if (nzchar(s)) paste0(toupper(substring(s, 1, 1)), substring(s, 2)) else s
  # On a decided case the note is a retrospective: past tense + "before the
  # decision" frame. On a pending case it's a live read (present tense).
  wv  <- if (retrospective) "weighted" else "weights"
  pre <- if (retrospective) "before the decision, " else ""

  lift <- score$lift; base <- pctd(score$base_rate)

  # Below the base rate, DON'T list drivers. The per-cue log-odds are measured
  # against a very-low-grant reference profile (a private party, a state court
  # below), so almost any federal case shows a large positive "up" cue for its
  # circuit of origin -- which reads as a grant signal on a case the model
  # actually rates as unremarkable. Say that plainly instead.
  if (!is.na(lift) && lift <= 0.85) {
    lead <- paste0(pre, "well below the ", base, " base rate")
    if (include_prob) lead <- paste0(pre, pcti(score$prob), " — well below the ", base, " base rate")
    return(paste0(cap1(lead), ", with no standout signals pointing toward a grant."))
  }

  # At or above the base rate, name the real drivers (biggest |log-odds| first).
  lift_ph <- if (is.na(lift)) "" else
    if (lift >= 1.5)       sprintf("about %s× the %s base rate", mult(lift), base)
    else if (lift >= 1.15) sprintf("modestly above the %s base rate", base)
    else                   sprintf("roughly the %s base rate", base)   # 0.85 < lift < 1.15
  lead <- paste0(pre, if (include_prob && nzchar(lift_ph))
                        paste0(pcti(score$prob), " — ", lift_ph) else lift_ph)
  if (nzchar(lead)) lead <- paste0(cap1(lead), ".")

  cu <- score$cues
  cu <- cu[is.finite(cu$log_odds) & abs(cu$log_odds) >= eps, , drop = FALSE]
  ph <- function(terms) { v <- unname(FORECAST_CUE_PHRASES[terms]); v[!is.na(v)] }
  up <- ph(head(dplyr::arrange(dplyr::filter(cu, log_odds > 0), dplyr::desc(log_odds))$term, top))
  dn <- ph(head(dplyr::arrange(dplyr::filter(cu, log_odds < 0), log_odds)$term, top))

  drv <- if (length(up) && length(dn))
      sprintf(" The model %s this up for %s, and down for %s.", wv, join(up), join(dn))
    else if (length(up)) sprintf(" The model %s this up for %s.", wv, join(up))
    else if (length(dn)) sprintf(" The model %s this down for %s.", wv, join(dn))
    else if (retrospective) " No single factor stood out." else " No single factor stands out."
  paste0(lead, drv)
}

# Convenience: score a raw case record (caption/lower/parties/...) at a given
# as-of date. Structural features always apply; process features are included
# only if the model uses them.
score_case <- function(model, caption, lower, parties, date, lower_date,
                       related, events = NULL, as_of = Sys.Date(), signals = NULL) {
  f <- petition_features(caption, lower, parties, date, lower_date, related)
  # Petition-derived Rule 10 signals: supplied by the caller (which fetched/parsed
  # the petition PDF) or defaulted to FALSE (absence) when unavailable at inference.
  for (nm in c("dissent_below", "dissent_argued", "enbanc_dissent", "split_argued"))
    f[[nm]] <- if (!is.null(signals) && !is.null(signals[[nm]])) isTRUE(signals[[nm]]) else FALSE
  if (any(PROCESS_FEATURES %in% model$features)) {
    f <- bind_cols(f, process_features(events, as.Date(as_of)))
    # Relists strictly before the as-of date, via the audited relist grammar in
    # classify_petition_events() (cert_funnel.R must be sourced). A grant at this
    # conference would be its final relist, so counting < as_of is correct.
    nrel <- 0L
    cl <- tryCatch(classify_petition_events(events), error = function(e) NULL)
    if (!is.null(cl)) {
      rd <- cl$relist_dates[[1]]
      nrel <- sum(!is.na(rd) & rd < as.Date(as_of))
    }
    f$n_relists <- nrel
    f$relist_bucket <- relist_bucket(nrel)
  }
  # Lump residual levels exactly as at training (e.g. an "OTHER" court -> STATE),
  # then coerce factor columns to the model's known levels.
  f <- lump_model_levels(f)
  for (v in intersect(names(model$xlevels), names(f)))
    f[[v]] <- factor(f[[v]], levels = model$xlevels[[v]])
  score_features(model, f)
}

# Combined disposition read for a petition at a conference: calibrated P(grant)
# and P(GVR) from the two models plus the hold flag. `granted_dockets` is the set
# of dockets already granted as of the conference (enables the companion
# "Vide"-linkage hold tier); a conference renderer has this set on hand. This is
# the interface the dashboards call.
score_disposition <- function(grant_model, gvr_model, caption, lower, parties,
                              date, lower_date, related, events, as_of,
                              granted_dockets = character()) {
  g <- score_case(grant_model, caption, lower, parties, date, lower_date,
                  related, events = events, as_of = as_of)
  v <- score_case(gvr_model, caption, lower, parties, date, lower_date,
                  related, events = events, as_of = as_of)
  cl <- tryCatch(classify_petition_events(events), error = function(e) NULL)
  nrel <- if (is.null(cl)) 0L else {
    rd <- cl$relist_dates[[1]]; sum(!is.na(rd) & rd < as.Date(as_of))
  }
  list(p_grant = g$prob, p_gvr = v$prob, n_relists = nrel,
       held = hold_signal(nrel, related, granted_dockets),
       lift = g$lift, grant_cues = g$cues)
}

# Load the trained models from `dir`; returns a named list with whichever exist
# (baseline / enhanced / gvr). Never errors -- a render pipeline can call this and
# simply omit the forecast column when the artifacts are absent or unreadable.
load_cert_models <- function(dir = "data") {
  want <- c(baseline = "cert_model_baseline.rds",
            enhanced = "cert_model_enhanced.rds",
            gvr      = "cert_model_gvr.rds")
  out <- list()
  for (nm in names(want)) {
    f <- file.path(dir, want[[nm]])
    if (file.exists(f)) {
      m <- tryCatch(readRDS(f), error = function(e) NULL)
      if (!is.null(m)) out[[nm]] <- m
    }
  }
  out
}

# A calibrated P(grant) percentage cell for a gt table, e.g. 0.42 -> "42%".
# NA (non-paid / unmodeled) renders as an em dash.
fmt_prob_cell <- function(p) ifelse(is.na(p), "—", paste0(round(100 * p), "%"))
