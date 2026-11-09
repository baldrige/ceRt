# model_facts.R ----------------------------------------------------------------
# The one place a figure about the cert models may be written down.
#
# R/palette.R exists because the palette once lived in six :root blocks and 66
# literals, half of them stale. The model prose had the same disease. The
# methods note said one relist means "about 20%" for three months after the
# rate became 42%; it said GVRs are left out of training after set_target()
# made them negatives; the reference page quoted gap_na at +0.98 through two
# retrains. Each was a number typed into a sentence, and a retrain does not
# re-type sentences.
#
# So, the same three rules as the palette:
#
#   1. data/model_facts.json is written by the trainer, next to the artifacts
#      it describes, and carries their md5s. Nothing else writes it.
#   2. The documents take every prose figure from it through fact_span(), which
#      ERRORS on an unknown key (a NULL would print as an empty string) and
#      wraps the figure in <span data-fact="key">. A figure computed straight
#      off an artifact inside a loop -- a Platt coefficient, an outcome mix --
#      goes through gen_span() instead.
#   3. audit_site.R FAILS when the facts do not belong to the artifacts beside
#      them, when a data-fact span disagrees with its fact, or when the prose
#      holds a percentage, decimal or thousands-grouped number that came from
#      neither. See check_model_prose().
#
# Base R plus jsonlite, on purpose: the audit sources this file and must not
# need the model stack. compute_model_facts() alone needs R/cert_model.R, and
# only the trainer calls it.

MODEL_FACTS_FILE <- "model_facts.json"

# The artifacts a published figure can come from. cert_model_sg.rds and
# counsel_index.rds are not here: neither document quotes them.
MODEL_FACT_ARTIFACTS <- c("cert_model_baseline.rds", "cert_model_enhanced.rds",
                          "cert_model_gvr.rds", "cert_model_conference.rds")

# Figures that are true of the models but cannot be computed by the trainer:
# they come from an evaluation that is run by hand. Each entry is
#   key = list(value =, as_of = "YYYY-MM-DD", source = "...")
# and audit_site.R WARNs when as_of is older than the models -- a retrain does
# not re-run a hand-run evaluation.
#
# Empty, and better so. The rolling-origin gap was the one entry: quoted as
# "within 0.003 AUC" for months with no harness behind it, then as a figure
# measured two days before a retrain. The trainer runs rolling_origin_eval()
# now and the gap is computed like everything else. Before adding an entry
# here, ask whether the trainer could run the measurement instead.
MEASURED_FACTS <- list()

model_fingerprint <- function(dir) {
  p <- file.path(dir, MODEL_FACT_ARTIFACTS)
  if (!all(file.exists(p)))
    stop("model_fingerprint(): missing ", paste(basename(p)[!file.exists(p)], collapse = ", "),
         " in ", dir, call. = FALSE)
  as.list(stats::setNames(unname(tools::md5sum(p)), MODEL_FACT_ARTIFACTS))
}

# Short form for a <meta> tag: enough of each md5 to tell two retrains apart.
fingerprint_id <- function(fp)
  paste(substr(unlist(fp[MODEL_FACT_ARTIFACTS]), 1, 8), collapse = "-")

# ---- compute (trainer only; needs R/cert_model.R) ------------------------------
# `corpus` is the disposition corpus the baseline was fitted on. Every rate is
# over the same rows the baseline's model frame starts from -- paid and
# resolved -- so a figure in the prose and a count in the metrics table are
# about the same petitions.
# `ro` is list(baseline =, enhanced =, gvr =), each a rolling_origin_eval().
compute_model_facts <- function(corpus, model_dir, ro, trained_on = Sys.Date()) {
  art <- lapply(stats::setNames(MODEL_FACT_ARTIFACTS, c("baseline", "enhanced", "gvr", "conference")),
                function(f) readRDS(file.path(model_dir, f)))
  f <- list()
  for (k in c("baseline", "enhanced", "gvr")) {
    m <- art[[k]]; mc <- m$metrics_calibrated; cal <- m$calibration
    f[[paste0(k, ".base_rate")]] <- m$base_rate
    f[[paste0(k, ".brier_ref")]] <- m$base_rate * (1 - m$base_rate)
    f[[paste0(k, ".n")]]     <- mc$n
    f[[paste0(k, ".n_pos")]] <- mc$n_pos
    f[[paste0(k, ".auc")]]   <- mc$auc
    f[[paste0(k, ".ap")]]    <- mc$ap
    f[[paste0(k, ".brier")]] <- mc$brier
    f[[paste0(k, ".top_decile.pred")]] <- cal$pred[nrow(cal)]
    f[[paste0(k, ".top_decile.obs")]]  <- cal$obs[nrow(cal)]
  }
  f[["baseline.ap_lift"]] <- art$baseline$metrics_calibrated$ap / art$baseline$base_rate
  f[["baseline.deny_all_accuracy"]] <- 1 - art$baseline$base_rate
  for (k in c("baseline", "enhanced")) {
    b <- stats::coef(art[[k]]$glm)
    if (!"gap_naTRUE" %in% names(b)) stop("compute_model_facts(): no gap_na term in ", k)
    f[[paste0(k, ".gap_na.beta")]] <- unname(b[["gap_naTRUE"]])
  }

  # Rolling origin against leave-one-term-out, on the rows both can score.
  # The evaluation must be of THESE models: its leave-one-term-out AUC over
  # every row is the figure the artifact stores, or the frames have moved.
  stopifnot(setequal(names(ro), c("baseline", "enhanced", "gvr")))
  for (k in names(ro)) {
    r <- ro[[k]]
    if (abs(r$loto_all$auc - f[[paste0(k, ".auc")]]) > 1e-9)
      stop(sprintf("compute_model_facts(): the rolling-origin evaluation of '%s' reproduces a leave-one-term-out AUC of %.6f, but the artifact stores %.6f. The frame it ran on is not the one the model was fitted on.",
                   k, r$loto_all$auc, f[[paste0(k, ".auc")]]), call. = FALSE)
    f[[paste0("ro.", k, ".n")]]        <- r$ro$n
    f[[paste0("ro.", k, ".n_pos")]]    <- r$ro$n_pos
    f[[paste0("ro.", k, ".auc")]]      <- r$ro$auc
    f[[paste0("ro.", k, ".ap")]]       <- r$ro$ap
    f[[paste0("ro.", k, ".brier")]]    <- r$ro$brier
    f[[paste0("ro.", k, ".auc_loto")]] <- r$loto$auc      # LOTO on the same rows
    f[[paste0("ro.", k, ".ap_loto")]]  <- r$loto$ap
    f[[paste0("ro.", k, ".auc_gap")]]  <- r$loto$auc - r$ro$auc
    f[[paste0("ro.", k, ".ap_gap")]]   <- r$loto$ap - r$ro$ap
  }
  gaps <- vapply(names(ro), function(k) f[[paste0("ro.", k, ".auc_gap")]], numeric(1))
  f[["ro.auc_gap.min"]] <- min(gaps); f[["ro.auc_gap.max"]] <- max(gaps)
  f[["ro.first"]] <- min(ro$baseline$origins); f[["ro.last"]] <- max(ro$baseline$origins)

  paid <- corpus[corpus$type == "paid" & !is.na(corpus$label), ]
  ifp  <- corpus[corpus$type == "ifp"  & !is.na(corpus$label), ]
  # The frame the prose describes must be the frame the baseline was fitted on,
  # or the corpus cache and the artifacts have come apart.
  if (nrow(paid) != f[["baseline.n"]] || sum(paid$label) != f[["baseline.n_pos"]])
    stop(sprintf("compute_model_facts(): the corpus holds %d paid resolved petitions (%d granted) but the baseline artifact was fitted on %d (%d). The corpus cache does not belong to these models.",
                 nrow(paid), sum(paid$label), f[["baseline.n"]], f[["baseline.n_pos"]]), call. = FALSE)
  rate <- function(keep) {
    if (!any(keep)) stop("compute_model_facts(): an empty cell; a rate over no petitions is not a fact")
    mean(paid$label[keep])
  }
  yrs <- sort(unique(corpus$term_year))
  f[["terms.n"]] <- length(yrs); f[["terms.first"]] <- min(yrs); f[["terms.last"]] <- max(yrs)

  us <- paid$pet_type == "us_fed"
  f[["us_pet.n"]] <- sum(us); f[["us_pet.rate"]] <- rate(us); f[["other_pet.rate"]] <- rate(!us)

  rb <- relist_bucket(paid$n_relists)
  for (lv in c("0", "1", "2", "3-4", "5+")) f[[paste0("relist.", lv, ".rate")]] <- rate(rb == lv)

  f[["words.lt3k.rate"]]   <- rate(word_band(paid$words) == "<3k")
  f[["cites.11plus.rate"]] <- rate(cite_bucket(paid$n_dissent_cite) == "11+")
  # No entry in the signals cache: no PDF, or one that yielded no text.
  f[["signals.unread.share"]] <- mean(is.na(paid$sig_v))

  f[["ifp.n"]] <- nrow(ifp); f[["ifp.granted"]] <- sum(ifp$label); f[["ifp.rate"]] <- mean(ifp$label)

  list(trained_on = as.character(trained_on), fingerprint = model_fingerprint(model_dir),
       facts = f, measured = MEASURED_FACTS)
}

write_model_facts <- function(mf, dir) {
  path <- file.path(dir, MODEL_FACTS_FILE)
  writeLines(jsonlite::toJSON(mf, auto_unbox = TRUE, digits = NA, pretty = TRUE), path, useBytes = TRUE)
  invisible(path)
}

# `verify`: refuse facts that were not computed beside the artifacts in `dir`.
# The documents pass TRUE, so a note cannot quote one retrain's rates under
# another retrain's coefficient tables.
read_model_facts <- function(dir = "data", verify = TRUE) {
  path <- file.path(dir, MODEL_FACTS_FILE)
  if (!file.exists(path))
    stop(path, " does not exist. The trainer writes it; to rebuild it for the ",
         "deployed artifacts without retraining, run .github/scripts/make_model_facts.R.", call. = FALSE)
  mf <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (verify) {
    now <- model_fingerprint(dir)
    if (!identical(unlist(mf$fingerprint[MODEL_FACT_ARTIFACTS]), unlist(now)))
      stop(path, " was computed for different artifacts than the ones in ", dir,
           " (md5 mismatch). Rerun the trainer or .github/scripts/make_model_facts.R.", call. = FALSE)
  }
  mf
}

# ---- use ---------------------------------------------------------------------
fact <- function(mf, key) {
  v <- mf$facts[[key]]
  if (is.null(v)) v <- mf$measured[[key]]$value
  if (is.null(v)) stop("fact(): no model fact named '", key, "'", call. = FALSE)
  v
}

# kind: "pct" (value is a proportion, shown x100 with a % sign), "num" (as is),
# "int" (as is, thousands-grouped, no decimals).
fact_text <- function(v, kind = c("pct", "num", "int"), digits = 1) {
  kind <- match.arg(kind)
  switch(kind,
    pct = sprintf(paste0("%.", digits, "f%%"), 100 * v),
    num = formatC(v, format = "f", digits = digits, big.mark = ","),
    int = formatC(round(v), format = "d", big.mark = ","))
}
fact_span <- function(mf, key, kind = "pct", digits = 1)
  sprintf('<span data-fact="%s">%s</span>', key, fact_text(fact(mf, key), kind, digits))

# A figure taken straight off an artifact where it is printed -- not prose.
gen_span <- function(text) sprintf("<span data-gen>%s</span>", text)

model_facts_meta <- function(mf)
  sprintf('<meta name="model-facts" content="%s">', fingerprint_id(mf$fingerprint))
read_model_facts_meta <- function(html) {
  m <- regmatches(html, regexpr('<meta name="model-facts" content="[^"]*">', html))
  if (!length(m)) NA_character_ else sub('.*content="([^"]*)".*', "\\1", m)
}

# Small counts read better as words ("eight Terms"); past twelve, the numeral.
number_word <- function(n) {
  w <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
         "ten", "eleven", "twelve")
  if (n >= 1 && n <= length(w)) w[n] else as.character(n)
}

# ---- check (audit) -----------------------------------------------------------
# Figures that are not about the models, per document. Anything else numeric in
# the prose must be a data-fact or data-gen span.
PROSE_ALLOW <- list(
  "cert_model_methods.html" = c(
    "17%",      # the worked example of how to read a forecast
    "0.5",      # a coin flip's AUC, and the odds ratio that halves
    "2.0",      # the odds ratio that doubles
    "0.05",     # the significance threshold
    "33.1",     # Rule 33.1(h)
    "9,000",    # the Rule's word limit
    "3,000"),   # the edge of the shortest word band
  "cert_model_reference.html" = c(
    "0.05"))    # the significance threshold

# The text a reader takes as the author's claim: tables, stat lists, figures,
# styles and scripts removed, then the sourced spans, then every remaining tag.
.prose_text <- function(html) {
  for (tag in c("style", "script", "table", "svg", "dl"))
    html <- gsub(sprintf("(?is)<%s\\b.*?</%s>", tag, tag), " ", html, perl = TRUE)
  html <- gsub("(?is)<span data-(fact|gen)\\b[^>]*>.*?</span>", " ", html, perl = TRUE)
  gsub("(?s)<[^>]*>", " ", html, perl = TRUE)
}

# Returns list(n = spans checked, wrong = data-fact spans that disagree with
# their fact, stray = unsourced figures in the prose). Both empty is a pass.
check_model_prose <- function(path, mf, allow = PROSE_ALLOW[[basename(path)]]) {
  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  spans <- regmatches(html, gregexpr('<span data-fact="[^"]*">[^<]*</span>', html))[[1]]
  wrong <- character(0)
  for (s in spans) {
    key <- sub('.*data-fact="([^"]*)".*', "\\1", s)
    txt <- sub(".*>([^<]*)</span>$", "\\1", s)
    v <- mf$facts[[key]]; if (is.null(v)) v <- mf$measured[[key]]$value
    shown <- suppressWarnings(as.numeric(gsub("[,%]", "", txt)))
    if (is.null(v) || is.na(shown)) { wrong <- c(wrong, sprintf("%s='%s' (no such fact)", key, txt)); next }
    if (grepl("%", txt, fixed = TRUE)) v <- 100 * v
    k <- if (grepl(".", txt, fixed = TRUE)) nchar(sub(".*\\.", "", gsub("%", "", txt))) else 0L
    # Correct to the precision shown, which is all a rounded figure can claim.
    if (abs(v - shown) > 0.5 * 10^-k + 1e-9)
      wrong <- c(wrong, sprintf("%s='%s' (fact is %s)", key, txt, format(v, digits = 6)))
  }

  prose <- .prose_text(html)
  pat <- "[0-9][0-9,]*(\\.[0-9]+)?%|[0-9]+\\.[0-9]+|[0-9]{1,3}(,[0-9]{3})+"
  stray <- setdiff(unique(regmatches(prose, gregexpr(pat, prose))[[1]]), allow)
  list(n = length(spans), wrong = wrong, stray = stray, meta = read_model_facts_meta(html))
}
