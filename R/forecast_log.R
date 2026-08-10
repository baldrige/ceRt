# forecast_log.R ----------------------------------------------------------------
# A prospective record of what this site said before the Court acted.
#
# WHY THIS IS NOT WHAT methods.html ALREADY PUBLISHES. That page reports
# leave-one-term-out validation: each Term scored by a model trained on the other
# seven, calibration fitted out-of-fold. It is honest and carefully done, and
# every number on it was produced after the outcomes were known. A sceptical
# reader is entitled to discount it accordingly.
#
# The conference pages are no better as evidence, for a subtler reason: they score
# AS OF a past conference date but with TODAY's fitted coefficients. That is the
# right choice for those pages -- a reader looking at an archived conference wants
# the best current estimate of what was in front of the Justices -- but it means
# nothing on the site records what was actually forecast in advance.
#
# This does. And nothing about it is reconstructible after the fact, which is the
# entire point.
#
# THE ONE RULE THAT MATTERS: only log a conference that has not happened yet.
#
#   * Score a PAST conference with today's model and log it, and the scorecard is
#     flattering by construction -- the model has seen that outcome through its
#     training data.
#   * Overwrite an existing key on a later run, and the log stops being a record
#     of what was said and becomes a record of what is currently believed.
#
# Both failure modes make the numbers look better and mean nothing, and neither
# is visible in the output. So: future conferences only, append-only, enforced
# here in code rather than by convention, and checked again in audit_site.R via
# an invariant the file can be held to on its own -- scored_on must precede
# conf_date. An entry that violates it was logged after the fact.
#
# THE FILE. cases/forecasts.json, a flat object keyed "<docket>@<conf_date>",
# which is the shape publish_site.sh's DERIVED union resolution merges correctly.
# Unlike cases/pending.json (rewritten wholesale, so deliberately NOT in DERIVED),
# this one is append-only per key, so a union is exactly right.
#
# NOT BACKFILLED, EVER. There is no way to reconstruct what the site would have
# said, and a backfilled entry is indistinguishable in the file from a real one.
# The log starts empty and earns its contents.

suppressPackageStartupMessages(library(jsonlite))

FORECAST_LOG <- "cases/forecasts.json"

.flog_path <- function(site_dir) file.path(site_dir, FORECAST_LOG)

read_forecast_log <- function(site_dir) {
  p <- .flog_path(site_dir)
  if (!file.exists(p)) return(list())
  tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) {
    warning("read_forecast_log(): ", basename(p), " unreadable -- treating as empty. ",
            "NOTE: a run that cannot read the log will re-log keys it already ",
            "holds, so investigate rather than letting this ride.", call. = FALSE)
    list()
  })
}

# Score every paid petition distributed to a conference that has NOT yet happened,
# and append the ones not already logged.
#
# `dist` is a conference_distributions() tibble; `models` a load_cert_models()
# list. Returns a small summary (invisibly) for the run log.
append_forecasts <- function(site_dir, dist, models, as_of = Sys.Date()) {
  as_of <- as.Date(as_of)
  empty <- list(added = 0L, considered = 0L, conferences = character(),
                skipped_past = 0L, total = 0L)
  if (is.null(models) || is.null(models$conference) || !exists("score_conference")) {
    warning("append_forecasts(): no conference model -- nothing logged.", call. = FALSE)
    return(invisible(empty))
  }
  if (is.null(dist) || !nrow(dist)) return(invisible(empty))

  # Future conferences only. This is the leakage gate; everything else here is
  # bookkeeping.
  fut <- dist |> dplyr::filter(type == "paid", conf_date > as_of)
  n_past <- sum(dist$type == "paid" & dist$conf_date <= as_of)
  if (!nrow(fut)) {
    return(invisible(c(empty, list(skipped_past = n_past,
                                   total = length(read_forecast_log(site_dir))))))
  }

  idx <- read_forecast_log(site_dir)
  # A hash of the whole model list, so a refit is visible in the record. Same
  # quantity docket_page.R stamps into the render manifest.
  mid <- substr(digest::digest(models), 1, 16)
  scored_on <- format(as_of)

  gc_ <- function(nm, def) if (nm %in% names(fut)) fut[[nm]] else rep(def, nrow(fut))
  dt <- gc_("date", as.Date(NA)); ld <- gc_("lower_date", as.Date(NA))
  rel <- gc_("related", NA_character_)
  has_parties <- "parties" %in% names(fut)

  added <- 0L
  for (i in seq_len(nrow(fut))) {
    key <- paste0(fut$dkt[i], "@", format(fut$conf_date[i]))
    if (!is.null(idx[[key]])) next          # never overwrite
    gd <- if (all(c("outcome", "outcome_date") %in% names(dist)))
      unique(dist$dkt[dist$outcome %in% "granted" & !is.na(dist$outcome_date) &
                      dist$outcome_date < fut$conf_date[i]]) else character()
    s <- tryCatch(score_conference(
      models, fut$caption[i], fut$lower[i],
      if (has_parties) fut$parties[[i]] else NULL,
      dt[i], ld[i], rel[i], events = fut$events[[i]],
      as_of = fut$conf_date[i], conf_idx = fut$distribution_no[i],
      granted_dockets = gd), error = function(e) NULL)
    if (is.null(s) || is.na(s$p_grant_now)) next
    idx[[key]] <- list(
      docket = fut$dkt[i], conf_date = format(fut$conf_date[i]),
      scored_on = scored_on, model_id = mid,
      p_grant_now = round(s$p_grant_now, 5),
      p_gvr_now = round(s$p_gvr_now %||% NA_real_, 5),
      p_grant_ever = round(s$p_grant_ever %||% NA_real_, 5),
      conf_idx = as.integer(s$conf_idx), n_relists = as.integer(s$n_relists),
      held = isTRUE(s$held))
    added <- added + 1L
  }

  if (added > 0L) {
    dir.create(dirname(.flog_path(site_dir)), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(idx, .flog_path(site_dir), auto_unbox = TRUE)
  }
  invisible(list(added = added, considered = nrow(fut),
                 conferences = format(sort(unique(fut$conf_date))),
                 skipped_past = n_past, total = length(idx)))
}

# Join the log to realised outcomes. `cases` must be a case tibble wide enough to
# cover the logged dockets; anything not present, or not yet disposed of at its
# conference, comes back unresolved.
#
# A prediction of "granted at THIS conference" resolves at that conference: if the
# petition was granted on or within a few days of it, the forecast was right; if
# the Court did anything else -- denied, relisted, GVR'd -- it was wrong. The
# window exists because an order list follows its conference by a day or two.
score_forecast_log <- function(site_dir, cases, window_days = 7L,
                               classify = NULL) {
  if (is.null(classify)) {
    if (!exists("classify_petition_events"))
      stop("score_forecast_log(): classify_petition_events() not available.")
    classify <- get("classify_petition_events")
  }
  idx <- read_forecast_log(site_dir)
  if (!length(idx)) return(NULL)
  cls <- setNames(lapply(seq_len(nrow(cases)), function(i)
    tryCatch(classify(cases$events[[i]]), error = function(e) NULL)), cases$dkt)
  rows <- lapply(names(idx), function(k) {
    e <- idx[[k]]
    cl <- cls[[e$docket]]
    outcome <- if (is.null(cl)) NA_character_ else cl$outcome[[1]]
    odate <- if (is.null(cl)) as.Date(NA) else cl$outcome_date[[1]]
    cd <- as.Date(e$conf_date)
    resolved <- !is.na(odate) && odate <= cd + window_days
    data.frame(
      key = k, docket = e$docket, conf_date = cd,
      scored_on = as.Date(e$scored_on), model_id = e$model_id,
      p = e$p_grant_now, p_ever = e$p_grant_ever %||% NA_real_,
      # Unresolved means the Court has not acted by this conference + window;
      # that is a relist or a hold, i.e. NOT granted here.
      granted_here = if (resolved) as.integer(outcome %in% "granted") else 0L,
      resolved = as.integer(!is.na(odate) || cd + window_days < Sys.Date()),
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
