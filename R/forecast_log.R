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
# THE FINAL FORECAST, AND WHY IT DOES NOT BREAK THE RULE. The entry is the FIRST
# forecast, written the first night a petition appears on an upcoming
# conference -- often weeks ahead, before the response, the relists, a CVSG. It
# is the honest record of what was said first, and a poor record of the best the
# site could say: 25-1131 was logged at 0.6% on 3 September and granted at the
# long conference, while the conference page's later re-score read 17% to be
# granted ever. So each entry
# also carries a `final` block, rewritten every run while its conference is
# ahead and frozen by the clock at FORECAST_FINAL_CUTOFF_UTC on conference day:
# the last forecast made before the Justices met. Overwriting it is the design,
# not a leak, because every version of it was prospective; the cutoff is the
# leakage gate, and the audit checks it from the file (final$scored_at before
# the cutoff). The top-level numbers are never touched by this.
#
# THE FILE. cases/forecasts.json, a flat object keyed "<docket>@<conf_date>",
# which is the shape publish_site.sh's DERIVED union resolution merges correctly.
# Unlike cases/pending.json (rewritten wholesale, so deliberately NOT in DERIVED),
# this one is append-only per key, so a union is exactly right.
#
# NOT BACKFILLED, EVER. There is no way to reconstruct what the site would have
# said, and a backfilled entry is indistinguishable in the file from a real one.
# The log starts empty and earns its contents.
#
# THE ONE EXCEPTION, AND ITS SHAPE. An entry written by code that was broken is
# still a record of what the site said, so it is not deleted or edited in
# place. It is SUPERSEDED: if its conference is still ahead, the entry is
# re-scored under the same key -- prospectively, so the leakage rule holds --
# and the numbers it replaced move into a `superseded` block with the reason.
# If its conference has passed it cannot be re-scored; it gets a `void` field
# and score_forecast_log() leaves it out. Each such episode is one row of
# FORECAST_VOID_WINDOWS, by scored_on date, so the file itself says which of
# its entries a reader may not trust and why.

suppressPackageStartupMessages(library(jsonlite))

FORECAST_LOG <- "cases/forecasts.json"

# scored_on windows whose entries were produced by broken code. Closed, dated,
# and never removed: an entry's `superseded` block cites the reason verbatim.
FORECAST_VOID_WINDOWS <- list(
  # docket_page.R's caption_sides() shadowed cert_model.R's (#175 -> #192):
  # every federal, agency and most State petitioners scored as private
  # individuals. 31 entries, all for the 9 October 2026 conference.
  list(from = as.Date("2026-09-16"), to = as.Date("2026-09-22"),
       reason = "petitioner mistyped as an individual (shadowed caption_sides, #192)")
)

# The void-window reason an entry falls under, or NULL.
.flog_void_reason <- function(e) {
  sd <- tryCatch(as.Date(e$scored_on), error = function(err) as.Date(NA))
  if (is.na(sd)) return(NULL)
  for (w in FORECAST_VOID_WINDOWS) if (sd >= w$from && sd <= w$to) return(w$reason)
  NULL
}

.flog_path <- function(site_dir) file.path(site_dir, FORECAST_LOG)

# The last moment a FINAL forecast may be written: noon UTC on conference day.
# The Court's conferences open at 9:30 or 10:00 Eastern -- 13:30 UTC at the
# earliest, under daylight time -- and the nightly run renders at about 07:00
# UTC, so the conference-morning run lands well inside it and a late one does
# not. One constant, read by append_forecasts() and by the audit.
FORECAST_FINAL_CUTOFF_UTC <- "12:00:00"
.flog_final_cutoff <- function(conf_date)
  as.POSIXct(paste(format(as.Date(conf_date)), FORECAST_FINAL_CUTOFF_UTC), tz = "UTC")

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
# append the ones not already logged, and refresh every logged entry's `final`
# block (conference morning included, up to the cutoff).
#
# `dist` is a conference_distributions() tibble; `models` a load_cert_models()
# list. Returns a small summary (invisibly) for the run log.
append_forecasts <- function(site_dir, dist, models, as_of = Sys.Date(), now = Sys.time()) {
  as_of <- as.Date(as_of)
  empty <- list(added = 0L, considered = 0L, conferences = character(),
                skipped_past = 0L, total = 0L)
  if (is.null(models) || is.null(models$conference) || !exists("score_conference")) {
    warning("append_forecasts(): no conference model -- nothing logged.", call. = FALSE)
    return(invisible(empty))
  }
  if (is.null(dist) || !nrow(dist)) return(invisible(empty))

  # Two gates, one per kind of record.
  #
  #   * The FIRST forecast (the entry itself): conferences after today only.
  #     The audit holds every entry to scored_on < conf_date.
  #   * The FINAL forecast (the entry's `final` block): also conference
  #     morning, until FORECAST_FINAL_CUTOFF_UTC. The Justices meet at 9:30 or
  #     10:00 Eastern, 13:30 UTC at the earliest; the nightly run renders about
  #     07:00 UTC. The audit holds every final to scored_at < conf_date + cutoff.
  now <- as.POSIXct(now, tz = "UTC")
  before_cutoff <- function(cd) now < .flog_final_cutoff(cd)
  live <- dist |> dplyr::filter(type == "paid", conf_date > as_of |
                                  (conf_date == as_of & before_cutoff(conf_date)))
  n_past <- sum(dist$type == "paid") - nrow(live)
  if (!nrow(live)) {
    return(invisible(c(empty, list(skipped_past = n_past,
                                   total = length(read_forecast_log(site_dir))))))
  }

  idx <- read_forecast_log(site_dir)
  # A hash of the whole model list, so a refit is visible in the record. Same
  # quantity docket_page.R stamps into the render manifest.
  mid <- substr(digest::digest(models), 1, 16)
  scored_on <- format(as_of)
  scored_at <- format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  gc_ <- function(nm, def) if (nm %in% names(live)) live[[nm]] else rep(def, nrow(live))
  dt <- gc_("date", as.Date(NA)); ld <- gc_("lower_date", as.Date(NA))
  rel <- gc_("related", NA_character_)
  has_parties <- "parties" %in% names(live)

  added <- 0L; superseded <- 0L; finals <- 0L
  for (i in seq_len(nrow(live))) {
    key <- paste0(live$dkt[i], "@", format(live$conf_date[i]))
    prior <- idx[[key]]
    first_ok <- live$conf_date[i] > as_of
    gd <- if (all(c("outcome", "outcome_date") %in% names(dist)))
      unique(dist$dkt[dist$outcome %in% "granted" & !is.na(dist$outcome_date) &
                      dist$outcome_date < live$conf_date[i]]) else character()
    s <- tryCatch(score_conference(
      models, live$caption[i], live$lower[i],
      if (has_parties) live$parties[[i]] else NULL,
      dt[i], ld[i], rel[i], events = live$events[[i]],
      as_of = live$conf_date[i], conf_idx = live$distribution_no[i],
      granted_dockets = gd), error = function(e) NULL)
    if (is.null(s) || is.na(s$p_grant_now)) next
    nums <- list(
      p_grant_now = round(s$p_grant_now, 5),
      p_gvr_now = round(s$p_gvr_now %||% NA_real_, 5),
      p_grant_ever = round(s$p_grant_ever %||% NA_real_, 5),
      conf_idx = as.integer(s$conf_idx), n_relists = as.integer(s$n_relists),
      held = isTRUE(s$held))
    final <- c(list(scored_on = scored_on, scored_at = scored_at, model_id = mid), nums)

    # The first forecast: never overwritten -- unless it was written inside a
    # void window and has not been superseded already. Its conference is still
    # ahead (first_ok), so the re-score is as prospective as the original.
    void <- if (is.null(prior) || !is.null(prior$superseded)) NULL else .flog_void_reason(prior)
    if (first_ok && (is.null(prior) || !is.null(void))) {
      idx[[key]] <- c(list(docket = live$dkt[i], conf_date = format(live$conf_date[i]),
                           scored_on = scored_on, model_id = mid), nums)
      if (!is.null(void)) {
        keep <- prior[intersect(names(prior), c("scored_on", "model_id", "p_grant_now",
                                                "p_gvr_now", "p_grant_ever", "held"))]
        idx[[key]]$superseded <- c(keep, list(reason = void))
        superseded <- superseded + 1L
      } else added <- added + 1L
    }
    # The final forecast: rewritten every run while the conference is ahead,
    # so it ends as the last one made before it. Only on a logged entry -- a
    # petition first seen on conference morning gets neither.
    if (!is.null(idx[[key]])) {
      idx[[key]]$final <- final
      finals <- finals + 1L
    }
  }

  # An entry from a void window whose conference has passed cannot be re-scored.
  # Mark it so the scorecard skips it; the numbers stay, as what was said.
  voided <- 0L
  for (key in names(idx)) {
    e <- idx[[key]]
    if (!is.null(e$superseded) || !is.null(e$void)) next
    r <- .flog_void_reason(e)
    cd <- tryCatch(as.Date(e$conf_date), error = function(err) as.Date(NA))
    if (!is.null(r) && !is.na(cd) && cd <= as_of) { idx[[key]]$void <- r; voided <- voided + 1L }
  }
  if (superseded + voided > 0L)
    message(sprintf("append_forecasts(): %d entr(y/ies) from a void window re-scored, %d voided (conference passed)",
                    superseded, voided))

  if (added + superseded + voided + finals > 0L) {
    dir.create(dirname(.flog_path(site_dir)), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(idx, .flog_path(site_dir), auto_unbox = TRUE)
  }
  invisible(list(added = added, finals = finals, considered = nrow(live),
                 conferences = format(sort(unique(live$conf_date))),
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
  # A voided entry (broken code, conference already past) is not a forecast.
  idx <- Filter(function(e) is.null(e$void), idx)
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
