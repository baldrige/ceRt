# site_forecast.R --------------------------------------------------------------
# "Likeliest grants" for the landing page: the week's newly-docketed paid-docket
# cases that the baseline structural model rates furthest above its own base rate.
#
# "cases", not "petitions". The paid docket carries 28 U.S.C. 1253 direct appeals
# from three-judge district courts alongside petitions for certiorari, and the
# first two entries this panel ever published were the Allen redistricting
# APPEALS -- captioned "Appellants", scored on mandatory appellate jurisdiction.
# Calling them petitions on the front page was wrong in the same way the model's
# "certiorari before judgment" cue text was; both were corrected 2026-07-30.
#
# This is the same number the daily dashboard prints in its "Grant forecast"
# column, from the same score_case() call on the same model, so the home page and
# the dashboard cannot drift apart. It is deliberately NOT a second estimate.
#
# Scope is the paid docket only (type == "paid"), matching scotus_dash(): the
# baseline model is fitted on it, and scoring an IFP docket or an application
# against that model would produce a number with no meaning behind it.

# strip_caption_roles() lives in page_style.R. build_dashboards.R happens to have
# loaded it by here -- source("R/argument_nav.R") pulls it in 125 lines earlier --
# but that is an accident of ordering this file should not be built on, and the
# call site is not wrapped, so a reorder would take the daily down rather than
# just dropping the panel. Declare the dependency, like the three dashboards do.
local({
  here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA)
  f <- if (!is.na(here) && file.exists(file.path(here, "page_style.R")))
    file.path(here, "page_style.R")
  else if (file.exists("R/page_style.R")) "R/page_style.R" else "page_style.R"
  sys.source(f, envir = globalenv())
})

# Publishing floor for the panel.
#
# A panel headed "likeliest" has to be about cases that are actually likely.
# The model's base rate is ~4.1%, and in an ordinary week every case scores
# within rounding distance of it -- so a straight top-5 would print 4.4%, 4.2%,
# 4.1%, 4.1%, 4.0% and present noise as a ranking. That is the mistake the
# most-read panel made (see R/site_analytics.R and the #20 commit): an ordering
# is only worth publishing if something produced it.
#
# The guard is LIFT, not probability, because lift is the model's own vocabulary
# for "how far above the floor is this" -- score_features() already returns it,
# and it stays correct if the base rate moves when the model is refitted. A
# hard-coded 8% threshold would silently become the wrong threshold the first
# time the corpus grows.
#
# Raise these as the model sharpens -- they are deliberately a one-line edit.
FORECAST_WINDOW_DAYS <- 7L    # trailing days of docketing to consider
FORECAST_WINDOW_LONG <- 28L   # the wider window the front page can toggle to
FORECAST_MIN_LIFT    <- 2.0   # x base rate required, per entry
FORECAST_MIN_ENTRIES <- 3L    # below this it is not a list, it is a coincidence

# The top `n` paid-docket cases filed in the trailing `days`, as a data frame of
# dkt / caption / prob / lift / href. Zero rows means "render nothing": no model,
# no cases in the window, or nothing clearing the floor.
#
# Non-fatal by design, like top_viewed_cases(). This is one decorative panel on a
# pipeline whose actual job is publishing dockets every morning; a scoring error
# must not take the daily down with it. The failure is loud in the workflow log.
top_forecast_cases <- function(cases, model, site_dir, signals_map = NULL,
                               counsel_index = NULL, n = 5L,
                               days = FORECAST_WINDOW_DAYS,
                               as_of = Sys.Date()) {
  none <- data.frame(dkt = character(), caption = character(), prob = numeric(),
                     lift = numeric(), href = character(), stringsAsFactors = FALSE)

  if (is.null(model)) {
    message("top_forecast_cases(): no baseline model -- skipping the panel.")
    return(none)
  }
  base <- model$base_rate
  if (is.null(base) || !is.finite(base) || base <= 0) {
    warning("top_forecast_cases(): model has no usable base_rate -- ",
            "skipping the panel.", call. = FALSE)
    return(none)
  }
  if (is.null(cases) || !nrow(cases)) return(none)

  from <- as.Date(as_of) - days
  w <- cases[!is.na(cases$date) & !is.na(cases$type) &
             cases$type == "paid" &
             as.Date(cases$date) > from & as.Date(cases$date) <= as.Date(as_of), ,
             drop = FALSE]
  if (!nrow(w)) {
    message("top_forecast_cases(): no paid-docket cases filed in the last ",
            days, " days -- skipping the panel.")
    return(none)
  }
  w <- w[!duplicated(w$dkt), , drop = FALSE]

  # Score one at a time so a single bad row cannot lose the whole panel.
  probs <- vapply(seq_len(nrow(w)), function(i) tryCatch(
    score_case(model, w$caption[i], w$lower[i], w$parties[[i]], w$date[i],
               w$lower_date[i], w$related[i],
               signals = if (!is.null(signals_map)) signals_map[[w$dkt[i]]] else NULL,
               counsel_index = counsel_index)$prob,
    error = function(e) NA_real_), numeric(1))

  n_scored <- sum(!is.na(probs))
  if (n_scored < nrow(w))
    message("top_forecast_cases(): ", nrow(w) - n_scored, " of ", nrow(w),
            " cases in the window could not be scored.")

  # strip_caption_roles() is page_style.R's, shared with the three dashboards so
  # the same case reads the same way here as it does on the page this links to.
  cap <- strip_caption_roles(w$caption)
  cap <- ifelse(is.na(cap) | !nzchar(cap), w$dkt, cap)   # a caption is never required
  df <- data.frame(dkt = w$dkt, caption = cap, prob = probs,
                   stringsAsFactors = FALSE)
  df <- df[!is.na(df$prob), , drop = FALSE]
  if (!nrow(df)) return(none)
  df$lift <- df$prob / base
  df <- df[order(-df$prob, df$dkt), , drop = FALSE]

  # A case can be scored and have no page (a renumbered docket, a render this
  # run skipped). Linking it would publish a 404, so drop before taking the top n.
  df <- df[file.exists(file.path(site_dir, "cases", paste0(df$dkt, ".html"))), ,
           drop = FALSE]
  if (!nrow(df)) return(none)

  # Apply the floor, and say so loudly when it bites: a panel that is dark
  # because the week was quiet must be distinguishable in the log from one that
  # is dark because something broke.
  ok <- df$lift >= FORECAST_MIN_LIFT
  if (sum(ok) < FORECAST_MIN_ENTRIES) {
    top <- utils::head(df, 3L)
    message(sprintf(
      paste0("top_forecast_cases(): panel SUPPRESSED -- %d of %d cases ",
             "clear %.1fx the %.1f%% base rate; need >=%d. Best this week: %s"),
      sum(ok), nrow(df), FORECAST_MIN_LIFT, 100 * base, FORECAST_MIN_ENTRIES,
      paste(sprintf("%s (%.1f%%, %.1fx)", top$dkt, 100 * top$prob, top$lift),
            collapse = ", ")))
    return(none)
  }
  df <- df[ok, , drop = FALSE]

  out <- utils::head(df, n)
  out$href <- paste0("cases/", out$dkt, ".html")
  message(sprintf(
    "top_forecast_cases(): %d of %d cases clear %.1fx over %dd; showing %d: %s",
    nrow(df), nrow(w), FORECAST_MIN_LIFT, days, nrow(out),
    paste(sprintf("%s (%.1f%%, %.1fx)", out$dkt, 100 * out$prob, out$lift),
          collapse = ", ")))
  rownames(out) <- NULL
  out[, c("dkt", "caption", "prob", "lift", "href")]
}

# ---- "All pending": the third window ----------------------------------------------
#
# The daily fetches the trailing ~50 dockets of each bucket, so the two windows
# above can only ever rank the last four weeks of filings. Every pending
# paid-docket case the site holds is a question only the weekly conferences run
# can answer -- it fetches the current and prior Terms in full -- so, as with
# the calendar and the decisions, that run writes a manifest and the daily
# reads it (docs/recent-decisions.md, "Data flow", for the pattern).
#
# The manifest goes stale inside a week: a Monday order list denies dozens of
# the petitions it names. So the daily does not trust it. It re-fetches the top
# PENDING_VERIFY dockets by name (a couple of dozen paced requests), drops any
# the docket now says is granted, denied or otherwise disposed of, and shows
# the top PENDING_SHOW that survive. A docket that could not be fetched is kept:
# a throttled run should cost the window a stale row, not the whole window.
#
# The number is the same petition-stage, structural estimate the other two
# windows print, from the same score_case() call, so the three windows rank on
# one scale. The conference-stage estimate (relists, a reply, a CVSG) is a
# different number, printed where it belongs -- on the conference reports and
# the docket pages -- and not mixed in here.
PENDING_FORECASTS <- "pending_forecasts.json"   # under conferences/
PENDING_KEEP   <- 40L    # rows the weekly writes
PENDING_VERIFY <- 25L    # rows the daily re-fetches by name
PENDING_SHOW   <- 10L    # rows the window shows

.pending_df <- function() data.frame(dkt = character(), caption = character(), date = as.Date(character()),
                                     prob = numeric(), lift = numeric(), stringsAsFactors = FALSE)

#' Score every pending paid-docket case in `cases` with the baseline model:
#' one row per case, sorted by probability, `lift` against the model's base
#' rate. The weekly writes the top of this to the manifest; the daily scores
#' its own fetch window with it and merges the result into the manifest rows,
#' because a case docketed after the weekly's fetch is otherwise invisible to
#' the "All pending" window until the next weekly -- 26-304 led the 7-day
#' window at 80% on the day it was docketed and was absent from the window
#' beside it.
score_pending_cases <- function(cases, model, site_dir, counsel_index = NULL) {
  if (is.null(model) || is.null(cases) || !nrow(cases) || !exists("classify_petitions")) return(.pending_df())
  base <- model$base_rate
  if (is.null(base) || !is.finite(base) || base <= 0) return(.pending_df())
  cls <- tryCatch(classify_petitions(cases), error = function(e) NULL)
  if (is.null(cls) || !nrow(cls)) return(.pending_df())
  pend <- cls$dkt[cls$type == "paid" & cls$outcome %in% "pending"]
  w <- cases[cases$dkt %in% pend, , drop = FALSE]
  w <- w[!duplicated(w$dkt), , drop = FALSE]
  if (!nrow(w)) return(.pending_df())
  # The Rule 10 signals, merged the way render_dockets_for() merges them.
  signals_map <- tryCatch(jsonlite::fromJSON("data-raw/petition_signals.json", simplifyVector = FALSE),
                          error = function(e) list())
  cache_p <- file.path(site_dir, "dashboards", "petition_signals_cache.json")
  if (file.exists(cache_p)) {
    fresh <- tryCatch(jsonlite::fromJSON(cache_p, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(fresh) && length(fresh)) signals_map[names(fresh)] <- fresh
  }
  probs <- vapply(seq_len(nrow(w)), function(i) tryCatch(
    score_case(model, w$caption[i], w$lower[i], w$parties[[i]], w$date[i],
               w$lower_date[i], w$related[i], signals = signals_map[[w$dkt[i]]],
               counsel_index = counsel_index)$prob,
    error = function(e) NA_real_), numeric(1))
  cap <- strip_caption_roles(w$caption)
  cap <- ifelse(is.na(cap) | !nzchar(cap), w$dkt, cap)
  df <- data.frame(dkt = w$dkt, caption = cap, date = as.Date(w$date), prob = probs,
                   stringsAsFactors = FALSE)
  df <- df[!is.na(df$prob), , drop = FALSE]
  df$lift <- df$prob / base
  df <- df[order(-df$prob, df$dkt), , drop = FALSE]
  rownames(df) <- NULL
  df
}

#' The weekly's manifest: the top PENDING_KEEP of score_pending_cases(),
#' written to `path`. Event dates only, never a build time.
write_pending_forecasts <- function(cases, model, site_dir, counsel_index = NULL,
                                    path = file.path(site_dir, "conferences", PENDING_FORECASTS),
                                    keep = PENDING_KEEP) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  all <- score_pending_cases(cases, model, site_dir, counsel_index)
  df <- utils::head(all, keep)
  message(sprintf("write_pending_forecasts(): %d pending paid-docket case(s) scored; kept %d (top %s %.1f%%, %.1fx)",
                  nrow(all), nrow(df), if (nrow(df)) df$dkt[1] else "-", if (nrow(df)) 100 * df$prob[1] else 0,
                  if (nrow(df)) df$lift[1] else 0))
  df$date <- format(as.Date(df$date), "%Y-%m-%d")
  jsonlite::write_json(df, path, auto_unbox = TRUE, dataframe = "rows", na = "null", digits = 6)
  invisible(nrow(df))
}

#' Manifest rows plus the daily's own freshly scored window, one row per
#' docket. A docket in both takes this run's score: it was fetched today, and
#' the manifest's copy is up to a week old.
merge_pending_forecasts <- function(manifest, fresh) {
  if (is.null(fresh) || !nrow(fresh)) return(manifest)
  if (is.null(manifest) || !nrow(manifest)) return(fresh)
  out <- rbind(fresh, manifest[!manifest$dkt %in% fresh$dkt, , drop = FALSE])
  out <- out[order(-out$prob, out$dkt), , drop = FALSE]
  rownames(out) <- NULL
  out
}

read_pending_forecasts <- function(path) {
  if (!file.exists(path)) return(.pending_df())
  j <- tryCatch(jsonlite::fromJSON(path, simplifyDataFrame = TRUE), error = function(e) NULL)
  if (is.null(j) || !is.data.frame(j) || !nrow(j) || !all(c("dkt", "caption", "prob", "lift") %in% names(j))) return(.pending_df())
  data.frame(dkt = as.character(j$dkt), caption = as.character(j$caption),
             date = as.Date(if ("date" %in% names(j)) j$date else NA),
             prob = as.numeric(j$prob), lift = as.numeric(j$lift), stringsAsFactors = FALSE)
}

#' The rows of `rows` whose docket, as fetched in `fetched`, is still pending.
#' A docket absent from `fetched` is kept (unverified, and said so in the log).
verify_pending <- function(rows, fetched) {
  if (!nrow(rows)) return(rows)
  if (is.null(fetched) || !nrow(fetched) || !exists("classify_petition_events")) {
    message("verify_pending(): nothing fetched -- ", nrow(rows), " row(s) unverified")
    return(rows)
  }
  gone <- character()
  for (d in intersect(rows$dkt, fetched$dkt)) {
    ev <- fetched$events[[match(d, fetched$dkt)]]
    cl <- tryCatch(classify_petition_events(ev), error = function(e) NULL)
    if (!is.null(cl) && !identical(cl$outcome[[1]], "pending")) gone <- c(gone, d)
  }
  unverified <- setdiff(rows$dkt, fetched$dkt)
  message(sprintf("verify_pending(): %d checked, %d no longer pending%s, %d unverified",
                  length(intersect(rows$dkt, fetched$dkt)), length(gone),
                  if (length(gone)) paste0(" (", paste(gone, collapse = ", "), ")") else "",
                  length(unverified)))
  rows[!rows$dkt %in% gone, , drop = FALSE]
}

#' The "All pending" window: the same floor as the other two, pages that
#' exist, the top `n`.
pending_forecast_rows <- function(rows, site_dir, n = PENDING_SHOW) {
  none <- data.frame(dkt = character(), caption = character(), prob = numeric(),
                     lift = numeric(), href = character(), stringsAsFactors = FALSE)
  if (is.null(rows) || !nrow(rows)) return(none)
  df <- rows[order(-rows$prob, rows$dkt), , drop = FALSE]
  df <- df[file.exists(file.path(site_dir, "cases", paste0(df$dkt, ".html"))), , drop = FALSE]
  ok <- df$lift >= FORECAST_MIN_LIFT
  if (sum(ok) < FORECAST_MIN_ENTRIES) {
    message(sprintf("pending_forecast_rows(): window SUPPRESSED -- %d of %d clear %.1fx", sum(ok), nrow(df), FORECAST_MIN_LIFT))
    return(none)
  }
  out <- utils::head(df[ok, , drop = FALSE], n)
  out$href <- paste0("cases/", out$dkt, ".html")
  rownames(out) <- NULL
  out[, c("dkt", "caption", "prob", "lift", "href")]
}
