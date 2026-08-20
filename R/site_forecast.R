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
