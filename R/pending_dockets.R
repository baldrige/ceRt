# pending_dockets.R -------------------------------------------------------------
# Naming the live stragglers, so an old docket does not have to cost a whole Term.
#
# THE PROBLEM. The conferences run fetches the current Term and the prior one, by
# docket-number RANGE. A petition is granted or denied in the Term it was filed in
# 75.8% of the time and one Term later 23.5% of the time -- but the remaining
# 0.7% are the long-held, heavily-relisted cases, which is to say exactly the ones
# a relist leaderboard is about. Measured over OT2017-OT2024, a {T, T-1} window
# loses 9 such cases, and 2 of the 24 most-relisted petitions in eight Terms.
# 19-333 (Arlene's Flowers) was relisted 12 times; 8 of those relists happened in
# a Term the window would not have loaded.
#
# THE CHEAP FIX. Those cases can be NAMED. Widening to a third Term costs ~5,000
# extra requests; the set of dockets that are (a) still undisposed and (b) older
# than the window has a median size of 8 and a measured maximum of 13. So instead
# of a Term, fetch a list.
#
# WHY IT SPLITS THIS WAY. You can only target a docket you already know exists.
# New petitions arrive constantly with fresh numbers in the current Term's bucket,
# and binary_search_max() is what discovers them -- so the current and prior Terms
# must stay RANGE fetches. Older Terms never gain new dockets, so every one of
# them is already known and targeting is exact rather than approximate.
#
# THE CACHE. cases/pending.json is a flat object keyed by docket, which is the
# shape publish_site.sh's DERIVED union resolution merges correctly. It is
# rewritten wholesale each conferences run from what that run actually classified,
# so it is self-correcting: a docket that has since been disposed of simply stops
# being written. A stale entry costs exactly one request until it resolves.
#
# THE AGE-OUT. A petition that never receives a recognised disposition stays
# "pending" forever -- the funnel's own canary puts that at 0.1-0.5% of a closed
# Term. Without a bound, a classifier gap would quietly become a growing fetch
# list. An entry whose docket has seen no activity for PENDING_MAX_IDLE_DAYS is
# dropped: no activity in two years is dormant, not live. Dropping it is visible
# in the run log rather than silent, because that count is also the canary for a
# classifier that has started missing dispositions.

suppressPackageStartupMessages(library(jsonlite))

# funnel_case_type() lives in cert_funnel.R. Sourced explicitly rather than
# assumed: every entry point that relied on transitive sourcing in this repo has
# eventually lost a feature to it silently.
if (!exists("funnel_case_type")) {
  local({
    f <- if (file.exists("R/cert_funnel.R")) "R/cert_funnel.R" else "cert_funnel.R"
    if (file.exists(f)) sys.source(f, envir = globalenv())
  })
}

PENDING_CACHE <- "cases/pending.json"

# Two years. Long enough to cover the longest genuine relist run measured
# (17-1511: 858 days between first and last relist) with room to spare, short
# enough that a mis-classified disposition ages out rather than accumulating.
PENDING_MAX_IDLE_DAYS <- 730L

.pending_path <- function(site_dir) file.path(site_dir, PENDING_CACHE)

read_pending <- function(site_dir) {
  p <- .pending_path(site_dir)
  if (!file.exists(p)) return(list())
  tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) {
    warning("read_pending(): ", basename(p), " unreadable -- treating as empty.",
            call. = FALSE)
    list()
  })
}

# The date of a case's most recent docket entry. This is the activity clock the
# age-out runs on -- not the docketing date, which never moves.
.last_event_date <- function(events) {
  if (!is.data.frame(events) || !("Date" %in% names(events)) || nrow(events) == 0)
    return(as.Date(NA))
  d <- suppressWarnings(lubridate::mdy(events$Date))
  if (all(is.na(d))) as.Date(NA) else max(d, na.rm = TRUE)
}

# Rebuild the cache from `cases`, which must be everything the run classified.
#
# Rewritten wholesale, NOT merged: the whole point is that a docket disposed of
# since the last run stops being listed. Merging would make the cache
# append-only, which is how a fetch list grows without bound.
#
# Entries are only dropped for a case the run actually SAW. A docket in the old
# cache that this run did not fetch is carried forward untouched -- otherwise a
# fetch that lost a docket to throttling would silently retire a live case.
update_pending <- function(site_dir, cases, classify = NULL, as_of = Sys.Date()) {
  if (is.null(classify)) {
    if (!exists("classify_petition_events")) {
      warning("update_pending(): classify_petition_events() not available.",
              call. = FALSE)
      return(invisible(list(kept = 0L, dropped_resolved = 0L, dropped_idle = 0L)))
    }
    classify <- get("classify_petition_events")
  }
  as_of <- as.Date(as_of)
  old <- read_pending(site_dir)
  seen <- character()
  new <- list()

  if (!is.null(cases) && nrow(cases)) {
    for (i in seq_len(nrow(cases))) {
      dkt <- cases$dkt[i]
      # Applications (NNA###) are excluded, exactly as classify_petitions() does.
      # Their dispositions are not in the grant/deny grammar, so the classifier
      # calls almost all of them "pending" forever -- seeding OT17-20 without this
      # filter produced a 496-docket fetch list, of which 18A1000..18A1013 and
      # their siblings were the bulk. They are also not what this cache is for: an
      # application is resolved in days and never relisted, so it can never be a
      # long-lived out-of-window straggler.
      # Original actions (22O###) likewise: a motion for leave to file a bill of
      # complaint is not a petition, and the original docket has its own by-name
      # fetch list (cases/original.json, R/original_dockets.R).
      if (!funnel_case_type(dkt) %in% c("paid", "ifp")) next
      seen <- c(seen, dkt)
      cl <- tryCatch(classify(cases$events[[i]]), error = function(e) NULL)
      if (is.null(cl) || !identical(cl$outcome[[1]], "pending")) next
      last <- .last_event_date(cases$events[[i]])
      new[[dkt]] <- list(
        last_event = if (is.na(last)) NA_character_ else format(last),
        n_relists  = as.integer(cl$n_relists[[1]]),
        first_dist = if (is.na(cl$first_dist[[1]])) NA_character_
                     else format(cl$first_dist[[1]]))
    }
  }

  # Carry forward anything this run did not look at.
  carried <- setdiff(names(old), seen)
  for (dkt in carried) new[[dkt]] <- old[[dkt]]

  # Age out the dormant.
  idle <- vapply(names(new), function(d) {
    le <- new[[d]]$last_event
    if (is.null(le) || is.na(le) || !nzchar(le)) return(TRUE)   # no clock -> drop
    as.numeric(as_of - as.Date(le)) > PENDING_MAX_IDLE_DAYS
  }, logical(1))
  dropped_idle <- sum(idle)
  new <- new[!idle]

  dir.create(dirname(.pending_path(site_dir)), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(new, .pending_path(site_dir), auto_unbox = TRUE)

  invisible(list(kept = length(new),
                 dropped_resolved = length(setdiff(names(old), c(names(new), carried))),
                 dropped_idle = dropped_idle,
                 carried = length(carried)))
}

# The dockets a run must NAME: live, and from a Term the range fetch will not
# cover. `window_terms` is the two-digit Terms being range-fetched this run.
#
# Returns them oldest-first so a truncated fetch keeps the longest-lived cases,
# which are the ones the leaderboard is about.
pending_to_fetch <- function(site_dir, window_terms, max_n = 500L) {
  idx <- read_pending(site_dir)
  if (!length(idx)) return(character())
  dkt <- names(idx)
  term <- suppressWarnings(as.integer(substr(dkt, 1, 2)))
  wt <- suppressWarnings(as.integer(window_terms))
  keep <- !is.na(term) & !(term %in% wt)
  dkt <- dkt[keep]; term <- term[keep]
  if (!length(dkt)) return(character())
  dkt <- dkt[order(term, dkt)]
  if (length(dkt) > max_n) {
    # Never silently truncate: the cap exists so a corrupted cache cannot turn
    # into a multi-thousand-request fetch, and if it ever fires that is news.
    warning("pending_to_fetch(): ", length(dkt), " dockets exceeds the cap of ",
            max_n, " -- fetching the ", max_n, " oldest. A list this size means ",
            "the classifier has stopped recognising dispositions.", call. = FALSE)
    dkt <- head(dkt, max_n)
  }
  dkt
}
