# Combine the per-term case artifacts in $CASES_DIR, then render the conferences
# on/after $MIN_CONF_DATE into $SITE_DIR/conferences and rebuild the index over
# the whole directory (so the refreshed conferences merge with the archive).
# Each case's Question Presented is resolved through a persistent, incremental
# cache (site/conferences/qp_cache.json) so the petition PDFs are only ever
# fetched once; QP_MAX_NEW caps how many uncached petitions are pulled per run.
#
# Env: SITE_DIR (default "site"), CASES_DIR (default "cases"), MIN_CONF_DATE,
#      QP_MAX_NEW (default 600).

suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse)
  library(jsonlite); library(htmltools); library(pdftools)
})

site_dir <- Sys.getenv("SITE_DIR", unset = "site")
cases_dir <- Sys.getenv("CASES_DIR", unset = "cases")
min_conf <- as.Date(Sys.getenv("MIN_CONF_DATE"))
if (is.na(min_conf)) stop("MIN_CONF_DATE not set / invalid")
qp_max_new <- as.integer(Sys.getenv("QP_MAX_NEW", unset = "600"))
conf_dir <- file.path(site_dir, "conferences")
dir.create(conf_dir, recursive = TRUE, showWarnings = FALSE)

source("R/qp_extract.R")
source("R/conference_dash.R")
source("R/site_calendar.R")   # upcoming_conferences(), write_upcoming()
source("R/cert_funnel.R")   # classify_petition_events (relist grammar)
source("R/cert_model.R")    # score_disposition + load_cert_models
source("R/argument_nav.R")  # classify_argument (docket-page lifecycle)
source("R/docket_page.R")   # render_dockets_for
cert_models <- load_cert_models("data")
cat("Cert models loaded:", paste(names(cert_models), collapse = ", "),
    if (length(cert_models) == 0) "(none — forecast column omitted)" else "", "\n")

files <- list.files(cases_dir, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE)
if (length(files) == 0) stop("no case artifacts found in ", cases_dir)
cat("Loading", length(files), "term file(s):", paste(basename(files), collapse = ", "), "\n")

# Refuse a snapshot set with a HOLE in it.
#
# conference_distributions() counts distributions across the whole `combined`
# history (see below), so a term missing from the middle of the range does not
# merely omit pages -- it silently under-counts relists for every case whose
# distributions straddle the hole, and a relist count is the headline number on
# a conference report. That is a wrong page, not a stale one.
#
# This became reachable when the fetch matrix moved to fail-fast: false and
# gained reuse_from_runs: the publish job now assembles snapshots from more than
# one run, so "did every artifact arrive" is no longer implied by "the matrix
# succeeded". Contiguity is the invariant that matters and it is cheap to check.
#
# NOT checked: that the set reaches back to OT17. The scheduled weekly run
# deliberately loads only the current and prior term, so a case relisted out of
# an older term already has its early distributions under-counted there. That is
# a pre-existing property of the design, not something reuse introduced, and
# enforcing full history here would break the weekly cron. Worth revisiting --
# it means "Relists" on a recent conference page is a floor, not a count.
.terms_present <- sort(unique(suppressWarnings(as.integer(
  sub("^cases-(\\d+)\\.rds$", "\\1", basename(files))))))
.terms_present <- .terms_present[!is.na(.terms_present)]
if (length(.terms_present) > 1) {
  .gaps <- setdiff(min(.terms_present):max(.terms_present), .terms_present)
  if (length(.gaps))
    stop("snapshot set has gap(s) at term(s) ", paste(.gaps, collapse = ", "),
         " (have ", paste(.terms_present, collapse = ", "), "). Refusing to ",
         "render: a missing term silently under-counts relists for cases that ",
         "span it. Re-dispatch with terms=", paste(.gaps, collapse = ","),
         " and reuse_from_runs=<this run id>.")
}
cat("Terms loaded:", paste(.terms_present, collapse = ", "), "(contiguous)\n")

combined <- files |> map(readRDS) |> bind_rows()

# cases-pending.rds names out-of-window stragglers by docket, and the fetch that
# builds it filters to Terms the matrix does not cover -- so an overlap should be
# impossible. Deduped anyway: a `reuse_from_runs` set can carry a Term artifact
# from an older run alongside a fresh straggler for the same docket, and a
# duplicated row would double-count that case's conference distributions. Keeps
# the copy with the most docket entries, i.e. the freshest fetch.
.n_before <- nrow(combined)
combined <- combined |>
  mutate(.n_ev = map_int(events, ~ if (is.data.frame(.x)) nrow(.x) else 0L)) |>
  arrange(dkt, desc(.n_ev)) |>
  distinct(dkt, .keep_all = TRUE) |>
  select(-.n_ev)
if (.n_before != nrow(combined))
  cat("Deduped", .n_before - nrow(combined), "duplicate docket row(s)\n")
cat("Combined cases:", nrow(combined), "\n")

# distribution_no is computed across the full combined history, then we keep only
# the target conference-date range to render.
#
# dist_all is kept unfiltered for Relist Tracker: a case's relist count and its
# last/next conference are properties of the CASE, and computing them from a
# date-windowed slice would compute them from a fragment.
dist_all <- conference_distributions(combined)

# The landing page's "Upcoming at the Court" list. dist_all, not the windowed
# `dist` below: the render window starts at min_conf and a FUTURE conference is
# always inside it, but the window is an argument the caller controls and this
# should not silently depend on it.
up_conf <- upcoming_conferences(dist_all)
write_upcoming(up_conf, file.path(conf_dir, "upcoming.json"))
cat("Upcoming conferences:", nrow(up_conf),
    if (nrow(up_conf)) paste0(" (next ", format(up_conf$date[1], "%b %e"), ")") else "", "\n")
dist <- dist_all |> filter(conf_date >= min_conf)
dates <- dist |> distinct(conf_date) |> arrange(conf_date) |> pull(conf_date)

# Resolve the QP for each distinct distributed docket (cache-backed).
#
# The set is the UNION of what the conference pages need and what the Relist
# Tracker needs, because the Tracker's rows are not a subset of the conference
# pages'. `dist` is windowed to conf_date >= min_conf; the Tracker reads
# dist_all. A petition that has been relisted and NOT yet re-scheduled has no
# distribution in the window at all, so it never entered `uniq` and rendered an
# em dash: 11 of the Tracker's first 20 rows, and precisely the interesting
# half -- a case relisted repeatedly with no next conference set is the one worth
# looking at (24-1030 has been sitting since January).
#
# min_conf is 20{max(term)}-08-01, so this would have recurred every August and
# starved the Tracker of QPs for months at a time.
#
# Bounded on purpose: only the live relisted dockets are added, not all of
# dist_all -- that would be every distribution across two terms, thousands of
# petitions, and would exhaust QP_MAX_NEW to no page's benefit.
source("R/relist_watch.R")
qp_src <- tryCatch({
  rw_dkt <- relist_watch_table(dist_all)$dkt
  bind_rows(dist |> distinct(dkt, .keep_all = TRUE),
            dist_all |> filter(dkt %in% rw_dkt) |> distinct(dkt, .keep_all = TRUE)) |>
    distinct(dkt, .keep_all = TRUE)
}, error = function(e) {
  message("QP union fell back to conference dockets only: ", conditionMessage(e))
  dist |> distinct(dkt, .keep_all = TRUE)
})
uniq <- qp_src
cat("QP set:", nrow(uniq), "docket(s) (conference window + live relisted)\n")
qp_raw <- resolve_qps(uniq$dkt, uniq$petition_url,
                      cache_path = file.path(conf_dir, "qp_cache.json"),
                      max_new = qp_max_new)
qp_map <- setNames(
  ifelse(qp_raw == "-", NA_character_, qp_details(qp_raw)),
  uniq$dkt
)
cat("QP resolved:", sum(!is.na(qp_map)), "of", length(qp_map), "distinct dockets\n")

cat("Rendering", length(dates), "conference(s) on/after", format(min_conf), "\n")
for (i in seq_along(dates)) {
  conference_dash(dist, dates[i], out_dir = conf_dir, qp_map = qp_map,
                  models = cert_models)
}
conference_index(conf_dir)

# Relist Tracker. Built from the UNFILTERED distributions, and after the conference
# loop so it shares the resolved QP cache. Never allowed to break a conference
# refresh -- the reports are the product, this is a view onto them.
tryCatch({
  # Already sourced above, for the QP union.
  rp <- relist_watch(dist_all, file.path(site_dir, "relists"),
                     qp_map = qp_map, models = cert_models)
  cat("Relist Tracker:", if (is.null(rp)) "no live relisted petitions -- not written"
      else paste(nrow(relist_watch_table(dist_all)), "live relisted petition(s)"), "\n")
}, error = function(e) message("Relist Tracker skipped: ", conditionMessage(e)))

# Docket pages for the fetched OT-sitting cases (incremental). Keeps /cases/
# current for the conference Case links.
render_dockets_for(combined, site_dir)

# The grants cache, which feeds grants.xml and the site feed.
#
# This is where grants are actually visible. The daily fetches only the trailing
# ~51 dockets of each bucket, and a petition is granted months after it is
# docketed -- so by the time it is granted its number is long outside that window.
# `combined` is the full current + prior term, which is why the cache is populated
# from here even though the feeds themselves are written by the daily.
#
# The feeds are NOT written here: they also enumerate dashboards/, which this run
# does not touch, and having one owner for the files keeps publish conflicts to
# the docket-keyed cache that publish_site.sh already knows how to union.
source("R/feeds.R")
cat("Grants cache: +", update_grants_cache(site_dir, combined), " new grant(s)\n",
    sep = "")

# The prospective forecast log: what this site says about conferences that have
# NOT happened yet, written down before the Court acts. See R/forecast_log.R --
# the future-only rule is the whole integrity of it, and it is enforced there.
#
# Never allowed to break a conference refresh: the log is a research artifact,
# the conference reports are the product.
tryCatch({
  source("R/forecast_log.R")
  fl <- append_forecasts(site_dir, dist, cert_models)
  cat(sprintf("Forecast log: +%d of %d live row(s) across %s | %d past conf row(s) skipped | %d total\n",
              fl$added, fl$considered,
              if (length(fl$conferences)) paste(fl$conferences, collapse = ", ") else "no future conference",
              fl$skipped_past, fl$total))
}, error = function(e) message("Forecast log skipped: ", conditionMessage(e)))

# The live-docket cache, which is how the NEXT run knows which out-of-window
# stragglers to name. Written from everything this run classified, and rewritten
# wholesale rather than merged -- a docket disposed of since last week has to stop
# being listed, or the fetch list only ever grows.
source("R/pending_dockets.R")
ps <- update_pending(site_dir, combined)
cat(sprintf("Pending cache: %d live docket(s) [resolved since last run: %d | aged out: %d | carried unseen: %d]\n",
            ps$kept, ps$dropped_resolved, ps$dropped_idle, ps$carried))
# The out-of-window slice is the actual fetch list for next time. Printed because
# it is the number that says whether this mechanism is doing anything, and
# because a sudden jump means the classifier has stopped recognising a
# disposition form.
nxt <- pending_to_fetch(site_dir, .terms_present)
cat("Next run will name", length(nxt), "out-of-window docket(s)",
    if (length(nxt)) paste0(": ", paste(head(nxt, 20), collapse = ", "),
                            if (length(nxt) > 20) ", ..." else "") else "", "\n")
cat("Done.\n")
