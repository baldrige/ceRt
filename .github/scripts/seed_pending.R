# One-off: seed cases/pending.json with live dockets from Terms the weekly
# conferences run has never loaded.
#
# WHY THIS IS NEEDED. update_pending() can only record a docket the run actually
# fetched, and the conferences run range-fetches {T, T-1}. So the cache it seeded
# on 2026-08-08 held 1,006 live dockets, every one of them Term 25 or 26 -- and
# "next run will name 0 out-of-window docket(s)", because nothing in it is out of
# window yet. The mechanism is armed but idle: it starts naming things only when
# the window advances past a Term whose petitions are still live, around July
# 2027. Meanwhile any petition from Term <= 24 that is live TODAY is invisible,
# and those are precisely the cases the targeted fetch exists for -- the current
# equivalents of 17-1511, which was relisted 10 times across three Terms.
#
# HOW. data-raw/ot_*.rds covers OT2017-OT2024. Those snapshots are stale (Feb and
# Jul 2025), but staleness does not matter here: the archives are only used to
# decide WHICH DOCKETS TO ASK ABOUT. Each candidate is then fetched live and
# re-classified, so the cache is written from current truth. A petition disposed
# of since the snapshot is fetched once, seen to be resolved, and never written.
#
# That is the whole reason this is cheap. Getting the same information by
# re-fetching Terms 17-24 would be ~49,000 requests; asking the archives first
# reduces it by two orders of magnitude.
#
# EXPECT THE CANDIDATE LIST TO BE MUCH LARGER THAN WHAT GETS WRITTEN, and do not
# read that as a bug. ot_2024.rds was captured mid-Term (its latest docket event
# is 2025-06-18), when ~690 OT24 petitions really were still pending; a year on,
# nearly all have been denied. So the run fetches ~700 dockets -- about six
# minutes at the 2 req/s cap -- and writes the handful still live. The
# out-of-window live set has a measured median of 8 and max of 13, so a result in
# that range is the expected one. A result in the hundreds would mean the
# classifier has stopped recognising denials, which is worth stopping for.
#
# The existing cache is preserved: update_pending() carries forward any docket the
# run did not see, and this run sees only the archive candidates, so the 1,006
# in-window entries survive untouched.
#
# Env: SITE_DIR (gh-pages checkout, default "site"), MAX_FETCH (safety cap,
#      default 400), DRY_RUN ("true" reports and writes nothing).

suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse)
  library(httr2); library(jsonlite); library(pdftools); library(htmltools)
})

site <- Sys.getenv("SITE_DIR", unset = "site")
# Sized to cover the whole archive candidate list (~710 as of 2026-08) rather
# than truncate it: a truncated seed silently omits live cases, which is the
# failure this whole mechanism exists to prevent. It is a runaway guard, not a
# budget.
max_fetch <- as.integer(Sys.getenv("MAX_FETCH", unset = "1200"))
dry <- tolower(Sys.getenv("DRY_RUN", "")) %in% c("1", "true", "yes")

src <- readLines("R/scotus_dash_new.R"); src <- src[-grep("^scotus_dash\\(", src)]
eval(parse(text = paste(src, collapse = "\n")))
source("R/cert_funnel.R")
source("R/pending_dockets.R")

paths <- sort(Sys.glob("data-raw/ot_*.rds"))
if (!length(paths)) stop("no archives matched data-raw/ot_*.rds")
cat("Archives:", paste(basename(paths), collapse = ", "), "\n")

arch <- paths |> map(readRDS) |> bind_rows()
cat("Archive petitions:", nrow(arch), "\n")

before <- read_pending(site)
cat("Existing cache:", length(before), "docket(s)\n")

# Candidates: not an application, classified pending in the snapshot, and with
# docket activity recent enough to survive the same age-out the live cache uses.
# The age-out is what keeps this from naming a decade of dormant petitions: a
# case whose last docket entry is from 2019 is not a straggler, it is over.
cand <- arch |>
  filter(funnel_case_type(dkt) != "app") |>
  mutate(cls = map(events, ~ tryCatch(classify_petition_events(.x),
                                      error = function(e) NULL))) |>
  filter(!map_lgl(cls, is.null)) |>
  mutate(outcome = map_chr(cls, ~ .x$outcome[[1]]),
         n_relists = map_int(cls, ~ as.integer(.x$n_relists[[1]])),
         last_event = as.Date(map_dbl(events, function(e) {
           d <- suppressWarnings(lubridate::mdy(e$Date))
           if (all(is.na(d))) NA_real_ else as.numeric(max(d, na.rm = TRUE))
         }), origin = "1970-01-01")) |>
  filter(outcome == "pending", !is.na(last_event),
         as.numeric(Sys.Date() - last_event) <= PENDING_MAX_IDLE_DAYS) |>
  # Already in the cache means the weekly run is already tracking it.
  filter(!(dkt %in% names(before))) |>
  arrange(desc(n_relists), dkt)

cat("Candidates (pending in snapshot, active within",
    PENDING_MAX_IDLE_DAYS, "days, not already cached):", nrow(cand), "\n")
cat("  NB: these were pending when the SNAPSHOT was taken. Most will have been\n")
cat("  disposed of since; the live fetch below decides. Expect single or low\n")
cat("  double digits to survive.\n")
if (nrow(cand)) {
  cat("  by Term:", paste(sprintf("%s=%d", names(table(substr(cand$dkt, 1, 2))),
                                  as.integer(table(substr(cand$dkt, 1, 2)))),
                          collapse = " "), "\n")
  cat("  most-relisted:",
      paste(sprintf("%s(%d)", head(cand$dkt, 10), head(cand$n_relists, 10)),
            collapse = ", "), "\n")
}

if (!nrow(cand)) {
  cat("Nothing to seed. The archives hold no live, recently-active petition that\n")
  cat("the cache is not already tracking.\n")
  quit(status = 0)
}

if (nrow(cand) > max_fetch) {
  cat("Candidate list (", nrow(cand), ") exceeds MAX_FETCH=", max_fetch,
      "; taking the most-relisted ", max_fetch, ".\n", sep = "")
  cand <- head(cand, max_fetch)
}

if (dry) {
  cat(sprintf("\nDRY RUN -- would fetch %d docket(s) (~%.0f min at the 2 req/s cap) ",
              nrow(cand), nrow(cand) / 2 / 60))
  cat("and write nothing.\n")
  cat("Most-relisted 25 of them:\n  ",
      paste(sprintf("%s(%d relists)", head(cand$dkt, 25), head(cand$n_relists, 25)),
            collapse = ", "), "\n", sep = "")
  quit(status = 0)
}

# Fetch live. The snapshot said "pending"; only the API knows whether that is
# still true, and the cache must be written from the fresh events, not the stale
# ones -- otherwise the first weekly run would immediately have to retire most of
# what this wrote.
cat("\nFetching", nrow(cand), "candidate(s) live...\n")
fresh <- tryCatch(fetch_cases(cand$dkt), error = function(e) {
  cat("fetch failed:", conditionMessage(e), "\n"); NULL
})
if (is.null(fresh) || !nrow(fresh)) {
  cat("nothing resolved; cache unchanged\n")
  quit(status = 0)
}
cat("fetched:", nrow(fresh), "of", nrow(cand),
    "| unresolved:", attr(fresh, "n_failed") %||% 0, "\n")

st <- update_pending(site, fresh)
after <- read_pending(site)
added <- setdiff(names(after), names(before))
cat(sprintf("\nCache: %d -> %d docket(s) [carried unseen: %d | aged out: %d]\n",
            length(before), length(after), st$carried, st$dropped_idle))
cat("Seeded", length(added), "docket(s):",
    if (length(added)) paste(added, collapse = ", ") else "(none still live)", "\n")

# The number that says whether this changed anything: the weekly run range-fetches
# the current and prior Term, so everything seeded here should be out-of-window.
wt <- sort(unique(substr(names(after), 1, 2)), decreasing = TRUE)[1:2]
cat("Out-of-window for a {", paste(wt, collapse = ","), "} window: ",
    length(pending_to_fetch(site, wt)), " docket(s)\n", sep = "")
