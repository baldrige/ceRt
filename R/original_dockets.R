# original_dockets.R -------------------------------------------------------------
# The original-jurisdiction docket: cases between States (and the occasional
# State v. United States) that begin in this Court. Spec and the measurements
# behind every rule here: docs/original-jurisdiction.md.
#
# WHAT THE COURT PUBLISHES. Every original case has a docket number of the form
# 22O###: a fixed "22O" prefix (every one of the 44 dockets the JSON API serves
# carries it, and their sJsonTerm is the sentinel "1922"), then a sequence number
# that runs across the whole original docket rather than per Term -- No. 65 was
# filed in 1974, No. 164 in August 2026. The JSON is served for 44 of the first
# 200 numbers (probed 2026-09-04); the rest 404. So the numbering is SPARSE and
# binary_search_max(), which assumes a contiguous bucket, cannot enumerate it.
#
# HOW THEY ARE FETCHED. Two lists, both from cases/original.json, which the
# weekly conferences run rewrites wholesale from what it fetched:
#   * the weekly re-fetches every known docket (44 requests) plus a short probe
#     above the highest known number, so a new filing is found within a week;
#   * the daily re-fetches only the LIVE ones -- any activity in the last two
#     years -- plus a three-number probe, a dozen requests at most.
# With no manifest at all (the first run), the weekly scans 1..ORIG_FIRST_SCAN_TO.
#
# WHAT THEY ARE NOT. Not petitions. funnel_case_type() returns "orig" for them,
# and every petition-side consumer -- the cert funnel, the conference reports,
# the relist tracker, the cert model, the grants feed, the pending-docket cache
# -- filters on paid/IFP explicitly rather than on "not an application". Before
# this module a 22O### docket would have been typed "paid" by its sequence
# number and fed the model a case that has no lower court.
#
# THE LIFECYCLE, measured over the 44 dockets:
#
#   motion for leave to file a bill of complaint filed     42 of 44
#     -> leave DENIED                                        24   (the usual end)
#     -> leave GRANTED                                       16
#          -> Special Master appointed                       13
#          -> report received and ordered filed              14
#          -> exceptions filed, set for argument, Argued.    16
#          -> exceptions sustained/overruled, opinion        16
#          -> decree entered                                 14
#          -> complaint or claims dismissed                   4
#
# Two things are easy to get wrong. A case can be argued more than once (No. 141
# was argued in 2018 and again in 2024, on successive interim reports), and the
# argument grammar takes the FIRST argument; that is a known limitation, noted in
# the spec. And a decree is not the end of activity: No. 65 (Texas v. New Mexico,
# 1974) has a decree and filings in 2026, which is why "live" is about recent
# activity and not about outcome.

suppressPackageStartupMessages({ library(stringr); library(jsonlite) })

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

ORIG_PREFIX          <- "22O"
ORIGINALS_CACHE      <- "cases/original.json"
ORIG_FIRST_SCAN_TO   <- 200L   # first run only: no manifest, scan the number space
ORIG_PROBE_ABOVE     <- 25L    # weekly: numbers to try above the highest known
ORIG_PROBE_DAILY     <- 3L     # daily: the same, but a new filing is rare
ORIG_LIVE_IDLE_DAYS  <- 730L   # "live" = any docket activity in the last two years

is_original_docket <- function(dkt) grepl("^\\d{2}O\\d+$", dkt %||% "")
.orig_seq <- function(dkt) suppressWarnings(as.integer(sub("^\\d{2}O", "", dkt)))

# ---- the lifecycle -------------------------------------------------------------

.ORIG_LEAVE <- "motion(s)? for leave to file (?:an? |the )?(?:amended |supplemental |second amended |first amended )?(?:bill of )?complaint"
.ORIG_LEAVE_GRANTED_RX <- regex(paste0(.ORIG_LEAVE, "[^.]*\\bgranted\\b"), ignore_case = TRUE)
.ORIG_LEAVE_DENIED_RX  <- regex(paste0(.ORIG_LEAVE, "[^.]*\\bdenied\\b"),  ignore_case = TRUE)
.ORIG_MASTER_RX  <- regex("appointed special master|is appointed as special master|special master in this case",
                          ignore_case = TRUE)
.ORIG_REPORT_RX  <- regex("report of the special master[^.]*ordered filed", ignore_case = TRUE)
.ORIG_EXC_RX     <- regex("exceptions?[^.]*\\b(sustained|overruled)\\b", ignore_case = TRUE)
.ORIG_OPINION_RX <- regex("delivered the opinion|announced the judgment|opinion per curiam|(?<![(\\[])per curiam",
                          ignore_case = TRUE)
.ORIG_DECREE_RX  <- regex("\\bdecree\\b[^.]*\\b(is |be |hereby )?entered\\b|\\bentered[^.]*\\bdecree\\b",
                          ignore_case = TRUE)
.ORIG_DISMISS_RX <- regex("^(the )?(bill of )?complaint[^.]*\\bdismissed\\b|^(the |this )?(case|action)[^.]*\\bdismissed\\b|dismissed with prejudice",
                          ignore_case = TRUE)

.orig_strip <- function(x) str_squish(str_remove_all(x, "<[^>]+>"))

#' Classify one original docket's events. `et` is the entry text, `ed` the
#' entry dates (any order). Returns a list:
#'   outcome  pending | leave_denied | leave_granted | decided | decree | dismissed
#'   date     the outcome's date (NA while pending)
#'   plus the dated stages: leave_granted, master, report, argued, decided, decree
#'
#' Resolution: a decree is the judgment and wins outright; otherwise the LATEST
#' of dismissal / decision / leave denied; otherwise leave granted; otherwise
#' pending. Latest, because the docket keeps going after a decision -- No. 142
#' (Florida v. Georgia) was decided and remanded in 2018 and dismissed in 2021.
classify_original_events <- function(et, ed) {
  none <- list(outcome = "pending", date = as.Date(NA), leave_granted = as.Date(NA),
               master = as.Date(NA), report = as.Date(NA), argued = as.Date(NA),
               decided = as.Date(NA), decree = as.Date(NA))
  if (!length(et)) return(none)
  et <- .orig_strip(ifelse(is.na(et), "", et)); ed <- as.Date(ed)
  ord <- order(ed); et <- et[ord]; ed <- ed[ord]
  first_on <- function(rx) { i <- which(str_detect(et, rx) & !is.na(ed)); if (length(i)) ed[i[1]] else as.Date(NA) }
  last_on  <- function(rx) { i <- which(str_detect(et, rx) & !is.na(ed)); if (length(i)) ed[i[length(i)]] else as.Date(NA) }

  out <- none
  out$leave_granted <- first_on(.ORIG_LEAVE_GRANTED_RX)
  out$master  <- first_on(.ORIG_MASTER_RX)
  out$report  <- last_on(.ORIG_REPORT_RX)
  out$argued  <- first_on(regex("^Argued\\.", ignore_case = TRUE))
  out$decided <- last_on(.ORIG_EXC_RX)
  op <- last_on(.ORIG_OPINION_RX)
  if (is.na(out$decided) || (!is.na(op) && op > out$decided)) out$decided <- op
  out$decree  <- last_on(.ORIG_DECREE_RX)
  denied    <- last_on(.ORIG_LEAVE_DENIED_RX)
  dismissed <- last_on(.ORIG_DISMISS_RX)

  # Latest wins. A dismissal counts only where there is no decree: after a decree
  # the Court tidies up ("the United States' claims ... are dismissed with
  # prejudice", No. 141, a fortnight after its 2026 decree) and the decree is
  # still the judgment. Ties go to the earlier name in the vector: No. 142 was
  # decided and dismissed in one entry, and "Dismissed" is what happened.
  # A denied motion for leave ends a case only when no motion for leave was ever
  # granted: No. 1 (Wisconsin v. Illinois, 1922) has a 1980 decree and a later
  # denied motion for leave to file an amended complaint, and it is a decree.
  cand <- c(dismissed = if (is.na(out$decree)) dismissed else as.Date(NA),
            decree = out$decree, decided = out$decided,
            leave_denied = if (is.na(out$leave_granted) && is.na(out$decree)) denied else as.Date(NA))
  cand <- cand[!is.na(cand)]
  if (length(cand)) {
    k <- which.max(cand)
    out$outcome <- names(cand)[k]; out$date <- as.Date(cand[[k]])
    return(out)
  }
  if (!is.na(out$leave_granted)) { out$outcome <- "leave_granted"; out$date <- out$leave_granted }
  out
}

#' The disposition-box word for an original docket, from its classification.
#' A granted motion is a beginning, not an end, so the word says where the case
#' has got to since: before the Special Master, or the report on file.
original_status_word <- function(oc) {
  switch(oc$outcome,
    pending       = "Motion for leave to file pending",
    leave_denied  = "Leave to file denied",
    decided       = "Decided",
    decree        = "Decree entered",
    dismissed     = "Dismissed",
    leave_granted = if (!is.na(oc$report)) "Special Master’s report filed"
                    else if (!is.na(oc$master)) "Before the Special Master"
                    else "Leave to file granted",
    "Original action")
}

#' The date the disposition box shows beside the word.
original_status_date <- function(oc) {
  if (identical(oc$outcome, "leave_granted")) {
    if (!is.na(oc$report)) return(oc$report)
    if (!is.na(oc$master)) return(oc$master)
  }
  oc$date
}

# ---- the manifest ---------------------------------------------------------------
#
# cases/original.json: {dkt: {caption, last_event, outcome}}, every original
# docket the site has ever fetched. Rewritten wholesale by the weekly run from
# what it fetched, carrying forward anything it did not see; read by the daily
# for its by-name fetch. The daily never writes it -- two writers on different
# schedules is a race, and the weekly's copy is the complete one.

.originals_path <- function(site_dir) file.path(site_dir, ORIGINALS_CACHE)

read_originals <- function(site_dir) {
  p <- .originals_path(site_dir)
  if (!file.exists(p)) return(list())
  tryCatch(fromJSON(p, simplifyVector = FALSE), error = function(e) {
    warning("read_originals(): ", basename(p), " unreadable -- treating as empty.", call. = FALSE)
    list()
  })
}

.orig_last_event <- function(events) {
  if (!is.data.frame(events) || !nrow(events) || !("Date" %in% names(events))) return(as.Date(NA))
  d <- suppressWarnings(lubridate::mdy(events$Date))
  if (all(is.na(d))) as.Date(NA) else max(d, na.rm = TRUE)
}

#' Rewrite the manifest from the original dockets in `cases` (other dockets are
#' ignored), carrying forward entries this run did not fetch.
update_originals <- function(site_dir, cases, as_of = Sys.Date()) {
  old <- read_originals(site_dir)
  new <- list(); seen <- character()
  if (!is.null(cases) && nrow(cases)) {
    keep <- which(is_original_docket(cases$dkt))
    for (i in keep) {
      dkt <- cases$dkt[i]; ev <- cases$events[[i]]
      oc <- tryCatch(classify_original_events(ev[["Proceedings and Orders"]],
                                              suppressWarnings(lubridate::mdy(ev$Date))),
                     error = function(e) list(outcome = NA_character_))
      last <- .orig_last_event(ev)
      cap <- cases$caption[i]
      if (exists("strip_caption_roles")) cap <- get("strip_caption_roles")(cap)
      new[[dkt]] <- list(
        caption    = if (is.na(cap) || !nzchar(cap)) dkt else cap,
        last_event = if (is.na(last)) NA_character_ else format(last),
        outcome    = oc$outcome %||% NA_character_)
      seen <- c(seen, dkt)
    }
  }
  carried <- setdiff(names(old), seen)
  for (dkt in carried) new[[dkt]] <- old[[dkt]]
  new <- new[order(.orig_seq(names(new)))]
  dir.create(dirname(.originals_path(site_dir)), recursive = TRUE, showWarnings = FALSE)
  write_json(new, .originals_path(site_dir), auto_unbox = TRUE, na = "null")
  invisible(list(total = length(new), fetched = length(seen), carried = length(carried)))
}

#' The original dockets a run should fetch by name.
#'   live_only = FALSE (weekly): every known docket, plus `probe_above` numbers
#'               past the highest known one.
#'   live_only = TRUE  (daily):  dockets with activity inside ORIG_LIVE_IDLE_DAYS,
#'               plus the probe.
#' No manifest yet: the whole number space up to ORIG_FIRST_SCAN_TO, which is the
#' one expensive call (200 requests, ~2 min at the fleet's pace) and happens once.
originals_to_fetch <- function(site_dir, live_only = FALSE, as_of = Sys.Date(),
                               probe_above = if (live_only) ORIG_PROBE_DAILY else ORIG_PROBE_ABOVE) {
  idx <- read_originals(site_dir)
  if (!length(idx)) {
    if (live_only) return(character())   # the daily waits for the weekly to seed it
    return(paste0(ORIG_PREFIX, seq_len(ORIG_FIRST_SCAN_TO)))
  }
  known <- names(idx)[is_original_docket(names(idx))]
  if (live_only) {
    as_of <- as.Date(as_of)
    le <- vapply(known, function(d) {
      x <- idx[[d]]$last_event
      if (is.null(x) || is.na(x) || !nzchar(x)) NA_real_ else as.numeric(as.Date(x))
    }, numeric(1))
    known <- known[!is.na(le) & le >= as.numeric(as_of - ORIG_LIVE_IDLE_DAYS)]
  }
  mx <- suppressWarnings(max(.orig_seq(names(idx)), na.rm = TRUE))
  probe <- if (is.finite(mx) && probe_above > 0) paste0(ORIG_PREFIX, (mx + 1L):(mx + probe_above)) else character()
  unique(c(known[order(.orig_seq(known))], probe))
}
