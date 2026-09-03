# site_decisions.R -------------------------------------------------------------
# "Recent decisions": the cases the Court has most recently decided by written
# opinion, for the landing page. Argued or not. Spec and the measurements behind
# every rule here: docs/recent-decisions.md.
#
# WHY THIS IS A FILE ON DISK AND NOT A COMPUTATION.
#
# Same gap as site_calendar.R. The landing page is written by the daily, which
# fetches the trailing ~50 dockets of each bucket -- 5 days of paid filings, 12
# of applications. An argued case is decided months after filing, an emergency
# application a median 25 days after, a summary reversal a median 66. The daily's
# own fetch sees essentially none of them. So the weekly conferences run, which
# fetches the current and prior Terms in full, writes a manifest as it renders
# the Navigator, and the daily reads it. Staleness is handled at READ time by a
# date window, so a manifest written Monday still renders correctly on Sunday.
#
# The weekly run also writes a WATCH LIST: the dockets that could produce an
# opinion soon (argued and undecided; applications referred to the Court and not
# yet disposed of). The daily fetches those by name -- a few dozen requests at
# most -- and writes its own manifest beside the weekly one, so a Thursday
# opinion is on the page Thursday rather than the following Monday.
#
# THREE KINDS, ONE TEST.
#
#   argued       a merits grant decided after oral argument
#   application  an NNA### docket decided by the Court with a written opinion
#   summary      a petition granted and the judgment below reversed or vacated
#                in the same order, with a per curiam opinion, no argument
#
# The obvious test -- "the entry links a supremecourt.gov/opinions/ PDF" -- is
# wrong in both directions, measured over OT24-25 (10,614 dockets):
#
#   * NOT SUFFICIENT. Dissents from denial and statements respecting denial link
#     the same NNpdf/ directory as merits opinions: 18 of 31 unargued cert
#     dockets with an opinion link were denials with a separate writing.
#   * NOT NECESSARY. Of 37 applications whose entry carries an opinion marker,
#     4 link the PDF. The Court writes the per curiam INTO the docket entry, or
#     flags it "(Detached Opinion)".
#
# So the test reads the entry text, with the anchors stripped first: the JSON
# embeds <a href> around the word "Opinion", so the raw text of 26A203 reads
# "<a ...>Opinion</a> per curiam" and a literal phrase match misses it. The URL
# comes from that anchor -- the entry's Links[] array is empty for opinion PDFs.

suppressPackageStartupMessages({ library(jsonlite); library(stringr) })

`%||%` <- function(a, b) if (is.null(a)) b else a

# The grammars this module leans on are audited elsewhere and are NOT duplicated
# here: classify_argument() (argument_nav.R), classify_application_events()
# (docket_page.R), classify_petition_events() + funnel_case_type() (cert_funnel.R),
# JUDGMENT_RX (counsel_table.R), strip_caption_roles() (page_style.R). Sourced by
# name because every entry point that relied on transitive sourcing in this repo
# has eventually lost a feature to it silently.
local({
  f <- function(x) if (file.exists(file.path("R", x))) file.path("R", x) else x
  need <- function(sym, file) if (!exists(sym) && file.exists(f(file)))
    sys.source(f(file), envir = globalenv())
  need("classify_petition_events",     "cert_funnel.R")
  need("classify_argument",            "argument_nav.R")
  need("classify_application_events",  "docket_page.R")
  need("JUDGMENT_RX",                  "counsel_table.R")
  need("strip_caption_roles",          "page_style.R")
})

DECISION_KINDS <- c("argued", "application", "summary")
DECIDED_FILE <- "decided.json"
WATCH_FILE   <- "watch.json"

# How far back the WRITER keeps rows. The reader applies its own, shorter window.
DECIDED_KEEP_DAYS <- 90L

# ---- the marker test ----------------------------------------------------------

# The Court's own opinion. "per curiam" is guarded against the citation form --
# 25A347 denies a stay and cites "Calderon v. Moore, 518 U. S. 149 (1996) (per
# curiam)", which is a case about someone else's opinion.
.DEC_COURT_RX <- regex(paste0(
  "opinion per curiam|(?<![(\\[])per curiam|delivered the opinion|",
  "announced the judgment|opinion of the court"), ignore_case = TRUE)
# A separate writing is present (dissent, concurrence, statement).
.DEC_SEPARATE_RX <- regex("dissent|concurr|statement of (the chief )?justice|respecting the denial",
                          ignore_case = TRUE)
# A GVR "for further consideration in light of" is an order, not an opinion,
# even when a Justice dissents from it in writing (25-273).
.DEC_GVR_RX <- regex("for further consideration in light of", ignore_case = TRUE)
# Slip-opinion URL, from the anchor in the RAW entry text.
.DEC_URL_RX <- "href\\s*=\\s*['\"]([^'\"]*supremecourt\\.gov/opinions/[^'\"]+)['\"]"

.strip_tags <- function(x) str_squish(str_remove_all(x, "<[^>]+>"))

.opinion_url <- function(raw) {
  m <- str_match(paste(raw, collapse = " "), .DEC_URL_RX)[1, 2]
  if (is.na(m)) NA_character_ else m
}

# Does this disposition entry carry an opinion OF THE COURT?
#   * the Court's marker is present                          -> yes
#   * no marker, a PDF is linked, and no separate writing     -> yes: the per
#     curiam is written inline (25A11: "The application is squarely controlled
#     by Trump v. Wilcox ...")
#   * no marker, a PDF is linked, and a separate writing      -> no: an order with
#     a dissent attached, which is most of the emergency docket
#   * nothing                                                 -> no
.is_court_opinion <- function(stripped, url) {
  s <- paste(stripped, collapse = " ")
  if (str_detect(s, .DEC_COURT_RX)) return(TRUE)
  !is.na(url) && !str_detect(s, .DEC_SEPARATE_RX)
}

# "Kagan" / "Roberts, C.J." / "Per Curiam" / NA. Same shape classify_argument()
# publishes, so the two cannot name one author two ways.
.decision_author <- function(stripped) {
  s <- paste(stripped, collapse = " ")
  am <- str_match(s, "([A-Z][A-Za-z'\u2019]+), ((?:C\\. )?J)\\.,.{0,120}?(?:delivered|announced)")
  if (!is.na(am[1, 2]))
    return(if (str_detect(am[1, 3], "C")) paste0(am[1, 2], ", C.J.") else am[1, 2])
  if (str_detect(s, regex("the chief justice delivered", ignore_case = TRUE)))
    return("Roberts, C.J.")
  if (str_detect(s, regex("opinion per curiam|(?<![(\\[])per curiam", ignore_case = TRUE)))
    return("Per Curiam")
  NA_character_
}

# The operative merits judgment as a short label, from the audited grammar.
.judgment_label <- function(stripped) {
  s <- paste(stripped, collapse = " ")
  m <- str_match(s, JUDGMENT_RX)[1, 1]
  if (is.na(m)) return(NA_character_)
  verb <- str_extract(tolower(m), "affirmed|reversed|vacated|appeal\\s+dismissed|improvidently granted")
  lab <- switch(verb,
    "affirmed" = "Affirmed", "reversed" = "Reversed", "vacated" = "Vacated",
    "improvidently granted" = "Dismissed as improvidently granted",
    if (!is.na(verb) && str_detect(verb, "appeal")) "Appeal dismissed" else NA_character_)
  if (is.na(lab)) return(NA_character_)
  if (str_detect(s, regex("in part", ignore_case = TRUE)) && lab %in% c("Affirmed", "Reversed", "Vacated"))
    lab <- paste(lab, "in part")
  if (str_detect(s, regex("\\bremanded\\b", ignore_case = TRUE)) && lab %in% c("Affirmed", "Reversed", "Vacated"))
    lab <- paste(lab, "and remanded")
  lab
}

# An application's disposition as a short label, from the docket page's outcome.
.application_label <- function(outcome, stripped) {
  s <- paste(stripped, collapse = " ")
  moot <- str_detect(s, regex("as moot", ignore_case = TRUE))
  switch(outcome,
    "granted"   = "Granted",
    "partial"   = "Granted in part",
    "denied"    = if (moot) "Denied as moot" else "Denied",
    "dismissed" = "Dismissed",
    NA_character_)
}

# ---- rows -----------------------------------------------------------------------

.dec_df <- function(date = as.Date(character()), dkt = character(), caption = character(),
                    kind = character(), author = character(), disposition = character(),
                    opinion_url = character(), argued = as.Date(character()),
                    term = integer()) {
  data.frame(date = as.Date(date), dkt = dkt, caption = caption, kind = kind,
             author = author, disposition = disposition, opinion_url = opinion_url,
             argued = as.Date(argued), term = as.integer(term),
             stringsAsFactors = FALSE)
}

.events_ok <- function(ev) is.data.frame(ev) && nrow(ev) > 0 &&
  all(c("Date", "Proceedings and Orders") %in% names(ev))

.last_event <- function(ev) {
  if (!.events_ok(ev)) return(as.Date(NA))
  d <- suppressWarnings(lubridate::mdy(ev$Date))
  if (all(is.na(d))) as.Date(NA) else max(d, na.rm = TRUE)
}

# Every entry made on `day`, raw and stripped. Dispositions and their opinion
# markers are sometimes split across two same-day entries ("referred to the
# Court." then "granted by the Court. ... Opinion per curiam."), so the day is
# the unit, not the entry.
.entries_on <- function(ev, day) {
  d <- suppressWarnings(lubridate::mdy(ev$Date))
  raw <- ev[["Proceedings and Orders"]][!is.na(d) & d == day]
  raw[is.na(raw)] <- ""
  list(raw = raw, stripped = .strip_tags(raw))
}

# One case -> zero or one decision row.
.classify_decision <- function(dkt, caption, ev) {
  if (!.events_ok(ev)) return(NULL)
  cap <- strip_caption_roles(caption %||% dkt)
  if (is.na(cap) || !nzchar(cap)) cap <- dkt
  type <- funnel_case_type(dkt)

  if (identical(type, "app")) {
    txt <- ev[["Proceedings and Orders"]]; txt[is.na(txt)] <- ""
    r <- classify_application_events(.strip_tags(txt), suppressWarnings(lubridate::mdy(ev$Date)), dkt)
    if (is.na(r$outcome) || !r$outcome %in% c("granted", "denied", "partial", "dismissed") ||
        is.na(r$date)) return(NULL)
    e <- .entries_on(ev, r$date)
    url <- .opinion_url(e$raw)
    if (!.is_court_opinion(e$stripped, url)) return(NULL)
    return(.dec_df(r$date, dkt, cap, "application", .decision_author(e$stripped),
                   .application_label(r$outcome, e$stripped), url, as.Date(NA), NA_integer_))
  }

  # Argued first, and NOT gated on the funnel calling the case "granted": 25-1083
  # and 25-1084 (Mullin v. Doe, Trump v. Miot) were granted as "the applications
  # are treated as petitions for certiorari before judgment, and the petitions
  # are granted", a form the funnel's grant grammar does not know, so it holds
  # them "pending" while the docket says Argued and then Decided. An argument
  # entry followed by a decision is a merits decision whatever the grant looked
  # like.
  arg <- classify_argument(ev)
  cls <- classify_petition_events(ev)
  out <- cls$outcome[[1]]
  if (!is.na(arg$argued_date) || identical(out, "granted")) {
    if (!identical(arg$status, "Decided") || is.na(arg$decided_date)) return(NULL)
    e <- .entries_on(ev, arg$decided_date)
    url <- .opinion_url(e$raw)
    author <- .decision_author(e$stripped)
    # A bare "Judgment Issued" is the mandate, not an opinion. Any one of the
    # three -- author, per curiam marker, or a linked PDF -- says an opinion exists.
    if (is.na(author) && is.na(url) && !.is_court_opinion(e$stripped, url)) return(NULL)
    return(.dec_df(arg$decided_date, dkt, cap, "argued", author, .judgment_label(e$stripped),
                   url, arg$argued_date,
                   if (!is.na(arg$argued_date) && exists("argument_term"))
                     as.integer(argument_term(arg$argued_date)) else NA_integer_))
  }
  if (identical(out, "gvr") && !is.na(cls$outcome_date[[1]])) {
    e <- .entries_on(ev, cls$outcome_date[[1]])
    s <- paste(e$stripped, collapse = " ")
    url <- .opinion_url(e$raw)
    # The ordinary GVR is an order. Only the Court's own opinion makes it a row.
    if (str_detect(s, .DEC_GVR_RX) && !str_detect(s, .DEC_COURT_RX)) return(NULL)
    if (!.is_court_opinion(e$stripped, url)) return(NULL)
    return(.dec_df(cls$outcome_date[[1]], dkt, cap, "summary", .decision_author(e$stripped),
                   .judgment_label(e$stripped), url, as.Date(NA), NA_integer_))
  }
  NULL
}

#' Every decision by opinion in `cases` within the last `days`, newest first.
#'
#' Prefiltered on the date of each docket's LAST entry: an opinion is always the
#' newest thing on its docket, so a docket quiet for longer than the window
#' cannot hold one. That turns a pass over the whole back-catalogue (~80k
#' dockets on the conferences run) into a pass over a few hundred.
recent_decisions <- function(cases, as_of = Sys.Date(), days = DECIDED_KEEP_DAYS) {
  if (is.null(cases) || !nrow(cases) || !all(c("dkt", "events") %in% names(cases)))
    return(.dec_df())
  as_of <- as.Date(as_of)
  last <- as.Date(vapply(cases$events, function(e) as.numeric(.last_event(e)), numeric(1)),
                  origin = "1970-01-01")
  keep <- which(!is.na(last) & last >= as_of - days & last <= as_of + 1L)
  if (!length(keep)) return(.dec_df())
  caps <- if ("caption" %in% names(cases)) cases$caption else rep(NA_character_, nrow(cases))
  rows <- lapply(keep, function(i)
    tryCatch(.classify_decision(cases$dkt[i], caps[i], cases$events[[i]]),
             error = function(e) NULL))
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(.dec_df())
  out <- do.call(rbind, rows)
  out <- out[!is.na(out$date) & out$date >= as_of - days & out$date <= as_of + 1L, , drop = FALSE]
  out <- out[!duplicated(out$dkt), , drop = FALSE]
  out[order(out$date, out$dkt, decreasing = c(TRUE, FALSE), method = "radix"), , drop = FALSE]
}

# ---- manifest ---------------------------------------------------------------------

#' Always writes, even empty: an absent file and an empty one mean different
#' things, and only one of them is "this pipeline ran and found nothing".
write_decided <- function(rows, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  df <- if (is.null(rows) || !nrow(rows)) .dec_df() else rows
  df$date   <- format(as.Date(df$date), "%Y-%m-%d")
  df$argued <- ifelse(is.na(df$argued), NA_character_, format(as.Date(df$argued), "%Y-%m-%d"))
  # Event dates only, never a build time: the file must be byte-identical when
  # nothing happened, or publish_site.sh stops short-circuiting (see feeds.R).
  write_json(df, path, auto_unbox = TRUE, dataframe = "rows", na = "null", null = "null")
  invisible(path)
}

.read_decided_one <- function(p) {
  if (!file.exists(p)) return(NULL)
  j <- tryCatch(fromJSON(p, simplifyDataFrame = TRUE), error = function(e) NULL)
  if (is.null(j) || !is.data.frame(j) || !nrow(j)) return(NULL)
  if (!all(c("date", "dkt", "caption", "kind") %in% names(j))) return(NULL)
  col <- function(nm, fill) if (nm %in% names(j)) j[[nm]] else rep(fill, nrow(j))
  chr <- function(x) { x <- as.character(x); x[!nzchar(x) | x == "NA"] <- NA_character_; x }
  .dec_df(as.Date(j$date), as.character(j$dkt), as.character(j$caption), as.character(j$kind),
          chr(col("author", NA_character_)), chr(col("disposition", NA_character_)),
          chr(col("opinion_url", NA_character_)),
          as.Date(chr(col("argued", NA_character_))), suppressWarnings(as.integer(col("term", NA))))
}

#' Merge every manifest found, dedupe by docket (first path wins, so pass the
#' fresher file first), drop anything outside the window, and return the newest
#' `n` GROUPS -- consolidated cases decided by one opinion share a `group` and
#' render as one row. Unreadable or missing files are skipped rather than
#' fatal: this is one panel on a page whose job is publishing dockets.
read_decided <- function(paths, as_of = Sys.Date(), n = 6L, days = 45L) {
  parts <- lapply(paths, .read_decided_one)
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) return(.dec_df())
  all <- do.call(rbind, parts)
  all <- all[!duplicated(all$dkt), , drop = FALSE]
  all <- all[!is.na(all$date) & all$kind %in% DECISION_KINDS &
             all$date >= as.Date(as_of) - days & all$date <= as.Date(as_of) + 1L, , drop = FALSE]
  if (!nrow(all)) return(.dec_df())
  all <- all[order(all$date, all$dkt, decreasing = c(TRUE, FALSE), method = "radix"), , drop = FALSE]
  # One opinion, several dockets: group on the URL, and only where the rows
  # also agree on disposition -- 26A139 was "denied as moot" in the order that
  # granted 26A124, and those are two rows, not one.
  all$group <- ifelse(!is.na(all$opinion_url) & nzchar(all$opinion_url),
                      paste(all$opinion_url, all$disposition %||% "", sep = "|"), all$dkt)
  keep_groups <- unique(all$group)[seq_len(min(n, length(unique(all$group))))]
  all[all$group %in% keep_groups, , drop = FALSE]
}

# ---- the watch list ---------------------------------------------------------------

# An application is LIVE -- worth a daily request -- once the Court has engaged
# with it beyond docketing: referred to the full Court, argued, answered by the
# other side, or given an administrative stay while the Court considers it.
# "Referred to the Court" alone was too narrow: 26A203 was referred and granted
# in the same day's entries, after ten days under a Chief Justice's
# administrative stay, and 25A312 (Trump v. Cook) was argued on the application
# and decided by signed opinion. The referral gate would have watched neither.
.WATCH_APP_LIVE_RX <- regex(paste0(
  "referred to the Court|^Argued\\.|^Response (to application|of respondent)|",
  "^Reply of applicant|administrative(ly)? stay|is hereby stayed pending"),
  ignore_case = TRUE)

# A live signal older than this is not live; the classifier missed the
# disposition. Measured: the emergency docket resolves in a median 25 days,
# third quartile 72.
WATCH_APP_MAX_DAYS <- 120L

#' Dockets that could produce an opinion soon, for the daily to fetch by name:
#' argued-and-undecided merits cases, and applications referred to the Court
#' without a disposition since the referral.
decisions_watch <- function(cases, as_of = Sys.Date()) {
  empty <- data.frame(dkt = character(), kind = character(), since = character(),
                      stringsAsFactors = FALSE)
  if (is.null(cases) || !nrow(cases) || !all(c("dkt", "events") %in% names(cases))) return(empty)
  as_of <- as.Date(as_of)
  # Cheap text gate before any classifier runs: a case can only be on the watch
  # list if its docket says "Argued." or "referred to the Court", and that is a
  # regex over a few hundred strings, where the classifiers are not. On the
  # conferences run this is the difference between a pass over ~80k dockets
  # taking seconds and taking twenty minutes.
  gate <- vapply(cases$events, function(ev) .events_ok(ev) &&
    any(str_detect(coalesce(ev[["Proceedings and Orders"]], ""),
                   regex(paste0("^Argued\\.|", as.character(.WATCH_APP_LIVE_RX)), ignore_case = TRUE))),
    logical(1))
  rows <- lapply(which(gate), function(i) {
    ev <- cases$events[[i]]; dkt <- cases$dkt[i]
    last <- .last_event(ev)
    if (is.na(last) || last < as_of - 730L) return(NULL)   # dormant; not a live docket
    txt <- ev[["Proceedings and Orders"]]; txt[is.na(txt)] <- ""
    d <- suppressWarnings(lubridate::mdy(ev$Date))
    if (identical(funnel_case_type(dkt), "app")) {
      live <- which(str_detect(txt, .WATCH_APP_LIVE_RX))
      if (!length(live) || all(is.na(d[live]))) return(NULL)
      live_date <- max(d[live], na.rm = TRUE)
      # An ARGUED application is decided on the merits calendar's clock, not the
      # emergency docket's: 25A312 (Trump v. Cook) was argued in January and
      # decided in late June, and the 120-day cap would have dropped it in May.
      argued <- any(str_detect(txt, regex("^Argued\\.", ignore_case = TRUE)))
      if (!argued && live_date < as_of - WATCH_APP_MAX_DAYS) return(NULL)
      r <- tryCatch(classify_application_events(.strip_tags(txt), d, dkt), error = function(e) NULL)
      disposed <- !is.null(r) && !is.na(r$outcome) && !is.na(r$date) && r$date >= live_date
      if (disposed) return(NULL)
      return(data.frame(dkt = dkt, kind = "application", since = format(live_date),
                        stringsAsFactors = FALSE))
    }
    # Not gated on the funnel's "granted" -- see .classify_decision() for why.
    arg <- tryCatch(classify_argument(ev), error = function(e) NULL)
    if (is.null(arg) || !identical(arg$status, "Argued") || is.na(arg$argued_date)) return(NULL)
    data.frame(dkt = dkt, kind = "argued", since = format(arg$argued_date),
               stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(empty)
  out <- do.call(rbind, rows)
  out[order(out$kind, out$since, out$dkt), , drop = FALSE]
}

write_watch <- function(watch, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  df <- if (is.null(watch) || !nrow(watch))
    data.frame(dkt = character(), kind = character(), since = character(), stringsAsFactors = FALSE)
    else watch
  write_json(df, path, auto_unbox = TRUE, dataframe = "rows")
  invisible(path)
}

read_watch <- function(path) {
  if (!file.exists(path)) return(character())
  j <- tryCatch(fromJSON(path, simplifyDataFrame = TRUE), error = function(e) NULL)
  if (is.null(j) || !is.data.frame(j) || !nrow(j) || !("dkt" %in% names(j))) return(character())
  unique(as.character(j$dkt[!is.na(j$dkt) & nzchar(j$dkt)]))
}
