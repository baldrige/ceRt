# court_calendar.R --------------------------------------------------------------
# The Court's own calendar, read from the widget on the supremecourt.gov home
# page, and the order-list dates that follow from it.
#
# WHY THE HOME PAGE.
#
# The Court publishes its Term calendar as a PDF with no text layer
# (/oral_arguments/2026TermCourtCalendar.pdf), and publishes no list of upcoming
# order lists at all -- only a July press release naming the three summer ones.
# But the home page's month calendar is the same data as the PDF, and the page
# carries it as text: a Telerik RadCalendar whose `specialDaysArray` names every
# marked day of the month in view ("Conference Day", "Argument Day", "Columbus
# Day" ...). It runs from September 2021 to the end of the announced Term.
#
# The widget shows one month. Another month is an ASP.NET postback of the page's
# own hidden fields with __EVENTARGUMENT "n:k", which moves the view k months
# (k may be negative, and need not be 1 -- "n:6" is one request, not six).
#
# WHAT THE COURT MARKS, AND WHEN.
#
# "Order List Issuance Day" is a category, but the Court fills it in after the
# fact: June 2026 has its four Mondays tagged, October 2026 has none. A day also
# holds ONE label, so a first-Monday order list that is also an argument day
# shows as "Argument Day" even in retrospect, and the summer lists are never
# tagged. So future order lists are inferred, by the Court's stated rule
# ("issued on each Monday that the Court sits") as it actually operates: the
# Monday after a conference, or the Tuesday when that Monday is a holiday. The
# holidays come from the widget too, so the rule needs no holiday table.
#
# Measured on OT2025 (2026-09-21): the rule names 27 dates and the Court issued
# an order list on all 27. It issued four more that no conference predicts --
# the three summer lists, and the 30 June clean-up list that follows the last
# opinions rather than a conference. Expect one of those each Term.

suppressPackageStartupMessages({ library(jsonlite) })

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

COURT_CAL_URL    <- "https://www.supremecourt.gov/"
COURT_CAL_TARGET <- "ctl00$MainEditable$Cal"
COURT_CAL_FILE   <- "court_calendar.json"   # under conferences/

# The labels the Court uses for its own business. Anything else on the calendar
# is a holiday: the widget names each one ("Labor Day") rather than typing it.
.court_cal_kind <- function(label) {
  conf <- grepl("Conference", label, fixed = TRUE)
  ifelse(grepl("Order List", label, fixed = TRUE), "orders",
  ifelse(conf, "conference",
  ifelse(grepl("Non-argument", label, fixed = TRUE), "nonargument",
  ifelse(grepl("Argument", label, fixed = TRUE), "argument",
  ifelse(grepl("Opinion", label, fixed = TRUE), "opinion", "holiday")))))
}

.court_cal_df <- function(date = as.Date(character()), label = character())
  data.frame(date = as.Date(date), label = label, kind = .court_cal_kind(label),
             stringsAsFactors = FALSE)

# ---- parse ----------------------------------------------------------------------

#' The marked days of the month in view: (date, label, kind).
#'
#' A combined label ("Conference/Non-argument Day", the June Thursdays) is typed
#' `conference`, because a conference is what an order list follows.
parse_court_calendar <- function(html) {
  # Cut the array out first and match inside it; the page is ~130 KB of
  # viewstate and the entry pattern has no business running across that.
  a <- regmatches(html, regexpr('"specialDaysArray":\\[.*?\\],"stylesHash"', html, perl = TRUE))
  if (!length(a)) stop("court calendar: no specialDaysArray on the page")
  m <- regmatches(a, gregexpr('\\[(\\d{4}),(\\d{1,2}),(\\d{1,2})\\],(?:-?\\d+,){6}"([^"]*)"', a, perl = TRUE))[[1]]
  if (!length(m)) return(.court_cal_df())
  p <- regmatches(m, regexec('\\[(\\d{4}),(\\d{1,2}),(\\d{1,2})\\].*"([^"]*)"$', m, perl = TRUE))
  d <- as.Date(vapply(p, function(x) sprintf("%s-%02d-%02d", x[2], as.integer(x[3]), as.integer(x[4])), ""))
  .court_cal_df(d, vapply(p, `[`, "", 5L))
}

# The first of the month in view, and the last month the widget will show.
.court_cal_view <- function(html) {
  t <- regmatches(html, regexec('id="ctl00_MainEditable_Cal_Title"[^>]*>([^<]+)<', html, perl = TRUE))[[1]]
  if (length(t) < 2L) stop("court calendar: no month title on the page")
  as.Date(paste("1", trimws(t[2])), "%d %B %Y")
}
.court_cal_max <- function(html) {
  r <- regmatches(html, regexec('Cal_AD" value="\\[\\[[^]]*\\],\\[(\\d{4}),(\\d{1,2}),', html, perl = TRUE))[[1]]
  if (length(r) < 3L) return(as.Date(NA))
  as.Date(sprintf("%s-%02d-01", r[2], as.integer(r[3])))
}

.court_cal_hidden <- function(html) {
  tags <- regmatches(html, gregexpr('<input type="hidden"[^>]*>', html, perl = TRUE))[[1]]
  nm <- sub('.*\\sname="([^"]*)".*', "\\1", tags)
  has_val <- grepl('\\svalue="', tags)
  val <- ifelse(has_val, sub('.*\\svalue="([^"]*)".*', "\\1", tags), "")
  ok <- grepl('\\sname="', tags)
  # The only entities a hidden value carries; viewstate itself is base64.
  val <- gsub("&quot;", '"', gsub("&amp;", "&", val, fixed = TRUE), fixed = TRUE)
  stats::setNames(as.list(val[ok]), nm[ok])
}

# ---- fetch ----------------------------------------------------------------------

.months_between <- function(a, b) {
  a <- as.POSIXlt(a); b <- as.POSIXlt(b)
  (b$year - a$year) * 12L + (b$mon - a$mon)
}

.court_cal_perform <- function(req) {
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- if (exists("scotus_perform")) scotus_perform(req) else { Sys.sleep(1); httr2::req_perform(req) }
  if (httr2::resp_status(resp) != 200L) stop("court calendar: HTTP ", httr2::resp_status(resp))
  httr2::resp_body_string(resp)
}

#' Every marked day from the month of `from` to the month of `to`, one request a
#' month plus one to open. `to` is clamped to the last month the widget offers
#' (the end of the announced Term), so the default asks for "everything ahead".
fetch_court_calendar <- function(from = Sys.Date(), to = as.Date("2100-01-01")) {
  base <- if (exists("scotus_req")) scotus_req(COURT_CAL_URL)
          else httr2::req_user_agent(httr2::request(COURT_CAL_URL), "ceRt SCOTUS docketing dashboard (httr2)")
  jar <- tempfile(fileext = ".cookies"); on.exit(unlink(jar), add = TRUE)
  base <- httr2::req_cookie_preserve(base, jar)
  html <- .court_cal_perform(base)
  first <- function(d) as.Date(format(as.Date(d), "%Y-%m-01"))
  last <- min(first(to), .court_cal_max(html), na.rm = TRUE)
  want <- first(from)
  out <- list()
  repeat {
    view <- .court_cal_view(html)
    if (view == want) {
      # Keep the month in view only: a postback page still carries the marked
      # days of the month the session opened on (today's) beside the month asked
      # for, so September 2026 rides along on every page of a 2025 backfill.
      m <- parse_court_calendar(html)
      out[[length(out) + 1L]] <- m[format(m$date, "%Y-%m") == format(want, "%Y-%m"), , drop = FALSE]
      if (want >= last) break
      want <- seq(want, by = "month", length.out = 2L)[2]
    }
    k <- .months_between(view, want)
    form <- .court_cal_hidden(html)
    form[["__EVENTTARGET"]] <- COURT_CAL_TARGET
    form[["__EVENTARGUMENT"]] <- sprintf("n:%d", k)
    html <- .court_cal_perform(do.call(httr2::req_body_form, c(list(base), form)))
    # A postback the server ignores returns the same month; without this the
    # loop would ask for it forever.
    if (.court_cal_view(html) != want)
      stop("court calendar: asked for ", format(want, "%B %Y"), ", got ", format(.court_cal_view(html), "%B %Y"))
  }
  cal <- do.call(rbind, out)
  cal <- cal[order(cal$date), , drop = FALSE]
  rownames(cal) <- NULL
  cal
}

# ---- the order lists ------------------------------------------------------------

#' Order-list dates implied by a court calendar: (date, conference, source).
#'
#' One list per conference, on the first Monday after it -- strictly after, so
#' the opening conference, itself a Monday, reports on the first Monday of the
#' Term -- moved to Tuesday when the widget marks that Monday a holiday. Two
#' conferences that resolve to one date (none has yet) collapse to the later.
#'
#' `source` is "court" where the widget tags the date "Order List Issuance Day"
#' and "inferred" otherwise. The Court tags only in retrospect and one label a
#' day, so "inferred" on a past date is not a miss -- see the head of this file.
#' The summer lists follow no conference and are not here; they come from the
#' July press release.
expected_order_lists <- function(cal) {
  empty <- data.frame(date = as.Date(character()), conference = as.Date(character()),
                      source = character(), stringsAsFactors = FALSE)
  if (is.null(cal) || !nrow(cal)) return(empty)
  conf <- sort(unique(cal$date[cal$kind == "conference"]))
  if (!length(conf)) return(empty)
  hol <- cal$date[cal$kind == "holiday"]
  wd <- as.integer(format(conf, "%u"))            # 1 = Monday
  d <- conf + (8L - wd)
  d <- d + (d %in% hol)
  out <- data.frame(date = d, conference = conf, stringsAsFactors = FALSE)
  out <- out[!duplicated(out$date, fromLast = TRUE), , drop = FALSE]
  out$source <- ifelse(out$date %in% cal$date[cal$kind == "orders"], "court", "inferred")
  rownames(out) <- NULL
  out
}

# ---- manifest -------------------------------------------------------------------

read_court_calendar <- function(site_dir) {
  p <- file.path(site_dir, "conferences", COURT_CAL_FILE)
  if (!file.exists(p)) return(.court_cal_df())
  j <- tryCatch(fromJSON(p, simplifyDataFrame = TRUE), error = function(e) NULL)
  if (is.null(j) || !is.data.frame(j) || !nrow(j) || !all(c("date", "label") %in% names(j))) return(.court_cal_df())
  .court_cal_df(j$date, as.character(j$label))
}

#' Refresh the manifest from the month of `as_of` on, keeping earlier months as
#' held. The calendar is fixed for a Term and amended rarely, so a failed fetch
#' returns what was there: a week-old calendar is still the calendar.
update_court_calendar <- function(site_dir, as_of = Sys.Date()) {
  old <- read_court_calendar(site_dir)
  new <- tryCatch(fetch_court_calendar(from = as_of),
                  error = function(e) { cat("Court calendar unavailable:", conditionMessage(e), "\n"); NULL })
  if (is.null(new) || !nrow(new)) return(old)
  keep <- old[old$date < min(new$date), , drop = FALSE]
  all <- rbind(keep, new)
  p <- file.path(site_dir, "conferences", COURT_CAL_FILE)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  w <- all; w$date <- format(w$date, "%Y-%m-%d")
  write_json(w, p, auto_unbox = TRUE, dataframe = "rows")
  cat(sprintf("Court calendar: %d marked day(s), %s to %s; %d conference(s) ahead\n", nrow(all),
              format(min(all$date)), format(max(all$date)),
              sum(all$kind == "conference" & all$date >= as_of)))
  all
}
