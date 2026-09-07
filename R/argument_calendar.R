# argument_calendar.R -------------------------------------------------------------
# The Court's monthly argument calendars and its Day Calls. Spec:
# docs/argument-calendar.md.
#
# THE CALENDAR. /oral_arguments/argument_calendars/MonthlyArgumentCal<Month><Year>.pdf,
# one per sitting, posted about two months ahead (the October 2026 sitting's on
# 4 August 2026), linked from /oral_arguments/calendarsandlists.aspx. One page,
# two weeks side by side:
#
#              Monday, February 23                    Monday, March 2
#                      (1)                                  (1)
#   24-983       HAVANA DOCKS CORP. V.     24-1234    UNITED STATES V.
#                ROYAL CARIBBEAN                      HEMANI
#
# A day's cases in the order they are heard, "(1)", "(2)"; a caption wraps
# under its docket; "LEGAL HOLIDAY" fills an empty day. The Term is on the
# header ("OCTOBER TERM 2025"), so a month before October belongs to the next
# calendar year. The column boundary is where the second weekday sits on the
# header line, and every line is cut there.
#
# THE DAY CALL. /oral_arguments/daycall/Day Call_MM-DD-YY.pdf, one per argument
# day, posted that morning. Advocates down the left, the case down the right:
#
#   MR. ALAN M. HURST                    No. 24-38.                 (2)
#   Solicitor General
#   Boise, Idaho                         BRADLEY LITTLE,
#   (20 minutes – for petitioners)       GOVERNOR OF IDAHO, ET AL.,
#                                                            Petitioners
#   MR. HASHIM M. MOOPPAN
#   Principal Deputy Solicitor General
#   Department of Justice                             V.                   1 hour for argument.
#   Washington, D. C.
#   (10 minutes - for United States, as amicus curiae, supporting petitioners)
#
# Each advocate's side and minutes are in the parenthetical, which can wrap; a
# name can wrap too ("MS. KATHLEEN R." / "HARTNETT"). The boundary is where
# "No." sits.
#
# WHAT THEY ADD. The calendar names a sitting's cases and their order about
# two months out, and is the cross-check for the docket's "SET FOR ARGUMENT"
# entries; where a docket lacks the entry, the calendar supplies the date. The
# Day Call names who argues, for which side, for how long -- before the
# argument, where the docket's "Argued. For petitioner: ..." entry comes after.
#
# WHO WRITES WHAT. render_arguments.R (the weekly) reads the index page, fetches
# calendars for sittings from six weeks ago on and every Day Call the manifest
# lacks, and rewrites arguments/calendar.json and arguments/daycalls.json.

suppressPackageStartupMessages({ library(stringr); library(jsonlite) })

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

CAL_INDEX_URL <- "https://www.supremecourt.gov/oral_arguments/calendarsandlists.aspx"
CAL_BASE      <- "https://www.supremecourt.gov/oral_arguments/"
CAL_FILE      <- "calendar.json"     # under arguments/
DAYCALL_FILE  <- "daycalls.json"     # under arguments/
CAL_REFRESH_DAYS <- 45L              # calendars for sittings from this far back are re-read
DAYCALL_MAX_NEW  <- 40L              # Day Calls fetched per run

.cal_get <- function(url, as = c("string", "pdf")) {
  as <- match.arg(as)
  resp <- if (exists("scotus_perform") && exists("scotus_req")) scotus_perform(scotus_req(url))
          else httr2::req_perform(httr2::req_user_agent(httr2::request(url), "ceRt SCOTUS docketing dashboard (httr2)"))
  if (httr2::resp_status(resp) != 200L) stop("HTTP ", httr2::resp_status(resp), " for ", url)
  if (as == "string") return(httr2::resp_body_string(resp))
  tmp <- tempfile(fileext = ".pdf"); writeBin(httr2::resp_body_raw(resp), tmp)
  pages <- pdftools::pdf_text(tmp); unlink(tmp); pages
}

# ---- the index page ----------------------------------------------------------------

#' The calendar and Day Call PDFs the index page links: (kind, key, url).
#' `key` is "October2026" for a calendar and the date for a Day Call.
fetch_calendar_index <- function() {
  html <- .cal_get(CAL_INDEX_URL)
  cal <- unique(str_match_all(html, "argument_calendars/MonthlyArgumentCal([A-Za-z]+)(\\d{4})\\.pdf")[[1]])
  dc  <- unique(str_match_all(html, "daycall/Day(?:%20| )Call_(\\d{2})-(\\d{2})-(\\d{2})\\.pdf")[[1]])
  rbind(
    if (nrow(cal)) data.frame(kind = "calendar", key = paste0(cal[, 2], cal[, 3]),
                              url = paste0(CAL_BASE, str_replace(cal[, 1], " ", "%20")), stringsAsFactors = FALSE),
    if (nrow(dc)) data.frame(kind = "daycall",
                             key = format(as.Date(sprintf("20%s-%s-%s", dc[, 4], dc[, 2], dc[, 3]))),
                             url = paste0(CAL_BASE, "daycall/Day%20Call_", dc[, 2], "-", dc[, 3], "-", dc[, 4], ".pdf"),
                             stringsAsFactors = FALSE))
}

# ---- the calendar -------------------------------------------------------------------

.cal_df <- function() data.frame(session = as.Date(character()), date = as.Date(character()), slot = integer(),
                                 dkt = character(), caption = character(), stringsAsFactors = FALSE)
.WEEKDAY_RX <- "(Monday|Tuesday|Wednesday|Thursday|Friday), ([A-Z][a-z]+ \\d{1,2})"
.DOCKET_RX  <- "^\\s*(\\d{2}-\\d{1,5}|\\d{2}A\\d{1,4}|22O\\d{1,4})\\s+(\\S.*)$"

# One column's lines -> rows. `year_of(month_name)` resolves the calendar year.
.cal_column <- function(lines, year_of, session) {
  out <- list(); date <- as.Date(NA); slot <- NA_integer_; cur <- 0L
  for (ln in lines) {
    t <- str_squish(ln)
    if (!nzchar(t)) next
    wd <- str_match(t, paste0("^", .WEEKDAY_RX, "$"))
    if (!is.na(wd[1, 1])) {
      md <- wd[1, 3]; mon <- str_extract(md, "^[A-Za-z]+")
      date <- suppressWarnings(as.Date(paste(md, year_of(mon)), "%B %d %Y")); slot <- NA_integer_
      next
    }
    if (str_detect(t, "^\\(\\d+\\)$")) { slot <- as.integer(str_extract(t, "\\d+")); next }
    if (str_detect(t, "LEGAL HOLIDAY|Court convenes|^[A-Z][a-z]+ \\d{1,2}, \\d{4}$")) next
    dk <- str_match(t, .DOCKET_RX)
    if (!is.na(dk[1, 2])) {
      cur <- cur + 1L
      out[[cur]] <- data.frame(session = session, date = date, slot = slot, dkt = dk[1, 2],
                               caption = str_squish(dk[1, 3]), stringsAsFactors = FALSE)
      next
    }
    if (cur > 0L && !str_detect(t, "[a-z]")) out[[cur]]$caption <- str_squish(paste(out[[cur]]$caption, t))
  }
  if (!length(out)) .cal_df() else do.call(rbind, out)
}

#' One calendar PDF -> (session, date, slot, dkt, caption).
parse_argument_calendar <- function(pages) {
  lines <- unlist(strsplit(paste(pages, collapse = "\n"), "\n", fixed = TRUE))
  lines <- str_replace_all(lines, " ", " ")
  head_txt <- paste(head(lines, 6), collapse = " ")
  term_year <- as.integer(str_match(head_txt, "OCTOBER TERM (\\d{4})")[1, 2])
  sess <- str_match(head_txt, "Session Beginning ([A-Z][a-z]+ \\d{1,2}, \\d{4})")[1, 2]
  session <- if (is.na(sess)) as.Date(NA) else suppressWarnings(as.Date(sess, "%B %d, %Y"))
  if (is.na(term_year)) term_year <- if (!is.na(session)) as.integer(format(session, "%Y")) - as.integer(as.integer(format(session, "%m")) < 10L) else NA_integer_
  year_of <- function(mon) { m <- match(mon, month.name); if (is.na(m) || is.na(term_year)) NA_integer_ else if (m >= 10L) term_year else term_year + 1L }
  # The column boundary. The header's second weekday sits further right than
  # the second column's dockets do ("Monday, March 2" at column 52, "24-1234"
  # at 41), so the boundary is the leftmost start of a docket number that is
  # not the first on its line; the header is the fallback for a page whose
  # two columns never share a line.
  dk_rx <- "\\d{2}-\\d{1,5}|\\d{2}A\\d{1,4}|22O\\d{1,4}"
  second <- unlist(lapply(str_locate_all(lines, paste0("(?<![-\\dA-Z])(", dk_rx, ")(?![-\\d])")), function(p) if (nrow(p) >= 2) p[2, 1] else NA_integer_))
  cut <- suppressWarnings(min(second, na.rm = TRUE))
  if (!is.finite(cut)) {
    hdr <- lines[str_detect(lines, paste0(.WEEKDAY_RX, ".*", .WEEKDAY_RX))]
    cut <- if (length(hdr)) { pos <- str_locate_all(hdr[1], .WEEKDAY_RX)[[1]]; if (nrow(pos) >= 2) pos[2, 1] else NA_integer_ } else NA_integer_
  }
  # The right column's slot markers "(2)" sit to the right of its dockets, and
  # the Court wraps a left caption before the right column begins, so the
  # docket start is the boundary exactly.
  if (is.na(cut)) return(.cal_column(lines, year_of, session))
  left  <- substr(lines, 1L, cut - 1L)
  right <- substr(lines, cut, nchar(lines))
  rbind(.cal_column(left, year_of, session), .cal_column(right, year_of, session))
}

# ---- the Day Call --------------------------------------------------------------------

.dc_df <- function() data.frame(date = as.Date(character()), dkt = character(), slot = integer(),
                                minutes_total = integer(), name = character(), affiliation = character(),
                                side = character(), minutes = integer(), stringsAsFactors = FALSE)

# "(20 minutes – for petitioners)" / "(10 minutes - for United States, as amicus
# curiae, supporting petitioners)" -> side and minutes.
.dc_role <- function(p) {
  p <- str_squish(p)
  mins <- as.integer(str_extract(p, "\\d+(?=\\s*minutes?)"))
  side <- if (str_detect(p, regex("amicus", ignore_case = TRUE))) {
    sup <- str_extract(p, regex("supporting (the )?(petitioners?|respondents?|appellants?|appellees?|plaintiffs?|defendants?|neither party)", ignore_case = TRUE))
    paste0("amicus", if (!is.na(sup)) paste0(" ", tolower(sup)) else "")
  } else if (str_detect(p, regex("petitioner|appellant|plaintiff|applicant", ignore_case = TRUE))) "petitioner"
  else if (str_detect(p, regex("respondent|appellee|defendant", ignore_case = TRUE))) "respondent"
  else NA_character_
  list(side = side, minutes = mins)
}

#' One Day Call PDF -> one row per advocate: the case, its slot and total time,
#' the advocate's name, affiliation, side and minutes.
parse_day_call <- function(pages, date = as.Date(NA)) {
  lines <- unlist(strsplit(paste(pages, collapse = "\n"), "\n", fixed = TRUE))
  lines <- str_replace_all(lines, " ", " ")
  if (is.na(date)) {
    d <- str_match(paste(head(lines, 4), collapse = " "), "([A-Z]+DAY), ([A-Z]+ \\d{1,2}, \\d{4})")[1, 3]
    if (!is.na(d)) date <- suppressWarnings(as.Date(str_to_title(d), "%B %d, %Y"))
  }
  no_pos <- str_locate(lines, "No\\.\\s+\\d")[, 1]
  cut <- suppressWarnings(min(no_pos, na.rm = TRUE))
  if (!is.finite(cut)) return(.dc_df())
  cases <- list(); cur_case <- 0L; advs <- list(); cur_adv <- 0L; paren <- NULL; seen_v <- FALSE
  for (ln in lines) {
    left <- str_squish(substr(ln, 1L, cut - 1L)); right <- str_squish(substr(ln, cut, nchar(ln)))
    # Right column first: a new case, its slot, its total time, and the "V."
    # that separates the petitioner's counsel (listed above it) from the
    # respondent's (below) on a Day Call that gives no per-advocate minutes.
    no <- str_match(right, "No\\.\\s+(\\d{2}-\\d{1,5}|\\d{2}A\\d{1,4}|22O\\d{1,4})\\.?")
    if (!is.na(no[1, 2])) {
      cur_case <- cur_case + 1L; seen_v <- FALSE
      cases[[cur_case]] <- list(dkt = no[1, 2], slot = as.integer(str_match(right, "\\((\\d+)\\)")[1, 2]), minutes_total = NA_integer_)
    }
    if (str_detect(right, "^V\\.(\\s|$)")) seen_v <- TRUE
    tm <- str_match(right, "(\\d+)\\s*(hours?|minutes?) for argument")
    if (!is.na(tm[1, 1]) && cur_case > 0L)
      cases[[cur_case]]$minutes_total <- as.integer(tm[1, 2]) * if (str_detect(tm[1, 3], "hour")) 60L else 1L
    if (!nzchar(left)) next
    # Left column: an advocate, then affiliation lines, then the parenthetical.
    if (!is.null(paren)) {
      paren <- paste(paren, left)
      if (str_detect(left, "\\)")) { r <- .dc_role(paren); advs[[cur_adv]]$side <- r$side; advs[[cur_adv]]$minutes <- r$minutes; paren <- NULL }
      next
    }
    nm <- str_match(left, "^(MR|MS|MRS|DR|GEN)\\.?\\s+(.*)$")
    if (!is.na(nm[1, 3])) {
      cur_adv <- cur_adv + 1L
      advs[[cur_adv]] <- list(case = max(cur_case, 1L), name = str_squish(nm[1, 3]), affiliation = character(),
                              side = if (seen_v) "respondent" else "petitioner", minutes = NA_integer_, name_open = TRUE)
      next
    }
    if (cur_adv == 0L) next
    if (str_detect(left, "^\\(")) {
      if (str_detect(left, "\\)")) { r <- .dc_role(left); advs[[cur_adv]]$side <- r$side; advs[[cur_adv]]$minutes <- r$minutes }
      else paren <- left
      advs[[cur_adv]]$name_open <- FALSE
      next
    }
    # A wrapped name is the next all-caps line right under the name.
    if (isTRUE(advs[[cur_adv]]$name_open) && !str_detect(left, "[a-z]")) {
      advs[[cur_adv]]$name <- str_squish(paste(advs[[cur_adv]]$name, left)); next
    }
    advs[[cur_adv]]$name_open <- FALSE
    advs[[cur_adv]]$affiliation <- c(advs[[cur_adv]]$affiliation, left)
  }
  if (!length(advs) || !length(cases)) return(.dc_df())
  do.call(rbind, lapply(advs, function(a) {
    cs <- cases[[min(a$case, length(cases))]]
    data.frame(date = date, dkt = cs$dkt, slot = cs$slot, minutes_total = cs$minutes_total,
               name = str_to_title(a$name), affiliation = paste(a$affiliation, collapse = ", "),
               side = a$side, minutes = a$minutes, stringsAsFactors = FALSE)
  }))
}

#' "Hurst (pet.) · Mooppan (amicus) · Hartnett (resp.)": one line per case.
day_call_line <- function(rows) {
  if (is.null(rows) || !nrow(rows)) return(NA_character_)
  surname <- vapply(rows$name, function(n) {
    n <- str_remove(n, ",?\\s*(Jr|Sr|II|III)\\.?$"); tail(str_split(str_squish(n), "\\s+")[[1]], 1) }, character(1))
  tag <- ifelse(is.na(rows$side), "", ifelse(rows$side == "petitioner", " (pet.)", ifelse(rows$side == "respondent", " (resp.)", " (amicus)")))
  paste(paste0(surname, tag), collapse = " · ")
}

# ---- manifests --------------------------------------------------------------------------

.cal_path <- function(site_dir, f) file.path(site_dir, "arguments", f)

read_calendar <- function(site_dir) {
  p <- .cal_path(site_dir, CAL_FILE)
  if (!file.exists(p)) return(.cal_df())
  j <- tryCatch(fromJSON(p, simplifyDataFrame = TRUE), error = function(e) NULL)
  if (is.null(j) || !is.data.frame(j) || !nrow(j)) return(.cal_df())
  data.frame(session = as.Date(j$session), date = as.Date(j$date), slot = as.integer(j$slot),
             dkt = as.character(j$dkt), caption = as.character(j$caption), stringsAsFactors = FALSE)
}
read_day_calls <- function(site_dir) {
  p <- .cal_path(site_dir, DAYCALL_FILE)
  if (!file.exists(p)) return(.dc_df())
  j <- tryCatch(fromJSON(p, simplifyDataFrame = TRUE), error = function(e) NULL)
  if (is.null(j) || !is.data.frame(j) || !nrow(j)) return(.dc_df())
  chr <- function(nm) { x <- as.character(j[[nm]]); x[is.na(x) | x == "NA"] <- NA_character_; x }
  data.frame(date = as.Date(j$date), dkt = chr("dkt"), slot = as.integer(j$slot), minutes_total = as.integer(j$minutes_total),
             name = chr("name"), affiliation = chr("affiliation"), side = chr("side"), minutes = as.integer(j$minutes),
             stringsAsFactors = FALSE)
}
.write_rows <- function(df, path, date_cols) {
  for (col in date_cols) df[[col]] <- ifelse(is.na(df[[col]]), NA_character_, format(as.Date(df[[col]]), "%Y-%m-%d"))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_json(df, path, auto_unbox = TRUE, dataframe = "rows", na = "null")
  invisible(nrow(df))
}

#' Re-read the calendars for sittings from CAL_REFRESH_DAYS ago on (a calendar
#' is amended as cases are added or removed), keep older sittings as held, and
#' rewrite the manifest. Returns the manifest.
update_argument_calendar <- function(site_dir, index = NULL, as_of = Sys.Date()) {
  index <- index %||% tryCatch(fetch_calendar_index(), error = function(e) { cat("Calendar index unavailable:", conditionMessage(e), "\n"); NULL })
  old <- read_calendar(site_dir)
  if (is.null(index)) return(old)
  cal <- index[index$kind == "calendar", , drop = FALSE]
  if (!nrow(cal)) return(old)
  # A key "October2026" is the sitting's month; fetch the ones from the recent past on.
  key_date <- suppressWarnings(as.Date(paste0("1", cal$key), "%d%B%Y"))
  want <- cal[!is.na(key_date) & key_date >= as.Date(as_of) - CAL_REFRESH_DAYS - 31L, , drop = FALSE]
  new <- do.call(rbind, lapply(seq_len(nrow(want)), function(i) tryCatch(parse_argument_calendar(.cal_get(want$url[i], "pdf")),
    error = function(e) { cat("Calendar", want$url[i], "failed:", conditionMessage(e), "\n"); NULL })))
  if (is.null(new) || !nrow(new)) { cat("Argument calendar: nothing new read\n"); return(old) }
  keep <- old[!old$session %in% unique(new$session), , drop = FALSE]
  all <- rbind(keep, new); all <- all[order(all$date, all$slot, all$dkt), , drop = FALSE]
  .write_rows(all, .cal_path(site_dir, CAL_FILE), c("session", "date"))
  cat(sprintf("Argument calendar: %d sitting(s) read, %d row(s) in manifest (%s to %s)\n",
              length(unique(new$session)), nrow(all), format(min(all$date, na.rm = TRUE)), format(max(all$date, na.rm = TRUE))))
  all
}

#' Fetch every Day Call the index links that the manifest lacks (at most
#' DAYCALL_MAX_NEW a run), append, rewrite. Returns the manifest.
update_day_calls <- function(site_dir, index = NULL) {
  index <- index %||% tryCatch(fetch_calendar_index(), error = function(e) { cat("Calendar index unavailable:", conditionMessage(e), "\n"); NULL })
  old <- read_day_calls(site_dir)
  if (is.null(index)) return(old)
  dc <- index[index$kind == "daycall", , drop = FALSE]
  todo <- dc[!dc$key %in% format(old$date), , drop = FALSE]
  todo <- todo[order(todo$key, decreasing = TRUE), , drop = FALSE]
  todo <- head(todo, DAYCALL_MAX_NEW)
  if (!nrow(todo)) { cat("Day Calls: none new (", length(unique(old$date)), " held)\n", sep = ""); return(old) }
  new <- do.call(rbind, lapply(seq_len(nrow(todo)), function(i) tryCatch(parse_day_call(.cal_get(todo$url[i], "pdf"), as.Date(todo$key[i])),
    error = function(e) { cat("Day Call", todo$url[i], "failed:", conditionMessage(e), "\n"); NULL })))
  if (is.null(new) || !nrow(new)) return(old)
  all <- rbind(old, new); all <- all[order(all$date, all$slot, all$dkt), , drop = FALSE]
  .write_rows(all, .cal_path(site_dir, DAYCALL_FILE), "date")
  cat(sprintf("Day Calls: %d day(s) read, %d advocate row(s); %d day(s) in manifest\n",
              length(unique(new$date)), nrow(new), length(unique(all$date))))
  all
}
