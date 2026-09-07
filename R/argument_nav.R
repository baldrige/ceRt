# argument_nav.R ---------------------------------------------------------------
# The Oral Argument Navigator: for every merits grant, extract its argument
# lifecycle from the docket events and present each Term's argument calendar,
# grouped by sitting (October, November, ... April sessions).
#
# A granted case's docket records the schedule directly:
#   "SET FOR ARGUMENT on Wednesday, April 23, 2025."   -> scheduled date
#   "Argued. For petitioners: ...  For respondents: ..." -> argued date + advocates
#   "Writ of certiorari DISMISSED as improvidently granted." -> DIG'd
#   a merits judgment / opinion entry                    -> decided
#
# A sitting draws cases from MULTIPLE docket-number terms (a 22-, 23- and 24-
# docket can all be argued in OT2024), so cases are grouped by the term they are
# ARGUED in, not their docket term. Grant detection is reused from cert_funnel.R
# (classify_petitions, adversarially verified); this module only adds the
# argument-stage extraction and rendering.

suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse); library(lubridate); library(htmltools)
  library(httr2)
})

# Shared modules: classify_petitions()/term_label() from cert_funnel.R and the
# page helpers from page_style.R, sourced relative to this file.
local({
  here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA)
  find <- function(f) {
    if (!is.na(here) && file.exists(file.path(here, f))) file.path(here, f)
    else if (file.exists(file.path("R", f))) file.path("R", f)
    else f
  }
  sys.source(find("cert_funnel.R"), envir = globalenv())
  sys.source(find("page_style.R"), envir = globalenv())
  sys.source(find("interactive_theme.R"), envir = globalenv())
  sys.source(find("qp_extract.R"), envir = globalenv())   # find_opening_doc_url()
})

# ---- argument-stage classification --------------------------------------------

# Classify one granted case's argument lifecycle from its events. One-row tibble.
# NB: the JSON proceedings text embeds <a href> anchors (e.g. around "opinion"),
# which break literal phrases like "opinion of the Court" -- detection here is
# written to tolerate them, and the anchor is what yields the slip-opinion URL.
classify_argument <- function(events) {
  empty <- tibble(scheduled_date = as.Date(NA), argued_date = as.Date(NA),
                  decided_date = as.Date(NA), n_settings = 0L,
                  vided = FALSE, dig = FALSE, argued_text = NA_character_,
                  opinion_author = NA_character_, opinion_url = NA_character_,
                  status = "granted")
  if (!is.data.frame(events) || !("Proceedings and Orders" %in% names(events)) ||
      nrow(events) == 0) return(empty)
  txt <- events[["Proceedings and Orders"]]; txt[is.na(txt)] <- ""
  edate <- suppressWarnings(mdy(events$Date))
  ord <- order(edate); txt <- txt[ord]; edate <- edate[ord]

  # SET FOR ARGUMENT on [Weekday, ]Month D, YYYY.  The LAST setting is effective
  # (a reschedule re-sets it), so a "Reset for argument" is handled implicitly.
  set_hits <- str_match(txt, regex(
    "SET FOR ARGUMENT on (?:[A-Za-z]+,\\s+)?([A-Za-z]+ \\d{1,2}, \\d{4})",
    ignore_case = TRUE))[, 2]
  set_idx <- which(!is.na(set_hits))
  scheduled <- if (length(set_idx)) mdy(tail(set_hits[set_idx], 1)) else as.Date(NA)

  # The LAST argument, not the first. A case restored to the calendar for
  # reargument -- Knick v. Township of Scott (17-647, argued Oct 2018 and Jan
  # 2019), Louisiana v. Callais (24-109, Mar 2025 and Oct 2025) -- is decided
  # after its last argument, and taking the first put Callais's June 2025
  # reargument order (a dissent's PDF attached) down as its decision, ten
  # months early. Measured against the Court's Granted & Noted Lists
  # (docs/granted-noted-audit-2026-09.md).
  arg_idx <- which(str_detect(txt, regex("^Argued\\.", ignore_case = TRUE)))
  argued_date <- if (length(arg_idx)) edate[arg_idx[length(arg_idx)]] else as.Date(NA)
  argued_text <- if (length(arg_idx)) txt[arg_idx[length(arg_idx)]] else NA_character_

  dig <- any(str_detect(txt, regex("DISMISSED as improvidently granted", ignore_case = TRUE)))
  # Post-grant dismissal (parties settle/withdraw): a granted case that ends
  # before argument, e.g. "Case Dismissed - Rule 46." or removed from the docket.
  dismissed <- any(
    str_detect(txt, regex("^Case [Dd]ismissed", ignore_case = TRUE)) |
    str_detect(txt, regex("dismissed[^.]{0,40}Rule 46|Rule 46[^.]{0,40}dismiss", ignore_case = TRUE)) |
    str_detect(txt, regex("removed from the docket", ignore_case = TRUE)))

  # Decision entry. Several tolerant signals; the caps-disposition uses a dot-
  # crossing gap because the docket infixes the case number ("Judgment in No.
  # 24-1287 is VACATED"). A linked supremecourt.gov/opinions/ PDF is definitive.
  dec_idx <- which(
    str_detect(txt, "href\\s*=\\s*['\"][^'\"]*supremecourt\\.gov/opinions/") |
    str_detect(txt, "Judgment.{0,80}(AFFIRMED|REVERSED|VACATED)") |
    str_detect(txt, regex("announced the judgment", ignore_case = TRUE)) |
    str_detect(txt, regex("delivered the .{0,180}?opinion", ignore_case = TRUE)) |
    str_detect(txt, regex("Adjudged to be (AFFIRMED|REVERSED)", ignore_case = TRUE)) |
    str_detect(txt, "^Judgment Issued")
  )
  # A case that was argued is decided AFTER it was argued. Without this, 25-332
  # (Trump v. Slaughter) was "decided" on 22 Sep 2025 -- its grant date -- because
  # the same day's entry granted the companion stay application with a linked
  # per curiam, and the first decision-shaped entry won. The merits opinion came
  # nine months later.
  if (!is.na(argued_date)) dec_idx <- dec_idx[edate[dec_idx] >= argued_date]
  decided_date <- if (length(dec_idx)) edate[dec_idx[1]] else as.Date(NA)

  # Opinion author + slip-opinion URL from the decision entries.
  opinion_author <- NA_character_; opinion_url <- NA_character_
  if (length(dec_idx)) {
    dtext <- paste(txt[dec_idx], collapse = " ")
    am <- str_match(dtext, "([A-Z][A-Za-z'’]+), ((?:C\\. )?J)\\.,.{0,120}?(?:delivered|announced)")
    if (!is.na(am[1, 2])) {
      opinion_author <- if (str_detect(am[1, 3], "C")) paste0(am[1, 2], ", C.J.") else am[1, 2]
    } else if (str_detect(dtext, regex("per curiam", ignore_case = TRUE))) {
      opinion_author <- "Per Curiam"
    }
    um <- str_match(dtext, "href\\s*=\\s*['\"]([^'\"]*supremecourt\\.gov/opinions/[^'\"]+)['\"]")
    if (!is.na(um[1, 2])) opinion_url <- um[1, 2]
  }

  status <- if (dig) "DIG'd"
    else if (!is.na(decided_date)) "Decided"
    else if (!is.na(argued_date)) "Argued"
    else if (!is.na(scheduled)) "Scheduled"
    else if (dismissed) "Dismissed"          # granted, then withdrawn before scheduling
    else "Granted"

  tibble(scheduled_date = scheduled, argued_date = argued_date,
         decided_date = decided_date, n_settings = length(set_idx),
         vided = any(str_detect(txt, regex("SET FOR ARGUMENT.*VIDED", ignore_case = TRUE))),
         dig = dig, argued_text = argued_text,
         opinion_author = opinion_author, opinion_url = opinion_url,
         status = status)
}

# Compact "argued by" from an "Argued. For <side>: <Name>, <City>. ..." entry:
# the lead advocate name from each side, joined.
extract_advocates <- function(argued_text) {
  if (is.na(argued_text) || argued_text == "") return(NA_character_)
  t <- str_remove(argued_text, regex("^Argued\\.\\s*", ignore_case = TRUE))
  segs <- str_match_all(t, "For [^:]+:\\s*([A-Z][^,;]+)")[[1]]
  if (nrow(segs) == 0) return(str_trunc(str_squish(t), 70))
  str_trunc(paste(unique(str_squish(segs[, 2])), collapse = " · "), 70)
}

# Petition-for-certiorari URL from a case's events -- or, for a direct appeal,
# the jurisdictional statement standing in for it. Thin alias; see
# find_opening_doc_url() in qp_extract.R. This was a byte-for-byte copy of
# conference_dash.R's find_petition_url(), which is why neither learned about
# appeals when the other did.
arg_petition_url <- function(events) find_opening_doc_url(events)

# ---- oral-argument media (transcript + audio) ---------------------------------
# The docket JSON does not carry the argument transcript/audio, but supremecourt
# .gov exposes them at stable locations. Audio is a predictable per-case URL;
# the transcript PDF has an unpredictable filename suffix, so its per-Term index
# is scraped once to map docket -> URL.

# Scrape one Term's transcript index -> named vector (docket -> absolute PDF URL).
# Returns an empty vector on any failure (media is best-effort, never fatal).
.media_get <- function(url) {
  if (exists("scotus_perform") && exists("scotus_req")) {
    resp <- scotus_perform(scotus_req(url))
    if (resp_status(resp) != 200L) stop("HTTP ", resp_status(resp))
    return(resp_body_string(resp))
  }
  request(url) |>
    req_user_agent("ceRt oral-argument navigator (github.com/baldrige/ceRt)") |>
    req_timeout(30) |> req_perform() |> resp_body_string()
}

fetch_transcript_map <- function(term) {
  url <- paste0("https://www.supremecourt.gov/oral_arguments/argument_transcript/", term)
  html <- tryCatch(.media_get(url), error = function(e) "")
  m <- str_match_all(html, "argument_transcripts/(\\d{4})/([^\"'>\\s]+\\.pdf)")[[1]]
  if (nrow(m) == 0) return(setNames(character(), character()))
  files <- m[, 3]; yrs <- m[, 2]
  dockets <- str_extract(files, "^[^_]+")               # "24-316_1a72.pdf" -> "24-316"
  urls <- paste0("https://www.supremecourt.gov/oral_arguments/argument_transcripts/",
                 yrs, "/", files)
  keep <- !duplicated(dockets)
  setNames(urls[keep], dockets[keep])
}

# The Court's argument feeds, one per Term and per medium:
#   /rss/argument_audio_rss.aspx?TYear=NN        item: title "Sripetch v. SEC (25-466)",
#   /rss/argument_transcripts_rss.aspx?TYear=NN        link, pubDate (when it was posted)
# Back to OT17. Every title names exactly one docket (checked over six feeds,
# 2026-09-06), an original action as "141-Orig". The transcript link is the PDF; the audio link is the Court's
# player page for the case. Returns (dkt, url, posted) or an empty frame when
# the feed is down, which the caller treats as "fall back to the scrape".
.media_df <- function() data.frame(dkt = character(), url = character(), posted = as.Date(character()), stringsAsFactors = FALSE)
fetch_media_feed <- function(kind = c("audio", "transcripts"), term) {
  kind <- match.arg(kind)
  url <- sprintf("https://www.supremecourt.gov/rss/argument_%s_rss.aspx?TYear=%02d", kind, as.integer(term) %% 100L)
  xml <- tryCatch(.media_get(url), error = function(e) { message("argument ", kind, " feed for OT", term %% 100L, " unavailable: ", conditionMessage(e)); "" })
  blocks <- str_match_all(xml, regex("<item>(.*?)</item>", dotall = TRUE))[[1]][, 2]
  if (!length(blocks)) return(.media_df())
  field <- function(b, tag) {
    m <- str_match(b, regex(paste0("<", tag, ">(.*?)</", tag, ">"), dotall = TRUE))[1, 2]
    if (is.na(m)) NA_character_ else str_squish(str_remove_all(m, "<!\\[CDATA\\[|\\]\\]>"))
  }
  rows <- lapply(blocks, function(b) {
    title <- field(b, "title"); link <- field(b, "link"); pub <- field(b, "pubDate")
    # An original action is titled "(141-Orig)"; the docket API calls it 22O141.
    dk <- str_extract(title %||% "", "\\d{2}-\\d{1,5}|\\d{2}A\\d{1,4}|22O\\d{1,4}|\\d{1,4}-Orig")
    if (!is.na(dk)) dk <- str_replace(dk, "^(\\d{1,4})-Orig$", "22O\\1")
    if (is.na(dk) || is.na(link) || !nzchar(link)) return(NULL)
    data.frame(dkt = dk, url = str_replace(link, "^http://", "https://"),
               posted = suppressWarnings(lubridate::dmy(str_extract(pub %||% "", "\\d{1,2} [A-Za-z]{3} \\d{4}"))),
               stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(.media_df())
  out <- do.call(rbind, rows)
  out[!duplicated(out$dkt), , drop = FALSE]
}

# Add transcript_url, audio_url (and their posting dates) to an argument table.
#
# The feeds first: a link the feed carries is a file the Court has posted. The
# old way built the audio URL for every argued case the moment the docket said
# "Argued.", which linked a page that 404s until the Court posts the audio, and
# scraped the transcript index page. The scrape stays as the fallback for a
# Term whose feed is down, and the built audio URL as the fallback for a Term
# whose audio feed is down, so a feed outage costs nothing that was there
# before. Two requests per Term.
attach_media <- function(tbl) {
  terms <- sort(unique(tbl$term[!is.na(tbl$term)]))
  feeds <- lapply(terms, function(t) list(
    audio = fetch_media_feed("audio", t),
    transcripts = fetch_media_feed("transcripts", t)))
  names(feeds) <- as.character(terms)
  tmap <- setNames(lapply(terms, function(t)
    if (nrow(feeds[[as.character(t)]]$transcripts)) NULL else fetch_transcript_map(t)), as.character(terms))
  look <- function(d, t, kind) {
    if (is.na(t)) return(list(url = NA_character_, posted = as.Date(NA), have_feed = FALSE))
    f <- feeds[[as.character(t)]][[kind]]
    if (nrow(f)) {
      i <- match(d, f$dkt)
      return(list(url = if (is.na(i)) NA_character_ else f$url[i],
                  posted = if (is.na(i)) as.Date(NA) else f$posted[i], have_feed = TRUE))
    }
    list(url = NA_character_, posted = as.Date(NA), have_feed = FALSE)
  }
  n <- nrow(tbl)
  tr_url <- character(n); tr_posted <- as.Date(rep(NA, n)); au_url <- character(n); au_posted <- as.Date(rep(NA, n))
  for (i in seq_len(n)) {
    d <- tbl$dkt[i]; t <- tbl$term[i]
    tr <- look(d, t, "transcripts")
    tr_url[i] <- if (tr$have_feed) tr$url else {
      m <- if (is.na(t)) NULL else tmap[[as.character(t)]]
      if (!is.null(m) && d %in% names(m)) unname(m[[d]]) else NA_character_ }
    tr_posted[i] <- tr$posted
    au <- look(d, t, "audio")
    au_url[i] <- if (au$have_feed) au$url
      else if (!is.na(tbl$argued_date[i])) paste0("https://www.supremecourt.gov/oral_arguments/audio/", t, "/", d)
      else NA_character_
    au_posted[i] <- au$posted
  }
  tbl$transcript_url <- tr_url; tbl$transcript_posted <- tr_posted
  tbl$audio_url <- au_url; tbl$audio_posted <- au_posted
  message(sprintf("attach_media(): %d Term(s); transcripts %d, audio %d of %d rows (feeds%s)",
                  length(terms), sum(!is.na(tr_url)), sum(!is.na(au_url)), n,
                  if (any(vapply(feeds, function(f) nrow(f$audio) > 0, logical(1)))) "" else " unavailable; scrape and built URLs"))
  tbl
}

# ---- assemble the argument table ----------------------------------------------

# The Term a case is ARGUED in: Oct-Dec -> that year's Term; Jan-Jun -> prior.
argument_term <- function(d) if_else(is.na(d), NA_integer_,
                                     if_else(month(d) >= 9L, year(d), year(d) - 1L))

# Projected argument Term for a granted case with no argument date yet. A grant
# made after the current Term's argument calendar has filled -- roughly mid-
# January onward through the summer -- is held to the following Term; grants in
# the fall and early winter belong to the Term in session. (A case granted late
# January, like the Jan-23-conference grants, is already too late for the spring
# sittings.)
unscheduled_arg_term <- function(grant_date) {
  m <- month(grant_date); d <- day(grant_date)
  gterm <- if_else(m >= 9L, year(grant_date), year(grant_date) - 1L)
  held <- (m >= 2L & m <= 8L) | (m == 1L & d >= 15L)
  as.integer(if_else(is.na(grant_date), NA_integer_, if_else(held, gterm + 1L, gterm)))
}

# Build a tidy table of every merits grant with an argument date (scheduled or
# argued), across all docket terms present in `cases`. `qp_map` (optional) maps
# raw docket -> QP <details> HTML.
# Apply the Court's monthly argument calendar (R/argument_calendar.R) to the
# classified rows: a docket with no "SET FOR ARGUMENT" entry yet but a place on
# a published calendar is Scheduled for that day. Where both exist and differ,
# the docket wins and the disagreement is logged -- the calendar is a
# cross-check, and it is amended as cases are added or moved.
.apply_calendar <- function(arg0, calendar) {
  if (is.null(calendar) || !is.data.frame(calendar) || !nrow(calendar)) return(arg0)
  cal1 <- calendar |> filter(!is.na(date)) |> arrange(desc(date)) |> distinct(dkt, .keep_all = TRUE) |>
    transmute(dkt, cal_date = date, cal_slot = slot)
  arg0 <- arg0 |> left_join(cal1, by = "dkt")
  fill <- is.na(arg0$scheduled_date) & is.na(arg0$argued_date) & !is.na(arg0$cal_date)
  if (any(fill)) {
    arg0$scheduled_date[fill] <- arg0$cal_date[fill]
    arg0$status[fill & arg0$status == "Granted"] <- "Scheduled"
    message("calendar: scheduled ", sum(fill), " case(s) the docket has not yet set: ",
            paste(arg0$dkt[fill], collapse = ", "))
  }
  differ <- !is.na(arg0$scheduled_date) & !is.na(arg0$cal_date) & is.na(arg0$argued_date) &
            arg0$scheduled_date != arg0$cal_date
  if (any(differ))
    message("calendar: ", sum(differ), " case(s) where the docket's argument date differs from the calendar's: ",
            paste(sprintf("%s (docket %s, calendar %s)", arg0$dkt[differ], arg0$scheduled_date[differ], arg0$cal_date[differ]), collapse = "; "))
  arg0
}

build_argument_table <- function(cases, qp_map = NULL, calendar = NULL, daycalls = NULL) {
  cls <- classify_petitions(cases)
  # No petitions at all (a frame of original actions alone) unnests to a frame
  # with no columns, and filter() on it would error.
  granted <- if (nrow(cls)) cls |> filter(outcome == "granted") |> distinct(dkt, .keep_all = TRUE) |>
    transmute(dkt, grant_date = outcome_date) else tibble(dkt = character(), grant_date = as.Date(character()))
  # Original actions (22O###) are not petitions and classify_petitions() no
  # longer sees them, but 16 of the 44 have been argued -- on exceptions to a
  # Special Master's report -- and a sitting that hears one should name it. The
  # "grant" is the order granting leave to file the bill of complaint, or the
  # docketing date for the handful with no such entry. Known limitation: a case
  # argued twice (No. 141: 2018 and 2024) keeps its first argument only, because
  # classify_argument() does. See docs/original-jurisdiction.md.
  if (exists("classify_original_events")) {
    oi <- which(str_detect(cases$dkt, "^\\d{2}O\\d+$"))
    oi <- oi[vapply(cases$events[oi], function(ev)
      is.data.frame(ev) && any(str_detect(coalesce(ev[["Proceedings and Orders"]], ""),
        regex("^Argued\\.|SET FOR ARGUMENT", ignore_case = TRUE))), logical(1))]
    if (length(oi)) {
      og <- tibble(dkt = cases$dkt[oi], grant_date = as.Date(vapply(oi, function(i) {
        ev <- cases$events[[i]]
        oc <- classify_original_events(ev[["Proceedings and Orders"]], suppressWarnings(mdy(ev$Date)))
        as.numeric(if (!is.na(oc$leave_granted)) oc$leave_granted else cases$date[i])
      }, numeric(1)), origin = "1970-01-01"))
      granted <- bind_rows(granted, og |> filter(!dkt %in% granted$dkt))
    }
  }
  orig_dkts <- granted$dkt[str_detect(granted$dkt, "^\\d{2}O\\d+$")]
  g <- cases |> filter(dkt %in% granted$dkt) |> distinct(dkt, .keep_all = TRUE)
  if (nrow(g) == 0) return(tibble())

  arg0 <- bind_cols(
    g |> transmute(dkt, caption = str_squish(caption %||% dkt)),
    map_dfr(g$events, classify_argument)
  ) |> .apply_calendar(calendar)
  # The Day Call's advocates, for a case not yet argued (the docket's own
  # "Argued. For petitioner: ..." entry comes after the argument).
  if (!is.null(daycalls) && is.data.frame(daycalls) && nrow(daycalls) && exists("day_call_line")) {
    dcl <- daycalls |> group_by(dkt) |> summarise(advocates_dc = day_call_line(pick(everything())),
                                                  minutes_total = suppressWarnings(max(minutes_total, na.rm = TRUE)), .groups = "drop") |>
      mutate(minutes_total = if_else(is.finite(minutes_total), minutes_total, NA_integer_))
    arg0 <- arg0 |> left_join(dcl, by = "dkt")
  } else arg0$advocates_dc <- NA_character_
  arg <- arg0 |>
    left_join(granted, by = "dkt") |>
    mutate(
      petition_url = map_chr(g$events, arg_petition_url),
      advocates = map_chr(argued_text, extract_advocates),
      arg_ref = coalesce(argued_date, scheduled_date),
      # Argued/scheduled cases key off the argument date; granted-but-unscheduled
      # cases are projected to the Term they will likely be heard in.
      term = if_else(!is.na(arg_ref), argument_term(arg_ref),
                     unscheduled_arg_term(grant_date)),
      sitting_date = if_else(is.na(arg_ref), as.Date(NA),
                             floor_date(arg_ref, "month")),
      sitting = if_else(is.na(arg_ref), NA_character_, format(arg_ref, "%B %Y")),
      qp = if (is.null(qp_map)) NA_character_ else unname(qp_map[dkt])
    ) |>
    # Keep cases with an argument date, or still pending ("Granted"). Drop cases
    # that ended before ever being scheduled (dismissed/DIG'd with no argument),
    # e.g. a Rule 46 settlement after cert -- they are not oral arguments.
    filter(!is.na(term), !is.na(arg_ref) | status == "Granted")
  # Original actions only in Terms the Navigator already covers: the archive
  # begins at OT17, and No. 8 (Arizona v. California) was argued in 1962. A
  # one-case page for OT1961 is not a Navigator Term.
  if (length(orig_dkts)) {
    pet_terms <- unique(arg$term[!arg$dkt %in% orig_dkts])
    arg <- arg |> filter(!(dkt %in% orig_dkts) | term %in% pet_terms)
  }
  arg
}

# ---- rendering ----------------------------------------------------------------

# Parchment-toned status tints, in status-factor-level order, applied to the
# reactable Status column via data_color(target_columns=).
# Must list every status classify_argument() can return. It omitted "Dismissed"
# -- granted, then withdrawn before scheduling (Rule 46, or removed from the
# docket) -- so factor() turned that case into NA and it rendered with an empty
# Status cell and the na_color fill. Silent, and only on a Term that happened to
# contain one, which is why it went unnoticed.
STATUS_LEVELS <- c("Granted", "Scheduled", "Argued", "Decided", "DIG'd", "Dismissed")
# STATUS_FILL itself lives in palette.R; STATUS_LEVELS above is the legend order.

# Render one Term's argument calendar as the interactive editorial table (matches
# the daily/conference dashboards): a flat, sortable/filterable reactable with a
# Sitting column (October, November, …) instead of row-group bands. When is a real
# date so it value-sorts; Status is colour-scaled by state and links decided cases
# to the slip opinion.
argument_term_page <- function(tbl, term, out_dir) {
  d <- tbl |>
    filter(term == !!term) |>
    # Scheduled cases order by when they are heard. Unscheduled ones have no
    # date to order by any more -- the grant date was doing that job and is no
    # longer shown -- so they go alphabetical on the caption the reader can
    # actually see. Scheduled rows all take "" on that key, so it is inert for
    # them and the later keys still decide ties within a sitting.
    arrange(is.na(sitting_date), sitting_date, arg_ref,
            if_else(is.na(arg_ref), str_to_lower(strip_caption_roles(caption)), ""),
            desc(grant_date), as.integer(str_extract(dkt, "\\d+$")))
  if (nrow(d) == 0) return(invisible(NULL))
  all_unscheduled <- all(is.na(d$arg_ref))

  d <- d |>
    mutate(
      Sitting = if_else(is.na(sitting), "Not yet scheduled", sitting),
      # The argument date, and nothing else. This used to fall back to the grant
      # date, which put two different quantities in one column under one heading:
      # a row reading "March 3, 2026" might mean argued that day or merely
      # granted that day, and nothing on the page said which. A case with no
      # argument date now reads "—" (sub_missing below).
      When = arg_ref,                                # Date -> value-sorts
      Case = str_c(
        "<a href='../cases/", dkt, ".html' target='_blank'>",
        strip_caption_roles(caption), "</a>"),
      Docket = dkt,
      status = factor(status, levels = STATUS_LEVELS),
      # Decided cases show the majority author and link to the slip opinion;
      # other states show the plain status word. `status` (hidden) drives colour.
      status_disp = case_when(
        status == "Decided" & !is.na(opinion_url) & !is.na(opinion_author) ~
          str_c("[Decided · ", opinion_author, "](", opinion_url, ")"),
        status == "Decided" & !is.na(opinion_url) ~ str_c("[Decided](", opinion_url, ")"),
        status == "Decided" & !is.na(opinion_author) ~ str_c("Decided · ", opinion_author),
        TRUE ~ as.character(status)
      ),
      # The docket's "Argued." entry names the advocates after the fact; the
      # Day Call names them the morning of. For a case not yet argued, the
      # Day Call's line stands in.
      argued_by = coalesce(advocates, if ("advocates_dc" %in% names(d)) advocates_dc else NA_character_, "—"),
      media = pmap_chr(list(transcript_url, audio_url), function(tr, au) {
        parts <- c(if (!is.na(tr)) str_c("[Transcript](", tr, ")"),
                   if (!is.na(au)) str_c("[Audio](", au, ")"))
        if (length(parts) == 0) "—" else paste(parts, collapse = " · ")
      }),
      qp = if_else(is.na(qp), "—", qp)
    )

  # Drop columns with no content this Term (all-unscheduled Terms lack advocates
  # and media; historical Terms may lack QP).
  has_argued <- any(d$argued_by != "—")
  has_media  <- any(d$media != "—")
  has_qp     <- any(d$qp != "—")
  # Separate writings, from the Court's Granted & Noted List (R/granted_noted.R):
  # "Thomas and Alito dissenting; Kagan concurring in the judgment". Joined in
  # by render_arguments.R as gn_writings; the column appears only where a Term
  # has any.
  if ("gn_writings" %in% names(d)) d$writings <- if_else(is.na(d$gn_writings), "—", d$gn_writings)
  has_writings <- "writings" %in% names(d) && any(d$writings != "—")
  keep <- c("Sitting", if (!all_unscheduled) "When", "Case", "Docket", "status_disp",
            if (has_writings) "writings",
            if (has_argued) "argued_by", if (has_media) "media", if (has_qp) "qp",
            "status")
  tb <- d |> select(all_of(keep))

  # Left-aligned DATA cells (headers stay centered); status is hidden, so measure
  # nth-child over the visible columns only.
  vis <- setdiff(names(tb), "status")
  left_cols <- match(intersect(c("Sitting", "Case", "status_disp", "writings", "argued_by", "media", "qp"), vis), vis)

  labels <- list(status_disp = "Status", writings = "Separate writings", argued_by = "Argued by",
                 media = "Argument", qp = "Questions Presented")
  labels <- labels[names(labels) %in% names(tb)]
  gt_tbl <- tb |>
    gt() |>
    fmt_markdown(columns = any_of(c("Case", "status_disp", "media", "qp"))) |>
    fmt_date(columns = any_of("When"), date_style = "m_day_year") |>
    sub_missing(columns = any_of("When"), missing_text = "—") |>
    data_color(columns = status, target_columns = status_disp, method = "factor",
               palette = unname(STATUS_FILL[STATUS_LEVELS]), na_color = GRANT_NA) |>
    cols_hide(columns = status) |>
    cols_align("center", columns = everything()) |>
    cols_label(.list = labels) |>
    cols_width(Case ~ px(240))
  if (has_qp) gt_tbl <- gt_tbl |> cols_width(qp ~ px(190))
  if (has_writings) gt_tbl <- gt_tbl |> cols_width(writings ~ px(170))

  n <- nrow(d)
  dek <- if (all_unscheduled)
    paste0(n, if (n == 1) " granted case" else " granted cases",
           " awaiting an argument date &mdash; sortable and filterable.")
  else
    paste0(n, if (n == 1) " case" else " cases",
           " argued or scheduled &mdash; sortable and filterable. Sort by any column, ",
           "or filter by <em>Sitting</em> or <em>Status</em>.")

  scr_interactive(gt_tbl, n_rows = n) |>
    scr_write_page(
      file.path(out_dir, str_c("arg_", term, ".html")),
      kicker = "Supreme Court of the United States",
      title = paste0(term_label(term - 2000L), " &mdash; Oral Arguments"),
      dek = dek, n_rows = n, left_cols = left_cols,
      footer = paste0("Status tracks each grant from Granted through Scheduled, ",
                      "Argued, and Decided (with the majority author, linked to the ",
                      "slip opinion)."),
      active = "/arguments/",
      crumb = list(label = paste0("October Term ", term),
                   section = list(href = "/arguments/", label = "Arguments")),
      back = list(href = "index.html", label = "&larr; All argument Terms"))
  invisible(file.path(out_dir, str_c("arg_", term, ".html")))
}

# Styled Term index over the arg_*.html pages (case counts read back from each).
argument_index <- function(out_dir) {
  files <- list.files(out_dir, pattern = "^arg_\\d{4}\\.html$")
  if (length(files) == 0) return(invisible(NULL))
  terms <- as.integer(str_extract(files, "\\d{4}"))
  ord <- order(terms, decreasing = TRUE); files <- files[ord]; terms <- terms[ord]

  read_count <- function(f) {
    h <- paste(readLines(file.path(out_dir, f), warn = FALSE, encoding = "UTF-8"),
               collapse = " ")
    # Subtitle is "N case(s) argued or scheduled" or, for an all-unscheduled
    # Term, "N granted case(s) awaiting an argument date" -- allow the "granted".
    m <- str_match(h, "([0-9,]+)\\s+(?:granted\\s+)?cases?")[, 2]
    if (is.na(m)) NA_integer_ else as.integer(str_remove_all(m, ","))
  }
  items <- purrr::map2(files, terms, function(f, t) {
    n <- read_count(f)
    list(href = f, label = term_label(t - 2000L),
         meta = if (!is.na(n)) paste0(n, if (n == 1) " case" else " cases") else "")
  })
  styled_index_page(
    file.path(out_dir, "index.html"),
    title = "Oral Argument Navigator — SCOTUS",
    kicker = "Supreme Court of the United States",
    heading = "Oral Argument Navigator",
    dek = "Every granted case and when it is heard, Term by Term and sitting by sitting.",
    items = items,
    active = "/arguments/",   # was back = "← All dashboards" -> "/" (wrong both ways)
    # Canonical URL for this page (see social_meta()).
    path = "/arguments/"
  )
  patch_prev_next(out_dir, "^arg_\\d{4}\\.html$", "Term",
                  key   = function(f) as.integer(str_extract(f, "\\d{4}")),
                  label = function(f) paste0("October Term ", str_extract(f, "\\d{4}")),
                  end_label = "Current Term")
  invisible(file.path(out_dir, "index.html"))
}

# Build the table (or use a prebuilt `tbl`), render a page per argued Term, and
# (re)build the index. Passing `tbl` avoids re-running the classifier when the
# caller has already built the table (e.g. to attach QP from a cache).
render_argument_nav <- function(cases = NULL, out_dir, qp_map = NULL, tbl = NULL) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (is.null(tbl)) tbl <- build_argument_table(cases, qp_map = qp_map)
  if (nrow(tbl) == 0) { message("No argued/scheduled grants found."); return(invisible(NULL)) }
  if (!"audio_url" %in% names(tbl)) tbl <- attach_media(tbl)   # transcript + audio links
  terms <- tbl |> distinct(term) |> filter(!is.na(term)) |> arrange(term) |> pull(term)
  # A Term's fall sitting is argued from the PRIOR docket term, so the earliest
  # Term in the archive is incomplete (we lack the term before it). Drop it so it
  # doesn't read as a data error; every later Term has both docket terms present.
  min_dkt_year <- 2000L + suppressWarnings(min(as.integer(str_sub(tbl$dkt, 1, 2)), na.rm = TRUE))
  if (length(terms) > 1) terms <- terms[terms > min_dkt_year]
  for (t in terms) argument_term_page(tbl, t, out_dir)
  argument_index(out_dir)
  invisible(terms)
}
