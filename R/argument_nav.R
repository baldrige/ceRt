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

  arg_idx <- which(str_detect(txt, regex("^Argued\\.", ignore_case = TRUE)))
  argued_date <- if (length(arg_idx)) edate[arg_idx[1]] else as.Date(NA)
  argued_text <- if (length(arg_idx)) txt[arg_idx[1]] else NA_character_

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

# Petition-for-certiorari URL from a case's events (handles both the historical
# scrape's Document_*/links_* and the JSON build_events() docs_*/links_* layouts).
arg_petition_url <- function(events) {
  if (!is.data.frame(events) || nrow(events) == 0) return(NA_character_)
  desc_cols <- str_subset(names(events), "^(docs_|Document_)")
  link_cols <- str_subset(names(events), "^links_")
  if (length(desc_cols) == 0 || length(link_cols) == 0) return(NA_character_)
  for (i in seq_len(nrow(events))) {
    descs <- unlist(events[i, desc_cols], use.names = FALSE)
    links <- unlist(events[i, link_cols], use.names = FALSE)
    hit <- which(!is.na(descs) & str_detect(descs, regex("^Petition", ignore_case = TRUE)))
    hit <- hit[hit <= length(links)]
    if (length(hit) > 0 && !is.na(links[hit[1]])) return(links[hit[1]])
  }
  NA_character_
}

# ---- oral-argument media (transcript + audio) ---------------------------------
# The docket JSON does not carry the argument transcript/audio, but supremecourt
# .gov exposes them at stable locations. Audio is a predictable per-case URL;
# the transcript PDF has an unpredictable filename suffix, so its per-Term index
# is scraped once to map docket -> URL.

# Scrape one Term's transcript index -> named vector (docket -> absolute PDF URL).
# Returns an empty vector on any failure (media is best-effort, never fatal).
fetch_transcript_map <- function(term) {
  url <- paste0("https://www.supremecourt.gov/oral_arguments/argument_transcript/", term)
  html <- tryCatch(
    request(url) |>
      req_user_agent("ceRt oral-argument navigator (github.com/baldrige/ceRt)") |>
      req_timeout(30) |> req_perform() |> resp_body_string(),
    error = function(e) "")
  m <- str_match_all(html, "argument_transcripts/(\\d{4})/([^\"'>\\s]+\\.pdf)")[[1]]
  if (nrow(m) == 0) return(setNames(character(), character()))
  files <- m[, 3]; yrs <- m[, 2]
  dockets <- str_extract(files, "^[^_]+")               # "24-316_1a72.pdf" -> "24-316"
  urls <- paste0("https://www.supremecourt.gov/oral_arguments/argument_transcripts/",
                 yrs, "/", files)
  keep <- !duplicated(dockets)
  setNames(urls[keep], dockets[keep])
}

# Add transcript_url + audio_url columns to an argument table. Audio is built for
# any case actually argued; the transcript is looked up in the per-Term index.
attach_media <- function(tbl) {
  terms <- sort(unique(tbl$term[!is.na(tbl$term)]))
  tmap <- setNames(lapply(terms, fetch_transcript_map), as.character(terms))
  tbl |>
    mutate(
      transcript_url = map2_chr(dkt, term, function(d, t) {
        m <- if (is.na(t)) NULL else tmap[[as.character(t)]]
        if (!is.null(m) && d %in% names(m)) unname(m[[d]]) else NA_character_
      }),
      audio_url = if_else(!is.na(argued_date),
        paste0("https://www.supremecourt.gov/oral_arguments/audio/", term, "/", dkt),
        NA_character_)
    )
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
build_argument_table <- function(cases, qp_map = NULL) {
  cls <- classify_petitions(cases)
  granted <- cls |> filter(outcome == "granted") |> distinct(dkt, .keep_all = TRUE) |>
    transmute(dkt, grant_date = outcome_date)
  g <- cases |> filter(dkt %in% granted$dkt) |> distinct(dkt, .keep_all = TRUE)
  if (nrow(g) == 0) return(tibble())

  arg <- bind_cols(
    g |> transmute(dkt, caption = str_squish(caption %||% dkt)),
    map_dfr(g$events, classify_argument)
  ) |>
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
  arg
}

# ---- rendering ----------------------------------------------------------------

STATUS_FILL <- c(Granted = "#DCE7F0", Scheduled = "#A6CEE3", Argued = "#B2DF8A",
                 Decided = "#E4DAC2", `DIG'd` = "#FB9A99")

# Render one Term's argument calendar (a gt table grouped by sitting). Cases with
# an argument date are grouped by their sitting (chronological); granted-but-
# unscheduled cases fall into a trailing "Not yet scheduled" group.
argument_term_page <- function(tbl, term, out_dir) {
  d <- tbl |>
    filter(term == !!term) |>
    arrange(is.na(sitting_date), sitting_date, arg_ref, desc(grant_date),
            as.integer(str_extract(dkt, "\\d+$")))
  if (nrow(d) == 0) return(invisible(NULL))
  all_unscheduled <- all(is.na(d$arg_ref))

  has_qp <- any(!is.na(d$qp))
  d <- d |>
    mutate(
      # Argument date for scheduled cases; grant date for the rest.
      when = case_when(
        !is.na(arg_ref) ~ format(arg_ref, "%a %b %d"),
        !is.na(grant_date) ~ paste0("Granted ", format(grant_date, "%b %d")),
        TRUE ~ "—"),
      case = caption,
      # Always link the docket number to the docket page (not the petition PDF).
      docket = str_c("[", dkt,
        "](https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/",
        dkt, ".html)"),
      argued_by = if_else(is.na(advocates), "—", advocates),
      # Decided cases show the majority author and link to the slip opinion where
      # available; other states show the plain status word. `status` is retained
      # (hidden) to drive the fill colour.
      status_disp = case_when(
        status == "Decided" & !is.na(opinion_url) & !is.na(opinion_author) ~
          str_c("[Decided · ", opinion_author, "](", opinion_url, ")"),
        status == "Decided" & !is.na(opinion_url) ~ str_c("[Decided](", opinion_url, ")"),
        status == "Decided" & !is.na(opinion_author) ~ str_c("Decided · ", opinion_author),
        TRUE ~ status
      ),
      # Transcript + audio links for the oral argument (dropped when neither).
      media = pmap_chr(list(transcript_url, audio_url), function(tr, au) {
        parts <- c(if (!is.na(tr)) str_c("[Transcript](", tr, ")"),
                   if (!is.na(au)) str_c("[Audio](", au, ")"))
        if (length(parts) == 0) "—" else paste(parts, collapse = " · ")
      }),
      qp = if_else(is.na(qp), "—", qp),
      # Unscheduled grants have no sitting; collect them in a trailing group.
      sitting = if_else(is.na(sitting), "Not yet scheduled", sitting)
    )
  has_media <- any(d$media != "—")
  has_argued <- any(d$argued_by != "—")     # drop empty columns on all-unscheduled Terms
  d <- d |>
    select(sitting, when, case, docket, status, status_disp,
           any_of(if (has_argued) "argued_by" else character()),
           any_of(if (has_media) "media" else character()),
           any_of(if (has_qp) "qp" else character()))

  labels <- list(when = "", case = "Case", docket = "Docket", status_disp = "Status",
                 argued_by = "Argued by", media = "Argument", qp = "QP")
  labels <- labels[names(labels) %in% names(d)]
  md_cols <- intersect(c("docket", "status_disp", "media", "qp"), names(d))

  tbl_gt <- d |>
    gt(groupname_col = "sitting") |>
    fmt_markdown(columns = all_of(md_cols)) |>
    tab_header(
      title = paste0(term_label(term - 2000L), " — Oral Argument Calendar"),
      subtitle = if (all_unscheduled) paste0(nrow(d), " granted case(s) awaiting an argument date")
                 else paste0(nrow(d), " case(s) argued or scheduled")
    ) |>
    gt_theme_nytimes() |>
    cols_label(.list = labels) |>
    cols_hide(columns = status) |>
    cols_align(align = "center", columns = everything()) |>
    cols_align(align = "left", columns = any_of("qp")) |>
    # Distinctive, centered sitting dividers (an oxblood-on-parchment band).
    tab_style(
      style = list(
        cell_fill(color = "#ece3cf"),
        cell_text(weight = "bold", align = "center", transform = "uppercase",
                  color = "#7a2e2e", size = px(13)),
        cell_borders(sides = c("top", "bottom"), color = "#c9b98f", weight = px(1))
      ),
      locations = cells_row_groups()
    )
  for (s in names(STATUS_FILL)) {
    tbl_gt <- tbl_gt |>
      tab_style(cell_fill(color = STATUS_FILL[[s]]),
                cells_body(columns = status_disp, rows = status == s))
  }
  gtsave_titled(tbl_gt, str_c("arg_", term, ".html"), path = out_dir,
                title = paste0(term_label(term - 2000L), " oral arguments — SCOTUS"))
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
    back = list(href = "../", label = "← All dashboards")
  )
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
