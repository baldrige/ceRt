# applications.R -----------------------------------------------------------------
# The application docket (NNA###): what each application asks for, who it went
# to, and how it ended -- read from the docket's own entries.
#
# MOST APPLICATIONS ARE NOT THE "EMERGENCY DOCKET". Over OT2017-OT2026, 10,205
# application dockets, of which ~8,400 ask for more time to file a petition (or
# for leave to exceed a word limit). Those are granted by a single Justice in
# days, and counting them beside a stay of a nationwide injunction would bury the
# 1,700-odd applications anyone means by the interim or emergency docket. So
# every application carries a `kind`, and `substantive` is what the tracker shows.
#
# THE OUTCOME GRAMMAR. An application is submitted to a Justice (normally the
# Circuit Justice), who either rules alone or refers it to the full Court. A
# denial by one Justice can be refiled with another (Rule 22.4), which reopens
# it. So the outcome is the LAST disposition, and a refiling after it means the
# application is live again.
#
# A disposition is read from the FIRST SENTENCE of an entry beginning
# "Application": "Application (25A123) denied by the Court. Justice X would
# grant the application." is a denial, and the second sentence must not be read
# as a grant. Measured over OT2017-OT2024: see docs/applications.md.

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(stringr)
})

# What an application asks for, in the order tested: the first match wins, so
# "stay of execution" is tested before "stay", and "stay or vacatur" reads as a
# stay because `stay` precedes `vacate`... except that `vacate` must win for "to
# vacate stay", whose object is a stay. Hence vacate is tested on the VERB
# ("to vacate", "for vacatur" as the request), before stay.
APP_KINDS <- c(
  extension  = "to extend the time|extension of time|for an extension|to extend",
  procedural = paste0("in excess of|word limit|page limit|under seal|out of time|for leave to file|^to file|",
                      "to file (a |the )?(petition|brief|reply|jurisdictional)|judgment forthwith|",
                      "issuance of the judgment|reconsideration of clerk|appeal bond"),
  execution  = "execution|sentence of death",
  vacate     = "^(to vacate|vacate|for vacatur|an order vacating)",
  injunction = "injunct|restraining order|interim relief",
  bail       = "\\bbail\\b|\\brelease\\b",
  coa        = "certificate of appealability",
  stay       = "\\bstay|recall|suspend"
)

# Kinds that make up the interim ("emergency") docket. A certificate of
# appealability is a prisoner's request that a Justice almost never grants
# alone; it is substantive but not interim relief, so it is kept out of the
# headline counts and shown under its own kind.
APP_INTERIM_KINDS <- c("execution", "stay", "injunction", "vacate", "bail")

APP_KIND_LABELS <- c(
  extension = "Extension of time", procedural = "Procedural",
  execution = "Stay of execution", vacate = "Vacate a stay or injunction",
  injunction = "Injunction", bail = "Bail or release",
  coa = "Certificate of appealability", stay = "Stay", other = "Other")

# The opening entry: the first that files an application ("Application (25A1)
# for a stay, submitted to Justice Alito." / "Application for stay (25A1)
# presented to ..."). An archive docket can open with a paper-filing notice or a
# "Response requested" entry, so this is a search, not txt[1].
.app_open_rx <- regex("^Applications?\\b.*\\b(submitted|presented) to\\b", ignore_case = TRUE)

# The request itself, from the opening entry: the text between the docket
# number and "submitted to". Both entry shapes put the request in a different
# place, so the docket number is removed first and the remainder read whole.
application_kind <- function(open_txt) {
  if (is.na(open_txt) || !nzchar(open_txt)) return("other")
  req <- open_txt |>
    str_remove(regex("^Applications?( of [^()]+?)?\\s*", ignore_case = TRUE)) |>
    str_remove("\\(\\d+A\\d+[^)]*\\)\\s*") |>
    str_remove(regex(",?\\s*(submitted|presented) to\\b.*$", ignore_case = TRUE)) |>
    str_squish() |> str_to_lower()
  # "for a stay" -> "a stay"; the request verb survives ("to vacate stay").
  req <- str_remove(req, "^for (an? |the )?")
  for (k in names(APP_KINDS)) if (str_detect(req, APP_KINDS[[k]])) return(k)
  "other"
}

# The Justice an application was first submitted to, as written on the docket
# ("Justice Alito", "The Chief Justice"). Normally the Circuit Justice.
.app_justice <- function(txt) {
  j <- str_match(txt, regex("(?:submitted|presented) to (The Chief Justice|Justice [A-Z][a-z]+)",
                            ignore_case = TRUE))[, 2]
  ifelse(is.na(j), NA_character_,
         ifelse(str_detect(j, regex("chief", ignore_case = TRUE)), "The Chief Justice",
                str_to_title(j)))
}

# First sentence of a docket entry. A sentence ends at ". " followed by a
# capital; the abbreviations an application order uses before its first full
# stop ("No.", "v.") are protected so they do not end it early.
.first_sentence <- function(x) {
  x <- str_replace_all(x, "\\b(No|v|U\\. ?S|Inc|Co|Corp|Jr|St)\\.", "\\1\u00a7")
  s <- str_split_fixed(x, "\\.\\s+(?=[A-Z(])", 2)[, 1]
  str_replace_all(s, "\u00a7", ".")
}

# Relief granted in the same breath as a ruling, on a request that is not the
# application: "The application for leave to file the application under seal
# granted. The application (18A655) for a stay ... denied by Justice Kagan."
# docket_page.R's APP_COLLATERAL, less "excess of the word limit", which is a
# request that does get its own docket and is always numbered when ruled on.
.APP_COLLATERAL_RX <- regex("leave to file|under seal|supplemental appendix|redacted cop",
                            ignore_case = TRUE)

# The sentence of an entry that names THIS docket ("Application (26A40) denied
# by Justice Kagan." after "The motion to file the supplemental appendix under
# seal is granted."), or NA. A sentence that numbers its own docket is
# authoritative wherever it sits in the entry -- docket_page.R's tier 1. Briefs,
# motions, responses and letters are skipped: "Response to application (24A884)
# requested" numbers the docket and rules on nothing.
.app_numbered_sentence <- function(txt, dkt) {
  out <- rep(NA_character_, length(txt))
  if (is.na(dkt) || !nzchar(dkt)) return(out)
  skip <- str_detect(txt, regex(
    "^(Brief|Motion|Response|Reply|Amicus|Letter|Supplemental|Record|DISTRIBUTED)",
    ignore_case = TRUE))
  rx <- regex(paste0("\\b", dkt, "\\b"))
  for (i in which(!skip & str_detect(txt, rx))) {
    p <- str_replace_all(txt[i], "\\b(No|v|U\\. ?S|Inc|Co|Corp|Jr|St)\\.", "\\1\u00a7")
    parts <- str_replace_all(str_split(p, "\\.\\s+(?=[A-Z(\\[])")[[1]], "\u00a7", ".")
    # Numbered AND opened by the application, so the docket is the subject:
    # 21A85's "Justice Sotomayor would grant the application to vacate stay in
    # No. 21A85 and dissents from the dismissal ... as improvidently granted"
    # numbers the docket inside a dissent, on an application the Court DENIED.
    hit <- parts[str_detect(parts, rx) & str_detect(parts, regex(
      "^(The |Consideration of the )?applications?\\b", ignore_case = TRUE))]
    if (length(hit)) out[i] <- hit[1]
  }
  out
}

# One disposition, or NA, from an entry. Only entries that begin "Application"
# (or "The application"), or that number this docket in a sentence, are read: a
# "Motion ... granted" on the same docket is not the application's outcome.
.app_disposition <- function(txt, dkt = NA_character_) {
  # The archive keeps the Court's hard line breaks inside an entry ("pending\r\n
  # review"), and `.` in a pattern does not cross them.
  txt <- str_squish(txt)
  out <- rep(NA_character_, length(txt))
  ns <- .app_numbered_sentence(txt, dkt)
  is_app <- !is.na(ns) | str_detect(txt, regex("^(The )?Applications?\\b", ignore_case = TRUE))
  s <- coalesce(ns, .first_sentence(txt))
  # An unnumbered collateral sentence rules on some other request.
  collateral <- is.na(ns) & str_detect(s, .APP_COLLATERAL_RX)
  hit <- function(p) is_app & !collateral & is.na(out) & str_detect(s, regex(p, ignore_case = TRUE))
  # Order matters: "refiled" and "treated as a petition" are tested before the
  # words "granted"/"denied" can appear in them.
  out[hit("refiled and submitted")] <- "refiled"
  # Consolidated with the merits: "Consideration of the application ... is
  # deferred pending oral argument" (Biden v. Nebraska, Trump v. CASA). The
  # application is then decided with the case, often on another docket.
  out[is.na(out) & str_detect(s, regex("^Consideration of the application.*deferred", ignore_case = TRUE))] <- "deferred"
  out[hit("treated as a petition")] <- "converted"
  out[hit("granted in part|granted as to\\b.*\\botherwise denied")] <- "granted in part"
  out[hit("\\bgranted\\b")] <- "granted"
  out[hit("\\bdenied\\b")] <- "denied"
  out[hit("\\bdismissed\\b")] <- "dismissed"
  out[hit("\\bwithdrawn\\b")] <- "withdrawn"
  # Withdrawn by letter ("Letter ... from the Solicitor General withdrawing the
  # application for stay filed."), and the Clerk's closing entry that follows
  # one, or a moot execution stay ("Application closed." / "Application
  # (17A145) completed.").
  out[is.na(out) & str_detect(s, regex("^Letter.*withdraw", ignore_case = TRUE))] <- "withdrawn"
  out[hit("^Applications?( \\(\\d+A\\d+\\))? (closed|completed)\\.?$")] <- "closed"
  # No quorum (23A617: six Justices recused): "the application is considered
  # closed".
  out[is.na(out) & str_detect(txt, regex("application is considered closed", ignore_case = TRUE))] <- "closed"
  # An ORDER whose ruling is not its first sentence: a full-Court opinion on the
  # application (24A831, 20A150 open with the facts), or a Justice lifting their
  # own administrative stay ("... is hereby vacated. The application for stay
  # is, in all respects, denied."). Read only from orders -- never a brief,
  # motion or response, which can quote an application's request -- and only
  # where "the application" OPENS its sentence (or follows "ordered that"), so
  # it is the subject of the verb. Anywhere else it can be an object: 17A745
  # (Rucho v. Common Cause) has "The motion of appellees to construe the
  # application for a stay as a jurisdictional statement ... is denied" on a
  # stay the Court GRANTED -- the trap docket_page.R's old classifier documented.
  # "Justice X would grant the application" is unread for the same reason.
  is_order <- !str_detect(txt, regex(
    "^(Brief|Motion|Response|Reply|Amicus|Letter|Supplemental|Record|DISTRIBUTED|Consideration)",
    ignore_case = TRUE))
  m <- str_match_all(txt, regex(paste0(
    # A sentence opens at the start, after ". ", ": ", "] " (25A319, 25A608
    # follow "[See Detached Opinion ...]"), ".) " (25A326), after "that", or
    # after a leading clause's comma ("..., the application for stay is
    # denied": 24A860, 25A515). Never after a verb, as in Rucho's "construe".
    "(?:^|[.:)\\]]\\s+|,\\s+|\\bthat\\s+)the (?:\\w+ )?applications?\\b[^.;]{0,250}?\\b(?:is|are)\\b,?",
    "(?: hereby| in all respects,?| therefore| accordingly)*\\s+",
    "(granted in part|granted|denied|dismissed)\\b"), ignore_case = TRUE))
  # The last ruling sentence that is not collateral: 18A669's "The applications
  # for leave to file the application for stay ... under seal ... are granted"
  # follows the denial of the stay and must not overturn it.
  v <- vapply(m, function(z) {
    z <- z[!str_detect(z[, 1], .APP_COLLATERAL_RX), , drop = FALSE]
    if (nrow(z)) str_to_lower(z[nrow(z), 2]) else NA_character_
  }, "")
  # A per curiam speaks in the first person: "We deny the application, without
  # prejudice ..." (20A150).
  we <- str_match(txt, regex("\\bWe (grant|deny|dismiss)\\b[^.;]{0,40}?\\bapplications?\\b",
                             ignore_case = TRUE))[, 2]
  we <- c(grant = "granted", deny = "denied", dismiss = "dismissed")[str_to_lower(we)]
  v <- coalesce(v, unname(we))
  fill <- is.na(out) & is_order & !is.na(v)
  out[fill] <- v[fill]
  # Referred with no ruling in the same sentence: the referral itself. A ruling
  # entry also names the referral ("presented to J and by him referred to the
  # Court is denied"), which the tests above have already claimed.
  out[hit("referred to the Court")] <- "referred"
  out
}

APP_FINAL <- c("granted", "granted in part", "denied", "dismissed", "withdrawn", "converted",
               "deferred", "closed")

# Everything the tracker needs from one application docket's events. `dkt`
# enables the numbered-sentence reading (.app_numbered_sentence()); without it
# only an entry's first sentence and the order fallback are read.
application_profile <- function(events, dkt = NA_character_) {
  none <- list(kind = "other", filed = as.Date(NA), submitted_to = NA_character_,
               referred = FALSE, referred_date = as.Date(NA), refiled = 0L,
               refiled_to = NA_character_, admin_stay = as.Date(NA),
               response_due = NA_character_, response_requested = as.Date(NA),
               outcome = "pending", outcome_date = as.Date(NA), decided_by = NA_character_,
               noted_dissent = FALSE, opinion = FALSE, opinion_url = NA_character_)
  if (!is.data.frame(events) || !nrow(events)) return(none)
  txt <- events[["Proceedings and Orders"]]; txt[is.na(txt)] <- ""
  txt <- str_squish(txt)
  ed <- suppressWarnings(lubridate::mdy(events$Date))
  o <- order(ed, seq_along(ed)); txt <- txt[o]; ed <- ed[o]

  open <- which(str_detect(txt, .app_open_rx))[1]
  # A docket whose opening entry names no Justice ("Application (17A275) for a
  # stay of mandate.") still states its request in the first Application entry.
  if (is.na(open)) open <- which(str_detect(txt, regex("^Applications?\\b", ignore_case = TRUE)))[1]
  disp <- .app_disposition(txt, dkt)
  kind <- application_kind(if (!is.na(open)) txt[open] else NA_character_)

  fin <- which(disp %in% APP_FINAL)
  ref <- which(disp %in% "refiled")
  last_fin <- if (length(fin)) max(fin) else NA_integer_
  # Live again if refiled after its last ruling. Entries within one day are not
  # in chronological order: 17A1375 lists "denied by Justice Alito" ABOVE
  # "refiled and submitted to Justice Alito", both Jun 22 2018 -- he cannot
  # have denied it before it reached him. So on a same-day tie, a ruling that
  # names the Justice it was refiled to came after the refiling.
  live <- is.na(last_fin)
  if (!live && length(ref)) {
    r <- max(ref)
    to <- .app_justice(txt[r])
    same_day <- !is.na(ed[r]) && !is.na(ed[last_fin]) && ed[r] == ed[last_fin]
    ruled_by_to <- !is.na(to) && str_detect(txt[last_fin], fixed(str_remove(to, "^The "), ignore_case = TRUE))
    live <- if (same_day) !ruled_by_to else r > last_fin
  }
  # A granted extension outranks a later refusal: "Application (22A209) granted
  # by Justice Thomas extending the time to file until October 12, 2022", then a
  # request to extend further and "Application (22A209) denied." The time that
  # was given stays given, and "granted" is what the case pages have always
  # said for these 67 dockets (docket_page.R's old classifier had the same rule).
  if (!live && identical(disp[last_fin], "denied")) {
    g <- fin[disp[fin] %in% c("granted", "granted in part") &
               str_detect(txt[fin], regex("extend", ignore_case = TRUE))]
    if (length(g)) last_fin <- max(g)
  }

  # Referred: a referral entry, or a ruling that names one, or a ruling "by the
  # Court" -- the full Court rules only on a referred application.
  ref_any <- str_detect(txt, regex("referred to the Court|by the Court", ignore_case = TRUE)) &
    str_detect(txt, regex("^(The )?Applications?\\b", ignore_case = TRUE))
  # Administrative stay: a single Justice's order holding the case below while
  # the application is considered ("UPON CONSIDERATION of the application ... IT
  # IS ORDERED that ... is stayed pending further order"). "stayed", not
  # "stay": a ruling can mention someone else's stay (24A860: "in light of the
  # stay of execution granted by the Texas Court of Criminal Appeals").
  adm <- which(str_detect(txt, regex("upon (further )?consideration of the application|^Order (entered|issued) by", ignore_case = TRUE)) &
                 str_detect(txt, regex("\\b(is|are) (hereby )?stayed\\b", ignore_case = TRUE)))
  rr <- which(str_detect(txt, regex("^Response to application.*requested", ignore_case = TRUE)))

  fin_txt <- if (!is.na(last_fin)) txt[last_fin] else ""
  list(
    kind = kind,
    filed = if (!is.na(open)) ed[open] else suppressWarnings(min(ed, na.rm = TRUE)),
    submitted_to = if (!is.na(open)) .app_justice(txt[open]) else NA_character_,
    referred = any(ref_any),
    referred_date = if (any(ref_any)) ed[which(ref_any)[1]] else as.Date(NA),
    refiled = length(ref),
    refiled_to = if (length(ref)) .app_justice(txt[max(ref)]) else NA_character_,
    admin_stay = if (length(adm)) ed[adm[1]] else as.Date(NA),
    response_requested = if (length(rr)) ed[rr[1]] else as.Date(NA),
    response_due = if (length(rr)) str_match(txt[rr[1]], regex("due (.*?)\\.?$", ignore_case = TRUE))[, 2]
                   else NA_character_,
    outcome = if (live) "pending" else disp[last_fin],
    outcome_date = if (live) as.Date(NA) else ed[last_fin],
    # The full Court rules only on a referred application, and its written
    # rulings need not say "by the Court" (24A831 is a per curiam), so this is
    # read from whether a referral preceded the ruling, not from its wording.
    decided_by = if (live) NA_character_ else
      if (any(ref_any[seq_len(last_fin)]) ||
          str_detect(fin_txt, regex("by the Court", ignore_case = TRUE))) "court" else "justice",
    # A Justice recorded as dissenting, or a written opinion, on the ruling.
    noted_dissent = str_detect(fin_txt, regex("would (grant|deny)|dissent", ignore_case = TRUE)),
    opinion = str_detect(fin_txt, regex("opinion|concurring|statement", ignore_case = TRUE)),
    # The ruling's entry carries its opinion inline when there is one:
    # "(Detached <a href = 'https://www.supremecourt.gov/opinions/25pdf/...'>Opinion</a>)".
    opinion_url = str_match(fin_txt, "href\\s*=\\s*['\"](https://www\\.supremecourt\\.gov/[^'\"]+)")[, 2])
}

# The applicant, from the caption ("Donald J. Trump, President of the United
# States, et al., Applicants v. ..."). Application dockets carry no party roles
# in the archive, so the caption is the only source -- read only up to the
# "Applicant" role, never past the "v.", which names the other side.
application_applicant <- function(caption) {
  a <- str_match(caption, regex("^(.*?),?\\s+Applicants?\\b", ignore_case = TRUE))[, 2]
  ifelse(is.na(a), str_squish(str_remove(caption, "\\s+v\\.\\s.*$")), str_squish(a))
}

# "us" / "state" / "private" for an applicant, by counsel_table.R's
# petitioner_gov_side() with one correction: a State's own "Secretary of State"
# reads as federal there, because FED_PARTY_RX guards only "Attorney General"
# against a State named alongside it. "Wes Allen, Alabama Secretary of State"
# (25A1229-31, 25A1314-16) is the State of Alabama, not the United States.
application_side <- function(applicant) {
  if (!exists("petitioner_gov_side", mode = "function"))
    return(rep(NA_character_, length(applicant)))
  side <- petitioner_gov_side(applicant)
  st <- if (exists(".state_rx")) .state_rx else "$^"
  state_sec <- str_detect(coalesce(applicant, ""),
                          regex(paste0(st, ".{0,40}\\bSecretary of State\\b"), ignore_case = TRUE))
  # Federal agencies FED_PARTY_RX does not name, which a cert petitioner
  # rarely is and an applicant for a stay of a land-use or grant injunction
  # often is (26A203, National Park Service).
  fed_extra <- str_detect(coalesce(applicant, ""), regex(paste0(
    "^(National Park Service|(United States )?Forest Service|(United States )?Army Corps of Engineers|",
    "Bureau of (Land Management|Prisons|Indian Affairs|Reclamation)|",
    "(United States )?Fish and Wildlife Service|National Institutes of Health|",
    "Department of (Justice|State|Defense|Education|Labor|Commerce|Energy|Agriculture|",
    "the Interior|the Treasury|Health and Human Services|Homeland Security|",
    "Housing and Urban Development|Transportation|Veterans Affairs))\\b"), ignore_case = TRUE))
  side <- ifelse(side == "private" & fed_extra, "us", side)
  ifelse(side == "us" & state_sec, "state", side)
}

# One row per application docket. `cases` is any frame with dkt, caption,
# events (and optionally lower); non-application rows are dropped.
#
# `full` names the kinds whose whole docket is read. The kind itself is cheap
# (one entry); the full read is not, and ~83% of applications are extensions of
# time that the tracker only counts. Reading all 10,205 takes ~50 s, the interim
# kinds alone ~10. Rows outside `full` carry their kind and filing date only.
classify_applications <- function(cases, full = c(APP_INTERIM_KINDS, "coa", "other")) {
  a <- cases |> filter(str_detect(dkt, "^\\d+A\\d+$"))
  if (!nrow(a)) return(tibble())
  open_of <- function(e) {
    if (!is.data.frame(e) || !nrow(e)) return(NA_character_)
    t <- str_squish(coalesce(e[["Proceedings and Orders"]], ""))
    i <- which(str_detect(t, .app_open_rx))[1]
    if (is.na(i)) i <- which(str_detect(t, regex("^Applications?\\b", ignore_case = TRUE)))[1]
    if (is.na(i)) NA_character_ else t[i]
  }
  kinds <- map_chr(a$events, ~ application_kind(open_of(.x)))
  cl <- pmap(list(a$events, kinds, a$dkt), function(e, k, d) {
    if (k %in% full) return(application_profile(e, d))
    list(kind = k, filed = suppressWarnings(min(lubridate::mdy(e$Date), na.rm = TRUE)),
         submitted_to = NA_character_, referred = NA, referred_date = as.Date(NA),
         refiled = NA_integer_, refiled_to = NA_character_, admin_stay = as.Date(NA),
         response_requested = as.Date(NA), response_due = NA_character_,
         outcome = NA_character_, outcome_date = as.Date(NA), decided_by = NA_character_,
         noted_dissent = NA, opinion = NA, opinion_url = NA_character_)
  })
  applicant <- application_applicant(a$caption)
  tibble(
    dkt = a$dkt, caption = a$caption,
    lower = if ("lower" %in% names(a)) a$lower else NA_character_,
    events = a$events,
    applicant = applicant,
    side = application_side(applicant),
    kind = map_chr(cl, "kind"),
    filed = do.call(c, map(cl, "filed")),
    submitted_to = map_chr(cl, "submitted_to"),
    referred = map_lgl(cl, "referred"),
    referred_date = do.call(c, map(cl, "referred_date")),
    refiled = map_int(cl, "refiled"),
    refiled_to = map_chr(cl, "refiled_to"),
    admin_stay = do.call(c, map(cl, "admin_stay")),
    response_requested = do.call(c, map(cl, "response_requested")),
    response_due = map_chr(cl, "response_due"),
    outcome = map_chr(cl, "outcome"),
    outcome_date = do.call(c, map(cl, "outcome_date")),
    decided_by = map_chr(cl, "decided_by"),
    noted_dissent = map_lgl(cl, "noted_dissent"),
    opinion = map_lgl(cl, "opinion"),
    opinion_url = map_chr(cl, "opinion_url")) |>
    mutate(interim = kind %in% APP_INTERIM_KINDS,
           term = 2000L + as.integer(str_extract(dkt, "^\\d+")))
}

# The applications in the committed archives (data-raw/ot_*.rds) plus `live`,
# one row per docket, the live copy winning: an archive is frozen when it is
# written (ot_2024.rds ends 2025-06-18), and a live fetch of the same docket
# has its later rulings.
application_history <- function(live = NULL, archive_glob = "data-raw/ot_*.rds") {
  paths <- Sys.glob(archive_glob)
  arch <- map(paths, function(p) tryCatch({
    x <- readRDS(p)
    x[str_detect(x$dkt, "^\\d+A\\d+$"), intersect(c("dkt", "caption", "lower", "events"), names(x))]
  }, error = function(e) { message("application_history(): skipped ", p, ": ", conditionMessage(e)); NULL }))
  arch <- bind_rows(compact(arch))
  if (!is.null(live) && nrow(live)) arch <- filter(arch, !dkt %in% live$dkt)
  # `live` marks what was read tonight. Only a live row can be shown as
  # pending: an archived docket that reads "pending" was last read before its
  # ruling (17A1375 is a 2018 stay "pending" for eight years).
  bind_rows(if (!is.null(live) && nrow(live)) mutate(live, live = TRUE),
            if (nrow(arch)) mutate(classify_applications(arch), live = FALSE))
}

# ---- the page -------------------------------------------------------------------

APP_STATUS <- c(
  "Before the Court", "Response requested", "With the Justice",
  "Granted", "Granted in part", "Denied", "Dismissed", "Withdrawn",
  "Deferred to argument", "Treated as a petition", "Closed")

application_status <- function(outcome, referred, response_requested) {
  case_when(
    outcome == "pending" & referred %in% TRUE ~ "Before the Court",
    outcome == "pending" & !is.na(response_requested) ~ "Response requested",
    outcome == "pending" ~ "With the Justice",
    outcome == "granted" ~ "Granted",
    outcome == "granted in part" ~ "Granted in part",
    outcome == "denied" ~ "Denied",
    outcome == "dismissed" ~ "Dismissed",
    outcome == "withdrawn" ~ "Withdrawn",
    outcome == "deferred" ~ "Deferred to argument",
    outcome == "converted" ~ "Treated as a petition",
    TRUE ~ "Closed")
}

APP_SIDE_LABELS <- c(us = "United States", state = "A State", private = "Private party")

# One row per Term (the docket's two digits: 25A### is OT2025). A Term whose
# application count is under half the median is flagged rather than shown: the
# OT2020 archive holds 178 applications against ~1,200 in its neighbours, almost
# all of the gap extensions of time. Its interim applications are complete
# (~150, in line with every other Term), so only the all-applications count is
# withheld.
application_term_table <- function(h) {
  tot <- count(h, term, name = "all")
  # Closed Terms only: the current Term's count is partial by definition.
  closed <- tot$term < max(tot$term)
  thin <- tot$term[closed & tot$all < 0.5 * stats::median(tot$all[closed])]
  h |>
    filter(interim) |>
    group_by(term) |>
    summarise(
      interim = n(),
      decided = sum(outcome %in% APP_FINAL & !outcome %in% c("withdrawn", "closed")),
      granted = sum(outcome %in% c("granted", "granted in part")),
      capital = sum(kind == "execution"),
      capital_granted = sum(kind == "execution" & outcome %in% c("granted", "granted in part")),
      us = sum(side %in% "us"),
      us_granted = sum(side %in% "us" & outcome %in% c("granted", "granted in part")),
      referred = sum(referred %in% TRUE),
      undecided = sum(outcome %in% "pending"),
      median_days = {
        dd <- as.integer(outcome_date - filed)[outcome %in% APP_FINAL & !is.na(outcome_date)]
        if (length(dd)) as.integer(stats::median(dd)) else NA_integer_
      },
      .groups = "drop") |>
    left_join(tot, by = "term") |>
    mutate(all = ifelse(term %in% thin, NA_integer_, all)) |>
    arrange(desc(term))
}

# Render /applications/index.html. `h` is application_history(): every
# application across the archive and the live fetch. The table shows interim
# applications that are pending or were ruled on in the last year; the by-Term
# table beneath it, every Term held.
application_tracker <- function(h, out_dir, as_of = Sys.Date()) {
  as_of <- as.Date(as_of)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (!"live" %in% names(h)) h$live <- TRUE
  d <- h |>
    filter(interim, (live & outcome %in% "pending") |
             (!is.na(outcome_date) & outcome_date > as_of - 365)) |>
    mutate(status = factor(application_status(outcome, referred, response_requested),
                           levels = APP_STATUS),
           days = as.integer(coalesce(outcome_date, as_of) - filed)) |>
    arrange(status, desc(coalesce(outcome_date, as_of)), desc(filed))
  if (!nrow(d)) {
    message("application_tracker(): no interim applications in the window -- page not written.")
    return(invisible(NULL))
  }

  fmt_d <- function(x) ifelse(is.na(x), "—", str_squish(format(x, "%b %e, %Y")))
  pct <- function(n, k) ifelse(k > 0, sprintf("%.0f%%", 100 * n / k), "—")
  justice_cell <- ifelse(is.na(d$submitted_to), "—",
    paste0(d$submitted_to,
           ifelse(d$refiled %in% 0L | is.na(d$refiled_to), "",
                  paste0("<span class='fc-sub'>refiled to ", d$refiled_to, "</span>"))))
  signal_cell <- map_chr(seq_len(nrow(d)), function(i) {
    s <- character()
    if (!is.na(d$admin_stay[i])) s <- c(s, paste0("Administrative stay ", fmt_d(d$admin_stay[i])))
    if (d$outcome[i] == "pending" && !is.na(d$response_due[i]))
      s <- c(s, paste0("Response due ", str_remove(d$response_due[i], "^by ")))
    if (isTRUE(d$referred[i]) && d$outcome[i] != "pending" && !is.na(d$referred_date[i]))
      s <- c(s, paste0("Referred ", fmt_d(d$referred_date[i])))
    if (isTRUE(d$noted_dissent[i])) s <- c(s, "Noted dissent")
    if (isTRUE(d$opinion[i])) s <- c(s, if (!is.na(d$opinion_url[i]))
      paste0("<a href='", d$opinion_url[i], "' target='_blank'>Written opinion</a>") else "Written opinion")
    if (!length(s)) "—" else paste(s, collapse = "<br>")
  })
  # Days pending for a live application, days to ruling for a decided one --
  # the number is the column's data, so it sorts; the label says which.
  days_cell <- ifelse(d$outcome == "pending",
    paste0("<b>", d$days, "</b><span class='fc-sub'>days pending</span>"),
    paste0("<b>", d$days, "</b><span class='fc-sub'>to ", fmt_d(d$outcome_date), "</span>"))

  tbl <- tibble(
    Case = paste0("[", str_squish(strip_caption_roles(d$caption)), "]",
                  "(/cases/", d$dkt, ".html)",
                  "<br><span class='cdk'>No. ", d$dkt, "</span>"),
    Status = as.character(d$status),
    Request = unname(APP_KIND_LABELS[d$kind]),
    Applicant = unname(coalesce(APP_SIDE_LABELS[d$side], "—")),
    Justice = justice_cell,
    Filed = fmt_d(d$filed),
    Days = d$days,
    Signals = signal_cell,
    Court = ifelse(is.na(d$lower) | d$lower == "", "—", d$lower),
    # A row carried over from the nightly's snapshot has no events, only the
    # cell rendered from them then (app_snapshot_write()).
    Documents = map_chr(seq_len(nrow(d)), function(i) {
      e <- if ("events" %in% names(d)) d$events[[i]] else NULL
      if (is.data.frame(e)) case_documents(e, c("Application", "Reply"))
      else coalesce(if ("documents" %in% names(d)) d$documents[i] else NA_character_, "—")
    }))
  if (all(tbl$Signals == "—")) tbl <- select(tbl, -Signals)
  if (all(tbl$Documents == "—")) tbl <- select(tbl, -Documents)
  left_cols <- match(intersect(c("Case", "Justice", "Signals", "Court", "Documents"), names(tbl)),
                     names(tbl))

  fills <- c("Before the Court" = STATUS_FILL[["Argued"]],
             "Response requested" = STATUS_FILL[["Scheduled"]],
             "With the Justice" = STATUS_FILL[["Scheduled"]],
             "Granted" = STATUS_FILL[["Granted"]], "Granted in part" = STATUS_FILL[["Granted"]],
             "Denied" = STATUS_FILL[["Dismissed"]])
  fills <- c(fills, setNames(rep(STATUS_FILL[["Decided"]], length(setdiff(APP_STATUS, names(fills)))),
                             setdiff(APP_STATUS, names(fills))))
  t <- tbl |>
    gt() |>
    fmt_markdown(columns = any_of(c("Case", "Justice", "Signals", "Documents"))) |>
    fmt(columns = Days, fns = function(x) days_cell) |>
    data_color(columns = Status, method = "factor",
               palette = unname(fills[intersect(APP_STATUS, unique(tbl$Status))]),
               levels = intersect(APP_STATUS, unique(tbl$Status))) |>
    cols_label(Justice = "Submitted to") |>
    cols_align("center", columns = everything()) |>
    cols_width(Case ~ px(240), Status ~ px(116), Request ~ px(120), Applicant ~ px(104),
               Justice ~ px(120), Filed ~ px(104), Days ~ px(104), Court ~ px(150))
  if ("Signals" %in% names(tbl)) t <- t |> cols_width(Signals ~ px(160))

  # ---- the dek: what is live, then the year in one sentence ----
  live <- filter(d, outcome == "pending")
  yr <- filter(d, outcome != "pending", !outcome %in% c("withdrawn", "closed"))
  g <- sum(yr$outcome %in% c("granted", "granted in part"))
  us <- filter(yr, side %in% "us")
  us_g <- sum(us$outcome %in% c("granted", "granted in part"))
  lead <- if (nrow(live)) paste0(
    nrow(live), if (nrow(live) == 1) " application" else " applications",
    " for interim relief pending",
    { nb <- sum(live$status == "Before the Court")
      if (nb) paste0(", ", nb, " of them before the full Court") else "" })
    else "No application for interim relief is pending"
  dek <- paste0(
    lead, ". In the last twelve months the Court and its Justices ruled on ",
    nrow(yr), " and granted ", g, " (", pct(g, nrow(yr)), ")",
    if (nrow(us)) paste0("; the federal government&rsquo;s were granted ", us_g, " of ",
                         nrow(us), " times") else "",
    ". Sortable and filterable.")

  # ---- by Term ----
  tt <- application_term_table(h)
  num <- function(x) ifelse(is.na(x), "—", format(x, big.mark = ","))
  term_rows <- paste0(
    "<tr><th scope='row'>OT", tt$term, "</th>",
    "<td>", num(tt$all), "</td>",
    "<td>", num(tt$interim), "</td>",
    "<td>", num(tt$granted), " <span class='apt-r'>", pct(tt$granted, tt$decided), "</span></td>",
    "<td>", num(tt$capital), " <span class='apt-r'>", num(tt$capital_granted), " granted</span></td>",
    "<td>", num(tt$us), " <span class='apt-r'>", ifelse(tt$us > 0, paste0(num(tt$us_granted), " granted"), ""), "</span></td>",
    "<td>", pct(tt$referred, tt$interim), "</td>",
    "<td>", ifelse(is.na(tt$median_days), "—", paste0(tt$median_days, " d")), "</td>",
    "<td>", num(tt$undecided), "</td></tr>", collapse = "")
  extra <- paste0(
    "<style>.apt{border-collapse:collapse;margin:1.5rem 0 .5rem;font-size:.85rem;",
    "font-variant-numeric:tabular-nums;width:100%}.apt th,.apt td{padding:.3rem .5rem;",
    "border-bottom:1px solid var(--rule);text-align:right;white-space:nowrap}",
    ".apt thead th{font-weight:600;vertical-align:bottom;white-space:normal}",
    ".apt th[scope=row]{text-align:left}.apt-r{color:var(--faint)}",
    ".apt-wrap{overflow-x:auto}</style>",
    "<h2>By Term</h2><div class='apt-wrap'><table class='apt'><thead><tr>",
    "<th scope='col'>Term</th><th scope='col'>All applications</th>",
    "<th scope='col'>Interim relief</th><th scope='col'>Granted</th>",
    "<th scope='col'>Stays of execution</th><th scope='col'>By the United States</th>",
    "<th scope='col'>Referred to the Court</th><th scope='col'>Median days to ruling</th>",
    "<th scope='col'>Undecided</th></tr></thead><tbody>", term_rows, "</tbody></table></div>")

  footer <- paste0(
    "An <em>application</em> asks a single Justice &mdash; normally the Circuit Justice ",
    "for the court below &mdash; for relief while a case proceeds: a stay, an injunction, ",
    "bail, or an order vacating one entered below. The Justice may rule alone or refer ",
    "it to the full Court; a denial by one Justice can be refiled with another. ",
    "Most applications are not this: about five in six ask only for more time to file, ",
    "and are counted under <em>All applications</em> but not shown. ",
    "<em>Status</em>: <em>With the Justice</em>, filed and not yet acted on; ",
    "<em>Response requested</em>, the Justice has called for the other side&rsquo;s ",
    "response; <em>Before the Court</em>, referred to all nine. ",
    "<em>Administrative stay</em> is a Justice&rsquo;s temporary order holding the ",
    "ruling below while the application is considered &mdash; not a ruling on it. ",
    "<em>Granted</em> counts grants in part. The rate is of applications ruled on, ",
    "leaving out those withdrawn. <em>Applicant</em> is read from the caption: ",
    "<em>United States</em> is a federal party or officer, not the President sued in a ",
    "personal capacity. <em>Undecided</em> in a closed Term means the docket was last ",
    "read before its ruling. The Term is the docket number&rsquo;s (25A### is OT2025). ",
    "Read from the Court&rsquo;s docket entries; an unrecognised ruling reads as ",
    "pending &mdash; under 1% of the interim applications of OT2017&ndash;OT2023.")

  scr_interactive(t, n_rows = nrow(tbl)) |>
    scr_write_page(
      file.path(out_dir, "index.html"),
      kicker = "Supreme Court of the United States",
      title = "Emergency Applications",
      dek = dek, n_rows = nrow(tbl), left_cols = left_cols, footer = footer,
      extra = extra, leaf_max = 92, active = "/applications/",
      back = list(href = "/dashboards/", label = "&larr; Daily petitions &amp; applications"))
  invisible(file.path(out_dir, "index.html"))
}

# ---- the daily's refresh --------------------------------------------------------
#
# The page is built from the nightly's full-Term fetch, the only place every
# NNA### docket is loaded. The daily sees its trailing ~50 dockets, the watch
# list and the dockets in recent orders -- not an application sitting with one
# Justice for a month. So the nightly leaves a snapshot of its classified live
# applications on the site, and each daily run re-reads, by name, the ones still
# undecided, overlays whatever applications it already fetched, and rebuilds the
# page. A ruling reaches the page within hours, not at the next 06:00 UTC run.

APP_SNAPSHOT <- "applications/applications.json"
.APP_SNAP_DATES <- c("filed", "referred_date", "admin_stay", "response_requested", "outcome_date")

# Keyed by docket so publish_site.sh can union two runs' copies. An interim row
# keeps every column the page reads, with its Documents cell rendered now (the
# events are not kept); any other row only what the by-Term count needs.
app_snapshot_write <- function(site_dir, live) {
  if (is.null(live) || !nrow(live)) return(invisible(0L))
  docs <- map_chr(seq_len(nrow(live)), function(i) {
    e <- if ("events" %in% names(live)) live$events[[i]] else NULL
    if (isTRUE(live$interim[i]) && is.data.frame(e)) case_documents(e, c("Application", "Reply"))
    else if ("documents" %in% names(live)) live$documents[i] %||% NA_character_ else NA_character_
  })
  rows <- live |> select(-any_of(c("events", "live"))) |> mutate(documents = docs) |>
    mutate(across(any_of(.APP_SNAP_DATES), ~ ifelse(is.na(.x), NA_character_, format(.x))))
  out <- lapply(seq_len(nrow(rows)), function(i) {
    r <- as.list(rows[i, ])
    if (!isTRUE(r$interim)) r <- r[c("dkt", "kind", "term", "interim")]
    r[!vapply(r, function(x) length(x) == 1 && is.na(x), logical(1))]
  })
  names(out) <- rows$dkt
  p <- file.path(site_dir, APP_SNAPSHOT)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(out, p, auto_unbox = TRUE, digits = NA)
  invisible(length(out))
}

app_snapshot_read <- function(site_dir) {
  p <- file.path(site_dir, APP_SNAPSHOT)
  if (!file.exists(p)) return(NULL)
  j <- tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) NULL)
  if (!length(j)) return(NULL)
  d <- bind_rows(lapply(j, function(r) as_tibble(lapply(r, function(x) x %||% NA))))
  for (c in setdiff(c(.APP_SNAP_DATES, "submitted_to", "refiled_to", "response_due", "outcome", "decided_by",
                      "opinion_url", "caption", "lower", "applicant", "side", "documents",
                      "referred", "noted_dissent", "opinion", "refiled"), names(d)))
    d[[c]] <- NA
  d |> mutate(across(all_of(.APP_SNAP_DATES), ~ as.Date(as.character(.x))),
              across(c(referred, noted_dissent, opinion, interim), as.logical),
              refiled = as.integer(refiled), term = as.integer(term),
              across(c(submitted_to, refiled_to, response_due, outcome, decided_by, opinion_url,
                       caption, lower, applicant, side, documents, kind), as.character))
}

# The daily's rebuild. `fetched` is every docket the run already holds (its
# trailing window, watch list and order dockets); `fetch` reads more by name.
# Returns NULL, leaving the nightly's page alone, when there is no snapshot yet.
applications_daily <- function(site_dir, fetched, fetch = fetch_cases) {
  snap <- app_snapshot_read(site_dir)
  if (is.null(snap)) {
    message("applications_daily(): no ", APP_SNAPSHOT, " yet -- the nightly writes it; page left alone.")
    return(invisible(NULL))
  }
  have <- if (!is.null(fetched) && nrow(fetched))
    fetched |> filter(str_detect(dkt, "^\\d+A\\d+$")) |>
      select(any_of(c("dkt", "caption", "lower", "events"))) |> distinct(dkt, .keep_all = TRUE)
    else tibble(dkt = character())
  need <- setdiff(snap$dkt[snap$interim %in% TRUE & snap$outcome %in% c("pending", "deferred")],
                  have$dkt)
  got <- if (length(need)) tryCatch(fetch(need), error = function(e) {
    message("applications_daily(): fetch failed: ", conditionMessage(e)); NULL }) else NULL
  if (!is.null(got) && nrow(got)) have <- bind_rows(have, select(got, any_of(c("dkt", "caption", "lower", "events"))))
  fresh <- if (nrow(have)) classify_applications(have) else tibble()
  live <- bind_rows(fresh, if (nrow(fresh)) filter(snap, !dkt %in% fresh$dkt) else snap)
  app_snapshot_write(site_dir, live)
  h <- application_history(live)
  application_tracker(h, file.path(site_dir, "applications"))
  invisible(list(refetched = length(need), got = if (is.null(got)) 0L else nrow(got), cases = got,
                 fresh = nrow(fresh), pending = sum(h$live & h$interim & h$outcome %in% "pending")))
}
