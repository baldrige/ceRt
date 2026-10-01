# R/argument_lean.R -- the post-argument lean: who the bench's questioning favours.
#
# Two logistic models, fitted by .github/scripts/train_argument_lean.R on every
# decided argument since OT2017 and stored as plain coefficients in
# data/argument_lean.json (small, diffable, no model object to deserialise):
#
#   case     P(petitioner wins) ~ log(resp/pet Justice turns) + log(resp/pet words)
#   justice  P(Justice votes for petitioner) ~ Justice + log(resp/pet of their
#            own words) + the bench's word ratio; `justice0` is the same with
#            the Justice intercept alone -- their usual rate -- for comparison.
#
# Words are the signal; turn counts add nothing once words are in (see
# docs/argument-transcripts.md). Only the parties' segments count: amicus time
# is questioned differently, and including it made the forecast worse.
#
# A Justice the model has not seen (a new appointment) takes the baseline
# intercept: no offset, rather than an error.

suppressPackageStartupMessages({ library(stringr); library(dplyr); library(tibble); library(jsonlite) })
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

ARGUMENT_LEAN_FILE <- "data/argument_lean.json"

load_argument_lean <- function(path = ARGUMENT_LEAN_FILE) {
  if (!file.exists(path)) { message("load_argument_lean(): ", path, " missing -- no leans"); return(NULL) }
  fromJSON(path, simplifyVector = FALSE)
}

#' The judgment line on a docket: the first entry that reverses, affirms,
#' vacates or dismisses. "Judgment issued." (the mandate) is not a judgment.
#
# Only an entry that IS a judgment, in either case. Two ways the first version
# went wrong, both found on the Term chart:
#   * it took any entry mentioning the words, so 25-1083's "Motion to dismiss
#     the writ of certiorari as improvidently granted filed by respondents" read
#     as a DIG, months before the Court reversed;
#   * it looked for capitals, so "Judgment is affirmed and case remanded"
#     (23-1187, 23-365) was no judgment at all.
# An argued application ends in "Application(s) ... granted / denied by the
# Court" (24A884, 25A312), which is its judgment.
#
# Matched at the start of any SENTENCE, not only of the entry: 22-506's reads
# "Application (22A444) DENIED AS MOOT. Judgment REVERSED and case REMANDED."
JUDGMENT_ENTRY_RX <- regex(paste0(
  "(^|[.;]\\s+)((The )?Judgments?\\b(?!\\s+issued)|Adjudged|Affirmed|Reversed|Vacated",
  "|Writs? of certiorari[^.]{0,40}DISMISSED",
  "|(The )?(petition|writ|appeal|case)s?\\b[^.]{0,80}\\bdismissed)"),
  ignore_case = TRUE)
# An argued application's disposition. Only on an application's own docket: a
# petition docket can carry an earlier stay ruling in these words (19-715's
# "Application (19A545) granted by the Court"), which is not its judgment.
#   Two forms: "Application (25A312) denied by the Court." and, in the OT2021
#   vaccine-mandate cases, "The applications for stay presented to JUSTICE
#   KAVANAUGH ... and by them referred to the Court are granted."
APPLICATION_JUDGMENT_RX <- regex(paste0(
  "^(The )?Applications?\\b[^.]{0,240}\\b(",
  "(granted|denied)\\b[^.]{0,20}\\bby the Court",
  "|referred to the Court (is|are) (granted|denied))"), ignore_case = TRUE)
# Bump when the rules above change: the reader index caches each argument's
# judgment, and a cached line read under older rules is read again.
JUDGMENT_RULES <- "j2"

judgment_of <- function(events, application = FALSE) {
  if (!is.data.frame(events) || !"Proceedings and Orders" %in% names(events)) return(NA_character_)
  txt <- str_squish(str_remove_all(coalesce(events[["Proceedings and Orders"]], ""), "<[^>]+>"))
  if (isTRUE(application)) {
    app <- txt[str_detect(txt, APPLICATION_JUDGMENT_RX)]
    if (length(app)) return(app[length(app)])   # the last: the one after argument
  }
  hit <- txt[str_detect(txt, JUDGMENT_ENTRY_RX)]
  if (length(hit)) hit[1] else NA_character_
}

is_application_docket <- function(dkt) str_detect(dkt %||% "", "^\\d{2}A\\d+$")

#' "pet" (the petitioner -- or applicant -- prevailed), "resp", "dismissed"
#' (disposed of with no winner: a DIG, a dismissal as moot), or NA (no judgment).
argument_disposition <- function(j) {
  case_when(is.na(j) ~ NA_character_,
            # Only the full "... granted/denied by the Court" form: 22-506's
            # judgment entry opens "Application (22A444) DENIED AS MOOT." and is
            # a merits reversal.
            str_detect(j, APPLICATION_JUDGMENT_RX) &
              str_detect(j, regex("\\bgranted\\b", ignore_case = TRUE)) ~ "pet",
            str_detect(j, APPLICATION_JUDGMENT_RX) ~ "resp",
            str_detect(j, regex("dismissed|improvidently", ignore_case = TRUE)) &
              !str_detect(j, regex("judgments?[^.]{0,40}(revers|vacat|affirm)", ignore_case = TRUE)) ~ "dismissed",
            str_detect(j, regex("revers|vacat", ignore_case = TRUE)) ~ "pet",
            str_detect(j, regex("affirm", ignore_case = TRUE)) ~ "resp",
            TRUE ~ NA_character_)
}

#' The judgment of one docket, read straight from its JSON -- for an argument
#' whose docket the run did not fetch (an OT2017 case docketed in 2015, a
#' decision since the last range fetch). One paced request; NA on any failure.
#' The reader index caches what this finds, so a decided case is asked once.
fetch_judgment <- function(dkt, pace = 0.6) {
  Sys.sleep(pace)
  j <- tryCatch({
    resp <- httr2::request(paste0("https://www.supremecourt.gov/RSS/Cases/JSON/", dkt, ".json")) |>
      httr2::req_user_agent("Mozilla/5.0 (ceRt SCOTUS research; +https://supremecourt.report)") |>
      httr2::req_timeout(30) |> httr2::req_perform()
    po <- httr2::resp_body_json(resp)$ProceedingsandOrder
    txt <- vapply(po, function(e) e$Text %||% "", "")
    judgment_of(data.frame(`Proceedings and Orders` = txt, check.names = FALSE),
                application = is_application_docket(dkt))
  }, error = function(e) NA_character_)
  j
}

#' 1 = petitioner won (reversed or vacated, in whole or in part), 0 = affirmed,
#' NA = dismissed or no judgment yet.
petitioner_won <- function(j) {
  d <- argument_disposition(j)
  case_when(d %in% "pet" ~ 1, d %in% "resp" ~ 0, TRUE ~ NA_real_)
}

#' The case-level inputs from a parsed transcript: Justice turns and words to
#' each side (party segments only) and their log ratios. NULL when a side drew
#' nothing -- a transcript the parser could not split.
argument_sides <- function(p) {
  b <- tx_bench(p, parties_only = TRUE)
  if (is.null(b) || !nrow(b)) return(NULL)
  g <- function(col, sd) sum(b[[col]][b$side == sd])
  s <- list(q_pet = g("turns", "pet"), q_resp = g("turns", "resp"),
            w_pet = g("words", "pet"), w_resp = g("words", "resp"))
  if (s$q_pet == 0 || s$q_resp == 0) return(NULL)
  s$lq <- log((s$q_resp + 1) / (s$q_pet + 1))
  s$lw <- log((s$w_resp + 1) / (s$w_pet + 1))
  s
}

case_lean <- function(model, sides) {
  if (is.null(model) || is.null(sides)) return(NA_real_)
  m <- model$case
  plogis(m$intercept + m$lq * sides$lq + m$lw * sides$lw)
}

#' One row per Justice who sat: their words and turns to each side and their
#' lean (`p`) beside their usual petitioner rate (`p0`).
justice_leans <- function(model, p, sides) {
  b <- tx_bench(p, parties_only = TRUE)
  if (is.null(model) || is.null(b) || is.null(sides)) return(NULL)
  who <- unique(p$turns$speaker[p$turns$role == "justice"])
  rows <- lapply(who, function(j) {
    g <- function(col, sd) sum(b[[col]][b$speaker == j & b$side == sd])
    tibble(name = j, words_pet = g("words", "pet"), words_resp = g("words", "resp"),
           turns_pet = g("turns", "pet"), turns_resp = g("turns", "resp"))
  })
  out <- bind_rows(rows)
  m <- model$justice; m0 <- model$justice0
  off  <- function(o, j) as.numeric(o[[j]] %||% 0)
  out |> mutate(
    silent = turns_pet + turns_resp == 0,
    lw_own = if_else(silent, 0, log((words_resp + 1) / (words_pet + 1))),
    p  = plogis(m$intercept + vapply(name, function(j) off(m$offset, j), numeric(1)) +
                  m$lw_own * lw_own + m$lw_case * sides$lw),
    p0 = plogis(m0$intercept + vapply(name, function(j) off(m0$offset, j), numeric(1))))
}
