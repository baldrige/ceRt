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
judgment_of <- function(events) {
  if (!is.data.frame(events) || !"Proceedings and Orders" %in% names(events)) return(NA_character_)
  txt <- str_squish(str_remove_all(coalesce(events[["Proceedings and Orders"]], ""), "<[^>]+>"))
  hit <- txt[str_detect(txt, "REVERSED|AFFIRMED|VACATED|DISMISSED|improvidently granted") &
               !str_detect(txt, regex("^Judgment issued", ignore_case = TRUE))]
  if (length(hit)) hit[1] else NA_character_
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
    judgment_of(data.frame(`Proceedings and Orders` = txt, check.names = FALSE))
  }, error = function(e) NA_character_)
  j
}

#' 1 = petitioner won (reversed or vacated, in whole or in part), 0 = affirmed,
#' NA = dismissed or no judgment yet.
petitioner_won <- function(j) {
  case_when(is.na(j) ~ NA_real_,
            str_detect(j, regex("DISMISSED|improvidently", ignore_case = TRUE)) ~ NA_real_,
            str_detect(j, "REVERSED|VACATED") ~ 1,
            str_detect(j, "AFFIRMED") ~ 0,
            TRUE ~ NA_real_)
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
