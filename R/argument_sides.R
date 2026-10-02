# R/argument_sides.R -- which side each advocate argued for, when the transcript
# does not say.
#
# Most argument headers name a role ("ON BEHALF OF THE PETITIONERS"), and
# tx_header_side() reads it. Some name only a party: "ON BEHALF OF THE FEDERAL
# PARTIES", "... OF MICHELLE COCHRAN", "... OF THE STATE PARTIES". The parser
# then fell back to lectern order -- the first to argue is the petitioner,
# everyone after the respondent -- which is wrong twice over:
#
#   * in a consolidated argument the first to argue is the petitioner of SOME
#     docket, not necessarily the one the page is about. 24-1287 (Learning
#     Resources v. Trump) was argued with 25-250 (Trump v. V.O.S. Selections);
#     the Solicitor General went first as 25-250's petitioner, and the 24-1287
#     page counted the government as its petitioner -- sides flipped;
#   * with several parties a side, "everyone after the first is the respondent"
#     put the House of Representatives against California in California v.
#     Texas, and Smith & Nephew against the United States in Arthrex.
#
# So the sides are worked out from the Court's own party lists. The page's
# docket anchors the frame: its title petitioner is the petitioner side, its
# title respondent the respondent side. Each consolidated companion ("Vide")
# then joins the frame through a title party already placed -- the President is
# 25-250's petitioner and 24-1287's respondent, so 25-250's respondent (V.O.S.)
# is on 24-1287's petitioner side. Only title parties: the full party lists
# record formal roles, not alignment (24-1287 lists the States among its
# respondents, beside the President they sued). A header's party is matched by
# name, by initials ("FCC"), or by class (federal, State, tribal, private); what
# that cannot place, argument order does, since each side argues as a block. A
# rebuttal takes its advocate's side. Whatever is still ambiguous is left
# unplaced, and the argument gets no lean.

suppressPackageStartupMessages({ library(stringr); library(dplyr); library(purrr) })
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

# Bump when these rules change: the reader index caches the resolved sides.
SIDES_RULES <- "s1"

.SIDE_STOP <- c("the", "of", "and", "et", "al", "inc", "llc", "ltd", "co", "corp", "corporation",
                "company", "a", "an", "for", "in", "on", "behalf", "no", "nos", "v", "jr", "sr",
                "parties", "party", "petitioner", "petitioners", "respondent", "respondents",
                "applicant", "applicants", "appellant", "appellants", "appellee", "appellees",
                "plaintiff", "plaintiffs", "defendant", "defendants", "intervenor", "intervenors",
                "others", "other", "case", "cases")
.FED_RX <- regex(paste0("\\b(united states|president|secretary|attorney general|commission(er)?|",
                        "department|administrat(or|ion)|director|bureau|agency|federal|solicitor general|",
                        "internal revenue|postal service|commissioner)\\b"), ignore_case = TRUE)
.STATE_NAMES <- c(state.name, "District of Columbia")
.STATE_RX <- regex(paste0("\\b(states?|commonwealth|", paste(.STATE_NAMES, collapse = "|"), ")\\b"), ignore_case = TRUE)
.TRIBE_RX <- regex("\\b(nation|tribes?|tribal|band|pueblo|indians?)\\b", ignore_case = TRUE)

.side_tokens <- function(x) {
  t <- str_split(str_to_lower(str_replace_all(x %||% "", "[^A-Za-z0-9&' ]", " ")), "\\s+")[[1]]
  setdiff(t[nzchar(t) & nchar(t) > 1], .SIDE_STOP)
}
.party_class <- function(name) {
  if (str_detect(name, .FED_RX) && !str_detect(name, regex("house of representatives|senate", ignore_case = TRUE))) "federal"
  else if (str_detect(name, .TRIBE_RX)) "tribal"
  else if (str_detect(name, .STATE_RX)) "state"
  else "private"
}
.initials <- function(name) {
  w <- str_extract_all(name %||% "", "\\b[A-Z][A-Za-z]+")[[1]]
  w <- w[!str_to_lower(w) %in% .SIDE_STOP]
  str_to_lower(paste(substr(w, 1, 1), collapse = ""))
}
.same_party <- function(a, b) {
  ta <- .side_tokens(a); tb <- .side_tokens(b)
  if (!length(ta) || !length(tb)) return(FALSE)
  length(intersect(ta, tb)) / min(length(ta), length(tb)) >= 0.5
}

#' One docket's parties from its JSON (the Court's docket API): title parties
#' and the full lists, as the resolver wants them.
docket_party_record <- function(j) {
  pick <- function(side) vapply(j[[side]] %||% list(), function(p) p$PartyName %||% "", "")
  strip <- function(x) str_squish(str_remove(x %||% "", ",?\\s*(Petitioners?|Respondents?|Applicants?|Appellants?|Appellees?)\\.?$"))
  list(pet_title = strip(j$PetitionerTitle), resp_title = strip(j$RespondentTitle),
       pet = pick("Petitioner"), resp = pick("Respondent"),
       related = unique(unlist(str_extract_all(paste(c(
         vapply(j$RelatedCaseNumber %||% list(), function(r) r$DisplayCaseNumber %||% "", ""),
         vapply(j$ProceedingsandOrder %||% list(), function(e) {
           t <- e$Text %||% ""; if (str_detect(t, regex("consolidat|petitions? for (a )?writs? of certiorari in Nos?\\.", ignore_case = TRUE))) t else "" }, "")),
         collapse = " "), "\\b\\d{2}-\\d{1,5}\\b|\\b\\d{2}A\\d{1,4}\\b"))))
}

#' Every party in the consolidated group, placed on the lead docket's
#' petitioner side (+1) or respondent side (-1). `records` is a named list of
#' docket_party_record()s, the lead first.
party_alignment <- function(records) {
  placed <- tibble(name = character(), side = numeric())
  side_of <- function(nm) {
    hit <- placed$side[vapply(placed$name, .same_party, logical(1), b = nm)]
    if (length(hit) && all(hit == hit[1])) hit[1] else NA_real_
  }
  place <- function(nm, s) if (nzchar(nm) && is.na(side_of(nm))) placed <<- bind_rows(placed, tibble(name = nm, side = s))
  lead <- records[[1]]
  place(lead$pet_title, 1); place(lead$resp_title, -1)
  # Companions join through a title party already placed; a few passes let a
  # chain of companions resolve in any order.
  for (pass in 1:3) for (r in records[-1]) {
    sp <- side_of(r$pet_title); sr <- side_of(r$resp_title)
    if (!is.na(sp) && is.na(sr)) place(r$resp_title, -sp)
    if (!is.na(sr) && is.na(sp)) place(r$pet_title, -sr)
  }
  # Title parties only. The full party lists record formal roles, not who
  # argued against whom: 24-1287 lists the States among its respondents beside
  # the President they sued, and California v. Texas lists the United States as
  # a respondent in both its dockets, on opposite sides of the caption. Read
  # as alignment they put the State parties with the government and the United
  # States with California. What the titles cannot place, argument order does
  # (resolve_argument_sides()).
  placed |> mutate(class = vapply(name, .party_class, ""), init = vapply(name, .initials, ""))
}

#' The side a header's party is on, or NA.
.header_party_side <- function(header, placed) {
  who <- str_squish(str_remove(str_match(header, regex("(?:ON BEHALF OF|FOR)\\s+(.+)$", ignore_case = TRUE))[, 2] %||% "",
                               regex(",?\\s*(AS AMICUS.*|ET AL\\.?)$", ignore_case = TRUE)))
  if (is.na(who) || !nzchar(who)) return(NA_real_)
  agree <- function(s) { s <- s[!is.na(s)]; if (length(s) && all(s == s[1])) s[1] else NA_real_ }
  # By name.
  s <- agree(placed$side[vapply(placed$name, .same_party, logical(1), b = who)])
  if (!is.na(s)) return(s)
  # By initials: "FCC, ET AL." for the Federal Communications Commission.
  tok <- str_to_lower(str_extract(who, "^[A-Z]{2,6}\\b"))
  if (!is.na(tok)) { s <- agree(placed$side[placed$init == tok]); if (!is.na(s)) return(s) }
  # By class: "THE FEDERAL PARTIES", "THE STATE PARTIES", "THE TRIBAL PARTIES".
  cls <- if (str_detect(who, regex("house of representatives|senate", ignore_case = TRUE))) NA_character_
         else if (str_detect(who, regex("\\bfederal\\b|united states|attorney general|secretary", ignore_case = TRUE))) "federal"
         else if (str_detect(who, .TRIBE_RX)) "tribal"
         else if (str_detect(who, regex("\\bstates?\\b", ignore_case = TRUE))) "state"
         else if (str_detect(who, regex("\\bprivate\\b", ignore_case = TRUE))) "private"
         else NA_character_
  if (!is.na(cls)) return(agree(placed$side[placed$class == cls]))
  NA_real_
}

#' The side of every segment of a parsed transcript. Segments whose header
#' names a role keep tx_header_side()'s reading; the rest are placed from the
#' party alignment. Returns a character vector ("pet", "resp", NA) and, as the
#' attribute "complete", whether every party segment was placed.
resolve_argument_sides <- function(segments, records) {
  explicit <- vapply(segments$header, tx_header_side, "")
  need <- is.na(explicit) & !segments$amicus
  sides <- explicit
  if (any(need) && length(records)) {
    placed <- tryCatch(party_alignment(records), error = function(e) NULL)
    if (!is.null(placed) && nrow(placed)) for (k in which(need & !segments$rebuttal)) {
      s <- .header_party_side(segments$header[k], placed)
      if (!is.na(s)) sides[k] <- if (s > 0) "pet" else "resp"
    }
  }
  # Argument order places the rest. The Court hears one side's advocates as a
  # block and then the other's, so along the parties' (non-rebuttal) segments
  # the side changes exactly once. An unplaced advocate after the first of the
  # second block is in it; one before the last of the first block is in that;
  # one after a run of a single side, when the other has not appeared, is the
  # other side (both must argue). One sitting between the last of one block and
  # the first of the other is genuinely ambiguous and stays unplaced.
  ord <- which(!segments$amicus & !segments$rebuttal)
  for (pass in 1:2) {
    known <- ord[!is.na(sides[ord])]
    if (!length(known)) break
    first_side <- sides[known[1]]
    other <- known[sides[known] != first_side]
    for (k in ord[is.na(sides[ord])]) {
      if (length(other)) {
        last_first <- max(known[sides[known] == first_side & known < other[1]])
        if (k > other[1]) sides[k] <- sides[other[1]]
        else if (k < last_first) sides[k] <- first_side
      } else if (k > max(known)) {
        sides[k] <- if (first_side == "pet") "resp" else "pet"
      } else if (k < known[1]) {
        # Before every placed advocate, with one side only in view: the first
        # to argue opens the first block, whichever side that is.
        sides[k] <- NA_character_
      } else sides[k] <- first_side
    }
  }
  # The first to argue, still unplaced (no title party matched and nothing after
  # it placed), is the petitioner: the old rule, now the last resort.
  if (length(ord) && is.na(sides[ord[1]]) && all(is.na(sides[ord]))) sides[ord[1]] <- "pet"
  # A rebuttal is its advocate's own side.
  for (k in which(need & segments$rebuttal)) {
    own <- which(!segments$rebuttal & segments$advocate == segments$advocate[k] & !is.na(sides))
    if (length(own)) sides[k] <- sides[own[1]]
  }
  principal <- !segments$amicus
  attr(sides, "complete") <- all(!is.na(sides[principal])) && any(sides[principal] %in% "pet") && any(sides[principal] %in% "resp")
  sides
}

#' The party records for an argument: its docket and every consolidated
#' companion the docket names, from the Court's JSON. Paced; NULL on failure.
fetch_party_records <- function(dkt, pace = 0.6) {
  get <- function(d) {
    Sys.sleep(pace)
    tryCatch(httr2::request(paste0("https://www.supremecourt.gov/RSS/Cases/JSON/", d, ".json")) |>
               httr2::req_user_agent("Mozilla/5.0 (ceRt SCOTUS research; +https://supremecourt.report)") |>
               httr2::req_timeout(30) |> httr2::req_perform() |> httr2::resp_body_json(),
             error = function(e) NULL)
  }
  j <- get(dkt); if (is.null(j)) return(NULL)
  lead <- docket_party_record(j)
  recs <- list(lead); names(recs) <- dkt
  for (d in setdiff(lead$related, dkt)) {
    if (grepl("A", d)) next          # a stay application is not a companion argument
    jj <- get(d); if (!is.null(jj)) recs[[d]] <- docket_party_record(jj)
  }
  recs
}
