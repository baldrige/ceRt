# justices.R ------------------------------------------------------------------
# The Justices: per-Term statistics on who wrote what and who joined whom.
# Spec, measurements and the counting rules: docs/justices.md.
#
# TWO SOURCES, ONE PAGE PER TERM.
#
#   * The Court's Granted & Noted List (R/granted_noted.R, arguments/
#     granted_noted.json), which the weekly already parses: for every argued
#     case since OT16 the author, each Justice who wrote separately and the
#     kind (D, C, C/J, C/P ...), the result, the unanimity flags and who took no
#     part. Panel 1 (opinions written) and panel 4 (the writings list) come from
#     it and nothing else, so they are complete for every Term the list covers.
#
#   * The syllabus lineup paragraph of each slip opinion -- "ALITO, J.,
#     delivered the opinion of the Court, in which ROBERTS, C. J., and THOMAS,
#     GORSUCH, KAVANAUGH, and BARRETT, JJ., joined. BARRETT, J., filed a
#     concurring opinion, in which THOMAS and GORSUCH, JJ., joined as to Part
#     II-B. KAGAN, J., filed a dissenting opinion." -- which is the only
#     structured statement of who JOINED what. Panels 2 and 3 (agreement, vote
#     splits, lineups) need it. Fetched one PDF per decision through the same
#     capped, paced, throttle-aware loop as the word counts (R/word_count.R),
#     cached in justices/lineups.json, and never fatal: a decision without a
#     parsed lineup is counted in panels 1 and 4 and left out of 2 and 3, and
#     the page says how many that is.
#
# The opinion PDF for a docket comes from the Court's own slip-opinion feed and
# listing pages (fetch_opinion_listing(), R/site_decisions.R): one request per
# Term, and it names the PDF for every decision including the per curiams.

suppressPackageStartupMessages({
  library(stringr); library(jsonlite); library(dplyr); library(purrr); library(tibble)
  library(htmltools); library(httr2); library(pdftools)
})
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

JUSTICES_DIR <- "justices"
LINEUPS_FILE <- "lineups.json"
JUSTICES_UA  <- "Mozilla/5.0 (ceRt SCOTUS research; +https://supremecourt.report)"
# Bump to force every Term page to be rewritten after a markup change. The
# pages are cheap (no fetch), so the render simply always rewrites them; this
# is stamped into each page for the audit to read.
# j2: the "How they write" panel (R/opinion_text.R), 2026-09-18.
# j3: portraits beside the names in "The Court, in order of seniority".
JUSTICES_TEMPLATE_VERSION <- "j3"
# Stamped on every cached lineup. Bump after a change to the lineup grammar;
# a LINEUP_RETRY=1 dispatch then re-reads every entry parsed under an older
# version (render-justices.yml, `lineup_retry`).
LINEUP_PARSER_VERSION <- "p5"

# ---- the roster ---------------------------------------------------------------
# Seat dates, for which nine sat in a Term and in what order. Seniority is the
# Court's own order: the Chief first, then by date of commission. A Term runs
# from the first Monday in October; the window here is Oct 1 to Sep 30, which
# is what the Court's own Term-numbered listings use.
JUSTICE_ROSTER <- tribble(
  ~name,        ~cj,   ~from,        ~to,
  "Roberts",    TRUE,  "2005-09-29", NA,
  "Kennedy",    FALSE, "1988-02-18", "2018-07-31",
  "Thomas",     FALSE, "1991-10-23", NA,
  "Ginsburg",   FALSE, "1993-08-10", "2020-09-18",
  "Breyer",     FALSE, "1994-08-03", "2022-06-30",
  "Alito",      FALSE, "2006-01-31", NA,
  "Sotomayor",  FALSE, "2009-08-08", NA,
  "Kagan",      FALSE, "2010-08-07", NA,
  "Gorsuch",    FALSE, "2017-04-10", NA,
  "Kavanaugh",  FALSE, "2018-10-06", NA,
  "Barrett",    FALSE, "2020-10-27", NA,
  "Jackson",    FALSE, "2022-06-30", NA
) |> mutate(from = as.Date(from), to = as.Date(to))

JUSTICE_NAMES_RX <- paste0("(", paste(toupper(JUSTICE_ROSTER$name), collapse = "|"), ")")

term_window <- function(term) {
  y <- 2000L + as.integer(term)
  c(as.Date(sprintf("%d-10-01", y)), as.Date(sprintf("%d-09-30", y + 1L)))
}

#' The Justices who sat at any point in `term` (two-digit), seniority order.
term_court <- function(term) {
  w <- term_window(term)
  JUSTICE_ROSTER |>
    filter(from <= w[2], is.na(to) | to >= w[1]) |>
    arrange(desc(cj), from) |>
    mutate(label = if_else(cj, paste0(name, ", C.J."), name))
}

# "Roberts, C.J." (the Granted & Noted form) and "ROBERTS" (the syllabus form)
# both -> "Roberts".
justice_key <- function(x) {
  x <- str_squish(x %||% "")
  x <- str_remove(x, ",?\\s*C\\.?\\s*J\\.?$")
  x <- str_to_title(tolower(x))
  x[!nzchar(x)] <- NA_character_
  x
}

# ---- the Granted & Noted side --------------------------------------------------
# One row per DECISION: consolidated dockets argued together share a block on
# the list (its `group`), and the opinion is one opinion.
gn_decisions <- function(gn, term) {
  # The same footnote-digit repair the list parser now applies (.gn_dkt()),
  # for a manifest written before it did.
  d <- gn |> filter(term == !!term, !is.na(decided)) |>
    mutate(dkt = str_replace(dkt, "^(\\d{2}-\\d{4})\\d$", "\\1"))
  if (!nrow(d)) return(d |> mutate(dkts = list(character())))
  d |>
    mutate(.g = if_else(is.na(group), paste0("solo:", dkt), paste0("g:", group, ":", decided))) |>
    group_by(.g) |>
    # `dkts` BEFORE `dkt`: summarise() evaluates in order and a later
    # expression sees the column an earlier one just made, so `dkt = first(dkt)`
    # followed by `list(dkt)` pooled exactly one docket per group. Advocate
    # Health (16-74, 16-86, 16-258) carried only 16-258, which the feed does
    # not name, and went without a lineup for a day.
    summarise(dkts = list(dkt), dkt = first(dkt), caption = first(caption),
              decided = first(decided), author = first(author), others = first(others),
              writings = first(writings), result = first(result), flag = first(flag),
              no_part = first(no_part), .groups = "drop") |>
    arrange(decided, dkt)
}

# A Granted & Noted kind code -> (kind, partial). "C/P, D/P" is mixed; the lead
# label is the first token's stance, and any "/P" marks it partial.
gn_kind <- function(code) {
  if (is.null(code) || is.na(code)) return(list(kind = NA_character_, partial = FALSE))
  toks <- str_squish(unlist(str_split(code, "\\s*[,&]\\s*"))); toks <- toks[nzchar(toks)]
  # Only stance tokens count. The list occasionally carries something else in
  # the parentheses (a "no part" note, a date); those are not writings.
  toks <- toks[str_detect(toks, "^[CD](/|$)")]
  if (!length(toks)) return(list(kind = NA_character_, partial = FALSE))
  t1 <- toks[1]
  kind <- if (str_starts(t1, "D")) "diss"
          else if (str_starts(t1, "C/J")) "judg"
          else "conc"
  mixed <- any(str_starts(toks, "D")) && any(str_starts(toks, "C"))
  list(kind = kind, partial = any(str_detect(toks, "/P")) || mixed)
}

# gn_others() with the Chief written the way the list writes him: "Chief
# Justice (D)" would otherwise come out as a Justice named "Justice".
.gn_others_j <- function(o) {
  o <- str_replace_all(o %||% "", regex("Chief Justice", ignore_case = TRUE), "Roberts")
  gn_others(o)
}

#' Opinions written per Justice for a Term: tibble(name, court, conc, conc_part,
#' judg, judg_part, diss, diss_part, total), seniority order, from the list.
gn_written <- function(dec, court) {
  z <- tibble(name = court$name, court = 0L, conc = 0L, conc_part = 0L, judg = 0L,
              judg_part = 0L, diss = 0L, diss_part = 0L)
  add <- function(z, nm, col) { i <- match(nm, z$name); if (!is.na(i)) z[[col]][i] <- z[[col]][i] + 1L; z }
  for (i in seq_len(nrow(dec))) {
    a <- justice_key(dec$author[i])
    if (!is.na(a) && a != "Per Curiam") z <- add(z, a, "court")
    o <- .gn_others_j(dec$others[i])
    for (k in seq_len(nrow(o))) {
      kk <- gn_kind(o$code[k]); nm <- justice_key(o$justice[k])
      if (is.na(kk$kind)) next
      z <- add(z, nm, kk$kind)
      if (kk$partial) z <- add(z, nm, paste0(kk$kind, "_part"))
    }
  }
  z |> mutate(total = court + conc + judg + diss)
}

# ---- the lineup parser --------------------------------------------------------
# Syllabus text -> the lineup paragraph, as one squished string, or NA.
#
# The paragraph starts at the first "NAME, J., delivered|announced" and runs to
# the end of the syllabus; the opinion proper opens with "JUSTICE NAME delivered
# the opinion of the Court" in mixed case, or a "NOTICE:" / "Cite as:" header,
# whichever comes first. Running heads that fall inside a wrapped paragraph
# ("4 LANDOR v. LOUISIANA DEPT. ... Syllabus") are removed before splitting.
#
# Two typographies. A slip opinion sets the names in small caps, which pdftools
# reads as "ROBERTS, C. J., delivered". The U.S. Reports volumes (where the
# Court's listing sends every decision once a volume is printed) set the same
# paragraph in mixed case -- "Roberts, C. J., delivered ... Thomas, J., fled a
# dissenting opinion, in which Alito, J., joined, post, p. 128." -- and their
# text layer drops the "fi" ligature, so "filed" comes out "fled". The names
# are matched case-insensitively and "f(i)led" both ways throughout.
lineup_text <- function(pages, max_pages = 15L) {
  txt <- paste(head(pages, max_pages), collapse = "\n")
  # De-hyphenate before looking for the start: "Alito, J., de- livered" hid
  # Republic of Sudan v. Harrison's lead sentence on the first pass.
  txt <- str_replace_all(txt, "([A-Za-z])-\\s*\\n\\s*([a-z])", "\\1\\2")
  # A per curiam's syllabus has no "delivered" sentence; its lineup paragraph
  # opens with the first separate writing ("Thomas, J., filed a dissenting
  # opinion."), so that is a start too. regexpr() takes the earliest match, so
  # a signed opinion still starts at "delivered".
  start <- regexpr(paste0("(?i)\\b", JUSTICE_NAMES_RX, ",\\s*(C\\.\\s*)?J\\.,\\s*(delivered|announced|f(?:i)?led)"), txt, perl = TRUE)
  if (start < 0) return(NA_character_)
  s <- substr(txt, start, min(nchar(txt), start + 6000L))
  # Running heads first, because a lineup that crosses a page break has one in
  # the middle -- "Cite as: 584 U. S. 1 (2018)" / "Syllabus" / "12 OCTOBER
  # TERM, 2017" in a volume, "4 LANDOR v. LOUISIANA ... Syllabus" in a slip --
  # and the end-of-paragraph test below would otherwise stop at it and lose
  # every writing after the break (Carpenter's three further dissents, on the
  # first pass).
  s <- str_remove_all(s, "(?m)^\\s*\\d+\\s+[A-Z0-9 .,'’&()\\-]+\\s+v\\.\\s+[A-Z0-9 .,'’&()\\-]+\\s*$")
  s <- str_remove_all(s, "(?m)^\\s*[A-Z0-9 .,'’&()\\-]+\\s+v\\.\\s+[A-Z0-9 .,'’&()\\-]+\\s+\\d+\\s*$")
  s <- str_remove_all(s, "(?m)^\\s*Syllabus\\s*$")
  s <- str_remove_all(s, "(?m)^\\s*Cite as:.*$")
  s <- str_remove_all(s, "(?m)^\\s*\\d+\\s+OCTOBER TERM, \\d{4}\\s*$")
  s <- str_remove_all(s, "(?m)^\\s*OCTOBER TERM, \\d{4}\\s+\\d+\\s*$")
  # "Opinion of the Court" is a running head too, and the volumes print it on
  # a page that is still the syllabus (Rosales-Mireles: the lineup breaks at
  # "JJ.," and resumes "joined." under that head). Stripped as a line, and
  # deliberately NOT an end marker: the body is recognised by its own first
  # sentence instead.
  s <- str_remove_all(s, "(?m)^\\s*Opinion of the Court\\s*$")
  # End: the next document header (fresh text only, so line-anchored) ...
  end <- regexpr("\\n\\s*(NOTICE:|SUPREME COURT OF THE UNITED STATES|_{6,})", s, perl = TRUE)
  if (end > 0) s <- substr(s, 1L, end - 1L)
  s <- substr(s, 1L, 4000L)
  # The volumes hyphenate at line ends ("con- curring", "Gor- such", "Mem-
  # bers") and cite each writing's page ("post, p. 128"); neither is grammar.
  s <- str_replace_all(s, "([A-Za-z])-\\s*\\n\\s*([a-z])", "\\1\\2")
  s <- str_remove_all(s, ",?\\s*post,\\s*p\\.\\s*\\d+")
  .cut_lineup(.strip_watermark(str_squish(s)))
}

# ... and the opinion body or the counsel paragraph, cut on the squished
# text so it applies to a cached paragraph as well as a fresh one. The body
# opens "JUSTICE X delivered" (slip) or "Justice X delivered" (volume) -- or
# "announced the judgment", which the first version did not know, so Sessions
# v. Dimaya's paragraph ran on into its plurality opinion and the body's own
# lineup sentence overwrote the syllabus's. A volume prints counsel between
# the lineup and the body ("Deputy Solicitor General Kneedler argued and
# reargued the cause"), which is the other stop.
#
# The counsel marker is anchored at a sentence boundary -- a period-space
# followed by one to eight capitalised, comma-free words and then "argued".
# The first version let the name run back through commas, so the match began
# inside the last lineup sentence ("Kagan, JJ., joined. George S. Isaacson
# argued") and four OT17 dissents lost their joiners.
.cut_lineup <- function(s) {
  end <- regexpr(paste0("(?:(?:CHIEF )?JUSTICE [A-Z]+|(?:Chief )?Justice [A-Z][a-z]+) (?:delivered|announced)\\b",
                        "|\\. (?=(?:[A-Z][A-Za-z.'-]+ ){1,8}(?:argued|reargued)\\b)"), s, perl = TRUE)
  if (end > 0) s <- substr(s, 1L, if (substr(s, end, end) == ".") end else end - 1L)
  str_squish(s)
}

# A preliminary print's text layer carries its watermark in the running text:
# "Breyer, J., filed a Page Proof Pending Publication dissenting opinion".
# Applied to fresh text and to cached text alike, so a re-parse sees it gone.
.strip_watermark <- function(s) str_squish(str_remove_all(s, regex("Page Proof Pending Publication", ignore_case = TRUE)))

# The Justices named in a fragment, in order, as keys.
.names_in <- function(x) justice_key(str_extract_all(x %||% "", regex(paste0("\\b", JUSTICE_NAMES_RX, "\\b"), ignore_case = TRUE))[[1]])

# Every "in which NAMES joined[ qualifier]" clause in a sentence:
# list(list(who = c(...), partial = TRUE/FALSE)).
.join_clauses <- function(sent) {
  m <- str_match_all(sent, regex("in which (.+?) join(?:ed|s)((?:,? (?:as to|except|only|but)[^.;]*)?)", ignore_case = TRUE))[[1]]
  if (!nrow(m)) return(list())
  lapply(seq_len(nrow(m)), function(i) list(who = .names_in(m[i, 2]),
                                            partial = nzchar(str_squish(m[i, 3]))))
}

# Kind of a separate writing from its description.
.writing_kind <- function(desc) {
  d <- tolower(desc)
  has_d <- str_detect(d, "dissent"); has_c <- str_detect(d, "concur")
  if (has_d && has_c) return(list(kind = "diss", partial = TRUE, mixed = TRUE))
  if (has_d) return(list(kind = "diss", partial = str_detect(d, "in part"), mixed = str_detect(d, "in part")))
  if (str_detect(d, "in the judgment")) return(list(kind = "judg", partial = str_detect(d, "in part"), mixed = FALSE))
  list(kind = "conc", partial = str_detect(d, "in part"), mixed = FALSE)
}

#' Parse a lineup paragraph. Returns list(lead = list(kind, author, joins,
#' split), writings = list(list(who, kind, partial, mixed, joins)), no_part).
#' lead$kind: "court" (delivered the opinion of the Court), "judgment" (announced
#' the judgment: a plurality), or NA when no lead sentence parsed.
parse_lineup <- function(s) {
  out <- list(lead = list(kind = NA_character_, author = NA_character_, joins = list(), split = FALSE),
              writings = list(), no_part = character())
  if (is.na(s) || !nzchar(s)) return(out)
  # Sentences: split where a Justice's name opens the next one.
  sents <- str_split(s, regex(paste0("(?<=\\.)\\s+(?=", JUSTICE_NAMES_RX, "(?:,|\\s+and\\s+))"), ignore_case = TRUE))[[1]]
  filed_rx <- regex("\\bf(?:i)?led\\b", ignore_case = TRUE)
  for (sent in sents) {
    # Recusal, in either place it is written: its own sentence ("GORSUCH, J.,
    # took no part in the consideration or decision of the case.") or a tail
    # on the lead sentence ("in which all other Members joined, except
    # GORSUCH, J., who took no part ..."). The names are the ones just before
    # the phrase, after any "except"; the sentence is NOT consumed, because the
    # lead sentence still has to be read.
    if (str_detect(sent, "took no part")) {
      # From the sentence start, not from the last period: "ROBERTS, C. J.,
      # took no part" has one inside the title, and a `[^.]*` there kept only
      # ", took no part" and no name (Life Technologies, Ziglar).
      seg <- str_extract(sent, "^.*?took no part")
      seg <- str_remove(seg, regex("^.*\\bexcept\\b", ignore_case = TRUE))
      out$no_part <- c(out$no_part, .names_in(seg))
      if (!str_detect(sent, regex("delivered|announced|f(?:i)?led", ignore_case = TRUE))) next
    }
    # The first lead sentence wins: a syllabus has one, and anything later
    # that reads like one is the body leaking in.
    if (is.na(out$lead$kind) &&
        str_detect(sent, regex("delivered the opinion (of|for) (the|a unanimous) Court|announced the judgment", ignore_case = TRUE))) {
      out$lead$kind <- if (str_detect(sent, "announced the judgment") &&
                           !str_detect(sent, "delivered the opinion of the Court")) "judgment" else "court"
      out$lead$author <- .names_in(str_extract(sent, regex("^.*?(?=delivered|announced)", ignore_case = TRUE)))[1]
      out$lead$split <- str_detect(sent, "with respect to|except as to|as to Parts?")
      out$lead$joins <- .join_clauses(sent)
      if (str_detect(sent, regex("unanimous Court|in which all (other )?Members joined", ignore_case = TRUE)))
        out$lead$joins <- list(list(who = "ALL", partial = FALSE))
      # The Court's convention: joiners are listed only where fewer than all
      # joined. "JACKSON, J., delivered the opinion of the Court with respect
      # to Parts I-IV-B, and an opinion with respect to Part IV-C, in which
      # ROBERTS, C. J., and SOTOMAYOR and KAGAN, JJ., joined" means the whole
      # Court joined Parts I-IV-B and three joined IV-C. A Court clause with
      # no "in which" of its own is therefore joined by every participant not
      # on the dissent side (Barrett v. United States read as 5-0 before this).
      court_clause <- str_extract(sent, regex("delivered the opinion of the Court.*?(?=, and an opinion|\\.$|$)", ignore_case = TRUE))
      out$lead$implicit_all <- !is.na(court_clause) && !str_detect(court_clause, regex("in which", ignore_case = TRUE)) &&
        !str_detect(sent, regex("unanimous Court|all (other )?Members", ignore_case = TRUE))
      next
    }
    if (str_detect(sent, filed_rx)) {
      # Everyone named before "filed" filed one: "THOMAS, J., and GINSBURG, J.,
      # filed opinions concurring in the judgment." is two writings.
      who <- .names_in(str_extract(sent, regex("^.*?(?=\\bf(?:i)?led\\b)", ignore_case = TRUE)))
      desc <- str_extract(sent, regex("f(?:i)?led .*?(?=,? in which|\\.$|$)", ignore_case = TRUE))
      if (is.na(desc)) desc <- sent
      k <- .writing_kind(desc)
      for (w in who)
        out$writings[[length(out$writings) + 1L]] <-
          list(who = w, kind = k$kind, partial = k$partial, mixed = k$mixed, joins = .join_clauses(sent))
      next
    }
    # "ROBERTS, C. J., and ALITO, J., joined that opinion in full, and THOMAS,
    # J., joined except as to Part IV-B." -- joins to the lead opinion written
    # as their own sentence, when the opinion of the Court is itself partial.
    if (str_detect(sent, regex("\\bjoined\\b", ignore_case = TRUE)) && !is.na(out$lead$kind)) {
      # Chunk at each "joined": the names before it are the joiners, the text
      # after it (up to the next names) is the qualifier. Qualifiers name
      # Parts, never Justices, so a chunk's names are exactly one group's.
      chunks <- str_split(sent, regex("\\bjoined\\b", ignore_case = TRUE))[[1]]
      for (k in seq_len(length(chunks) - 1L)) {
        who <- .names_in(chunks[k])
        if (!length(who)) next
        qual <- str_extract(chunks[k + 1L], regex(paste0("^.*?(?=\\b", JUSTICE_NAMES_RX, "\\b|$)"), ignore_case = TRUE))
        partial <- str_detect(qual %||% "", regex("except|as to|only|in part", ignore_case = TRUE))
        out$lead$joins[[length(out$lead$joins) + 1L]] <- list(who = who, partial = partial)
      }
    }
  }
  out$no_part <- unique(out$no_part[!is.na(out$no_part)])
  out
}

# Per curiam and other unsyllabused opinions carry no lineup paragraph. Their
# separate writings announce themselves in the body: "JUSTICE SOTOMAYOR, with
# whom JUSTICE KAGAN and JUSTICE JACKSON join, dissenting." One header per
# writing; the join list is complete by construction.
parse_body_headers <- function(pages, max_pages = 40L) {
  txt <- str_squish(str_replace_all(paste(head(pages, max_pages), collapse = "\n"), "([A-Za-z])-\\s*\\n\\s*([a-z])", "\\1\\2"))
  # Small caps in a slip ("JUSTICE THOMAS, dissenting."), mixed case in a
  # volume ("Justice Breyer, with whom Justice Ginsburg joins, dissenting.").
  m <- str_match_all(txt, regex(paste0("(?:chief )?justice ", JUSTICE_NAMES_RX,
    "(, with whom ((?:(?:chief )?justice ", JUSTICE_NAMES_RX, "(?:, )?(?:and )?)+) joins?)?, ",
    "(dissenting|concurring in the judgment|concurring in part and dissenting in part|",
    "concurring in the judgment in part and dissenting in part|dissenting in part|concurring in part|concurring)\\."),
    ignore_case = TRUE))[[1]]
  if (!nrow(m)) return(list())
  seen <- character(); out <- list()
  # Columns: full match, name, the "with whom" clause, its name list, the last
  # name inside that list (an artefact of the repeated group), the kind.
  for (i in seq_len(nrow(m))) {
    who <- justice_key(m[i, 2]); kind_txt <- m[i, 6]; key <- paste(who, kind_txt)
    if (key %in% seen) next
    seen <- c(seen, key)
    k <- .writing_kind(kind_txt)
    joins <- .names_in(m[i, 4])
    out[[length(out) + 1L]] <- list(who = who, kind = k$kind, partial = k$partial, mixed = k$mixed,
                                    joins = if (length(joins)) list(list(who = joins, partial = FALSE)) else list())
  }
  out
}

# ---- fetch and cache ----------------------------------------------------------
# Once a Term's opinions are printed in the U.S. Reports, the Court's listing
# stops naming slip opinions and points into the BOUND VOLUME instead:
# /opinions/boundvolumes/580BV.pdf#page=324 -- one 1,100-page PDF for the whole
# volume, with a page anchor per case. Every OT16 decision resolved that way
# on 2026-09-13, and reading page one of the book for each was how the first
# pass "parsed" twelve per curiams with no writings. So a volume is downloaded
# and text-extracted ONCE per run (memoised here), and a decision reads the
# fifteen pages from its anchor, which is where its syllabus and lineup are.
.volume_memo <- new.env(parent = emptyenv())

#
# The feed names volumes two ways, and one of them rots: a PRELIMINARY PRINT
# (/opinions/preliminaryprint/585US2PP_final.pdf) while the volume is in
# press, replaced by the BOUND VOLUME (/opinions/boundvolumes/585BV.pdf) once
# printed -- but the feed keeps the preliminary-print link, which then 404s.
# Measured 2026-09-13: volumes 580-587 are served bound and their preliminary
# prints are gone; 588 onward are preliminary prints. So a dead preliminary
# print falls back to the bound volume of the same number. The page anchor is
# only a hint either way (the two files paginate differently): the case's
# syllabus is found by searching the volume for its docket number, which the
# syllabus header prints as "No. 15-8049. Argued ... Decided ...".
.download_pdf <- function(url, timeout) {
  tf <- tempfile(fileext = ".pdf")
  ok <- tryCatch({
    resp <- request(url) |> req_user_agent(JUSTICES_UA) |> req_timeout(timeout) |>
      req_error(is_error = \(resp) FALSE) |> req_perform(path = tf)
    # The Court's 404 page is 1,027 bytes of HTML; pdftools then "reads" it
    # with a screen of hex-string errors. Status and type, not size.
    resp_status(resp) == 200L && str_detect(resp_content_type(resp) %||% "", "pdf") &&
      file.exists(tf) && file.size(tf) > 1000
  }, error = function(e) FALSE)
  if (!ok) { unlink(tf); return(NULL) }
  tf
}

# Text pages of a volume, memoised per run; NULL when it cannot be had.
# Returns list(pages, fetched).
.volume_pages <- function(base) {
  if (exists(base, envir = .volume_memo, inherits = FALSE))
    return(list(pages = get(base, envir = .volume_memo), fetched = FALSE))
  cands <- base
  vol <- str_match(base, "preliminaryprint/(\\d{3})US")[1, 2]
  if (!is.na(vol)) cands <- c(base, sprintf("https://www.supremecourt.gov/opinions/boundvolumes/%sBV.pdf", vol))
  for (u in cands) {
    # A bound volume already fetched for the other half of the same
    # preliminary print (585US1PP and 585US2PP both fall back to 585BV).
    if (exists(u, envir = .volume_memo, inherits = FALSE) && !is.null(get(u, envir = .volume_memo))) {
      pages <- get(u, envir = .volume_memo); assign(base, pages, envir = .volume_memo)
      return(list(pages = pages, fetched = FALSE))
    }
    tf <- .download_pdf(u, 600)
    if (is.null(tf)) { message("  volume ", basename(u), ": not served"); next }
    pages <- tryCatch(suppressWarnings(pdf_text(tf)), error = function(e) character()); unlink(tf)
    if (!length(pages)) { message("  volume ", basename(u), ": unreadable"); next }
    message("  volume ", basename(u), ": ", length(pages), " pages, memoised for this run")
    assign(base, pages, envir = .volume_memo); assign(u, pages, envir = .volume_memo)
    return(list(pages = pages, fetched = TRUE))
  }
  # Memoise the failure too, so the run does not retry a dead volume for
  # every decision in it; the next run tries again.
  assign(base, NULL, envir = .volume_memo)
  list(pages = NULL, fetched = TRUE)
}

# Returns list(pages = character(), fetched = TRUE/FALSE): `fetched` says a
# network request was made, for the pacing and throttle counters. `dkts` are
# the decision's docket numbers, for locating it inside a volume.
# `whole = TRUE` reads the decision's every page (for the text measures in
# R/opinion_text.R): a slip is one file anyway; in a volume the window runs
# from the case's syllabus to the next case's header without the fifteen-page
# cap the lineup needs.
.fetch_pdf_pages <- function(url, dkts = character(), whole = FALSE) {
  anchor <- suppressWarnings(as.integer(str_match(url, "#page=(\\d+)")[1, 2]))
  base <- str_remove(url, "#.*$")
  if (is.na(anchor)) {
    tf <- .download_pdf(base, 60)
    if (is.null(tf)) return(list(pages = character(), fetched = TRUE))
    pages <- tryCatch(suppressWarnings(pdf_text(tf)), error = function(e) character()); unlink(tf)
    return(list(pages = pages, fetched = TRUE))
  }
  v <- .volume_pages(base)
  pages <- v$pages
  if (is.null(pages) || !length(pages)) return(list(pages = character(), fetched = v$fetched))
  # "No. 15-8049. Argued ..." or, consolidated, "Nos. 15-1406 and 15-1512.
  # Argued ..."; the volume prints the dash as an en dash.
  rx <- paste0("Nos?\\.[^\\n]{0,80}?\\b(", paste(str_replace_all(dkts, "-", "[–-]"), collapse = "|"), ")\\b[^\\n]{0,80}?Argued")
  hit <- if (length(dkts)) which(str_detect(pages, rx))[1] else NA_integer_
  p <- if (!is.na(hit)) hit else anchor
  if (p > length(pages)) return(list(pages = character(), fetched = v$fetched))
  win <- seq(p, min(length(pages), p + (if (whole) 299L else 14L)))
  # Stop at the next case. A dismissal or a short per curiam runs a page or
  # two, and a fixed window would then read the following case's lineup as
  # this one's (Cox v. United States took Ohio v. American Express's on the
  # first pass). The next case opens with its own "No. ... Argued" header.
  nxt <- which(str_detect(pages[win[-1]], "Nos?\\.[^\\n]{0,80}?\\b\\d{2}[–-]\\d{1,5}\\b[^\\n]{0,80}?Argued"))
  if (length(nxt)) win <- win[seq_len(nxt[1])]
  list(pages = pages[win], fetched = v$fetched)
}

# Every docket number an opinion names on its first two pages: its own
# ("No. 16-74. Argued ..."), and the companions decided with it ("Together
# with No. 16-258, Dignity Health v. Rollins ..." in a slip's footnote; "Nos.
# 16-74 and 16-258" in a volume's header). Stored on the cache entry as `also`,
# so a companion that has no PDF of its own -- the feed names one file per
# opinion, under the lead docket -- can find the lineup through the entry
# that names it. Dashes are normalised to the hyphen the list uses.
.dockets_in <- function(pages) {
  # The first three pages -- the excerpt-style slips (603us1r54_o7jp.pdf)
  # open with a cover page, so page one alone came back empty for Loper
  # Bright -- but only the dockets in a header or a "Together with" clause:
  # "No. 22-451. Argued", "Nos. 19-1442 and 20-105. Argued", "Together with
  # No. 22-1219, Relentless ... and No. 16-1017, Cox ...". A syllabus that
  # merely cites another case's docket does not link the two.
  txt <- str_squish(paste(head(pages, 3L), collapse = " "))
  segs <- c(str_extract_all(txt, "\\bNos?\\.\\s*\\d{2}[–-]\\d{1,5}[^.]{0,80}?(?=\\.\\s*(?:Argued|Decided|Reargued))")[[1]],
            str_extract_all(txt, "Together with No\\.\\s*\\d{2}[–-]\\d{1,5}(?:[^.]{0,120}?(?:,| and) No\\.\\s*\\d{2}[–-]\\d{1,5})*")[[1]])
  d <- unlist(str_extract_all(segs, "\\b\\d{2}[–-]\\d{1,5}\\b|\\b\\d{2}A\\d{1,4}\\b|\\b22O\\d{1,4}\\b"))
  unique(str_replace_all(d, "–", "-"))
}

lineups_path <- function(site_dir) file.path(site_dir, JUSTICES_DIR, LINEUPS_FILE)

read_lineups <- function(site_dir) {
  p <- lineups_path(site_dir)
  if (!file.exists(p)) return(list())
  tryCatch(fromJSON(p, simplifyVector = FALSE), error = function(e) list())
}

#' Fetch and parse the lineup for every decision in `dec` (a gn_decisions()
#' frame) whose docket the cache lacks, up to `max_new`, `pace` seconds apart,
#' stopping after `max_consecutive_empty` empty downloads (the runner is being
#' throttled). `urls` is a named character vector docket -> opinion PDF from
#' fetch_opinion_listing(). A decision with no known URL is skipped and retried
#' whenever a later listing names it. A fetched PDF that yields no lineup is
#' cached as `parsed = FALSE` so it is not fetched again unless `retry` is set.
resolve_lineups <- function(dec, urls, site_dir, max_new = 0L, pace = 0.75,
                            retry = FALSE, max_consecutive_empty = 8L) {
  cache <- read_lineups(site_dir)
  # A grammar fix first re-reads the cached lineup TEXT, which costs nothing:
  # every entry parsed from a syllabus keeps its paragraph. Only entries whose
  # writings came from the body headers (per curiams) need the pages again,
  # and those go through the fetch below when `retry` is set.
  stale <- names(cache)[vapply(names(cache), function(dk) {
    e <- cache[[dk]]; isTRUE(e$parsed) && !identical(e$pv, LINEUP_PARSER_VERSION) &&
      !is.null(e$text) && nzchar(e$text) && !identical(e$lead$kind, "per curiam")
  }, logical(1))]
  if (length(stale)) {
    for (dk in stale) {
      # Parse a cut copy; the cached text stays as fetched, so a later, better
      # cut still has the whole paragraph to work from.
      e <- cache[[dk]]; p <- parse_lineup(.cut_lineup(.strip_watermark(e$text)))
      if (is.na(p$lead$kind)) next   # leave it for the fetch path
      e$lead <- p$lead; e$writings <- p$writings; e$no_part <- p$no_part; e$pv <- LINEUP_PARSER_VERSION
      cache[[dk]] <- e
    }
    message("lineups: re-parsed ", length(stale), " cached entr", if (length(stale) == 1) "y" else "ies",
            " under parser ", LINEUP_PARSER_VERSION)
    write_lineups(cache, site_dir)
  }
  # With `retry`, an entry is stale if its PDF yielded nothing OR it was parsed
  # by an older parser and could not be re-read from its text: the stamp lets
  # a grammar fix reach the archive with one dispatch instead of a
  # hand-deleted cache.
  # ... or it predates the `also` field, which only a fetch can fill.
  is_cached <- function(dk) !is.null(cache[[dk]]) &&
    (!retry || (isTRUE(cache[[dk]]$parsed) && identical(cache[[dk]]$pv, LINEUP_PARSER_VERSION) && length(cache[[dk]]$also) > 0))
  url_for <- function(dkts) { u <- urls[dkts]; u <- u[!is.na(u)]; if (length(u)) u[[1]] else NA_character_ }
  todo <- dec |> filter(!vapply(dkt, is_cached, logical(1))) |>
    mutate(url = vapply(dkts, url_for, character(1))) |> filter(!is.na(url))
  n_uncached <- sum(!vapply(dec$dkt, is_cached, logical(1)))
  todo <- head(todo, max_new)
  if (nrow(todo)) {
    message("lineups: fetching ", nrow(todo), " of ", n_uncached, " uncached decision(s) (cap ", max_new, ")")
    empties <- 0L
    for (i in seq_len(nrow(todo))) {
      res <- .fetch_pdf_pages(todo$url[i], todo$dkts[[i]]); pages <- res$pages
      txt <- lineup_text(pages)
      entry <- list(url = todo$url[i], decided = as.character(todo$decided[i]),
                    fetched = as.character(Sys.Date()), pv = LINEUP_PARSER_VERSION)
      if (!length(pages)) {
        if (res$fetched) empties <- empties + 1L
        entry$parsed <- FALSE; entry$chars <- 0L
      } else {
        if (res$fetched) empties <- 0L
        p <- parse_lineup(txt)
        if (is.na(p$lead$kind)) {
          # No "delivered" sentence: a per curiam. Its syllabus may still list
          # the separate writings (the volumes do); failing that, read them off
          # the body headers ("JUSTICE THOMAS, with whom ..., dissenting.").
          if (!length(p$writings)) p$writings <- parse_body_headers(pages)
          p$lead$kind <- "per curiam"
        }
        entry$parsed <- TRUE; entry$chars <- sum(nchar(pages)); entry$text <- txt
        entry$lead <- p$lead; entry$writings <- p$writings; entry$no_part <- p$no_part
        entry$also <- .dockets_in(pages)
      }
      cache[[todo$dkt[i]]] <- entry
      if (empties >= max_consecutive_empty) {
        message("lineups: ", empties, " empty downloads in a row after ", i,
                " fetch(es) -- throttled; stopping this run (retry next run)")
        break
      }
      if (i %% 50 == 0) { write_lineups(cache, site_dir); message("  ...", i, "/", nrow(todo)) }
      if (pace > 0 && res$fetched) Sys.sleep(pace)
    }
    write_lineups(cache, site_dir)
    got <- vapply(head(todo$dkt, i), function(d) isTRUE(cache[[d]]$parsed), logical(1))
    message("lineups: ", sum(got), " parsed of ", i, " fetched")
    attr(cache, "n_fetched") <- i
  } else attr(cache, "n_fetched") <- 0L
  invisible(cache)
}

write_lineups <- function(cache, site_dir) {
  p <- lineups_path(site_dir)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  write_json(cache, p, auto_unbox = TRUE, null = "null", na = "null", pretty = FALSE)
}

# ---- votes ---------------------------------------------------------------------
# One decision's cached lineup -> per-Justice side and full-join flag:
# tibble(name, side, full). side: "majority", "dissent", "mixed", or NA when the
# lineup did not account for the Justice (which excludes the decision from the
# matrix; see docs/justices.md).
decision_votes <- function(entry, court, gn_no_part = NULL, decided = NULL) {
  names <- court$name
  no_part <- unique(c(justice_key(unlist(entry$no_part)), justice_key(gn_no_part)))
  no_part <- no_part[!is.na(no_part)]
  # Who sat on the DAY, not in the Term: OT16 opened with eight Justices and
  # Gorsuch took his seat in April; OT20 opened with eight and Barrett came in
  # late October. A Justice not yet seated is neither a participant nor a
  # recusal -- they simply do not count for that decision.
  if (!is.null(decided) && !is.na(decided)) {
    d <- as.Date(decided)
    seated <- court$name[court$from <= d & (is.na(court$to) | court$to >= d)]
    no_part <- unique(c(no_part, setdiff(names, seated)))
  }
  part <- setdiff(names, no_part)
  maj <- character(); dis <- character(); mix <- character(); full <- character()
  lead <- entry$lead
  if (!is.null(lead) && !is.na(lead$kind %||% NA) && lead$kind %in% c("court", "judgment")) {
    a <- justice_key(lead$author); if (!is.na(a)) { maj <- c(maj, a); if (lead$kind == "court" && !isTRUE(lead$split)) full <- c(full, a) }
    for (j in lead$joins %||% list()) {
      who <- unlist(j$who); if (identical(who, "ALL")) who <- setdiff(part, a)
      who <- justice_key(who)
      maj <- c(maj, who)
      if (lead$kind == "court" && !isTRUE(lead$split) && !isTRUE(j$partial)) full <- c(full, who)
    }
  }
  for (w in entry$writings %||% list()) {
    who <- c(justice_key(w$who), justice_key(unlist(lapply(w$joins %||% list(), function(j) unlist(j$who)))))
    who <- who[!is.na(who)]
    if (identical(w$kind, "diss") && !isTRUE(w$mixed)) dis <- c(dis, who)
    else if (isTRUE(w$mixed)) mix <- c(mix, who)
    else maj <- c(maj, who)
  }
  if (identical(lead$kind %||% "", "per curiam")) maj <- c(maj, setdiff(part, c(dis, mix)))
  # A Court clause with no joiner list (see parse_lineup): everyone who did not
  # dissent joined it. Full-opinion agreement still follows `split`.
  if (isTRUE(lead$implicit_all)) {
    who <- setdiff(part, c(dis, mix)); maj <- c(maj, who)
    if (identical(lead$kind, "court") && !isTRUE(lead$split)) full <- c(full, who)
  }
  # A signed opinion's syllabus names every participant -- as author, joiner,
  # or separate writer. A Justice named nowhere did not sit: seated after the
  # argument (Gorsuch in the spring of OT16, Barrett in the autumn of OT20) or
  # recused without the syllabus saying so. Neither is a vote.
  if (!is.null(lead) && (lead$kind %||% "") %in% c("court", "judgment")) {
    named <- unique(c(maj, dis, mix))
    no_part <- unique(c(no_part, setdiff(part, named)))
    part <- setdiff(names, no_part)
  }
  side <- vapply(names, function(n) {
    if (n %in% no_part) return(NA_character_)
    in_m <- n %in% maj; in_d <- n %in% dis; in_x <- n %in% mix
    if (in_x || (in_m && in_d)) "mixed" else if (in_d) "dissent" else if (in_m) "majority" else NA_character_
  }, character(1))
  tibble(name = names, side = unname(side), full = names %in% full & side == "majority",
         no_part = names %in% no_part)
}

# ---- statistics ----------------------------------------------------------------
#' Everything a Term page shows. `gn` is read_granted_noted(); `lineups` is
#' read_lineups(); `captions` an optional docket -> caption map (cases/search.json).
term_stats <- function(term, gn, lineups, captions = NULL, texts = NULL) {
  court <- term_court(term)
  dec <- gn_decisions(gn, term)
  if (!nrow(dec)) return(NULL)

  # The lineup for a decision: under its own docket, else an entry from the
  # same day that names the docket among the cases decided with it (`also`).
  find_entry <- function(dkts, decided) {
    for (d in dkts) if (!is.null(lineups[[d]]) && isTRUE(lineups[[d]]$parsed)) return(lineups[[d]])
    for (e in lineups) if (isTRUE(e$parsed) && identical(e$decided, as.character(decided)) &&
                           any(dkts %in% unlist(e$also))) return(e)
    NULL
  }
  ent <- lapply(seq_len(nrow(dec)), function(i) find_entry(dec$dkts[[i]], dec$decided[i]))
  # One opinion, one decision. Companions the list files as separate blocks
  # (Little v. Hecox beside West Virginia v. B. P. J.; Lamone beside Rucho)
  # were being counted twice in the matrix and the splits. Two rows are the
  # same opinion when, on the same day, the dockets they and their entries
  # name overlap (the smallest docket of the pooled set is the key), or when
  # they share a slip-opinion file. NOT when their lineup text matches: two
  # unanimous opinions by one author on one day read identically. A volume
  # URL is one file for a hundred opinions, so it never keys anything.
  dec$.key <- vapply(seq_along(ent), function(i) {
    e <- ent[[i]]
    if (is.null(e)) return(paste0("own:", i))
    if (!is.null(e$also)) return(paste0(dec$decided[i], "|", sort(unique(c(dec$dkts[[i]], unlist(e$also))))[1]))
    if (!grepl("#", e$url %||% "")) return(paste0(dec$decided[i], "|url:", e$url))
    paste0("own:", i)
  }, character(1))
  dec <- dec |> group_by(.key) |> mutate(dkts = list(unique(unlist(dkts)))) |>
    arrange(is.na(author), .by_group = TRUE) |> slice_head(n = 1) |> ungroup() |> select(-.key) |>
    arrange(decided, dkt)
  ent <- lapply(seq_len(nrow(dec)), function(i) find_entry(dec$dkts[[i]], dec$decided[i]))
  # A decided row with no author, no separate writings and no opinion anywhere
  # is a disposition by order -- a dismissal, a vacatur as moot -- not a
  # decision by written opinion, and does not belong in the count.
  keep <- !(is.na(dec$author) & vapply(ent, is.null, logical(1)) & !nzchar(coalesce(dec$others, "")))
  dec <- dec[keep, ]; ent <- ent[keep]
  if (!nrow(dec)) return(NULL)

  written <- gn_written(dec, court)
  n_signed <- sum(!is.na(dec$author) & dec$author != "Per Curiam")

  # Votes per decision, where the lineup parsed and accounts for everyone.
  votes <- lapply(seq_len(nrow(dec)), function(i) {
    e <- ent[[i]]
    if (is.null(e)) return(NULL)
    v <- decision_votes(e, court, dec$no_part[i], dec$decided[i])
    if (any(is.na(v$side) & !v$no_part)) return(NULL)   # incomplete: leave out
    v$i <- i; v
  })
  ok <- !vapply(votes, is.null, logical(1))
  V <- if (any(ok)) bind_rows(votes[ok]) else tibble(name = character(), side = character(), full = logical(), no_part = logical(), i = integer())
  n_lineup <- sum(ok)

  # Agreement matrices: share of decisions both took part in where both were on
  # the same side (judgment) / both joined the Court's opinion in full (full).
  # A mixed Justice is on neither side, so a pair with one drops out of that
  # decision's denominator.
  J <- court$name; k <- length(J)
  agree_j <- matrix(NA_real_, k, k, dimnames = list(J, J)); agree_f <- agree_j; n_pair <- matrix(0L, k, k, dimnames = list(J, J))
  if (n_lineup) {
    for (a in seq_len(k)) for (b in seq_len(k)) if (a != b) {
      va <- V |> filter(name == J[a]); vb <- V |> filter(name == J[b])
      m <- inner_join(va, vb, by = "i", suffix = c(".a", ".b")) |>
        filter(!is.na(side.a), !is.na(side.b), side.a != "mixed", side.b != "mixed")
      n_pair[a, b] <- nrow(m)
      if (nrow(m)) {
        agree_j[a, b] <- mean(m$side.a == m$side.b)
        agree_f[a, b] <- mean(m$full.a & m$full.b)
      }
    }
  }

  # Per decision: majority size, minority size, the majority set.
  per_dec <- if (n_lineup) V |> filter(!no_part) |> group_by(i) |>
    summarise(n_maj = sum(side == "majority"), n_min = sum(side != "majority"),
              maj_set = paste(sort(name[side == "majority"]), collapse = "|"),
              min_set = paste(sort(name[side != "majority"]), collapse = "|"), .groups = "drop") else
    tibble(i = integer(), n_maj = integer(), n_min = integer(), maj_set = character(), min_set = character())
  splits <- per_dec |> count(n_maj, n_min) |> arrange(desc(n_maj))
  # Exact splits, not "three in the minority": on an eight-member Court (OT16
  # until April, OT20 until late October) a 5-3 is the close case, and the
  # first version counted those as 6-3s -- OT16's page said seven where the
  # split list showed one. The closest splits each get their own lineup:
  # tibble(n_maj, n_min, n, maj_set, min_set, top) with the most frequent
  # lineup per split, for the splits that occurred at least twice.
  # `close_splits`, NOT `lineups`: that name is this function's argument, the
  # cache, and the writings list below reads it. The first version of this
  # block shadowed it with this tibble, and every "joined by" cell on every
  # Term page read "lineup not parsed" for a day.
  close_splits <- per_dec |> filter((n_maj == 6L & n_min == 3L) | (n_maj == 5L & n_min == 4L) | (n_maj == 5L & n_min == 3L)) |>
    group_by(n_maj, n_min) |> mutate(n = n()) |> count(n_maj, n_min, n, maj_set, min_set, name = "top", sort = TRUE) |>
    group_by(n_maj, n_min) |> slice_head(n = 1) |> ungroup() |> filter(n >= 2L) |> arrange(desc(n_maj), desc(n_min))
  n_63 <- sum(per_dec$n_maj == 6L & per_dec$n_min == 3L)
  top_lineup <- close_splits |> filter(n_maj == 6L, n_min == 3L)

  # Per Justice from the votes: in the majority, lone dissents, solo writings.
  by_j <- if (n_lineup) V |> filter(!no_part) |> group_by(name) |>
    summarise(n_part = n(), in_maj = mean(side == "majority"), .groups = "drop") else
    tibble(name = character(), n_part = integer(), in_maj = numeric())
  lone <- per_dec |> filter(n_min == 1L) |> count(min_set, name = "lone")
  solo_w <- if (n_lineup) {
    rows <- list()
    for (i in which(ok)) for (w in ent[[i]]$writings %||% list()) {
      nj <- length(unlist(lapply(w$joins %||% list(), function(j) unlist(j$who))))
      rows[[length(rows) + 1L]] <- tibble(name = justice_key(w$who), kind = w$kind, solo = nj == 0L)
    }
    if (length(rows)) bind_rows(rows) |> filter(solo) |> count(name, kind) else tibble(name = character(), kind = character(), n = integer())
  } else tibble(name = character(), kind = character(), n = integer())

  # Unanimity from the list's own flags (complete even where lineups are not).
  n_unan <- sum(dec$flag %in% "unanimous"); n_unan_j <- sum(dec$flag %in% "unanimous in judgment")

  # The writings list: every writing, per Justice, with joins where known.
  wl <- list()
  for (i in seq_len(nrow(dec))) {
    e <- ent[[i]]
    joined_by <- function(who, kind) {
      if (is.null(e)) return(NA_character_)
      if (kind == "court") {
        js <- unlist(lapply(e$lead$joins %||% list(), function(j) { w <- unlist(j$who); if (identical(w, "ALL")) "the full Court" else paste0(justice_key(w), if (isTRUE(j$partial)) " (in part)" else "") }))
      } else {
        ws <- Filter(function(w) identical(justice_key(w$who), who), e$writings %||% list())
        js <- unlist(lapply(ws, function(w) unlist(lapply(w$joins %||% list(), function(j) paste0(justice_key(unlist(j$who)), if (isTRUE(j$partial)) " (in part)" else "")))))
      }
      if (!length(js)) "no one" else paste(js, collapse = ", ")
    }
    a <- justice_key(dec$author[i])
    if (!is.na(a) && a != "Per Curiam")
      wl[[length(wl) + 1L]] <- tibble(name = a, decided = dec$decided[i], kind = "court", partial = FALSE,
                                      code = "Court", dkt = dec$dkt[i], caption = dec$caption[i],
                                      author = a, joined = joined_by(a, "court"), result = dec$result[i])
    o <- .gn_others_j(dec$others[i])
    for (r in seq_len(nrow(o))) {
      kk <- gn_kind(o$code[r]); nm <- justice_key(o$justice[r])
      if (is.na(kk$kind) || is.na(nm)) next
      wl[[length(wl) + 1L]] <- tibble(name = nm, decided = dec$decided[i], kind = kk$kind, partial = kk$partial,
                                      code = o$code[r], dkt = dec$dkt[i], caption = dec$caption[i],
                                      author = if (is.na(a)) "Per Curiam" else a,
                                      joined = joined_by(nm, kk$kind), result = dec$result[i])
    }
  }
  wl <- if (length(wl)) bind_rows(wl) |> arrange(desc(decided)) else NULL
  if (!is.null(wl)) {
    # The list's captions are set in capitals ("WOLFORD V. LOPEZ"); the site's
    # own (cases/search.json) are already cased and win where present.
    wl$caption <- .jx_caption(wl$caption)
    if (!is.null(captions)) { cp <- unname(captions[wl$dkt]); wl$caption <- ifelse(is.na(cp), wl$caption, cp) }
  }

  # The text measures (R/opinion_text.R), where the cache has the Term's
  # opinions; NULL renders no panel.
  text <- if (!is.null(texts) && length(texts) && exists("term_text_rows"))
    tryCatch(term_text_stats(term_text_rows(dec, wl, texts), court), error = function(e) {
      message("text measures skipped for OT", term, ": ", conditionMessage(e)); NULL }) else NULL

  list(term = term, court = court, dec = dec, n_dec = nrow(dec), n_signed = n_signed,
       n_lineup = n_lineup, written = written, agree_j = agree_j, agree_f = agree_f, n_pair = n_pair,
       splits = splits, top_lineup = top_lineup, lineups = close_splits, n_63 = n_63, by_j = by_j, lone = lone, solo_w = solo_w,
       n_unan = n_unan, n_unan_j = n_unan_j, writings = wl, text = text)
}

# ---- rendering ---------------------------------------------------------------------
.jx_esc <- function(x) htmlEscape(x %||% "")
.jx_pct <- function(p) if (is.na(p)) "" else paste0(round(100 * p), "%")
.jx_date <- function(d) format(as.Date(d), "%b %e, %Y")

# Title-case a Granted & Noted caption ("WOLFORD V. LOPEZ"), keeping the
# initialisms the Court writes in caps. Used only when cases/search.json has no
# caption for the docket.
.jx_caption <- function(x) {
  x <- str_to_title(tolower(x))
  x <- str_replace_all(x, "\\bV\\.", "v.")
  x <- str_replace_all(x, "\\b(Llc|Llp|Inc|Usa|Us|Fcc|Ftc|Fda|Epa|Nlrb|Sec|Irs|Doj|Dhs|Hhs|Nrc|Fbi|Cia|Atf|Eeoc|Uaw|Naacp|Aclu|Ncaa|Nra|Rnc|Dnc)\\b", toupper)
  x <- str_replace_all(x, "\\bU\\. S\\.", "U. S.")
  x
}

# The ramp stops are spliced in from GRANT_RAMP after fill_palette(), so the
# only place the ramp's two mid-tones are written down stays R/palette.R.
JUSTICES_CSS <- sub("RAMP_STOPS", paste(GRANT_RAMP, collapse = ","), fixed = TRUE, x = fill_palette("
:root{--op-court:@op-court@;--op-conc:@op-conc@;--op-judg:@op-judg@;--op-diss:@op-diss@}
main.wrap{max-width:54rem}
.jx-court{display:grid;grid-template-columns:repeat(3,1fr);gap:.6rem 1rem;margin:0 0 2.2rem;padding:0;list-style:none}
@media (max-width:520px){.jx-court{grid-template-columns:1fr 1fr}}
.jx-court li{border-top:1px solid var(--rule);padding-top:.4rem;font-size:.92rem;display:grid;grid-template-columns:44px 1fr;grid-template-rows:auto auto;column-gap:.6rem;align-items:center}
.jx-court .pt{grid-row:1/3;width:44px;height:44px;border-radius:50%;background-repeat:no-repeat;background-color:var(--stripe);border:1px solid var(--rule);box-shadow:inset 0 0 0 1px rgba(@paper:rgb@,.6)}
.jx-court .nm{display:block;font-weight:600;align-self:end}.jx-court .since{display:block;color:var(--faint);font-size:.8rem;font-variant-numeric:tabular-nums;align-self:start}
.jx h2{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.55rem;letter-spacing:-.01em;margin:0 0 .3rem;display:flex;flex-wrap:wrap;align-items:center;gap:.6rem}
.jx h3{font-family:'Newsreader',serif;font-weight:600;font-size:.8rem;letter-spacing:.12em;text-transform:uppercase;color:var(--ink-soft);margin:1.4rem 0 .6rem}
.jx{margin:0 0 3rem}
.jx .note{font-style:italic;color:var(--ink-soft);font-size:.95rem;max-width:46rem;margin:.2rem 0 1.1rem}
.jx .tag{display:inline-block;font-size:.7rem;letter-spacing:.08em;text-transform:uppercase;padding:.15rem .5rem;border:1px solid var(--rule);border-radius:2px;color:var(--faint);vertical-align:middle;white-space:nowrap}
.jx-legend{display:flex;flex-wrap:wrap;gap:.4rem 1.1rem;font-size:.86rem;color:var(--ink-soft);margin:0 0 .9rem;padding:0;list-style:none}
.jx-legend i{display:inline-block;width:.85rem;height:.85rem;border-radius:2px;vertical-align:-.1em;margin-right:.35rem}
.jx-legend .part i{background-image:repeating-linear-gradient(135deg,transparent 0 3px,var(--paper) 3px 5px)}
.jx-bars{display:grid;grid-template-columns:8.5rem 1fr 2.6rem;gap:.45rem .7rem;align-items:center;font-size:.95rem}
@media (max-width:700px){.jx-bars{grid-template-columns:6.5rem 1fr 2.2rem}}
.jx-bars .lab{text-align:right}.jx-bars .tot{font-variant-numeric:tabular-nums;color:var(--ink-soft);font-size:.9rem}
.jx-bar{display:flex;gap:2px;height:1.15rem}
.jx-seg{height:100%;min-width:2px;position:relative}
.jx-seg:first-child{border-radius:2px 0 0 2px}.jx-seg:last-child{border-radius:0 2px 2px 0}
.jx-seg.court{background:var(--op-court)}.jx-seg.conc{background:var(--op-conc)}.jx-seg.judg{background:var(--op-judg)}.jx-seg.diss{background:var(--op-diss)}
.jx-seg.part{background-image:repeating-linear-gradient(135deg,transparent 0 3px,rgba(@paper:rgb@,.75) 3px 5px)}
.jx-axis{grid-column:2;display:flex;justify-content:space-between;font-size:.72rem;color:var(--faint);font-variant-numeric:tabular-nums;border-top:1px solid var(--rule);padding-top:.2rem;margin-top:.2rem}
.jx-tw{overflow-x:auto}
.jx table{border-collapse:collapse;width:100%;font-size:.92rem;font-variant-numeric:tabular-nums}
.jx th{font-weight:600;font-size:.74rem;letter-spacing:.06em;text-transform:uppercase;color:var(--accent);text-align:left;border-bottom:2px solid var(--ink);padding:.4rem .55rem;white-space:nowrap}
.jx td{padding:.4rem .55rem;border-bottom:1px solid var(--rule);vertical-align:top}
.jx tbody tr:nth-child(even){background:var(--stripe)}
.jx td.n,.jx th.n{text-align:right;white-space:nowrap}
.jx td.pc,.jx .pc{font-size:.82rem;color:var(--faint);font-weight:400;text-transform:none;letter-spacing:0}
.jx .dim{color:var(--rule)}
.jx tr.pc-row td{color:var(--ink-soft);font-style:italic}
.jx tfoot td{color:var(--faint);font-size:.82rem;border-bottom:0;padding-top:.5rem;font-style:italic}
.jx-toggle{display:inline-flex;border:1px solid var(--rule);border-radius:2px;overflow:hidden;font-size:.82rem;margin:0 0 .9rem}
.jx-toggle label{padding:.3rem .7rem;cursor:pointer;color:var(--ink-soft);background:var(--field)}
.jx-mx{position:relative}
/* Only the two radios are hidden -- the range slider is an input in the same
   block and vanished with them when this said `input`. The selected label is
   reached through the controls row the toggle now sits in: the radios' sibling
   is .jx-controls, not .jx-toggle. */
.jx-mx > input[type=radio]{position:absolute;opacity:0;width:0;height:0}
.jx-mx #mx-judg:checked~.jx-controls label[for=mx-judg],.jx-mx #mx-full:checked~.jx-controls label[for=mx-full]{background:var(--ink);color:var(--paper)}
.jx-mx #mx-judg:focus-visible~.jx-controls label[for=mx-judg],.jx-mx #mx-full:focus-visible~.jx-controls label[for=mx-full]{outline:2px solid var(--accent)}
.jx-mx .m{display:none}
.jx-mx #mx-judg:checked~.m.judg,.jx-mx #mx-full:checked~.m.full{display:block}
/* The agreement network (justices/network.js draws into #jx-svg) and the
   matrix tables beneath it, which the same radios switch. */
.jx-controls{display:flex;flex-wrap:wrap;gap:.8rem 1.6rem;align-items:center;font-size:.9rem;margin:0 0 1rem}
.jx-controls label.thr{display:inline-flex;align-items:center;gap:.5rem;color:var(--ink-soft)}
.jx-controls input[type=range]{accent-color:var(--accent);width:11rem}
.jx-thrv{font-variant-numeric:tabular-nums;min-width:3ch;display:inline-block;color:var(--ink)}
.jx-stage{position:relative;background:var(--panel);border:1px solid var(--rule)}
.jx-stage svg{display:block;width:100%;height:auto;max-width:100%}
.jx-edge{stroke:var(--accent);stroke-linecap:round;transition:opacity .2s}
.jx-leader{stroke:var(--faint);stroke-width:1}
.jx-node circle{fill:var(--panel);stroke:var(--ink);stroke-width:2;cursor:default}
.jx-node .ring{fill:none}.jx-node image{pointer-events:none}
.jx-node text{font-family:'Newsreader',serif;font-weight:600;font-size:14px;fill:var(--ink);pointer-events:none}
.jx-node .sub{font-weight:400;font-size:11px;fill:var(--faint);font-variant-numeric:tabular-nums}
.jx-node.dim circle{stroke:var(--rule)}.jx-node.dim text{fill:var(--faint)}
.jx-node.hot circle{stroke:var(--accent);stroke-width:3}
.jx-edge.dim{opacity:.06!important}.jx-edge.hot{opacity:1!important}
.jx-tip{position:absolute;pointer-events:none;background:var(--ink);color:var(--paper);font-size:.8rem;padding:.25rem .55rem;border-radius:2px;white-space:nowrap;transform:translate(-50%,-130%);opacity:0;transition:opacity .12s}
.jx-tip.on{opacity:1}
.jx-legend{display:flex;flex-wrap:wrap;gap:.6rem 1.4rem;align-items:center;font-size:.84rem;color:var(--faint);margin:.7rem 0 0}
.jx-legend i{display:inline-block;vertical-align:middle;margin-right:.4rem;border-radius:2px;background:var(--accent)}
.jx-legend .n{display:inline-block;width:.9rem;height:.9rem;border:2px solid var(--ink);border-radius:50%;background:var(--panel);vertical-align:middle;margin-right:.4rem}
.jx-mwrap{overflow-x:auto;margin-top:1.2rem}
.jx-mtable th{font-size:.66rem;letter-spacing:.03em;padding:.3rem .35rem}
.jx-mtable td{padding:.3rem .4rem;font-size:.88rem}
.jx-mtable td.self{color:var(--rule);text-align:center}
.jx-mtable td.na{color:var(--faint);text-align:center}
.jx-mtable th.n,.jx-mtable td.n{text-align:right}
.jx-mtable tbody tr:nth-child(even){background:transparent}
@media (prefers-reduced-motion:reduce){.jx-edge,.jx-tip{transition:none}}
.jx-tiles{display:grid;grid-template-columns:repeat(auto-fit,minmax(11rem,1fr));gap:1rem;margin:0 0 1.2rem}
.jx-tile{border-top:2px solid var(--ink);padding-top:.5rem}
.jx-tile .big{font-family:'Fraunces',Georgia,serif;font-size:2rem;font-weight:500;line-height:1;letter-spacing:-.02em;font-variant-numeric:tabular-nums}
.jx-tile .big small{font-size:1rem;color:var(--faint);letter-spacing:0;margin-left:.15rem}
.jx-tile .l{font-size:.78rem;letter-spacing:.06em;text-transform:uppercase;color:var(--ink-soft);margin:.3rem 0 .15rem}
.jx-tile .s{font-size:.84rem;color:var(--faint)}
.jx-splits{display:grid;grid-template-columns:3.2rem 1fr 2.4rem;gap:.35rem .6rem;align-items:center;font-size:.92rem;max-width:34rem}
.jx-splits .b{height:.9rem;background:var(--ink);border-radius:2px}.jx-splits .b.u{background:var(--op-court)}
.jx-splits .n{font-variant-numeric:tabular-nums;color:var(--ink-soft);font-size:.88rem}
.jx-lineup{font-size:.95rem}.jx-lineup .side{display:inline-block;padding:.1rem .45rem;border-radius:2px;margin:.15rem .3rem .15rem 0;background:var(--stripe);border:1px solid var(--rule)}
.jx-chip{display:inline-block;font-size:.72rem;letter-spacing:.05em;text-transform:uppercase;padding:.1rem .45rem;border-radius:2px;color:#fff;white-space:nowrap}
.jx-chip.court{background:var(--op-court)}.jx-chip.conc{background:var(--op-conc)}.jx-chip.judg{background:var(--op-judg)}.jx-chip.diss{background:var(--op-diss)}
.jx-chip.part{background-image:repeating-linear-gradient(135deg,transparent 0 3px,rgba(@paper:rgb@,.35) 3px 5px)}
.jx .cap{font-weight:500}.jx .dk{display:block;color:var(--faint);font-size:.78rem;font-variant-numeric:tabular-nums}
.jx details{border-top:1px solid var(--rule);padding:.5rem 0}
.jx summary{cursor:pointer;font-weight:600;display:flex;gap:.6rem;align-items:baseline}
.jx summary .cnt{font-weight:400;color:var(--faint);font-size:.86rem;font-variant-numeric:tabular-nums}
.jx .pend{color:var(--faint);font-style:italic;font-size:.82rem}
.jx-defs{columns:2;column-gap:2.4rem;font-size:.92rem}.jx-defs p{break-inside:avoid;margin:0 0 .7rem}
@media (max-width:700px){.jx-defs{columns:1}}
.jx-terms{display:flex;flex-wrap:wrap;gap:.35rem;margin:0 0 1.2rem;padding:0;list-style:none;font-variant-numeric:tabular-nums;font-size:.9rem}
.jx-terms a{display:inline-block;padding:.2rem .55rem;border:1px solid var(--rule);border-radius:2px;color:var(--ink-soft);text-decoration:none}
.jx-terms a[aria-current]{background:var(--ink);border-color:var(--ink);color:var(--paper)}
"))

# The agreement ramp: paper -> accent, the grant ramp, over 50%..100%.
# The matrix as a table: seniority order both ways, each cell the rounded
# share with a wash of the accent behind it that deepens from 40% to 100%
# (an accent alpha, from the palette -- no colour is written here). This is
# the form the OT25 network mockup used, and it reads better than the grid of
# squares it replaces: the numbers line up, the row names are a column.
.jx_matrix_table <- function(M, N, court, cls, note) {
  J <- court$name; k <- length(J); acc <- pal_rgb("accent")
  head <- paste0("<tr><th></th>", paste(sprintf("<th class='n'>%s</th>", J), collapse = ""), "</tr>")
  rows <- vapply(seq_len(k), function(a) {
    cells <- vapply(seq_len(k), function(b) {
      if (a == b) return("<td class='self'>&middot;</td>")
      v <- M[a, b]; n <- N[a, b]
      if (is.na(v) || n == 0) return("<td class='na' title='no shared decisions with a parsed lineup'>&middot;</td>")
      t <- max(0, min(1, (v - 0.4) / 0.6))
      sprintf("<td class='n' style='background:rgba(%s,%.2f)' title='%s and %s: %d of %d decisions both took part in'>%d</td>",
              acc, 0.04 + 0.42 * t, J[a], J[b], round(n * v), n, round(100 * v))
    }, character(1))
    paste0("<tr><td>", J[a], "</td>", paste(cells, collapse = ""), "</tr>")
  }, character(1))
  paste0("<div class='m ", cls, "'><div class='jx-mwrap'><table class='jx-mtable' aria-label='Pairwise agreement'><thead>", head,
         "</thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div><p class='pend'>", note, "</p></div>")
}

# The portraits the network draws into its circles: data/portraits/<name>.jpg
# with the face geometry OpenCV found (data/portraits/crops.json), copied to
# justices/portraits/ by render_justices(). Official Court photographs, U.S.
# government works in the public domain. A Justice with no portrait on file
# gets an empty circle, never an error.
PORTRAITS_SRC <- "data/portraits"
.jx_portraits <- function(court) {
  cj <- file.path(PORTRAITS_SRC, "crops.json")
  crops <- if (file.exists(cj)) fromJSON(cj, simplifyVector = FALSE) else list()
  out <- list()
  for (nm in court$name) {
    c <- crops[[nm]]
    if (is.null(c)) next
    out[[nm]] <- list(url = paste0("portraits/", c$file), iw = c$iw, ih = c$ih, fw = c$fw, nx = c$nx, ny = c$ny)
  }
  out
}

#' Write justices/otYYYY.html for one Term from term_stats() output.
#' `terms_all` lists every Term with a page, for the switcher.
render_justices_term <- function(st, site_dir, terms_all) {
  term <- st$term; yyyy <- 2000L + as.integer(term)
  court <- st$court; W <- st$written
  out_dir <- file.path(site_dir, JUSTICES_DIR); dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  path <- paste0("/", JUSTICES_DIR, "/ot", yyyy, ".html")
  title <- paste0("October Term ", yyyy, " — The Justices")
  dek <- paste0(st$n_dec, " argued case", if (st$n_dec == 1) "" else "s",
                " decided by written opinion. Who wrote, who joined whom, and how often the Court split.")
  max_tot <- max(W$total, 1L)
  kinds <- c(court = "Opinion of the Court", conc = "Concurrence", judg = "Concurring in the judgment", diss = "Dissent")

  # Term switcher.
  sw <- paste0("<ul class='jx-terms' aria-label='Terms'>",
               paste(vapply(sort(terms_all), function(t) {
                 y <- 2000L + as.integer(t)
                 sprintf("<li><a href='ot%d.html'%s>OT%d</a></li>", y, if (t == term) " aria-current='page'" else "", y)
               }, character(1)), collapse = ""), "</ul>")
  # A portrait beside each name, cropped to the face from the geometry in
  # data/portraits/crops.json (the same file the network uses): a square
  # window two and a half face-widths wide, centred a little above the face
  # centre so the hair is in and the collar is not, clamped to the image, and
  # expressed as a CSS background so no image is re-encoded. A Justice with no
  # portrait keeps the slot empty and the columns aligned.
  pts <- .jx_portraits(court); PT <- 44   # thumbnail size in CSS px
  thumb <- function(nm) {
    p <- pts[[nm]]; if (is.null(p)) return("<span class='pt none'></span>")
    side <- min(max(p$fw * 2.1, 70), p$iw, p$ih)
    x0 <- min(max(p$nx - side / 2, 0), p$iw - side)
    y0 <- min(max(p$ny - side * 0.5, 0), p$ih - side)
    k <- PT / side
    sprintf("<span class='pt' style='background-image:url(%s);background-size:%.1fpx %.1fpx;background-position:%.1fpx %.1fpx'></span>",
            p$url, p$iw * k, p$ih * k, -x0 * k, -y0 * k)
  }
  court_html <- paste0("<div class='jx'><h3>The Court, in order of seniority</h3><ul class='jx-court'>",
                       paste(sprintf("<li>%s<span class='nm'>%s</span><span class='since'>%s</span></li>",
                                     vapply(court$name, thumb, character(1)), court$label,
                                     ifelse(court$cj, paste0("Chief Justice since ", format(court$from, "%Y")),
                                            paste0("since ", format(court$from, "%Y")))), collapse = ""), "</ul></div>")

  # Panel 1.
  bars <- character()
  for (i in seq_len(nrow(W))) {
    w <- W[i, ]
    segs <- character()
    for (kd in names(kinds)) {
      n_all <- w[[kd]]
      n_part <- if (paste0(kd, "_part") %in% names(w)) w[[paste0(kd, "_part")]] else 0L   # the Court's opinion is never partial
      n_full <- n_all - n_part
      if (n_full > 0) segs <- c(segs, sprintf("<div class='jx-seg %s' style='flex:%d 0 0' title='%s: %d'></div>", kd, n_full, kinds[[kd]], n_full))
      if (n_part > 0) segs <- c(segs, sprintf("<div class='jx-seg %s part' style='flex:%d 0 0' title='%s, in part: %d'></div>", kd, n_part, kinds[[kd]], n_part))
    }
    bars <- c(bars, sprintf("<div class='lab'>%s</div><div class='jx-bar' style='width:%.1f%%'>%s</div><div class='tot'>%d</div>",
                            court$label[i], 100 * w$total / max_tot, paste(segs, collapse = ""), w$total))
  }
  ticks <- pretty(c(0, max_tot), n = 3); ticks <- ticks[ticks <= max_tot]
  axis <- paste0("<div></div><div class='jx-axis'>", paste(sprintf("<span>%s</span>", c(head(ticks, -1), paste(tail(ticks, 1), "opinions"))), collapse = ""), "</div>")
  rows1 <- paste(vapply(seq_len(nrow(W)), function(i) { w <- W[i, ]
    pc <- function(n, p) paste0(n, if (p > 0) sprintf(" <span class='pc'>(%d in part)</span>", p) else "")
    sprintf("<tr><td>%s</td><td class='n'>%d</td><td class='n'>%s</td><td class='n'>%s</td><td class='n'>%s</td><td class='n'><b>%d</b></td><td class='n'>%s</td></tr>",
            court$label[i], w$court, pc(w$conc, w$conc_part), pc(w$judg, w$judg_part), pc(w$diss, w$diss_part), w$total,
            if (st$n_signed) .jx_pct(w$court / st$n_signed) else "")
  }, character(1)), collapse = "")
  n_pc <- st$n_dec - st$n_signed
  panel1 <- paste0(
    "<section class='jx' id='written'><h2>Opinions written <span class='tag'>Granted &amp; Noted List</span></h2>",
    "<p class='note'>One bar per Justice, in seniority order, so the chart reads the same every Term. A mixed writing (concurring in part, dissenting in part) counts once under its lead label; the striped end of a segment is the share that was partial. Hover a segment for its count.</p>",
    "<ul class='jx-legend'>", paste(sprintf("<li><i style='background:var(--op-%s)'></i>%s</li>", names(kinds), kinds), collapse = ""),
    "<li class='part'><i style='background:var(--op-conc)'></i>… in part</li></ul>",
    "<div class='jx-bars'>", paste(bars, collapse = ""), axis, "</div>",
    "<h3>The counts</h3><div class='jx-tw'><table><thead><tr><th>Justice</th><th class='n'>Court</th><th class='n'>Conc.</th><th class='n'>Conc. in judgment</th><th class='n'>Dissent</th><th class='n'>Total</th><th class='n'>Share of majorities</th></tr></thead>",
    "<tbody>", rows1, "</tbody><tfoot><tr><td colspan='7'>",
    if (n_pc) sprintf("%d %s per curiam. ", n_pc, if (n_pc == 1) "opinion was" else "opinions were") else "",
    sprintf("Share of majorities is out of the %d signed opinions of the Court. Counts are of argued cases on the Granted &amp; Noted List; writings on the order lists are not included.", st$n_signed),
    "</td></tr></tfoot></table></div></section>")

  # Panel 2.
  cov <- sprintf("%d of %d decisions", st$n_lineup, st$n_dec)
  # The network needs a script; the tables do not. The radios sit first inside
  # .jx-mx so the CSS ~ combinator switches the labels and the tables, and
  # network.js listens to the same radios so the picture follows. Without
  # script the tables still show and the stage says why it is empty.
  by_name <- function(v) { m <- match(court$name, st$by_j$name); ifelse(is.na(m), NA, st$by_j$in_maj[m]) }
  net <- list(names = court$label, short = court$name,
              judg = unname(round(st$agree_j, 3)), full = unname(round(st$agree_f, 3)), n = unname(st$n_pair),
              in_maj = round(by_name(), 3), portraits = .jx_portraits(court), accent_rgb = pal_rgb("accent"))
  net_json <- toJSON(net, auto_unbox = TRUE, digits = NA, na = "null", matrix = "rowmajor", null = "null")
  panel2 <- paste0(
    "<section class='jx' id='agree'><h2>Who agreed with whom <span class='tag'>Lineups parsed for ", cov, "</span></h2>",
    "<p class='note'>Every pair of Justices, as a network. A line joins two who ended on the same side of the judgment at least as often as the threshold; the thicker and darker, the more often. Distance is the layout's reading of the whole matrix: pairs who agree sit close. Circle size is the share of decisions in the majority. The stricter reading counts only decisions where both joined the opinion of the Court in full. Hover a Justice or a line for the numbers.</p>",
    if (st$n_lineup == 0) "<p class='pend'>No lineups parsed for this Term yet.</p>" else paste0(
      "<div class='jx-mx'>",
      "<input type='radio' name='mx' id='mx-judg' checked><input type='radio' name='mx' id='mx-full'>",
      "<div class='jx-controls'><div class='jx-toggle' role='radiogroup' aria-label='Agreement measure'>",
      "<label for='mx-judg'>Same side of the judgment</label>",
      "<label for='mx-full'>Joined the same opinion in full</label></div>",
      "<label class='thr' for='jx-thr'>Show pairs agreeing at least <span class='jx-thrv' id='jx-thrv'>&ndash;</span>% <input type='range' id='jx-thr' min='40' max='95' step='1' value='50'></label></div>",
      "<div class='jx-stage' id='jx-stage'><svg id='jx-svg' viewBox='0 0 860 560' role='img' aria-label='Agreement network of the Justices'></svg><div class='jx-tip' id='jx-tip'></div>",
      "<noscript><p class='pend' style='padding:1rem'>The network is drawn by script; the matrix below carries the same numbers.</p></noscript></div>",
      "<p class='jx-legend'><span><i style='width:2.2rem;height:2px'></i>weaker</span><span><i style='width:2.2rem;height:7px'></i>stronger agreement</span><span><span class='n'></span>circle size: share of decisions in the majority</span></p>",
      if (length(net$portraits)) "<p class='pend'>Portraits: the Justices&rsquo; official photographs, Collection of the Supreme Court of the United States, public domain as works of the U.S. government, via Wikimedia Commons.</p>" else "",
      .jx_matrix_table(st$agree_j, st$n_pair, court, "judg",
        "Share of decisions both took part in where both were on the same side of the judgment. A Justice who concurred in part and dissented in part is on neither side for that decision."),
      .jx_matrix_table(st$agree_f, st$n_pair, court, "full",
        "Share of decisions both took part in where both joined the opinion of the Court without a qualifier; a plurality or a split opinion counts for no one."),
      "</div>",
      "<script type='application/json' id='jx-net'>", net_json, "</script>",
      "<script src='/justices/network.js' defer></script>"),
    "</section>")

  # Panel 3.
  bj <- st$by_j |> left_join(st$lone |> rename(name = min_set), by = "name") |> mutate(lone = coalesce(lone, 0L))
  top_maj <- if (nrow(bj)) bj |> arrange(desc(in_maj)) |> slice_head(n = 1) else NULL
  top_lone <- if (nrow(bj)) bj |> arrange(desc(lone)) |> slice_head(n = 1) else NULL
  solo_d <- function(nm) { r <- st$solo_w |> filter(name == nm, kind == "diss"); if (nrow(r)) r$n else 0L }
  solo_c <- function(nm) { r <- st$solo_w |> filter(name == nm, kind %in% c("conc", "judg")); sum(r$n) }
  lab_of <- function(nm) court$label[match(nm, court$name)]
  tiles <- paste0(
    sprintf("<div class='jx-tile'><div class='big'>%d<small>of %d</small></div><div class='l'>Unanimous</div><div class='s'>%d fully, %d in the judgment only</div></div>",
            st$n_unan + st$n_unan_j, st$n_dec, st$n_unan, st$n_unan_j),
    if (st$n_lineup) sprintf("<div class='jx-tile'><div class='big'>%d</div><div class='l'>Decided 6–3</div><div class='s'>%s</div></div>",
                             st$n_63, if (nrow(st$top_lineup)) sprintf("%d of them on the most frequent lineup", st$top_lineup$top)
                                      else if (st$n_63 == 1L) "one decision" else "") else "",
    if (!is.null(top_lone) && top_lone$lone > 0) sprintf("<div class='jx-tile'><div class='big'>%s</div><div class='l'>Most often alone</div><div class='s'>%d lone dissent%s, %d solo concurrence%s</div></div>",
                                                        lab_of(top_lone$name), top_lone$lone, if (top_lone$lone == 1) "" else "s", solo_c(top_lone$name), if (solo_c(top_lone$name) == 1) "" else "s") else "",
    if (!is.null(top_maj)) sprintf("<div class='jx-tile'><div class='big'>%s</div><div class='l'>Most often in the majority</div><div class='s'>%s of the decisions with a parsed lineup</div></div>",
                                   lab_of(top_maj$name), .jx_pct(top_maj$in_maj)) else "")
  sp <- st$splits
  splits <- if (nrow(sp)) paste0("<h3>Vote splits</h3><div class='jx-splits'>",
    paste(vapply(seq_len(nrow(sp)), function(i) sprintf("<div>%d–%d</div><div class='b%s' style='width:%.1f%%' title='%d decision%s'></div><div class='n'>%d</div>",
      sp$n_maj[i], sp$n_min[i], if (sp$n_min[i] == 0) " u" else "", 100 * sp$n[i] / max(sp$n), sp$n[i], if (sp$n[i] == 1) "" else "s", sp$n[i]), character(1)), collapse = ""),
    "</div><p class='pend'>From the ", st$n_lineup, " decisions with a parsed lineup. A Justice concurring in part and dissenting in part counts on the minority side.</p>") else ""
  # One line per close split that occurred at least twice: 6-3 and 5-4, and
  # 5-3 on an eight-member Court. Each names its own split, so a Term with one
  # 6-3 and six 5-3s says so rather than calling all seven six-to-three.
  side <- function(s) paste(sprintf("<span class='side'>%s</span>", lab_of(str_split(s, "\\|")[[1]])), collapse = "")
  words <- c("3" = "three", "4" = "four", "5" = "five", "6" = "six")
  lineup <- if (nrow(st$lineups)) paste(vapply(seq_len(nrow(st$lineups)), function(i) {
    tl <- st$lineups[i, ]
    paste0(sprintf("<h3>Most frequent %d–%d lineup</h3><p class='jx-lineup'>", tl$n_maj, tl$n_min),
           side(tl$maj_set), " over ", side(tl$min_set),
           sprintf(" <span class='pend'>· %d of %d %s-to-%s decisions</span></p>", tl$top, tl$n,
                   words[[as.character(tl$n_maj)]], words[[as.character(tl$n_min)]]))
  }, character(1)), collapse = "") else ""
  panel3 <- paste0("<section class='jx' id='shape'><h2>The shape of the Term</h2>",
                   "<p class='note'>Unanimity comes from the Court's own flags on the Granted &amp; Noted List and covers every decision. The splits and lineups come from the parsed syllabi and cover ", cov, ".</p>",
                   "<div class='jx-tiles'>", tiles, "</div>", splits, lineup, "</section>")

  # Panel 4.
  wl <- st$writings
  chip <- function(kind, partial, code) sprintf("<span class='jx-chip %s%s' title='%s'>%s</span>", kind, if (partial) " part" else "", .jx_esc(code),
                                                c(court = "Court", conc = "Concurring", judg = "Conc. in judgment", diss = "Dissenting")[[kind]])
  lists <- paste(vapply(seq_len(nrow(court)), function(i) {
    nm <- court$name[i]; rows <- if (is.null(wl)) NULL else wl |> filter(name == nm)
    n <- if (is.null(rows)) 0L else nrow(rows)
    body <- if (!n) "<p class='pend'>No writings on the list for this Term.</p>" else paste0(
      "<div class='jx-tw'><table><thead><tr><th>Decided</th><th>Writing</th><th>Case</th><th>Opinion of the Court by</th><th>Joined by</th><th>Result</th></tr></thead><tbody>",
      paste(vapply(seq_len(n), function(r) sprintf("<tr><td>%s</td><td>%s</td><td><a class='cap' href='/cases/%s.html'>%s</a><span class='dk'>No. %s</span></td><td>%s</td><td>%s</td><td>%s</td></tr>",
        .jx_date(rows$decided[r]), chip(rows$kind[r], rows$partial[r], rows$code[r]), rows$dkt[r], .jx_esc(rows$caption[r]), rows$dkt[r],
        if (rows$author[r] == nm) "<span class='pc'>this opinion</span>" else .jx_esc(lab_of(rows$author[r]) %||% rows$author[r]),
        if (is.na(rows$joined[r])) "<span class='pend'>lineup not parsed</span>" else .jx_esc(rows$joined[r]),
        .jx_esc(rows$result[r])), character(1)), collapse = ""), "</tbody></table></div>")
    sprintf("<details%s><summary>%s <span class='cnt'>%d writing%s</span></summary>%s</details>", if (i == 1) " open" else "", court$label[i], n, if (n == 1) "" else "s", body)
  }, character(1)), collapse = "")
  panel4 <- paste0("<section class='jx' id='writings'><h2>The writings <span class='tag'>Granted &amp; Noted List</span></h2>",
                   "<p class='note'>Every opinion each Justice wrote this Term, newest first, each linking to the case's docket page. A count is a claim; this list is the evidence.</p>", lists, "</section>")

  defs <- paste0("<section class='jx' id='defs'><h2>How these are counted</h2><div class='jx-defs'>",
    "<p><b>A decision</b> is one written opinion of the Court in an argued case. Consolidated dockets argued together count once.</p>",
    "<p><b>A mixed writing</b> counts once, under its lead label, and is marked partial. A concurrence in part and dissent in part is never two writings.</p>",
    "<p><b>A plurality</b> credits its author with the lead opinion; there is no opinion of the Court to have joined in full.</p>",
    "<p><b>Partial joins</b> (“joined as to Part II–B”) count as agreement on the judgment but not on the full opinion.</p>",
    "<p><b>Denominators</b> are per pair: only decisions both Justices took part in, and in which neither was on both sides.</p>",
    "<p><b>Merits only.</b> Argued cases on the Granted &amp; Noted List. Dissents from denial and emergency-docket writings are not counted here.</p>",
    "</div></section>")

  # Panel 5: how they write. Word counts by kind of opinion, and the style
  # measures pooled over every writing (R/opinion_text.R). Rendered only when
  # the text cache covers this Term's decisions; the tag says how many.
  panel_text <- ""
  tx <- st$text
  if (!is.null(tx) && !is.null(tx$by_justice) && nrow(tx$by_justice)) {
    B <- tx$by_justice
    fmt_n <- function(n, med) if (is.na(n) || n == 0) "<span class='dim'>—</span>" else
      sprintf("%d <span class='pc'>· %s</span>", as.integer(n), format(round(med), big.mark = ","))
    f1 <- function(x, d = 1) if (is.na(x)) "<span class='dim'>—</span>" else formatC(x, format = "f", digits = d)
    rows5 <- paste(vapply(seq_len(nrow(B)), function(i) { b <- B[i, ]
      sprintf("<tr><td>%s</td><td class='n'>%s</td><td class='n'>%s</td><td class='n'>%s</td><td class='n'>%s</td><td class='n'>%s</td><td class='n'>%s</td><td class='n'>%s</td><td class='n'>%s</td><td class='n'>%s</td></tr>",
              b$label, fmt_n(b$court_n, b$court_med), fmt_n(b$conc_n, b$conc_med), fmt_n(b$diss_n, b$diss_med),
              f1(b$sent_mean), if (is.na(b$sent_over40)) "<span class='dim'>—</span>" else .jx_pct(b$sent_over40),
              f1(b$fk_grade), f1(b$cites_per_k), if (is.na(b$fn_share)) "<span class='dim'>—</span>" else .jx_pct(b$fn_share),
              f1(b$contractions_per_k, 1))
    }, character(1)), collapse = "")
    pcrow <- if (!is.null(tx$per_curiam)) { p <- tx$per_curiam
      sprintf("<tr class='pc-row'><td>Per curiam</td><td class='n'>%d <span class='pc'>· %s</span></td><td class='n'><span class='dim'>—</span></td><td class='n'><span class='dim'>—</span></td><td class='n'>%s</td><td class='n'>%s</td><td class='n'>%s</td><td class='n'>%s</td><td class='n'>%s</td><td class='n'>%s</td></tr>",
              as.integer(p$n), format(round(p$words_total / p$n), big.mark = ","), f1(p$sent_mean), .jx_pct(p$sent_over40), f1(p$fk_grade), f1(p$cites_per_k),
              if (is.na(p$fn_share)) "<span class='dim'>—</span>" else .jx_pct(p$fn_share), f1(p$contractions_per_k))
    } else ""
    panel_text <- paste0(
      "<section class='jx' id='write'><h2>How they write <span class='tag'>Measured from ", tx$n_dec, " of ", st$n_dec, " decisions</span></h2>",
      "<p class='note'>Every opinion of the Term, read from the slip-opinion PDF and measured by writing: the Court's opinion, each concurrence and each dissent, body text only (footnotes counted apart, citations counted once each). Each count is followed by the median length in words. The style measures pool a Justice's writings of every kind, weighted by length.</p>",
      "<div class='jx-tw'><table><thead><tr><th>Justice</th><th class='n'>Court<br><span class='pc'>n · median words</span></th><th class='n'>Concurrences<br><span class='pc'>n · median words</span></th><th class='n'>Dissents<br><span class='pc'>n · median words</span></th>",
      "<th class='n'>Words per sentence</th><th class='n'>Sentences over 40 words</th><th class='n'>Grade level</th><th class='n'>Citations per 1,000 words</th><th class='n'>Footnote share</th><th class='n'>Contractions per 1,000 words</th></tr></thead>",
      "<tbody>", rows5, pcrow, "</tbody><tfoot><tr><td colspan='10'>",
      sprintf("Across the Term's %d writings: %.1f words per sentence, grade level %.1f, %.1f citations per 1,000 words. ", tx$n_writings, tx$court_sent, tx$court_fk, tx$court_cites),
      "Grade level is Flesch–Kincaid, computed after citations are masked; on legal prose it is an index for comparing Justices, not a reading age. A concurrence in the judgment is counted with the concurrences; a mixed writing with the dissents.",
      "</td></tr></tfoot></table></div></section>")
  }

  crumb <- list(href = paste0("/", JUSTICES_DIR, "/"), label = "Justices")
  html <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n",
    page_head(paste0(title, " — Supreme Court Report"),
              site_breadcrumb_jsonld(paste0("October Term ", yyyy), crumb),
              extra_css = JUSTICES_CSS, description = dek, path = path, og_type = "article",
              extra_head = paste0("<meta name='jtv' content='", JUSTICES_TEMPLATE_VERSION, "'>")),
    "<body>", site_masthead(active = paste0("/", JUSTICES_DIR, "/")),
    "<main class='wrap' id='main'>",
    site_breadcrumb(paste0("October Term ", yyyy), crumb),
    "<p class='kicker'>Supreme Court of the United States · The Justices</p>",
    "<h1>October Term ", yyyy, "</h1><p class='dek'>", dek, "</p>", sw, "<hr class='brule'>",
    court_html, panel1, panel_text, panel2, panel3, panel4, defs,
    "<p class='back'><a href='index.html'>&larr; All Terms</a> · <a href='/arguments/arg_", yyyy, ".html'>This Term's oral arguments &rarr;</a></p>",
    "</main></body>\n</html>\n")
  out <- file.path(out_dir, paste0("ot", yyyy, ".html"))
  writeLines(enc2utf8(smarten_html(html)), out, useBytes = TRUE)
  invisible(out)
}

#' Render every Term page and the section index. Returns the Terms rendered.
render_justices <- function(site_dir, gn, lineups, captions = NULL, texts = NULL) {
  # The section's static assets, re-asserted on every render like
  # analytics.js: the network script and the portraits with their face
  # geometry. Copied whole so a page never references a file that is not there.
  out_dir <- file.path(site_dir, JUSTICES_DIR); dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  if (file.exists("justices_network.js")) file.copy("justices_network.js", file.path(out_dir, "network.js"), overwrite = TRUE)
  if (dir.exists(PORTRAITS_SRC)) {
    pd <- file.path(out_dir, "portraits"); dir.create(pd, showWarnings = FALSE)
    file.copy(list.files(PORTRAITS_SRC, full.names = TRUE), pd, overwrite = TRUE)
  }
  terms <- sort(unique(gn$term[!is.na(gn$decided)]))
  stats <- lapply(terms, function(t) term_stats(t, gn, lineups, captions, texts))
  keep <- !vapply(stats, is.null, logical(1)); terms <- terms[keep]; stats <- stats[keep]
  for (st in stats) render_justices_term(st, site_dir, terms)
  items <- lapply(rev(seq_along(terms)), function(i) {
    st <- stats[[i]]; y <- 2000L + as.integer(terms[i])
    list(href = paste0("ot", y, ".html"), label = paste0("October Term ", y),
         meta = sprintf("%d decisions · lineups for %d", st$n_dec, st$n_lineup))
  })
  styled_index_page(
    file.path(site_dir, JUSTICES_DIR, "index.html"),
    title = "The Justices — Supreme Court Report", heading = "The Justices",
    kicker = "Supreme Court of the United States",
    dek = "Each Term's opinions by author and kind, who joined whom, and how often the Court split. From the Court's Granted &amp; Noted List and the syllabus of every slip opinion.",
    items = items, new_tab = FALSE, active = paste0("/", JUSTICES_DIR, "/"),
    crumb = list(label = "Justices"), path = paste0("/", JUSTICES_DIR, "/"))
  invisible(terms)
}
