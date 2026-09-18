# opinion_text.R ---------------------------------------------------------------
# Text measures of each opinion in a slip opinion: word counts and a small set
# of stylistic measures, per writing (the Court's opinion, each concurrence and
# dissent), so the Justices pages can say how long and how plainly each Justice
# writes each kind of opinion in a Term.
#
# SEGMENTATION. A slip opinion is one PDF: syllabus, the Court's opinion, then
# each separate writing, each with its own page run. The running head on every
# page names the section -- "Syllabus", "Opinion of the Court", "Per Curiam",
# "SOTOMAYOR, J., dissenting", "Opinion of KAGAN, J." (a plurality or a partial
# opinion), "ROBERTS, C. J., concurring in the judgment" -- so a page is
# assigned to a writing by its head, and a writing's text is its pages
# concatenated. Measured on OT25 (2026-09-18): the head is within the first
# three non-empty lines of every page, since the even-page caption can wrap.
#
# WHAT IS MEASURED, and what is deliberately not. The body only: running heads,
# page numbers and the footnote block below the rule are removed before
# counting, and citations are masked before sentence and word measures so that
# "Loper Bright Enterprises v. Raimondo, 603 U. S. 369, 412 (2024)" counts as
# one token, not nine, and its periods do not end sentences. Readability
# scores are reported because readers expect them, but the measures that
# separate Justices on this corpus are sentence length and its spread,
# citation density, footnote share, and a few register markers (first person
# plural, contractions, rhetorical questions, hedges and boosters). Nothing
# here needs a model or a network; it is regexes over pdftools text.

suppressPackageStartupMessages({ library(stringr); library(tibble); library(dplyr) })

OPINION_TEXT_VERSION <- "t1"

# ---- segmentation --------------------------------------------------------------

# The section a page belongs to, from its running head; NA when no head is
# recognised (a cover page, an appendix).
.page_section <- function(page) {
  ls <- str_squish(str_split(page, "\n")[[1]]); ls <- head(ls[nzchar(ls)], 4L)
  h <- paste(ls, collapse = " | ")
  # Order matters: "Opinion of the Court" before "Opinion of X, J."
  # A slip sets the head in small caps ("SOTOMAYOR, J., dissenting"); a
  # preliminary print or bound volume in mixed case ("Gorsuch, J.,
  # concurring"), with "Counsel" and "Syllabus" sections of its own before
  # the opinion. Both are read; the name is lower-cased either way.
  if (str_detect(h, "\\bSyllabus\\b")) return("syllabus")
  if (str_detect(h, "\\bCounsel\\b\\s*\\|") || str_detect(h, "\\|\\s*Counsel\\s*(\\||$)")) return("counsel")
  if (str_detect(h, "Opinion of the Court")) return("court")
  if (str_detect(h, "\\bPer Curiam\\b")) return("percuriam")
  if (str_detect(h, "Appendix to opinion|\\bDecree\\b")) return("appendix")
  nm <- "([A-Z][A-Za-z]+)"
  m <- str_match(h, paste0(nm, ", (C\\. )?J\\., (concurring in the judgment in part and dissenting in part|concurring in part and dissenting in part|concurring in the judgment|concurring in part|dissenting in part|concurring|dissenting)"))
  if (!is.na(m[1, 1])) return(paste0(tolower(m[1, 2]), ":", m[1, 4]))
  m <- str_match(h, paste0("Opinion of ", nm, ", (C\\. )?J\\."))
  if (!is.na(m[1, 1])) return(paste0(tolower(m[1, 2]), ":opinion"))
  NA_character_
}

# The body of a page: running head (first lines up to the section label) and
# the footnote block (everything below the first rule of dashes) removed.
# Returns list(body, notes): the footnotes are kept apart so they can be
# counted and their words attributed without inflating sentence measures.
.page_body <- function(page) {
  ls <- str_split(page, "\n")[[1]]
  # Drop the head: the first three non-empty lines when they carry the cite,
  # the page number/caption and the section label.
  nz <- which(nzchar(str_squish(ls)))
  drop <- integer()
  for (i in head(nz, 3L)) {
    l <- str_squish(ls[i])
    if (str_detect(l, "^Cite as:|^\\(Slip Opinion\\)|^\\d+\\s+[A-Z]|^[A-Z][A-Z .,'&-]+ v\\. |^[A-Z][A-Z .,'&-]+$|^Syllabus$|^Opinion of|^Per Curiam$|, (C\\. )?J\\., (concurring|dissenting)|^SUPREME COURT OF THE UNITED STATES$"))
      drop <- c(drop, i) else break
  }
  if (length(drop)) ls <- ls[-drop]
  # The footnote rule is a run of em dashes. The underscore rules that box
  # the docket number on a writing's first page ("_________________") are
  # not it -- treating them as the rule emptied Barrett's one-page dissent in
  # Chatrie v. United States into the notes.
  # A preliminary print's watermark sits inside the text as its own line.
  ls <- ls[!str_detect(ls, regex("^\\s*Page Proof Pending Publication\\s*$", ignore_case = TRUE))]
  rule <- which(str_detect(ls, "^\\s*[-—–]{6,}\\s*$"))
  # A preliminary print or bound volume draws NO rule: its footnotes are set
  # in a smaller face with nothing in the text layer to mark where they
  # begin (a body line "83 Fed. Reg. 3553 (2018). And those ..." looks exactly
  # like a note's first line). So footnotes are separated only where the rule
  # is, and a writing from a print reports its footnote measures as unknown
  # rather than as zero -- `has_rule` carries that up.
  if (length(rule)) {
    notes <- ls[(rule[1] + 1L):length(ls)]; ls <- ls[seq_len(rule[1] - 1L)]
  } else notes <- character()
  list(body = paste(ls, collapse = "\n"), notes = paste(notes, collapse = "\n"), has_rule = length(rule) > 0)
}

# The front matter of a writing's first page, cut at the byline. Three
# bylines: "JUSTICE X delivered the opinion of the Court.", "JUSTICE X
# announced the judgment of the Court and delivered ...", and the separate
# writing's "JUSTICE X, with whom JUSTICE Y joins, dissenting." (which is
# also how a per curiam's dissent opens); a per curiam itself has no byline
# and its front matter ends at the "PER CURIAM." line or the date. When no
# byline is found the text is returned whole rather than emptied.
.strip_front_matter <- function(body) {
  x <- body
  rx <- paste0("(?s)^.*?(?:",
    "delivered the opinion of the Court(?: with respect to [^.]{0,80})?\\.|",
    "announced the judgment of the Court[^.]{0,200}\\.|",
    "(?:CHIEF )?JUSTICE [A-Z]+(?:, with whom [^.]{0,200}?)?, (?:concurring|dissenting)[^.]{0,80}\\.|",
    "\\bPER CURIAM\\.)")
  # Case-insensitive: a slip's byline is "JUSTICE KAVANAUGH delivered ...", a
  # print's "Chief Justice Roberts announced the judgment ...".
  y <- str_replace(x, regex(rx, ignore_case = TRUE), "")
  if (identical(y, x)) {
    # No byline: drop just the NOTICE paragraph and the masthead if present.
    y <- str_replace(x, regex("(?s)^.*?formal errors\\.\\s*"), "")
    y <- str_replace(y, regex("(?s)^\\s*SUPREME COURT OF THE UNITED STATES.*?\\[[A-Z][a-z]+ \\d{1,2}, \\d{4}\\]\\s*"), "")
  }
  y
}

# Clean a body for measuring: de-hyphenate line ends ("Exec-\nutive"), drop
# lines that are only a section numeral ("I", "II", "A", "1"), and join lines.
.clean_body <- function(body) {
  x <- str_replace_all(body %||% "", "([A-Za-z])-\\s*\\n\\s*([a-z])", "\\1\\2")
  ls <- str_split(x, "\n")[[1]]
  ls <- ls[!str_detect(str_squish(ls), "^(?:[IVX]{1,5}|[A-D]|\\d{1,2})$")]
  str_squish(paste(ls, collapse = " "))
}

# Every writing in a slip opinion: tibble(section, who, kind, pages, body, notes).
# `who` is the Justice's surname in lower case ("court" / "percuriam" for the
# Court's own), `kind` one of court / percuriam / concurring / dissenting /
# mixed / judgment / opinion (a partial or plurality opinion of X, J.).
opinion_sections <- function(pages) {
  sec <- vapply(pages, .page_section, character(1), USE.NAMES = FALSE)
  # A page with no head continues the section before it (a wrapped head).
  for (i in seq_along(sec)) if (is.na(sec[i]) && i > 1) sec[i] <- sec[i - 1]
  keep <- !is.na(sec) & !sec %in% c("syllabus", "counsel", "appendix")
  if (!any(keep)) return(tibble(section = character(), who = character(), kind = character(),
                                pages = integer(), body = character(), notes = character()))
  out <- lapply(unique(sec[keep]), function(s) {
    idx <- which(sec == s)
    pb <- lapply(pages[idx], .page_body)
    # The first page of a writing opens with the slip's front matter: the
    # "NOTICE: This opinion is subject to formal revision ..." paragraph (the
    # Court's opinion only), then "SUPREME COURT OF THE UNITED STATES", the
    # docket line, the caption, "ON WRIT OF CERTIORARI TO ...", the date, and
    # the byline ("JUSTICE KAVANAUGH delivered the opinion of the Court." /
    # "JUSTICE JACKSON, dissenting."). The body starts after the byline.
    pb[[1]]$body <- .strip_front_matter(pb[[1]]$body)
    who <- if (s %in% c("court", "percuriam")) s else str_remove(s, ":.*$")
    k <- if (s %in% c("court", "percuriam")) s else str_remove(s, "^[a-z]+:")
    kind <- switch(k, "concurring" = "concurring", "dissenting" = "dissenting",
                   "concurring in the judgment" = "judgment", "opinion" = "opinion",
                   "concurring in part" = "concurring", "dissenting in part" = "mixed",
                   "concurring in part and dissenting in part" = "mixed",
                   "concurring in the judgment in part and dissenting in part" = "mixed", k)
    tibble(section = s, who = who, kind = kind, pages = length(idx),
           body = paste(vapply(pb, `[[`, character(1), "body"), collapse = "\n"),
           notes = paste(vapply(pb, `[[`, character(1), "notes"), collapse = "\n"),
           has_rule = any(vapply(pb, `[[`, logical(1), "has_rule")))
  })
  bind_rows(out)
}

# ---- measures ------------------------------------------------------------------

# Citations, masked to one token each before anything is counted: reporter
# cites ("603 U. S. 369, 412 (2024)", "45 F. 4th 1002", "142 S. Ct. 2228"),
# statutes ("28 U. S. C. §1331", "42 U. S. C. §§1983, 1988"), and the
# short-form "Id., at 412" / "Ibid." The count is returned with the text.
.CITE_RX <- paste0(
  "\\b\\d{1,3} U\\. ?S\\. ?(?:C\\. ?)?(?:§+ ?[\\dA-Za-z().\\-, ]+|\\d+|_{2,})(?:, (?:at )?\\d+(?:[-–]\\d+)?)*(?: \\(\\d{4}\\))?|",
  "\\b\\d{1,4} (?:F\\. ?(?:2d|3d|4th)|F\\. ?Supp\\. ?(?:2d|3d)?|S\\. ?Ct\\.|L\\. ?Ed\\. ?2d|So\\. ?(?:2d|3d)|N\\. ?E\\. ?(?:2d|3d)|P\\. ?(?:2d|3d)|A\\. ?(?:2d|3d)|Wheat\\.|How\\.|Cranch|Pet\\.|Wall\\.|Dall\\.) \\d+(?:, \\d+)*(?: \\([^)]{1,40}\\))?|",
  "\\bIbid\\.|\\bId\\.,? (?:at \\d+[\\dA-Za-z–-]*)?|\\b\\d{1,3} Stat\\. \\d+|\\b\\d{1,3} C\\. ?F\\. ?R\\. ?§+ ?[\\d.]+")
.mask_cites <- function(x) {
  n <- str_count(x, .CITE_RX)
  list(text = str_replace_all(x, .CITE_RX, " CITE "), n = n)
}

# Words: letters with internal apostrophes/hyphens; "CITE" tokens excluded.
.words <- function(x) { w <- str_extract_all(tolower(x), "[a-z][a-z'’-]*")[[1]]; w[w != "cite"] }
# Sentences: split at . ? ! followed by space and a capital or quote; the
# cite mask has already removed the periods inside citations, and the
# commonest legal abbreviations are protected.
.sentences <- function(x) {
  x <- str_replace_all(x, "\\b(v|Mr|Mrs|Ms|Dr|J|JJ|C|No|Nos|Inc|Co|Corp|Art|Amdt|cf|e\\.g|i\\.e|U\\.S|Stat|Cong|Sess|Rev|Ct|App|Cir|Dist|Ed|Tr|Pet|Br|Ibid|Id|Sec|supra|infra|ante|post|ch|para|pp|p|n|nn|vol|ed|Jr|Sr)\\.", "\\1<DOT>")
  s <- str_split(x, "(?<=[.?!][\"”')\\]]?)\\s+(?=[\"“(\\[]?[A-Z])")[[1]]
  s <- str_replace_all(s, "<DOT>", "."); s <- str_squish(s); s[nchar(s) > 1]
}
# Syllables, the usual vowel-group heuristic; good enough for a grade level.
.syllables <- function(w) {
  w <- str_remove(w, "(?<=[a-z])e$|es$|ed$"); n <- str_count(w, "[aeiouy]+"); pmax(n, 1L)
}

HEDGES   <- c("may", "might", "could", "perhaps", "arguably", "seems", "seem", "appears", "appear", "suggests", "suggest", "likely", "generally", "often", "sometimes", "somewhat", "largely", "possible", "possibly", "presumably", "tends", "tend")
BOOSTERS <- c("clearly", "plainly", "obviously", "certainly", "undoubtedly", "surely", "indeed", "of course", "must", "always", "never", "beyond doubt", "unquestionably", "squarely", "flatly", "simply", "precisely", "manifestly", "wholly", "entirely")

# The measures for one text (a writing's body) and its footnote block.
text_measures <- function(body, notes = "", notes_known = TRUE) {
  m <- .mask_cites(.clean_body(body)); nm <- .mask_cites(.clean_body(notes))
  w <- .words(m$text); nw <- length(w)
  # From a print (no footnote rule) the note measures are unknown, not zero.
  n_notes <- if (notes_known) length(.words(nm$text)) else NA_integer_
  if (nw < 20) return(tibble(words = nw, words_notes = n_notes, sentences = NA_integer_))
  s <- .sentences(m$text); sl <- vapply(s, function(z) length(.words(z)), integer(1)); sl <- sl[sl > 0]
  syl <- .syllables(w)
  low <- tolower(m$text)
  count_terms <- function(terms) sum(vapply(terms, function(t) str_count(low, paste0("\\b", t, "\\b")), numeric(1)))
  tibble(
    words = nw, words_notes = n_notes,
    sentences = length(sl),
    sent_mean = mean(sl), sent_sd = sd(sl), sent_over40 = mean(sl > 40),
    fk_grade = 0.39 * mean(sl) + 11.8 * mean(syl) - 15.59,
    syl_per_word = mean(syl), long_words = mean(nchar(w) >= 9),
    ttr = length(unique(w)) / nw,
    cites = m$n + nm$n, cites_per_k = 1000 * (m$n + nm$n) / (nw + (if (notes_known) length(.words(nm$text)) else 0L)),
    footnotes = if (notes_known) str_count(notes %||% "", "(?m)^\\s*\\d{1,3}\\s*[A-Z“\"]") else NA_integer_,
    we_per_k = 1000 * sum(w %in% c("we", "our", "us", "ourselves")) / nw,
    i_per_k = 1000 * sum(w %in% c("i", "my", "me")) / nw,
    # Not "'s": a possessive ("the Court's") is not a contraction, and it is
    # the commonest apostrophe in an opinion. Both apostrophe glyphs.
    contractions_per_k = 1000 * sum(str_detect(w, "n['’]t$|['’](re|ve|ll|d|m)$|^(it|that|there|what|who|here|let)['’]s$")) / nw,
    questions_per_k = 1000 * str_count(m$text, "\\?") / nw,
    hedges_per_k = 1000 * count_terms(HEDGES) / nw,
    boosters_per_k = 1000 * count_terms(BOOSTERS) / nw,
    dissent_refs_per_k = 1000 * str_count(low, "\\bthe (principal )?dissent\\b|\\bthe concurrence\\b|\\bthe majority\\b|\\bthe court'?s opinion\\b|\\bthe plurality\\b") / nw,
    passive_per_k = 1000 * str_count(low, "\\b(is|are|was|were|be|been|being)\\s+(\\w+ly\\s+)?\\w+(ed|en)\\b") / nw)
}

# Every writing in a slip opinion, measured: one row per section.
opinion_text_stats <- function(pages) {
  secs <- opinion_sections(pages)
  if (!nrow(secs)) return(secs)
  ms <- bind_rows(lapply(seq_len(nrow(secs)), function(i) text_measures(secs$body[i], secs$notes[i], secs$has_rule[i])))
  bind_cols(secs |> select(section, who, kind, pages), ms)
}

# ---- cache and resolver ------------------------------------------------------------
# justices/opinion_text.json, keyed by docket: {url, decided, fetched, tv, ok,
# sections: [{who, kind, pages, words, ...}]}. Measures only -- never the text.
# A decision whose PDF yielded no section is cached with ok = FALSE so it is
# not fetched again unless `retry` is set. `tv` is OPINION_TEXT_VERSION: a
# measure that changes needs the pages again, so a bump plus `retry` re-fetches.
OPINION_TEXT_FILE <- "opinion_text.json"
opinion_text_path <- function(site_dir) file.path(site_dir, "justices", OPINION_TEXT_FILE)
read_opinion_text <- function(site_dir) {
  p <- opinion_text_path(site_dir)
  if (!file.exists(p)) return(list())
  tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) list())
}
write_opinion_text <- function(cache, site_dir) {
  p <- opinion_text_path(site_dir)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(cache, p, auto_unbox = TRUE, null = "null", na = "null", digits = 4, pretty = FALSE)
}
# A cache entry from a decision's pages (shared with the lineup fetch, which
# has the same pages in hand -- see resolve_lineups()).
opinion_text_entry <- function(pages, url, decided) {
  st <- tryCatch(opinion_text_stats(pages), error = function(e) NULL)
  e <- list(url = url, decided = as.character(decided), fetched = as.character(Sys.Date()), tv = OPINION_TEXT_VERSION)
  if (is.null(st) || !nrow(st)) { e$ok <- FALSE; return(e) }
  st <- st |> mutate(across(where(is.numeric), ~ round(.x, 4)))
  e$ok <- TRUE; e$sections <- lapply(seq_len(nrow(st)), function(i) as.list(st[i, ]))
  e
}

#' Fetch and measure every decision in `dec` (a gn_decisions() frame) the
#' cache lacks, up to `max_new`, `pace` seconds apart; mirrors resolve_lineups().
#' `urls`: docket -> opinion PDF. Whole opinions are read (a slip is one file;
#' a volume is windowed from the case's syllabus to the next case's header).
resolve_opinion_text <- function(dec, urls, site_dir, max_new = 0L, pace = 1,
                                 retry = FALSE, max_consecutive_empty = 8L) {
  cache <- read_opinion_text(site_dir)
  is_cached <- function(dk) !is.null(cache[[dk]]) &&
    (!retry || (isTRUE(cache[[dk]]$ok) && identical(cache[[dk]]$tv, OPINION_TEXT_VERSION)))
  url_for <- function(dkts) { u <- urls[dkts]; u <- u[!is.na(u)]; if (length(u)) u[[1]] else NA_character_ }
  todo <- dec |> filter(!vapply(dkt, is_cached, logical(1))) |>
    mutate(url = vapply(dkts, url_for, character(1))) |> filter(!is.na(url))
  n_uncached <- sum(!vapply(dec$dkt, is_cached, logical(1)))
  todo <- head(todo, max_new)
  if (nrow(todo)) {
    message("opinion text: fetching ", nrow(todo), " of ", n_uncached, " uncached decision(s) (cap ", max_new, ")")
    empties <- 0L
    for (i in seq_len(nrow(todo))) {
      res <- .fetch_pdf_pages(todo$url[i], todo$dkts[[i]], whole = TRUE); pages <- res$pages
      if (!length(pages)) {
        if (res$fetched) empties <- empties + 1L
        cache[[todo$dkt[i]]] <- list(url = todo$url[i], decided = as.character(todo$decided[i]),
                                     fetched = as.character(Sys.Date()), tv = OPINION_TEXT_VERSION, ok = FALSE)
      } else {
        if (res$fetched) empties <- 0L
        cache[[todo$dkt[i]]] <- opinion_text_entry(pages, todo$url[i], todo$decided[i])
      }
      if (empties >= max_consecutive_empty) {
        message("opinion text: ", empties, " empty downloads in a row after ", i,
                " fetch(es) -- throttled; stopping this run (retry next run)")
        break
      }
      if (i %% 25 == 0) { write_opinion_text(cache, site_dir); message("  ...", i, "/", nrow(todo)) }
      if (pace > 0 && res$fetched) Sys.sleep(pace)
    }
    write_opinion_text(cache, site_dir)
    got <- vapply(head(todo$dkt, i), function(d) isTRUE(cache[[d]]$ok), logical(1))
    message("opinion text: ", sum(got), " measured of ", i, " fetched")
    attr(cache, "n_fetched") <- i
  } else attr(cache, "n_fetched") <- 0L
  invisible(cache)
}

# ---- per-Term aggregation -------------------------------------------------------
# One row per measured writing of a Term's decisions, joined to the Granted &
# Noted writings list for the kind (court / conc / judg / diss / mixed) and the
# Justice's roster name. `dec` is the Term's decisions (term_stats' `dec`),
# `writings` its writings list (st$writings), `cache` the text cache.
term_text_rows <- function(dec, writings, cache) {
  rows <- list()
  for (i in seq_len(nrow(dec))) {
    e <- NULL; for (d in dec$dkts[[i]]) if (!is.null(cache[[d]]) && isTRUE(cache[[d]]$ok)) { e <- cache[[d]]; break }
    if (is.null(e)) next
    for (s in e$sections) {
      if (is.null(s$sentences) || is.na(s$sentences %||% NA)) next
      # Roster form throughout ("Roberts", not the list's "Roberts, C.J."),
      # via justice_key() where the Justices module is loaded.
      who <- if (s$who %in% c("court", "percuriam")) {
        a <- dec$author[i]; if (is.na(a) || a == "Per Curiam") "Per Curiam" else a
      } else paste0(toupper(substr(s$who, 1, 1)), substr(s$who, 2, nchar(s$who)))
      if (who != "Per Curiam" && exists("justice_key")) { k <- justice_key(who); if (!is.na(k)) who <- k }
      lead <- dec$author[i]; if (!is.na(lead) && exists("justice_key")) { k <- justice_key(lead); if (!is.na(k)) lead <- k }
      kind <- if (s$who %in% c("court", "percuriam") ||
                  (identical(s$kind, "opinion") && identical(who, lead))) "court" else {
        w <- if (!is.null(writings)) writings[writings$dkt %in% dec$dkts[[i]] & writings$name == who & writings$kind != "court", ] else NULL
        if (!is.null(w) && nrow(w)) w$kind[1] else switch(s$kind, concurring = "conc", judgment = "judg", dissenting = "diss", mixed = "diss", opinion = "judg", "conc")
      }
      rows[[length(rows) + 1L]] <- tibble(dkt = dec$dkt[i], decided = as.character(dec$decided[i]), name = who, kind = kind,
                                          words = s$words, words_notes = as.numeric(s$words_notes %||% NA), sent_mean = s$sent_mean,
                                          sent_sd = s$sent_sd, sent_over40 = s$sent_over40, fk_grade = s$fk_grade,
                                          cites_per_k = s$cites_per_k, footnotes = as.numeric(s$footnotes %||% NA), we_per_k = s$we_per_k,
                                          contractions_per_k = s$contractions_per_k, questions_per_k = s$questions_per_k,
                                          hedges_per_k = s$hedges_per_k, boosters_per_k = s$boosters_per_k,
                                          dissent_refs_per_k = s$dissent_refs_per_k)
    }
  }
  if (!length(rows)) return(NULL)
  out <- bind_rows(rows)
  # A plurality prints the lead author's opinion in two runs -- "Opinion of
  # the Court" for the joined Parts, "Opinion of ROBERTS, C. J." for the rest
  # (24-1287) -- and that is one writing, not a Court opinion plus a
  # concurrence. Any rows of one decision by one Justice under one kind are
  # merged: counts summed, rates weighted by words.
  na_sum <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  out |> group_by(dkt, decided, name, kind) |> summarise(
    words_notes = na_sum(words_notes), footnotes = na_sum(footnotes),
    across(c(sent_mean, sent_sd, sent_over40, fk_grade, we_per_k, contractions_per_k, questions_per_k,
             hedges_per_k, boosters_per_k, dissent_refs_per_k), ~ weighted.mean(.x, words)),
    cites_per_k = weighted.mean(cites_per_k, words + coalesce(words_notes, 0)),
    words = sum(words), .groups = "drop") |>
    relocate(words, .after = kind)
}

# Per Justice: counts and medians by kind, plus the style measures pooled over
# every writing (weighted by words). `court` is term_court(); rows in seniority.
term_text_stats <- function(rows, court) {
  if (is.null(rows) || !nrow(rows)) return(NULL)
  by_kind <- rows |> group_by(name, kind) |> summarise(n = n(), med = median(words), .groups = "drop")
  pick <- function(nm, k, f) { r <- by_kind[by_kind$name == nm & by_kind$kind == k, ]; if (nrow(r)) r[[f]] else NA }
  # Footnote measures pool only the writings whose notes are known (slips);
  # a Justice whose Term came entirely from prints shows them as unknown.
  pooled <- rows |> group_by(name) |> summarise(
    n = n(), words_total = sum(words), pages_words = sum(words + coalesce(words_notes, 0)),
    sent_mean = weighted.mean(sent_mean, words), sent_over40 = weighted.mean(sent_over40, words),
    fk_grade = weighted.mean(fk_grade, words), cites_per_k = weighted.mean(cites_per_k, words + coalesce(words_notes, 0)),
    footnotes = if (all(is.na(footnotes))) NA_real_ else mean(footnotes, na.rm = TRUE),
    fn_share = if (all(is.na(words_notes))) NA_real_ else
      sum(words_notes, na.rm = TRUE) / sum((words + words_notes)[!is.na(words_notes)]),
    we_per_k = weighted.mean(we_per_k, words), contractions_per_k = weighted.mean(contractions_per_k, words),
    questions_per_k = weighted.mean(questions_per_k, words),
    hedge_boost = sum(hedges_per_k * words) / pmax(sum(boosters_per_k * words), 1),
    dissent_refs_per_k = weighted.mean(dissent_refs_per_k, words), .groups = "drop")
  out <- tibble(name = court$name, label = court$label) |>
    mutate(court_n = vapply(name, pick, numeric(1), k = "court", f = "n"), court_med = vapply(name, pick, numeric(1), k = "court", f = "med"),
           conc_n = vapply(name, function(nm) sum(by_kind$n[by_kind$name == nm & by_kind$kind %in% c("conc", "judg")]), numeric(1)),
           conc_med = vapply(name, function(nm) { r <- rows[rows$name == nm & rows$kind %in% c("conc", "judg"), ]; if (nrow(r)) median(r$words) else NA }, numeric(1)),
           diss_n = vapply(name, pick, numeric(1), k = "diss", f = "n"), diss_med = vapply(name, pick, numeric(1), k = "diss", f = "med")) |>
    left_join(pooled, by = "name")
  pc <- pooled[pooled$name == "Per Curiam", ]
  list(by_justice = out, per_curiam = if (nrow(pc)) pc else NULL, n_writings = nrow(rows),
       # Writings read from a print or volume: their footnotes stayed in the
       # body (no rule to cut at), which shortens sentences and raises the
       # citation rate. The page says how many so a Term is read accordingly.
       n_print = sum(is.na(rows$words_notes)),
       n_dec = dplyr::n_distinct(rows$dkt), court_sent = weighted.mean(rows$sent_mean, rows$words),
       court_fk = weighted.mean(rows$fk_grade, rows$words), court_cites = weighted.mean(rows$cites_per_k, rows$words + rows$words_notes))
}
