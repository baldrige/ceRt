# qp_extract.R -----------------------------------------------------------------
# Extract the Question(s) Presented from petition PDFs, with a persistent,
# incremental on-disk cache so a large corpus (e.g. all cases distributed for a
# term's conferences) is only fetched once. Shared by the daily dashboard
# (scotus_dash_new.R) and the conference reports (render_conferences.R).

suppressPackageStartupMessages({
  library(tidyverse)
  library(pdftools)
  library(jsonlite)
})

# Drop the boilerplate that opens a petition's Question-Presented page: an
# optional leading page number (usually "i") and the "Question(s) Presented"
# heading. ANCHORED to the very start of the text so only a genuine leading
# heading is removed -- crucially NOT an in-body repetition of the phrase (e.g.
# a QP that says "...raises the same question presented here..."), which the old
# "first 300 chars" search would match and then discard everything before it.
strip_qp_heading <- function(txt) {
  if (length(txt) == 0 || is.na(txt) || identical(txt, "-")) return(txt)
  m <- str_locate(
    txt,
    regex(paste0("^\\s*(?:[ivxlcdm]{1,4}\\s+)?",
                 "QUESTION\\s*\\(?\\s*[Ss]?\\s*\\)?\\s+PRESENTED(\\s+FOR\\s+REVIEW)?\\s*[:.]?"),
          ignore_case = TRUE)
  )
  if (is.na(m[1, "end"])) return(str_trim(txt))
  str_trim(str_sub(txt, m[1, "end"] + 1L))
}

# The petition's "Question(s) Presented" heading. Tolerant of the plural, of the
# "(S)" form with or without spaces around the parens ("QUESTION (S) PRESENTED"
# on the pro-se/IFP form), of "... FOR REVIEW", and of "ISSUE(S) PRESENTED".
QP_HEADING_RE <- regex(
  "(?:QUESTION|ISSUE)S?\\s*\\(?\\s*[Ss]?\\s*\\)?\\s+PRESENTED(?:\\s+FOR\\s+REVIEW)?",
  ignore_case = TRUE)

# Front-matter section headings that mark the END of the QP block -- the next
# section a petition prints after the questions. Anchored to line start, with an
# optional "I."/"1." enumerator, so the phrase only counts as a heading (not
# prose). Only reliable *front-matter* sections are listed: body headings like
# "Introduction" are deliberately excluded (pro-se petitioners open their QP with
# that word). Used to trim a QP that runs to the bottom of its page into the
# following section.
QP_END_RE <- regex(paste0(
  "(?m)^[ \\t]*(?:[IVXLCDM0-9]{1,4}[.)]\\s+)?(?:",
  "PARTIES TO THE",                                   # PROCEEDING(S)/PETITION/CASE
  "|PARTIES[,]? (?:RELATED|AND)",                     # "PARTIES, RELATED PROCEEDINGS..."
  "|(?:INTERESTED|LIST(?:ING)? OF(?: ALL)?) PARTIES",
  "|LIST OF PROCEEDINGS",
  "|CORPORATE DISCLOSURE",
  "|RULE 29\\.6",
  "|TABLE OF CONTENTS",
  "|TABLE OF (?:CITED )?AUTHORITIES",
  "|STATEMENT OF RELATED",
  "|RELATED (?:CASES|PROCEEDINGS)",
  "|OPINIONS? (?:AND ORDERS? )?BELOW",
  "|STATEMENT OF (?:THE )?JURISDICTION",
  "|JURISDICTIONAL STATEMENT",
  ")"),
  ignore_case = TRUE)

# Strip a leading page-number folio (roman or arabic) and its blank line(s) --
# the running header at the top of a QP continuation page.
strip_leading_pagenum <- function(pg) {
  sub("^[ \\t\\r\\n]*(?:[ivxlcdm]{1,6}|\\d{1,3})[ \\t\\r]*\\n+", "", pg,
      ignore.case = TRUE, perl = TRUE)
}
# Strip a trailing page-number folio ("(i)", "ii", "12") on its own final line.
strip_trailing_pagenum <- function(s) {
  sub("\\s*\\n\\s*\\(?\\s*(?:[ivxlcdm]{1,6}|\\d{1,3})\\s*\\)?[.]?\\s*$", "", s,
      perl = TRUE, ignore.case = TRUE)
}
# Cut a string at the first next-section heading, if any.
cut_at_qp_end <- function(s) {
  e <- str_locate(s, QP_END_RE)[1, "start"]
  if (!is.na(e)) str_sub(s, 1L, e - 1L) else s
}
# Petition pages use layout whitespace that markdown misreads as code blocks
# (monospace). Normalize horizontal whitespace to single spaces (no leading
# indent) so the QP renders as prose / a clean numbered list.
normalize_qp <- function(txt) {
  txt <- str_replace_all(txt, "\t", " ")
  txt <- str_replace_all(txt, regex("^ +", multiline = TRUE), "")
  txt <- str_replace_all(txt, " {2,}", " ")
  txt <- str_replace_all(txt, "\n{3,}", "\n\n")
  str_trim(txt)
}

# TRUE when the QP genuinely continues onto the next page: either the text so far
# was cut mid-thought (no terminal punctuation) or the next page opens with a
# question/list enumerator ("2.", "(3)", "b)", "II."). This guards against
# pulling in the following section when a heading isn't recognized.
qp_continues <- function(acc, next_pg) {
  open <- !str_detect(str_trim(acc), "[.?!][\"')”]?$")
  nxt  <- str_detect(str_trim(next_pg),
                     "^(?:\\(?[0-9]{1,2}[.)]|\\(?[a-z]\\)|[ivx]{1,4}[.)])\\s")
  open || nxt
}

# Extract the QP from a vector of page texts (works for the text layer OR OCR).
# Locates the FIRST page carrying the heading -- which is robust to cover-page
# counsel that overflows onto a second page and pushes the QP to page 3 -- then
# extends across continuation pages until the next front-matter section, so a QP
# that overflows its own page isn't truncated. Returns "-" if no heading is found
# (better an em dash than the counsel block that a fixed-page grab would return).
qp_from_pages <- function(pages, scan_pages = 8L, max_cont = 3L) {
  if (length(pages) == 0) return("-")
  win <- head(pages, scan_pages)
  k <- Find(function(i) str_detect(win[i], QP_HEADING_RE), seq_along(win))
  if (is.null(k)) return("-")
  h <- str_locate(pages[k], QP_HEADING_RE)
  body <- str_sub(pages[k], h[1, "end"] + 1L)
  page_k_ends_qp <- !is.na(str_locate(body, QP_END_RE)[1, "start"])
  # Strip the QP page's bottom folio so a stray "i" doesn't read as an open
  # (mid-sentence) ending and wrongly trigger continuation onto the next section.
  acc <- strip_trailing_pagenum(cut_at_qp_end(body))
  if (!page_k_ends_qp) {
    j <- k + 1L; cont <- 0L
    while (j <= length(pages) && cont < max_cont) {
      pg <- strip_leading_pagenum(pages[j])
      st <- str_locate(pg, QP_END_RE)[1, "start"]
      if (!is.na(st) && st <= 3L) break            # page opens with next section
      if (!qp_continues(acc, pg)) break            # QP looks complete -> stop
      piece <- if (!is.na(st)) str_sub(pg, 1L, st - 1L) else pg
      acc <- strip_trailing_pagenum(paste0(str_trim(acc), " ", str_trim(piece)))
      if (!is.na(st)) break                        # next section began mid-page
      cont <- cont + 1L; j <- j + 1L
    }
  }
  out <- normalize_qp(acc)
  if (out == "") "-" else out
}

# Extract the QP from a petition PDF (a URL or a local path). Tries the text
# layer first (fast); falls back to OCR over the front pages only when there's no
# text layer at all (a scanned petition).
extract_qp <- function(src, scan_pages = 8L) {
  pages <- tryCatch(pdftools::pdf_text(src), error = function(e) character(0))
  res <- qp_from_pages(pages, scan_pages)
  if (!identical(res, "-")) return(res)
  layer_empty <- length(pages) == 0 || all(str_squish(head(pages, scan_pages)) == "")
  if (layer_empty) {
    npg  <- tryCatch(pdftools::pdf_info(src)$pages, error = function(e) NA_integer_)
    kmax <- if (is.na(npg)) 5L else min(5L, npg)
    ocr  <- tryCatch(pdftools::pdf_ocr_text(src, pages = seq_len(kmax)),
                     error = function(e) character(0))
    res  <- qp_from_pages(ocr, scan_pages)
  }
  res
}

# Single petition QP (pdftools downloads the URL itself).
get_qp <- function(url) {
  if (is.na(url) || url == "") return("-")
  extract_qp(url)
}

# A cached "QP" that is really the cover-page counsel block -- the pre-fix
# extractor's failure mode when counsel overflowed onto page 2 and pushed the
# real QP to page 3. It carries law-firm / attorney markers and none of the
# sentence structure a real Question Presented has. Such entries are re-fetched
# (in place, no URL change) so the improved extractor replaces them; this rebuilds
# only the broken entries, not the whole cache.
qp_looks_stale <- function(qp) {
  if (is.null(qp) || identical(qp, "-") || !nzchar(qp)) return(FALSE)
  str_detect(qp, regex("Counsel of Record|Attorneys? for|\\bLLP\\b|\\bPLLC\\b|\\(\\d{3}\\)\\s?\\d{3}",
                       ignore_case = TRUE)) &&
    !str_detect(qp, regex("whether|question presented|the question|\\bis\\b|\\bare\\b",
                          ignore_case = TRUE))
}

# Cache-backed batch resolver. Returns a named character vector docket -> QP for
# every docket in `dockets` (aligned; "-" where unavailable). The cache is a
# JSON object keyed by docket, storing the petition URL and extracted text so a
# stale URL (petition re-filed) is re-fetched. Only up to `max_new` uncached
# petitions are fetched per run to bound per-run PDF load; the rest stay "-"
# until a later run, and the cache fills in incrementally.
resolve_qps <- function(dockets, urls, cache_path = NULL, max_new = Inf) {
  stopifnot(length(dockets) == length(urls))
  cache <- list()
  if (!is.null(cache_path) && file.exists(cache_path)) {
    cache <- tryCatch(fromJSON(cache_path, simplifyVector = FALSE),
                      error = function(e) list())
  }

  # Unique dockets with a usable URL and no fresh cache hit.
  uniq <- tibble(dkt = dockets, url = urls) |>
    filter(!is.na(url), url != "") |>
    distinct(dkt, .keep_all = TRUE)
  # Guard the empty case: a rowwise mutate over a 0-row tibble evaluates its
  # expression once on character(0) to infer the column type, and
  # `cache[[character(0)]]` errors ("attempt to select less than one element").
  needs <- if (nrow(uniq) == 0) {
    uniq
  } else {
    uniq |>
      rowwise() |>
      mutate(cached = {
        c <- cache[[dkt]]
        # unname(): a named `urls` vector would otherwise fail identical() on the
        # names alone and silently re-fetch the whole cache.
        !is.null(c) && identical(unname(c$url), unname(url)) && !qp_looks_stale(c$qp)
      }) |>
      ungroup() |>
      filter(!cached)
  }

  n_fetch <- min(nrow(needs), max_new)
  if (n_fetch > 0) {
    fetch <- needs |> slice_head(n = n_fetch)
    message("QP: fetching ", n_fetch, " of ", nrow(needs), " uncached petition(s)",
            if (is.finite(max_new)) paste0(" (cap ", max_new, ")") else "")
    for (i in seq_len(nrow(fetch))) {
      qp <- tryCatch(get_qp(fetch$url[i]), error = function(e) "-")
      # Cache only successful extractions; a "-" (empty page, OCR miss, or a
      # transient throttle) is left uncached so it retries on a later run. Drop
      # any prior entry on a "-" so a re-fetched stale/garbage entry that no
      # longer extracts isn't re-flagged (and re-fetched) on every run.
      if (!identical(qp, "-")) cache[[fetch$dkt[i]]] <- list(url = fetch$url[i], qp = qp)
      else cache[[fetch$dkt[i]]] <- NULL
    }
    if (!is.null(cache_path)) {
      dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
      write_json(cache, cache_path, auto_unbox = TRUE)
    }
  }

  vapply(dockets, function(d) {
    c <- cache[[d]]
    if (is.null(c) || is.null(c$qp)) "-" else c$qp
  }, character(1), USE.NAMES = FALSE)
}

# ---- granted-case Question Presented (the Court's dedicated QP PDF) -----------
# For a GRANTED case the Court posts a clean, typeset "Questions Presented" PDF
# (the JSON docket's QPLink). It is far more reliable than page 2 of the petition
# -- machine-readable text (no OCR), present even when the petition is a scan or
# the case came up on cert-before-judgment, and it is the QP *as granted*. The
# URL is derivable from the docket number (same form as QPLink).

qp_pdf_url <- function(dkt) {
  m <- str_match(dkt, "^(\\d{2})-(\\d+)$")             # paid/IFP dockets only
  if (is.na(m[1, 1])) return(NA_character_)
  sprintf("https://www.supremecourt.gov/qp/%s-%05dqp.pdf", m[1, 2], as.integer(m[1, 3]))
}

# QP text from the dedicated PDF: read all pages, cut to after the
# "Question(s) Presented" heading, drop the trailing "CERT. GRANTED ..." note.
get_granted_qp <- function(dkt) {
  url <- qp_pdf_url(dkt)
  if (is.na(url)) return("-")
  txt <- tryCatch(paste(pdftools::pdf_text(url), collapse = "\n"),
                  error = function(e) NA_character_)
  if (is.na(txt) || str_squish(txt) == "") return("-")
  txt <- txt |>
    str_replace_all("\t", " ") |>
    str_replace_all(regex("^ +", multiline = TRUE), "") |>
    str_replace_all(" {2,}", " ") |>
    str_replace_all("\n{3,}", "\n\n")
  m <- str_locate(txt, regex("QUESTIONS?\\s+PRESENTED\\s*[:.]?", ignore_case = TRUE))
  if (is.na(m[1, "end"])) return("-")
  qp <- str_trim(str_sub(txt, m[1, "end"] + 1L))
  # Capture notes from the trailing (all-caps) administrative block before it is
  # stripped: a limited grant and a consolidation.
  limited <- str_match(qp, "GRANTED LIMITED TO ([^.]+?)\\.")[, 2]
  consol  <- str_match(qp, "CONSOLIDATED WITH ([0-9A-Za-z-]+)")[, 2]
  # Strip the trailing grant-disposition / administrative blocks (each runs to
  # the end of the text). These are boilerplate and one even repeats the phrase
  # "QUESTION PRESENTED", which would otherwise re-trigger strip_qp_heading and
  # discard the real question.
  qp <- str_replace(qp, regex("\\s*CERT\\.?\\s+GRANTED\\b.*$", ignore_case = TRUE, dotall = TRUE), "")
  qp <- str_replace(qp, regex("\\s*GRANTED LIMITED TO\\b.*$", dotall = TRUE), "")
  qp <- str_replace(qp, regex("\\s*(PETITIONS?\\s+GRANTED\\b|CONSOLIDATED WITH\\b).*$", dotall = TRUE), "")
  # Some QP PDFs append the full disposition order ("ORDER OF M/D/YYYY: THE
  # PETITION FOR A WRIT OF CERTIORARI IS ...") after the questions -- strip it.
  qp <- str_replace(qp, regex("\\s*ORDER OF \\d{1,2}/\\d{1,2}/\\d{2,4}\\b.*$", dotall = TRUE), "")
  qp <- str_replace(qp, regex("\\s*THE PETITION FOR A WRIT OF CERTIORARI IS\\b.*$", ignore_case = TRUE, dotall = TRUE), "")
  qp <- str_trim(qp)
  qp <- split_court_questions(qp)              # Court-directed additional question
  note_parts <- c(
    if (!is.na(consol)) paste0("Consolidated with ", consol),
    if (!is.na(limited)) paste0("Granted limited to ", str_squish(str_to_lower(limited)))
  )
  if (length(note_parts) && qp != "") {
    qp <- paste0("*", paste(note_parts, collapse = "; "), ".*\n\n", qp)
  }
  if (qp == "") "-" else qp
}

# Turn ALL-CAPS legal prose into sentence case, restoring a few proper nouns and
# Roman numerals (the Court prints its added questions in caps).
soften_caps <- function(s) {
  if (!str_detect(s, "[A-Z]{5,}")) return(s)          # not shouting -> leave as-is
  s <- str_to_sentence(str_to_lower(s))
  for (w in c("Court", "Congress", "Constitution", "Article", "Amendment", "Clause",
              "United States", "Federal", "Commerce", "Fourteenth", "Fifth", "First",
              "Fourth", "Sixth", "Eighth", "Second", "Third", "Act", "Government")) {
    s <- str_replace_all(s, regex(paste0("\\b", w, "\\b"), ignore_case = TRUE), w)
  }
  for (r in c("i", "ii", "iii", "iv", "v", "vi")) {   # "Article iii" -> "Article III"
    s <- str_replace_all(s, regex(paste0("\\b(Article|Amendment|Title) ", r, "\\b"),
                                  ignore_case = TRUE), paste0("\\1 ", toupper(r)))
  }
  s
}

# The Court sometimes adds a question at the grant stage:
#   "In addition to the question presented by the petition, the parties are
#    directed to brief and argue the following question: <Q>"
# Split that out so both the petitioner's and the Court's questions are listed.
split_court_questions <- function(qp) {
  rx <- regex("\\bIN ADDITION TO THE QUESTIONS?\\s+PRESENTED\\b.*?THE FOLLOWING QUESTIONS?:\\s*",
              ignore_case = TRUE, dotall = TRUE)
  if (!str_detect(qp, rx)) return(qp)
  parts <- str_split(qp, rx, n = 2)[[1]]
  pet <- str_trim(parts[1]); court <- str_trim(parts[2])
  if (court == "") return(pet)
  court_item <- paste0("*Added by the Court:* ", soften_caps(court))
  n_pet <- str_count(pet, regex("(?m)^\\s*\\(?\\d+[.)]\\s"))
  if (n_pet >= 1) paste0(pet, "\n\n", n_pet + 1L, ". ", court_item)
  else paste0("1. ", pet, "\n\n2. ", court_item)
}

# Cache-backed resolver keyed by docket -> {qp}. The URL is derived, so none is
# stored. Fetches at most `max_new` uncached QP PDFs per run (they are small,
# clean text -- no OCR -- so this is far lighter than the petition path).
resolve_granted_qps <- function(dockets, cache_path = NULL, max_new = Inf) {
  cache <- list()
  if (!is.null(cache_path) && file.exists(cache_path)) {
    cache <- tryCatch(fromJSON(cache_path, simplifyVector = FALSE),
                      error = function(e) list())
  }
  uniq <- unique(dockets[!is.na(dockets)])
  uniq <- uniq[!is.na(vapply(uniq, qp_pdf_url, character(1)))]   # paid/IFP only
  needs <- uniq[vapply(uniq, function(d) is.null(cache[[d]]), logical(1))]
  n_fetch <- min(length(needs), max_new)
  if (n_fetch > 0) {
    message("granted QP: fetching ", n_fetch, " of ", length(needs), " uncached PDF(s)",
            if (is.finite(max_new)) paste0(" (cap ", max_new, ")") else "")
    for (d in head(needs, n_fetch)) {
      qp <- tryCatch(get_granted_qp(d), error = function(e) "-")
      if (!identical(qp, "-")) cache[[d]] <- list(qp = qp)
    }
    if (!is.null(cache_path)) {
      dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
      write_json(cache, cache_path, auto_unbox = TRUE)
    }
  }
  vapply(dockets, function(d) {
    c <- cache[[d]]; if (is.null(c) || is.null(c$qp)) "-" else c$qp
  }, character(1), USE.NAMES = FALSE)
}

# Reflow an extracted QP for clean markdown rendering. Petition PDFs wrap each
# question across lines and separate questions with blank lines, which markdown
# fragments into stray one-item lists and loose paragraphs. This rebuilds a
# multi-question QP as a single tight ordered list (wrapped lines joined,
# soft-hyphen line breaks repaired, items renumbered) and collapses a
# single-question QP to flowing prose. Idempotent.
reflow_qp <- function(txt) {
  if (length(txt) == 0 || is.na(txt) || identical(txt, "-") || txt == "") return(txt)
  lines <- str_trim(str_split(txt, "\n")[[1]])
  lines <- lines[lines != ""]
  if (length(lines) == 0) return("-")
  # Drop a trailing lone page number (some petitions number at the bottom).
  if (length(lines) > 1 &&
      str_detect(lines[length(lines)], "^[ivxlcdmIVXLCDM0-9]{1,4}$")) {
    lines <- lines[-length(lines)]
  }
  # Join a run of lines into one string, repairing soft-hyphen line breaks
  # ("harm-" + "lessness" -> "harmlessness").
  join_lines <- function(ls) {
    out <- ls[1]
    for (k in seq_len(length(ls) - 1L) + 1L) {
      out <- if (str_detect(out, "[A-Za-z]-$")) {
        paste0(str_sub(out, 1, -2), ls[k])
      } else {
        paste(out, ls[k])
      }
    }
    str_squish(out)
  }

  marker <- str_detect(lines, "^\\(?\\d+[.)]\\s")
  if (sum(marker) < 2) return(join_lines(lines)) # not a numbered list -> prose

  grp <- cumsum(marker)
  preamble <- if (any(grp == 0)) join_lines(lines[grp == 0]) else ""
  items <- tapply(seq_along(lines)[grp > 0], grp[grp > 0], function(idx) {
    str_replace(join_lines(lines[idx]), "^\\(?\\d+[.)]\\s*", "")
  })
  # Blank line between items: gt's markdown renders a loose list; a single
  # newline is NOT recognized as a list and falls back to prose.
  md_list <- paste0(seq_along(items), ". ", items, collapse = "\n\n")
  if (nzchar(preamble)) paste0(preamble, "\n\n", md_list) else md_list
}

# Wrap raw QP text as a collapsible <details> block (markdown-rendered).
qp_details <- function(qp) {
  qp <- vapply(qp, function(q) {
    if (is.na(q) || q == "" || q == "-") return("—") # em-dash, not a list marker
    reflow_qp(strip_qp_heading(q))
  }, character(1), USE.NAMES = FALSE)
  qp <- str_replace_all(qp, "\\$", "&#36;")
  # Blank lines around the content so a leading "1." sits at a line boundary and
  # markdown parses it as a list item (glued to </summary> it would not).
  str_c("<details><summary>Question(s) presented</summary>\n\n", qp, "\n\n</details>")
}
