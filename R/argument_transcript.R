# R/argument_transcript.R -- oral-argument transcripts, parsed into speaker turns.
#
# The Court posts each argument's transcript as a PDF (Heritage Reporting's
# layout: a cover page, an appearances page, a contents page, then numbered
# lines 1-25 per page, then a word index). This file turns one into a table of
# turns -- who spoke, in which advocate's segment, how many words -- which is
# the foundation for both a synced reader and any analysis of the bench.
#
# Where the PDFs are: fetch_media_feed("transcripts", term) in R/argument_nav.R
# (the Court's per-Term RSS feed, back to OT2017 at least), with the transcript
# index scrape as its fallback. Nothing here is on the daily path.
#
# See docs/argument-transcripts.md.

suppressPackageStartupMessages({
  library(stringr); library(dplyr); library(tibble); library(purrr)
})
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

TX_UA <- "Mozilla/5.0 (ceRt SCOTUS research; +https://supremecourt.report)"

# A speaker label opens a line in capitals and ends in a colon: "JUSTICE KAGAN:",
# "CHIEF JUSTICE ROBERTS:", "MR. DUPREE:", "GENERAL PRELOGAR:". Case-sensitive on
# purpose -- "Mr. Chief Justice, and may it please the Court:" is speech.
.TX_SPEAKER_RX <- paste0("^((?:CHIEF )?JUSTICE [A-Z][A-Z'-]+|(?:MR|MS|MRS|MISS)\\. [A-Z][A-Z'. -]{0,30}[A-Z]",
                         "|GENERAL [A-Z][A-Z'-]+)\\s?:\\s*(.*)$")
# An argument header: "ORAL ARGUMENT OF THOMAS H. DUPREE, JR." / "REBUTTAL
# ARGUMENT OF ...", continued on the next line(s) by "ON BEHALF OF THE
# PETITIONERS" or "FOR THE UNITED STATES, AS AMICUS CURIAE, SUPPORTING ...".
.TX_HEADER_RX <- "^(REBUTTAL ARGUMENT|ORAL ARGUMENT|RESUMED ORAL ARGUMENT|FURTHER ORAL ARGUMENT|REARGUMENT)( OF)?\\b"

#' The transcript's numbered body lines, in order, margin numbers removed. The
#' page number and the "Official" / "Heritage Reporting" furniture are not
#' numbered, so they fall away on their own.
tx_lines <- function(pages) {
  lines <- unlist(strsplit(pages, "\n", fixed = TRUE))
  m <- str_match(lines, "^\\s{0,3}(\\d{1,2})(?:\\s+(.*))?$")
  keep <- !is.na(m[, 1]) & as.integer(m[, 2]) <= 25L
  str_squish(coalesce(m[keep, 3], ""))
}

#' Which side an argument header argues for: "pet", "resp" or NA (an amicus
#' supporting neither side, or a header the rules cannot read). Support beats
#' party: "ON BEHALF OF THE RESPONDENTS SUPPORTING PETITIONERS" is the
#' petitioner's side of the lectern.
tx_header_side <- function(h) {
  h <- toupper(str_squish(h %||% ""))
  sup <- str_match(h, "(?:SUPPORTING|IN SUPPORT OF)\\s+(?:THE\\s+)?([A-Z]+)")[, 2]
  if (!is.na(sup)) {
    if (str_detect(sup, "^(PETITIONER|APPELLANT|PLAINTIFF|REVERSAL|VACATUR|MOVANT)")) return("pet")
    if (str_detect(sup, "^(RESPONDENT|APPELLEE|DEFENDANT|AFFIRMANCE|JUDGMENT)")) return("resp")
    return(NA_character_)
  }
  # First role named wins: in a consolidated argument the lead docket comes
  # first ("ON BEHALF OF THE PETITIONERS IN 20-1199 AND THE RESPONDENTS IN 21-707").
  p <- str_locate(h, "PETITIONER|APPELLANT|PLAINTIFF|MOVANT")[1, 1]
  r <- str_locate(h, "RESPONDENT|APPELLEE|DEFENDANT")[1, 1]
  if (is.na(p) && is.na(r)) return(NA_character_)
  if (is.na(r) || (!is.na(p) && p < r)) "pet" else "resp"
}

#' Who is speaking, as a role and a key: a Justice by surname ("Kagan",
#' "Roberts"), anyone else by their label.
TX_JUSTICES <- c("Roberts", "Kennedy", "Thomas", "Ginsburg", "Breyer", "Alito", "Sotomayor",
                 "Kagan", "Gorsuch", "Kavanaugh", "Barrett", "Jackson")
tx_speaker <- function(label) {
  is_j <- str_detect(label, "^(CHIEF )?JUSTICE ")
  key <- ifelse(is_j, str_to_title(str_remove(label, "^(CHIEF )?JUSTICE ")), str_squish(label))
  # The reporters' typos: "JUSTICE SOTOYMAYOR", "CHIEF JUSTICE ROBERT".
  fix <- is_j & !key %in% TX_JUSTICES
  if (any(fix)) key[fix] <- vapply(key[fix], function(k) {
    d <- adist(k, TX_JUSTICES)[1, ]
    if (min(d) <= 2) TX_JUSTICES[which.min(d)] else k
  }, character(1))
  tibble(role = ifelse(is_j, "justice", "advocate"), speaker = key)
}

.tx_words <- function(x) lengths(str_split(str_squish(x), "\\s+")) * (nzchar(str_squish(x)))

#' Parse one transcript. `pages` is pdftools::pdf_text() output. Returns a list:
#'   turns    -- one row per turn: seq, segment, role, speaker, words, text
#'   segments -- one row per argument segment: segment, header, advocate, side,
#'               rebuttal, amicus
#'   checks   -- what the parse could and could not find, for the quality report
parse_transcript <- function(pages) {
  ln <- tx_lines(pages)
  start <- which(str_detect(ln, "^P ?R ?O ?C ?E ?E ?D ?I ?N ?G ?S$"))[1]
  if (is.na(start)) start <- which(str_detect(ln, .TX_SPEAKER_RX))[1]
  end <- which(str_detect(ln, "^\\(Whereupon"))
  end <- end[!is.na(start) & end > start]
  end <- if (length(end)) end[length(end)] - 1L else length(ln)
  checks <- list(start = !is.na(start), submitted = any(str_detect(ln, "submitted\\.\\)")),
                 lines = length(ln))
  empty <- list(turns = tibble(seq = integer(), segment = integer(), role = character(),
                               speaker = character(), words = integer(), text = character()),
                segments = tibble(segment = integer(), header = character(), advocate = character(),
                                  side = character(), rebuttal = logical(), amicus = logical(),
                                  side_inferred = logical()),
                checks = checks)
  if (is.na(start)) return(empty)
  body <- ln[(start + 1L):max(start + 1L, end)]
  body <- body[nzchar(body)]
  # Stage directions: "(Laughter.)", "(10:05 a.m.)".
  body <- str_squish(str_remove_all(body, "\\((Laughter|Recess|Pause|Inaudible|Crosstalk|Simultaneous speaking|\\d{1,2}:\\d{2} [ap]\\.m\\.)[^)]*\\)"))
  body <- body[nzchar(body)]
  # "MR. McCONNELL:" and "ORAL ARGUMENT OF S. MICHAEL McCOLLOCH" are capitals to
  # everyone but a regex: the lower-case "c" made 22-859's whole respondent
  # argument read as more of the petitioner's. Raise the prefix wherever it
  # opens a capitalised name.
  body <- str_replace_all(body, "\\b(Mc|Mac|De|Di|Da|Du|La|Le|Van|Von|St)(?=[A-Z]{2})", toupper)

  turns <- list(); segs <- list()
  seg <- 0L; cur <- NULL; buf <- character()
  flush <- function() {
    if (!is.null(cur)) turns[[length(turns) + 1L]] <<- tibble(segment = cur$segment, label = cur$label,
                                                                text = paste(buf, collapse = " "))
    cur <<- NULL; buf <<- character()
  }
  i <- 1L
  while (i <= length(body)) {
    l <- body[i]
    if (str_detect(l, .TX_HEADER_RX) && l == toupper(l)) {
      flush()
      h <- l; j <- i + 1L
      # The header runs on in capitals until the next speaker label.
      while (j <= length(body) && body[j] == toupper(body[j]) && !str_detect(body[j], .TX_SPEAKER_RX) &&
             !str_detect(body[j], .TX_HEADER_RX)) { h <- paste(h, body[j]); j <- j + 1L }
      seg <- seg + 1L
      adv <- str_squish(str_match(h, "ARGUMENT OF ([^,]+?)(?:,? (?:JR|SR|III|II|ESQ)\\.?)*(?:,| ON BEHALF| FOR | AS AMICUS|$)")[, 2])
      segs[[length(segs) + 1L]] <- tibble(segment = seg, header = h, advocate = adv,
                                          side = tx_header_side(h),
                                          rebuttal = str_detect(h, "^REBUTTAL"),
                                          amicus = str_detect(h, "AMICUS|AMICI"),
                                          side_inferred = FALSE)
      i <- j; next
    }
    m <- str_match(l, .TX_SPEAKER_RX)
    if (!is.na(m[1, 1])) {
      flush(); cur <- list(segment = seg, label = m[1, 2]); buf <- m[1, 3]
    } else if (!is.null(cur)) buf <- c(buf, l)
    i <- i + 1L
  }
  flush()
  if (!length(turns)) return(empty)
  tt <- bind_rows(turns)
  sp <- tx_speaker(tt$label)
  tt <- tibble(seq = seq_len(nrow(tt)), segment = tt$segment, role = sp$role, speaker = sp$speaker,
               words = as.integer(.tx_words(tt$text)), text = str_squish(tt$text))
  sg <- if (length(segs)) bind_rows(segs) else empty$segments
  # A party's header that names no role -- "ON BEHALF OF THE UNITED STATES"
  # when the United States is a party -- takes the lectern order: the first
  # principal segment is the petitioner's, a later one the respondent's, and a
  # rebuttal is always the petitioner's. An amicus that names no side stays NA.
  if (nrow(sg)) {
    first_principal <- which(!sg$amicus & !sg$rebuttal)[1]
    for (k in which(is.na(sg$side) & !sg$amicus)) {
      sg$side[k] <- if (sg$rebuttal[k] || identical(k, first_principal)) "pet" else "resp"
      sg$side_inferred[k] <- TRUE
    }
  }
  checks$submitted <- checks$submitted || any(str_detect(tail(tt$text, 2), "submitted"))
  checks$turns <- nrow(tt)
  checks$segments <- nrow(sg)
  checks$segments_sided <- sum(!is.na(sg$side))
  list(turns = tt, segments = sg, checks = checks)
}

#' Per-case bench measures from a parsed transcript: for each side, the
#' Justices' turns ("questions" -- every Justice turn in that side's segments,
#' interjections included) and words, overall and per Justice. Segments with no
#' side (amicus supporting neither, unreadable header) count toward neither.
#' `parties_only` drops amicus segments, the definition the literature mostly uses.
tx_bench <- function(p, parties_only = FALSE) {
  if (!nrow(p$turns)) return(NULL)
  sg <- p$segments
  if (parties_only) sg <- sg[!sg$amicus, , drop = FALSE]
  p$turns |>
    filter(role == "justice") |>
    inner_join(sg |> select(segment, side), by = "segment") |>
    filter(!is.na(side)) |>
    group_by(speaker, side) |>
    summarise(turns = n(), words = sum(words), .groups = "drop")
}

# ---- fetching --------------------------------------------------------------------

#' Every transcript the Court lists for `terms` (four-digit), one row per PDF:
#' its dockets (a consolidated argument is one PDF), url, posted date, term.
transcript_index <- function(terms) {
  bind_rows(lapply(terms, function(t) {
    f <- fetch_media_feed("transcripts", t)
    if (!nrow(f)) {
      m <- fetch_transcript_map(t)
      f <- tibble(dkt = names(m), url = unname(m), posted = as.Date(NA))
    }
    as_tibble(f) |> mutate(term = as.integer(t))
  })) |>
    group_by(url, term) |>
    summarise(dkt = first(dkt), dkts = list(unique(dkt)), posted = min(posted), .groups = "drop")
}

#' Download each transcript once into `dir`, paced. Returns the local paths.
download_transcripts <- function(idx, dir, pace = 1) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  vapply(seq_len(nrow(idx)), function(i) {
    fn <- file.path(dir, basename(idx$url[i]))
    if (file.exists(fn) && file.size(fn) > 1000) return(fn)
    ok <- tryCatch({
      resp <- httr2::request(idx$url[i]) |> httr2::req_user_agent(TX_UA) |>
        httr2::req_timeout(60) |> httr2::req_retry(max_tries = 3) |> httr2::req_perform()
      writeBin(httr2::resp_body_raw(resp), fn); TRUE
    }, error = function(e) { message("transcript ", idx$url[i], ": ", conditionMessage(e)); FALSE })
    Sys.sleep(pace)
    if (ok) fn else NA_character_
  }, character(1))
}
