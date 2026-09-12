# word_count.R -----------------------------------------------------------------
# The petition's length as the filer certified it. Rule 33.1(h) requires every
# booklet-format petition to carry a certificate stating its word count, and
# the Clerk dockets it as its own document ("Certificate of Word Count", or a
# "Certificate of Compliance") beside the petition -- or, for the Solicitor
# General and some private filers, inside the proof of service. 91-94% of paid
# petitions across OT19/22/24 carry a separate certificate (measured
# 2026-09-11); it is a page or two, and 14 of 15 sampled parsed on the first
# pattern (the fifteenth was a scanned image, which OCR now reads).
#
# Why this and not the petition text's length: the text length counts the
# appendix whenever it is bound into the same PDF, which 2 petitions in 7 do
# and which is itself a grant-correlated house style. The certified count is
# the body alone, under the Rule's own exclusions, and reads directly against
# the 9,000-word limit.
#
# Same shape as the QP and petition-signal resolvers: a JSON cache keyed by
# docket, incremental and resumable, `max_new` new fetches per run.

suppressPackageStartupMessages({
  library(tidyverse); library(httr2); library(pdftools); library(jsonlite)
})

WORD_COUNT_UA <- "Mozilla/5.0 (ceRt SCOTUS research; +https://supremecourt.report)"
WORD_COUNT_DOC_RE <- "(?i)word\\s*count|certificate\\s+of\\s+compliance"
# The Solicitor General dockets no separate certificate: its Rule 33.1(h)
# certification sits inside the "Proof of Service" document (6 of 6 sampled
# OT24 SG petitions, all parsing). Private filers sometimes combine the two the
# same way, so the proof of service is the fallback for anyone.
WORD_COUNT_FALLBACK_RE <- "(?i)proof\\s+of\\s+service|^\\s*certificate\\b"

# URL of the document that carries the petition's word-count certificate, from
# an `events` tibble in either layout (Document_k/links_k for the archive,
# docs_k/links_k for the JSON build): a "Certificate of Word Count" when one is
# docketed, else the proof of service. Only the "Petition for a writ of
# certiorari filed" entry is read, so a certificate filed with a later brief is
# never mistaken for it. NA when there is neither.
find_word_count_url <- function(events) {
  if (!is.data.frame(events) || nrow(events) == 0) return(NA_character_)
  pcol <- intersect(c("Proceedings and Orders", "text", "Text"), names(events))[1]
  desc_cols <- str_subset(names(events), "^(docs_|Document_)")
  link_cols <- str_subset(names(events), "^links_")
  if (is.na(pcol) || !length(desc_cols) || !length(link_cols)) return(NA_character_)
  rows <- which(str_detect(coalesce(events[[pcol]], ""),
                           "(?i)^Petition for a writ of certiorari filed"))
  for (rx in c(WORD_COUNT_DOC_RE, WORD_COUNT_FALLBACK_RE)) for (i in rows) {
    descs <- unlist(events[i, desc_cols], use.names = FALSE)
    links <- unlist(events[i, link_cols], use.names = FALSE)
    hit <- which(!is.na(descs) & str_detect(descs, rx))
    hit <- hit[hit <= length(links)]
    if (length(hit) && !is.na(links[hit[1]]) && nzchar(links[hit[1]])) return(links[hit[1]])
  }
  NA_character_
}

# The stated count. "contains 8,608 words", "consists of 7,446 words", "a total
# of 2,559 words", "8996 words" -- the verb varies and the comma is optional.
# The first number that precedes "words" wins; a certificate quotes only one.
# NA when nothing matches (a scanned certificate, or an empty fetch).
RX_WORD_COUNT <- "(?i)\\b(\\d{1,2},\\d{3}|\\d{3,5})\\s+words?\\b"
# Some text layers put a space between every character ("8 , 4 9 6 w o r d s")
# or break "words" across a line; a second pass over the text with every space
# removed catches those.
RX_WORD_COUNT_TIGHT <- "(?i)(\\d{1,2},\\d{3}|\\d{3,5})words?\\b"
parse_word_count <- function(text) {
  t <- str_squish(text %||% "")
  m <- str_match(t, RX_WORD_COUNT)[, 2]
  m2 <- str_match(str_remove_all(t, "\\s"), RX_WORD_COUNT_TIGHT)[, 2]
  m <- ifelse(is.na(m), m2, m)
  n <- suppressWarnings(as.integer(str_remove_all(m, ",")))
  # A count under 100 or over 40,000 is not a petition's; treat as unparsed.
  ifelse(!is.na(n) & n >= 100 & n <= 40000, n, NA_integer_)
}

fetch_word_count_text <- function(url) {
  if (is.na(url) || !nzchar(url)) return("")
  tf <- tempfile(fileext = ".pdf")
  on.exit(unlink(tf), add = TRUE)
  ok <- tryCatch({
    request(url) |> req_user_agent(WORD_COUNT_UA) |> req_timeout(60) |>
      req_error(is_error = \(resp) FALSE) |> req_perform(path = tf)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) return("")
  txt <- tryCatch(paste(suppressWarnings(pdf_text(tf)), collapse = "\n"), error = function(e) "")
  # A scanned certificate (a page image, no text layer) is common in the older
  # terms -- 2 of 6 sampled from OT19 -- and a garbage text layer ("^POOwords")
  # is the same problem wearing text. Either way the count did not parse, the
  # document is a page or two, and tesseract is on the runners already: OCR
  # the first page and keep it if IT parses.
  if (is.na(parse_word_count(txt)) && file.exists(tf) && file.size(tf) > 0 &&
      requireNamespace("tesseract", quietly = TRUE)) {
    ocr <- tryCatch(paste(suppressWarnings(pdftools::pdf_ocr_text(tf, pages = 1)), collapse = "\n"),
                    error = function(e) "")
    if (!is.na(parse_word_count(ocr)) || nchar(str_squish(txt)) < 20) txt <- ocr
  }
  txt
}

# Resolve certified word counts for `dkts` (with matching certificate `urls`),
# backed by a JSON cache keyed by docket: {words: int|null, chars: int}. A
# docket whose certificate yielded no count is cached too (words null), so it
# is not re-fetched. Returns tibble(dkt, words) with NA where unresolved or
# unparsed. `pace` seconds between fetches keeps a corpus pass polite.
#
# `retry_unparsed` re-fetches cached dockets that hold no count: an empty
# download (a corpus pass paced at 0.25 s between fetches came back empty for
# 8,593 of 9,514 on 2026-09-11 -- the per-IP rate limit CLAUDE.md warns about
# for the docket JSON applies to the PDFs too), or text the parser could not
# read, which a widened pattern or the OCR fallback may now handle. A
# certificate that genuinely states no number costs one fetch per pass.
#
# `max_consecutive_empty`: once that many downloads in a row come back with no
# text, the runner's IP is being throttled and every further fetch this run
# would only write another empty entry -- the first conferences.yml run after
# this shipped (2026-09-12) had already pulled ~1,200 petition PDFs for QPs and
# cues, and all 595 certificates it then fetched came back empty. Stop, keep
# what was learned, and let the next run (with retry_unparsed) take the rest.
resolve_word_counts <- function(dkts, urls, cache_path, max_new = 0L, pace = 0.75,
                                retry_unparsed = FALSE, max_consecutive_empty = 10L) {
  cache <- if (file.exists(cache_path)) fromJSON(cache_path, simplifyDataFrame = FALSE) else list()
  is_cached <- function(dk) !is.null(cache[[dk]]) &&
    (!retry_unparsed || !is.na(word_count_of(cache[[dk]])))
  todo <- dkts[!vapply(dkts, is_cached, logical(1)) & !is.na(urls) & nzchar(urls)]
  todo <- head(unique(todo), max_new)
  if (length(todo)) {
    message("word counts: fetching ", length(todo), " of ",
            sum(!vapply(dkts, is_cached, logical(1))), " uncached (cap ", max_new, ")")
    url_of <- setNames(urls, dkts)
    empties <- 0L
    for (i in seq_along(todo)) {
      dk <- todo[i]
      txt <- fetch_word_count_text(url_of[[dk]])
      # An unparsed count is written as JSON null, not the string "NA" that
      # jsonlite's default would emit for an NA integer.
      cache[[dk]] <- list(words = parse_word_count(txt), chars = nchar(txt))
      empties <- if (nchar(txt) == 0) empties + 1L else 0L
      if (empties >= max_consecutive_empty) {
        message("word counts: ", empties, " empty downloads in a row after ", i,
                " fetch(es) -- throttled; stopping this run (retry next run)")
        break
      }
      if (i %% 100 == 0) { write_json(cache, cache_path, auto_unbox = TRUE, na = "null"); message("  ...", i, "/", length(todo)) }
      if (pace > 0) Sys.sleep(pace)
    }
    write_json(cache, cache_path, auto_unbox = TRUE, na = "null")
    message("word counts: ", sum(!is.na(vapply(todo[seq_len(i)], function(d) word_count_of(cache[[d]]), integer(1)))),
            " parsed of ", i, " fetched")
  }
  tibble(dkt = dkts, words = vapply(dkts, function(dk) word_count_of(cache[[dk]]), integer(1), USE.NAMES = FALSE))
}

# The integer count out of a cache entry: NA for a missing entry, a null, or
# the string "NA" the first corpus pass wrote.
word_count_of <- function(entry) {
  w <- entry$words
  if (is.null(w) || length(w) == 0) return(NA_integer_)
  suppressWarnings(as.integer(w))
}

# The committed corpus-wide file (data-raw/word_counts.json) is read by
# load_word_counts() in cert_model.R, which needs no pdftools -- the model is
# scored from many entry points that never fetch a PDF.

# Merge resolved counts into a petition-signals map (docket -> named list), as
# `words`, so score_case(signals = ) sees one entry per docket. Dockets not in
# the map get a fresh entry holding only the count.
attach_word_counts <- function(signals_map, counts) {
  counts <- counts[!is.na(counts$words), , drop = FALSE]
  for (i in seq_len(nrow(counts))) {
    dk <- counts$dkt[i]
    e <- signals_map[[dk]]; if (is.null(e)) e <- list()
    e$words <- counts$words[i]
    signals_map[[dk]] <- e
  }
  signals_map
}
