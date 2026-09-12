# petition_signals.R -----------------------------------------------------------
# The Rule 10 signal (a dissent below / a circuit split), recovered by parsing
# the cert PETITION PDF -- which we already have a URL for from the docket, and
# whose appendix reproduces the lower-court opinion (with any dissent) plus the
# petitioner's own "reasons for granting" argument.
#
# Why the petition and not CourtListener: a 2026-07-16 feasibility study found the
# CourtListener MCP unusable for this (100 req/hr rate cap; dissents are NOT in
# cluster metadata -- circuit rulings are stored as one combined opinion; and
# coverage is biased against the unpublished dispositions common among denials).
# The petition PDF is ~94% text-extractable, rate-limit-free (SCOTUS CDN), and
# leakage-safe: the petition is filed at docketing, before any conference.
#
# Signals are cached per docket (the extracted booleans, not the PDF), so a
# corpus enrichment is incremental and resumable -- the same pattern as the QP
# cache in qp_extract.R. Optionally the extracted TEXT is cached too (one gzipped
# file per docket under PETITION_TEXT_DIR), so a pattern change is a local
# re-scan rather than a 10k-PDF re-download.
#
# TWO GENERATIONS OF CUES live side by side, on purpose:
#
#  * v1 (`dissent_below`, `dissent_argued`, `dissent_header`, `enbanc_dissent`,
#    `split_argued`) run the original patterns over the whole text. They are
#    what the fitted baseline model was trained on, so their SEMANTICS MUST NOT
#    CHANGE until the model is refitted -- tightening a feature under a trained
#    coefficient is a silent train/serve skew.
#  * v2 (`dissent_below2`, `dissent_argued2`, `dissent_byline`, `dissent_toc`,
#    `enbanc_dissent2`, `split_argued2`, `split_toc`) segment the PDF first and
#    match each cue only where it means something. Measured on a 32-petition
#    sample (2026-09-11), 104 of the 134 v1 "dissent header" hits were CITATION
#    PARENTHETICALS to this Court's own dissents -- "(Stevens, J., dissenting)"
#    -- which say nothing about the court below; and the v1 split pattern missed
#    the Solicitor General's house style ("circuit conflict", "conflicts with
#    the decisions of other circuits") while matching "conflicting views" and
#    "intra-circuit split". v2 is measured by
#    .github/scripts/measure_petition_signals.R and is NOT yet a model feature.

suppressPackageStartupMessages({
  library(tidyverse); library(httr2); library(pdftools); library(jsonlite)
})

PETITION_UA <- "Mozilla/5.0 (ceRt SCOTUS research; +https://supremecourt.report)"
PETITION_SIGNALS_VERSION <- 2L

# ---- v1 patterns (operate on the whole text; semantics frozen, see above) ------

# A reproduced dissent in the appendix opinion (a judge byline turning to
# "dissenting", or a "dissenting opinion" heading). Fairly specific to an actual
# dissent below, not merely the word "dissent".
RX_DISSENT_HEADER <- str_c(
  "(?i),\\s*JJ?\\.,?\\s+(concurring in part and )?dissenting",
  "|(?i)\\b(circuit|district|chief|senior)\\s+judge[, ]+[A-Z][A-Za-z.'-]+,?\\s+",
  "(with whom[^.\\n]{0,90})?(concurring in part and )?dissent",
  "|(?i)\\bdissenting opinion\\b")
# The petitioner ARGUES dissension below (the strongest separator in testing:
# 44% of grants vs 12% of denials).
RX_DISSENT_ARG <- str_c(
  "(?i)\\b(over|despite|notwithstanding)\\s+(a|the|judge\\s+[A-Z][a-z]+'?s?|",
  "chief\\s+judge\\s+[A-Z][a-z]+'?s?)\\s+(\\w+\\s+){0,2}dissent",
  "|(?i)\\bdivided\\s+(panel|court)\\b",
  "|(?i)\\bpanel\\s+majority\\b",
  "|(?i)\\bdissenting\\s+judges?\\b",
  "|(?i)\\b(two[- ]to[- ]one|2[- ]to[- ]1|2-1)\\b\\s+(decision|panel|vote|ruling)")
# Dissent(s) from denial of rehearing en banc -- a classic, strong cert cue.
RX_ENBANC_DISSENT <- str_c(
  "(?i)dissent(al|ed|ing)?[^.\\n]{0,70}denial of (rehearing )?en banc",
  "|(?i)denial of (rehearing )?en banc[^.\\n]{0,70}dissent",
  "|(?i)dissent(al)?\\s+from (the )?denial of (rehearing )?en banc")
# An asserted circuit split / conflict (Rule 10(a)). Weaker: most petitions claim
# one, so it is far less discriminating than the dissent signals.
RX_SPLIT <- str_c(
  "(?i)\\bcircuit split\\b",
  "|(?i)\\bsplit (among|between) (the )?(circuits|courts|federal)",
  "|(?i)(acknowledged|express(ly)?|entrenched|squarely?|direct(ly)?|deepen(s|ed|ing)?|widen(s|ed)?)",
  "[^.\\n]{0,30}(circuit )?(split|conflict)",
  "|(?i)(creates?|deepens?|widens?|entrenches?|in (direct |square |acknowledged )?)conflict\\b")

# ---- v2 patterns (operate on a SEGMENT of the text) ----------------------------

# A citation parenthetical to a dissent, anywhere: "(Stevens, J., dissenting)",
# "(Breyer, J., dissenting from the grant of a stay)", "(Roberts, C.J.,
# concurring in part and dissenting in part)", "(Callahan, J., dissenting from
# denial of en banc review)". These are quotations of OTHER courts' dissents --
# overwhelmingly this Court's -- and are blanked out before any v2 dissent cue
# is matched. The name class allows a wrapped line inside the parenthetical.
RX_CITE_DISSENT <- str_c(
  "\\([A-Z][A-Za-z.'\u2019 \\n-]{1,40},\\s*(C\\.\\s*)?JJ?\\.,?\\s+",
  "(concurring[^)]{0,40})?dissent[^)]{0,80}\\)")
# A reproduced-dissent BYLINE at the start of a line, the way an appendix
# opinion prints it: "HARTZ, J., dissenting.", "W. EUGENE DAVIS, Circuit Judge,
# dissenting:", "Judge Smith, with whom Judge Jones joins, dissenting". A
# A byline ENDS at "dissenting" (a period, a colon, or the line end, after an
# optional "in part" / "from the denial of rehearing en banc"); a sentence that
# runs on -- "Justice Thomas dissenting and noting that", "Justice Marshall,
# dissenting in Richardson, put it plainly" -- is prose about someone else's
# dissent, and a byline that closes a parenthesis is a wrapped citation.
RX_BYLINE_TAIL <- "dissenting(\\s+in\\s+part)?(\\s+from[^.:\\n)]{0,80})?\\s*([.:]|$)"
RX_BYLINE <- str_c(
  "(?im)^[ \\t]*[A-Z][A-Za-z.'\u2019 -]{1,60},\\s*",
  "(Circuit|District|Chief|Senior|Bankruptcy)?\\s*(Judges?|Justices?|JJ?\\.|C\\.\\s*J\\.),?\\s*",
  "((with whom|joined by)[^.\\n]{0,140})?(concurring in (part|the judgment)[^.\\n]{0,40})?",
  RX_BYLINE_TAIL,
  "|(?im)^[ \\t]*(Chief\\s+)?(Circuit|District|Senior|Bankruptcy)?\\s*(Judges?|Justices?)\\s+",
  "[A-Z][A-Za-z.'\u2019-]+,?\\s*((with whom|joined by)[^.\\n]{0,140})?(concurring in part and )?",
  RX_BYLINE_TAIL)
# The appendix table of contents naming a dissent: "Dissenting opinion of Judge
# Hartz ....... 25a", "Opinion of Davis, J., dissenting", "Dissent of ...". The
# entry must read like an opinion listing, so a table-of-authorities line that
# cites a dissent and happens to end in an appendix page does not count.
RX_TOC_DISSENT <- "(?i)\\bdissent(ing|al)?\\b"
RX_TOC_OPINION <- "(?i)\\b(appendix|app\\.|opinion|order|judgment|decision|court|panel|en banc)\\b"
# A line of the front matter that points into the appendix ("... 12a").
RX_TOC_APPX_LINE <- "(?m)^[^\\n]*\\b\\d{1,3}a\\s*$"
# Split, widened to the SG's vocabulary and narrowed away from generic
# "conflict": the word must sit next to a court word or a split adjective.
RX_SPLIT2 <- str_c(
  "(?i)\\bcircuit[- ](split|conflict)s?\\b",
  "|(?i)\\b(split|conflict|division|divided|disagreement)\\s+(among|between|in|within)\\s+(the\\s+)?",
  "(circuits|courts of appeals|federal courts of appeals|lower courts|state supreme courts|",
  "state courts|federal and state courts|state high courts|circuit courts)",
  "|(?i)\\b(circuits|courts of appeals|lower courts|state supreme courts|state courts|federal courts)",
  "\\s+(are|were|remain|have become|have long been|are now)\\s+(\\w+\\s+){0,2}",
  "(split|divided|in conflict|in disagreement|in disarray)",
  "|(?i)\\bconflicts?\\s+(directly\\s+|squarely\\s+)?with\\s+(the\\s+)?(decisions?|holdings?|rulings?|precedents?|law)\\s+",
  "of\\s+(the\\s+)?(\\w+\\s+){0,3}(circuits?|courts of appeals|state supreme courts?|high courts?)",
  "|(?i)\\b(acknowledged|entrenched|deep|deepening|widening|intractable|square|direct|express|open|",
  "lopsided|mature|longstanding|\\d+[-\u2013]\\d+)\\s+(circuit\\s+)?(split|conflict)\\b",
  "|(?i)\\b(creates?|created|deepens?|deepened|widens?|widened|entrenches?|exacerbates?|cements?)\\s+",
  "(a|an|the)\\s+(\\w+\\s+){0,2}(circuit\\s+)?(split|conflict)\\b")
# "intra-circuit split" is a plea about one court's own inconsistency, not a
# Rule 10(a) conflict; it is blanked before RX_SPLIT2 runs.
RX_INTRA_SPLIT <- "(?i)\\bintra-?\\s?circuit\\s+(split|conflict)s?\\b"

# Structural headings that segment a petition.
RX_REASONS_HEAD <- "(?m)^[ \\t]*([IVX]+\\.\\s+)?(REASONS? FOR (GRANTING|ALLOWING) THE (PETITION|WRIT)|ARGUMENT)\\b"
RX_STATEMENT_HEAD <- "(?m)^[ \\t]*(STATEMENT( OF THE CASE)?|INTRODUCTION|OPINIONS? BELOW)\\s*$"
RX_CONCLUSION_HEAD <- "(?m)^[ \\t]*CONCLUSION\\s*$"
RX_APPENDIX_HEAD <- "(?m)^[ \\t]*(APPENDIX(\\s+[A-Z0-9]{1,3})?|App\\.\\s*1a|1a)\\s*$"

# ---- segmentation ---------------------------------------------------------------

# Split the extracted text into the front matter (cover through the tables of
# contents and authorities), the body (statement + reasons for granting), and
# the appendix (the reproduced opinions below). Every boundary is a heading at
# line start; a missing heading degrades gracefully -- no appendix heading
# means the appendix is separately bound (common) and `appendix` is "". The
# TOC's appendix lines (the ones that end in "12a") are returned on their own,
# since that is where a petition names a dissent below in so many words.
segment_petition <- function(text) {
  text <- text %||% ""
  n <- nchar(text)
  if (n == 0) return(list(front = "", body = "", appendix = "", toc_appx = character(),
                          has_appendix = FALSE))
  reasons_at <- str_locate(text, RX_REASONS_HEAD)[1, "start"]
  # The TOC lists the reasons heading too (with dotted leaders and a page
  # number); the body starts at the LAST heading occurrence within the first
  # 60% of the file, which skips the TOC entry but not an argument heading.
  heads <- str_locate_all(text, RX_REASONS_HEAD)[[1]]
  heads <- heads[heads[, "start"] < 0.6 * n, , drop = FALSE]
  body_at <- if (nrow(heads)) max(heads[, "start"]) else NA_integer_
  stmt <- str_locate_all(text, RX_STATEMENT_HEAD)[[1]]
  stmt <- stmt[stmt[, "start"] < 0.6 * n, , drop = FALSE]
  if (nrow(stmt)) body_at <- min(c(body_at, max(stmt[, "start"])), na.rm = TRUE)
  if (is.na(body_at)) body_at <- 1L
  # The appendix begins at the first appendix heading AFTER the body's
  # conclusion (an appendix opinion can have a CONCLUSION of its own, so the
  # first conclusion after the body start is the one that counts).
  concl <- str_locate_all(text, RX_CONCLUSION_HEAD)[[1]]
  concl <- concl[concl[, "start"] > body_at, , drop = FALSE]
  search_from <- if (nrow(concl)) concl[1, "start"] else body_at
  appx <- str_locate_all(text, RX_APPENDIX_HEAD)[[1]]
  appx <- appx[appx[, "start"] > search_from, , drop = FALSE]
  appx_at <- if (nrow(appx)) appx[1, "start"] else NA_integer_
  front <- str_sub(text, 1, max(1, body_at - 1))
  body <- if (is.na(appx_at)) str_sub(text, body_at, n) else str_sub(text, body_at, appx_at - 1)
  appendix <- if (is.na(appx_at)) "" else str_sub(text, appx_at, n)
  # The appendix contents are listed in the petition's own TOC, in a second
  # TOC at the head of the appendix, or both -- scan both places.
  toc_src <- paste(front, str_sub(appendix, 1, 8000), sep = "\n")
  list(front = front, body = body, appendix = appendix,
       toc_appx = toc_appendix_entries(toc_src), has_appendix = !is.na(appx_at))
}

# The appendix entries of the table of contents, each as ONE string. An entry
# wraps: "Appendix B -- Opinion of Judge Hartz, dissenting from" on one line and
# "the denial of rehearing (Mar. 3, 2024) ......... 25a" on the next, and only
# the last line carries the "25a" page reference. So each page-reference line
# is joined with the lines above it back to the previous page-reference line,
# a blank line, or a heading -- at most three lines, which covers every wrap
# seen in the samples without swallowing the table of authorities.
toc_appendix_entries <- function(front) {
  lines <- str_split(front, "\n")[[1]]
  ends <- str_detect(lines, "\\b\\d{1,3}a\\s*$")
  stops <- ends | !nzchar(str_trim(lines)) | str_detect(lines, "\\.{2,}\\s*\\d+\\s*$|^[ \\t]*[A-Z][A-Z ]{3,}\\s*$")
  vapply(which(ends), function(i) {
    j <- i
    while (j > 1 && (i - j) < 3 && !stops[j - 1]) j <- j - 1
    str_squish(paste(lines[j:i], collapse = " "))
  }, "")
}

# Blank out every citation parenthetical to a dissent (keeps offsets stable).
strip_dissent_citations <- function(text) {
  str_replace_all(text, RX_CITE_DISSENT, function(m) strrep(" ", nchar(m)))
}

# The appendix is usually a SEPARATE PDF. On a 300-petition sample (2026-09-11)
# 201 of 288 "Petition for a writ of certiorari filed" entries carried a
# document described as "Appendix" beside the "Petition" -- so the reproduced
# opinion below, dissent included, is in a file the petition-only fetch never
# opens. This returns that entry's appendix URL(s) from an events table in the
# archive shape (a "Proceedings and Orders" column plus Document_k / links_k or
# docs_k / links_k pairs), or character() when there is none.
find_appendix_urls <- function(events) {
  if (!is.data.frame(events) || nrow(events) == 0) return(character())
  pcol <- intersect(c("Proceedings and Orders", "text", "Text"), names(events))[1]
  desc_cols <- str_subset(names(events), "^(docs_|Document_)")
  link_cols <- str_subset(names(events), "^links_")
  if (is.na(pcol) || !length(desc_cols) || !length(link_cols)) return(character())
  rows <- which(str_detect(events[[pcol]] %||% "", "(?i)^Petition for a writ of certiorari filed"))
  out <- character()
  for (i in rows) {
    descs <- unlist(events[i, desc_cols], use.names = FALSE)
    links <- unlist(events[i, link_cols], use.names = FALSE)
    hit <- which(!is.na(descs) & str_detect(descs, "(?i)^\\s*appendix"))
    hit <- hit[hit <= length(links)]
    out <- c(out, links[hit][!is.na(links[hit])])
  }
  unique(out[nzchar(out)])
}

# ---- signal extraction (operates on already-extracted petition text) ----------

# Extract the signal booleans + size covariates from petition text. `n_dissent`
# is kept for diagnostics but is length-confounded; prefer the binaries. The v1
# columns are computed exactly as before (from the petition text ALONE, so a
# separately filed appendix cannot move them); the v2 columns follow, and they
# read `appendix_text` -- the separately filed appendix PDF(s), concatenated --
# wherever the petition's own appendix would be read.
extract_petition_signals <- function(text, appendix_text = "") {
  text <- text %||% ""
  appendix_text <- appendix_text %||% ""
  seg <- segment_petition(text)
  has_appendix <- seg$has_appendix || nzchar(appendix_text)
  body_clean <- strip_dissent_citations(seg$body)
  appx_clean <- strip_dissent_citations(paste(seg$appendix, appendix_text, sep = "\n"))
  # With no appendix at all the reproduced opinions may still be in-line
  # (some petitions print "APPENDIX" only on a cover page pdftools drops), so
  # the byline test falls back to the whole text; a line-start byline outside
  # a citation is still specific.
  byline_src <- if (has_appendix) appx_clean else strip_dissent_citations(text)
  # A separate appendix opens with its own table of contents.
  toc_entries <- unique(c(seg$toc_appx,
                          if (nzchar(appendix_text)) toc_appendix_entries(str_sub(appendix_text, 1, 8000))))
  toc_txt <- paste(toc_entries, collapse = "\n")
  body_split <- str_replace_all(body_clean, RX_INTRA_SPLIT, function(m) strrep(" ", nchar(m)))
  front_split <- str_replace_all(seg$front, RX_INTRA_SPLIT, function(m) strrep(" ", nchar(m)))
  n_cite <- str_count(text, RX_CITE_DISSENT)
  dissent_byline  <- str_detect(byline_src, RX_BYLINE)
  dissent_toc     <- any(str_detect(toc_entries, RX_TOC_DISSENT) & str_detect(toc_entries, RX_TOC_OPINION))
  dissent_argued2 <- str_detect(body_clean, RX_DISSENT_ARG)
  tibble(
    # ---- v1 (frozen) ----
    dissent_below   = str_detect(text, RX_DISSENT_HEADER) | str_detect(text, RX_DISSENT_ARG),
    dissent_argued  = str_detect(text, RX_DISSENT_ARG),
    dissent_header  = str_detect(text, RX_DISSENT_HEADER),
    enbanc_dissent  = str_detect(text, RX_ENBANC_DISSENT),
    split_argued    = str_detect(text, RX_SPLIT),
    n_dissent       = str_count(text, "(?i)\\bdissent"),
    pet_chars       = nchar(text),
    # ---- v2 (segmented; not yet a model feature) ----
    dissent_byline  = dissent_byline,
    dissent_toc     = dissent_toc,
    dissent_argued2 = dissent_argued2,
    dissent_below2  = dissent_byline | dissent_toc | dissent_argued2,
    enbanc_dissent2 = str_detect(paste(body_clean, toc_txt, appx_clean, sep = "\n"), RX_ENBANC_DISSENT),
    split_argued2   = str_detect(body_split, RX_SPLIT2),
    split_toc       = str_detect(front_split, RX_SPLIT2),
    n_dissent_cite  = n_cite,
    n_dissent_net   = pmax(0L, str_count(text, "(?i)\\bdissent") - n_cite),
    has_appendix    = has_appendix,
    appx_chars      = nchar(appendix_text),
    sig_v           = PETITION_SIGNALS_VERSION
  )
}

# ---- fetching -----------------------------------------------------------------

# Download a petition PDF and return its extracted text ("" on any failure or a
# scanned/no-text PDF -- OCR is intentionally NOT attempted here; scanned IFP
# petitions are rare in the paid docket this feature targets).
fetch_petition_text <- function(url) {
  if (is.na(url) || !nzchar(url)) return("")
  tf <- tempfile(fileext = ".pdf")
  on.exit(unlink(tf), add = TRUE)
  ok <- tryCatch({
    request(url) |> req_user_agent(PETITION_UA) |> req_timeout(90) |>
      req_error(is_error = \(resp) FALSE) |> req_perform(path = tf)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) return("")
  tryCatch(paste(suppressWarnings(pdf_text(tf)), collapse = "\n"),
           error = function(e) "")
}

# The optional text cache: one gzipped UTF-8 file per docket under `text_dir`
# (default the PETITION_TEXT_DIR env, blank = no text cache). A cached "" (a
# PDF that yielded no text) is kept as an empty file so it is not re-fetched.
petition_text_path <- function(dkt, text_dir) {
  file.path(text_dir, paste0(str_replace_all(dkt, "[^A-Za-z0-9-]", "_"), ".txt.gz"))
}
get_petition_text <- function(dkt, url, text_dir = Sys.getenv("PETITION_TEXT_DIR", "")) {
  if (!nzchar(text_dir)) return(fetch_petition_text(url))
  p <- petition_text_path(dkt, text_dir)
  if (file.exists(p)) {
    return(tryCatch(paste(readLines(gzfile(p, encoding = "UTF-8"), warn = FALSE),
                          collapse = "\n"), error = function(e) ""))
  }
  txt <- fetch_petition_text(url)
  dir.create(text_dir, showWarnings = FALSE, recursive = TRUE)
  con <- gzfile(p, "w", encoding = "UTF-8"); on.exit(close(con), add = TRUE)
  writeLines(txt, con)
  txt
}

# ---- cache-backed resolver ----------------------------------------------------

V1_FIELDS <- c("dissent_below", "dissent_argued", "dissent_header", "enbanc_dissent", "split_argued")
V2_FIELDS <- c("dissent_byline", "dissent_toc", "dissent_argued2", "dissent_below2",
               "enbanc_dissent2", "split_argued2", "split_toc", "has_appendix")

# Resolve petition signals for `dkts` (with matching `urls`), backed by a JSON
# cache keyed by docket. Cached dockets are returned immediately; up to `max_new`
# uncached dockets are fetched this run (0 = cache-only). Returns a tibble with
# one row per docket in `dkts` (signals NA when unresolved). Mirrors resolve_qps.
#
# A cache entry written by the v1 extractor has no v2 columns; they come back NA
# (not FALSE) so a consumer can tell "not measured" from "measured, absent".
# `refresh_v1_entries = TRUE` re-extracts those entries (from the text cache when
# present, else re-downloading), counting against `max_new`.
#
# `appx_urls`, when given, is a list parallel to `dkts` of each petition's
# separately filed appendix URL(s) (see find_appendix_urls()); they are fetched
# (and text-cached as "<dkt>.appx<i>") and read by the v2 dissent cues. The
# daily and the enrichment workflow do not pass them yet, so their v2 columns
# see only the petition PDF.
resolve_petition_signals <- function(dkts, urls, cache_path, max_new = 0L,
                                     text_dir = Sys.getenv("PETITION_TEXT_DIR", ""),
                                     refresh_v1_entries = FALSE, appx_urls = NULL) {
  cache <- if (file.exists(cache_path)) {
    fromJSON(cache_path, simplifyDataFrame = FALSE)
  } else list()
  is_current <- function(dk) !is.null(cache[[dk]]) &&
    (!refresh_v1_entries || isTRUE((cache[[dk]]$sig_v %||% 1L) >= PETITION_SIGNALS_VERSION))
  todo <- dkts[!vapply(dkts, is_current, logical(1)) & !is.na(urls) & nzchar(urls %|||% "")]
  todo <- head(unique(todo), max_new)
  if (length(todo) > 0) {
    message("petition signals: fetching ", length(todo), " of ",
            sum(!vapply(dkts, is_current, logical(1))), " uncached (cap ", max_new, ")")
    url_of <- setNames(urls, dkts)
    appx_of <- if (is.null(appx_urls)) NULL else setNames(appx_urls, dkts)
    for (i in seq_along(todo)) {
      dk <- todo[i]
      au <- if (is.null(appx_of)) character() else appx_of[[dk]] %||% character()
      au <- au[!is.na(au) & nzchar(au)]
      appx_txt <- paste(vapply(seq_along(au), function(j)
        get_petition_text(paste0(dk, ".appx", j), au[j], text_dir), ""), collapse = "\n")
      sig <- extract_petition_signals(get_petition_text(dk, url_of[[dk]], text_dir), appx_txt)
      cache[[dk]] <- as.list(sig)
      # Flush periodically so a crash/timeout keeps progress (resumable).
      if (i %% 50 == 0) {
        write_json(cache, cache_path, auto_unbox = TRUE)
        message("  ...", i, "/", length(todo))
      }
    }
    write_json(cache, cache_path, auto_unbox = TRUE)
  }
  # Assemble one row per requested docket.
  lgl_or_na <- function(s, k, measured) if (!measured) NA else isTRUE(s[[k]])
  purrr::map_dfr(dkts, function(dk) {
    s <- cache[[dk]]
    if (is.null(s)) {
      return(tibble(dkt = dk, dissent_below = NA, dissent_argued = NA,
                    dissent_header = NA, enbanc_dissent = NA, split_argued = NA,
                    n_dissent = NA_integer_, pet_chars = NA_integer_,
                    !!!setNames(rep(list(NA), length(V2_FIELDS)), V2_FIELDS),
                    n_dissent_cite = NA_integer_, n_dissent_net = NA_integer_,
                    appx_chars = NA_integer_, sig_v = NA_integer_))
    }
    v2 <- isTRUE((s$sig_v %||% 1L) >= 2L)
    tibble(dkt = dk, dissent_below = isTRUE(s$dissent_below),
           dissent_argued = isTRUE(s$dissent_argued),
           dissent_header = isTRUE(s$dissent_header),
           enbanc_dissent = isTRUE(s$enbanc_dissent),
           split_argued = isTRUE(s$split_argued),
           n_dissent = as.integer(s$n_dissent %||% NA),
           pet_chars = as.integer(s$pet_chars %||% NA),
           !!!setNames(lapply(V2_FIELDS, function(k) lgl_or_na(s, k, v2)), V2_FIELDS),
           n_dissent_cite = as.integer(s$n_dissent_cite %||% NA),
           n_dissent_net = as.integer(s$n_dissent_net %||% NA),
           appx_chars = as.integer(s$appx_chars %||% NA),
           sig_v = as.integer(s$sig_v %||% 1L))
  })
}

# small helpers reused from the rest of the codebase if not already present
if (!exists("%|||%")) `%|||%` <- function(x, y) if (length(x) == 0 || is.null(x) || is.na(x[1])) y else x
