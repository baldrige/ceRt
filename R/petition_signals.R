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
# cache in qp_extract.R.

suppressPackageStartupMessages({
  library(tidyverse); library(httr2); library(pdftools); library(jsonlite)
})

PETITION_UA <- "Mozilla/5.0 (ceRt SCOTUS research; +https://supremecourt.report)"

# ---- signal extraction (operates on already-extracted petition text) ----------

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

# Extract the signal booleans + size covariates from petition text. `n_dissent`
# is kept for diagnostics but is length-confounded; prefer the binaries.
extract_petition_signals <- function(text) {
  text <- text %||% ""
  tibble(
    dissent_below   = str_detect(text, RX_DISSENT_HEADER) | str_detect(text, RX_DISSENT_ARG),
    dissent_argued  = str_detect(text, RX_DISSENT_ARG),
    dissent_header  = str_detect(text, RX_DISSENT_HEADER),
    enbanc_dissent  = str_detect(text, RX_ENBANC_DISSENT),
    split_argued    = str_detect(text, RX_SPLIT),
    n_dissent       = str_count(text, "(?i)\\bdissent"),
    pet_chars       = nchar(text)
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

# ---- cache-backed resolver ----------------------------------------------------

# Resolve petition signals for `dkts` (with matching `urls`), backed by a JSON
# cache keyed by docket. Cached dockets are returned immediately; up to `max_new`
# uncached dockets are fetched this run (0 = cache-only). Returns a tibble with
# one row per docket in `dkts` (signals NA when unresolved). Mirrors resolve_qps.
resolve_petition_signals <- function(dkts, urls, cache_path, max_new = 0L) {
  cache <- if (file.exists(cache_path)) {
    fromJSON(cache_path, simplifyDataFrame = FALSE)
  } else list()
  todo <- dkts[!dkts %in% names(cache) & !is.na(urls) & nzchar(urls %|||% "")]
  todo <- head(unique(todo), max_new)
  if (length(todo) > 0) {
    message("petition signals: fetching ", length(todo), " of ",
            sum(!dkts %in% names(cache)), " uncached (cap ", max_new, ")")
    url_of <- setNames(urls, dkts)
    for (i in seq_along(todo)) {
      dk <- todo[i]
      sig <- extract_petition_signals(fetch_petition_text(url_of[[dk]]))
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
  purrr::map_dfr(dkts, function(dk) {
    s <- cache[[dk]]
    if (is.null(s)) tibble(dkt = dk, dissent_below = NA, dissent_argued = NA,
                           dissent_header = NA, enbanc_dissent = NA,
                           split_argued = NA, n_dissent = NA_integer_,
                           pet_chars = NA_integer_)
    else tibble(dkt = dk, dissent_below = isTRUE(s$dissent_below),
                dissent_argued = isTRUE(s$dissent_argued),
                dissent_header = isTRUE(s$dissent_header),
                enbanc_dissent = isTRUE(s$enbanc_dissent),
                split_argued = isTRUE(s$split_argued),
                n_dissent = as.integer(s$n_dissent %||% NA),
                pet_chars = as.integer(s$pet_chars %||% NA))
  })
}

# small helpers reused from the rest of the codebase if not already present
if (!exists("%|||%")) `%|||%` <- function(x, y) if (length(x) == 0 || is.null(x) || is.na(x[1])) y else x
