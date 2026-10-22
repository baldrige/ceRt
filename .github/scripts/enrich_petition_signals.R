# Enrich one term's paid, decided (granted/denied) petitions with the Rule 10
# signals parsed from each petition PDF (R/petition_signals.R). Writes a per-term
# cache JSON; the workflow uploads it as an artifact and a combine step merges the
# terms into data-raw/petition_signals.json. Run from a fresh CI runner IP.
#
# Env: TERM_YEAR (e.g. "2024"), OUT (cache path), MAX_NEW (default 6000),
# SEED_FILES (optional, comma-separated: more caches to seed from -- the
# workflow passes the on-site conference cache from gh-pages).
#
# TERM_YEAR names an archive, and two of them are not ot_YYYY.rds:
#   "2025"     data-raw/snapshot_25.rds, the closed Term that is deliberately not
#              an ot_* file (it would re-base the Funnel and the cert model).
#   "refresh"  data-raw/snapshot_refresh.rds, the petitions the Term archives
#              froze at pending, re-fetched once they were decided.
# Enriching them adds entries to the committed layer and changes no training
# frame: assemble_corpus() joins signals onto the ot_* dockets only, and the
# refresh dockets are still pending there. The Counsel Table's
# against-expectation board is what reads them (make_counsel_expected.R).

suppressPackageStartupMessages({
  library(tidyverse); library(httr2); library(pdftools); library(jsonlite)
})
source("R/cert_funnel.R")        # classify_petitions, funnel_case_type
source("R/conference_dash.R")    # find_petition_url
source("R/petition_signals.R")   # resolve_petition_signals

term    <- Sys.getenv("TERM_YEAR")
out     <- Sys.getenv("OUT", unset = paste0("petition_signals_", term, ".json"))
max_new <- as.integer(Sys.getenv("MAX_NEW", unset = "6000"))
if (!nzchar(term)) stop("TERM_YEAR not set")

f <- if (term == "refresh") "data-raw/snapshot_refresh.rds" else {
  ot <- sprintf("data-raw/ot_%s.rds", term)
  snap <- sprintf("data-raw/snapshot_%s.rds", substr(term, 3, 4))
  if (file.exists(ot)) ot else snap
}
if (!file.exists(f)) stop("no archive for TERM_YEAR=", term, ": ", f)
cat("archive:", f, "\n")
cases <- readRDS(f)
cls <- classify_petitions(cases) |> select(dkt, outcome)

sel <- cases |>
  left_join(cls, by = "dkt") |>
  mutate(ctype = funnel_case_type(dkt),
         url = purrr::map_chr(events, find_petition_url)) |>
  # Every RESOLVED paid petition, not only granted-or-denied. GVR'd and dismissed
  # petitions are negatives in the at-risk grant model, and when only grants and
  # denials were enriched their cues were absent by construction -- which the
  # model read as "no cue means not a grant", inflating the cues' apparent lift
  # at conference five-fold (+0.043 AP measured vs +0.008 real; 2026-09-11).
  filter(ctype == "paid", outcome %in% c("granted", "denied", "gvr", "dismissed"),
         !is.na(url), nzchar(url)) |>
  distinct(dkt, .keep_all = TRUE)

cat("OT", term, "— paid resolved with a petition URL:", nrow(sel), "\n")
# Seed the run's cache from the committed layer, so a re-dispatch fetches only
# what is missing or below PETITION_SIGNALS_VERSION rather than the whole Term
# again from a fresh, empty file -- which is what made a throttled run cost a
# full re-download to repair (2026-10-15).
#
# Also from any SEED_FILES: the daily and conference runs resolve these same
# signals, with the same resolver, for live petitions -- so a closed Term's
# petitions are often already extracted in an on-site cache. A seeded entry
# below PETITION_SIGNALS_VERSION is still re-extracted, exactly as a committed
# one is; a later source replaces an earlier one only at an equal or higher
# version.
committed <- "data-raw/petition_signals.json"
seeds <- c(committed, trimws(strsplit(Sys.getenv("SEED_FILES", ""), ",")[[1]]))
seeds <- seeds[nzchar(seeds) & file.exists(seeds)]
if (!file.exists(out) && length(seeds)) {
  seed <- list()
  for (sf in seeds) {
    have <- tryCatch(fromJSON(sf, simplifyDataFrame = FALSE), error = function(e) list())
    have <- have[intersect(names(have), sel$dkt)]
    for (d in names(have))
      if (is.null(seed[[d]]) || (have[[d]]$sig_v %||% 1L) >= (seed[[d]]$sig_v %||% 1L))
        seed[[d]] <- have[[d]]
    cat("seed:", sf, "->", length(have), "entries for this archive\n")
  }
  if (length(seed)) write_json(seed, out, auto_unbox = TRUE)
  cat("seeded", length(seed), "entries\n")
}
# refresh: an entry below PETITION_SIGNALS_VERSION is re-extracted, so a new
# cue (gvr_ask, v3) reaches the whole corpus by re-dispatching this workflow.
invisible(resolve_petition_signals(sel$dkt, sel$url, cache_path = out, max_new = max_new,
                                   refresh_v1_entries = TRUE))

done <- fromJSON(out, simplifyDataFrame = FALSE)
have_text <- sum(purrr::map_lgl(done, ~ isTRUE((.x$pet_chars %||% 0) > 500)))
stale <- sum(!vapply(sel$dkt, function(d) isTRUE((done[[d]]$sig_v %||% 0L) >= PETITION_SIGNALS_VERSION), logical(1)))
cat("cache entries:", length(done), "| text-extractable:", have_text,
    "| still below v", PETITION_SIGNALS_VERSION, ": ", stale, "\n", sep = "")
if (stale > 0)
  cat("::warning::OT", term, ": ", stale, " petition(s) not at signals v", PETITION_SIGNALS_VERSION,
      " (fetch failed or capped) -- re-dispatch this term; their committed entries are kept.\n", sep = "")
