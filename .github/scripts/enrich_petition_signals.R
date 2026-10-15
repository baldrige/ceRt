# Enrich one term's paid, decided (granted/denied) petitions with the Rule 10
# signals parsed from each petition PDF (R/petition_signals.R). Writes a per-term
# cache JSON; the workflow uploads it as an artifact and a combine step merges the
# terms into data-raw/petition_signals.json. Run from a fresh CI runner IP.
#
# Env: TERM_YEAR (e.g. "2024"), OUT (cache path), MAX_NEW (default 6000).

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

f <- sprintf("data-raw/ot_%s.rds", term)
if (!file.exists(f)) stop("no archive: ", f)
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

cat("OT", term, "— paid granted/denied with a petition URL:", nrow(sel), "\n")
# Seed the run's cache from the committed layer, so a re-dispatch fetches only
# what is missing or below PETITION_SIGNALS_VERSION rather than the whole Term
# again from a fresh, empty file -- which is what made a throttled run cost a
# full re-download to repair (2026-10-15).
committed <- "data-raw/petition_signals.json"
if (!file.exists(out) && file.exists(committed)) {
  have <- fromJSON(committed, simplifyDataFrame = FALSE)
  seed <- have[intersect(names(have), sel$dkt)]
  if (length(seed)) write_json(seed, out, auto_unbox = TRUE)
  cat("seeded", length(seed), "entries from", committed, "\n")
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
