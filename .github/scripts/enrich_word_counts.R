# Corpus pass for the certified word counts (R/word_count.R): every paid,
# resolved petition in the term archives that docketed a word-count
# certificate, fetched once into data-raw/word_counts.json. Resumable -- the
# cache is written every 100 fetches -- so re-run until the count stops growing.
#
# Env: TERMS_GLOB (default "data-raw/ot_*.rds"), OUT (default
# data-raw/word_counts.json), MAX_NEW (default 20000), PACE (seconds between
# fetches, default 0.75 -- at 0.25 the per-IP limit returned empty files for
# nine fetches in ten).
suppressPackageStartupMessages({library(tidyverse); library(jsonlite)})
source("R/cert_funnel.R")   # classify_petitions, funnel_case_type
source("R/word_count.R")

out     <- Sys.getenv("OUT", "data-raw/word_counts.json")
max_new <- as.integer(Sys.getenv("MAX_NEW", "20000"))
pace    <- as.numeric(Sys.getenv("PACE", "0.75"))

sel <- map_dfr(Sys.glob(Sys.getenv("TERMS_GLOB", "data-raw/ot_*.rds")), function(f) {
  cases <- readRDS(f)
  cls <- classify_petitions(cases) |> select(dkt, outcome)
  cases |> left_join(cls, by = "dkt") |>
    mutate(ctype = funnel_case_type(dkt), url = map_chr(events, find_word_count_url)) |>
    filter(ctype == "paid", outcome %in% c("granted", "denied", "gvr", "dismissed"),
           !is.na(url), nzchar(url)) |>
    distinct(dkt, .keep_all = TRUE) |>
    transmute(term = basename(f), dkt, url)
})
cat("paid resolved petitions with a word-count certificate:", nrow(sel), "\n")
print(table(sel$term))
# retry_unparsed: a cached entry with no count is a failed download or a
# certificate the parser could not read, so every re-dispatch takes another
# run at those (with OCR for the scanned ones).
wc <- resolve_word_counts(sel$dkt, sel$url, cache_path = out, max_new = max_new,
                          pace = pace, retry_unparsed = TRUE)
cat("resolved:", sum(!is.na(wc$words)), "of", nrow(wc), "| unparsed:", sum(is.na(wc$words)), "\n")
print(summary(wc$words))
