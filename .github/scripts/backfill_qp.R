# Backfill Question Presented for the historical argued grants. arg_refresh.rds
# is exactly the re-fetched OT17-24 grants, so their petition URLs are in hand.
# resolve_qps() fetches each uncached petition PDF (text layer, OCR fallback),
# extracts the QP, and folds it into the shared conference cache
# ($QP_CACHE) -- so the argument navigator (which reads that cache read-only)
# then shows QP for the older cases too. Best run on a fresh CI runner: ~500 PDF
# fetches trip supremecourt.gov's rate limit from a warmed local IP.
#
# Env: QP_CACHE (path to qp_cache.json), QP_MAX_NEW (fetch cap, default 1000).
suppressPackageStartupMessages({
  library(tidyverse); library(pdftools); library(jsonlite)
})
source("R/qp_extract.R")   # resolve_qps()

# Petition-for-certiorari URL from a case's events (JSON docs_*/links_* layout).
petition_url <- function(events) {
  if (!is.data.frame(events) || nrow(events) == 0) return(NA_character_)
  desc_cols <- str_subset(names(events), "^(docs_|Document_)")
  link_cols <- str_subset(names(events), "^links_")
  if (length(desc_cols) == 0 || length(link_cols) == 0) return(NA_character_)
  for (i in seq_len(nrow(events))) {
    descs <- unlist(events[i, desc_cols], use.names = FALSE)
    links <- unlist(events[i, link_cols], use.names = FALSE)
    hit <- which(!is.na(descs) & str_detect(descs, regex("^Petition", ignore_case = TRUE)))
    hit <- hit[hit <= length(links)]
    if (length(hit) > 0 && !is.na(links[hit[1]])) return(links[hit[1]])
  }
  NA_character_
}

gr <- readRDS("data-raw/arg_refresh.rds") |> distinct(dkt, .keep_all = TRUE)
urls <- map_chr(gr$events, petition_url)
cat("historical grants:", nrow(gr),
    "| with a petition URL:", sum(!is.na(urls) & nzchar(urls)), "\n")

cache <- Sys.getenv("QP_CACHE", unset = "site/conferences/qp_cache.json")
count_cache <- function() if (file.exists(cache))
  length(fromJSON(cache, simplifyVector = FALSE)) else 0L
before <- count_cache()

resolve_qps(gr$dkt, urls, cache_path = cache,
            max_new = as.integer(Sys.getenv("QP_MAX_NEW", "1000")))

cat("cache entries:", before, "->", count_cache(), "(+", count_cache() - before, ")\n")
cat("Done.\n")
