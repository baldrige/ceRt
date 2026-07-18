# Backfill Question Presented for ONE term's paid petitions. Parses each petition
# PDF (R/qp_extract.R) and writes a per-term QP cache JSON (resolve_qps format:
# docket -> {url, qp}). The workflow uploads it as an artifact and a combine step
# folds all terms into the shared conference QP cache on gh-pages, which
# render_dockets_for() reads. Run one term per fresh CI runner IP (spreads
# supremecourt.gov's PDF rate limit); the cache is resumable via SEED + max_new.
#
# Env: TERM_YEAR (e.g. "2024"), OUT (cache path), MAX_NEW (default 3000),
#      SEED (optional shared cache to seed already-extracted QPs from).

suppressPackageStartupMessages({
  library(tidyverse); library(httr2); library(pdftools); library(jsonlite)
})
source("R/cert_funnel.R")        # funnel_case_type
source("R/conference_dash.R")    # find_petition_url
source("R/qp_extract.R")         # resolve_qps

term    <- Sys.getenv("TERM_YEAR")
out     <- Sys.getenv("OUT", unset = paste0("qp_", term, ".json"))
max_new <- as.integer(Sys.getenv("MAX_NEW", unset = "3000"))
seed    <- Sys.getenv("SEED", unset = "")
if (!nzchar(term)) stop("TERM_YEAR not set")

f <- sprintf("data-raw/ot_%s.rds", term)
if (!file.exists(f)) stop("no archive: ", f)
cases <- readRDS(f)

sel <- cases |>
  mutate(ctype = funnel_case_type(dkt),
         url = purrr::map_chr(events, find_petition_url)) |>
  filter(ctype == "paid", !is.na(url), nzchar(url)) |>
  distinct(dkt, .keep_all = TRUE)
cat("OT", term, "— paid petitions with a petition URL:", nrow(sel), "\n")

# Seed the per-term cache from the shared cache so already-extracted QPs (many
# OT24/OT25 cases are already cached from conference rendering) aren't re-fetched.
if (nzchar(seed) && file.exists(seed)) {
  s <- tryCatch(fromJSON(seed, simplifyVector = FALSE), error = function(e) list())
  s <- s[intersect(names(s), sel$dkt)]
  write_json(s, out, auto_unbox = TRUE)
  cat("seeded", length(s), "already-cached QP(s) for OT", term, "\n")
}

invisible(resolve_qps(sel$dkt, sel$url, cache_path = out, max_new = max_new))
have <- fromJSON(out, simplifyVector = FALSE)
cat("OT", term, "cache entries:", length(have),
    "of", nrow(sel), "paid-with-url\n")
