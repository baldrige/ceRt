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
  filter(ctype == "paid", outcome %in% c("granted", "denied"),
         !is.na(url), nzchar(url)) |>
  distinct(dkt, .keep_all = TRUE)

cat("OT", term, "— paid granted/denied with a petition URL:", nrow(sel), "\n")
invisible(resolve_petition_signals(sel$dkt, sel$url, cache_path = out, max_new = max_new))

done <- fromJSON(out, simplifyDataFrame = FALSE)
have_text <- sum(purrr::map_lgl(done, ~ isTRUE((.x$pet_chars %||% 0) > 500)))
cat("cache entries:", length(done), "| text-extractable:", have_text, "\n")
