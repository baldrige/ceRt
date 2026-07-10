# Fetch one full term's cases and save them to $OUT_FILE (an rds artifact). Run
# once per term in a separate job so each term is fetched from its own runner IP,
# keeping the sustained per-IP load (and throttling) down.
#
# Env: TERM_YEAR (e.g. "25"), OUT_FILE (default "cases-<term>.rds").

suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse)
  library(httr2); library(jsonlite); library(pdftools); library(htmltools)
})

term <- Sys.getenv("TERM_YEAR")
if (term == "") stop("TERM_YEAR not set")
out <- Sys.getenv("OUT_FILE", unset = paste0("cases-", term, ".rds"))

src <- readLines("R/scotus_dash_new.R")
src <- src[-grep("^scotus_dash\\(", src)]
eval(parse(text = paste(src, collapse = "\n")))

cat("Full-fetching OT", term, "term...\n")
ot <- get_scotus_term(term)
cat("OT", term, ":", nrow(ot), "cases | unresolved:",
    attr(ot, "n_failed") %||% 0, "/", attr(ot, "n_attempted") %||% nrow(ot), "\n")

# Fail the job on a throttle-degraded term so nothing partial gets published.
if (fetch_is_degraded(ot)) {
  cat("OT", term, "fetch degraded by throttling; failing job.\n")
  quit(status = 1)
}

saveRDS(ot, out)
cat("Saved", nrow(ot), "cases to", out, "\n")
