# Backfill Question Presented for ONE term's petitions (paid, IFP or both).
# Parses each petition PDF (R/qp_extract.R) and writes a per-term QP cache JSON
# (resolve_qps format: docket -> {url, qp}). The workflow uploads it as an
# artifact and a combine step folds all terms into the shared conference QP
# cache on gh-pages, which render_dockets_for() and the questions index read.
# Run one term per fresh CI runner IP; resumable via SEED + TRIED_SEED.
#
# This does not go through resolve_qps(), for three reasons that only matter at
# this scale (an IFP Term is 3,000-5,000 petitions):
#
#   * A petition that yields no question is REMEMBERED (the ledger, TRIED). The
#     daily's resolver leaves a miss uncached so it retries, which is right for
#     a few dozen recent petitions and fatal here: the misses sit at the head
#     of the list, and by the third dispatch a run is spent re-reading the same
#     handwritten forms. A ledger entry is docket -> the URL that was read, so
#     a re-filed petition is tried again.
#   * A PDF that could not be DOWNLOADED is not a miss. It is counted, never
#     written to the ledger, and ten in a row stop the run: that is throttling,
#     and carrying on would only record a few thousand petitions as tried.
#   * Downloads are paced with the docket fetch's own scotus_pace(). pdftools
#     fetching a URL itself is unpaced, and text-layer extraction takes ~0.3 s,
#     so a run of them goes out at ~3 requests a second.
#
# Env: TERM_YEAR (e.g. "2024"), OUT (cache path), TRIED_OUT (ledger path),
#      MAX_NEW (default 3000), CASE_TYPES ("paid", "ifp" or "paid,ifp"),
#      SEED / TRIED_SEED (the published cache and ledger, optional),
#      BUDGET_MIN (stop cleanly after this many minutes; default 100).

suppressPackageStartupMessages({
  library(tidyverse); library(httr2); library(pdftools); library(jsonlite)
})
source("R/scotus_dash_new.R")    # scotus_req, scotus_pace
source("R/cert_funnel.R")        # funnel_case_type
source("R/conference_dash.R")    # find_petition_url
source("R/qp_extract.R")         # extract_qp

term      <- Sys.getenv("TERM_YEAR")
out       <- Sys.getenv("OUT", unset = paste0("qp_", term, ".json"))
tried_out <- Sys.getenv("TRIED_OUT", unset = paste0("qp_tried_", term, ".json"))
max_new   <- as.integer(Sys.getenv("MAX_NEW", unset = "3000"))
seed      <- Sys.getenv("SEED", unset = "")
tseed     <- Sys.getenv("TRIED_SEED", unset = "")
budget    <- as.numeric(Sys.getenv("BUDGET_MIN", unset = "100")) * 60
if (!nzchar(term)) stop("TERM_YEAR not set")
t_start <- Sys.time()

f <- sprintf("data-raw/ot_%s.rds", term)
if (!file.exists(f)) stop("no archive: ", f)
cases <- readRDS(f)

# CASE_TYPES: which petitions, as funnel_case_type() names them.
types <- strsplit(Sys.getenv("CASE_TYPES", unset = "paid"), "[, ]+")[[1]]
sel <- cases |>
  mutate(ctype = funnel_case_type(dkt),
         url = purrr::map_chr(events, find_petition_url)) |>
  filter(ctype %in% types, !is.na(url), nzchar(url)) |>
  distinct(dkt, .keep_all = TRUE)
cat("OT", term, "—", paste(types, collapse = "+"), "petitions with a petition URL:", nrow(sel), "\n")

read_obj <- function(p) if (nzchar(p) && file.exists(p))
  tryCatch(fromJSON(p, simplifyVector = FALSE), error = function(e) list()) else list()
save_obj <- function(x, p) {
  # An empty list serialises as [], which the next reader takes for an array.
  if (length(x)) write_json(x, p, auto_unbox = TRUE) else writeLines("{}", p)
}

# Seed from the published cache and ledger, cut to this Term's petitions.
cache <- read_obj(seed);  cache <- cache[intersect(names(cache), sel$dkt)]
tried <- read_obj(tseed); tried <- tried[intersect(names(tried), sel$dkt)]
cat("seeded", length(cache), "cached QP(s) and", length(tried), "known miss(es) for OT", term, "\n")

has_qp <- vapply(sel$dkt, function(d) {
  c <- cache[[d]]
  !is.null(c) && identical(unname(c$url), sel$url[match(d, sel$dkt)]) && !qp_looks_stale(c$qp)
}, logical(1))
was_tried <- vapply(seq_len(nrow(sel)), function(i)
  identical(unname(unlist(tried[[sel$dkt[i]]])), sel$url[i]), logical(1))
needs <- sel[!has_qp & !was_tried, c("dkt", "url")]
n_fetch <- min(nrow(needs), max_new)
cat("to read:", nrow(needs), "(this run: up to", n_fetch, ")\n")

# One petition: "ok" with its text, "miss" (a readable PDF with no question
# found), or "down" (no PDF came back).
read_one <- function(url) {
  tmp <- tempfile(fileext = ".pdf"); on.exit(unlink(tmp), add = TRUE)
  got <- tryCatch({ scotus_pace(); req_perform(scotus_req(url) |> req_timeout(120), path = tmp); TRUE },
                  error = function(e) FALSE)
  is_pdf <- got && file.exists(tmp) && file.size(tmp) > 4 &&
    identical(readBin(tmp, "raw", 4L), charToRaw("%PDF"))
  if (!is_pdf) return(list(state = "down"))
  qp <- tryCatch(extract_qp(tmp), error = function(e) "-")
  if (identical(qp, "-")) list(state = "miss") else list(state = "ok", qp = qp)
}

n <- c(ok = 0L, miss = 0L, down = 0L); down_run <- 0L; stopped <- ""
for (i in seq_len(n_fetch)) {
  if (as.numeric(Sys.time() - t_start, units = "secs") > budget) { stopped <- "time budget"; break }
  d <- needs$dkt[i]; u <- needs$url[i]
  r <- read_one(u)
  n[[r$state]] <- n[[r$state]] + 1L
  if (r$state == "ok") {
    cache[[d]] <- list(url = u, qp = r$qp); tried[[d]] <- NULL; down_run <- 0L
  } else if (r$state == "miss") {
    cache[[d]] <- NULL; tried[[d]] <- u; down_run <- 0L
  } else {
    down_run <- down_run + 1L
    if (down_run >= 10L) { stopped <- "10 downloads in a row failed -- throttled"; break }
  }
  if (i %% 100 == 0) {
    save_obj(cache, out); save_obj(tried, tried_out)
    cat(sprintf("  %d/%d  ok %d  miss %d  down %d  (%.0f min)\n", i, n_fetch, n[["ok"]], n[["miss"]],
                n[["down"]], as.numeric(Sys.time() - t_start, units = "mins")))
  }
}
save_obj(cache, out); save_obj(tried, tried_out)
if (nzchar(stopped)) cat("stopped early:", stopped, "\n")
cat("OT", term, "this run: extracted", n[["ok"]], "| no question found", n[["miss"]],
    "| not downloaded", n[["down"]], "\n")
left <- nrow(needs) - n[["ok"]] - n[["miss"]]
cat("OT", term, "cache entries:", length(cache), "of", nrow(sel), "with a petition URL;",
    length(tried), "known misses;", left, "still to read\n")
out_gh <- Sys.getenv("GITHUB_STEP_SUMMARY", unset = "")
if (nzchar(out_gh)) cat(sprintf(
  "OT%s (%s): +%d extracted, %d no question, %d not downloaded; %d still to read%s\n\n",
  term, paste(types, collapse = "+"), n[["ok"]], n[["miss"]], n[["down"]], left,
  if (nzchar(stopped)) paste0(" — stopped: ", stopped) else ""), file = out_gh, append = TRUE)
