# CI render: the Justices section into $SITE_DIR/justices -- one page per Term
# plus the index. Spec: docs/justices.md; module: R/justices.R.
#
# Needs only the gh-pages checkout and the network: the Granted & Noted List is
# read from arguments/granted_noted.json (written by render_arguments.R), the
# opinion PDF for each decision is named by the Court's slip-opinion feed and
# listing pages (one request per Term), and the parsed lineups are cached in
# justices/lineups.json. No cases/ artifacts, so this can run on its own
# (render-justices.yml) as well as at the end of the weekly.
#
# Env: SITE_DIR (default "site"); LINEUP_MAX_NEW, how many opinion PDFs to
# fetch this run (default 0: cache-only); LINEUP_RETRY=1 re-fetches cached
# decisions whose PDF yielded no lineup; LINEUP_PACE seconds between fetches.

suppressPackageStartupMessages({
  library(tidyverse); library(jsonlite); library(htmltools); library(pdftools); library(httr2)
})

site_dir <- Sys.getenv("SITE_DIR", unset = "site")
max_new  <- as.integer(Sys.getenv("LINEUP_MAX_NEW", unset = "0"))
retry    <- tolower(Sys.getenv("LINEUP_RETRY", "")) %in% c("1", "true", "yes")
pace     <- as.numeric(Sys.getenv("LINEUP_PACE", unset = "0.75"))
# The text measures (R/opinion_text.R) re-read the opinion PDFs the lineups
# were parsed from, under their own cap: a slip is ~1 MB and the archive is
# ~600 of them, so the backlog is spread over runs like the lineups were.
text_max <- as.integer(Sys.getenv("TEXT_MAX_NEW", unset = "0"))
text_retry <- tolower(Sys.getenv("TEXT_RETRY", "")) %in% c("1", "true", "yes")

source("R/palette.R")
source("R/site_nav.R")
source("R/site_meta.R")
source("R/page_style.R")
source("R/granted_noted.R")   # read_granted_noted(), gn_others()
source("R/site_decisions.R")  # fetch_opinion_listing()
source("R/justices.R")
source("R/opinion_text.R")    # resolve_opinion_text(), term_text_stats()

# Refresh the Granted & Noted manifest the same way the weekly does: the
# current, next and prior Terms, any Term the manifest lacks, and any Term
# whose rows carry an older GN_PARSER_VERSION. Two to eleven small PDFs, and
# it is what lets a list-parser fix (Carr v. Saul's dropped row) reach the
# Justices pages from this workflow alone. Never fatal.
gn_fetch <- tryCatch({
  gn_new <- fetch_granted_noted(gn_terms_to_fetch(site_dir))
  if (nrow(gn_new)) { write_granted_noted(site_dir, gn_new)
    cat("Granted & Noted List refreshed for Term(s)", paste(sort(unique(gn_new$term)), collapse = ", "), "\n") }
  TRUE
}, error = function(e) { cat("Granted & Noted List refresh skipped:", conditionMessage(e), "\n"); FALSE })
gn <- read_granted_noted(site_dir)
if (!nrow(gn)) stop("no Granted & Noted manifest at ", file.path(site_dir, "arguments"), " -- run render_arguments.R first")
terms <- sort(unique(gn$term[!is.na(gn$decided)]))
cat("Granted & Noted List:", nrow(gn), "rows;", sum(!is.na(gn$decided)), "decided across Terms",
    paste(terms, collapse = ", "), "\n")

# Opinion URLs, only for the Terms that still have uncached decisions. Two
# sources: the Court's slip-opinion feed (one request per Term), and, for a
# decision the feed does not name -- seven of OT20's on 2026-09-14, the tail of
# a Term the preliminary print had not reached -- the docket page already on
# the site, which carries the slip-opinion link from the docket JSON (OT19 on).
lineups <- read_lineups(site_dir)
urls <- character()
docket_page_url <- function(dk) {
  p <- file.path(site_dir, "cases", paste0(dk, ".html"))
  if (!file.exists(p)) return(NA_character_)
  txt <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "")
  m <- regmatches(txt, regexpr("https://www\\.supremecourt\\.gov/opinions/[0-9]+pdf/[^'\"]+\\.pdf", txt))
  if (length(m)) m[1] else NA_character_
}
is_current <- function(k) !is.null(lineups[[k]]) && (!retry || (isTRUE(lineups[[k]]$parsed) && identical(lineups[[k]]$pv, LINEUP_PARSER_VERSION)))
texts <- read_opinion_text(site_dir)
text_current <- function(k) !is.null(texts[[k]]) && (!text_retry || (isTRUE(texts[[k]]$ok) && identical(texts[[k]]$tv, OPINION_TEXT_VERSION)))
need <- terms[vapply(terms, function(t) {
  d <- gn_decisions(gn, t)$dkt
  (max_new > 0 && any(!vapply(d, is_current, logical(1)))) || (text_max > 0 && any(!vapply(d, text_current, logical(1))))
}, logical(1))]
if ((max_new > 0 || text_max > 0) && length(need)) {
  cat("Fetching opinion listings for Term(s)", paste(need, collapse = ", "), "\n")
  lst <- fetch_opinion_listing(sprintf("%02d", as.integer(need)), kinds = "slipopinion")
  lst <- lst |> filter(!is.na(url)) |> distinct(dkt, .keep_all = TRUE)
  urls <- setNames(lst$url, lst$dkt)
  n_feed <- length(urls)
  for (t in need) for (dk in gn_decisions(gn, t)$dkt) if (is.na(urls[dk])) {
    u <- docket_page_url(dk); if (!is.na(u)) urls[dk] <- u
  }
  cat("Opinion PDFs named:", n_feed, "by the feed,", length(urls) - n_feed, "from docket pages\n")
}
# Every Term goes through resolve_lineups() even at cap 0: that is where a
# cached entry parsed under an older grammar is re-read from its text.
for (t in terms) {
  d <- gn_decisions(gn, t)
  lineups <- resolve_lineups(d, urls, site_dir, max_new = max(max_new, 0L), pace = pace, retry = retry)
  # One cap for the whole run, spent oldest Term first.
  max_new <- max_new - (attr(lineups, "n_fetched") %||% 0L)
}
n_parsed <- sum(vapply(lineups, function(e) isTRUE(e$parsed), logical(1)))
cat("Lineups cached:", length(lineups), "| parsed:", n_parsed, "\n")

# The text measures, newest Term first: the current Term's page is the one
# read, and the cap is spent there before the archive.
for (t in rev(terms)) {
  d <- gn_decisions(gn, t)
  texts <- resolve_opinion_text(d, urls, site_dir, max_new = max(text_max, 0L), pace = pace, retry = text_retry)
  text_max <- text_max - (attr(texts, "n_fetched") %||% 0L)
}
cat("Opinion text cached:", length(texts), "| measured:", sum(vapply(texts, function(e) isTRUE(e$ok), logical(1))), "\n")

# Proper captions for the writings list, where the site has them.
captions <- NULL
sj <- file.path(site_dir, "cases", "search.json")
if (file.exists(sj)) captions <- tryCatch({
  s <- fromJSON(sj, simplifyVector = TRUE)
  if (is.data.frame(s) && all(c("d", "c") %in% names(s))) setNames(s$c, s$d)
  else if (is.list(s) && !is.null(names(s))) unlist(s) else NULL
}, error = function(e) NULL)

rendered <- render_justices(site_dir, gn, lineups, captions, texts)
cat("Rendered Justices pages for Term(s):", paste(rendered, collapse = ", "), "+ index\n")
