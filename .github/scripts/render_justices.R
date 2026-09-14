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

source("R/palette.R")
source("R/site_nav.R")
source("R/site_meta.R")
source("R/page_style.R")
source("R/granted_noted.R")   # read_granted_noted(), gn_others()
source("R/site_decisions.R")  # fetch_opinion_listing()
source("R/justices.R")

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
need <- terms[vapply(terms, function(t) any(!vapply(gn_decisions(gn, t)$dkt, is_current, logical(1))), logical(1))]
if (max_new > 0 && length(need)) {
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

# Proper captions for the writings list, where the site has them.
captions <- NULL
sj <- file.path(site_dir, "cases", "search.json")
if (file.exists(sj)) captions <- tryCatch({
  s <- fromJSON(sj, simplifyVector = TRUE)
  if (is.data.frame(s) && all(c("d", "c") %in% names(s))) setNames(s$c, s$d)
  else if (is.list(s) && !is.null(names(s))) unlist(s) else NULL
}, error = function(e) NULL)

rendered <- render_justices(site_dir, gn, lineups, captions)
cat("Rendered Justices pages for Term(s):", paste(rendered, collapse = ", "), "+ index\n")
