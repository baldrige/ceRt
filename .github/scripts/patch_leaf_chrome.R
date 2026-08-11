# Bring already-published LEAF pages' feed-autodiscovery links up to date.
#
# The pages this exists for are the ones no render ever revisits.
# render_conferences.R renders only conferences on/after MIN_CONF_DATE, so
# conf_2017-09-25.html has not been rewritten since the day it was made and never
# will be by a normal run. Same for dashboards outside the daily's window. When
# interactive_theme.R gained autodiscovery those leaves simply stayed without it,
# and the audit reported a WARN that nothing would ever clear.
#
# Re-rendering them properly would mean reassembling the whole multi-term case
# database to change two lines of chrome. Patching is the established answer here
# -- inject_masthead() and patch_prev_next() are both post-passes for exactly
# this reason.
#
# DELIBERATELY NOT DOCKET PAGES. They carry a template-version stamp and have a
# real re-render path (rerender-dockets.yml, reuse_from_runs, ~20 min). Patching
# them would leave the page current and the stamp stale, which is the one thing
# the stamp exists to prevent. Roll v19 through that workflow instead.
#
# Env: SITE_DIR (gh-pages checkout, default "site"), DRY_RUN.

suppressPackageStartupMessages({ library(htmltools) })
source("R/palette.R")
source("R/site_nav.R")
source("R/page_style.R")     # feed_autodiscovery_links(), site_feeds_present()

site <- Sys.getenv("SITE_DIR", unset = "site")
dry <- tolower(Sys.getenv("DRY_RUN", "")) %in% c("1", "true", "yes")
Sys.setenv(SITE_DIR = site)  # site_feeds_present() reads this

feeds <- site_feeds_present(site)
cat("Feeds published:", if (length(feeds)) paste(feeds, collapse = ", ") else "(none)", "\n")
if (!length(feeds)) {
  cat("Nothing to advertise; no page touched.\n")
  quit(status = 0)
}

targets <- c(
  list.files(file.path(site, "conferences"), pattern = "^conf_\\d{4}-\\d{2}-\\d{2}\\.html$",
             full.names = TRUE),
  list.files(file.path(site, "dashboards"), pattern = "^dash_\\d{4}-\\d{2}-\\d{2}\\.html$",
             full.names = TRUE),
  list.files(file.path(site, "arguments"), pattern = "^arg_\\d{4}\\.html$",
             full.names = TRUE))
cat("Leaf pages found:", length(targets), "\n")
if (!length(targets)) quit(status = 0)

if (dry) {
  n_missing <- sum(vapply(targets, function(p) {
    txt <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    length(gregexpr('rel="alternate" type="application/atom\\+xml"', txt)[[1]][
      gregexpr('rel="alternate" type="application/atom\\+xml"', txt)[[1]] > 0]) != length(feeds)
  }, logical(1)))
  cat("DRY RUN --", n_missing, "of", length(targets),
      "leaf page(s) would be patched; nothing written.\n")
  quit(status = 0)
}

res <- vapply(targets, patch_feed_links, character(1))
tab <- table(res)
cat("Result:", paste(sprintf("%s=%d", names(tab), as.integer(tab)), collapse = " | "), "\n")
if ("added" %in% names(tab)) {
  ex <- basename(names(res)[res == "added"])
  cat("Patched", sum(res == "added"), "page(s), e.g.",
      paste(utils::head(ex, 5), collapse = ", "), "\n")
} else {
  cat("Every leaf already carried the current links; nothing written.\n")
}
