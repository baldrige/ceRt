# Bring already-published LEAF pages' chrome up to date without re-rendering:
# feed-autodiscovery links, and the shared widget libraries.
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
# Two passes, in order:
#
#   1. Feed autodiscovery (patch_feed_links, site_nav.R).
#   2. Shared libraries (patch_shared_libs, leaf_assets.R). Every leaf used to
#      carry ~575 KB of base64-inlined React/reactable; new renders link the
#      copies published once under /lib/ instead. This pass swaps an inlined
#      script for the shared reference ONLY when its decoded bytes equal a file
#      already published there, so it cannot break a page -- an unmatched
#      script stays inline and keeps working. It therefore needs a daily or
#      conference run to have published /lib/ first; before that it reports
#      every page unchanged.
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
source("R/leaf_assets.R")    # leaf_lib_files(), patch_shared_libs()

site <- Sys.getenv("SITE_DIR", unset = "site")
dry <- tolower(Sys.getenv("DRY_RUN", "")) %in% c("1", "true", "yes")
Sys.setenv(SITE_DIR = site)  # site_feeds_present() reads this

targets <- c(
  list.files(file.path(site, "conferences"), pattern = "^conf_\\d{4}-\\d{2}-\\d{2}\\.html$",
             full.names = TRUE),
  list.files(file.path(site, "dashboards"), pattern = "^dash_\\d{4}-\\d{2}-\\d{2}\\.html$",
             full.names = TRUE),
  list.files(file.path(site, "arguments"), pattern = "^arg_\\d{4}\\.html$",
             full.names = TRUE),
  Filter(file.exists, file.path(site, "relists", "index.html")))
cat("Leaf pages found:", length(targets), "\n")
if (!length(targets)) quit(status = 0)

slurp <- function(p) paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# ---- 1. feed autodiscovery ---------------------------------------------------
feeds <- site_feeds_present(site)
cat("Feeds published:", if (length(feeds)) paste(feeds, collapse = ", ") else "(none)", "\n")
if (length(feeds)) {
  if (dry) {
    n_missing <- sum(vapply(targets, function(p) {
      m <- gregexpr('rel="alternate" type="application/atom\\+xml"', slurp(p))[[1]]
      length(m[m > 0]) != length(feeds)
    }, logical(1)))
    cat("DRY RUN -- feeds:", n_missing, "of", length(targets),
        "leaf page(s) would be patched.\n")
  } else {
    res <- vapply(targets, patch_feed_links, character(1))
    tab <- table(res)
    cat("Feeds:", paste(sprintf("%s=%d", names(tab), as.integer(tab)), collapse = " | "), "\n")
    if ("added" %in% names(tab)) {
      ex <- basename(names(res)[res == "added"])
      cat("  patched", sum(res == "added"), "page(s), e.g.",
          paste(utils::head(ex, 5), collapse = ", "), "\n")
    }
  }
} else {
  cat("No feeds to advertise; feed pass skipped.\n")
}

# ---- 2. shared widget libraries ---------------------------------------------
libs <- leaf_lib_files(site)
cat("Shared library files published under /lib/:", length(libs), "\n")
if (!length(libs)) {
  cat("Nothing to migrate onto yet -- a daily or conference render publishes /lib/ first.\n")
  quit(status = 0)
}
inlined <- Filter(function(p) grepl("data:application/javascript;base64,", slurp(p), fixed = TRUE),
                  targets)
cat("Leaf pages still carrying inlined libraries:", length(inlined), "\n")
if (!length(inlined)) {
  cat("Every leaf already links /lib/; nothing to migrate.\n")
} else if (dry) {
  # Patch a COPY, so the dry run reports what a real one would match without
  # writing into the checkout.
  saved <- 0; n_would <- 0L
  for (p in inlined) {
    tmp <- tempfile(fileext = ".html"); file.copy(p, tmp)
    r <- patch_shared_libs(tmp, site, libs)
    if (identical(as.character(r), "patched")) { n_would <- n_would + 1L; saved <- saved + attr(r, "saved") }
    unlink(tmp)
  }
  cat(sprintf("DRY RUN -- libraries: %d of %d inlined page(s) would be migrated, freeing %.1f MB.\n",
              n_would, length(inlined), saved / 1e6))
} else {
  res <- lapply(inlined, patch_shared_libs, site_root = site, libs = libs)
  st <- vapply(res, as.character, character(1))
  saved <- sum(vapply(res, function(r) {
    s <- attr(r, "saved"); if (is.null(s)) 0 else as.numeric(s) }, numeric(1)))
  tab <- table(st)
  cat("Libraries:", paste(sprintf("%s=%d", names(tab), as.integer(tab)), collapse = " | "),
      sprintf("| %.1f MB freed", saved / 1e6), "\n")
  if (sum(st == "unchanged")) {
    ex <- basename(inlined[st == "unchanged"])
    cat("  ", sum(st == "unchanged"), "page(s) embed a library version /lib/ does not hold and",
        "were left inline, e.g.", paste(utils::head(ex, 5), collapse = ", "), "\n")
  }
}
