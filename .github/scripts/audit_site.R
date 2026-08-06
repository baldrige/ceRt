#!/usr/bin/env Rscript
# audit_site.R -----------------------------------------------------------------
# Check the published site for the failure modes this pipeline actually has.
#
#   SITE_DIR=site Rscript .github/scripts/audit_site.R
#
# Written because a claim about coverage got made three times from arithmetic --
# "total files minus v15 minus v16 = 11 stale pages" -- and was wrong every time.
# The residual was the /cases/ browse pages, which carry no template stamp
# because they are not docket pages. One command listing the files would have
# settled it. That is what this is: the commands, run every time, so a coverage
# claim is something the site reports rather than something anyone infers.
#
# Deliberately dependency-light -- base R plus jsonlite. An audit that fails
# because tidyverse would not install tells you nothing about the site.
#
# FAIL = a broken invariant; exits 1. WARN = a coverage gap worth a number but
# expected transiently (a template bump before its re-render lands, dated leaves
# predating the prev/next slot). A permanently red audit is an ignored audit.

suppressPackageStartupMessages(library(jsonlite))

site <- Sys.getenv("SITE_DIR", unset = "site")
if (!dir.exists(site)) stop("SITE_DIR does not exist: ", site)

# Read the template version out of the source rather than duplicating it. Parsed,
# not sourced: docket_page.R pulls in tidyverse and the whole model stack, and
# this script must not be able to fail for a reason unrelated to the site.
tv_src <- grep("^PAGE_TEMPLATE_VERSION <- ",
               readLines("R/docket_page.R", warn = FALSE), value = TRUE)
CUR_TV <- sub('.*"(v[0-9]+)".*', "\\1", tv_src[1])

# SITE_SECTIONS is the source of truth for what the masthead links to.
local({ e <- new.env(); sys.source("R/site_nav.R", envir = e)
        assign("SECTIONS", e$SITE_SECTIONS, envir = globalenv()) })

results <- list()
record <- function(level, check, detail) {
  results[[length(results) + 1L]] <<- list(level = level, check = check, detail = detail)
  cat(sprintf("  [%-4s] %-34s %s\n", level, check, detail))
}
ok   <- function(check, detail) record("ok",   check, detail)
warn <- function(check, detail) record("WARN", check, detail)
fail <- function(check, detail) record("FAIL", check, detail)

# First `n` bytes of a file. Docket pages are a single long line, so readLines()
# would pull the whole ~30KB page; the <meta name='tv'> sits in the first ~250.
head_bytes <- function(path, n = 600L) {
  con <- file(path, "rb"); on.exit(close(con))
  rawToChar(readBin(con, "raw", n = n))
}

cat("Auditing ", normalizePath(site), " against ", CUR_TV, "\n\n", sep = "")

# ---- 1. docket pages: template coverage --------------------------------------
cat("Docket pages\n")
cases_dir <- file.path(site, "cases")
all_html <- list.files(cases_dir, pattern = "\\.html$")
# The browse pages are NOT docket pages and carry no template stamp by design.
browse <- c("index.html", grep("^ot[0-9]+\\.html$", all_html, value = TRUE))
docket_files <- setdiff(all_html, browse)

if (!length(docket_files)) {
  fail("docket pages present", "no docket pages found under cases/")
} else {
  heads <- vapply(file.path(cases_dir, docket_files), head_bytes, character(1),
                  USE.NAMES = FALSE)
  tv <- sub(".*name='tv' content='([^']*)'.*", "\\1", heads)
  tv[!grepl("name='tv'", heads)] <- "(unstamped)"
  tab <- sort(table(tv), decreasing = TRUE)
  stale <- sum(tv != CUR_TV)
  if (stale == 0) {
    ok("template version", sprintf("all %s docket pages at %s",
                                   format(length(docket_files), big.mark = ","), CUR_TV))
  } else {
    warn("template version",
         sprintf("%s of %s not at %s -- %s", format(stale, big.mark = ","),
                 format(length(docket_files), big.mark = ","), CUR_TV,
                 paste(sprintf("%s:%s", names(tab), as.integer(tab)), collapse = " ")))
  }
  ok("browse pages excluded",
     sprintf("%d (index + per-Term) correctly carry no tv stamp", length(browse)))

  # Pre-v8 pages predate the template entirely and render as a bare <li> list.
  pre <- sum(grepl("<li>Petition for a writ", heads, fixed = TRUE))
  if (pre) fail("pre-template pages", sprintf("%d bare-<li> page(s) remain", pre))
  else     ok("pre-template pages", "none")
}

# ---- 2. index integrity ------------------------------------------------------
cat("\nIndex integrity\n")
sj_path <- file.path(cases_dir, "search.json")
if (!file.exists(sj_path)) {
  fail("search.json", "missing -- home-page search and the /cases/ index are both dead")
} else {
  idx <- tryCatch(names(fromJSON(sj_path, simplifyVector = TRUE)),
                  error = function(e) NULL)
  if (is.null(idx)) {
    fail("search.json", "unreadable / not valid JSON")
  } else {
    pages <- sub("\\.html$", "", docket_files)
    orphan <- setdiff(pages, idx)   # page exists but nothing links to it
    ghost  <- setdiff(idx, pages)   # indexed but absent -> a published 404
    if (length(orphan))
      fail("pages reachable from index",
           sprintf("%d page(s) absent from search.json, e.g. %s",
                   length(orphan), paste(utils::head(sort(orphan), 5), collapse = ", ")))
    else ok("pages reachable from index", "every docket page is indexed")
    if (length(ghost))
      fail("index entries resolve",
           sprintf("%d entr(y/ies) have no page -- would publish a 404, e.g. %s",
                   length(ghost), paste(utils::head(sort(ghost), 5), collapse = ", ")))
    else ok("index entries resolve", sprintf("all %s entries have a page",
                                             format(length(idx), big.mark = ",")))
  }
}

# ---- 3. navigation targets ---------------------------------------------------
# The failure this catches is the one that nearly shipped: every case page
# linking to /cases/, which did not exist. Root-absolute hrefs resolve against
# the site root, so a missing target is a 404 on every page that carries it.
cat("\nNavigation targets\n")
resolve <- function(href) {
  p <- sub("^/", "", href)
  if (p == "" || grepl("/$", p)) p <- paste0(p, "index.html")
  file.exists(file.path(site, p))
}
targets <- c("/", vapply(SECTIONS, function(s) s$href, character(1)), "/cases/")
bad <- targets[!vapply(targets, resolve, logical(1))]
if (length(bad)) {
  fail("nav targets resolve", paste("404:", paste(bad, collapse = ", ")))
} else {
  ok("nav targets resolve", sprintf("all %d resolve", length(targets)))
}

# Do the pages actually emit only targets that exist? Case pages share one
# template, so a sample settles it; the non-case pages are all scanned.
scan_hrefs <- function(paths) unique(unlist(lapply(paths, function(p) {
  txt <- paste(readLines(p, warn = FALSE), collapse = " ")
  m <- regmatches(txt, gregexpr("href=['\"]/[^'\"]*['\"]", txt))[[1]]
  sub("^href=['\"]", "", sub("['\"]$", "", m))
})))
n_sample <- min(40L, length(docket_files))
sample_files <- file.path(cases_dir, docket_files[
  round(seq(1, length(docket_files), length.out = n_sample))])
others <- Filter(file.exists, file.path(site, c(
  "index.html", "about.html", "methods.html", "cases/index.html",
  "dashboards/index.html", "conferences/index.html", "arguments/index.html",
  "funnel/index.html")))
emitted <- setdiff(scan_hrefs(c(sample_files, others)), c("/favicon.svg", "/favicon.ico",
                                                          "/analytics.js"))
dangling <- emitted[!vapply(emitted, resolve, logical(1))]
if (length(dangling)) {
  fail("emitted links resolve",
       paste("404:", paste(utils::head(dangling, 6), collapse = ", ")))
} else {
  ok("emitted links resolve",
     sprintf("%d distinct root-absolute link(s); case pages sampled at %d of %s",
             length(emitted), n_sample, format(length(docket_files), big.mark = ",")))
}

# ---- 4. navigation presence --------------------------------------------------
cat("\nNavigation presence\n")
has <- function(path, needle) file.exists(path) &&
  grepl(needle, paste(readLines(path, warn = FALSE), collapse = " "), fixed = TRUE)
missing_mast <- Filter(function(p) !has(p, "smast-wm"), others)
if (length(missing_mast)) {
  fail("masthead on key pages",
       paste("absent:", paste(sub(paste0("^", site, "/"), "", missing_mast), collapse = ", ")))
} else {
  ok("masthead on key pages", sprintf("present on all %d", length(others)))
}

if (length(docket_files)) {
  s <- sample_files
  no_mast  <- sum(!vapply(s, has, logical(1), "class='smast-wm'"))
  no_crumb <- sum(!vapply(s, has, logical(1), "class='bcrumb'"))
  no_foot  <- sum(!vapply(s, has, logical(1), "class='cfoot'"))
  if (no_mast + no_crumb + no_foot > 0)
    fail("case-page nav (sampled)",
         sprintf("of %d sampled: %d without masthead, %d without breadcrumb, %d without footer",
                 n_sample, no_mast, no_crumb, no_foot))
  else ok("case-page nav (sampled)",
          sprintf("masthead, breadcrumb and footer on all %d sampled", n_sample))
}

# ---- 5. dated-leaf prev/next coverage ---------------------------------------
# WARN, not FAIL: leaves rendered before the slot existed have nothing to patch,
# and patch_prev_next() correctly declines to guess. The number is the point.
cat("\nDated leaves\n")
for (spec in list(list("conferences", "^conf_.*\\.html$", "conference"),
                  list("dashboards",  "^dash_.*\\.html$", "dashboard"),
                  list("arguments",   "^arg_.*\\.html$",  "argument"))) {
  d <- file.path(site, spec[[1]])
  f <- if (dir.exists(d)) list.files(d, pattern = spec[[2]]) else character()
  if (!length(f)) { warn(paste(spec[[3]], "leaves"), "none found"); next }
  withslot <- sum(vapply(file.path(d, f), has, logical(1), "<!--PNAV-->"))
  msg <- sprintf("%d of %d carry the prev/next slot", withslot, length(f))
  if (withslot == length(f)) ok(paste(spec[[3]], "leaves"), msg)
  else warn(paste(spec[[3]], "leaves"),
            paste(msg, "-- the rest predate it and need a re-render to gain it"))
}

# ---- one colour source -------------------------------------------------------
# R/palette.R is the only place a colour may be written down. This check exists
# because the alternative is what the tree looked like before it: five :root
# blocks, 33 literals inside gt calls, and 33 more hidden in var(--token,
# #fallback) where a stale one would have gone on painting the OLD palette,
# silently, and only on the pages that happened to omit that token.
#
# Source files, not the rendered site -- the point is to stop a literal being
# COMMITTED, and by the time it is on gh-pages the palette has already forked.
# #fff/#000 are exempt: three-digit structural values (a print background, the
# black end of a mask gradient) that are not palette colours.
#
# Hex is not the only spelling. The first version of this check looked for
# #rrggbb only and passed a clean tree that still held 19 copies of the palette
# written as decimal -- rgba(138,43,43,.05) is --accent, rgba(35,38,45,.45) is
# --ink, and neither is legible as such. Those are exactly the copies that go
# stale, so a colour is stray whether it is written in hex or in decimal.
src <- c(list.files("R", pattern = "\\.R$", full.names = TRUE),
         "docs/make_methods_note.R")
src <- setdiff(src[file.exists(src)], "R/palette.R")

# The palette in decimal, so an rgb()/rgba() copy is recognisable.
pal_dec <- vapply(
  c(PALETTE, PALETTE_FUNNEL, PALETTE_UI, PALETTE_EVENTS,
    as.list(c(GRANT_RAMP, TYPE_CHIPS, STATUS_FILL, CHART_SERIES))),
  function(h) paste(as.vector(grDevices::col2rgb(h)), collapse = ","),
  character(1))

stray <- unlist(lapply(src, function(f) {
  ln <- readLines(f, warn = FALSE)
  hit <- grep("#[0-9a-fA-F]{6}\\b", ln)
  dec <- which(vapply(ln, function(l)
    any(vapply(pal_dec, function(d) grepl(paste0("rgba?(", d), l, fixed = TRUE),
               logical(1))), logical(1), USE.NAMES = FALSE))
  hit <- sort(unique(c(hit, dec)))
  if (!length(hit)) return(NULL)
  sprintf("%s:%d", basename(f), hit)
}))
if (!length(stray)) {
  ok("one colour source", sprintf("no colour literals outside palette.R (%d files)",
                                  length(src)))
} else {
  fail("one colour source",
       sprintf("%d colour literal(s) outside R/palette.R: %s",
               length(stray), paste(utils::head(stray, 8), collapse = ", ")))
}

# ---- verdict -----------------------------------------------------------------
lv <- vapply(results, function(r) r$level, character(1))
cat(sprintf("\n%d checks: %d ok, %d WARN, %d FAIL\n",
            length(lv), sum(lv == "ok"), sum(lv == "WARN"), sum(lv == "FAIL")))
if (any(lv == "FAIL")) {
  cat("\nFAILED:\n")
  for (r in results[lv == "FAIL"]) cat("  -", r$check, "--", r$detail, "\n")
  quit(status = 1)
}
cat("No broken invariants.\n")
