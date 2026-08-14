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

# Colour tokens. Needed twice over, and this file went without them from #36
# until the next morning's run: site_nav.R now builds NAV_CSS with
# fill_palette(), so merely SOURCING it below dies with "could not find function
# fill_palette", and the one-colour-source check further down reads PALETTE and
# friends directly. The audit fell over at line 34 and ran zero checks.
#
# It caught itself, which is the system working -- but note what that means: an
# audit that cannot start is indistinguishable from a red build, so a failure
# here says nothing about the site. Read the log, not the badge.
source("R/palette.R")

# SITE_SECTIONS is the source of truth for what the masthead links to. The env is
# isolated so a stray global in site_nav.R cannot shadow anything here, but its
# parent chain still reaches globalenv, which is where palette.R just landed.
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

# Whole file as one string. Lives HERE, with the other helpers, and not beside
# its first heavy user.
#
# It was defined in the feeds section, 67 lines below the placeholder check that
# also calls it, and the 2026-08-11 audit died at "could not find function
# slurp" after twelve green checks. This file already carries the lesson at the
# top -- an audit that cannot finish is indistinguishable from a red build -- and
# had already been bitten once the same way, when fill_palette() was missing.
# Helpers go at the top; sections below may use them, never define them.
slurp <- function(p) paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Values of a repeated simple tag, e.g. every <loc> in a sitemap.
tagvals <- function(txt, tag) {
  m <- regmatches(txt, gregexpr(paste0("<", tag, ">[^<]*</", tag, ">"), txt))[[1]]
  sub(paste0("^<", tag, ">"), "", sub(paste0("</", tag, ">$"), "", m))
}

# An absolute site URL back to a path resolve() understands.
SITE_BASE <- "https://supremecourt.report"
unbase <- function(u) sub(paste0("^", SITE_BASE), "", u)

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

# Unresolved @token@ placeholders in PUBLISHED HTML.
#
# The "one colour source" check below enforces that no colour is written down
# outside palette.R. It is what caused this: it flagged a literal
# rgba(138,43,43,...) in cert_funnel.R's relist table, the fix replaced it with
# @accent:rgb@, and nothing verified that the placeholder is ever substituted in
# that file -- cert_funnel.R never called fill_palette() at all.
#
# A placeholder that reaches the browser is not a wrong colour, it is NO colour:
# the declaration is invalid and drops. On the funnel's relist table the
# background dropped while the color:var(--paper) set for contrast against it
# survived, so three of six Granted figures were near-white text on the
# near-white page -- in the DOM, invisible on screen, and reported by a reader
# rather than by any check.
#
# fill_palette() already stop()s on leftovers, but only for strings handed TO it.
# This is the same invariant enforced where it actually matters: the output.
cat("\nBuild-time substitution\n")
ph_files <- Filter(file.exists, c(
  file.path(site, c("index.html", "about.html", "funnel/index.html",
                    "conferences/index.html", "arguments/index.html",
                    "dashboards/index.html", "cases/index.html",
                    "relists/index.html", "counsel/index.html")),
  head(list.files(file.path(site, "cases"), pattern = "^[0-9]", full.names = TRUE), 5),
  head(sort(list.files(file.path(site, "conferences"),
                       pattern = "^conf_", full.names = TRUE), decreasing = TRUE), 2),
  head(sort(list.files(file.path(site, "dashboards"),
                       pattern = "^dash_", full.names = TRUE), decreasing = TRUE), 2)))
ph_hits <- unlist(lapply(ph_files, function(f) {
  m <- regmatches(slurp(f), gregexpr("@[a-z0-9]+(:rgb)?@", slurp(f)))[[1]]
  if (length(m)) sprintf("%s (%s)", sub(paste0("^", site, "/"), "", f),
                         paste(unique(m), collapse = " ")) else NULL
}))
if (length(ph_hits)) {
  fail("placeholders substituted",
       sprintf("%d page(s) ship an unresolved @token@: %s", length(ph_hits),
               paste(utils::head(ph_hits, 4), collapse = "; ")))
} else {
  ok("placeholders substituted",
     sprintf("no unresolved @token@ in %d sampled page(s)", length(ph_files)))
}

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

# ---- 9. feeds and sitemaps ---------------------------------------------------
# Base R, like the rest of this file -- no xml2. These are not a schema check;
# they are the three ways THIS pipeline can break these files.
cat("\nFeeds and sitemaps\n")

# slurp / tagvals / unbase / SITE_BASE are defined with the other helpers at the
# top of this file. They were here, which is what broke the 2026-08-11 run.

# feed.xml must exist -- it is built from directory listings that are never empty.
# grants.xml legitimately may not: it is built from cases/grants.json, which only
# a full-term run (conferences.yml) can populate, so on a fresh site it is absent
# until the first weekly run. That is a transient coverage gap, which is a WARN
# here, not a broken invariant.
feeds <- c("feed.xml", "grants.xml")[file.exists(file.path(site, c("feed.xml", "grants.xml")))]
if (!file.exists(file.path(site, "feed.xml"))) {
  fail("feeds present", "feed.xml absent")
} else if (!file.exists(file.path(site, "grants.xml"))) {
  ng <- if (file.exists(file.path(site, "cases/grants.json")))
    length(fromJSON(file.path(site, "cases/grants.json"))) else 0L
  warn("feeds present",
       sprintf("feed.xml only; grants.xml awaits a grant (cases/grants.json holds %d)",
               ng))
} else {
  ok("feeds present", paste(feeds, collapse = ", "))
}

if (length(feeds)) {

  # THE anti-churn invariant. The feed-level <updated> must equal the newest
  # entry's, never the build time -- a build-time stamp would re-notify every
  # subscriber three times a day and add a gh-pages commit per run whose only
  # content is a timestamp. Expressed as a property of the published file so it
  # survives someone "fixing" write_atom_feed() later.
  bad_stamp <- character()
  for (f in feeds) {
    txt <- slurp(file.path(site, f))
    up <- tagvals(txt, "updated")
    if (length(up) < 2) { bad_stamp <- c(bad_stamp, paste0(f, " (no entries)")); next }
    if (up[1] != max(up[-1]))
      bad_stamp <- c(bad_stamp, sprintf("%s (feed %s, newest entry %s)", f, up[1],
                                        max(up[-1])))
  }
  if (length(bad_stamp)) {
    fail("feed <updated> is an event date", paste(bad_stamp, collapse = "; "))
  } else {
    ok("feed <updated> is an event date", "matches the newest entry in each feed")
  }

  # No entry may be dated in the future. Conference reports are published before
  # the conference they cover, so an unfiltered feed put its newest entry -- and
  # its own <updated> -- seven weeks ahead of today, which pins that entry to the
  # top of every reader that sorts by date.
  ahead <- unlist(lapply(feeds, function(f) {
    up <- as.Date(substr(tagvals(slurp(file.path(site, f)), "updated"), 1, 10))
    if (any(up > Sys.Date())) sprintf("%s (%d entr(y/ies), newest %s)", f,
                                      sum(up > Sys.Date()), max(up)) else NULL
  }))
  if (length(ahead)) {
    fail("no future-dated entries", paste(ahead, collapse = "; "))
  } else {
    ok("no future-dated entries", "every entry dated today or earlier")
  }

  # Every entry links somewhere that exists.
  ent <- unlist(lapply(feeds, function(f) {
    txt <- slurp(file.path(site, f))
    m <- regmatches(txt, gregexpr("<link href=\"[^\"]+\"", txt))[[1]]
    unbase(sub("\"$", "", sub("^<link href=\"", "", m)))
  }))
  dead <- unique(ent[!vapply(ent, resolve, logical(1))])
  if (length(dead)) {
    fail("feed entry links resolve", paste("404:", paste(utils::head(dead, 5),
                                                         collapse = ", ")))
  } else {
    ok("feed entry links resolve", sprintf("%d entry link(s)", length(ent)))
  }
}

if (!file.exists(file.path(site, "sitemap.xml"))) {
  fail("sitemap index present", "sitemap.xml absent")
} else {
  kids <- unbase(tagvals(slurp(file.path(site, "sitemap.xml")), "loc"))
  gone <- kids[!vapply(kids, function(k) file.exists(file.path(site, sub("^/", "", k))),
                       logical(1))]
  if (length(gone)) {
    fail("sitemap children exist", paste("absent:", paste(gone, collapse = ", ")))
  } else {
    # 50,000 URLs per file is the spec limit, and a file over it is rejected
    # whole rather than truncated. cases/ passed 55k in 2026, which is why the
    # index-plus-children shape exists at all.
    counts <- vapply(kids, function(k)
      length(tagvals(slurp(file.path(site, sub("^/", "", k))), "loc")), integer(1))
    over <- names(counts)[counts > 50000L]
    if (length(over)) {
      fail("sitemap under the 50k cap",
           sprintf("%s holds %s URLs", over[1], format(max(counts), big.mark = ",")))
    } else {
      ok("sitemap coverage",
         sprintf("%d child sitemap(s), %s URLs, largest %s",
                 length(kids), format(sum(counts), big.mark = ","),
                 format(max(counts), big.mark = ",")))
    }
  }
}

if (!file.exists(file.path(site, "robots.txt"))) {
  fail("robots.txt", "absent -- nothing points a crawler at the sitemap")
} else if (!grepl("Sitemap:", slurp(file.path(site, "robots.txt")), fixed = TRUE)) {
  fail("robots.txt", "present but carries no Sitemap: line")
} else {
  ok("robots.txt", "present, points at sitemap.xml")
}

# Every generated index page must advertise every feed that exists.
#
# The conditional-link design was itself the fix for advertising a feed that did
# not exist. Then site_feeds_present() lived in feeds.R, which three of the four
# renderers do not source before writing their pages -- so /conferences/,
# /arguments/ and /funnel/ advertised NOTHING, and nothing noticed for a day. A
# missing link is quieter than a dangling one, which is exactly why it needs a
# check rather than a convention.
#
# methods.html is excluded on purpose: it is a hand-authored document copied from
# docs/ with only the masthead injected, so it has no generated <head>.
n_feeds <- length(feeds)
if (n_feeds) {
  # Index pages AND a sample of leaves. Checking only the indexes is how the
  # third hand-built <head> (interactive_theme.R, which writes every conference,
  # dashboard, argument and relist LEAF) went unnoticed while the seven indexes
  # read green. Leaves matter more here, not less: a search result lands a reader
  # on a conference report, not on /conferences/.
  leaf <- function(dir, pat) {
    f <- sort(list.files(file.path(site, dir), pattern = pat, full.names = TRUE))
    if (length(f)) f[unique(round(seq(1, length(f), length.out = min(3L, length(f)))))]
    else character()
  }
  idx <- Filter(file.exists, file.path(site, c(
    "index.html", "about.html", "cases/index.html", "dashboards/index.html",
    "conferences/index.html", "arguments/index.html", "funnel/index.html",
    "relists/index.html", "counsel/index.html")))
  lvs <- Filter(file.exists, c(
    leaf("conferences", "^conf_\\d{4}-\\d{2}-\\d{2}\\.html$"),
    leaf("dashboards", "^dash_\\d{4}-\\d{2}-\\d{2}\\.html$"),
    leaf("arguments", "^arg_\\d{4}\\.html$")))

  # Count only the HEAD links. The first version counted every
  # "application/atom+xml" on the page, which was right until the visible
  # "Follow by feed" line landed: those <a> tags carry the same type attribute,
  # so the landing page and About went from 2 matches to 4 and the check called
  # them broken while they were correct. rel="alternate" appears on the <link>
  # tags and nowhere else.
  nlinks <- function(p) {
    m <- gregexpr('rel="alternate" type="application/atom\\+xml"', slurp(p))[[1]]
    length(m[m > 0])
  }
  short_idx <- Filter(function(p) nlinks(p) != n_feeds, idx)
  short_lvs <- Filter(function(p) nlinks(p) != n_feeds, lvs)
  rel <- function(v) paste(sub(paste0("^", site, "/"), "", utils::head(v, 4)),
                           collapse = ", ")

  if (length(short_idx)) {
    fail("feed autodiscovery (indexes)",
         sprintf("%d of %d index page(s) do not advertise %d feed(s): %s",
                 length(short_idx), length(idx), n_feeds, rel(short_idx)))
  } else {
    ok("feed autodiscovery (indexes)",
       sprintf("all %d index page(s) advertise all %d feed(s)", length(idx), n_feeds))
  }

  # Leaves are a WARN, not a FAIL, and for the same reason the template-version
  # check is: a leaf rendered before autodiscovery existed has nothing to patch,
  # and normal runs never rewrite it -- conferences renders only on/after
  # min_conf, so conf_2017-09-25.html will sit at zero until a back-catalogue
  # re-render. That is a coverage number worth reporting, not a broken
  # invariant, and a permanently red audit is an ignored audit.
  if (length(short_lvs)) {
    warn("feed autodiscovery (leaves)",
         sprintf("%d of %d sampled leaf/leaves predate it and need a re-render: %s",
                 length(short_lvs), length(lvs), rel(short_lvs)))
  } else if (length(lvs)) {
    ok("feed autodiscovery (leaves)",
       sprintf("all %d sampled leaf/leaves advertise all %d feed(s)", length(lvs), n_feeds))
  }
}

# ---- the counsel table --------------------------------------------------------
# Two things, and neither is "does the page exist".
#
# FRESHNESS is the one that matters. data/counsel_stats.json is a committed
# summary of a classifier's output, which is exactly the shape of file that went
# stale on this site before: funnel_baselines.json was generated 2026-07-10,
# invalidated two weeks later by a change inside classify_petition_events(), and
# the funnel published a relist count 2.5x too high until a reader noticed.
# render_counsel.R recomputes on a mismatch so the PAGE is never wrong, but a
# silent 4-minute recompute on every weekly run is a cost nobody sees. This is
# what makes it visible.
#
# The second is that the published page carries its boards at all. A rendering
# path that quietly produced an empty table would still emit a valid, styled,
# feed-advertising page -- every other check here would pass it.
cat("\nCounsel table\n")
cstats <- "data/counsel_stats.json"
cpage <- file.path(site, "counsel/index.html")
if (!file.exists(cstats) && !file.exists(cpage)) {
  cat("  (no counsel table published yet -- skipped)\n")
} else {
  arch <- sort(Sys.glob("data-raw/ot_*.rds"))
  # Loaded HERE and defensively, not at the top with palette.R. Computing the
  # fingerprint means deparsing the classifier, so the whole module chain
  # (cert_funnel, cert_model, page_style) has to load -- a much larger surface
  # than anything else this script touches. An audit that cannot start reports
  # nothing about the site, so a missing package degrades this one check to a
  # WARN instead of taking the other thirty with it.
  cenv <- tryCatch({ e <- new.env(parent = globalenv())
                     sys.source("R/counsel_table.R", envir = e); e },
                   error = function(err) NULL)
  if (file.exists(cstats) && length(arch) && !is.null(cenv)) {
    have <- tryCatch(jsonlite::fromJSON(cstats)$fingerprint, error = function(e) NA)
    want <- tryCatch(cenv$counsel_stats_fingerprint(arch),
                     error = function(e) NA_character_)
    if (is.na(want)) {
      warn("counsel stats fresh", "could not compute the fingerprint")
    } else if (identical(as.character(have), want)) {
      ok("counsel stats fresh",
         sprintf("committed stats match the classifier (%s)", substr(want, 1, 12)))
    } else {
      fail("counsel stats fresh",
           sprintf(paste("data/counsel_stats.json is STALE (committed %s, code",
                         "produces %s) -- every render recomputes it. Refresh:",
                         "Rscript .github/scripts/make_counsel_stats.R"),
                   if (length(have) && !is.na(have)) substr(have, 1, 12) else "none",
                   substr(want, 1, 12)))
    }
  } else if (file.exists(cstats) && is.null(cenv)) {
    warn("counsel stats fresh",
         "R/counsel_table.R would not load here -- freshness unverified")
  }
  if (file.exists(cpage)) {
    ch <- slurp(cpage)
    nrows <- length(gregexpr("<tr><td class='rk'>", ch, fixed = TRUE)[[1]])
    ntab <- length(gregexpr("<table class='ctab'>", ch, fixed = TRUE)[[1]])
    # A stray NA/NaN in a cell is what a division by an empty pool looks like
    # after it has been through format(): the page renders, the number is
    # nonsense. Bounded to cell contents so the word cannot match in prose.
    bad <- length(gregexpr(">(NA|NaN|Inf|-Inf)%?<", ch, perl = TRUE)[[1]][
      gregexpr(">(NA|NaN|Inf|-Inf)%?<", ch, perl = TRUE)[[1]] > 0])
    # Seven boards: four over petitions, three over oral argument. The argument
    # half reads a DIFFERENT file (data-raw/arg_refresh.rds), so it can go missing
    # on its own while the petition boards stay perfect -- and the page would
    # still render, styled and valid, simply four tables shorter.
    if (ntab < 7 || nrows < 20) {
      fail("counsel boards populated",
           sprintf("%d table(s), %d ranked row(s) -- expected 7 tables", ntab, nrows))
    } else if (bad) {
      fail("counsel boards populated", sprintf("%d cell(s) render NA/NaN/Inf", bad))
    } else {
      ok("counsel boards populated",
         sprintf("%d tables, %d ranked rows, no NA cells", ntab, nrows))
    }
  }
}

# ---- 10. the prospective forecast log ----------------------------------------
# The log is only worth anything if every entry was written BEFORE the conference
# it forecasts. That is checkable from the file itself -- scored_on must precede
# conf_date -- so it does not depend on trusting the code that wrote it, and it
# would catch a backfill, a re-score of a past conference, or a clock problem.
#
# Absent is fine and expected until the first conferences run after it ships;
# empty is fine during a recess with no conference scheduled.
flog <- file.path(site, "cases/forecasts.json")
if (file.exists(flog)) {
  cat("\nForecast log\n")
  fj <- tryCatch(fromJSON(flog, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(fj)) {
    fail("forecast log parses", "cases/forecasts.json is unreadable")
  } else if (!length(fj)) {
    ok("forecast log", "present, empty (no future conference scored yet)")
  } else {
    sd <- as.Date(vapply(fj, function(e) e$scored_on %||% NA_character_, character(1)))
    cd <- as.Date(vapply(fj, function(e) e$conf_date %||% NA_character_, character(1)))
    late <- which(!is.na(sd) & !is.na(cd) & sd >= cd)
    if (length(late)) {
      fail("forecasts logged in advance",
           sprintf("%d of %d entr(y/ies) scored on or after the conference: %s",
                   length(late), length(fj),
                   paste(utils::head(names(fj)[late], 4), collapse = ", ")))
    } else {
      ok("forecasts logged in advance",
         sprintf("all %d entries scored before their conference (median %d day(s) ahead)",
                 length(fj), as.integer(stats::median(as.numeric(cd - sd), na.rm = TRUE))))
    }
    # Named fp, not p: this is a long script of top-level assignments and a bare
    # `p` is exactly the kind of name that collides with something three hundred
    # lines away.
    fp <- vapply(fj, function(e) e$p_grant_now %||% NA_real_, numeric(1))
    bad_p <- sum(is.na(fp) | fp < 0 | fp > 1)
    nm <- sum(vapply(fj, function(e) !nzchar(e$model_id %||% ""), logical(1)))
    if (bad_p || nm) {
      fail("forecast entries well-formed",
           sprintf("%d bad probabilit(y/ies), %d missing model_id", bad_p, nm))
    } else {
      ok("forecast entries well-formed",
         sprintf("%d entries, %d distinct model_id(s)", length(fj),
                 length(unique(vapply(fj, function(e) e$model_id, character(1))))))
    }
  }
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
