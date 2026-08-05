# site_nav.R -------------------------------------------------------------------
# Sitewide navigation: masthead, breadcrumb, case footer, prev/next.
#
# Before this existed the site had none. Dated leaf pages linked *down* into case
# pages; case pages and methods.html linked nowhere internally at all. 55,167 of
# ~55,470 published pages -- 99.5% of the site, and its entire search surface --
# were terminal nodes, so a reader arriving from Google could reach nothing else.
#
# Design notes that are easy to undo by accident:
#
#   * ROOT-ABSOLUTE hrefs ("/conferences/"), not relative. Pages live at three
#     different depths (/, /cases/x.html, /dashboards/x.html) and a shared
#     component cannot carry a per-caller "../" prefix without every caller
#     getting it right. The site is served from a domain root (CNAME =
#     supremecourt.report) and already does this for /analytics.js and
#     /favicon.svg. The cost is that file:// previews lose their nav links.
#
#   * Every var() carries a LITERAL FALLBACK. Three stylesheets host this CSS
#     (INDEX_CSS, DOCKET_CSS, SCR_CSS) and two of them spelled the accent colour
#     --ox where the third spelled it --oxblood. The fallbacks mean NAV_CSS
#     renders correctly under either spelling, so a half-finished token
#     migration cannot silently ship an unstyled masthead.
#
#   * NO JAVASCRIPT. The mobile section row is a scroll strip under a mask-fade,
#     not a drawer. A drawer needs an open/close script, aria-expanded, a focus
#     trap, Escape handling and scroll locking, shipped to 55k static pages, to
#     hide five short words -- and would be the site's first render-blocking
#     interactive element. See docs/navigation.md.

# The five sections, in masthead order, defined once. `label` is the one-word
# masthead form (the row must hold one line at 390px); `long` is the full name
# used in the case footer, matching the landing page's own wording.
#
# /cases/ is deliberately absent: it is reached from the breadcrumb and from the
# landing page, and a sixth item would push the mobile strip past two visible
# sections for a surface nobody navigates to by name.
SITE_SECTIONS <- list(
  list(href = "/dashboards/",  label = "Docket",      long = "Daily petitions &amp; applications"),
  list(href = "/conferences/", label = "Conferences", long = "Conference reports"),
  list(href = "/arguments/",   label = "Arguments",   long = "Oral arguments"),
  list(href = "/funnel/",      label = "The Funnel",  long = "The cert funnel"),
  list(href = "/methods.html", label = "Model",       long = "The forecast model"),
  # About carries the contact address. It is in the masthead and the case footer
  # rather than only on the landing page because the error reports worth having
  # come from readers who arrived at a CASE page from a search result -- 99.5% of
  # the site -- and would otherwise have nowhere to send them.
  list(href = "/about.html",   label = "About",       long = "About &amp; contact")
)

# The middle crumb for a case page. Named rather than inlined because it points
# at /cases/index.html, which did not exist before this change -- 55,167 files in
# a directory that returned a 404. If that index is ever dropped, this is the one
# place that has to change, and a grep for CASES_CRUMB finds every page affected.
CASES_CRUMB <- list(href = "/cases/", label = "Cases")

# Shared by all three stylesheets. --nav-max lets one component serve both the
# 40rem index container and the 54rem case container; each host :root sets it.
NAV_CSS <- "
/* ---- masthead ---- */
.smast-in{max-width:var(--nav-max,54rem);margin:0 auto;padding:.85rem 1.5rem .5rem;
  display:flex;align-items:baseline;justify-content:space-between;gap:1.4rem;flex-wrap:wrap}
.smast-wm{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.02rem;
  letter-spacing:.005em;color:var(--ink,@ink@);text-decoration:none;white-space:nowrap;flex:none}
.smast-wm em{font-style:italic;font-weight:500;color:var(--oxblood,var(--ox,@oxblood@))}
.smast-wm:hover{color:var(--oxblood,var(--ox,@oxblood@))}
/* Row gap first, then column. At 1.35rem the six section links measured 596px
   of max-content -- 4px wider than the 592px row the 40rem index container
   leaves them -- so ABOUT alone wrapped to a third line under the wordmark, on
   every page page_style.R writes. 1.05rem brings the row to 572px: 20px of
   slack at 40rem, and 45px on the 54rem case pages, which were themselves only
   21px from wrapping. The .4rem row gap is for the day a seventh section
   overruns it anyway -- a nav that wraps should read as a stack, not an orphan. */
.snav{display:flex;gap:.4rem 1.05rem;list-style:none;margin:0;padding:0;flex-wrap:wrap}
.snav a{font:600 .72rem/1 'Newsreader',Georgia,serif;letter-spacing:.15em;text-transform:uppercase;
  color:var(--ink-soft,var(--soft,@ink-soft@));text-decoration:none;display:inline-block;
  padding:.5rem 0;border-bottom:1.5px solid transparent}
.snav a:hover{color:var(--oxblood,var(--ox,@oxblood@));border-bottom-color:var(--rule,@rule@)}
.snav a[aria-current='page']{color:var(--oxblood,var(--ox,@oxblood@));
  border-bottom-color:var(--oxblood,var(--ox,@oxblood@))}
.smast-rule-w{max-width:var(--nav-max,54rem);margin:0 auto;padding:0 1.5rem}
.smast-rule{border:0;height:0;border-top:2px solid var(--ink,@ink@);margin:0;position:relative}
.smast-rule::after{content:'';position:absolute;left:0;top:4px;width:100%;
  border-top:1px solid var(--rule,@rule@)}
.skip{position:absolute;left:-9999px;top:0;background:var(--panel,@panel@);
  color:var(--oxblood,var(--ox,@oxblood@));padding:.6rem 1rem;
  border:1px solid var(--oxblood,var(--ox,@oxblood@));
  font:600 .8rem/1 'Newsreader',Georgia,serif;letter-spacing:.1em;text-transform:uppercase;z-index:10}
.skip:focus{left:.5rem;top:.5rem}
/* ---- breadcrumb ---- */
.bcrumb{margin:.7rem 0 0}
.bcrumb ol{list-style:none;display:flex;flex-wrap:wrap;align-items:baseline;
  gap:.1rem .45rem;margin:0;padding:0}
.bcrumb li{display:inline-flex;align-items:baseline;gap:.45rem}
.bcrumb a{font:400 .8rem/1.5 'Newsreader',Georgia,serif;color:var(--ink-soft,var(--soft,@ink-soft@));
  text-decoration:none;border-bottom:1px solid var(--rule,@rule@)}
.bcrumb a:hover{color:var(--oxblood,var(--ox,@oxblood@));border-bottom-color:var(--oxblood,var(--ox,@oxblood@))}
.bcrumb .sep{color:var(--faint,@faint@);font-size:.8rem;line-height:1.5}
.bcrumb [aria-current='page']{font:400 .8rem/1.5 'Newsreader',Georgia,serif;
  color:var(--faint,@faint@);font-variant-numeric:tabular-nums}
/* ---- case footer ---- */
.cfoot{margin:2.4rem 0 0}
.cfoot-rule{border:0;height:0;border-top:2px solid var(--ink,@ink@);margin:0 0 1rem;position:relative}
.cfoot-rule::after{content:'';position:absolute;left:0;top:4px;width:100%;
  border-top:1px solid var(--rule,@rule@)}
.cfoot-off{font-size:.95rem;margin:0 0 1.1rem}
.cfoot-off a{color:var(--sienna,@sienna@);text-decoration:none;border-bottom:1px solid rgba(160,89,26,.4)}
.cfoot-off a:hover{border-bottom-color:var(--sienna,@sienna@)}
.cfoot-lab{font:600 .68rem/1 'Newsreader',Georgia,serif;letter-spacing:.18em;text-transform:uppercase;
  color:var(--faint,@faint@);margin:0 0 .5rem}
.cfoot-nav{display:flex;flex-wrap:wrap;gap:.35rem 1.1rem;list-style:none;margin:0;padding:0}
.cfoot-nav a{font-size:.92rem;color:var(--ink-soft,var(--soft,@ink-soft@));text-decoration:none;
  border-bottom:1px solid var(--rule,@rule@);padding-bottom:1px}
.cfoot-nav a:hover{color:var(--oxblood,var(--ox,@oxblood@));border-bottom-color:var(--oxblood,var(--ox,@oxblood@))}
.cfoot-stamp{margin:1.2rem 0 0;font-size:.8rem;color:var(--faint,@faint@);font-style:italic}
/* ---- prev / next ---- */
.pnav{margin:2.2rem 0 0;border-top:1px solid var(--rule,@rule@);padding-top:1rem;
  display:grid;grid-template-columns:1fr 1fr;gap:1.2rem}
.pnav a,.pnav span.none{text-decoration:none;color:inherit;display:block;padding:.5rem .2rem}
.pnav a:hover{background:rgba(138,43,43,.05)}
.pnav .dir{font:600 .66rem/1 'Newsreader',Georgia,serif;letter-spacing:.18em;text-transform:uppercase;
  color:var(--faint,@faint@);display:block;margin-bottom:.3rem}
.pnav .lab{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1rem;line-height:1.25;
  color:var(--oxblood,var(--ox,@oxblood@));display:block}
.pnav a:hover .lab{text-decoration:underline;text-underline-offset:3px}
.pnav .nx{text-align:right}
.pnav .none{opacity:.35}
@media(max-width:640px){
  .smast-in{padding:.7rem 1.1rem .25rem;gap:.15rem}
  .smast-wm{font-size:.98rem}
  .smast-in nav{width:100%;overflow-x:auto;-webkit-overflow-scrolling:touch;scrollbar-width:none;
    -webkit-mask-image:linear-gradient(90deg,#000 86%,transparent);
    mask-image:linear-gradient(90deg,#000 86%,transparent)}
  .smast-in nav::-webkit-scrollbar{display:none}
  .snav{flex-wrap:nowrap;gap:1.15rem;padding-right:2.4rem}
  .snav a{padding:.7rem 0;white-space:nowrap}
  .smast-rule-w{padding:0 1.1rem}
  .cfoot-nav{gap:.5rem 1rem}
  .cfoot-nav a{padding-bottom:.3rem}
  .pnav{grid-template-columns:1fr;gap:.2rem}
  .pnav .nx{text-align:left;border-top:1px solid var(--rule,@rule@)}
}
" |> fill_palette()

# Deliberately does not use %||%: this file is sourced standalone by generators
# that have not loaded tidyverse, and %||% only reached base R in 4.4.
.nav_esc <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  x <- as.character(x[1]); if (is.na(x)) return("")
  htmltools::htmlEscape(x)
}

# Masthead. `active` is a SITE_SECTIONS href ("/conferences/") or NULL.
#
# The landing page passes wordmark_only = TRUE: it *is* the section list, and
# repeating it 200px above itself is noise.
site_masthead <- function(active = NULL, wordmark_only = FALSE) {
  links <- ""
  if (!isTRUE(wordmark_only)) {
    items <- vapply(SITE_SECTIONS, function(s) paste0(
      "<li><a href='", s$href, "'",
      if (!is.null(active) && identical(active, s$href)) " aria-current='page'" else "",
      ">", s$label, "</a></li>"), character(1))
    links <- paste0("<nav aria-label='Sections'><ul class='snav'>",
                    paste(items, collapse = ""), "</ul></nav>")
  }
  paste0(
    "<a class='skip' href='#main'>Skip to content</a>",
    "<header><div class='smast-in'>",
    "<a class='smast-wm' href='/'>Supreme Court <em>Report</em></a>",
    links,
    "</div><div class='smast-rule-w'><hr class='smast-rule'></div></header>")
}

# Breadcrumb: Home > Section > This page. Three levels, never four -- the site
# has no deeper hierarchy and a trail that can grow is one that eventually wraps.
# `section` is a list(href=, label=) or NULL to go straight Home > page.
site_breadcrumb <- function(label, section = NULL) {
  mid <- if (!is.null(section)) paste0(
    "<li><a href='", section$href, "'>", .nav_esc(section$label),
    "</a><span class='sep' aria-hidden='true'>&rsaquo;</span></li>") else ""
  paste0(
    "<nav class='bcrumb' aria-label='Breadcrumb'><ol>",
    "<li><a href='/'>Home</a><span class='sep' aria-hidden='true'>&rsaquo;</span></li>",
    mid,
    "<li><span aria-current='page'>", .nav_esc(label), "</span></li>",
    "</ol></nav>")
}

# schema.org BreadcrumbList for the same trail. On a site whose search surface is
# 55k case pages this replaces the bare URL in a Google result with a labelled
# path, and it reuses the strings site_breadcrumb() already has.
site_breadcrumb_jsonld <- function(label, section = NULL, base = "https://supremecourt.report") {
  esc <- function(s) gsub('"', '\\\\"', s, fixed = TRUE)
  el <- list(paste0('{"@type":"ListItem","position":1,"name":"Home","item":"', base, '/"}'))
  if (!is.null(section))
    el <- c(el, paste0('{"@type":"ListItem","position":2,"name":"', esc(section$label),
                       '","item":"', base, section$href, '"}'))
  el <- c(el, paste0('{"@type":"ListItem","position":', length(el) + 1L,
                     ',"name":"', esc(label), '"}'))
  paste0("<script type=\"application/ld+json\">",
         '{"@context":"https://schema.org","@type":"BreadcrumbList","itemListElement":[',
         paste(el, collapse = ","), "]}</script>")
}

# Case-page footer. The supremecourt.gov link keeps its primacy -- it is what a
# practitioner came for and demoting it would be wrong; the browse row sits
# beneath it in muted ink. Full section names here, one-word labels in the
# masthead: the masthead is scanned before reading and must hold one line, the
# footer is read after and is the only place the site says what its sections are.
case_footer <- function(docket_url, stamp) {
  rows <- vapply(SITE_SECTIONS, function(s)
    paste0("<li><a href='", s$href, "'>", s$long, "</a></li>"), character(1))
  paste0(
    "<footer class='cfoot'><hr class='cfoot-rule'>",
    "<p class='cfoot-off'><a href='", docket_url, "' target='_blank' rel='noopener'>",
    "Full docket on supremecourt.gov&nbsp;&rarr;</a></p>",
    "<p class='cfoot-lab'>Elsewhere on Supreme Court Report</p>",
    "<ul class='cfoot-nav'>", paste(rows, collapse = ""), "</ul>",
    "<p class='cfoot-stamp'>Last refreshed ", .nav_esc(stamp), ".</p></footer>")
}

# Sequential nav for DATED leaf pages only. `prev`/`nxt` are list(href=, label=)
# or NULL; a NULL side renders as a dimmed non-link so the live side does not
# jump columns between consecutive visits.
#
# Case pages deliberately get none: docket numbers are assigned in filing order,
# so 24-1121 and 24-1123 have nothing to do with 24-1122. The control would be
# cheap and would offer two doors that lead nowhere the reader wanted, dressed as
# continuity. The sequence exists in the data but not in the world.
#' Splice the masthead into a page this codebase did not lay out.
#'
#' methods.html is a checked-in, hand-maintained document (docs/cert_model_methods.html)
#' that build_dashboards.R copies to the site verbatim. Patching it at COPY time
#' rather than editing the source keeps the source portable and means a
#' regenerated methods note picks the nav up automatically instead of silently
#' losing it. Returns TRUE if the file was patched.
#'
#' Position-based, and refuses rather than guesses: a page with no </head> or no
#' <body> is left exactly as it was and says so.
inject_masthead <- function(path, active = NULL) {
  if (!file.exists(path)) return(invisible(FALSE))
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (grepl("class='smast-wm'", txt, fixed = TRUE)) return(invisible(TRUE))  # idempotent
  h <- regexpr("</head>", txt, fixed = TRUE)
  b <- regexpr("<body", txt, fixed = TRUE)
  if (h < 0 || b < 0) {
    warning("inject_masthead(): ", basename(path),
            " has no </head> or <body>; left unmodified.", call. = FALSE)
    return(invisible(FALSE))
  }
  bclose <- regexpr(">", substr(txt, b, nchar(txt)), fixed = TRUE)
  if (bclose < 0) return(invisible(FALSE))
  bend <- b + bclose - 1L                       # index of the '>' closing <body ...>
  paste0(substr(txt, 1L, h - 1L), "<style>", NAV_CSS, "</style>",
         substr(txt, h, bend), site_masthead(active = active),
         substr(txt, bend + 1L, nchar(txt))) |>
    enc2utf8() |> writeLines(path, useBytes = TRUE)
  invisible(TRUE)
}

#' Fill every page's prev/next slot from the complete on-disk sequence.
#'
#' Runs as a POST-PASS, after the section index is rebuilt, because no generator
#' sees the whole set: the daily renders only the dates in the current fetch
#' window and render_conferences.R only conferences on/after a cutoff. Computing
#' neighbours at render time would freeze each page's "next" at whatever existed
#' the day it was written -- so the newest page of every batch would permanently
#' claim to be the most recent one. Every page instead emits an empty
#' <!--PNAV--><!--/PNAV--> slot and this rewrites it, which makes the pass
#' idempotent and the sequence always complete.
#'
#' `key` maps the filename vector to a sortable vector (ascending = oldest
#' first); `label` maps it to display strings.
patch_prev_next <- function(dir, pattern, kind, key, label,
                            end_label = "Most recent") {
  if (!dir.exists(dir)) return(invisible(0L))
  files <- list.files(dir, pattern = pattern)
  if (length(files) < 2L) return(invisible(0L))
  files <- files[order(key(files))]
  labs <- label(files)
  n <- 0L
  for (i in seq_along(files)) {
    p <- file.path(dir, files[i])
    txt <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    # Spliced by POSITION, not regex. These pages carry base64-inlined widget
    # libraries and run past 700KB; a (?s).*? sub over that is how scr_write_page
    # previously tripped PCRE's match limit and silently did nothing.
    a <- regexpr("<!--PNAV-->", txt, fixed = TRUE)
    b <- regexpr("<!--/PNAV-->", txt, fixed = TRUE)
    if (a < 0 || b < 0 || b < a) next
    pv <- if (i > 1L) list(href = files[i - 1L], label = labs[i - 1L]) else NULL
    nx <- if (i < length(files)) list(href = files[i + 1L], label = labs[i + 1L]) else NULL
    txt <- paste0(substr(txt, 1L, a + attr(a, "match.length") - 1L),
                  prev_next_nav(pv, nx, kind = kind, end_label = end_label),
                  substr(txt, b, nchar(txt)))
    writeLines(enc2utf8(txt), p, useBytes = TRUE)
    n <- n + 1L
  }
  message("prev/next: patched ", n, " of ", length(files), " ", kind, " page(s)")
  invisible(n)
}

prev_next_nav <- function(prev = NULL, nxt = NULL, kind = "page",
                          end_label = "Most recent") {
  side <- function(x, cls, dir, fallback) {
    if (is.null(x)) paste0("<span class='", cls, " none'><span class='dir'>", dir,
                           "</span><span class='lab'>", fallback, "</span></span>")
    else paste0("<a class='", cls, "' href='", x$href, "'><span class='dir'>", dir,
                "</span><span class='lab'>", .nav_esc(x$label), "</span></a>")
  }
  if (is.null(prev) && is.null(nxt)) return("")
  paste0("<nav class='pnav' aria-label='", .nav_esc(kind), "'>",
         side(prev, "pv", paste0("&larr; Previous ", kind), "Earliest"),
         side(nxt,  "nx", paste0("Next ", kind, " &rarr;"), end_label),
         "</nav>")
}
