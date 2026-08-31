# page_style.R ----------------------------------------------------------------
# Shared presentation layer for the static site: an editorial "law-review
# broadsheet" look (warm parchment, deep ink, single accent accent, Fraunces +
# Newsreader serifs) matching the Cert Funnel page, plus small helpers that fix
# two things gtsave() omits from every dashboard page: a <title> and a mobile
# viewport meta (without the latter, phones render at ~980px and links become
# hard to tap). Sourced by scotus_dash_new.R and conference_dash.R.

suppressPackageStartupMessages({
  library(htmltools)
})

# palette.R (the colour source) and then the sitewide nav components (NAV_CSS,
# site_masthead, site_breadcrumb, ...). Loaded unconditionally rather than behind
# an exists() guard: a missing NAV_CSS would render every masthead unstyled but
# still *present*, which is exactly the silent degradation this codebase has been
# bitten by three times. Fail loudly instead.
#
# Order matters. NAV_CSS is built by fill_palette(), so palette.R has to be in
# scope before site_nav.R is sourced, not merely by the time a page is written.
local({
  here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA)
  find <- function(f) {
    if (!is.na(here) && file.exists(file.path(here, f))) file.path(here, f)
    else if (file.exists(file.path("R", f))) file.path("R", f) else f
  }
  sys.source(find("palette.R"),  envir = globalenv())
  sys.source(find("site_nav.R"), envir = globalenv())
  sys.source(find("site_meta.R"), envir = globalenv())   # social_meta()
})

# Google Fonts used across the site (kept identical to the Cert Funnel page).
PAGE_FONTS_URL <- paste0(
  "https://fonts.googleapis.com/css2?",
  "family=Fraunces:ital,opsz,wght@0,9..144,500;0,9..144,600;1,9..144,500&",
  "family=Newsreader:ital,opsz,wght@0,6..72,400;0,6..72,500;0,6..72,600;1,6..72,400&",
  "display=swap")

# Base editorial styles for the index / landing pages. Colours come from
# palette.R; 40rem is the measure both .wrap and the masthead centre on.
INDEX_CSS <- paste0("\n  ", palette_root(), "
  *{box-sizing:border-box}
  html{-webkit-text-size-adjust:100%}
  body{font-family:'Newsreader',Georgia,serif;font-size:19px;line-height:1.6;
    color:var(--ink);background:var(--paper);margin:0;font-feature-settings:'onum' 1}
  body::before{content:'';position:fixed;inset:0;z-index:-1;pointer-events:none;
    opacity:.5;mix-blend-mode:multiply;
    background-image:url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='140' height='140'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.9' numOctaves='2' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)' opacity='0.045'/%3E%3C/svg%3E\");}
  .wrap{max-width:40rem;margin:0 auto;padding:3rem 1.4rem 4rem}
  .kicker{font:600 .78rem/1 'Newsreader';letter-spacing:.22em;text-transform:uppercase;
    color:var(--accent);margin:0 0 1rem}
  h1{font-family:'Fraunces',Georgia,serif;font-weight:600;
    font-size:clamp(2.4rem,7vw,3.6rem);line-height:1;letter-spacing:-.015em;
    margin:0 0 1rem;color:var(--ink)}
  h1 em{font-style:italic;font-weight:500;color:var(--accent)}
  .dek{font-size:1.15rem;line-height:1.5;color:var(--ink-soft);font-style:italic;
    margin:0 0 1.4rem}
  .brule{border:0;height:0;border-top:2px solid var(--ink);margin:1.4rem 0 .3rem;
    position:relative}
  .brule::after{content:'';position:absolute;left:0;top:4px;width:100%;
    border-top:1px solid var(--rule)}
  ul.idx{list-style:none;padding:0;margin:1.6rem 0}
  ul.idx li{border-bottom:1px solid var(--rule)}
  /* Whole row is one block-level link: a large, reliable tap target on mobile. */
  ul.idx a.row{display:flex;justify-content:space-between;align-items:baseline;
    gap:1rem;padding:.9rem .4rem;text-decoration:none;border:0;color:inherit}
  ul.idx a.row:hover{background:rgba(@accent:rgb@,.05)}
  ul.idx .d{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.16rem;
    color:var(--accent);line-height:1.25}
  ul.idx a.row:hover .d{text-decoration:underline;text-underline-offset:3px}
  ul.idx .count{color:var(--faint);font-size:.9rem;font-style:italic;white-space:nowrap;
    font-variant-numeric:tabular-nums}
  /* Compact horizontal strip of the most recent pages beneath a category row. */
  ul.idx .recent{display:flex;flex-wrap:wrap;align-items:baseline;
    gap:.3rem .85rem;padding:0 .4rem 1rem;margin-top:-.35rem}
  ul.idx .recent .rlabel{font:600 .68rem/1 'Newsreader';letter-spacing:.18em;
    text-transform:uppercase;color:var(--faint)}
  ul.idx .recent a{font-family:'Newsreader',Georgia,serif;font-size:.92rem;
    color:var(--ink-soft);text-decoration:none;font-variant-numeric:tabular-nums;
    border-bottom:1px solid var(--rule);padding-bottom:1px}
  ul.idx .recent a:hover{color:var(--accent);border-color:var(--accent)}
  /* Optional panel beneath the section index (home page: most-read cases). */
  .panel{margin:2.4rem 0 0}
  .panel h2{font:600 .78rem/1 'Newsreader';letter-spacing:.22em;text-transform:uppercase;
    color:var(--accent);margin:0 0 .25rem}
  .panel .pnote{color:var(--faint);font-size:.85rem;font-style:italic;margin:0 0 .5rem}
  /* --- Likeliest grants -----------------------------------------------------
     Its own rows rather than ol.mostread's: this panel carries a question under
     the caption, which that list has no room for, and a percentage that has to
     read as the row's subject rather than as a trailing note.
     The window toggle is two real radio inputs plus :has() -- no JavaScript.
     Both windows are rendered at build time and one is hidden, which is the only
     way a static site can offer the choice: there is no server to ask for the
     other one. Real radios, not styled buttons, so the control is reachable by
     keyboard and announced as a choice. */
  .lead{margin:0 0 1.9rem}
  .lead h2{margin-bottom:.35rem}
  .lead .pnote{margin:0 0 .7rem}
  .wr{position:absolute;width:1px;height:1px;opacity:0;pointer-events:none}
  .win{display:inline-flex;gap:.25rem;margin:0 0 .55rem;
    border:1px solid var(--rule);border-radius:2px;padding:2px}
  .win label{font-family:'Newsreader',Georgia,serif;font-size:.82rem;line-height:1;
    padding:.34rem .62rem;color:var(--faint);cursor:pointer;border-radius:1px}
  .win label:hover{color:var(--ink)}
  .lead:has(#w7:checked) label[for=w7],
  .lead:has(#w28:checked) label[for=w28]{background:var(--accent);color:var(--paper)}
  .wr:focus-visible+label{outline:2px solid var(--accent);outline-offset:2px}
  .lead:has(#w7:checked) .g28,.lead:has(#w7:checked) .n28,
  .lead:has(#w28:checked) .g7,.lead:has(#w28:checked) .n7{display:none}
  /* --- Upcoming at the Court ------------------------------------------------
     A date gutter and a two-line body, the same shape as the grant rows above
     it, so the two panels read as one page rather than two widgets. */
  .cal{margin:0 0 1.9rem}
  ol.cal{list-style:none;margin:0;padding:0}
  ol.cal li{border-top:1px solid var(--rule)}
  ol.cal li:last-child{border-bottom:1px solid var(--rule)}
  /* The row is a container, not an anchor: an argument day carries one link per
     case it hears and anchors cannot nest. Hover lives on the row so it still
     reads as one unit, and each link colours on its own hover. */
  ol.cal .crow{display:flex;gap:.95rem;align-items:baseline;padding:.55rem .1rem}
  ol.cal .crow:hover{background:rgba(@accent:rgb@,.05)}
  ol.cal a{text-decoration:none;color:inherit}
  ol.cal a:hover,ol.cal a:focus-visible{color:var(--accent)}
  ol.cal .cdet a{border-bottom:1px solid rgba(@link:rgb@,.35)}
  ol.cal .cdet a:hover{border-bottom-color:var(--accent)}
  ol.cal .cwhen{flex:none;width:4.6rem;font-family:'Fraunces',Georgia,serif;
    font-weight:600;font-size:.96rem;color:var(--ink);white-space:nowrap;
    font-variant-numeric:tabular-nums}
  ol.cal .cdow{display:block;font-family:'Newsreader',Georgia,serif;font-weight:400;
    font-size:.7rem;color:var(--faint);text-transform:uppercase;letter-spacing:.08em}
  ol.cal .ctx{flex:1;min-width:0}
  ol.cal .ckind{display:block;font-family:'Fraunces',Georgia,serif;font-weight:600;
    font-size:.98rem;line-height:1.25}
  ol.cal .cdet{display:block;font-size:.9rem;line-height:1.4;color:var(--ink-soft);
    margin-top:.12rem}
  ol.grants{list-style:none;margin:0;padding:0}
  ol.grants li{border-top:1px solid var(--rule)}
  ol.grants li:last-child{border-bottom:1px solid var(--rule)}
  ol.grants a{display:flex;gap:.95rem;align-items:baseline;text-decoration:none;
    color:inherit;padding:.62rem .1rem}
  ol.grants a:hover{background:rgba(@accent:rgb@,.05)}
  ol.grants a:hover .gc{color:var(--accent)}
  ol.grants .gv{flex:none;width:3.5rem;text-align:right;
    font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.22rem;
    line-height:1.1;color:var(--accent);font-variant-numeric:tabular-nums}
  ol.grants .gx{font-family:'Newsreader',Georgia,serif;font-weight:400;
    font-size:.7rem;color:var(--faint);margin-left:.3rem}
  ol.grants .gtx{flex:1;min-width:0}
  ol.grants .gc{display:block;font-family:'Fraunces',Georgia,serif;font-weight:600;
    font-size:1rem;line-height:1.25}
  ol.grants .gq{display:block;font-size:.9rem;line-height:1.4;color:var(--ink-soft);
    margin-top:.16rem}
  ol.grants .gd{flex:none;font-size:.76rem;color:var(--faint);white-space:nowrap;
    font-variant-numeric:tabular-nums}
  .more{margin:.75rem 0 0;font-size:.92rem}
  .more a{color:var(--link);text-decoration:none;
    border-bottom:1px solid rgba(@link:rgb@,.35)}
  .more a:hover{border-bottom-color:var(--link)}
  h2.sec{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:.88rem;
    letter-spacing:.09em;color:var(--faint);text-transform:uppercase;
    margin:2.1rem 0 .6rem;padding-bottom:.28rem;border-bottom:1px solid var(--rule)}
  @media(max-width:40rem){
    ol.grants a{flex-wrap:wrap}
    ol.grants .gd{width:100%;padding-left:4.45rem;margin-top:-.1rem}
  }
  ol.mostread{list-style:none;counter-reset:mr;padding:0;margin:0}
  ol.mostread li{counter-increment:mr;border-bottom:1px solid var(--rule)}
  ol.mostread a{display:flex;align-items:baseline;gap:.7rem;padding:.6rem .4rem;
    text-decoration:none;color:inherit}
  ol.mostread a:hover{background:rgba(@accent:rgb@,.05)}
  ol.mostread a::before{content:counter(mr);font-family:'Fraunces',Georgia,serif;
    font-weight:600;font-size:.95rem;color:var(--faint);min-width:1.1rem;
    font-variant-numeric:tabular-nums}
  ol.mostread .mc{flex:1;font-size:1rem;line-height:1.3}
  ol.mostread a:hover .mc{color:var(--accent);text-decoration:underline;
    text-underline-offset:3px}
  ol.mostread .mdk{color:var(--accent);font-weight:600;font-size:.82rem;
    font-variant-numeric:tabular-nums;white-space:nowrap}
  ol.mostread .mv{color:var(--faint);font-size:.85rem;font-style:italic;
    white-space:nowrap;font-variant-numeric:tabular-nums}
  /* Forecast percentages carry weight the most-read counts do not: the number is
     the claim, not a footnote to the ranking. Upright, accent, tabular. */
  ol.mostread .mv.fc{color:var(--accent);font-style:normal;font-weight:600;
    font-size:.95rem;min-width:2.6rem;text-align:right}
  /* Feed follow line. Deliberately quiet -- a footnote under the section list,
     styled like .recent rather than like a call to action. */
  .feeds{display:flex;flex-wrap:wrap;align-items:baseline;gap:.3rem .85rem;
    margin:2.2rem 0 0;padding-top:1.1rem;border-top:1px solid var(--rule)}
  .feeds .flabel{font:600 .68rem/1 'Newsreader';letter-spacing:.18em;
    text-transform:uppercase;color:var(--faint)}
  .feeds a{font-family:'Newsreader',Georgia,serif;font-size:.92rem;
    color:var(--link);text-decoration:none;
    border-bottom:1px solid rgba(@link:rgb@,.4)}
  .feeds a:hover{border-color:var(--link)}
  .back{margin-top:2rem;font-size:.95rem}
  .back a{color:var(--link);text-decoration:none;
    border-bottom:1px solid rgba(@link:rgb@,.4)}
  .back a:hover{border-color:var(--link)}
  /* Home-page case search. */
  .csearch{position:relative;margin:0 0 1.4rem}
  #cq{width:100%;font-family:'Newsreader',Georgia,serif;font-size:1.05rem;color:var(--ink);
    background:var(--panel);border:1px solid var(--rule);border-radius:3px;padding:.7rem .9rem}
  #cq:focus{outline:none;border-color:var(--accent);box-shadow:0 0 0 3px rgba(@accent:rgb@,.1)}
  #cq::placeholder{color:var(--faint)}
  #cq.loading{background-image:linear-gradient(90deg,transparent,rgba(@accent:rgb@,.06),transparent);
    background-size:40% 100%;background-repeat:no-repeat;animation:csl 1s infinite}
  @keyframes csl{0%{background-position:-40% 0}100%{background-position:140% 0}}
  .cres{list-style:none;margin:.35rem 0 0;padding:0;max-height:24rem;overflow-y:auto;
    border:1px solid var(--rule);border-radius:3px;background:var(--panel)}
  .cres:empty{display:none}
  .cres li{border-bottom:1px solid var(--rule)}
  .cres li:last-child{border-bottom:0}
  .cres a{display:block;padding:.55rem .7rem;text-decoration:none;color:var(--ink);
    font-size:.98rem;line-height:1.3}
  .cres a:hover{background:rgba(@accent:rgb@,.06)}
  .cres .cd{color:var(--accent);font-variant-numeric:tabular-nums;font-weight:600;
    margin-right:.5rem;white-space:nowrap}
  .cnone{padding:.55rem .7rem;color:var(--faint);font-style:italic}
  /* /cases/ browse index: grouped sections with per-bucket counts. */
  .csec{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.02rem;
    text-transform:uppercase;letter-spacing:.12em;color:var(--accent);
    margin:2.2rem 0 .3rem;padding-bottom:.35rem;border-bottom:1px solid var(--rule);
    display:flex;justify-content:space-between;align-items:baseline;gap:1rem}
  .csec .cn{font-family:'Newsreader',Georgia,serif;font-weight:400;font-size:.85rem;
    letter-spacing:0;text-transform:none;color:var(--faint);
    font-variant-numeric:tabular-nums}
  .cnote{color:var(--faint);font-size:.85rem;font-style:italic;margin:.5rem 0 0}
  .cnote a{color:var(--accent)}
  ul.terms{list-style:none;padding:0;margin:1rem 0 0;display:flex;flex-wrap:wrap;
    gap:.5rem 1.6rem}
  ul.terms li{display:inline-flex;align-items:baseline;gap:.4rem}
  ul.terms a{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.05rem;
    color:var(--accent);text-decoration:none;border-bottom:1px solid var(--rule)}
  ul.terms a:hover{border-bottom-color:var(--accent)}
  ul.terms .cn{color:var(--faint);font-size:.85rem;font-variant-numeric:tabular-nums}
  /* About page: running prose rather than an index, so it takes a measure and a
     little more leading than the section rows above it. */
  .about{margin:1.6rem 0 0;font-size:1.05rem;line-height:1.65;max-width:34rem}
  .about p{margin:0 0 1rem}
  .about a{color:var(--accent);text-decoration:none;
    border-bottom:1px solid var(--rule)}
  .about a:hover{border-bottom-color:var(--accent)}
  .about .contact{margin:2rem 0 0;background:var(--panel);border:1px solid var(--rule);
    border-left:4px solid var(--accent);padding:1.1rem 1.3rem}
  .about .contact h2{font:600 .78rem/1 'Newsreader',Georgia,serif;letter-spacing:.2em;
    text-transform:uppercase;color:var(--accent);margin:0 0 .5rem}
  .about .contact p{margin:0;font-size:1rem}
") |> fill_palette()

# Home-page case search: a lazy-loaded client-side index (docket -> caption).
SEARCH_HTML <- paste0(
  "<div class='csearch'><input type='search' id='cq' autocomplete='off' spellcheck='false' ",
  "placeholder='Search all cases by name or docket number…' aria-label='Search cases'>",
  "<ul id='cres' class='cres' role='listbox'></ul></div>")

# `json` and `href_prefix` are relative to the PAGE being written: the landing
# page reaches the index at cases/search.json, the /cases/ index at search.json.
# Getting this wrong yields /cases/cases/search.json and a search box that
# silently never returns a result, so it is a parameter rather than a constant.
search_script <- function(json = "cases/search.json", href_prefix = "cases/")
  gsub("@JSON@", json, gsub("@HREF@", href_prefix, SEARCH_SCRIPT, fixed = TRUE), fixed = TRUE)

SEARCH_SCRIPT <- paste0("<script>(function(){",
  "var q=document.getElementById('cq'),r=document.getElementById('cres'),E=null,t;",
  "function esc(s){return s.replace(/[&<>]/g,function(c){return {'&':'&amp;','<':'&lt;','>':'&gt;'}[c];});}",
  "function load(){if(E)return;q.classList.add('loading');",
  "fetch('@JSON@').then(function(x){return x.json();}).then(function(j){",
  "E=Object.keys(j).map(function(d){return [d,j[d],d.toLowerCase(),j[d].toLowerCase()];});",
  "q.classList.remove('loading');run();}).catch(function(){q.classList.remove('loading');});}",
  "q.addEventListener('focus',load);",
  "q.addEventListener('input',function(){clearTimeout(t);t=setTimeout(run,110);});",
  "function run(){var s=q.value.trim().toLowerCase();if(!s||!E){r.innerHTML='';return;}",
  "var o=[],n=0;for(var i=0;i<E.length;i++){if(E[i][2].indexOf(s)>-1||E[i][3].indexOf(s)>-1){",
  "o.push(E[i]);if(++n>=40)break;}}",
  "r.innerHTML=o.length?o.map(function(e){return \"<li><a href='@HREF@\"+e[0]+\".html'>",
  "<span class='cd'>No. \"+e[0]+\"</span>\"+esc(e[1])+\"</a></li>\";}).join(''):",
  "\"<li class='cnone'>No matching cases.</li>\";}})();</script>")

# Strip the party-role tail the Court appends to a docket caption, for DISPLAY
# only. "United States v. E. Jean Carroll, et al." is how the docket reads; on a
# ranked panel five rows deep it is four words of boilerplate in the widest
# column, repeated.
#
# One pattern, because there were four and they had already drifted: the daily
# stripped Applicants but not Respondents, conferences and arguments stripped
# Respondents but not Applicants, and the landing-page forecast panel stripped
# neither "et al." nor anything else it did not share with the other three.
# Appellants/Appellees are here because the paid docket carries 28 U.S.C. 1253
# direct appeals -- the first cases this panel ever published were captioned
# "Appellants" (see R/site_forecast.R).
#
# DISPLAY ONLY. court_bucket() keys USDC_APPEAL off "\\bAppellants?\\b" in the
# raw caption and score_case() is handed that raw caption, so this must never be
# fed back into scoring -- every caller cleans a copy on its way into a cell.
CAPTION_ROLE_TAIL <- paste0(
  ", Petitioners?|, Respondents?|, Applicants?|, Appellants?|, Appellees?",
  "|, et al\\.")

strip_caption_roles <- function(x) {
  if (is.null(x)) return(x)
  trimws(gsub("\\s+", " ", gsub(CAPTION_ROLE_TAIL, "", x)))
}

# Convert straight quotes/apostrophes in DISPLAY text to typographic ("smart")
# ones. HTML tags (<...>) are passed through untouched so attribute quotes and
# markup survive; existing entities (&rsquo;, &mdash;) are already curly and are
# left alone. Vectorised. Apply only to human-readable prose -- never to a JSON
# widget payload or a URL, where a " is structural.
smarten <- function(x) {
  if (is.null(x)) return(x)
  os <- "‘"; cs <- "’"; od <- "“"; cd <- "”"
  one <- function(s) {
    if (is.na(s) || !nzchar(s)) return(s)
    segs <- regmatches(s, gregexpr("<[^>]*>|[^<]+", s, perl = TRUE))[[1]]
    segs <- vapply(segs, function(p) {
      if (startsWith(p, "<")) return(p)                                    # HTML tag
      p <- gsub('(^|[[:space:](])"', paste0("\\1", od), p, perl = TRUE)    # opening "
      p <- gsub('"', cd, p, fixed = TRUE)                                  # closing "
      p <- gsub(paste0('(^|[[:space:](', od, "])'"), paste0("\\1", os), p, perl = TRUE) # opening '
      p <- gsub("'", cs, p, fixed = TRUE)                                  # apostrophe / closing '
      p
    }, character(1), USE.NAMES = FALSE)
    paste0(segs, collapse = "")
  }
  vapply(x, one, character(1), USE.NAMES = FALSE)
}

# Smarten quotes across a whole HTML string: skip <style>/<script> blocks and all
# tag internals (attributes), smartening only the visible text nodes. Safe for a
# STATIC page (no client-side JSON). Do NOT use on a page carrying a reactable /
# htmlwidget JSON payload -- there a straight " is structural (use smarten() on
# the prose fields instead, as scr_write_page() does).
smarten_html <- function(html) {
  segs <- regmatches(html, gregexpr(
    "(?is)<style\\b[^>]*>.*?</style>|<script\\b[^>]*>.*?</script>|<[^>]*>|[^<]+",
    html, perl = TRUE))[[1]]
  out <- vapply(segs, function(p) if (startsWith(p, "<")) p else smarten(p),
                character(1), USE.NAMES = FALSE)
  paste0(out, collapse = "")
}

# Which feeds the published site actually has, as root-absolute paths.
#
# page_head() advertises only these. Advertising both unconditionally put a
# <link rel="alternate"> to a 404 on every index page for one release, because
# grants.xml turned out never to be written.
#
# Resolved from the gh-pages checkout, so it reflects what the PREVIOUS run
# published: a feed first written at the end of run N is advertised from run N+1.
# That one-run lag is the price of never emitting a dangling link, and it is the
# right way round -- a link to a feed that exists is always correct, a link to one
# that might exist is not.
#
# This lives HERE, beside its only consumer, rather than in feeds.R. It was in
# feeds.R and read through a SITE_FEEDS global that only build_dashboards.R set --
# so /conferences/, /arguments/ and /funnel/, whose renderers never set it and do
# not source feeds.R before writing their pages, advertised nothing at all.
# Making the link conditional fixed a wrong link and introduced a missing one on
# half the site, which is quieter and therefore worse.
#
# The default reads SITE_DIR, the environment convention every render entry point
# already uses, so a new caller gets the right answer without having to know any
# of this exists. Resolving on each call rather than pinning a global is safe:
# a page can only ever advertise a feed that is on disk when the page is written,
# which is the invariant that matters regardless of when the feeds get written.
site_feeds_present <- function(site_dir = Sys.getenv("SITE_DIR", unset = "site")) {
  f <- c("/feed.xml", "/grants.xml")
  f[file.exists(file.path(site_dir, sub("^/", "", f)))]
}

FEED_TITLES <- c("/feed.xml" = "Supreme Court Report",
                 "/grants.xml" = "Certiorari grants")

# A visible "follow by feed" line, or NULL when there are no feeds to offer.
#
# The feeds shipped with autodiscovery only -- a <link rel="alternate"> in the
# head and nothing a reader can see or copy. That makes them discoverable by
# software and invisible to people, which for a feature whose whole purpose was
# reach is most of the value left on the table.
#
# Labels are reader-facing, not file names: someone deciding whether to subscribe
# cares about "everything" versus "grants only", not about feed.xml.
FEED_LABELS <- c("/feed.xml" = "All updates", "/grants.xml" = "Certiorari grants")

feed_follow_line <- function(label = "Follow by feed") {
  f <- site_feeds_present()
  if (!length(f)) return(NULL)
  tags$p(
    class = "feeds",
    tags$span(class = "flabel", label),
    lapply(f, function(x) tags$a(href = x, type = "application/atom+xml",
                                 FEED_LABELS[[x]])))
}

# The <link rel="alternate"> tags themselves, as one raw string.
#
# Factored out because there are TWO hand-built heads on this site, not one: the
# funnel page carries its own fonts and stylesheet and so does not go through
# page_head(). Putting the loop only in page_head() left /funnel/ advertising
# nothing while the other six pages were fixed -- the audit caught it, but a
# shared helper is what stops it happening to a third head.
feed_autodiscovery_links <- function() {
  paste0(vapply(site_feeds_present(), function(f) sprintf(
    '<link rel="alternate" type="application/atom+xml" title="%s" href="%s">',
    FEED_TITLES[[f]], f), character(1)), collapse = "")
}

# Raw <head> for an index page (built as a string because htmltools drops the
# <head> singleton from as.character()).
#
# `extra_css` is appended AFTER INDEX_CSS and NAV_CSS, inside the same <style>,
# for a page that needs rules the shared sheet has no business carrying (the
# counsel leaderboards). It exists so such a page can still come through here:
# the alternative is a hand-built <head>, and this site has four of those, each
# of which has silently missed a sitewide head change at least once.
# `description` and `path` feed social_meta(): the one-or-two sentences a shared
# link shows, and the canonical URL. Both default to NULL so a caller that has
# nothing sensible to say says nothing, rather than shipping a card captioned
# with boilerplate.
# `extra_head` is raw markup appended last, for the rare page that needs a tag
# the shared head has no business carrying sitewide -- currently only the 404's
# robots noindex.
page_head <- function(title, jsonld = NULL, extra_css = NULL,
                      description = NULL, path = NULL,
                      og_type = c("website", "article"), extra_head = NULL) {
  og_type <- match.arg(og_type)
  paste0(
    "<head>",
    "<script async src='/analytics.js'></script>",
    '<meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    '<link rel="icon" href="/favicon.svg" type="image/svg+xml"><link rel="alternate icon" href="/favicon.ico" sizes="any">',
    # Feed autodiscovery, on every index page rather than only the landing page:
    # a reader's "subscribe" button looks at whatever page they are standing on,
    # and the section indexes are where a returning reader lands.
    #
    # Resolved from disk on every call. There is deliberately no global to set and
    # therefore none to forget: the previous design had build_dashboards.R assign
    # a SITE_FEEDS global, which meant the three renderers that never assigned it
    # advertised nothing at all.
    feed_autodiscovery_links(),
    "<title>", htmlEscape(title), "</title>",
    social_meta(title, description, path, og_type),
    '<link rel="preconnect" href="https://fonts.googleapis.com">',
    '<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>',
    '<link rel="stylesheet" href="', PAGE_FONTS_URL, '">',
    "<style>", INDEX_CSS, NAV_CSS, if (!is.null(extra_css)) extra_css else "",
    "</style>",
    if (!is.null(jsonld)) jsonld else "",
    if (!is.null(extra_head)) extra_head else "",
    "</head>")
}

# A ranked "most-read" panel from a data frame of docket / caption / views /
# href (see top_viewed_cases() in R/site_analytics.R). Returns NULL for zero
# rows so the caller can pass the result straight through: no data, no block.
#
# `note` should say plainly what window the ranking covers. The counts are real
# page views, not a smoothed or modelled figure, and are labelled as such.
most_read_panel <- function(df, heading = "Most-Read Cases", note = NULL,
                            show_counts = TRUE) {
  if (is.null(df) || !nrow(df)) return(NULL)
  rows <- lapply(seq_len(nrow(df)), function(i) {
    tags$li(tags$a(
      href = df$href[i],
      tags$span(class = "mc", smarten(df$caption[i])),
      tags$span(class = "mdk", paste0("No. ", df$docket[i])),
      # One text node, not two: htmltools joins sibling children with a newline,
      # which HTML would collapse to "55 views" anyway but leaves the markup
      # reading as though the number and its unit were separate fields.
      if (isTRUE(show_counts))
        tags$span(class = "mv", paste0(format(df$views[i], big.mark = ","),
                                       if (df$views[i] == 1) " view" else " views"))
    ))
  })
  tags$section(
    class = "panel",
    tags$h2(heading),
    if (!is.null(note)) tags$p(class = "pnote", smarten(note)),
    tags$ol(class = "mostread", rows)
  )
}

# The About page: who makes this, where the code is, and how to report a problem.
#
# The contact line is the point of the page, not a footnote to it. This site
# publishes ~55,000 machine-generated pages about real litigation, and the people
# best placed to catch an error -- a wrong caption, a misread docket entry, a
# forecast cue that names the wrong mechanism -- are the lawyers who arrive from
# a search result. Giving them somewhere to send it is the whole job, which is
# why "About" is in SITE_SECTIONS rather than tucked into the landing page: it
# has to be reachable from a case page, since that is where readers actually are.
# The 404. GitHub Pages serves this file for ANY path it cannot resolve, and
# that one fact drives the whole design:
#
#   * Every href and fetch is root-absolute. The browser's URL stays at the
#     path the reader asked for, so a relative "cases/search.json" resolves to
#     /cases/cases/search.json from a case URL and the search silently returns
#     nothing -- the exact failure search_script()'s own comment warns about.
#   * The page cannot know what was requested at build time, so the one thing
#     it can usefully do about it is read location.pathname in the browser.
#
# Case pages are 99.5% of the site, so a mistyped or stale docket number is by
# far the likeliest way to land here. That is worth answering specifically
# rather than with a generic apology: the script below recognises a docket-
# shaped path, names the number that is not on file, and pre-runs the search
# for it.
write_404_page <- function(out_path) {
  # Root-absolute, because this page is served from paths it cannot predict.
  search_html <- search_script("/cases/search.json", "/cases/")

  # Docket-aware opener. Kept to one small script rather than a framework:
  #   /cases/24-1234.html -> "No. 24-1234 is not on file here."
  # and the search box is seeded with the number so the reader sees near
  # matches (a renumbered docket, the same case at a different stage) without
  # retyping it.
  #
  # dispatchEvent rather than focus(): the search loads its index on focus, and
  # calling focus() would yank the caret and the screen-reader cursor away from
  # the top of the page. Dispatching the event runs the same listener without
  # moving anything.
  dkt_script <- paste0("<script>(function(){",
    "var m=location.pathname.match(/\\/cases\\/([0-9]{2}[A-Za-z]?-?[0-9]+)\\.html$/);",
    "if(!m)return;var d=m[1];",
    "var h=document.getElementById('nf-dkt');",
    "if(h){h.textContent='No. '+d+' is not on file here.';h.hidden=false;}",
    "var q=document.getElementById('cq');",
    "if(q){q.value=d;q.dispatchEvent(new Event('focus'));",
    "q.dispatchEvent(new Event('input'));}",
    "})();</script>")

  body <- tags$body(
    HTML(site_masthead()),
    tags$main(
      id = "main", class = "wrap",
      tags$p(class = "kicker", "Not found"),
      tags$h1("No page at that address"),
      tags$hr(class = "brule"),
      # Filled in by the script when the path names a docket; hidden otherwise,
      # so a reader who lands here from a non-case URL is not told about a
      # docket number that was never in play.
      tags$p(id = "nf-dkt", class = "dek", hidden = NA, ""),
      tags$p(class = "dek", smarten(paste(
        "The link may be mistyped or out of date. Every docket the Court has",
        "opened since October Term 2017 has a page here -- search for a case",
        "by name or number below, or start from one of the sections."))),
      HTML(SEARCH_HTML),
      tags$ul(class = "idx", lapply(SITE_SECTIONS, function(sec)
        tags$li(tags$a(
          class = "row", href = sec$href,
          tags$span(class = "d", HTML(sec$label)),
          tags$span(class = "count", HTML(sec$long)))))),
      # HTML(), not a bare string: htmltools escapes the text children of a tag,
      # so smarten("&larr; ...") shipped a literal "&amp;larr;" and the page
      # showed the entity instead of the arrow. The section labels two lines up
      # already use HTML() for the same reason. The leaf pages get away with the
      # bare entity because scr_write_page() pastes its back link as a string
      # rather than building it with tags.
      tags$p(class = "back", tags$a(href = "/", HTML("&larr; Supreme Court Report")))
    ),
    # No footer block: about.html does not carry one either, and case_footer()
    # is specific to a docket page (it links out to that case on
    # supremecourt.gov). The masthead is the navigation here.
    HTML(search_html),
    HTML(dkt_script)
  )
  html <- paste0("<!DOCTYPE html>\n<html lang=\"en\">\n",
    page_head("Page not found — Supreme Court Report",
              description = paste(
                "That address does not match a page on Supreme Court Report.",
                "Search the full docket by case name or number."),
              # No canonical: this file answers for every unresolved path, and
              # naming one URL as its canonical would be a lie on all the others.
              path = NULL,
              # A 404 is served with a 404 status, which crawlers already
              # respect -- but /404.html itself returns 200 if visited directly,
              # and that copy should not be indexed.
              extra_head = "<meta name=\"robots\" content=\"noindex\">"),
    "\n", as.character(body), "\n</html>\n")
  writeLines(enc2utf8(smarten_html(html)), out_path, useBytes = TRUE)
  invisible(out_path)
}

write_about_page <- function(out_path) {
  a <- function(href, ...) tags$a(href = href, target = "_blank", rel = "noopener", ...)
  body <- tags$body(
    HTML(site_masthead(active = "/about.html")),
    tags$main(
      id = "main", class = "wrap",
      tags$p(class = "kicker", "About"),
      tags$h1("Supreme Court Report"),
      tags$hr(class = "brule"),
      tags$p(class = "dek", smarten(paste(
        "Quantifying the U.S. Supreme Court's behavior and making it legible",
        "for the public."))),
      tags$div(
        class = "about",
        tags$p(HTML(paste0(
          "Supreme Court Report is built and maintained by ",
          as.character(a("https://tommybennett.com", "Tommy Bennett")), "."))),
        tags$p(smarten(paste(
          "Every page here is generated from the Court's own public docket data.",
          "The daily dashboards, the conference reports, the argument navigator",
          "and a page for each of the roughly 55,000 dockets on file are all",
          "pre-rendered as static HTML -- there is no server, no tracking beyond",
          "aggregate page views, and no paywall."))),
        tags$p(HTML(paste0(
          "The cert-grant forecasts are estimates from a statistical model, not ",
          "predictions about any particular case. How the model is built, what it ",
          "gets right and where it is weakest are set out in ",
          as.character(tags$a(href = "/methods.html", "the methods note")), "."))),
        tags$p(HTML(paste0(
          "The code that fetches the data, fits the models and writes these pages ",
          "is open source: ",
          as.character(a("https://github.com/baldrige/ceRt", "github.com/baldrige/ceRt")),
          "."))),
        # Written as prose rather than as the .feeds strip used on the landing
        # page: About is where someone goes to find out what exists, so the
        # sentence explaining the difference between the two feeds is the useful
        # part.
        #
        # Built from site_feeds_present(), not hardcoded. Naming both feeds in
        # fixed prose and merely CHECKING that some feed exists is precisely the
        # bug the autodiscovery links shipped with once already: a link to a
        # grants.xml that was never written. Each clause is emitted only if its
        # own file is there.
        local({
          have <- site_feeds_present()
          clause <- c(
            "/feed.xml" = paste0(
              as.character(tags$a(href = "/feed.xml", type = "application/atom+xml",
                                  "all updates")),
              " carries new grants, conference reports and the daily docket"),
            "/grants.xml" = paste0(
              as.character(tags$a(href = "/grants.xml", type = "application/atom+xml",
                                  "certiorari grants")),
              " carries grants alone, which is a handful a month"))
          if (!length(have)) return(NULL)
          tags$p(HTML(paste0(
            "Updates are published as Atom feeds, which any feed reader can ",
            "follow: ", paste(clause[have], collapse = "; "), ".")))
        }),
        tags$section(
          class = "contact",
          tags$h2("Found a problem?"),
          tags$p(HTML(paste0(
            "Corrections and questions are welcome — email ",
            as.character(tags$a(href = "mailto:tbbennett@smu.edu", "tbbennett@smu.edu")),
            ". These pages are generated automatically, so an error in one is ",
            "usually an error in many; reports are genuinely useful."))))
      )
    ))
  html <- paste0("<!DOCTYPE html>\n<html lang=\"en\">\n",
                 page_head("About — Supreme Court Report",
                   description = paste(
                     "Who publishes Supreme Court Report, where the docket data comes from,",
                     "and what the forecasts do and do not claim."),
                   path = "/about.html"), "\n",
                 as.character(body), "\n</html>\n")
  writeLines(enc2utf8(html), out_path, useBytes = TRUE)
  invisible(out_path)
}

# A "likeliest grants" panel from a data frame of dkt / caption / prob / lift /
# href (see top_forecast_petitions() in R/site_forecast.R). Returns NULL for zero
# rows, like most_read_panel(), so the caller passes the result straight through.
#
# The probability is ALWAYS printed. The most-read panel deliberately withholds
# its counts -- there the ordering is the story and the raw numbers would leak
# the site's traffic volume -- but here the number *is* the claim, and a rank
# without it would assert a distinction the reader cannot check. `note` must
# carry the base rate: 14% reads as "unlikely" until you know the floor is 4.1%.
# "Upcoming at the Court": the next conferences and argument days, chronological.
#
# Dates carry a weekday. The Court's calendar is a weekly rhythm -- conferences
# on Fridays, arguments Monday through Wednesday -- and "Mon Oct 5" tells a
# reader something "Oct 5" does not.
#
# Returns NULL on no events, which is the correct state for most of the summer:
# an empty "Upcoming" heading is worse than no heading, because it reads as a
# thing that broke rather than a Court that is in recess.
# Put `sep` between every pair of `xs`, so a list of links renders as one run of
# text rather than needing a separator baked into each item.
.interleave <- function(xs, sep) {
  if (!length(xs)) return(list())
  out <- vector("list", 2L * length(xs) - 1L)
  out[seq(1, length(out), by = 2L)] <- xs
  if (length(xs) > 1L) out[seq(2, length(out), by = 2L)] <- list(sep)
  out
}

calendar_panel <- function(events, heading = "Upcoming at the Court",
                           note = NULL) {
  if (is.null(events) || !nrow(events)) return(NULL)
  # The row is no longer one big anchor. An argument day names every case it
  # hears and each one links to its own page, and anchors cannot nest -- so the
  # row is a plain container, the label links to the day's page (the argument
  # session, or that conference's report), and each case links to itself.
  has_items <- "items" %in% names(events)
  rows <- lapply(seq_len(nrow(events)), function(i) {
    d <- events$date[i]
    it <- if (has_items) events$items[[i]] else NULL
    detail <- if (is.data.frame(it) && nrow(it)) {
      # Separated by a middot, not a comma: captions contain commas of their own
      # ("Trump, President of the United States v. ..."), and a comma list of
      # them cannot be read apart.
      linked <- lapply(seq_len(nrow(it)), function(k)
        tags$a(href = paste0("cases/", it$dkt[k], ".html"), smarten(it$cap[k])))
      tags$span(class = "cdet",
                do.call(tagList, .interleave(linked, HTML(" &middot; "))))
    } else tags$span(class = "cdet", smarten(events$detail[i]))
    tags$li(tags$div(
      class = "crow",
      tags$span(class = "cwhen",
                tags$span(class = "cdow", format(d, "%a")),
                format(d, "%b %e")),
      tags$span(class = "ctx",
                tags$a(class = "ckind", href = events$href[i], events$label[i]),
                detail)))
  })
  tags$section(
    class = "panel cal",
    tags$h2(heading),
    if (!is.null(note)) tags$p(class = "pnote", smarten(note)),
    tags$ol(class = "cal", rows))
}

# The Likeliest-Grants panel: the site's front-page answer to "what should I
# look at". Each row is a case, its forecast, and -- where one could be reduced
# to a single readable line -- the question it asks.
#
# `short` and `long` are top_forecast_cases() frames for the two windows; either
# may be zero rows, because the panel suppresses itself when too few cases clear
# the lift floor. All three combinations are handled:
#   both        -> the toggle
#   one only    -> that window, rendered plain, with no control to click
#   neither     -> NULL, and the caller drops the panel entirely
# A toggle with one working side would be worse than no toggle: it invites a
# click that silently does nothing.
#
# `qp` is a named dkt -> one-line question vector (qp_lines_from_cache()). A
# missing entry is normal and shows the caption alone -- see qp_line() for why
# roughly a quarter of cases have no usable line.
forecast_panel <- function(short, long = NULL, qp = character(),
                           base_rate = NULL, heading = "Likeliest grants",
                           note_short = NULL, note_long = NULL,
                           links = NULL) {
  has <- function(d) !is.null(d) && is.data.frame(d) && nrow(d) > 0
  if (!has(short) && !has(long)) return(NULL)

  rows <- function(df, cls) {
    if (!has(df)) return(NULL)
    lis <- lapply(seq_len(nrow(df)), function(i) {
      dkt <- df$dkt[i]
      q   <- if (!is.null(qp) && dkt %in% names(qp)) unname(qp[[dkt]]) else NA_character_
      # lift comes off the frame rather than being recomputed here: it is the
      # model's own number, and dividing by a base rate this function was handed
      # separately is how the two quietly disagree.
      mult <- if (!is.null(df$lift) && is.finite(df$lift[i]))
        sprintf("%.0f×", df$lift[i]) else NULL
      tags$li(tags$a(
        href = df$href[i],
        tags$span(class = "gv", sprintf("%d%%", round(100 * df$prob[i])),
                  if (!is.null(mult)) tags$span(class = "gx", mult)),
        tags$span(class = "gtx",
                  tags$span(class = "gc", smarten(df$caption[i])),
                  if (!is.na(q)) tags$span(class = "gq", smarten(q))),
        tags$span(class = "gd", paste0("No. ", dkt))))
    })
    tags$ol(class = paste("grants", cls), lis)
  }

  both <- has(short) && has(long)
  toggle <- if (both) tagList(
    # Checked state lives on the SHORT window, so a reader who never touches the
    # control sees the same seven days the panel has always shown.
    tags$input(class = "wr", type = "radio", name = "win", id = "w7",
               checked = NA, `aria-label` = "Last 7 days"),
    tags$input(class = "wr", type = "radio", name = "win", id = "w28",
               `aria-label` = "Last 28 days"),
    tags$div(class = "win",
             tags$label(`for` = "w7", "7 days"),
             tags$label(`for` = "w28", "28 days"))) else NULL

  note <- function(txt, cls) if (!is.null(txt))
    tags$p(class = paste("pnote", cls), smarten(txt)) else NULL

  tags$section(
    class = "panel lead",
    tags$h2(heading),
    toggle,
    # When only one window survived, its note carries no toggle class -- there is
    # nothing to hide it against.
    if (both) tagList(note(note_short, "n7"), note(note_long, "n28"))
    else note(if (has(short)) note_short else note_long, ""),
    rows(short, "g7"), rows(long, "g28"),
    if (!is.null(links)) tags$p(class = "more", HTML(links)))
}

# Render a styled index/landing page. `items` is a list of lists with $href,
# $label and optional $meta (a muted right-aligned note). `new_tab` opens the
# row links in a new tab (matches the prior dashboard-index behavior).
# `panel` is an optional extra block (e.g. most_read_panel()) placed after the
# section index -- the sections are the site's navigation and stay on top.
# `active` is a SITE_SECTIONS href to mark in the masthead, or NULL. `crumb` is
# list(label=, section=) for a breadcrumb beneath the masthead. `wordmark_only`
# suppresses the section links (the landing page, which IS the section list).
styled_index_page <- function(out_path, title, heading, items,
                              kicker = NULL, dek = NULL, back = NULL,
                              new_tab = TRUE, search = FALSE, panel = NULL,
                              # A block that belongs ABOVE the search box and the
                              # section list. `panel` sits below both, which was
                              # right while that list was the page's navigation --
                              # the landing page's masthead carries that now, so the
                              # forecast leads and the list is a reference under it.
                              panel_top = NULL,
                              active = NULL, crumb = NULL, wordmark_only = FALSE,
                              search_json = "cases/search.json",
                              search_prefix = "cases/", feeds = FALSE,
                              # Shared-link card. `dek` is the page's own one-line
                              # summary and is exactly what a card wants, so it is
                              # the default rather than a second thing to write.
                              description = NULL, path = NULL) {
  rows <- lapply(items, function(it) {
    a_args <- list(class = "row", href = it$href)
    if (isTRUE(new_tab)) { a_args$target <- "_blank"; a_args$rel <- "noopener" }
    a_args <- c(a_args, list(
      tags$span(class = "d", smarten(it$label)),
      if (!is.null(it$meta) && nzchar(it$meta)) tags$span(class = "count", smarten(it$meta))
    ))
    # Optional strip of the category's most recent pages, listed horizontally in
    # a smaller, muted style beneath the main link. Each is its own <a>, so it
    # sits outside the block-level row link (anchors can't nest).
    recent_strip <- NULL
    if (!is.null(it$recent) && length(it$recent) > 0) {
      rlinks <- lapply(it$recent, function(rc) {
        ra <- list(href = rc$href, rc$label)
        if (isTRUE(new_tab)) { ra$target <- "_blank"; ra$rel <- "noopener" }
        do.call(tags$a, ra)
      })
      recent_strip <- tags$div(
        class = "recent",
        tags$span(class = "rlabel", it$recent_label %||% "Latest"),
        rlinks
      )
    }
    tags$li(do.call(tags$a, a_args), recent_strip)
  })
  heading_node <- if (grepl("<em>", heading, fixed = TRUE))
    tags$h1(HTML(smarten(heading))) else tags$h1(smarten(heading))
  body <- tags$body(
    HTML(site_masthead(active = active, wordmark_only = wordmark_only)),
    tags$main(
    id = "main",
    class = "wrap",
    if (!is.null(crumb)) HTML(site_breadcrumb(crumb$label, crumb$section)),
    if (!is.null(kicker)) tags$p(class = "kicker", smarten(kicker)),
    heading_node,
    tags$hr(class = "brule"),
    if (!is.null(dek)) tags$p(class = "dek", smarten(dek)),
    panel_top,
    if (isTRUE(search)) HTML(SEARCH_HTML),
    tags$ul(class = "idx", rows),
    panel,
    # Below the panels, above the back link: a reader who has scrolled the whole
    # section list is the one who might want telling when it changes.
    if (isTRUE(feeds)) feed_follow_line(),
    if (!is.null(back)) tags$p(class = "back", tags$a(href = back$href, smarten(back$label))),
    if (isTRUE(search)) HTML(search_script(search_json, search_prefix))
  ))
  jsonld <- if (!is.null(crumb))
    site_breadcrumb_jsonld(crumb$label, crumb$section) else NULL
  html <- paste0("<!DOCTYPE html>\n<html lang=\"en\">\n",
                 page_head(title, jsonld, description = description %||% dek,
                           path = path), "\n", as.character(body), "\n</html>\n")
  writeLines(enc2utf8(html), out_path, useBytes = TRUE)
  invisible(out_path)
}

# Return the `n` most recent child pages of `dir` as a list of {href, label},
# newest first, for the compact "recent" strip beneath a landing-page category.
# `pattern` selects the files; `sort_key` maps the filename vector to a sortable
# vector (Date or integer, newest = largest); `label` maps the filename vector
# to display strings. `prefix` is prepended to each href so links resolve from
# the landing page (e.g. "dashboards/"). Empty list if the dir or matches are
# absent, so callers can attach it unconditionally.
recent_children <- function(dir, pattern, sort_key, label, prefix, n = 3L) {
  if (!dir.exists(dir)) return(list())
  files <- list.files(dir, pattern = pattern)
  if (length(files) == 0) return(list())
  files <- utils::head(files[order(sort_key(files), decreasing = TRUE)], n)
  labs <- label(files)
  lapply(seq_along(files), function(i)
    list(href = paste0(prefix, files[i]), label = labs[i]))
}

# gtsave() with a browser <title> and a mobile viewport meta injected into the
# generated <head> (gt emits neither). Keeps the gt table's own styling intact.
gtsave_titled <- function(tbl, filename, path, title) {
  gt::gtsave(tbl, filename, path = path)
  fp <- file.path(path, filename)
  txt <- paste(readLines(fp, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  inject <- paste0(
    '<meta name="viewport" content="width=device-width, initial-scale=1"/>',
    '<link rel="icon" href="/favicon.svg" type="image/svg+xml"><link rel="alternate icon" href="/favicon.ico" sizes="any">',
    "<title>", htmlEscape(title), "</title>")
  if (grepl("</head>", txt, fixed = TRUE)) {
    txt <- sub("</head>", paste0(inject, "</head>"), txt, fixed = TRUE)
  }
  writeLines(enc2utf8(txt), fp, useBytes = TRUE)
  invisible(fp)
}
