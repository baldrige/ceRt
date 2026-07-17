# page_style.R ----------------------------------------------------------------
# Shared presentation layer for the static site: an editorial "law-review
# broadsheet" look (warm parchment, deep ink, single oxblood accent, Fraunces +
# Newsreader serifs) matching the Cert Funnel page, plus small helpers that fix
# two things gtsave() omits from every dashboard page: a <title> and a mobile
# viewport meta (without the latter, phones render at ~980px and links become
# hard to tap). Sourced by scotus_dash_new.R and conference_dash.R.

suppressPackageStartupMessages({
  library(htmltools)
})

# Google Fonts used across the site (kept identical to the Cert Funnel page).
PAGE_FONTS_URL <- paste0(
  "https://fonts.googleapis.com/css2?",
  "family=Fraunces:ital,opsz,wght@0,9..144,500;0,9..144,600;1,9..144,500&",
  "family=Newsreader:ital,opsz,wght@0,6..72,400;0,6..72,500;0,6..72,600;1,6..72,400&",
  "display=swap")

# Base editorial styles for the index / landing pages.
INDEX_CSS <- "
  :root{
    --paper:#f3ecdd;--panel:#f7f1e4;--ink:#23262d;--ink-soft:#5f5847;
    --faint:#8a8271;--oxblood:#8a2b2b;--sienna:#b5651d;--rule:#d8cdb4;
  }
  *{box-sizing:border-box}
  html{-webkit-text-size-adjust:100%}
  body{font-family:'Newsreader',Georgia,serif;font-size:19px;line-height:1.6;
    color:var(--ink);background:var(--paper);margin:0;font-feature-settings:'onum' 1}
  body::before{content:'';position:fixed;inset:0;z-index:-1;pointer-events:none;
    opacity:.5;mix-blend-mode:multiply;
    background-image:url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='140' height='140'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.9' numOctaves='2' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)' opacity='0.045'/%3E%3C/svg%3E\");}
  .wrap{max-width:40rem;margin:0 auto;padding:3rem 1.4rem 4rem}
  .kicker{font:600 .78rem/1 'Newsreader';letter-spacing:.22em;text-transform:uppercase;
    color:var(--oxblood);margin:0 0 1rem}
  h1{font-family:'Fraunces',Georgia,serif;font-weight:600;
    font-size:clamp(2.4rem,7vw,3.6rem);line-height:1;letter-spacing:-.015em;
    margin:0 0 1rem;color:var(--ink)}
  h1 em{font-style:italic;font-weight:500;color:var(--oxblood)}
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
  ul.idx a.row:hover{background:rgba(138,43,43,.05)}
  ul.idx .d{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.16rem;
    color:var(--oxblood);line-height:1.25}
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
  ul.idx .recent a:hover{color:var(--oxblood);border-color:var(--oxblood)}
  .back{margin-top:2rem;font-size:.95rem}
  .back a{color:var(--sienna);text-decoration:none;
    border-bottom:1px solid rgba(181,101,29,.35)}
  .back a:hover{border-color:var(--sienna)}
  /* Home-page case search. */
  .csearch{position:relative;margin:0 0 1.4rem}
  #cq{width:100%;font-family:'Newsreader',Georgia,serif;font-size:1.05rem;color:var(--ink);
    background:var(--panel);border:1px solid var(--rule);border-radius:3px;padding:.7rem .9rem}
  #cq:focus{outline:none;border-color:var(--oxblood);box-shadow:0 0 0 3px rgba(138,43,43,.1)}
  #cq::placeholder{color:var(--faint)}
  #cq.loading{background-image:linear-gradient(90deg,transparent,rgba(138,43,43,.06),transparent);
    background-size:40% 100%;background-repeat:no-repeat;animation:csl 1s infinite}
  @keyframes csl{0%{background-position:-40% 0}100%{background-position:140% 0}}
  .cres{list-style:none;margin:.35rem 0 0;padding:0;max-height:24rem;overflow-y:auto;
    border:1px solid var(--rule);border-radius:3px;background:var(--panel)}
  .cres:empty{display:none}
  .cres li{border-bottom:1px solid var(--rule)}
  .cres li:last-child{border-bottom:0}
  .cres a{display:block;padding:.55rem .7rem;text-decoration:none;color:var(--ink);
    font-size:.98rem;line-height:1.3}
  .cres a:hover{background:rgba(138,43,43,.06)}
  .cres .cd{color:var(--oxblood);font-variant-numeric:tabular-nums;font-weight:600;
    margin-right:.5rem;white-space:nowrap}
  .cnone{padding:.55rem .7rem;color:var(--faint);font-style:italic}
"

# Home-page case search: a lazy-loaded client-side index (docket -> caption).
SEARCH_HTML <- paste0(
  "<div class='csearch'><input type='search' id='cq' autocomplete='off' spellcheck='false' ",
  "placeholder='Search all cases by name or docket number…' aria-label='Search cases'>",
  "<ul id='cres' class='cres' role='listbox'></ul></div>")

SEARCH_SCRIPT <- paste0("<script>(function(){",
  "var q=document.getElementById('cq'),r=document.getElementById('cres'),E=null,t;",
  "function esc(s){return s.replace(/[&<>]/g,function(c){return {'&':'&amp;','<':'&lt;','>':'&gt;'}[c];});}",
  "function load(){if(E)return;q.classList.add('loading');",
  "fetch('cases/search.json').then(function(x){return x.json();}).then(function(j){",
  "E=Object.keys(j).map(function(d){return [d,j[d],d.toLowerCase(),j[d].toLowerCase()];});",
  "q.classList.remove('loading');run();}).catch(function(){q.classList.remove('loading');});}",
  "q.addEventListener('focus',load);",
  "q.addEventListener('input',function(){clearTimeout(t);t=setTimeout(run,110);});",
  "function run(){var s=q.value.trim().toLowerCase();if(!s||!E){r.innerHTML='';return;}",
  "var o=[],n=0;for(var i=0;i<E.length;i++){if(E[i][2].indexOf(s)>-1||E[i][3].indexOf(s)>-1){",
  "o.push(E[i]);if(++n>=40)break;}}",
  "r.innerHTML=o.length?o.map(function(e){return \"<li><a href='cases/\"+e[0]+\".html'>",
  "<span class='cd'>No. \"+e[0]+\"</span>\"+esc(e[1])+\"</a></li>\";}).join(''):",
  "\"<li class='cnone'>No matching cases.</li>\";}})();</script>")

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

# Raw <head> for an index page (built as a string because htmltools drops the
# <head> singleton from as.character()).
page_head <- function(title) {
  paste0(
    "<head>",
    "<script async src='/analytics.js'></script>",
    '<meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    "<title>", htmlEscape(title), "</title>",
    '<link rel="preconnect" href="https://fonts.googleapis.com">',
    '<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>',
    '<link rel="stylesheet" href="', PAGE_FONTS_URL, '">',
    "<style>", INDEX_CSS, "</style>",
    "</head>")
}

# Render a styled index/landing page. `items` is a list of lists with $href,
# $label and optional $meta (a muted right-aligned note). `new_tab` opens the
# row links in a new tab (matches the prior dashboard-index behavior).
styled_index_page <- function(out_path, title, heading, items,
                              kicker = NULL, dek = NULL, back = NULL,
                              new_tab = TRUE, search = FALSE) {
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
  body <- tags$body(tags$main(
    class = "wrap",
    if (!is.null(kicker)) tags$p(class = "kicker", smarten(kicker)),
    heading_node,
    tags$hr(class = "brule"),
    if (!is.null(dek)) tags$p(class = "dek", smarten(dek)),
    if (isTRUE(search)) HTML(SEARCH_HTML),
    tags$ul(class = "idx", rows),
    if (!is.null(back)) tags$p(class = "back", tags$a(href = back$href, smarten(back$label))),
    if (isTRUE(search)) HTML(SEARCH_SCRIPT)
  ))
  html <- paste0("<!DOCTYPE html>\n<html lang=\"en\">\n",
                 page_head(title), "\n", as.character(body), "\n</html>\n")
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
    "<title>", htmlEscape(title), "</title>")
  if (grepl("</head>", txt, fixed = TRUE)) {
    txt <- sub("</head>", paste0(inject, "</head>"), txt, fixed = TRUE)
  }
  writeLines(enc2utf8(txt), fp, useBytes = TRUE)
  invisible(fp)
}
