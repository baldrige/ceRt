# site_meta.R -------------------------------------------------------------------
# The social / search metadata block: description, canonical, Open Graph, Twitter
# card. One implementation, because this site builds its <head> in FIVE places --
# page_head() in page_style.R, scr_write_page() in interactive_theme.R,
# docket_page() in docket_page.R, the funnel in cert_funnel.R and the methods
# note in docs/ -- and page_style.R already records what happens to a sitewide
# head change under those conditions: each hand-built head "has silently missed a
# sitewide head change at least once".
#
# Sourced by name from each of the five. Not folded into palette.R, which is the
# only file all five already reach: palette.R is THE colour source and metadata
# is not a colour.

SITE_BASE <- "https://supremecourt.report"

# One card image for the whole site, re-asserted into the site root by
# build_dashboards.R. Regenerate with docs/make_og_image.R.
OG_IMAGE   <- "/og.png"
OG_IMAGE_W <- "1200"
OG_IMAGE_H <- "630"
OG_IMAGE_ALT <- "Supreme Court Report - docket analytics for the Supreme Court of the United States"

# Attribute-safe escape. Deliberately not htmltools::htmlEscape: this file is
# sourced by builders that do not all load htmltools, and a metadata block is not
# worth a dependency. Escapes the quote too, which htmlEscape() only does with
# attribute = TRUE -- and an unescaped quote in a case caption would end the
# content attribute early and spill the rest of the caption into the markup.
.meta_esc <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

# Trim to a length the cards actually show, on a word boundary. Facebook and
# Bluesky cut around 200 characters and Google around 155; going long is not
# penalised, it is just invisible, so this keeps the useful part first.
# Strip markup and decode the entities the page chrome uses, so what reaches a
# card is text rather than source.
#
# Both the title AND the description need this, which the first version got half
# right: only the description was decoded, so a page whose title carries an
# entity -- "OT2025 &mdash; Oral Arguments" -- had its ampersand escaped by
# .meta_esc() into "&amp;mdash;" and shipped a card reading the entity out loud.
# Nine argument pages went out that way.
#
# &amp; is decoded LAST on purpose. Decoding it first would turn "&amp;mdash;"
# into "&mdash;" and then into an em dash, silently unescaping one level too
# many; leaving it until the named entities are gone means a genuine "AT&amp;T"
# resolves to "AT&T" and nothing else moves.
.meta_decode <- function(x) {
  x <- gsub("<[^>]*>", "", as.character(x %||% ""))
  for (e in list(c("&mdash;", "\u2014"), c("&ndash;", "\u2013"), c("&rsquo;", "\u2019"),
                 c("&lsquo;", "\u2018"), c("&ldquo;", "\u201c"), c("&rdquo;", "\u201d"),
                 c("&middot;", "\u00b7"), c("&hellip;", "\u2026"),
                 c("&nbsp;", " "), c("&amp;", "&"), c("&lt;", "<"), c("&gt;", ">")))
    x <- gsub(e[1], e[2], x, fixed = TRUE)
  x
}

.meta_clip <- function(x, n = 200L) {
  x <- .meta_decode(x)
  x <- gsub("\\s+", " ", trimws(x))
  if (!nzchar(x) || nchar(x) <= n) return(x)
  cut <- substr(x, 1L, n)
  sp <- regexpr("\\s[^\\s]*$", cut, perl = TRUE)
  if (sp > 0) cut <- substr(cut, 1L, sp - 1L)
  paste0(sub("[ ,;:.—-]+$", "", cut), "…")
}

#' Social + search metadata for one page.
#'
#' @param title       the page title, as it should read in a shared card
#' @param description one or two sentences; clipped to 200 characters
#' @param path        site-absolute path ("/cases/26-205.html"); "/" for the
#'                    landing page. Omitted -> no canonical, which is right for a
#'                    page whose URL this builder cannot know.
#' @param type        Open Graph type: "website" for indexes, "article" for a
#'                    page about one thing (a case, a conference, the methods note)
social_meta <- function(title, description = NULL, path = NULL,
                        type = c("website", "article")) {
  type <- match.arg(type)
  d <- .meta_clip(description)
  url <- if (!is.null(path) && nzchar(path))
    paste0(SITE_BASE, if (startsWith(path, "/")) path else paste0("/", path))
  paste0(
    if (nzchar(d)) paste0('<meta name="description" content="', .meta_esc(d), '">'),
    if (!is.null(url)) paste0('<link rel="canonical" href="', .meta_esc(url), '">'),
    '<meta property="og:site_name" content="Supreme Court Report">',
    '<meta property="og:type" content="', type, '">',
    '<meta property="og:title" content="', .meta_esc(.meta_decode(title)), '">',
    if (nzchar(d)) paste0('<meta property="og:description" content="', .meta_esc(d), '">'),
    if (!is.null(url)) paste0('<meta property="og:url" content="', .meta_esc(url), '">'),
    '<meta property="og:image" content="', SITE_BASE, OG_IMAGE, '">',
    '<meta property="og:image:width" content="', OG_IMAGE_W, '">',
    '<meta property="og:image:height" content="', OG_IMAGE_H, '">',
    '<meta property="og:image:alt" content="', .meta_esc(OG_IMAGE_ALT), '">',
    # summary_large_image, not summary: the card is a 1200x630 wordmark and the
    # small variant crops it to a square that cuts the wordmark in half.
    '<meta name="twitter:card" content="summary_large_image">',
    '<meta name="twitter:title" content="', .meta_esc(.meta_decode(title)), '">',
    if (nzchar(d)) paste0('<meta name="twitter:description" content="', .meta_esc(d), '">'),
    '<meta name="twitter:image" content="', SITE_BASE, OG_IMAGE, '">')
}
