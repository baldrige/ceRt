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
.meta_clip <- function(x, n = 200L) {
  x <- as.character(x %||% "")
  # Strip markup and decode the few entities the deks use. Page deks are HTML --
  # "docketed <strong>December 03, 2024</strong> &mdash; sortable" -- and a card
  # renders a description literally, tags and all. This is the difference between
  # a shared link that reads like a sentence and one that reads like source.
  x <- gsub("<[^>]*>", "", x)
  for (e in list(c("&mdash;", "\u2014"), c("&ndash;", "\u2013"), c("&rsquo;", "\u2019"),
                 c("&lsquo;", "\u2018"), c("&ldquo;", "\u201c"), c("&rdquo;", "\u201d"),
                 c("&nbsp;", " "), c("&amp;", "&"), c("&lt;", "<"), c("&gt;", ">")))
    x <- gsub(e[1], e[2], x, fixed = TRUE)
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
    '<meta property="og:title" content="', .meta_esc(title), '">',
    if (nzchar(d)) paste0('<meta property="og:description" content="', .meta_esc(d), '">'),
    if (!is.null(url)) paste0('<meta property="og:url" content="', .meta_esc(url), '">'),
    '<meta property="og:image" content="', SITE_BASE, OG_IMAGE, '">',
    '<meta property="og:image:width" content="', OG_IMAGE_W, '">',
    '<meta property="og:image:height" content="', OG_IMAGE_H, '">',
    '<meta property="og:image:alt" content="', .meta_esc(OG_IMAGE_ALT), '">',
    # summary_large_image, not summary: the card is a 1200x630 wordmark and the
    # small variant crops it to a square that cuts the wordmark in half.
    '<meta name="twitter:card" content="summary_large_image">',
    '<meta name="twitter:title" content="', .meta_esc(title), '">',
    if (nzchar(d)) paste0('<meta name="twitter:description" content="', .meta_esc(d), '">'),
    '<meta name="twitter:image" content="', SITE_BASE, OG_IMAGE, '">')
}
