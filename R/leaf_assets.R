# leaf_assets.R ---------------------------------------------------------------
# The shared assets behind every INTERACTIVE leaf page (conference reports,
# daily dashboards, argument navigators, the relist tracker): the React /
# reactable / htmlwidgets libraries that gtsave() writes beside each widget,
# published ONCE at the site root, and the post-pass that migrates an
# already-published page onto them.
#
# Before this, scr_write_page() base64-inlined those libraries into every page.
# Measured on a mid-sized conference report: 575 KB of identical library bytes
# and 31 KB of identical stylesheet in a 1.3 MB page, so the smallest report
# (673 KB) was ~90% chrome. Across 328 leaves that is ~190 MB of the same six
# files, re-committed to gh-pages on every rebuild, and a reader clicking
# through three reports downloaded React three times.
#
# The inlining existed for a real reason -- gtsave() references `lib/...`
# RELATIVELY, and the leaves live in four directories, so the relative refs
# 404ed and the table rendered blank. Site-ABSOLUTE refs (`/lib/...`) solve that
# the same way cases/style.css already does for the docket pages, and the site
# is served at the domain root (CNAME), so `/lib/` resolves everywhere.
#
# Layout under the gh-pages root:
#
#   /lib/<pkg>-<version>/<file>   exactly gtsave()'s own layout, copied verbatim,
#                                 so a package upgrade lands in a NEW directory
#                                 and every page already published keeps
#                                 resolving the version it was rendered against.
#                                 The files never change under a given name,
#                                 which is what makes Pages' 10-minute cache
#                                 safe to trust.
#   /leaf.css                     the editorial theme + nav CSS (written by
#                                 interactive_theme.R, which owns SCR_CSS); the
#                                 href carries ?v=<content hash> to bust that
#                                 same cache on a recolour.
#
# Base R + jsonlite only, deliberately: patch_leaf_chrome.R sources this without
# the model stack, and the audit must not be able to fail for a reason unrelated
# to the site.

LEAF_LIB_DIR <- "lib"
LEAF_CSS     <- "leaf.css"

# Where the site root is for a page being written to `out_path`: SITE_DIR when
# out_path lies under it, else NULL. NULL means "a local render to a scratch
# directory", and the writer falls back to inlining so the page still works
# on its own. Mirrors .site_rel_path() in interactive_theme.R, which derives the
# canonical URL the same way and for the same reason -- asking every caller to
# also pass the site root is how the two would come to disagree.
leaf_site_root <- function(out_path) {
  sd <- Sys.getenv("SITE_DIR", unset = "site")
  op <- gsub("\\\\", "/", out_path)
  sd <- sub("/+$", "", gsub("\\\\", "/", sd))
  if (!nzchar(sd) || !startsWith(op, paste0(sd, "/"))) return(NULL)
  sd
}

# The `lib/...` references in a gtsave() head, as they appear in the HTML:
# script src and stylesheet href. Returned as a data frame of the full tag, the
# relative path, and which attribute carried it.
.leaf_lib_refs <- function(html) {
  sm <- regmatches(html, gregexpr("<script[^>]*\\ssrc=\"lib/[^\"]+\"[^>]*>\\s*</script>", html))[[1]]
  lm <- regmatches(html, gregexpr("<link[^>]*\\shref=\"lib/[^\"]+\\.css\"[^>]*/?>", html))[[1]]
  rbind(
    data.frame(tag = sm, path = sub(".*\\ssrc=\"(lib/[^\"]+)\".*", "\\1", sm),
               kind = rep("script", length(sm)), stringsAsFactors = FALSE),
    data.frame(tag = lm, path = sub(".*\\shref=\"(lib/[^\"]+)\".*", "\\1", lm),
               kind = rep("style", length(lm)), stringsAsFactors = FALSE))
}

# Rewrite a freshly gtsave()d widget's `lib/...` references to `/lib/...` and
# publish the referenced files under `site_root`. Only the files the page
# actually references are copied (plus any LICENSE beside them -- React's MIT
# terms ask for it, and the inlined form never carried one at all), not the
# .map / .yaml / package.json clutter gtsave() writes alongside.
#
# Copies unconditionally. The files are identical byte-for-byte to what is
# already there whenever the package versions match, so git sees no change;
# when they do not match, the directory name differs too and this is the first
# publish of that version.
scr_link_libs <- function(html, base_dir, site_root) {
  refs <- .leaf_lib_refs(html)
  for (k in seq_len(nrow(refs))) {
    src <- file.path(base_dir, refs$path[k])
    if (!file.exists(src)) next
    dst <- file.path(site_root, refs$path[k])
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    file.copy(src, dst, overwrite = TRUE)
    for (lic in list.files(dirname(src), pattern = "^(LICENSE|AUTHORS)", full.names = TRUE))
      file.copy(lic, file.path(dirname(dst), basename(lic)), overwrite = TRUE)
    html <- sub(refs$tag[k],
                sub(paste0("\"", refs$path[k], "\""), paste0("\"/", refs$path[k], "\""),
                    refs$tag[k], fixed = TRUE),
                html, fixed = TRUE)
  }
  html
}

# Every published shared library file, with its bytes, keyed by site-absolute
# path. This is what the migration matches an inlined script against.
leaf_lib_files <- function(site_root) {
  d <- file.path(site_root, LEAF_LIB_DIR)
  if (!dir.exists(d)) return(list())
  rel <- list.files(d, pattern = "\\.(js|css)$", recursive = TRUE)
  out <- lapply(file.path(d, rel), function(f) readBin(f, "raw", file.info(f)$size))
  names(out) <- paste0("/", LEAF_LIB_DIR, "/", rel)
  out
}

# Migrate an already-published leaf page onto the shared libraries, in place.
#
# A POST-PASS, for the reason patch_feed_links() is: render_conferences.R
# renders only conferences on/after MIN_CONF_DATE (the current term), so 300-odd
# of the 328 leaves would otherwise carry their 575 KB of inlined React forever.
#
# Content-matched, never guessed. Each base64 script is decoded and replaced
# ONLY if its bytes equal a file already published under /lib/ -- so a page
# rendered against a library version the site has not published keeps its
# inline copy and keeps working. The same for the inlined reactable stylesheet,
# matched against the published .css (through the same readLines/paste
# transform scr_inline_libs() applied when it inlined it). The page's own
# theme <style> is left alone: it is 31 KB, not 575, and rewriting it would
# mean deciding that today's CSS fits markup from an older template.
#
# Returns "patched", "unchanged" (nothing inlined, or nothing matched), or
# "skipped" (no file / no published libs). Attribute "saved" carries the bytes
# removed, for the run summary.
patch_shared_libs <- function(path, site_root, libs = leaf_lib_files(site_root)) {
  if (!file.exists(path) || !length(libs)) return("skipped")
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  n0 <- nchar(txt, type = "bytes")
  # Positional, like every other pass over these pages: a (?s).*? sub across a
  # 1 MB string is how scr_write_page() once tripped PCRE's match limit and
  # silently did nothing.
  open_tag <- "<script src=\"data:application/javascript;base64,"
  close_tag <- "\"></script>"
  css_open <- "<style>"
  css_close <- "</style>"
  # Read the published .css through the SAME readLines/paste that inlined it.
  # Not from the bytes via textConnection: that keeps a trailing empty line
  # readLines() on a file drops, and the one-character difference misses.
  css_names <- names(libs)[grepl("\\.css$", names(libs))]
  css_text <- vapply(css_names, function(nm)
    paste(readLines(file.path(site_root, sub("^/", "", nm)), warn = FALSE), collapse = "\n"),
    character(1))
  out <- character(); pos <- 1L; n <- nchar(txt)
  repeat {
    rest <- substr(txt, pos, n)
    a <- regexpr(open_tag, rest, fixed = TRUE)
    if (a < 0) { out <- c(out, rest); break }
    b <- regexpr(close_tag, substr(rest, a, nchar(rest)), fixed = TRUE)
    if (b < 0) { out <- c(out, rest); break }
    tag_end <- a + b + nchar(close_tag) - 2L
    b64 <- substr(rest, a + nchar(open_tag), a + b - 2L)
    bytes <- tryCatch(jsonlite::base64_dec(gsub("\\s", "", b64)), error = function(e) NULL)
    hit <- NULL
    if (!is.null(bytes)) for (nm in names(libs))
      if (length(libs[[nm]]) == length(bytes) && identical(libs[[nm]], bytes)) { hit <- nm; break }
    repl <- if (is.null(hit)) substr(rest, a, tag_end)
            else sprintf("<script src=\"%s\"></script>", hit)
    out <- c(out, substr(rest, 1L, a - 1L), repl)
    pos <- pos + tag_end
  }
  txt2 <- paste(out, collapse = "")
  # The inlined reactable.css: the widget head's <style> whose text equals a
  # published stylesheet. Only the head is searched, and only exact matches.
  h <- regexpr("</head>", txt2, fixed = TRUE)
  if (h > 0 && length(css_text)) {
    head <- substr(txt2, 1L, h - 1L)
    st <- gregexpr(css_open, head, fixed = TRUE)[[1]]
    for (s in rev(st[st > 0])) {
      e <- regexpr(css_close, substr(head, s, nchar(head)), fixed = TRUE)
      if (e < 0) next
      body <- substr(head, s + nchar(css_open), s + e - 2L)
      m <- match(body, css_text)
      if (!is.na(m)) {
        head <- paste0(substr(head, 1L, s - 1L),
                       sprintf("<link href=\"%s\" rel=\"stylesheet\" />", names(css_text)[m]),
                       substr(head, s + e + nchar(css_close) - 1L, nchar(head)))
      }
    }
    txt2 <- paste0(head, substr(txt2, h, nchar(txt2)))
  }
  if (identical(txt2, txt)) return(structure("unchanged", saved = 0L))
  writeLines(enc2utf8(txt2), path, useBytes = TRUE)
  structure("patched", saved = n0 - nchar(txt2, type = "bytes"))
}
