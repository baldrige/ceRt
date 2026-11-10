# opinion_page.R ----------------------------------------------------------------
# The Court's opinions as pages of this site: /opinions/<yyyy>/<docket>.html,
# with a Markdown file and an EPUB beside each. docs/opinions.md.
#
# The Court publishes an opinion as a PDF. Two Python scripts turn that PDF
# into text -- .github/scripts/opinions/extract_opinion.py (the PDF's text
# layer, geometry and fonts -> sections, paragraphs, notes, page breaks, as
# JSON) and render_opinion.py (that JSON -> Markdown, EPUB, and the reader's
# HTML body). This file is the rest: which decisions to build, the page around
# the body, and the index.
#
# Requires palette.R, site_nav.R, page_style.R (page_head, smarten_html).
#
# WHAT IS BUILT. The merits decisions the Justices section already tracks
# (justices/lineups.json: docket -> the opinion's PDF), where that PDF is one
# case's file -- a slip opinion, or the excerpt that replaces it. Decisions
# that the Court's site now serves only as a page range of a whole volume
# (OT2016-OT2020, mostly) are not built yet; the extractor reads their layout,
# but cutting one case out of a 1,200-page PDF is a separate job.
#
# CACHE. opinions/index.json: docket -> {href, url, xv, ov, ...}. An opinion
# is rebuilt when its PDF's URL changes (the Court replaces a slip with the
# excerpt, and a "_new_" revision), or when the extractor's version does. The
# extracted JSON is published beside the page, so a change to the PAGE alone
# (OPINION_PAGE_VERSION, or render_opinion.py) re-renders without a download.

suppressPackageStartupMessages({ library(stringr); library(jsonlite) })

OPINION_PAGE_VERSION <- "p1"
OPINION_DIR <- "opinions"

opinion_href <- function(term, dkt, ext = "html") sprintf("/%s/%s/%s.%s", OPINION_DIR, term, dkt, ext)
opinion_index_path <- function(site_dir) file.path(site_dir, OPINION_DIR, "index.json")

read_opinion_index <- function(site_dir) {
  p <- opinion_index_path(site_dir)
  if (!file.exists(p)) return(list())
  tryCatch(fromJSON(p, simplifyVector = FALSE), error = function(e) list())
}

write_opinion_index <- function(site_dir, idx) {
  p <- opinion_index_path(site_dir)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  idx <- idx[order(names(idx))]
  writeLines(if (length(idx)) toJSON(idx, auto_unbox = TRUE, null = "null") else "{}", p, useBytes = TRUE)
}

# October Term of a decision date: the Term that began the October before.
opinion_term <- function(decided) {
  d <- as.Date(decided)
  as.integer(format(d, "%Y")) - as.integer(as.integer(format(d, "%m")) < 10L)
}

# The decisions that can be built: one row per PDF. Companion cases share a
# PDF and so a page; `dkts` carries them all and the page lives at the first.
opinion_worklist <- function(site_dir) {
  lu <- tryCatch(fromJSON(file.path(site_dir, "justices", "lineups.json"), simplifyVector = FALSE),
                 error = function(e) list())
  if (!length(lu)) return(tibble::tibble())
  cap <- tryCatch(fromJSON(file.path(site_dir, "cases", "search.json"), simplifyVector = FALSE),
                  error = function(e) list())
  rows <- lapply(names(lu), function(d) {
    e <- lu[[d]]
    if (is.null(e$url) || is.null(e$decided)) return(NULL)
    tibble::tibble(dkt = d, url = e$url, decided = e$decided)
  })
  df <- dplyr::bind_rows(rows)
  if (!nrow(df)) return(df)
  # One case's own file: not a page range inside a volume.
  df <- df[!grepl("#page=|preliminaryprint|boundvolumes", df$url), , drop = FALSE]
  df <- df[order(df$decided, df$dkt), , drop = FALSE]
  out <- lapply(split(df, df$url), function(g) {
    lead <- g$dkt[1]
    tibble::tibble(dkt = lead, dkts = list(g$dkt), url = g$url[1], decided = g$decided[1],
                   term = opinion_term(g$decided[1]),
                   caption = as.character(cap[[lead]] %||% paste("No.", lead)))
  })
  df <- dplyr::bind_rows(out)
  df[order(df$decided, df$dkt, decreasing = TRUE), , drop = FALSE]
}

# The caption the opinion's own running head carries ("WOLFORD v. LOPEZ"), in
# the case of the docket's caption where a word of it appears there ("Wolford",
# "FCC"), and title case otherwise. NULL when the head gave none.
opinion_short_caption <- function(short, long) {
  if (is.null(short) || !nzchar(short)) return(NULL)
  w <- strsplit(str_squish(short), " ")[[1]]
  lw <- str_extract_all(long %||% "", "[A-Za-z0-9.'’&-]+")[[1]]
  key <- function(x) tolower(gsub("[.,]", "", x))
  out <- vapply(w, function(x) {
    if (tolower(x) == "v.") return("v.")
    hit <- lw[key(lw) == key(x)]
    core <- if (length(hit)) sub(",$", "", hit[1]) else str_to_title(x)
    paste0(core, if (grepl(",$", x) && !grepl(",$", core)) "," else "")
  }, character(1))
  s <- paste(out, collapse = " ")
  sub("^In Re ", "In re ", sub("^Ex Parte ", "Ex parte ", s))
}

.op_esc <- function(x) htmltools::htmlEscape(x %||% "")
.op_long_date <- function(d) str_squish(format(as.Date(d), "%B %e, %Y"))

# ---- the page -----------------------------------------------------------------
# Set for reading: one column of 38rem, the site's serif at its body size, notes
# at the foot of each writing. The original's page numbers stay in the text as
# small markers, because an opinion is cited by them.
OPINION_CSS <- fill_palette("
  .wrap.opn{max-width:42rem}
  .opn .ometa{font-size:.92rem;color:var(--ink-soft);margin:.3rem 0 0}
  .opn .oget{display:flex;flex-wrap:wrap;gap:.4rem .6rem;margin:1.1rem 0 0;padding:0;list-style:none;font-size:.9rem}
  .opn .oget a{display:inline-block;padding:.32rem .7rem;border:1px solid var(--rule);border-radius:4px;color:var(--ink);text-decoration:none;background:var(--panel)}
  .opn .oget a:hover{border-color:var(--accent);color:var(--accent)}
  .opn .osrc{font-size:.84rem;color:var(--faint);margin:.9rem 0 0;line-height:1.45}
  .opn .osrc a{color:var(--link)}
  .opn .otoc{margin:1.6rem 0 0;padding:.8rem 1rem;border:1px solid var(--rule);border-radius:4px;background:var(--panel);font-size:.95rem}
  .opn .otoc ol{margin:0;padding:0;list-style:none}
  .opn .otoc li{margin:.25rem 0}
  .opn .otoc a{color:var(--ink);text-decoration:none;border-bottom:1px solid rgba(@link:rgb@,.35)}
  .opn .otoc a:hover{color:var(--accent)}
  .opn section.op{margin-top:3rem;border-top:2px solid var(--ink);padding-top:1.1rem}
  .opn section.op h2{font-family:'Fraunces',Georgia,serif;font-weight:500;font-size:1.5rem;line-height:1.2;margin:0 0 1rem;color:var(--ink)}
  .opn section.op p{margin:0 0 .95rem;line-height:1.62;hyphens:auto;overflow-wrap:break-word}
  .opn section.op p.byline{color:var(--ink-soft);font-style:italic}
  .opn section.op p.sep{text-align:center;letter-spacing:.3em;color:var(--faint);margin:1.6rem 0}
  .opn section.op h3.oh{font-family:'Fraunces',Georgia,serif;font-weight:500;font-size:1.12rem;text-align:center;margin:2rem 0 1rem;color:var(--ink)}
  .opn section.op blockquote{margin:0 0 .95rem;padding:0 0 0 1.1rem;border-left:2px solid var(--rule);color:var(--ink-soft)}
  .opn section.op blockquote p{margin-bottom:.6rem;font-size:.96em}
  .opn .sc{font-variant-caps:small-caps;letter-spacing:.02em}
  .opn .pg{font-size:.66em;color:var(--faint);vertical-align:.3em;margin:0 .3em 0 .1em;font-variant-numeric:tabular-nums;user-select:none;white-space:nowrap}
  .opn .pg::before{content:'*'}
  .opn sup.fnr{font-size:.68em;line-height:0;margin-left:.05em}
  .opn sup.fnr a{color:var(--link);text-decoration:none;padding:0 .1em}
  .opn aside.notes{margin-top:2rem;border-top:1px solid var(--rule);padding-top:.8rem;font-size:.88rem;color:var(--ink-soft)}
  .opn aside.notes h3{font-family:'Fraunces',Georgia,serif;font-weight:500;font-size:1rem;margin:0 0 .6rem;color:var(--ink)}
  .opn aside.notes ol{margin:0;padding-left:1.7rem}
  .opn aside.notes li{margin:0 0 .55rem}
  .opn aside.notes li.sym{list-style:none;margin-left:-1.7rem}
  .opn aside.notes li.sym .mk{margin-right:.3rem}
  .opn aside.notes li.sym p{display:inline}
  .opn aside.notes p{margin:0 0 .4rem;line-height:1.5}
  .opn aside.notes a.back{color:var(--link);text-decoration:none;margin-left:.2rem}
  .opn aside.notes li:target,.opn sup.fnr a:target,.opn .pg:target{background:rgba(@accent:rgb@,.12);outline:.2rem solid rgba(@accent:rgb@,.12)}
  .opn section.op,.opn aside.notes li,.opn sup.fnr a{scroll-margin-top:1rem}
  .opn section.op-syllabus p,.opn section.op-counsel p{font-size:.95em}
  .opn section.op-counsel summary{cursor:pointer;font-family:'Fraunces',Georgia,serif;font-size:1.05rem;color:var(--ink-soft)}
  .opn .osyl{font-size:.84rem;color:var(--faint);font-style:italic;margin:-.5rem 0 1rem}
  .opn p.back{margin-top:3rem;font-size:.92rem}
  @media print{.smast,.bcrumb,.opn .oget,.opn .otoc,.opn p.back{display:none}.opn section.op{break-before:page}}
")

# One opinion's page from the body render_opinion.py wrote. `o` is a worklist
# row; `doc` the extracted JSON (for the citation and the count of writings).
render_opinion_page <- function(site_dir, o, body, doc) {
  term <- o$term; dkt <- o$dkt
  dkts <- unlist(o$dkts)
  cite <- doc$cite %||% NA_character_
  nos <- paste0(if (length(dkts) > 1) "Nos. " else "No. ", paste(dkts, collapse = ", "))
  when <- .op_long_date(o$decided)
  kicker <- paste(c(nos, paste("decided", when), if (!is.na(cite)) cite), collapse = " · ")
  writings <- vapply(doc$sections, function(s) s$kind, character(1))
  n_sep <- sum(!writings %in% c("syllabus", "counsel", "court", "percuriam", "plurality"))
  dek <- paste0("The opinion in ", o$caption, " (", nos, "), decided ", when,
                ": the full text with its footnotes, as a page, a Markdown file and an EPUB.")
  crumb <- list(href = paste0("/", OPINION_DIR, "/"), label = "Opinions")
  # A note under the syllabus heading: it is the Reporter's, not the Court's.
  body <- sub("(<section id=\"s1\" class=\"op op-syllabus\"><h2>Syllabus</h2>)",
              "\\1<p class='osyl'>Prepared by the Reporter of Decisions for the convenience of the reader; it is no part of the opinion of the Court.</p>",
              body, perl = TRUE)
  html <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n",
    page_head(paste0(o$caption, " — Opinion — Supreme Court Report"), site_breadcrumb_jsonld(o$caption, crumb),
              extra_css = OPINION_CSS, description = dek, path = opinion_href(term, dkt), og_type = "article",
              extra_head = paste0("<meta name='otv' content='", OPINION_PAGE_VERSION, "'>",
                                  "<link rel='alternate' type='text/markdown' href='", opinion_href(term, dkt, "md"), "'>",
                                  "<link rel='alternate' type='application/epub+zip' href='", opinion_href(term, dkt, "epub"), "'>")),
    "<body>", site_masthead(active = "/justices/"),
    "<main class='wrap opn' id='main' data-dkt='", dkt, "'>",
    site_breadcrumb(o$caption, crumb),
    "<p class='kicker'>", .op_esc(kicker), "</p>",
    "<h1 style='font-size:clamp(1.7rem,4.6vw,2.4rem);line-height:1.12'>", .op_esc(o$caption), "</h1>",
    "<p class='ometa'>", if (!identical(o$caption, o$long) && nzchar(o$long %||% "")) paste0(.op_esc(o$long), ". ") else "",
    if (n_sep == 0) "No separate writings." else
      sprintf("%d separate writing%s.", n_sep, if (n_sep == 1) "" else "s"), "</p>",
    "<ul class='oget'>",
    "<li><a href='", opinion_href(term, dkt, "epub"), "' download>EPUB</a></li>",
    "<li><a href='", opinion_href(term, dkt, "md"), "' download>Markdown</a></li>",
    "<li><a href='", .op_esc(o$url), "' rel='noopener'>The Court’s PDF</a></li>",
    "<li><a href='/cases/", dkt, ".html'>The docket</a></li></ul>",
    "<p class='osrc'>Set from the PDF the Court published, which is the authoritative text. ",
    "The small starred numbers are that PDF’s page numbers. A conversion can introduce errors; ",
    "check a quotation against the <a href='", .op_esc(o$url), "' rel='noopener'>original</a>.</p>",
    body,
    "<p class='back'><a href='/", OPINION_DIR, "/'>&larr; All opinions</a> · <a href='/justices/ot", term,
    ".html'>The Justices, October Term ", term, " &rarr;</a></p>",
    "</main></body>\n</html>\n")
  out <- file.path(site_dir, OPINION_DIR, term, paste0(dkt, ".html"))
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  # Not smarten_html(): the Court's own quotation marks are in the text already,
  # and an opinion quotes code and statutes whose straight marks are meant.
  writeLines(enc2utf8(html), out, useBytes = TRUE)
  invisible(out)
}

# ---- the index page -------------------------------------------------------------
write_opinion_index_page <- function(site_dir, idx) {
  ent <- idx[vapply(idx, function(e) isTRUE(e$lead), logical(1))]
  if (!length(ent)) return(invisible(NULL))
  df <- tibble::tibble(dkt = names(ent),
                       caption = vapply(ent, function(e) e$caption %||% "", character(1)),
                       decided = vapply(ent, function(e) e$decided %||% "", character(1)),
                       term = vapply(ent, function(e) as.integer(e$term), integer(1)),
                       href = vapply(ent, function(e) e$href, character(1)))
  df <- df[order(df$decided, decreasing = TRUE), , drop = FALSE]
  terms <- sort(unique(df$term), decreasing = TRUE)
  blocks <- vapply(terms, function(t) {
    g <- df[df$term == t, , drop = FALSE]
    paste0("<section class='oterm' id='ot", t, "'><h2>October Term ", t, " <span>", nrow(g), " opinion", if (nrow(g) == 1) "" else "s",
           "</span></h2><ul>",
           paste(sprintf("<li><a href='%s'>%s</a><span class='od'>No. %s · %s</span></li>", g$href, .op_esc(g$caption), g$dkt,
                         vapply(g$decided, .op_long_date, character(1))), collapse = ""),
           "</ul></section>")
  }, character(1))
  css <- fill_palette("
    .oix .oterm{margin-top:2.2rem}
    .oix .oterm h2{font-family:'Fraunces',Georgia,serif;font-weight:500;font-size:1.3rem;border-bottom:1px solid var(--rule);padding-bottom:.3rem;margin:0 0 .6rem}
    .oix .oterm h2 span{font-family:'Newsreader',Georgia,serif;font-size:.82rem;color:var(--faint);font-style:italic;margin-left:.5rem}
    .oix .oterm ul{list-style:none;margin:0;padding:0}
    .oix .oterm li{padding:.42rem 0;border-bottom:1px solid var(--rule);line-height:1.3}
    .oix .oterm li a{color:var(--ink);text-decoration:none;font-weight:600}
    .oix .oterm li a:hover{color:var(--accent)}
    .oix .od{display:block;font-size:.84rem;color:var(--faint);font-variant-numeric:tabular-nums}
    .oix .dek{color:var(--ink-soft)}
  ")
  dek <- "The Court’s merits opinions as pages you can read, with a Markdown file and an EPUB of each."
  html <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n",
    page_head("Opinions — Supreme Court Report", site_breadcrumb_jsonld("Opinions"), extra_css = css,
              description = dek, path = paste0("/", OPINION_DIR, "/")),
    "<body>", site_masthead(active = "/justices/"),
    "<main class='wrap oix' id='main'>", site_breadcrumb("Opinions"),
    "<p class='kicker'>", nrow(df), " opinions · October Terms ", min(terms), "–", max(terms), "</p>",
    "<h1>Opinions</h1>",
    "<p class='dek'>", dek, " Each is set from the PDF the Court published, which remains the authoritative text.</p>",
    paste(blocks, collapse = ""),
    "</main></body>\n</html>\n")
  writeLines(enc2utf8(smarten_html(html)), file.path(site_dir, OPINION_DIR, "index.html"), useBytes = TRUE)
  invisible(nrow(df))
}
