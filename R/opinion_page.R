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
# (justices/lineups.json: docket -> the opinion's PDF). That PDF is either one
# case's file -- a slip opinion, or the excerpt that replaces it -- or a page
# range of a whole volume (OT2016-OT2020, mostly), which the build downloads
# once and reads every wanted case out of.
#
# CACHE. opinions/index.json: docket -> {href, url, xv, ov, ...}. An opinion
# is rebuilt when its PDF's URL changes (the Court replaces a slip with the
# excerpt, and a "_new_" revision), or when the extractor's version does. The
# extracted JSON is published beside the page, so a change to the PAGE alone
# (OPINION_PAGE_VERSION, or render_opinion.py) re-renders without a download.

suppressPackageStartupMessages({ library(stringr); library(jsonlite) })

# p2: companion dockets (a consolidated case's page is its lead's), and the
#     opinions relating to orders.
# p3: captions cased properly where the docket's caption cannot say how.
OPINION_PAGE_VERSION <- "p3"
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
    # `also`: the dockets the opinion's own first pages name -- consolidated
    # cases, which the listing files under the lead alone. The parser that
    # fills it also catches page ranges ("68-69"), so only numbers that are
    # cases on this site count.
    also <- intersect(setdiff(as.character(unlist(e$also)), d), names(cap))
    tibble::tibble(dkt = d, url = e$url, decided = e$decided, also = list(also[grepl("^[0-9]{2}-[0-9]{1,5}$", also)]))
  })
  df <- dplyr::bind_rows(rows)
  if (!nrow(df)) return(df)
  # A case is either its own file or a page range of a whole volume; a row
  # says which (`volume` is the volume's URL, `anchor` the listing's page hint).
  df <- df[order(df$decided, df$dkt), , drop = FALSE]
  out <- lapply(split(df, df$url), function(g) {
    lead <- g$dkt[1]
    in_vol <- grepl("#page=|preliminaryprint|boundvolumes", g$url[1])
    tibble::tibble(dkt = lead, dkts = list(unique(c(g$dkt, unlist(g$also)))), url = g$url[1], decided = g$decided[1],
                   volume = if (in_vol) sub("#.*$", "", g$url[1]) else NA_character_,
                   anchor = suppressWarnings(as.integer(str_match(g$url[1], "#page=(\\d+)")[1, 2])),
                   term = opinion_term(g$decided[1]),
                   caption = as.character(cap[[lead]] %||% paste("No.", lead)), class = "merits")
  })
  df <- dplyr::bind_rows(out)
  df[order(df$decided, df$dkt, decreasing = TRUE), , drop = FALSE]
}

# The caption the opinion's own running head carries ("WOLFORD v. LOPEZ"), in
# the case of the docket's caption where a word of it appears there ("Wolford",
# "FCC"), and title case otherwise. NULL when the head gave none.
# One word of a running head ("O'DONNELL", "OF", "LLC") in the case a caption
# gives it, for when the docket's caption does not contain the word (or there
# is no docket caption: the OT2016 cases are not in cases/search.json). Title
# case alone wrote "O'donnell v. City Of Chicago" and "Tc Heartland Llc".
.OP_SMALL <- c("of", "and", "the", "for", "in", "on", "to", "by", "a", "an", "at", "ex", "rel.", "et", "al.", "de", "la", "del", "ex")
.OP_UPPER <- c("USA", "U.S.", "U.S.A.", "II", "III", "IV", "VI", "EPA", "SEC", "FBI", "IRS", "INS", "ICE", "CIA", "DOJ", "FERC",
               "NLRB", "FCC", "FTC", "FDA", "HHS", "CFPB", "OSHA", "FEC", "LLC", "LLP", "L.P.", "L.L.C.", "N.A.", "PLC", "AG", "SA", "S.A.", "NV", "N.V.")
.op_title_word <- function(x, first = FALSE) {
  bare <- toupper(sub("[,;]+$", "", x)); tail <- sub("^.*?([,;]*)$", "\\1", x)
  if (bare %in% .OP_UPPER) return(paste0(bare, tail))
  if (!first && tolower(bare) %in% .OP_SMALL) return(paste0(tolower(bare), tail))
  # Initials with no vowel in them stay capitals: "TC", "NRG", "BNSF".
  if (grepl("^[A-Z]{2,5}$", bare) && !grepl("[AEIOUY]", bare)) return(paste0(bare, tail))
  t <- str_to_title(x)
  # The capital after O' and D' ("O'Donnell", "D'Amico") and after Mc; each
  # half of a hyphenated name ("O'Connor-Ratcliff") is already capitalised.
  t <- gsub("\\b([OD])(['’])([a-z])", "\\1\\2\\U\\3", t, perl = TRUE)
  gsub("\\bMc([a-z])", "Mc\\U\\1", t, perl = TRUE)
}

opinion_short_caption <- function(short, long) {
  if (is.null(short) || !nzchar(short)) return(NULL)
  w <- strsplit(str_squish(short), " ")[[1]]
  lw <- str_extract_all(long %||% "", "[A-Za-z0-9.'’&-]+")[[1]]
  # The head sets a typographic apostrophe where the docket types a straight one.
  key <- function(x) tolower(gsub("[.,]", "", gsub("’", "'", x, fixed = TRUE)))
  out <- vapply(seq_along(w), function(i) {
    x <- w[i]
    if (tolower(x) == "v.") return("v.")
    hit <- lw[key(lw) == key(x)]
    # An agency's initials ("FCC" where the docket says "Federal Communications
    # Commission") stay capitals: the letters are the initials of words that
    # follow one another in the long caption.
    bare <- gsub("[^A-Za-z]", "", x)
    initials <- paste(substr(lw[grepl("^[A-Z]", lw)], 1, 1), collapse = "")
    core <- if (length(hit)) sub(",$", "", hit[1])
            else if (nchar(bare) >= 2 && nchar(bare) <= 6 && bare == toupper(bare) && grepl(bare, initials, fixed = TRUE)) sub(",$", "", x)
            else .op_title_word(x, first = i == 1L || identical(tolower(w[i - 1L]), "v."))
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

# ---- files the index no longer names ---------------------------------------------
# A page is published under its lead docket. When two dockets of one decision
# each had a page (the listing named the same opinion twice, as a slip under
# one number and as its U.S. Reports excerpt under the other) and one became
# the other's companion, its four files stayed: unlinked, but still served,
# and a second copy of the opinion for a search engine to find. So does the
# JSON of an orders opinion later found short. Anything in a Term's folder
# that is not a lead's file goes.
#
# Refuses to act on an index that names nothing, or when it would remove more
# than a tenth of what is there: that is a broken index, not orphans.
prune_opinion_files <- function(site_dir, idx) {
  lead <- idx[vapply(idx, function(e) isTRUE(e$lead), logical(1))]
  root <- file.path(site_dir, OPINION_DIR)
  files <- list.files(root, pattern = "[.](html|md|epub|json)$", recursive = TRUE)
  files <- files[grepl("^[0-9]{4}/[^/]+$", files)]
  if (!length(lead) || !length(files)) return(invisible(character()))
  keep <- sub("^/[^/]+/", "", sub("[.]html$", "", vapply(lead, function(e) e$href, character(1))))
  orphan <- files[!sub("[.](body[.]html|html|md|epub|json)$", "", files) %in% keep]
  if (length(orphan) > 0.1 * length(files)) {
    message("opinions: ", length(orphan), " of ", length(files), " files are not in the index -- too many to be orphans; none removed")
    return(invisible(character()))
  }
  unlink(file.path(root, orphan))
  invisible(orphan)
}

# ---- the index page -------------------------------------------------------------
write_opinion_index_page <- function(site_dir, idx) {
  ent <- idx[vapply(idx, function(e) isTRUE(e$lead), logical(1))]
  if (!length(ent)) return(invisible(NULL))
  df <- tibble::tibble(dkt = names(ent),
                       caption = vapply(ent, function(e) e$caption %||% "", character(1)),
                       decided = vapply(ent, function(e) e$decided %||% "", character(1)),
                       term = vapply(ent, function(e) as.integer(e$term), integer(1)),
                       href = vapply(ent, function(e) e$href, character(1)),
                       class = vapply(ent, function(e) e$class %||% "merits", character(1)))
  df <- df[order(df$decided, decreasing = TRUE), , drop = FALSE]
  terms <- sort(unique(df$term), decreasing = TRUE)
  blocks <- vapply(terms, function(t) {
    g <- df[df$term == t, , drop = FALSE]
    paste0("<section class='oterm' id='ot", t, "'><h2>October Term ", t, " <span>", nrow(g), " opinion", if (nrow(g) == 1) "" else "s",
           "</span></h2><ul>",
           paste(sprintf("<li><a href='%s'>%s</a><span class='od'>No. %s · %s%s</span></li>", g$href, .op_esc(g$caption), g$dkt,
                         vapply(g$decided, .op_long_date, character(1)),
                         ifelse(g$class == "orders", " · opinion relating to an order", "")), collapse = ""),
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
    .oix .oopds{font-size:.9rem;color:var(--ink-soft);border:1px solid var(--rule);border-radius:4px;background:var(--panel);padding:.6rem .8rem}
    .oix .oopds a{color:var(--link)}
    .oix .oopds code{font-size:.92em;overflow-wrap:anywhere}
  ")
  dek <- "The Court’s opinions as pages you can read, with a Markdown file and an EPUB of each: every merits decision, and the longer opinions on applications and orders."
  html <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n",
    page_head("Opinions — Supreme Court Report", site_breadcrumb_jsonld("Opinions"), extra_css = css,
              description = dek, path = paste0("/", OPINION_DIR, "/"),
              extra_head = paste0("<link rel='alternate' type='", OPDS_NAV, "' title='Opinions (OPDS catalogue)' href='/",
                                  OPINION_DIR, "/opds.xml'>")),
    "<body>", site_masthead(active = "/justices/"),
    "<main class='wrap oix' id='main'>", site_breadcrumb("Opinions"),
    "<p class='kicker'>", nrow(df), " opinions · October Terms ", min(terms), "–", max(terms), "</p>",
    "<h1>Opinions</h1>",
    "<p class='dek'>", dek, " Each is set from the PDF the Court published, which remains the authoritative text.</p>",
    "<p class='oopds'>To have new opinions arrive in an e-reader app, add this OPDS catalogue to it: ",
    "<a href='/", OPINION_DIR, "/opds.xml'><code>", OPDS_BASE, "/", OPINION_DIR, "/opds.xml</code></a></p>",
    paste(blocks, collapse = ""),
    "</main></body>\n</html>\n")
  writeLines(enc2utf8(smarten_html(html)), file.path(site_dir, OPINION_DIR, "index.html"), useBytes = TRUE)
  invisible(nrow(df))
}

# ---- opinions relating to orders ----------------------------------------------------
# The Court's other opinions: a per curiam on an emergency application (26A560),
# a dissent from a denial of certiorari, a statement respecting one. They are
# not on the Granted & Noted List, so the Justices section never names them;
# the Court lists them itself at /opinions/relatingtoorders/<yy>, one row per
# writing, several rows to a PDF (a second writing is the same file at
# "#page=N").
#
# One page per PDF, at the docket the listing gives it first. Only the LONG
# ones: OPINION_MIN_WORDS, counted after reading the file, because nothing in
# the listing says how long a writing is. A two-paragraph statement is not
# worth a page and an EPUB; No. 26A560's per curiam (1,085 words) is.
#
# opinions/orders.json is the listing as last read, keyed by PDF, so a Term is
# fetched once and only the newest two are re-read each run. Decisions the site
# already knows about (applications/applications.json, the Recent decisions
# manifests) are folded in, so a listing that cannot be fetched loses nothing
# that is already on the site.
OPINION_MIN_WORDS <- 750L
OPINION_ORDERS_URL <- "https://www.supremecourt.gov/opinions/relatingtoorders/%s"

opinion_orders_path <- function(site_dir) file.path(site_dir, OPINION_DIR, "orders.json")
opinion_skipped_path <- function(site_dir) file.path(site_dir, OPINION_DIR, "skipped.json")
.op_read_json <- function(p) if (file.exists(p)) tryCatch(fromJSON(p, simplifyVector = FALSE), error = function(e) list()) else list()
.op_write_json <- function(x, p) {
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  writeLines(if (length(x)) toJSON(x[order(names(x))], auto_unbox = TRUE, null = "null") else "{}", p, useBytes = TRUE)
}

# The listing's table -> tibble(dkt, date, url): a row is a docket number, a
# date and a PDF link ("#page=N" kept; a revision's "_diff_" file passed over).
# R/site_decisions.R has a fuller parser of the same table, for the Recent
# decisions panel; it is not used here because sourcing that file pulls in the
# argument navigator and gt with it, which this build does not otherwise need
# -- and did not have, the first time it ran (2026-11-10).
.op_parse_orders_listing <- function(html) {
  rows <- str_match_all(html, regex("<tr[^>]*>(.*?)</tr>", dotall = TRUE))[[1]][, 2]
  out <- lapply(rows, function(r) {
    tds <- str_match_all(r, regex("<td[^>]*>(.*?)</td>", dotall = TRUE))[[1]][, 2]
    if (length(tds) < 3) return(NULL)
    txt <- str_squish(gsub("&nbsp;|&#160;", " ", gsub("<[^>]+>", " ", tds)))
    dk <- txt[grepl("^[0-9]{2}(-[0-9]{1,5}|[AM][0-9]{1,4})$", txt)]
    hrefs <- str_match_all(r, "href\\s*=\\s*['\"]([^'\"]+[.]pdf(?:#[^'\"]*)?)['\"]")[[1]][, 2]
    hrefs <- hrefs[!grepl("_diff_", hrefs)]
    dt <- txt[grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$", txt)]
    if (!length(dk) || !length(hrefs) || !length(dt)) return(NULL)
    p <- as.integer(strsplit(dt[1], "/")[[1]])
    yr <- if (p[3] < 100L) 2000L + p[3] else p[3]
    href <- if (startsWith(hrefs[1], "/")) paste0("https://www.supremecourt.gov", hrefs[1]) else hrefs[1]
    tibble::tibble(dkt = dk[1], date = sprintf("%04d-%02d-%02d", yr, p[1], p[2]), url = href)
  })
  dplyr::bind_rows(out)
}

# One Term's listing -> tibble(dkt, date, url). A request that fails returns
# no rows, never an error.
fetch_order_opinions_listing <- function(term2) {
  url <- sprintf(OPINION_ORDERS_URL, term2)
  html <- tryCatch({
    resp <- httr2::request(url) |> httr2::req_user_agent("ceRt SCOTUS docketing dashboard (httr2)") |>
      httr2::req_timeout(45) |> httr2::req_retry(max_tries = 3) |> httr2::req_error(is_error = \(r) FALSE) |> httr2::req_perform()
    if (httr2::resp_status(resp) == 200L) httr2::resp_body_string(resp) else NA_character_
  }, error = function(e) NA_character_)
  if (is.na(html)) { message("  orders listing for OT", term2, ": not fetched"); return(NULL) }
  x <- .op_parse_orders_listing(html)
  if (!nrow(x)) return(NULL)
  x
}

# The cache, refreshed for `terms` (two-digit) and topped up from what the site
# already knows. Returns it: a list keyed by the PDF's URL without "#page",
# each {dkts, decided, url}.
refresh_orders_cache <- function(site_dir, terms = character()) {
  cache <- .op_read_json(opinion_orders_path(site_dir))
  add <- function(dkt, date, url) {
    if (is.na(url) || !nzchar(url) || is.na(dkt) || !grepl("\\.pdf", url)) return(invisible())
    # Once a Term is printed the listing points these into the volume too
    # (boundvolumes/586BV.pdf#page=826), where they sit among the orders in a
    # different layout. Not read: only an opinion that is a file of its own.
    if (grepl("boundvolumes|preliminaryprint", url)) return(invisible())
    base <- sub("#.*$", "", sub("^http:", "https:", url))
    e <- cache[[base]] %||% list(dkts = list(), decided = NULL, url = base)
    e$dkts <- as.list(unique(c(unlist(e$dkts), dkt)))
    if (!is.na(date) && nzchar(date)) e$decided <- min(c(e$decided, date))
    cache[[base]] <<- e
  }
  for (t in terms) {
    x <- fetch_order_opinions_listing(t)
    if (!is.null(x)) for (i in seq_len(nrow(x))) add(x$dkt[i], x$date[i], x$url[i])
    Sys.sleep(1)
  }
  apps <- .op_read_json(file.path(site_dir, "applications", "applications.json"))
  for (a in apps) if (!is.null(a$opinion_url)) add(a$dkt, a$outcome_date %||% NA_character_, a$opinion_url)
  for (f in c("arguments/decided.json", "dashboards/decided.json")) for (r in .op_read_json(file.path(site_dir, f))) {
    if (identical(r$kind, "argued")) next
    urls <- c(r$opinion_url, unlist(str_extract_all(r$writing_urls %||% "", "https?://\\S+")))
    for (u in urls) add(r$dkt, r$date %||% NA_character_, u)
  }
  cache <- cache[vapply(cache, function(e) !is.null(e$decided) && length(e$dkts) > 0, logical(1))]
  .op_write_json(cache, opinion_orders_path(site_dir))
  cache
}

# The cache as work-list rows, shaped like opinion_worklist()'s.
opinion_orders_worklist <- function(site_dir, cache) {
  if (!length(cache)) return(tibble::tibble())
  cap <- tryCatch(fromJSON(file.path(site_dir, "cases", "search.json"), simplifyVector = FALSE), error = function(e) list())
  rows <- lapply(cache, function(e) {
    d <- unlist(e$dkts); lead <- d[1]
    tibble::tibble(dkt = lead, dkts = list(d), url = e$url, decided = e$decided, volume = NA_character_, anchor = NA_integer_,
                   term = opinion_term(e$decided), caption = as.character(cap[[lead]] %||% paste("No.", lead)), class = "orders")
  })
  df <- dplyr::bind_rows(rows)
  # One page per docket: where a docket has two files (an opinion revised under
  # a new name), the newer.
  df <- df[order(df$decided, df$url, decreasing = TRUE), , drop = FALSE]
  df[!duplicated(df$dkt), , drop = FALSE]
}

# ---- OPDS: the EPUBs as a catalogue an e-reader can subscribe to ----------------
# OPDS 1.2 is Atom with a few link relations, so a "server" is a set of static
# files: nothing here needs anything GitHub Pages does not do.
#
#   opinions/opds.xml           the root: "Newest", then one entry per Term
#   opinions/opds/new.xml       the OPDS_NEW most recent opinions
#   opinions/opds/<yyyy>.xml    a Term's opinions
#
# A reader app is given https://supremecourt.report/opinions/opds.xml once; it
# re-reads the feeds itself and shows what is new. Two things a static host
# cannot do, both left out rather than faked: search (OpenSearch needs a
# server to answer the query) and paging (no feed here is long enough to need
# it -- a Term is ~60 entries).
#
# GitHub Pages serves these as application/xml, not OPDS's
# application/atom+xml;profile=opds-catalog. The apps tested against static
# catalogues read the feed's own `type` attributes on its links, which is
# where the profile is declared here; a `.xml` name keeps the bytes being
# served as XML at all.
OPDS_NEW <- 50L
OPDS_BASE <- "https://supremecourt.report"
OPDS_NAV <- "application/atom+xml;profile=opds-catalog;kind=navigation"
OPDS_ACQ <- "application/atom+xml;profile=opds-catalog;kind=acquisition"

.opds_x <- function(x) {
  x <- gsub("&", "&amp;", x %||% "", fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE); x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}
.opds_url <- function(path) paste0(OPDS_BASE, path)
.opds_time <- function(d) paste0(format(as.Date(d), "%Y-%m-%d"), "T00:00:00Z")

.opds_feed <- function(id, title, updated, self, self_type, entries, subtitle = NULL) {
  paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
    "<feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:dc=\"http://purl.org/dc/terms/\" xmlns:opds=\"http://opds-spec.org/2010/catalog\">\n",
    "<id>", id, "</id>\n<title>", .opds_x(title), "</title>\n",
    if (!is.null(subtitle)) paste0("<subtitle>", .opds_x(subtitle), "</subtitle>\n") else "",
    "<updated>", updated, "</updated>\n",
    "<author><name>Supreme Court Report</name><uri>", OPDS_BASE, "/</uri></author>\n",
    "<link rel=\"self\" href=\"", .opds_url(self), "\" type=\"", self_type, "\"/>\n",
    "<link rel=\"start\" href=\"", .opds_url(paste0("/", OPINION_DIR, "/opds.xml")), "\" type=\"", OPDS_NAV, "\"/>\n",
    if (!identical(self, paste0("/", OPINION_DIR, "/opds.xml")))
      paste0("<link rel=\"up\" href=\"", .opds_url(paste0("/", OPINION_DIR, "/opds.xml")), "\" type=\"", OPDS_NAV, "\"/>\n") else "",
    paste(entries, collapse = ""), "</feed>\n")
}

# One opinion as an acquisition entry. `df` rows: dkt, caption, decided, term,
# href, cite, words.
.opds_entries <- function(df) {
  if (!nrow(df)) return(character())
  stem <- sub("\\.html$", "", df$href)
  what <- vapply(seq_len(nrow(df)), function(i) paste(c(
    paste0("No. ", df$dkt[i]), paste("decided", .op_long_date(df$decided[i])),
    if (!is.na(df$cite[i]) && nzchar(df$cite[i])) df$cite[i],
    if (!is.na(df$words[i])) paste(format(df$words[i], big.mark = ","), "words")), collapse = " · "), character(1))
  paste0(
    "<entry>\n<id>urn:supremecourt-report:opinion:", df$term, ":", df$dkt, "</id>\n",
    "<title>", .opds_x(df$caption), "</title>\n",
    "<author><name>Supreme Court of the United States</name></author>\n",
    "<updated>", vapply(df$decided, .opds_time, character(1)), "</updated>\n",
    "<published>", vapply(df$decided, .opds_time, character(1)), "</published>\n",
    "<dc:language>en</dc:language>\n<dc:issued>", df$decided, "</dc:issued>\n",
    "<category term=\"ot", df$term, "\" label=\"October Term ", df$term, "\"/>\n",
    "<summary type=\"text\">", .opds_x(what), "</summary>\n",
    "<link rel=\"http://opds-spec.org/acquisition/open-access\" href=\"", .opds_url(paste0(stem, ".epub")), "\" type=\"application/epub+zip\"/>\n",
    "<link rel=\"http://opds-spec.org/acquisition/open-access\" href=\"", .opds_url(paste0(stem, ".md")), "\" type=\"text/markdown\"/>\n",
    "<link rel=\"alternate\" href=\"", .opds_url(df$href), "\" type=\"text/html\"/>\n",
    "</entry>\n")
}

write_opinion_opds <- function(site_dir, idx) {
  ent <- idx[vapply(idx, function(e) isTRUE(e$lead), logical(1))]
  if (!length(ent)) return(invisible(NULL))
  df <- tibble::tibble(dkt = names(ent),
                       caption = vapply(ent, function(e) e$caption %||% "", character(1)),
                       decided = vapply(ent, function(e) e$decided %||% "", character(1)),
                       term = vapply(ent, function(e) as.integer(e$term), integer(1)),
                       href = vapply(ent, function(e) e$href, character(1)),
                       cite = vapply(ent, function(e) e$cite %||% NA_character_, character(1)),
                       words = vapply(ent, function(e) as.integer(e$words %||% NA_integer_), integer(1)))
  # Only what is on disk: a feed must never offer a download that 404s.
  has <- file.exists(file.path(site_dir, sub("^/", "", sub("\\.html$", ".epub", df$href))))
  df <- df[has, , drop = FALSE]
  if (!nrow(df)) return(invisible(NULL))
  df <- df[order(df$decided, df$dkt, decreasing = TRUE), , drop = FALSE]
  dir <- file.path(site_dir, OPINION_DIR, "opds")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  put <- function(x, path) writeLines(enc2utf8(x), path, useBytes = TRUE)
  newest <- .opds_time(max(df$decided))
  root <- paste0("/", OPINION_DIR, "/opds.xml")
  terms <- sort(unique(df$term), decreasing = TRUE)
  nav <- function(href, title, text, updated, rel = "subsection") paste0(
    "<entry>\n<id>", .opds_url(href), "</id>\n<title>", .opds_x(title), "</title>\n<updated>", updated, "</updated>\n",
    "<content type=\"text\">", .opds_x(text), "</content>\n",
    "<link rel=\"", rel, "\" href=\"", .opds_url(href), "\" type=\"", OPDS_ACQ, "\"/>\n</entry>\n")
  new_href <- paste0("/", OPINION_DIR, "/opds/new.xml")
  entries <- c(
    nav(new_href, "Newest opinions", sprintf("The %d most recent", min(OPDS_NEW, nrow(df))), newest,
        rel = "http://opds-spec.org/sort/new"),
    vapply(terms, function(t) {
      g <- df[df$term == t, , drop = FALSE]
      nav(sprintf("/%s/opds/%d.xml", OPINION_DIR, t), paste("October Term", t),
          sprintf("%d opinion%s", nrow(g), if (nrow(g) == 1) "" else "s"), .opds_time(max(g$decided)))
    }, character(1)))
  put(.opds_feed(.opds_url(root), "Supreme Court Report: Opinions", newest, root, OPDS_NAV, entries,
                 subtitle = "The Supreme Court's merits opinions as EPUBs, set from the PDFs the Court publishes"),
      file.path(site_dir, OPINION_DIR, "opds.xml"))
  top <- utils::head(df, OPDS_NEW)
  put(.opds_feed(.opds_url(new_href), "Newest opinions", newest, new_href, OPDS_ACQ, .opds_entries(top)),
      file.path(dir, "new.xml"))
  for (t in terms) {
    g <- df[df$term == t, , drop = FALSE]
    href <- sprintf("/%s/opds/%d.xml", OPINION_DIR, t)
    put(.opds_feed(.opds_url(href), paste("Opinions, October Term", t), .opds_time(max(g$decided)), href, OPDS_ACQ,
                   .opds_entries(g)), file.path(dir, paste0(t, ".xml")))
  }
  # A Term with nothing left in it (never, in practice) must not keep its feed.
  stale <- setdiff(list.files(dir, pattern = "^\\d{4}\\.xml$"), paste0(terms, ".xml"))
  unlink(file.path(dir, stale))
  invisible(nrow(df))
}
