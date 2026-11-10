# Opinions: the Court's PDFs as pages, Markdown and EPUB

The Court publishes an opinion as a PDF and nothing else. `/opinions/` holds
each merits opinion as a page of this site, with a Markdown file and an EPUB
beside it:

| file | what |
| --- | --- |
| `opinions/<yyyy>/<docket>.html` | the reader: syllabus, the Court's opinion, each separate writing, footnotes at the foot of each |
| `opinions/<yyyy>/<docket>.md` | the same text as Markdown, footnotes as `[^s3-12]` |
| `opinions/<yyyy>/<docket>.epub` | EPUB 3, one chapter per writing, pop-up footnotes, the original's page list |
| `opinions/<yyyy>/<docket>.json` | the extracted structure; the cache the other three are rendered from |
| `opinions/index.json` | docket → `{href, url, xv, ov, caption, cite, words, notes, unmatched, lead}`; a companion docket points at its lead's page |
| `opinions/index.html` | every opinion, by Term |
| `opinions/opds.xml`, `opinions/opds/new.xml`, `opinions/opds/<yyyy>.xml` | the OPDS catalogue: the EPUBs as feeds an e-reader app subscribes to |

`<yyyy>` is the October Term (a June 2026 decision is under `2025`). No
masthead link: there is no room for an eleventh (CLAUDE.md), and the section
sits under **Justices** in the nav's active state. Readers arrive from a case
page or a Justices page.

## The pipeline

```
justices/lineups.json  ──►  build_opinions.R  ──►  download the PDF (paced)
 (docket → PDF URL)             │
                                ├─ extract_opinion.py   PDF  → <docket>.json
                                ├─ render_opinion.py    JSON → .md, .epub, HTML body
                                └─ R/opinion_page.R     body → the site's page; index
```

`opinions.yml` runs it on dispatch (`max_new` PDFs a run; resumable): the
back-catalogue, and the repair tool. The nightly `conferences.yml` runs the
same script with a cap of 25 after the Justices render, which is what names a
new decision's PDF.

**Links in.** A case page whose docket is in `opinions/index.json` links its
"Decided" to the opinion page and adds "Read the opinion · EPUB · the Court's
PDF" (`render_docket_pages()` reads the index; the href is part of that
page's manifest key, appended only where there is one, so building an opinion
re-renders that case's page and no other). The Justices' writings tables add
"opinion" beside the docket number. Both pick a new page up the next time
they render.

**One reading of the PDF, three renderings.** `extract_opinion.py` is the
only thing that reads the PDF. A fault in the text is fixed there, once. The
JSON is published, so a change to the page or to a renderer re-renders
everything without downloading anything (`max_new: 0`).

**Versions.** `EXTRACT_VERSION` (extractor), `RENDER_VERSION` (renderer) and
`OPINION_PAGE_VERSION` (R) are recorded per opinion in `index.json`. An
opinion is re-extracted when its PDF's URL changes — the Court replaces a
slip with a U.S. Reports excerpt, and issues `_new_` revisions — or when the
extractor's version does; re-rendered when either of the other two does.

## The OPDS catalogue

An e-reader app (KOReader, Thorium, Moon+ Reader, Marvin and the like) is
given `https://supremecourt.report/opinions/opds.xml` once; it re-reads the
feeds itself and offers what is new. OPDS 1.2 is Atom with a few link
relations, so the "server" is three kinds of static file written by
`write_opinion_opds()` at the end of every build:

- `opds.xml` — the root, a navigation feed: "Newest opinions", then one entry
  per Term.
- `opds/new.xml` — the 50 most recent opinions.
- `opds/<yyyy>.xml` — a Term's opinions.

Each opinion is an entry with an open-access link to its EPUB (and one to its
Markdown, and the page as `alternate`). The entry's `id` is the EPUB's own
`dc:identifier`, so an app recognises a book it already holds.

What a static host cannot do is left out rather than faked: **no search**
(OpenSearch needs a server to answer the query) and no paging (a Term is
about 60 entries). GitHub Pages serves the feeds as `application/xml`, not
as `application/atom+xml;profile=opds-catalog`; the profile is declared on
every link's `type` instead. This has **not been tested in a reader app** —
only for well-formed XML and for every link resolving, which `audit_site.R`
also FAILS on.

A feed lists only EPUBs that are on disk, so it cannot offer a download
that 404s.

## What is built, and what is not

The merits decisions the Justices section tracks (`justices/lineups.json`),
OT2016 on: about 600. The Court's site serves them two ways, and both are
built:

- **A file of its own** — a slip opinion, or the U.S. Reports excerpt that
  replaces it. 343 decisions on 2026-11-10, almost all of OT2021–OT2025.
- **A page range of a whole volume** —
  `boundvolumes/580BV.pdf#page=324`, `preliminaryprint/588US2PP_final.pdf#page=502`:
  one PDF of a thousand pages and more, 22 of them holding about 257
  decisions, most of OT2016–OT2020.

Not built: opinions relating to orders, in-chambers opinions, emergency-docket
per curiams.

### A case inside a volume

The volume is downloaded once per run, however many of its cases are wanted,
and `extract_opinion.py --batch` reads each case out of it. The listing's
`#page=` is only a hint: the listing can name a preliminary print the Court
no longer serves, and the bound volume that replaced it (the build falls back
to `boundvolumes/<vol>BV.pdf`, as `R/justices.R` does) paginates differently.
So a case is **found by the line its first page prints** — `No. 15-8049.
Argued … Decided …` — or, where that line wraps between the number and
"Argued", by the first page of a case that prints the number at all. It ends
at the next case's first page, or at the volume's back matter.

The page links "the Court's PDF" to the file actually read, at the page the
case was found on (`pdf` in `index.json`); `url` stays the listing's, which is
what decides whether to re-read.

Measured on three whole volumes (580BV, 587BV, 588US2PP; 56 decisions): all
56 found; every one has exactly one opinion of the Court, every one but a
one-page per curiam a syllabus, and every footnote reference its note.

## Reading the PDF

PyMuPDF's `rawdict`: every character with its box, font and size. Two
layouts, both measured on real files:

| | slip (`25-365_xxxx.pdf`) | print (`609us1r58_xxxx.pdf`, preliminary prints) |
| --- | --- | --- |
| pagination | each writing restarts at 1 | U.S. Reports pages, continuous |
| a writing begins | on a new page, under a masthead | anywhere, mid-page |
| footnotes | 9 pt under an em-dash rule | 9 pt, no rule |
| small caps | capitals two points smaller | a small-caps font |
| fi / fl | intact (as ligature characters) | **the second letter is missing** |

What decides each thing:

- **Running head** — the small lines at the top. It names the page's number
  and, usually, its section; the section is used only for the syllabus.
- **Sections** — a new one at each byline (`Justice Alito delivered the
  opinion of the Court.`, `Justice Jackson, with whom … joins, dissenting.`,
  `Per Curiam.`). This is what makes a print's mid-page start work. A byline
  in the syllabus's small type is not one: the syllabus of a fractured
  decision summarises each part in the byline's words.
- **Footnotes** — below the rule, or the trailing run of lines set smaller
  than the body. A note's number is a superscript by flag, by rise, or by
  being much smaller than its line (the bound volumes give neither of the
  first two). A note belongs to the section that cites its number on that
  page; a note with no number continues the one before it. Several short
  notes can share a line.
- **Paragraphs** — a first line indented one em. A line indented two or more
  is a quotation. The margin is measured over the whole document, because a
  page can be two lines of opinion above a page of footnote.
- **The syllabus** has no fixed margin (`Held:` hangs, each point is set
  in), so a paragraph's own second line says where its lines sit.
- **Headings** — a short line centred on the page (`I`, `A`, `1`, `* * *`).
- **Page breaks** — kept as markers carrying the original's page number, so
  a pin cite can be checked. One that falls inside a word is moved to the
  word's end at render time.

### The missing ligatures

In the print layout the fonts map the "fi" and "fl" glyphs to a bare "f":
`frearm`, `fled a concurring opinion`, `refect`. Every text tool reads it
that way (poppler, MuPDF), and `R/justices.R` already works around it for
the lineup paragraph. The glyph is still there and still wide — 0.56 em
against a plain f's 0.33 — so a wide "f" is a ligature. Which one comes from
`fi_fl_words.json`, a count of fi/fl words in clean text (the QP corpus and
the argument transcripts): `fled` with a wide f is `filed`; `fed` with one is
`fled`. A word the list does not know is guessed (before a vowel, "fl";
otherwise "fi") and counted in the JSON's `qa.ligatures_guessed`.

### Hyphens at a line's end

The PDF's soft hyphen marks a word broken for the line, but not reliably: a
slip sets `not-so-strange` and `self-deport` with a soft hyphen at the
break, and `Con-gress` with a real one. So the joined word is looked up in
`vocab.txt` (22,000 words from the same clean text) and in the document's
own words; a compound the document writes with its hyphen elsewhere keeps
it. `qa.hyphens_kept` lists every hyphen kept at a break.

## Checks

`qa` in each JSON, and the same in `index.json`:

- `notes_unmatched` — per section, note numbers cited with no note, and
  notes never cited. Zero on the 60 most recent decisions (58 excerpts, 2 slips; 2026-11-10), each
  of which also came out with a syllabus and exactly one opinion of the Court;
  `audit_site.R` WARNs on any.
- `ligatures_guessed`, `hyphens_kept` — the judgement calls, to read.
- `build_opinions.R` refuses to publish a file in which it found no opinion
  of the Court; `audit_site.R` FAILS on a page with no text or an index
  entry missing one of its four files.

The EPUB's XML is checked for well-formedness in development; it has **not**
been run through `epubcheck`.

## Known faults

- A URL broken across lines keeps the break's space or hyphen.
- A figure (a map, a photograph) is not carried over; its caption is.
- A table, or verse, is read as paragraphs.
- An appendix is read as part of the writing it follows.
- The print layout's counsel paragraphs and amici note are kept, folded away
  under "Counsel" on the page.
- Captions are the running head's ("Wolford v. Lopez"), cased from the
  docket's caption; a head that wraps gives none, and the docket's long
  caption is used.

## Files

| file | role |
| --- | --- |
| `.github/scripts/opinions/extract_opinion.py` | PDF → JSON |
| `.github/scripts/opinions/render_opinion.py` | JSON → Markdown, EPUB, HTML body |
| `.github/scripts/opinions/fi_fl_words.json`, `vocab.txt` | the word lists; built once from site text, not regenerated by any workflow |
| `.github/scripts/opinions/requirements.txt` | PyMuPDF, pinned |
| `R/opinion_page.R` | work list, page, index |
| `.github/scripts/build_opinions.R` | the driver |
| `.github/workflows/opinions.yml` | dispatch |
