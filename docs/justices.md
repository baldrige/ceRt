# The Justices

Per-Term statistics on each Justice: opinions written by kind, who joined whom,
how often the Court split, and the list of writings behind every number.
Published at `justices/` — one page per Term (`ot2025.html`) and an index —
by `render_justices.R`, from `R/justices.R`. The design was mocked up first
(2026-09-13) and the page follows it panel for panel.

## Two sources

| source | what it gives | covers |
| --- | --- | --- |
| **The Granted & Noted List** (`arguments/granted_noted.json`, `R/granted_noted.R`) | per argued case: author, each separate writer and the kind code (`D`, `C`, `C/J`, `C/P`, …), result, unanimity flags, who took no part | every decided argued case, OT16 on |
| **The syllabus lineup paragraph** of each slip opinion (`justices/lineups.json`) | who *joined* what: "ALITO, J., delivered the opinion of the Court, in which ROBERTS, C. J., and THOMAS, GORSUCH, KAVANAUGH, and BARRETT, JJ., joined. BARRETT, J., filed a concurring opinion, in which THOMAS and GORSUCH, JJ., joined as to Part II–B." | whatever has been fetched and parsed; the page says how many |

Panels 1 (opinions written) and 4 (the writings list) come from the list alone
and are complete. Panels 2 (agreement) and 3 (splits, lineups, most-often-alone,
most-often-in-the-majority) need the lineups, and each names its coverage
("Lineups parsed for 57 of 61 decisions"). Unanimity in panel 3 is from the
list's own flags, so it too covers every decision.

## Where the opinion PDF comes from, and the three shapes it takes

The Court's slip-opinion feed (`fetch_opinion_listing()`, `R/site_decisions.R`)
names the PDF for every decision of a Term in one request. Three shapes,
measured 2026-09-13:

- **A slip opinion** (`/opinions/25pdf/24-1046_nmio.pdf`), OT19 on and every
  current Term. Names in small caps; the lineup paragraph closes the syllabus.
- **A preliminary print** (`/opinions/preliminaryprint/588US2PP_final.pdf#page=502`),
  volumes 588 on. One PDF for half a volume, a page anchor per case.
- **A bound volume** (`/opinions/boundvolumes/580BV.pdf#page=324`), volumes
  580–587. The feed keeps naming the *preliminary print* for these, which now
  404s, so a dead preliminary print falls back to the bound volume of the same
  number. The anchor is a hint only (the two paginate differently): the case is
  located by searching the volume for its syllabus header, `No. 15–8049. Argued
  … Decided …`, and the window stops at the next case's header so a one-page
  dismissal cannot inherit its neighbour's lineup (Cox v. United States took
  Ohio v. American Express's on the first pass).

A volume is downloaded and text-extracted once per run and memoised (6–8 MB,
1,100 pages), so the OT16–OT18 back-catalogue costs eight downloads, not 190.

The volumes set the paragraph in mixed case and their text layer drops the
"fi" ligature — `Thomas, J., fled a dissenting opinion, in which Alito, J.,
joined, post, p. 128.` — and hyphenate at line ends (`con- curring`, `Gor-
such`, `Mem- bers`). Running heads land inside a paragraph that crosses a page
(`Cite as: 584 U. S. 357 (2018)`, `Syllabus`, and `Opinion of the Court`, which
the volumes print above a page that is still the syllabus). The parser is
case-insensitive, de-hyphenates, strips the page cites and the running heads
before looking for the end of the paragraph, and matches `f(i)?led`.

## The grammar

One sentence per statement, split where a Justice's name opens the next:

| sentence | read as |
| --- | --- |
| `X, J., delivered the opinion of the Court, in which A, B, and C, JJ., joined.` | lead opinion, author X, full joiners A B C |
| `… delivered the opinion for a unanimous Court.` / `… in which all other Members joined` | everyone participating joined |
| `X, J., announced the judgment of the Court and delivered the opinion of the Court with respect to Parts I and II, in which …, and an opinion with respect to Part III, in which …` | a plurality (`judgment`): joiners of any clause are on the judgment side; nobody joined "in full" |
| `… delivered the opinion of the Court, except as to Part IV–B. A, C. J., and B, J., joined that opinion in full; C, J., joined except as to Part IV–B.` | the split-opinion form: each `joined` chunk names its joiners, the qualifier after it marks them partial |
| `X, J., filed a concurring opinion, in which Y, J., joined as to Part II–B.` | a separate writing; kind from the description (`concurring`, `concurring in the judgment`, `dissenting`, `concurring in part and dissenting in part` → mixed); joiners with a qualifier are partial |
| `X, J., and Y, J., filed opinions concurring in the judgment.` | two writings, one each |
| `… in which all other Members joined, except Z, J., who took no part …` / `Z, J., took no part in the consideration or decision of the case.` | Z did not participate; the lead sentence is still read |
| *(no `delivered` sentence)* | a per curiam: writings from the syllabus if it lists them, else from the body headers `JUSTICE X, with whom JUSTICE Y joins, dissenting.` |

Verified against the list on the first 140 decisions (OT16 and most of OT17):
**0 author disagreements; the separate writers match on 137 of 140**, and the
three that differ are the list's own gaps (an `Other:` the list left empty,
and Cox v. United States, dismissed, under which the list files Ortiz's
writings). Each cached entry carries `pv = LINEUP_PARSER_VERSION` and its lineup text.
Bump the version after a grammar change: the next render re-reads every
syllabus-parsed entry from its cached text at no cost (546 entries in under a
second on 2026-09-14), and only per curiams, whose writings came from the body
pages, need `lineup_retry` to fetch again. A preliminary print's "Page Proof
Pending Publication" watermark sits inside the text and is stripped before
either parse.

## How things are counted

- **A decision** is one written opinion of the Court in an argued case.
  Consolidated dockets argued together share a block on the list (its `group`)
  and count once.
- **A mixed writing** (`C/P, D/P`) counts once, under its lead label (the first
  token's stance), and is marked partial. It is never two writings.
- **A plurality** credits its author with the lead opinion; nobody "joined in
  full", so the strict matrix reading has no agreement on it.
- **Sides.** A Justice is on the majority side if they wrote or joined the
  Court's opinion (any part) or wrote or joined a concurrence; on the dissent
  side if they wrote or joined a full dissent; **mixed** if they did both, or
  wrote or joined a partial dissent. Per curiam: everyone not dissenting is the
  majority.
- **Agreement (judgment)**: share of decisions both took part in where both were
  on the same side. A mixed Justice is on neither side, so that decision drops
  out of any pair including them.
- **Agreement (full)**: share where both joined the opinion of the Court without
  a qualifier (the author counts as joining), and the opinion itself was not
  split into parts.
- **Vote splits** count majority against everyone else, so a partial dissent is
  on the minority side; `9–0` therefore includes cases with a concurrence in the
  judgment, which the unanimity tile (from the list's flags) separates.
- **Who sat.** The roster (`JUSTICE_ROSTER`) is by seat date, and participation
  is decided per decision date, not per Term: OT16 opened with eight Justices,
  OT20 with eight. A Justice not yet seated is neither a participant nor a
  recusal. And a signed opinion's syllabus names every participant, as author,
  joiner or separate writer, so a Justice named nowhere in it did not sit —
  seated after the argument (Gorsuch through the spring of 2017, Barrett
  through the autumn of 2020) or recused without the syllabus saying so. That
  rule took OT16's coverage from 42 to 61 of 62.
- **A decision with no PDF.** The feed named none for ~20 of 691 (seven of
  OT20's, the tail of a Term the preliminary print had not reached). The
  docket page is tried next (`opinions/NNpdf/` links, OT19 on); failing that
  the decision counts in panels 1 and 4 and is left out of 2 and 3.
- **Merits only.** Argued cases on the Granted & Noted List. Dissents from denial
  and emergency-docket writings live on the order lists and are not counted
  here (a separate stream, when it comes).

## Colour

Four opinion kinds, `PALETTE_OPINIONS` in `R/palette.R`: the model chart's
baseline blue for the Court's opinion, the timeline's orange and light green
for concurrences, the accent for dissents. Validated with the data-viz palette
checker on the paper surface; the orange/green pair sits in the colour-vision
warn band, which the 2px gaps between segments and the counts table beside the
chart are there to cover. The agreement matrix is the grant ramp (paper →
accent) over 50–100%.

## Rollout

`SITE_SECTIONS` gains `/justices/`, and the audit fails a nav target that does
not resolve, so the section must exist on gh-pages before the next daily
rewrites the masthead. Order after merge:

1. `gh workflow run render-justices.yml --ref main` — the back-catalogue
   (default cap 800 PDFs; the volumes make OT16–OT18 cheap). Re-dispatch until
   the log reports no uncached decisions.
2. The weekly then keeps it current (`LINEUP_MAX_NEW=120` in `conferences.yml`).

Each argument navigator page (`arguments/arg_YYYY.html`, OT16 on) links its
Term's Justices page from the footer, and the landing page lists the section
after the Oral Argument Navigator.
