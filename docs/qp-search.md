# Questions presented: search, and similar questions

Two things built from the Question Presented (QP) text the site already
extracts for its conference reports, dashboards and case pages:

- **`/questions/`** — a search over the questions themselves, granted and
  denied. The case search (`docs/search.md`) finds a case by party or number;
  this finds petitions by issue.
- **"Similar questions"** on a case page — the petitions whose question is
  worded most like this one.

Both are static. `R/qp_index.R` builds JSON under `/questions/`; `/qp.js` (one
file, at the site root beside `search.js`) reads it in the browser. Neither has
a masthead link — the nav has no room for one. The search is reached from the
line under every case-search box, from the case search's "no matching cases"
row (which carries the reader's words over), and from each case page's block.

Figures below are from the build of 2026-11-09 against the published caches.

## What is built

| file | what | size |
| --- | --- | --- |
| `questions/index.html` | the search page; its coverage table is rendered here, from `meta.json` | 35 KB |
| `questions/meta.json` | Terms, counts, coverage, thresholds, the input hash | 1 KB |
| `questions/text/<tt>.json` | one Term: `[docket, caption, subject, granted, text]` per petition | 12 files, 5.8 MB gzipped in all; the largest (OT2025) 1.2 MB |
| `questions/like/<tt>.json` | docket → up to 25 `[neighbour, cosine×100]`, for the search page's `?like=` view | 0.14 MB gzipped in all |
| `questions/near/<tt>-<b>.json` | docket → `{n, k, same, top}` with up to six neighbours **and their captions**, in buckets of 100 docket numbers — what a case page loads | 255 files, median 3 KB gzipped, none over 5 KB |
| `/qp.js` | matcher, snippets, both pages' wiring | 16 KB |

`write_qp_index(site_dir)` does all of it and is called by `build_dashboards.R`
(the daily) and `render_conferences.R`, after each has rendered its docket
pages. The build takes about 70 seconds and is **skipped when nothing it reads
has changed** — it hashes the usable texts, captions, subject labels, granted
flags, coverage counts and its own parameters into `meta.json`.

## The gate: what counts as a question

The caches hold 17,046 petition "QPs". 1,201 of them (7%) are not questions,
and `qp_usable()` keeps them out of both the search and every neighbour list:

| rejected as | about | what it is |
| --- | --- | --- |
| not prose | 750 | OCR of a handwritten form |
| counsel block | 260 | the cover's list of counsel, read as the QP when it ran onto page ii |
| short | 125 | a page folio, a stray line, a sentence cut off |
| cover or contents | 65 | the cover page, a table of contents, a parties list |

The rule doing most of the work is the vocabulary one: the share of a text's
words that at least five petitions in the corpus use. The median real QP
scores 0.99; the gate is at 0.88. In a sample between 0.80 and 0.88, ten of
twelve were OCR; between 0.88 and 0.93, eleven of twelve were real. It needs
the whole corpus, which is why the gate is vectorised.

`qp_plain()` runs first: it strips the heading, rejoins a word the PDF broke
across lines, and cuts a table of contents that the extractor caught *after* a
real question (dot leaders), keeping the question.

**The case page applies the same gate.** `render_dockets_for()` passes its
merged docket → QP map through `usable_qp_map()`, so a page prints a "Question
presented" section exactly when this index lists the question; before that,
about 1,200 pages printed a counsel block or OCR noise under the heading. The
gate, `qp_plain()` and their three parameters therefore live in
`R/qp_extract.R`, which every page render already sources, not in
`R/qp_index.R`. Two consequences:

- The verdict is an *input* to the page (the manifest key hashes the QP), so a
  text the gate starts or stops rejecting re-renders that one page. No
  `PAGE_TEMPLATE_VERSION` bump is involved.
- The gate is relative to the corpus — a word counts as vocabulary when five
  petitions use it — so a text near the 0.88 line can change sides as the
  caches grow. That is a handful of pages, each a cheap re-render.

`usable_qp_map()` is memoised on the map's content, because the gate costs
about a millisecond a text and the daily calls `render_dockets_for()` half a
dozen times in one session.

## Coverage

15,845 questions. Coverage is close to complete for paid petitions from OT2018
and for *in forma pauperis* petitions only from OT2025:

| Term | paid | IFP |
| --- | --- | --- |
| OT2026 | 582 of 612 | 921 of 1,092 |
| OT2025 | 1,328 of 1,432 | 2,087 of 2,702 |
| OT2024 | 1,186 of 1,329 | 252 of 2,529 |
| OT2023 | 1,227 of 1,375 | 3 of 2,848 |
| OT2022 | 1,135 of 1,252 | 4 of 2,907 |
| OT2021 | 1,445 of 1,611 | 3 of 3,288 |
| OT2020 | 1,611 of 1,829 | 330 of 3,477 |
| OT2019 | 1,341 of 1,478 | 5 of 3,930 |
| OT2018 | 1,447 of 1,593 | 8 of 4,847 |
| OT2017 | 873 of 1,718 | 9 of 4,594 |

OT2015 and OT2016 contribute the 48 argued cases the argument navigator holds.
The page prints this table itself, from `meta.json`, so it cannot go stale.
The IFP gap is closed by `backfill-qp-all.yml` (`types: ifp`), about 28,000
petition PDFs; the index picks up whatever the caches gain on the next build,
and a `rerender-dockets.yml` run puts the questions on the case pages.

What that backfill can and cannot get, measured on 40 OT2022 IFP petitions
before running it: 38 carried a text layer (the Court's own OCR of the paper
filing), so extraction is the fast path, about a second a petition with
pacing; 36 yielded a text; and about 8 of those 36 were the OCR of
handwriting, which the gate rejects. Roughly seven IFP petitions in ten end up
with a searchable question. The rest are handwritten, or forms with no
"Question presented" heading at all.

`enrich_qp.R` keeps a **ledger** of the petitions that yielded nothing
(`conferences/qp_tried.json`, docket → the URL read) and does not read them
again; a failed *download* is never written to it, and ten in a row stop the
run as throttled. Without the ledger the misses sit at the head of every
later dispatch — at three in ten of 4,000 petitions a Term, the third run
would do little but re-read them.

**"Review granted"** is the union of `cases/grants.json` (complete from
OT2025) and the argued cases in `arguments/qp_cache.json`. A grant that was
neither argued nor recent — a summary reversal in OT2019, say — is missing, so
the flag is shown as a positive mark and as a filter, and the page says a
petition without it was not necessarily denied.

## Search

There is no inverted index. A Term is a few hundred kilobytes of text; `qp.js`
folds each to lower-case ASCII once on arrival and scans it. Terms load newest
first, one at a time, and every arrival re-runs the query, so the Terms a
reader most likely wants are searchable after the first megabyte. Nothing is
fetched until the box takes focus or the URL carries a query.

1. **Fold** both sides: lower-case, strip diacritics, anything not a letter or
   digit becomes a space. `922(g)(1)` is `922 g 1` on both.
2. **Terms.** Each run of non-space characters is a term. One that folds to
   several tokens (`922(g)(1)`, `u.s.c.`, `rooker-feldman`) is a phrase, as is
   anything in quotation marks. A quoted phrase matches whole words; anything
   else also matches a longer word it begins (`arbitra` finds arbitration).
3. **Every term must appear.** A few function words (`the`, `of`, `whether`…)
   are ignored unless they are all the reader typed. When no question has
   every term and there are at least two, the questions with all but one are
   returned and the status line says so.
4. **Rank:** BM25 (k 1.2, b 0.6) over the terms, with the document frequency
   taken over the petitions the filters pass; plus a little for a whole-word
   match, for the terms standing within 90 characters, and for the query
   appearing verbatim. Ties go to the newest Term.
5. **Filters:** Term, subject area (`cases/subjects.json`), review granted,
   and order (best match or newest). A filter with no words is a browse.

The state lives in the URL (`?q=…&term=24&subject=…&granted=1&sort=new`), so a
search can be linked.

Ranking is relevance only. A landmark is not boosted: "chevron deference"
ranks *Loper Bright* 36th of 44, because its question mentions Chevron once at
the end of a long paragraph and three dozen petitions are about nothing else.
With "Review granted" ticked it is one of five.

### Probes

`node .github/scripts/qp_probe.js site/questions` runs 20 queries and 6
neighbour pairs against a built index; a miss fails, a query over 80 ms warns.
`--explore "<query>"` prints the top ten instead. On 2026-11-09 (15,845
questions, folded in ~370 ms):

| probe | finds | rank | ms |
| --- | --- | --- | --- |
| `chevron overrule` | Loper Bright, Relentless | 5, 4 of 14 | 30–36 |
| `"habitual residence" hague` | Monasky v. Taglieri | 1 of 12 | 43 |
| `transgender athletes` | Little v. Hecox | 1 of 1 | 27 |
| `section 230`, review granted | Gonzalez v. Google | 1 of 1 | 2 |
| `race admissions harvard` | SFFA v. Harvard | 1 of 2 | 53 |
| `bump stock machinegun` | Garland v. Cargill | 2 of 4 | 64 |
| `presidential immunity criminal prosecution` | Trump v. United States | 1 of 1 | 45 |
| `1983 malicious prosecution` | Chiaverini v. Napoleon | 3 of 29 | 49 |
| `922(g)(1) bruen`, `almendarez-torres`, `mcgirt overruled`, `rooker-feldman` | a named petition among the many | found | 14–42 |
| `xyzzyqq` | nothing | — | 15 |

## Similar questions

TF-IDF over the gated text, cosine similarity, every pair
(`qp_tokens()`, `qp_tfidf()`, `qp_neighbours()`):

- **Tokens** are single words and adjacent pairs, after a stop list that
  includes the words every QP shares ("whether", "court", "petitioner"). A
  statute section stays whole — `922(g)(1)` is the token `922g1` — because a
  section number is the most specific word a question contains.
- **Weights:** sublinear term frequency, `log(N/df)`, pairs at 0.7 of a word.
  A term in one petition only, or in more than a quarter of them, is dropped.
- **Pairs earn their place.** Measured by whether a petition's five nearest
  carry the same subject-area label: 0.662 with words alone, 0.702 with pairs,
  against 0.178 by chance.

Two thresholds, both read off the data:

| cosine | same subject area | what the pairs are |
| --- | --- | --- |
| 0.15–0.20 | 0.76 | same field, different question |
| 0.20–0.25 | 0.80 | same doctrine or statute (Rooker-Feldman; § 922(g)(1) worded differently) |
| 0.30–0.40 | 0.85–0.89 | same question, independently written |
| ≥ 0.75 | 0.94–1.00 | the same text with small edits: a public defender's standard petition, a pro se litigant's second filing |

`QP_SIM_FLOOR` is **0.22**: 7,572 petitions (48%) have at least one neighbour
above it. `QP_SIM_SAME` is **0.75**, shown as "nearly the same words". Known
companions land where they should: *Loper Bright*/*Relentless*,
*SFFA v. Harvard*/*v. UNC*, *Moody v. NetChoice*/*NetChoice v. Paxton* and
*Trump v. Anderson* and its companion are each other's first neighbour;
*Hecox*/*B.P.J.* second; *Biden v. Nebraska*/*Department of Education v.
Brown* first and fourth.

**Boilerplate.** Some questions are filed dozens of times in one form — the
§ 922(g)(1) challenge, *Almendarez-Torres*, *McGirt*. A case page is handed at
most three near-identical neighbours of its six, so it also shows a question
that is not a copy, and says how many copies there are ("21 petitions ask this
in nearly the same words"). The `?like=` view lists the 25 closest.

**It compares wording.** Two petitions about the same decision below, worded
differently, can miss each other; two that quote the same statute at length
can match on the quotation. The page and the block both say what the
comparison is.

## Case pages (template v51)

A petition with a QP carries

```html
<section id='qpsim' class='qpsim' data-d='24-38' hidden>…</section>
<script src='/qp.js' defer></script>
```

and `qp.js` fills it from `questions/near/24-0.json` or leaves it hidden — no
entry, no file, or no JavaScript all look the same: no block.

The neighbours are **not a page input**, deliberately. Baked into the page, a
list would go stale the day a newer petition asked the same question: the
daily re-renders only the dockets it fetched, and the manifest key would see
nothing changed on the older page. Loaded at read time, a 2019 page shows a
2026 neighbour the morning after it is indexed, and the index can be rebuilt
without touching a case page.

The hook itself is markup, so it needed the bump to v51. The daily carries it
to the current Term; **the back-catalogue needs one `rerender-dockets.yml` run**
(`reuse_from_runs`, render-only). Until then an older page simply has no block.

## Publishing

`questions/` and `qp.js` are written by two workflows that can overlap.
`publish_site.sh` resolves a conflict there by keeping the copy being replayed:
either side is a correct index of the caches it saw, the caches themselves are
unioned, and the next build of either workflow rebuilds from the union.

`build_qp_index()` deletes any JSON under `questions/` that the build did not
write, so a bucket that lost its last neighbour is not served forever.

## Audit

`audit_site.R`, "Questions presented": the page exists and loads `/qp.js`;
`/qp.js` exists; each Term file parses and holds the count `meta.json` gives;
**every indexed question has a case page** (a result is a link); every
neighbour in `like/` and `near/` is itself indexed; each `near/` file holds
only dockets from its own bucket; and `like/` and `near/` agree on how many
petitions have neighbours. A site with no `questions/` at all is a WARN, since
that is every checkout before the first build.

## Not done

- **A granted/denied outcome on every result.** The site has no docket →
  disposition map on gh-pages; the funnel classifies outcomes at build time
  from term archives. Publishing one would let the search filter on "denied".
- **Meaning rather than wording.** An embedding index would find the same
  question asked in different words. It would also add a model dependency to a
  build that has none, for a corpus where the same-area agreement of the first
  neighbour is already 0.82 at cosine 0.30.
