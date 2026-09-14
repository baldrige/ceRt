# Case search

The search box on the landing page, the `/cases/` index and the 404. One
input, one lazy-loaded index, one static matcher: `/search.js` at the site
root, copied by the daily beside `analytics.js`. The page sets
`window.SCR_SEARCH = {json, prefix}` before it loads — where the index is and
what precedes `<docket>.html` in a result link, both relative to that page
(`search_script()` in `R/page_style.R` emits exactly that, and getting the
prefix wrong is a box that silently never finds anything).

## The index

`cases/search.json`, one object, docket → caption, ~56,000 entries, 3.6 MB
(gzipped ~1 MB by Pages). Fetched the first time the box takes focus, never on
page load. Turned into normalised tokens in the browser once, ~700 ms, so the
JSON stays exactly what the docket pages already write.

## What the matcher does

1. **Normalise** both sides: lower-case, strip diacritics (NFD, drop U+0300–036F)
   and punctuation; `v.` / `vs` / `versus` become a separator; `&` becomes
   "and". Long forms the captions use gain the short form readers type as an
   extra token — `Department` also matches `Dept`, `United States` also
   matches `US` / `U.S.`, and so on for Attorney, General, Board,
   Commissioner, Secretary, Corporation, Company, Association, National,
   International, Incorporated.
2. **Docket-shaped queries** (`24-1046`, `24 1046`, `241046`, `No. 24-1046`,
   `25A312`) match dockets by their characters alone: exact first, then prefix
   (`24-10`).
3. **Token prefix matching.** Every query token must prefix-match some
   caption token, in any order. Noise words (`et al`, `inc`, `the`, `of`, …)
   count only when they are all the reader typed. The most selective token is
   tested first so a caption that misses it costs one `indexOf`.
4. **Typo tolerance**, only when the exact pass returns fewer than ten hits:
   a token of five letters or more may match a caption token within one edit
   (two at eight letters or more, optimal-string-alignment distance, so a
   transposition is one). Neighbours are found among the *distinct* tokens,
   bucketed by first letter and length, and only the records that contain one
   are scored — never the whole index. `Loper Brite` is two edits from
   `Bright` and deliberately does not match; `Loper Brigth` does.
5. **Rank.** Exact docket 100, docket prefix 90; text matches start at 60,
   plus 15 if the caption starts with the query, 10 if the tokens appear in
   order, 5 per whole-token match, minus 12 per typo match, plus a bucket
   weight (paid petition +4, original action +1, IFP 0, application −3).
   Ties break by Term, newest first — the old substring search returned file
   order, oldest docket first, so "Trump" led with 2017.

Results carry the docket, the Term and the caption with matched words in
`<mark>`. Arrow keys move through the list, Enter opens, Escape clears.

## Probes

`node .github/scripts/search_probe.js site/cases/search.json` runs thirty
queries against the real index and reports the rank of the case each should
find and the time taken. A miss or a rank past 3 fails; over 50 ms warns.
On 2026-09-14 (55,981 cases, index built in 712 ms):

| probe | finds | rank | ms |
| --- | --- | --- | --- |
| `24-1046`, `24 1046`, `241046`, `No. 24-1046` | Wolford v. Lopez | 1 | 8–15 |
| `25A312`, `25a312` | Trump v. Cook | 1 | 9–13 |
| `Wolford`, `wolford lopez`, `lopez wolford`, `Wolford v Lopez`, `wolf lop` | Wolford v. Lopez | 1 | 11–17 |
| `Skrmetti`, `Skermetti`, `Skrmeti` | United States v. Skrmetti | 2 (L. W. v. Skrmetti, same Term, is first) | 11–12 |
| `Loper Bright`, `Loper Brigth` | Loper Bright | 1 | 15 |
| `Trump v. Cook`, `trump cook` | Trump v. Cook | 1 | 8–9 |
| `Genalo Black` | Genalo v. Black | 1 | 9 |
| `Carr v. Saul` | Carr v. Saul (19-1442) | 1 | 8 |
| `Bostock` | Bostock, 17-1618 | 1 | 9 |
| `Diaz Colon` | Díaz-Colón v. United States | 1 | 8 |
| `United States v. Skrmetti`, `US v Skrmetti` | 23-477 | 1 | 10–12 |
| `Dept of Commerce` | Relentless v. Department of Commerce | 2 | 9 |
| `FCC` | any | 1 | 9 |
| `xyzzyqq` | nothing | — | 10 |
| `24-` | any OT24 docket | 1 | 13 |

Before the rewrite (substring, file order) seven of these failed: the
reversed parties, the prefixes, every typo, `US`, `Dept`, and the diacritic;
and the one-word queries took 30–90 ms because the typo pass scanned every
caption.

## Placement

`styled_index_page(search_top = TRUE)` puts the box directly under the dek,
above the forecast panels; the landing page asks for it. The `/cases/` index
has no panels, so its box was already first. The docket pages carry no search
box: adding one to the masthead would mean a template bump and a 3.6 MB index
behind a box on 56,000 pages, and is a separate decision.
