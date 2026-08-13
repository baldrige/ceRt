# The Counsel Table (`/counsel/`)

Four leaderboards over petitioner's counsel of record, computed from the
committed term archives. **No fetch, ever** — `data-raw/ot_*.rds` is the only
input, so the page changes when the archives or the classifier change and at no
other time.

| piece | where |
| --- | --- |
| Aggregation + renderer | `R/counsel_table.R` |
| Regenerate the committed stats | `.github/scripts/make_counsel_stats.R` |
| CI render | `.github/scripts/render_counsel.R` |
| Committed summary | `data/counsel_stats.json` |
| Workflows | `conferences.yml` (weekly, self-healing) · `render-counsel.yml` (dispatch) |

## The unit is a case, not a petition

One dispute is often docketed as several petitions — one per patent, per
consolidated appeal, per petitioner — so `counsel_cases()` collapses
`(advocate, caption)` to a single row keeping the strongest outcome and the
highest relist count.

This is 1.3% of petitions and it decides the top of a rate board. Counted as
petitions, the relist leaderboard opened with **10 filed, 10 relisted, 100%** —
eight identical *Paice LLC v. Ford* petitions docketed across three days plus two
identical *KIP CR* ones. Two disputes, each redistributed once, published as an
advocate whose every filing drew a second look. Collapsed, that advocate has
three cases and does not qualify.

Companion petitions do **not** share a docketing date, so the caption alone is the
key.

## Two floors, both measured

| floor | applies to | value |
| --- | --- | --- |
| `COUNSEL_MIN_CASES` | the volume board, and the "qualifying" count | 5 |
| `COUNSEL_MIN_RATE_CASES` | the relist board and both grant boards | 8 |

A rate needs more evidence than a count. At a floor of five, **eleven of the
twenty-five relist rows were the same shape** — 3 relisted of 5 filed, a 60% share
and a 23.1% lower bound, eleven times. A real ranking that told the reader
nothing. At eight the largest tie in the top twenty-five is two rows; ten and
twelve buy nothing further.

The grant boards take the same floor one step removed: a Wilson bound *shrinks* a
small sample, it does not rescue one. At five, 3 grants in 5 outranked 8 in 23.
It costs the government board four rows, including a 6-of-7 that would have led
it.

Rate boards rank on the **Wilson 95% lower bound** and print the raw rate in bold
beside it — the bound is the order, the rate is the claim. Ranked on the raw rate
instead, 3 grants in 10 sits above 22 in 94.

## The government split, and why it is where it is

Grant rates by petitioner, OT2017–2024, case-level:

| petitioner | cases | granted | rate |
| --- | --- | --- | --- |
| The United States | 167 | 84 | **50.3%** |
| A State | 203 | 29 | **14.3%** |
| A private party | 8,230 | 379 | **4.6%** |

A pooled ranking is therefore the Solicitor General's office at the top, which
measures who the client is rather than how the petition was written.

**The split is made at the case, not the advocate.** An advocate-level cut needs a
threshold and there is none to pick: `gov_share` puts 245 of 286 qualifying
advocates at exactly zero and strings the rest out in a gradient thick with
*former* state solicitors general now in private practice. Splitting cases needs
no threshold, and an advocate who did both kinds of work appears on both boards —
which several did.

### `petitioner_gov_side()`: precise, and deliberately not recall-tuned

Measured against hand-checked ground truth: **~100% precision** (1 apparent false
positive in 547 private-bar petitions, and that one is a real government filing),
**~82% recall**. The misses are not exotic — the docket's party field carries bare
officer names (`Merrick B. Garland`), abbreviations (`FCC`, `Att'y Gen.`), source
typos (`Securites and Exchange Commission`, `Homeland Secuirty`) and offices with
no fixed written form (`Office of the United States Trustee`).

Recall is the dangerous direction: every miss moves a government case onto the
**private** board. So `COUNSEL_GOV_MAX_PRIVATE` (0.25) keeps an advocate whose
filings are a quarter or more government off the private board entirely. Split
purely by case, that board opened with Elizabeth Prelogar at 6 grants in 7 — all
seven government filings the grammar missed. Across the top of the board the
government share runs 92%, 44%, then 3%, 2%, 0%: anything from 5% to 40% draws the
same line, and a quarter is the round number inside the gap.

It costs a few real private-practice rows. That is the right direction to err — an
advocate wrongly left off a board is invisible; an advocate wrongly on top of one
is a published claim that is false.

### Adding the caption is a trap, and it is measured

Recall rises 82% → 90% and precision collapses 99.8% → **81%**, because a private
petitioner *suing* the government has the government in their caption: 50 of Raed
Gonzalez's 51 immigration petitions read as federal filings. **The party field
names the petitioner; the caption names both sides.**

## Staleness

`data/counsel_stats.json` is a committed summary of a classifier's output — the
same shape of file as `data/funnel_baselines.json`, which went stale for two weeks
and published a relist count 2.5× too high. The fix that broke it lived inside a
*function body*, so `counsel_stats_fingerprint()` deparses the eleven functions
the file is a product of, not just the tunables, plus the archive md5sums.

Three things act on it, in order of how loudly:

- **`render_counsel.R` recomputes in-process on a mismatch** (~4 min) rather than
  failing. It is `continue-on-error` inside `conferences.yml`, so a hard stop
  would be silent in exactly the case that matters *and* would leave the previous,
  wrong page live.
- **`audit_site.R` FAILs** on a stale committed copy — the page is right either
  way, but a silent 4-minute recompute on every weekly run is a cost nobody sees.
- **`CHECK_ONLY=1 make_counsel_stats.R`** exits 1, for use anywhere a gate is
  wanted.

After any change to the classifier, `counsel_key()`, the party grammar or the
constants:

```
Rscript .github/scripts/make_counsel_stats.R     # ~4 min, prints what moved
```

and commit the result.

## What the page refuses to claim

- An advocate is a **name, not a verified identity**. Rows key on first + last
  token, so `Neal K. Katyal` and `Neal Kumar Katyal` merge — and so would two
  different lawyers who share both names. Every row that merged more than one
  spelling prints them beneath the name; that visible merge is the whole
  mitigation, because **0 of 8,989** archive petitions carry a firm to
  disambiguate on.
- **Petitioner's counsel of record only.** An advocate who argued a case they did
  not petition in does not appear, and respondents' counsel — most of what the
  Supreme Court bar actually does — is invisible here.
- Paid petitions only; self-represented petitioners excluded; pending petitions
  count toward cases filed and relists but never toward a grant rate.
