# Feature backlog

Candidate additions to supremecourt.report, ranked by what the pipeline already
computes versus what would need new machinery. Nothing here is committed work —
this is the list, plus a real scope for the three worth doing first.

The organising principle: **the site derives far more than it publishes.**
`cert_funnel.R` already classifies relists, CVSGs, called-for responses and
outcomes for ~49k petitions; `cert_model.R` already scores four models; the
QP and petition-signal caches already hold parsed text for thousands of
petitions. Most of Tier 1 below is a renderer over values that exist today.

---

## Tier 1 — derived but unpublished

| # | Feature | Where the data already is | What's missing |
| --- | --- | --- | --- |
| 1 | **Relist Tracker** — every currently-relisted petition, relist count, QP, conference forecast | `classify_petition_events()` (`R/cert_funnel.R:163`) returns `n_relists` + `relist_dates` under the audited true-relist grammar | A page, and a decision about the term window (see scope below) |
| 2 | **CVSG tracker** — pending CVSGs, days since invitation, historical SG response time and post-CVSG grant rate | `FUNNEL_PATTERNS$cvsg` (`R/cert_funnel.R:71`) already matches the invitation entry; 124 CVSG'd paid petitions in the corpus, granted at 29% against a ~4% base rate | Response-arrival detection (the SG's brief entry), a page |
| 3 | **The opinion clock** — argued-but-undecided cases ranked by days outstanding, against the historical per-term distribution | `classify_argument()` in `R/argument_nav.R` already resolves argued/decided | A page and the historical lag distribution |
| 4 | **Applications / emergency docket** — pending applications by Circuit Justice, time-to-ruling, outcomes | The `NNA###` bucket is fetched daily; `derive_case_type()` already separates it | Applications are excluded from `classify_petitions()` by design; they need their own outcome grammar |
| 5 | **Long Conference page** — the September conference, ranked by the model | `plan_terms.R` already fetches the prior term specifically so the long conference renders correctly | A dedicated page; the conference report already exists but isn't framed as the annual event it is |

## Tier 2 — new index pages over data held

| # | Feature | Where the data already is | What's missing |
| --- | --- | --- | --- |
| 6 | **The Counsel Table** — filings, relists and grants per advocate | `petitioner_counsel()` + `counsel_key()` over the term archives | Scoped in detail below |
| 7 | **Lower-court pages** — `/courts/CA9.html`: volume, grant rate, pending, relisted, CVSG'd | `petition_features()` already carries `lower` | Court-name normalisation across ten terms of free text |
| 8 | **Term scoreboard** — filings, grants, GVRs, DIGs, and the timing lags across ten terms | `funnel_stats()` (`R/cert_funnel.R:318`) and the `data-raw/ot_*.rds` archives | Charts, and the lag computations |

## Tier 3 — research tooling

| # | Feature | Where the data already is | What's missing |
| --- | --- | --- | --- |
| 9 | **QP full-text search** — search every petition's Question Presented, granted *and* denied | The QP caches (`conferences/qp_cache.json`, ~16 MB) | `search.json` is docket→caption only (`R/docket_page.R:904`). A QP index is much bigger and needs a client-side strategy that isn't "ship 16 MB" |
| 10 | **Bulk data download** — the parsed corpus as CSV/Parquet under `/data/` | Everything; the term snapshots already exist as artifacts | One workflow step and a licence statement |
| 11 | **Per-case JSON** — `/cases/25-170.json` beside the HTML | `render_dockets_for()` holds the whole record at render time | ~55k extra files on gh-pages; needs a size check first |
| 12 | **Similar petitions** — link a case to petitions raising the same question | QP text | An embedding or TF-IDF index, and a way to keep it cheap |

## Tier 4 — credibility and reach

| # | Feature | Where the data already is | What's missing |
| --- | --- | --- | --- |
| 13 | **Live forecast scorecard** — what the site said before the Court acted, scored afterwards | `score_conference()` (`R/cert_model.R:1409`); `model_id` already computed as `digest::digest(models)` (`R/docket_page.R:690`) | A prospective, append-only forecast log. `methods.html` publishes *retrospective* LOTO validation, which is a different claim |
| 14 | **RSS/Atom + sitemap** | Grants and conferences are derivable at build time | Nothing exists today: no feed, no `sitemap.xml`, no `robots.txt` |
| 15 | **Dark mode** | `R/palette.R` is now the only place a colour is written down | A second token set and a `prefers-color-scheme` block. Cheap now; it was a six-`:root` sweep before the consolidation |

---

# Scoped: the first three

Picked for the combination of reader value, honesty, and reuse of machinery that
already exists. Relist Tracker gives readers a reason to return weekly; feeds are
how they learn the site exists; the scorecard is what makes the numbers
trustworthy once they arrive.

---

## 1. Relist Tracker

### Why

Relists are the strongest publicly-visible cert signal, and the tracking of them
is a well-established genre with a devoted readership. What no one else can do is
put a **calibrated probability** next to the relist count. The conference model
already produces exactly that number, and the true-relist grammar in
`cert_funnel.R` is more careful than the naive count everyone else uses — it
excludes reschedules, called-for-response redistributions and CVSG
redistributions, which pooled over OT17–22 would otherwise **overstate relists by
~55%**.

### What ships

`/relists/index.html` — one table, refreshed by the weekly conferences run:

| Column | Source |
| --- | --- |
| Case (docket + caption) | `dkt`, `strip_caption_roles(caption)` |
| Relists | `n_relists` from `classify_petition_events()` |
| Last conference | `max(relist_dates)` |
| Next conference | the distribution whose `conf_date` is still in the future, if any |
| Granted here / ever | `score_conference()` → `p_grant_now`, `p_grant_ever` |
| Held | `hold_signal()` (`R/cert_model.R:485`) |
| QP | the existing `qp_map` `<details>` cell |

Sorted by relist count descending, then by next conference date. Same shading
ramp as the conference reports, so a reader moving between the two pages reads
the same colours as the same thing.

### Data path

Everything needed is computed inside `conference_distributions()`
(`R/conference_dash.R:85`) and then **thrown away**:

```r
.cls = map(events, ~ tryCatch(classify_petition_events(.x), ...)),
outcome      = map_chr(.cls, ...),      # line 104 — kept
outcome_date = ...,                     # line 107 — kept
) |> select(-.cls)                      # line 109 — n_relists and relist_dates die here
```

So step one is widening that `select()` to keep `n_relists` and `relist_dates`.
That is a genuinely one-line change, and it costs nothing: `.cls` is already
being computed for every case.

"Currently relisted" is then:

- `type == "paid"`
- `outcome == "pending"`
- `n_relists >= 1`
- the case's latest distribution has a `conf_date` that has already passed
  (a scheduled-but-unheld conference means it's pending *at* that conference,
  not relisted out of it)

### The correctness problem — measured, and solved

**Resolved. See the measurements below; implemented as the targeted pending
fetch.**

The scheduled weekly run range-fetches only the current and prior term
(`plan_terms.R:22`), so a petition docketed two or more terms back — precisely
what a heavily-relisted or held petition *is* — never enters `combined`.

Measured over OT2017–OT2024 (48,985 petitions; 12,147 paid; 1,723 with ≥1
relist), the term lag of every relist:

| lag | relists | share | cumulative |
| --- | --- | --- | --- |
| 0 (same term) | 2,237 | 75.8% | 75.8% |
| 1 | 694 | 23.5% | 99.3% |
| 2 | 20 | 0.7% | 99.97% |
| 3 | 1 | 0.03% | 100% |

99.29% coverage from the current window sounds fine and is misleading, because a
leaderboard is **ranked** — what matters is whether a case that belongs at the
top is missing entirely:

| relists | cases | lost by 2-term | lost by 3-term |
| --- | --- | --- | --- |
| ≥1 | 1,723 | 9 (0.5%) | 1 (0.06%) |
| ≥5 | 112 | 3 (2.7%) | 1 (0.9%) |
| ≥10 | 24 | **2 (8.3%)** | 1 (4.2%) |

Coverage degrades exactly as relist count rises, because a heavily-relisted case
is by definition one that has been sitting a long time. The worst case is
**19-333 (Arlene's Flowers)** — 12 relists, **8 of them in a term the window
never loads**, so the board would have omitted a nationally-followed case for
eight consecutive conferences while it was live.

### Why the answer is not a third term

Widening to `{T, T-1, T-2}` costs a whole extra term. The set of dockets that are
(a) still undisposed and (b) older than the window, sampled twice a year across
OT2019–OT2024, has a **median size of 8 and a measured maximum of 13**:

```
3rd term (range fetch):    ~5,063 req  ≈ 42 min on its own runner
targeted pending (median):     ~8 req  ≈ 0.1 min
targeted pending (worst):     ~13 req  ≈ 0.1 min
```

All nine cases a two-term window loses were undisposed at the time of their
invisible relists — a relist implies pendency — so every one would have been in
the live set and named.

**The constraint that makes this work:** you can only target a docket you already
know exists. New petitions arrive constantly with fresh numbers in the current
term's bucket, and `binary_search_max()` is what discovers them — so the current
and prior terms must stay range fetches. **Older terms never gain new dockets**,
so every one is already known and targeting is exact, not approximate.

Implemented as `R/pending_dockets.R` + `.github/scripts/fetch_pending.R` + a
`fetch-pending` job in `conferences.yml`, backed by `cases/pending.json`.

Two things about that cache are load-bearing:

- **It is rewritten wholesale, not merged.** A docket disposed of since the last
  run has to *disappear*, or the fetch list only ever grows. This is why
  `pending.json` is deliberately **not** in `publish_site.sh`'s `DERIVED` list —
  every cache there is append-only per key, which is what makes a union the right
  resolution; unioning this one would resurrect exactly the keys the render step
  just retired.
- **Applications are excluded**, as `classify_petitions()` already does. Their
  dispositions aren't in the grant/deny grammar, so the classifier calls almost
  all of them pending forever — seeding OT17–20 without that filter produced a
  496-docket fetch list instead of 8, nearly all of it `18A####`.

### A doc nit this measurement corrects

`render_conferences.R` says the window means *"'Relists' on a recent conference
page is a floor, not a count."* That names the wrong failure.
`classify_petition_events()` reads only a single case's own events, so any case
that is *loaded* gets a complete relist count. What is incomplete is the **case
list** — old dockets are absent from the page altogether. Same root cause,
different thing for a reader to distrust.

### Work items

1. Widen `select(-.cls)` in `conference_dash.R:109` to keep `n_relists`,
   `relist_dates`.
2. `plan_terms.R` — three terms instead of two. Verify `min_conf` still derives
   from `max(term_vec)` (it does, line 28) so the render window doesn't widen.
3. New `relist_watch()` renderer. Model it on `conference_dash()`: same
   `scr_interactive()` theme, same `fc_shade()` ramp, same `case_documents()`
   links.
4. `render_conferences.R` — call it after `conference_index()`, inside the
   existing publish. No new workflow.
5. `SITE_SECTIONS` (`R/site_nav.R:39`) — add `/relists/`. This is what puts it in
   every masthead on the site.
6. `audit_site.R` — `/relists/` joins the nav-target resolution check
   automatically via `SECTIONS`; add `relists/index.html` to the `others` list
   in the emitted-links scan (`audit_site.R:159`) so its masthead is checked too.

### Open questions

- **Include IFP?** The model is paid-only, so IFP rows would carry an em dash in
  the forecast columns. A relisted IFP petition is rare and interesting. Lean
  toward including them with the forecast blank rather than hiding them.
- **Held cases.** `hold_signal()` already distinguishes a hold from a relist.
  These are arguably a separate table — a case held for a pending decision is not
  being reconsidered, it's waiting. Consider two sections on one page.

### Effort

Small. The renderer is the only real work, and it is a near-clone of an existing
one. No new workflow, no new fetch, no template version bump (nothing under
`cases/` changes).

---

## 2. Feeds and sitemap

### Why

Nothing in the repo emits a feed, a `sitemap.xml`, or a `robots.txt` today. For a
site that is 55k static pages and whose audience lives in feed readers and on
social platforms that read link metadata, this is the largest available gain in
reach per line of code. It is also the cheapest thing on this list.

### What ships

| File | Contents |
| --- | --- |
| `/feed.xml` | Atom. Site-wide: new grants, new conference reports, new argument sittings |
| `/grants.xml` | Atom. Newly granted cases only — the feed most people actually want |
| `/sitemap.xml` | A sitemap **index** |
| `/sitemap-ot{NN}.xml` | One child sitemap per term, plus one for section pages |
| `/robots.txt` | `Sitemap:` pointer |

### Data path

> **Shipped in #49, corrected in #50.** The scope below was wrong on one point,
> and it is left here with the correction because the mistake is instructive.

The original plan was to filter the current term's cases to `outcome ==
"granted"` inside `build_dashboards.R`, which already holds `ot`. **That does not
work.** `get_scotus_update()` fetches `max(hi - 50, lo):hi` — the trailing ~51
dockets of each bucket. A petition is granted months after it is docketed, so by
then its number is far outside the window. Built that way, the grants feed was
structurally empty and `grants.xml` was never written at all.

Grants are visible only where a full term is loaded — `render_conferences.R`'s
`combined`. So the grant set accumulates in a docket-keyed `cases/grants.json`
that full-term runs contribute to and the daily reads. Cost: a grant reaches the
feed on the next weekly run, not the same day.

Conference and argument entries come from the existing directory listings —
`conference_index()` and the argument index already walk them.

### Two gotchas that will bite if ignored

**1. `<updated>` must never be `Sys.time()`.**

The daily runs three times a day. A feed whose document-level `<updated>` is the
build time changes on every run, which means:

- every feed reader re-notifies its subscribers three times a day, forever; and
- `publish_site.sh` stops short-circuiting on `git diff --cached --quiet`, so
  gh-pages gains a commit per run whose only content is a timestamp.

Per-entry `<updated>` must be the **event** date (the grant date, the conference
date). Document-level `<updated>` must be `max()` of the entries. Then a run that
found nothing new produces a byte-identical file and publishes nothing — which is
the correct behaviour and is also the test for it.

**2. Sitemaps cap at 50,000 URLs.**

`cases/` is already 55,327 dockets. A single `sitemap.xml` is over the limit and
would be rejected wholesale. Hence the index-plus-children shape: one child per
term keeps every file comfortably under both the URL cap and the 50 MB limit, and
matches how `write_cases_index()` (`R/docket_page.R:791`) already partitions the
back catalogue into `ot{NN}.html` browse pages.

`<lastmod>` per URL should come from the render manifest (`cases/.manifest.json`)
rather than filesystem mtime — a gh-pages checkout rewrites mtimes on every run,
which would tell crawlers all 55k pages changed today.

### Work items

1. `R/feeds.R` — `write_atom_feed(entries, path, ...)` and
   `write_sitemaps(site_dir)`. Base R plus a small amount of XML escaping;
   deliberately no new package dependency.
2. `build_dashboards.R` — call both after the landing page is written. Feeds
   belong to the daily because grants land daily.
3. `robots.txt` — write it from `build_dashboards.R` alongside `analytics.js`,
   the same re-assert-every-run pattern (`build_dashboards.R:25`), so a rebuild
   can't silently drop it.
4. `<link rel="alternate" type="application/atom+xml">` in the page `<head>` —
   `R/page_style.R` is where the shared head is assembled.
5. `audit_site.R` — new checks: the feeds parse as XML; every entry link
   resolves; the sitemap index's children all exist; total sitemap URL count is
   under the cap.

### Open questions

- **Feed granularity.** One combined feed plus grants, or a feed per section? Start
  with two; splitting later is additive and breaks no subscriber.
- **Full text or link-only?** Link plus the QP first sentence is probably the right
  balance — enough to judge relevance in a reader, not so much that nobody clicks.

### Effort

Small, and almost entirely additive — no existing renderer changes behaviour. The
riskiest part is the `<updated>` discipline, which is why it's called out above
rather than left to discovery.

### What the first release actually got wrong

Worth recording, because both defects passed a synthetic test suite and only
showed up against the real site:

1. **The grants feed was wired to data that cannot contain grants** (above). The
   synthetic fixture had a granted case in the tibble, so the test passed; the
   real daily's tibble is 153 trailing dockets and had none.
2. **Future-dated entries.** Conference pages are published before their
   conference, so the feed's newest entry — and its own `<updated>` — was seven
   weeks ahead. No fixture had a future-dated page.
3. Consequence of (1): every index page advertised a `grants.xml` that returned
   404, because autodiscovery was emitted unconditionally.

The lesson for the remaining scopes: **a fixture built from the same assumption
as the code cannot test that assumption.** For Relist Tracker, that means checking
the term-window question against a real multi-term snapshot before trusting a
relist count — not against a tibble assembled to have relists in it.

---

## 3. Live forecast scorecard

### Why

`methods.html` publishes leave-one-term-out validation: each term scored by a
model trained on the other seven, calibration fitted out-of-fold. That is honest
and it is well done — but it is a **retrospective** claim, and a sceptical reader
is right to note that every number on it was produced after the outcomes were
known.

A scorecard is a different and stronger claim: *here is what the site said about
pending petitions, on the record, before the Court acted, and here is how it
did.* Nothing about it is re-derivable after the fact, which is exactly what makes
it worth publishing.

It also closes a real gap. The conference pages score **as of** a past conference
date, but with **today's** fitted coefficients. That is the right choice for those
pages — a reader looking at an archived conference wants the best current estimate
of what was in front of the Justices — but it means no artifact on the site today
records what was actually forecast in advance.

### What ships

**A. The log** — `forecasts.json` on gh-pages. Append-only, keyed
`"<docket>@<conf_date>"`:

```json
{
  "25-198@2026-09-28": {
    "docket": "25-198",
    "conf_date": "2026-09-28",
    "scored_on": "2026-09-21",
    "model_id": "a3f9c1…",
    "p_grant_now": 0.031,
    "p_gvr_now": 0.008,
    "p_grant_ever": 0.094,
    "conf_idx": 1,
    "n_relists": 0,
    "held": false
  }
}
```

The flat, string-keyed object shape is not incidental: it is exactly what
`publish_site.sh`'s union resolution (`jq -s '.[0] * .[1]'`, line ~90) merges
correctly, so the log inherits the concurrent-publish handling that already
protects `qp_cache.json` — provided it is added to the `DERIVED` list at
`publish_site.sh:53`.

**B. The page** — `/scorecard/index.html`:

- N predictions logged, N resolved, N grants realised
- Brier score against the constant-base-rate reference, the same reference
  `brier_ref()` uses in `make_methods_note.R:14`
- A reliability curve on the prospective log alone
- The raw log, downloadable

### The one design constraint that matters

**Only log a conference that has not happened yet.**

The append rule is: for each paid petition distributed to a conference where
`conf_date > Sys.Date()`, if the key is absent, write it. Never overwrite.

Score a *past* conference with today's model and log it, and the scorecard becomes
flattering by construction — the model has seen the outcome through its training
data. Overwrite an existing key on a later run and the log stops being a record of
what was said and becomes a record of what is currently believed. Both failure
modes produce a scorecard that looks better and means nothing, and neither is
visible in the output. The append-only, future-conferences-only rule is the entire
integrity of the feature.

`model_id` is recorded per entry because a refit changes the forecaster.
`digest::digest(models)` is already computed for the render manifest
(`R/docket_page.R:690`), so this is a lookup, not new machinery. When the log
spans refits, the scorecard must either segment by `model_id` or say plainly that
it doesn't.

### Sample size, and refusing to publish noise

At roughly a 2.9% per-conference grant hazard, predictions accumulate fast and
**grants do not** — a term produces thousands of logged rows but only ~60–70
realised grants. A Brier score computed on 200 rows containing 4 grants is noise
presented as a verdict.

So: the page states N-resolved and N-grants above the metrics, and **suppresses
the summary statistics entirely below a stated threshold**, showing the raw log
and a "not yet enough resolved predictions" note instead. A scorecard that
publishes a flattering number off nine grants would do more damage to the site's
credibility than having no scorecard at all.

Expect the page to be honestly empty for its first term. That is the cost of the
claim being real.

### Work items

1. `R/forecast_log.R` — `append_forecasts(log_path, rows)` with the
   never-overwrite rule enforced in code, not by convention.
2. `render_conferences.R` — after the conference loop, append entries for every
   `conf_date > Sys.Date()` in `dist`. The scoring already happens inside
   `conference_dash()`; this needs `score_conference()` called for future
   conferences too, or its results captured and returned rather than only
   rendered.
3. `publish_site.sh:53` — add `forecasts.json` to `DERIVED`.
4. `R/scorecard.R` — join the log to realised outcomes via
   `classify_petition_events()`, compute Brier and the reliability table, render.
5. `SITE_SECTIONS` — add `/scorecard/`, or link it from `methods.html`. Probably
   `methods.html`: it is the same argument, and the masthead is already six items
   wide.
6. `audit_site.R` — a check that no logged key was ever rewritten, by comparing
   against the previous gh-pages revision. This is the check that keeps the
   feature honest over time, and it is the one most likely to be skipped.

### Open questions

- **Which quantity to score?** `p_grant_now` (this conference) and `p_grant_ever`
  are different claims resolving on different clocks — `now` resolves at the
  conference, `ever` only when the petition finally dies. Log both; score
  `p_grant_now` first, since it resolves within a week.
- **Retroactive backfill.** Tempting, and it must be refused. There is no way to
  reconstruct what the site would have said, and a backfilled entry is
  indistinguishable in the file from a real one. Start the log empty.
- **Where does it live long-term?** gh-pages makes it publicly downloadable, which
  is the point. But gh-pages is rebuilt by four workflows; the append-only audit
  check above is what makes that safe.

### Effort

Medium — the largest of the three, and the only one with a delayed payoff. The log
must run for a term before the page says anything. Worth starting *now* for
exactly that reason: every week it isn't logging is a week of evidence that can
never be recovered.

---

## Suggested order

1. **Feeds** — smallest, purely additive, immediate reach.
2. **Forecast log** (part A of the scorecard) — start it accumulating before
   anything else, because it cannot be backfilled. The page can wait a term.
3. **Relist Tracker** — the reader-facing win, and it pairs naturally with the
   three-term fetch window, which improves the conference pages too.
4. **Scorecard page** — once the log has resolved enough predictions to say
   something.

---

## 4. The Counsel Table

### What the corpus supports

Over OT2017–OT2024: **8,989 paid petitions carry a named counsel of record** (74%
of paid petitions; the rest are pro se or unattributed), across **4,561 distinct
advocates**. The distribution is severely long-tailed — 72% filed exactly one
petition, 99 filed ten or more, 8 filed fifty or more. That tail is what makes a
leaderboard viable: it is a small, nameable group, and everyone else is noise.

### Three tables, because they are three different claims

**Filings.** Straight counting, no methodology needed.
Clement 100 · Prelogar 87 · Francisco 61 · Shanmugam 54.

**Relists.** Not redundant with filings, but only if it is published as a
**share**. Raw relist count correlates with filing count at **0.81**, so a count
column mostly re-ranks the same people. The share is a different claim — *this
advocate's petitions get a second look*:

```
john dragseth       10 filed, 10 relisted  100%
mithun mansinghani  57        42            74%
elizabeth prelogar  87        57            66%
```

**Grants.** Publishable only after two corrections; see below.

### Correction 1 — the Solicitor General is a different population

```
US petitioner:  1,175 petitions, 132 granted, 11.2%
private:        7,532 petitions, 361 granted,  4.8%
```

Pooled, the whole top of a grants table is the SG's office — Prelogar, Francisco,
Harris, Wall. That measures the office, not advocacy, and "Elizabeth Prelogar
tops cert-grant success" would be true and misleading in the same breath. Split
into **Office of the Solicitor General** and **private bar**; the private table is
then genuinely informative (Clement 21 · Shanmugam 13 · Blatt 12 · Geyser 7).

Identify by petitioner caption, not by a list of names — a name list goes stale
every administration.

### Correction 2 — never rank on a raw rate

```
min  1 petition:  best rate 100% (1/1)
min  5:           best rate  82% (9/11)
min 20:           best rate  54% (47/87)
```

A raw-rate column is a small-sample generator. Rank by a **Wilson 95% lower
bound** instead, showing the raw rate alongside: it needs no arbitrary cutoff and
it demotes exactly what it should.

```
                    n   g    raw   wilson
elizabeth prelogar 24  18  75.0%   55.1%
sarah harris        6   5  83.3%   43.6%     <- 83% raw, but on six petitions
lisa blatt         29  12  41.4%   25.5%
paul clement       85  21  24.7%   16.8%
```

### Same-name handling

The systematic collisions are gone: `counsel_key()` took the last name token
blindly, so "Robert L. Sirianni Jr." keyed as `robert jr` along with fourteen
other Roberts — 75 petitions, 0 grants, read as one veteran. Fixed, with a
build-time guard, in the suffix change.

What remains is irreducible from docket text: two real people who share a first
and last name. Three mitigations, none of them a solution.

- **Publish the merged variants.** Each row shows the name strings that keyed
  together ("Neal K. Katyal / Neal Kumar Katyal"), so a reader can see the merge
  and judge it. This is the important one.
- **Flag wide lower-court spread.** The median advocate with ≥6 petitions appears
  before 6 distinct lower courts. Far above that is a plausible collision — a
  review flag, never an automatic split.
- **Firm matching works only forward.** **0 of 8,989** archive petitions carry a
  firm column; only the live JSON pipeline has one. It can disambiguate new
  filings and can never clean the back catalogue.

### What ships

`/counsel/index.html` — one page, three tables, minimum **5 petitions** to appear
anywhere. Per-advocate detail pages are deliberately out of scope: 4,561 pages
for a tail that is 72% single filings is a lot of surface for very little.

The footer must say plainly that an advocate here is a **name, not a verified
identity**, and that this counts **petitioner's counsel of record only** — an
advocate who argues a case they did not petition in does not appear at all.

### Where it is computed

From `data-raw/ot_*.rds`, which are committed, so **no fetch**. Aggregation costs
~4 minutes over ~49k petitions, which is too slow for every weekly render — so it
follows the funnel-baselines pattern: a committed `data/counsel_stats.json` with a
**fingerprint of the classifier plus the archives**, and a renderer that
recomputes in-process when the fingerprint does not match. That pattern already
exists, and it exists because the funnel baselines went stale for two weeks and
published a number 2.5× wrong.
