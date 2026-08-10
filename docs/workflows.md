# Workflows (`.github/workflows/`)

Every page on **supremecourt.report** is pre-rendered and committed to the
`gh-pages` branch by a GitHub Actions workflow — there is no server. This file
inventories those workflows: what triggers each one, whether it **updates data**,
whether it **updates the public site**, and exactly **which pages** it owns.

## Reading the two columns: "data" vs. "public pages"

"Updates data" and "updates public pages" are independent, and *several jobs do
one without the other*. Keep three destinations distinct:

| kind of write | where it lands | public? |
| --- | --- | --- |
| Rendered HTML / site caches | **`gh-pages`** branch (`./site`) | **Yes** — this is the live site |
| Model inputs / refresh layers | **`main`** branch (`data-raw/*.rds`, `data-raw/*.json`) | No — consumed by later renders |
| Per-term fetch snapshots | **ephemeral Actions artifacts** (`cases-<term>.rds`, 3–5 day retention) | No — transient build inputs, never committed |

So "fetches from supremecourt.gov" ≠ "changes the site." A QP backfill commits a
JSON *cache* to `gh-pages` but renders no HTML; an enrichment job commits training
data to `main`; a probe writes nothing at all. Only a job that renders HTML into
`gh-pages` changes what a visitor sees — and even then, only on its next run.

## Scheduled runs — the only truly automated ones

Two workflows run on a cron with no human involved. Together they cover the whole
site, partitioned so they never fight over the same paths.

| workflow | schedule (cron is **UTC**) | data | public pages |
| --- | --- | --- | --- |
| **`daily.yml`** | 3×/day: `30 0`, `0 18`, `0 22` (00:30 / 18:00 / 22:00 UTC — the ET-anchored two ≈ 2pm & 6pm ET) | **Yes** — incremental fetch | **Yes** — dashboards, recent cases, landing |
| **`conferences.yml`** | Weekly `0 6 * * 1` (**Mon 06:00 UTC**), year-round | **Yes** — full-term fetch | **Yes** — conferences, arguments, funnel |

### `daily.yml`

- **Data:** live *incremental* fetch (`get_scotus_update`) of the **current term
  only** — the trailing ~50 dockets of each bucket (paid / IFP / applications).
  Fetched cases live in memory for the render; **no `.rds` is committed**. It does
  persist on-site caches to `gh-pages`: `cases/.manifest.json` (content-hash
  manifest, merged), `cases/search.json` (docket→caption index), and
  `dashboards/petition_signals_cache.json` (Rule 10 signals, capped
  `PET_SIG_MAX_NEW=400`/run). Loads the cert model read-only; **no retrain**.
- **Public pages:** `dashboards/` (per-day dashboards + section index),
  **`cases/<docket>.html`** for current-term dockets just fetched (incremental),
  the landing **`index.html`**, `methods.html` (copied from `docs/`),
  `analytics.js`, and `CNAME`. **Does not** touch `conferences/`, `arguments/`, or
  `funnel/` — those are preserved from the checkout and merely linked.
- **Also the site-wide syndication files:** `feed.xml`, `grants.xml`,
  `sitemap.xml` + `sitemap-*.xml`, and `robots.txt` (see
  [Feeds and sitemaps](#feeds-and-sitemaps) below). These *enumerate* the whole
  site including sections the daily does not render, which is fine because they
  read the gh-pages checkout, not the render output.
- **Optional secrets:** `GA4_PROPERTY_ID` (numeric, *not* the `G-…` measurement
  id) and `GA4_SA_KEY` (service-account JSON with Viewer on that property) add
  the landing page's **Most-Read Cases** panel — top 5 `cases/` pages by views
  over the trailing 30 days, read from GA4 (`R/site_analytics.R`). Unset,
  failing, or credential-expired, the panel is omitted and the build is
  otherwise unchanged; it never fails the daily.
  A **publishing floor** gates it: each entry needs ≥3 distinct readers and ≥5
  views, and ≥3 entries must clear that before anything renders (fewer than 5
  qualifying entries renders fewer than 5, not nothing). Below the floor the
  ordering would be produced by the docket-number tiebreak rather than by
  readers. Suppression is logged with the counts that failed it, so a dark panel
  is always distinguishable from a broken one.
- **Guard:** `fetch_is_degraded(tol=0.1)` — if >10% of dockets were lost to
  throttling it `quit(0)`s and publishes nothing (site unchanged).
- **Scope caveat:** only the current term's ~150 most-recent pages. A
  `PAGE_TEMPLATE_VERSION` bump does **not** reach the back-catalog from here.

### `conferences.yml`

- **Data:** a per-term matrix runs `fetch_term.R` → `get_scotus_term(year)` for a
  **full-term** scrape (current sitting's term + the prior year), each term on its
  own runner/IP, saved to ephemeral `cases-<term>.rds` artifacts (**not
  committed**). The publish job also incrementally fetches Question-Presented PDFs
  into caches committed to `gh-pages`: `conferences/qp_cache.json` (cap
  `QP_MAX_NEW`, default 600) and `arguments/qp_cache.json` (cap 200). **No retrain.**
- **Public pages:** `conferences/` (per-conference reports + rebuilt index),
  **`funnel/index.html`**, `arguments/` (navigator index + per-term `arg_*.html`),
  and **`cases/<docket>.html`** for the conference/argument cases touched
  (incremental). Re-asserts `CNAME`. **Does not** touch `dashboards/`,
  `methods.html`, or the landing page.
- **No recess skip.** It ran weekly in-season only until 2026-08-06, on the
  assumption nothing happens over the summer. Petitions are distributed to the
  September long conference throughout it: the 2026-09-28 page carried **452
  cases** while the skip was still in force, and it was only being refreshed by
  hand. Both terms are fetched even in summer, because that conference is
  stocked with prior-term petitions (25-198, 25-153, 25-238 among them), so
  rendering it without OT25 would render it wrong.
- **Guard:** `fetch_term.R` `quit(1)`s a term that lost >10% to throttling, so a
  partial term never publishes. `funnel`/`arguments` renders are
  `continue-on-error` and can't block the conference publish.

## On-demand maintenance runs — dispatch only, **not** automated

None of these fire on a schedule. Trigger with `gh workflow run <file> --ref main
[-f k=v]`; all are incremental / re-dispatchable / resumable unless noted.

| workflow | data — where it lands | public pages — which |
| --- | --- | --- |
| **`rerender-dockets.yml`** | Fetch mode: full-term → ephemeral `cases-*.rds`. `reuse_from_runs` mode: **no fetch** | **Yes → `cases/` only** — full back-catalog re-render |
| **`fill-throttled-dockets.yml`** | Targeted re-fetch of only *stale* dockets → ephemeral `cases-*.rds` | **Yes → `cases/` only** — just the stale pages |
| **`enrich-petitions.yml`** | Petition-PDF Rule 10 signals → commits `data-raw/petition_signals.json` to **`main`** | **No** |
| **`backfill-qp.yml`** | QP PDFs (argued grants) → commits `conferences/qp_cache.json` **cache** to `gh-pages` | **No HTML** |
| **`backfill-qp-all.yml`** | QP PDFs (all paid petitions) → same `conferences/qp_cache.json` cache | **No HTML** |
| **`refetch-argued.yml`** | Re-fetch ~500 granted OT17–24 dockets → commits `data-raw/arg_refresh.rds` to **`main`** | **No** |
| **`probe-scotus.yml`** | **No** — read-only WAF/throttle diagnostic; logs HTTP status codes | **No** |

### Back-catalog docket renders — `rerender-dockets.yml`, `fill-throttled-dockets.yml`

The two workflows that reach the `cases/` back-catalog the daily never rebuilds.
Use them to roll a `PAGE_TEMPLATE_VERSION` bump across all ~55k pages. Both write
**`cases/` only** (`cases/<docket>.html`, `style.css`, `.manifest.json`,
`search.json`, `CNAME`), split into a per-term fetch matrix (fresh IPs) + a
**single** render/commit job (one commit avoids manifest/`search.json` races), and
share the `rerender-dockets` concurrency lane so they never race each other. See
**[docket-pages.md](docket-pages.md)** for the full rollout procedure.

- `rerender-dockets.yml` — full re-render. `reuse_from_runs=<run-ids>` skips the
  ~3 h re-scrape and renders from prior runs' cached snapshots (~20 min).
- `fill-throttled-dockets.yml` — mop-up: scans the published site for stale
  (bare-`<li>`, pre-template) pages and re-fetches **only those** docket numbers.
  Re-dispatch until every term reports zero stale pages.

### Data-only jobs (commit data, render nothing)

These feed later renders; **nothing goes public until a rendering job runs.**

- `enrich-petitions.yml` — parses each paid, decided petition's PDF for Rule 10
  signals (dissent below / circuit split) and merges them into
  `data-raw/petition_signals.json` on **`main`**, the enrichment layer the cert
  model reads at train time. One term per runner; `max_new` caps PDFs/run.
- `backfill-qp.yml` / `backfill-qp-all.yml` — extract the Question Presented from
  petition PDFs into the shared `conferences/qp_cache.json` on `gh-pages`. The
  first covers argued grants (from `data-raw/arg_refresh.rds`); the second covers
  **all** paid petitions per term (from `data-raw/ot_<term>.rds`). Docket and
  argument pages surface these QPs only on their **next** render.
- `refetch-argued.yml` — re-fetches the ~500 granted OT17–24 dockets so their
  decisions/opinions are current, saving `data-raw/arg_refresh.rds` to **`main`**
  (the refresh layer `render_arguments.R` prefers over stale snapshots).
  ⚠️ **No `fetch_is_degraded` guard**, and all ~500 requests run on a single
  runner IP — a heavily throttled run can commit a *partial* refresh layer. It is
  the one fetch job without the fleet's throttle-abort discipline.

### Diagnostic — `probe-scotus.yml`

A throwaway probe of whether supremecourt.gov's Akamai WAF throttles a runner IP,
measuring HTTP status codes sequentially and at 4/8-way concurrency. Writes
nothing — no data, no pages. Run it and read the job log; it explains the
sequential / one-term-per-runner fetch strategy the rest of the fleet relies on.

## Who owns which `gh-pages` path

No two workflows write the same site section, which is why concurrent pushes
rebase cleanly.

| site path | written by |
| --- | --- |
| `dashboards/`, landing `index.html`, `methods.html`, `analytics.js` | `daily.yml` |
| `conferences/` (+ `qp_cache.json`) | `conferences.yml`; cache also by `backfill-qp*.yml` |
| `arguments/` (+ `qp_cache.json`) | `conferences.yml` |
| `funnel/` | `conferences.yml` |
| `cases/` (+ `.manifest.json`, `search.json`, `style.css`) | `daily.yml` (current term) · `conferences.yml` (touched cases) · `rerender-dockets.yml` / `fill-throttled-dockets.yml` (back-catalog) |
| `feed.xml`, `grants.xml`, `sitemap*.xml`, `robots.txt` | `daily.yml` |
| `cases/grants.json` (grants cache) | `conferences.yml` (full-term, the real source) · `daily.yml` (only what is inside its trailing fetch window) |
| `cases/pending.json` (live-docket cache) | `conferences.yml` only |
| `cases/forecasts.json` (prospective forecast log) | `conferences.yml` only |

## Naming the live stragglers instead of widening the Term window

`conferences.yml` range-fetches the current and prior Term. Measured over
OT2017–OT2024, that covers 99.29% of all relists — but the misses are the
long-held, heavily-relisted cases, which is exactly what a relist product is
about. **19-333 (Arlene's Flowers)** was relisted 12 times; 8 of those happened in
a Term the window never loads.

Widening to a third Term costs ~5,000 requests. The set of dockets that are still
undisposed *and* older than the window has a **median size of 8, measured maximum
13**. So the `fetch-pending` job names them instead:

- `cases/pending.json` — docket → `{last_event, n_relists, first_dist}`, written
  by `render_conferences.R` from everything that run classified.
- `.github/scripts/fetch_pending.R` — reads the cache, keeps the dockets from
  Terms this run will *not* range-fetch, and fetches exactly those into
  `cases-pending.rds`.

Why the split is exact rather than approximate: **you can only target a docket you
already know exists.** New petitions arrive with fresh numbers in the current
Term's bucket, and `binary_search_max()` is what discovers them — so the current
and prior Terms must stay range fetches. Older Terms never gain new dockets.

Four things here are load-bearing:

- **The cache is rewritten wholesale, not merged.** A docket disposed of since the
  last run must *disappear*, or the fetch list only grows. This is why
  `pending.json` is deliberately **not** in `publish_site.sh`'s `DERIVED` list:
  every cache there is append-only per key, so a union is right for them and
  would, for this one, resurrect precisely the keys the render step just retired.
- **A docket the run did not see is carried forward**, not retired. Otherwise a
  fetch that lost a docket to throttling would silently drop a live case.
- **Applications are excluded**, as `classify_petitions()` already does. Their
  dispositions are not in the grant/deny grammar, so the classifier calls nearly
  all of them pending forever — seeding OT17–20 without the filter produced a
  496-docket list instead of 8, almost all `18A####`.
- **`cases-pending.rds` is named so it does not match `^cases-\d{2}\.rds$`.**
  `render_funnel.R` selects on that pattern and must not have its live-Term
  statistics perturbed by a handful of old stragglers.
  `render_conferences.R` (pattern `\.rds$`) and `render_arguments.R` (explicitly
  widened) both do pick it up — the latter because a named straggler can since
  have been granted, and would otherwise be missing from the Navigator alone.

`publish` gates on `always() && needs.fetch.result == 'success'`: it waits for
`fetch-pending` so the artifact is downloadable, but a throttled straggler cannot
block a clean Term fetch. `always()` is what waives the needs-success rule — a
plain `if:` does not.
| `CNAME` | re-asserted by **every** publishing job |

## Feeds and sitemaps

Written by `daily.yml` (`R/feeds.R`, called at the end of `build_dashboards.R`),
and owned by it alone even though they index the whole site — a feed is cheap to
regenerate and the daily is the job that runs most often.

| file | contents |
| --- | --- |
| `feed.xml` | Atom. Grants, conference reports and daily dashboards, 50 most recent |
| `grants.xml` | Atom. Certiorari grants only |
| `sitemap.xml` | A sitemap **index**, not a urlset |
| `sitemap-pages.xml` | Sections, dated leaves, `cases/ot*.html` browse pages |
| `sitemap-cases-ot{NN}.xml` | One per term, docket pages only |
| `robots.txt` | Points crawlers at `sitemap.xml`; re-asserted every run like `CNAME` |

**The grants feed is not built from the daily's fetch, and cannot be.**
`get_scotus_update()` pulls `max(hi - 50, lo):hi` — the trailing ~51 dockets of
each bucket. A petition is granted months after it is docketed, by which time its
number is far outside that window, so a grant is almost never visible to the
daily. Shipped that way in #49, `grants.xml` was never written at all.

Grants are instead accumulated in **`cases/grants.json`**, a docket-keyed cache
that any workflow holding full-term data contributes to — in practice
`conferences.yml`, whose `combined` is the current + prior term. The daily reads
the cache to build both feeds, and also contributes whatever it can see. The
cache is append-only per key and is listed in `publish_site.sh`'s `DERIVED`, so
two workflows publishing concurrently union their grants rather than clobbering.

Consequence: **a new grant reaches the feed on the next weekly conferences run**,
not the same day. That is a real limitation, not a bug to be fixed by widening the
daily fetch — widening it means thousands of requests three times a day against a
WAF that throttles on requests-per-second-per-IP.

Five things about the feeds are load-bearing:

- **`<updated>` is always an event date, never the build time.** The daily runs
  three times a day. A build-time stamp would re-notify every subscriber three
  times a day forever *and* defeat `publish_site.sh`'s `git diff --cached
  --quiet` short-circuit, adding a gh-pages commit per run containing only a
  timestamp. `audit_site.R` checks that each feed's document-level `<updated>`
  equals its newest entry's, so a later "fix" cannot quietly reintroduce it.
- **The sitemap is an index because `cases/` is past the 50,000-URL cap** (55,327
  dockets at the last audit). A single `sitemap.xml` would be rejected whole
  rather than truncated. The audit fails any child over the cap.
- **Docket URLs carry no `<lastmod>`.** File mtime is wrong — the gh-pages
  checkout rewrites every mtime on every run, so it would claim all 55k pages
  changed today — and `cases/.manifest.json` stores a content hash, not a date.
  Dated leaves (conferences, dashboards) carry their date in the filename and do
  get a real one.

- **No entry may be dated in the future.** Conference reports are published
  *before* the conference they cover — the 2026-09-28 long-conference page existed
  on 2026-08-07 — so an unfiltered feed put its newest entry, and with it the
  feed's own `<updated>`, seven weeks ahead of today. `write_atom_feed()` drops
  future-dated entries and the audit fails on any that survive. The cost is that
  a conference report enters the feed on its conference date rather than when it
  is published; the alternative (stamping it with the publication date) would
  re-stamp it every run, which is the churn above.
- **Autodiscovery is driven by what is on disk, resolved per page.**
  `page_head()` calls `site_feeds_present()` (in `R/page_style.R`), which reads
  `SITE_DIR` — the environment convention every render entry point already uses —
  and emits a `<link rel="alternate">` only for feeds that exist. A feed first
  written at the end of run N is therefore advertised from run N+1.

  This has been wrong twice, in opposite directions, and the second was worse.
  #49 advertised both feeds unconditionally, so every index page linked to a
  `grants.xml` that was never written. #50 made the links conditional on a
  `SITE_FEEDS` global — but only `build_dashboards.R` set it, so `/conferences/`,
  `/arguments/` and `/funnel/`, rendered by a different workflow, advertised
  **nothing**. A dangling link is at least visible; a missing one is not. There is
  now no global to set and therefore none to forget, and `audit_site.R` checks
  that every generated index page advertises every feed that exists.

Feed autodiscovery tags live in `page_head()` (`R/page_style.R`). Docket pages
build their own `<head>` and are unaffected — **the feeds need no
`PAGE_TEMPLATE_VERSION` bump.**

## Shared publish mechanics

Every gh-pages writer checks out `gh-pages` into `./site`, renders, then commits
and pushes with a **5-attempt rebase-and-retry loop** (`git pull --rebase` +
random 3–7s backoff), because daily / conferences / backfill can all push
concurrently. Each publish re-asserts `CNAME = supremecourt.report`. Every job
uses `cancel-in-progress: false` so runs queue rather than get killed mid-publish.
Commits are no-ops when `git diff` is empty. Fetch jobs run **one term per
runner** for fresh IPs and are sequential within a runner (Akamai WAF throttles
bursty clients).

## Relevant files

| workflow | fetches | renders | script(s) |
| --- | --- | --- | --- |
| `daily.yml` | current term (incremental) | dashboards, recent cases, landing | `build_dashboards.R` |
| `conferences.yml` | current + prior term (full) | conferences, arguments, funnel, cases | `fetch_term.R`, `render_conferences.R`, `render_arguments.R`, `render_funnel.R`, `render_dockets_backfill.R` |
| `rerender-dockets.yml` | per-term full (or reuse) | back-catalog cases | `fetch_term.R`, `render_dockets_backfill.R` |
| `fill-throttled-dockets.yml` | stale dockets only | stale cases | `fetch_missing_dockets.R`, `render_dockets_backfill.R` |
| `enrich-petitions.yml` | petition PDFs | — (data → `main`) | `enrich_petition_signals.R`, `combine_petition_signals.R` |
| `backfill-qp.yml` | argued-grant petition PDFs | — (cache → gh-pages) | `backfill_qp.R` |
| `backfill-qp-all.yml` | all paid petition PDFs | — (cache → gh-pages) | `enrich_qp.R`, `combine_qp.R` |
| `refetch-argued.yml` | ~500 granted OT17–24 dockets | — (data → `main`) | `refetch_argued.R` |
| `probe-scotus.yml` | read-only probe | — (nothing) | inline bash/curl |
