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
| **`conferences.yml`** | Weekly `0 6 * * 1` (**Mon 06:00 UTC**); auto-skips Jul–Aug recess | **Yes** — full-term fetch | **Yes** — conferences, arguments, funnel |

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
- **Recess skip:** `plan_terms.R` sets `skip=true` for **scheduled** runs in July
  and August — a manual dispatch still runs in summer.
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
| `CNAME` | re-asserted by **every** publishing job |

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
