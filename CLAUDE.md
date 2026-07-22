# ceRt — Supreme Court Report

An R pipeline that fetches U.S. Supreme Court docket data from supremecourt.gov and
publishes a static analytics site — **supremecourt.report** — from the `gh-pages`
branch. There is no server; every page is pre-rendered HTML committed to gh-pages
and served by GitHub Pages.

## Site sections (all under gh-pages, linked from the landing page)

| section | path | built by |
| --- | --- | --- |
| Daily petitions & applications dashboards | `dashboards/` | `daily.yml` → `build_dashboards.R` |
| Per-case docket pages | `cases/` | `render_dockets_for()` in `R/docket_page.R` — see **[docs/docket-pages.md](docs/docket-pages.md)** |
| Conference reports | `conferences/` | `conferences.yml` → `render_conferences.R` |
| Oral Argument Navigator | `arguments/` | `conferences.yml` → `render_arguments.R` |
| The Cert Funnel (explainer) | `funnel/` | `render_funnel.R` |
| Cert-grant forecast model + methods | `methods.html` | `R/cert_model.R` — see **[docs/cert_model.md](docs/cert_model.md)** |

Every section is built and published by a GitHub Actions workflow. For the full
inventory — each workflow's triggers, whether it updates data and/or the public
site, and which pages it writes — see **[docs/workflows.md](docs/workflows.md)**.

## Architecture

- **Fetch** (`R/scotus_dash_new.R`): per-docket JSON fetch from supremecourt.gov,
  **one request at a time** (Akamai WAF throttles bursty clients). No persistent
  per-docket cache. Docket buckets per term: paid `NN-1..`, IFP `NN-5001..`,
  applications `NNA###` (note the "A", not a dash). The daily fetches only the
  trailing ~50 dockets of each bucket; full-term fetches (`fetch_term.R`) hit
  thousands and run one term per runner for fresh IPs. `fetch_is_degraded()` refuses
  to publish a fetch that lost >10% of dockets to throttling.
- **Render**: R builds static HTML/`gt` tables. Docket pages are incremental via a
  content-hash manifest keyed by a `PAGE_TEMPLATE_VERSION` constant — bump it to
  force a full re-render after a markup/logic change (render-only, no re-fetch).
- **Publish**: workflows check out gh-pages into `./site`, render, and push with a
  **rebase-and-retry** loop (daily/conferences/backfill can race). Each publish
  re-asserts `CNAME` = `supremecourt.report`.

## Rolling a template change across the whole back-catalog

The daily only rebuilds the current term's ~150 recent pages. To apply a
`PAGE_TEMPLATE_VERSION` bump to the full ~55k-page back-catalog:

- **`rerender-dockets.yml`** — full re-render. Fetch mode = per-term matrix →
  single render+commit. `reuse_from_runs` input = render-only from cached snapshot
  artifacts of prior runs (skip the ~3 h re-scrape; ~20 min).
- **`fill-throttled-dockets.yml`** — mop up dockets a full fetch lost to throttling
  (they keep a bare-`<li>` "pre-template" page). Fetches only each term's stale
  dockets. Re-dispatch until zero remain.

Details and gotchas (amicus coloring, stale-page detection, application dockets):
**[docs/docket-pages.md](docs/docket-pages.md)**.

## Conventions

- Commit/push only when asked; branch off `main` for feature work. gh-pages is
  written only by CI workflows, never by hand.
- Workflows are dispatched with `gh workflow run <file> --ref main [-f k=v]` and are
  designed to be **re-dispatchable / resumable** (incremental manifest).
- Trigger workflows from the GitHub side (`gh`), not by editing gh-pages locally.
