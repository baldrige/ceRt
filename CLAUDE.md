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
| The Counsel Table | `counsel/` | `R/counsel_table.R` — see **[docs/counsel-table.md](docs/counsel-table.md)** |
| Cert-grant forecast model + methods | `methods.html` | `R/cert_model.R` — see **[docs/cert_model.md](docs/cert_model.md)** |
| Conference forecast (competing risks) | `conferences/` | `R/cert_model.R` — two published columns, see **[docs/cert_model.md](docs/cert_model.md)** |

Every section is built and published by a GitHub Actions workflow. For the full
inventory — each workflow's triggers, whether it updates data and/or the public
site, and which pages it writes — see **[docs/workflows.md](docs/workflows.md)**.

## Architecture

- **Fetch** (`R/scotus_dash_new.R`): per-docket JSON fetch from supremecourt.gov,
  **one request at a time AND rate-paced** — these are two different things, and
  conflating them cost five degraded runs in four days. Sequential only means
  not concurrent; unpaced, it still sustained ~3 req/s, and Akamai limits on
  requests-per-second-per-IP, not concurrency. `scotus_pace()` holds it to
  `SCOTUS_FETCH_RPS` (default 2) with jitter, shared across the binary search and
  the docket fetch. Do not swap it for `httr2::req_throttle()`: that installs a
  fresh full token bucket on every call, so a per-docket builder silently
  defeats it. No persistent
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

## The cert model: four artifacts, two training frames

`train_cert_model.R` writes `cert_model_{baseline,conference,enhanced,gvr}.rds`
plus `counsel_index.rds`. Two things about it are load-bearing and easy to undo
by accident:

- **Two frames, not one.** The petition-stage model is fitted on the disposition
  corpus (one row per petition); everything served at a conference is fitted on
  the at-risk panel (one row per petition × conference). Fitting the conference
  tier on the disposition corpus is what made it answer "given the Court acts
  today, is this a grant?" while the page claimed otherwise.
- **`counsel_index.rds` is not optional.** The baseline model cannot be scored
  without it, and a missing index would rate every advocate first-time rather
  than error. `load_cert_models()` drops the model instead.

A feature that is constant in training aliases to an `NA` coefficient and then
contributes exactly zero at serve time, silently — this shipped undetected for
months. `fit_cert_model()` now fails on any aliased coefficient; do not relax it.

Findings, measured-and-rejected proposals, and open questions:
**[docs/cert_model_review_2026-07.md](docs/cert_model_review_2026-07.md)**.

## Colour: one source, and an audit check that keeps it that way

**`R/palette.R` is the only place a colour may be written down.** Every `:root`
block, every `gt` palette and every `var(--token, #fallback)` derives from it:

- `palette_root(nav_max, extra)` emits a page's `:root{}`. `nav_max` now defaults
  to **`SITE_NAV_MAX` (54rem) for every page** and no caller passes anything else.
  It used to track each page's own container so the masthead rule landed flush on
  the text column; that worked with five section links and broke at seven —
  measured, the nav needs 654px and a 40rem masthead offers 592px, so every index
  page wrapped. The masthead is now deliberately wider than the 40rem index and
  44rem funnel columns (see **[docs/navigation.md](docs/navigation.md)**).
- `fill_palette()` substitutes `@token@` (hex) and `@token:rgb@` (a bare `r,g,b`
  triplet, for the few rules that need partial alpha) in a stylesheet template.
  `NAV_CSS` is built this way, so its fallbacks cannot disagree with the tokens
  they back up.
- `pal()` **errors** on an unknown token rather than returning `NULL`. A `NULL`
  colour reaching `gt` is a cell that silently loses its shading.
- Data colours — `GRANT_RAMP`/`GRANT_DOMAIN`/`GRANT_NA`, `TYPE_CHIPS`,
  `STATUS_FILL` — are R values, not CSS: `gt` resolves them into inline styles
  where `var()` is unavailable. The grant ramp runs from `--paper` to
  `--accent`, so a recolour carries it along.

Before this, the palette lived in six `:root` blocks and 66 literals, half of
them fallbacks that would have gone on painting the *old* colours wherever a
token was missing — silently, and only on some pages. `audit_site.R` now FAILS
on any six-digit hex outside `palette.R` (`#fff`/`#000` are exempt as structural).

The palette is **Bone & Cochineal**: a plain near-white ground, neutral ink, and
one cold red. Its tightest text pair measures 5.37:1 — the parchment palette it
replaced passed at exactly 4.50:1, with no margin at all.

Two things about it are easy to misread as mistakes:

- **`--accent` and `--link` hold the same value on purpose.** One red, spent on
  editorial emphasis and on outbound document links, which never share a slot on
  a page. They are separate tokens so that giving links their own hue is a
  one-line change rather than a sweep. (They were `--oxblood`/`--sienna` until
  the recolour made those names describe nothing.)
- **`PALETTE_EVENTS` was not recoloured.** The ten docket-entry categories are a
  legend, not a scale, and they still separate on the new ground; their swatches
  carry a `--ink` border so even `--c-white` stays visible.

## Conventions

- Commit/push only when asked; branch off `main` for feature work. gh-pages is
  written only by CI workflows, never by hand.
- Workflows are dispatched with `gh workflow run <file> --ref main [-f k=v]` and are
  designed to be **re-dispatchable / resumable** (incremental manifest).
- Trigger workflows from the GitHub side (`gh`), not by editing gh-pages locally.
