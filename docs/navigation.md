# Sitewide navigation

Design system (components, states, mobile, foundations): the **Supreme Court Report**
project on [claude.ai/design](https://claude.ai/design).

## The problem

The site has no navigation. Not "thin" navigation — none. As of the gh-pages tip on
2026-07-28, the link graph runs one way and terminates:

| page type | count | internal links out |
| --- | --- | --- |
| `cases/*.html` | 55,167 | **0** |
| `methods.html` | 1 | **0** |
| `dashboards/index.html` | 1 | 1, mislabelled |
| `conferences/index.html` | 1 | 1, **wrong label + wrong target** |
| `arguments/index.html` | 1 | 1, **wrong label + wrong target** |
| `funnel/index.html` | 1 | 3 (hand-rolled footer) |
| dated leaf pages | ~299 | 6–62, all downward into `cases/` |

Every path through the site ends at a case page, and a case page's only anchors point
off-site to supremecourt.gov. 99.5% of the published site is a terminal node.

That is also the entire search surface. A reader arriving at `cases/24-1122.html` from
Google — which is how nearly everyone will arrive, because it is 55,167 of our 55,470
indexed pages — cannot reach the funnel explainer, the forecast model, the conference
reports, or the landing page, except by editing the URL bar.

The GA4 numbers corroborate it exactly. Over the 30 days to 2026-07-25: `/` 33 views,
`/funnel/` 15, `/methods.html` 13, top dated pages 7–9 each. Those are all top-entry
readers walking down. Case-page traffic exists — 22 distinct case pages drew views in
the window ending 07-27 — and none of it flows anywhere, because there is nowhere for
it to flow.

Two smaller defects found in the same audit:

- **`conferences/index.html` and `arguments/index.html` both ship the string
  `← All dashboards`, pointing at `/`.** Wrong label (the reader is in Conferences or
  Arguments) and wrong target (the site root is not the dashboards index). The string
  was copy-pasted from `dashboards/index.html`, which is the only place it reads
  correctly — the predictable outcome of three pages hand-rolling the same link with no
  shared component to inherit from.
- **`cases/` has no `index.html`.** Every other section has one. The directory holds
  55,167 files and returns a 404.

## Target information architecture

Three levels, no deeper. The site has no fourth level and a crumb trail that can grow
is one that eventually wraps.

```
/                                    landing — the five sections
├── /dashboards/     index → dash_YYYY-MM-DD.html      (dated leaf)
├── /conferences/    index → conf_YYYY-MM-DD.html      (dated leaf)
├── /arguments/      index → arg_YYYY.html             (dated leaf)
├── /cases/          index → NN-NNNN.html              (case page)  ← index is new
├── /funnel/         single page
└── /methods.html    single page
```

`cases/` gets an index because the breadcrumb needs a middle crumb to point at, and
because a browsable case list is the site's largest missing surface in its own right.
Scope it small: current term, split by the three docket buckets (paid, IFP,
applications), most recent first, with links to prior terms. It does not need to be
55,167 rows — it needs to not be a 404.

## Per-page-type specification

| page type | masthead | breadcrumb | prev/next | footer nav |
| --- | --- | --- | --- | --- |
| landing | wordmark only | — | — | — |
| section index | ✓ active | — | — | — |
| dated leaf | ✓ active | ✓ | ✓ by date | — |
| case page | ✓ | ✓ | **no** | ✓ |
| funnel, methods | ✓ active | — | — | — |

The landing page carries no section links in its masthead — it *is* the section list,
and repeating it 200px above itself is noise.

**Case pages get no prev/next.** Docket numbers are assigned in filing order, so
24-1121 and 24-1123 have nothing to do with 24-1122 — different parties, courts,
questions. The control would be cheap to build and would offer two doors that lead
nowhere the reader wanted, dressed as continuity. The sequence exists in the data but
not in the world. The meaningful adjacency for a case is its conference cohort; see
*Deferred* below.

## Components

Eight cards in the design system. Details, states and rationale live there; this is the
inventory.

- **Masthead** — wordmark + 5 one-word section links, `aria-current` on the active one,
  reuses the existing `.brule` double-rule as its divider. Reads `--nav-max` so one
  component serves both the 40rem index container and the 54rem case container.
- **Breadcrumb** — `Home › Cases › No. 24-1122`, plus a `BreadcrumbList` JSON-LD block.
  On a site whose search surface is 55k case pages, the JSON-LD is the highest-leverage
  200 bytes on the page and reuses the three strings the visible crumb already has.
- **Case footer** — keeps the supremecourt.gov link primary (it is what a practitioner
  came for), adds a labelled browse row beneath it.
- **Section nav** — section index header; the `← All dashboards` back-links are
  *deleted*, not corrected, because the masthead now says the same thing in one place.
- **Prev / next** — dated leaves only. Labels name the destination, never bare
  "Previous". Injected by `patch_prev_next()` as a **post-pass** after the section
  index is rebuilt, not at render time: no generator sees the complete sequence
  (the daily renders only the dates in the current fetch window,
  `render_conferences.R` only conferences on/after a cutoff), so computing
  neighbours during render would freeze each page's "next" at whatever existed the
  day it was written — every batch's newest page would permanently claim to be the
  most recent one. Each page emits an empty `<!--PNAV--><!--/PNAV-->` slot and the
  post-pass splices into it by string position, which makes it idempotent and
  self-healing. This is why dashboards get prev/next after all, rather than being
  the documented exception the first draft of this plan assumed.
- **Mobile patterns** — the whole stack at 390px, CSS only.
- **Type scale**, **Palette** — foundations.

### No JavaScript

The mobile nav is a horizontally scrolling strip under a mask-fade, not a drawer. A
drawer needs an open/close script, `aria-expanded`, a focus trap, `Escape` handling,
scroll locking and a resize reset — shipped to 55,167 static pages, and the site's
first render-blocking interactive element on exactly the pages Google measures for Core
Web Vitals. The strip costs six declarations, degrades to a wrapped list where
`mask-image` is unsupported, and leaves the two highest-traffic sections visible with no
tap. A drawer is what you reach for when the nav is too big to show; this nav is five
short words.

## Accessibility corrections to ride along

Measured against `--paper` `#f3ecdd`. Two shipping tokens fail WCAG AA (4.5:1) for
normal-size text:

| token | hex | ratio | used for | proposed | ratio |
| --- | --- | --- | --- | --- | --- |
| `--sienna` | `#b5651d` | **3.69** | every PDF link on every case page (`.tl-docs a`, 0.85rem) | `#a0591a` | 4.53 |
| `--faint` | `#8a8271` | **3.24** | timeline dates, refresh stamp, index counts | `#716b5d` | 4.50 |

`--sienna` is the more serious: it colours the links to the actual briefs, which for a
practitioner are the reason the page exists. Both replacements are the *lightest* value
that clears 4.5:1 — keeping 89% and 82% of the original lightness respectively, so
neither perceptibly changes the palette.

Also the token split, which turned out wider than the first audit showed —
**three** stylesheets, not two. `DOCKET_CSS` (`R/docket_page.R`) and `SCR_CSS`
(`R/interactive_theme.R`) both spelled the accent `--ox` and the muted ink
`--soft`; `INDEX_CSS` (`R/page_style.R`) spelled the same two values `--oxblood`
and `--ink-soft`. All three are now on the longer, self-describing pair.

Two `docs/` generators carried the palette too. `make_methods_note.R` writes
`docs/cert_model_methods.html`, which is copied to `site/methods.html` — so its
failing `--faint`/`--sienna` were shipping, at 8.5–9pt. Corrected.
`make_model_reference.R` output is not published; corrected anyway, so the
divergence has nowhere to creep back from.

`NAV_CSS` gives every `var()` a literal fallback (`var(--oxblood, var(--ox,
#8a2b2b))`), so a shared component renders correctly under either spelling. That is
what makes the migration safe to do incrementally rather than all at once.

All three are render-only one-line edits, and a `PAGE_TEMPLATE_VERSION` bump is already
required for the navigation work — so they cost nothing extra if done in the same pass.
That is the entire argument for doing them now rather than later.

## Rollout

Ordered so nothing ships pointing at a 404.

1. **`cases/` index** — new build step in `build_dashboards.R`, published by the next
   `daily.yml` run. Must land before or with the breadcrumb. *(No template bump.)*
2. **Palette + token normalisation** — `docket_page.R`, `interactive_theme.R`,
   `page_style.R`, `cert_funnel.R`, plus the two `docs/` generators. Fold into
   step 3's bump.
3. **Case-page nav** — masthead, breadcrumb, footer into `R/docket_page.R`; drop the
   now-duplicated docket number from `.kicker`, keeping its off-site link. Bump
   `PAGE_TEMPLATE_VERSION` v14 → v15, then `rerender-dockets.yml` with
   `reuse_from_runs` for a render-only pass from cached snapshots (~20 min, no
   re-scrape). Then `fill-throttled-dockets.yml` until zero remain.
4. **Section indexes, landing, funnel, methods** — masthead into `page_style.R`; delete
   the three back-links. Published by `daily.yml` / `conferences.yml`.
5. **Prev/next** on dated leaves.

Step 3 is the only expensive one and the only one that touches the back-catalogue. Do
2 and 3 in a single bump; a second full re-render to fix a hex value would be a poor
trade.

### Watch out

- The daily aborts before rendering when `fetch_is_degraded()` trips — it did on
  2026-07-27 04:11 and 2026-07-29 03:47, both the 00:30 UTC cron, losing ~24% of
  dockets to throttling. A degraded run publishes nothing, so don't read an unchanged
  landing page as a broken template.
- Verify step 3 on a handful of pages before dispatching the full re-render: an
  application docket (`NNA###`), a granted case, and a pre-template page that
  `fill-throttled-dockets.yml` has not yet reached.

## Deferred

**Contextual links on case pages** — "distributed for the conference of June 26, 2026",
linking to that conference report, plus the argument sitting for granted cases. This is
the strongest link the page could carry, and the edge already exists in the data:
`conf_2026-06-29.html` links out to 32 case pages. Inverting that index is cheap in
principle but needs conference membership threaded into `render_dockets_for()`, so it is
out of scope for this pass. It is the difference between a set of exits and an actual
graph — worth doing next.
