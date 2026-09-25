# Editing the words on the site

Every sentence a reader sees is a hard-coded English string inside an R file — or,
in three places, a JavaScript file. There is no CMS, no content directory, no
template language. To change a word you edit a string, push to `main`, and
dispatch the workflow that rebuilds that page.

This guide is for wordsmithing only — changing what a page *says*, not what it
computes.

---

## 1. Find the sentence

Everything is greppable, but only if you search the right trees. Prose lives in
`R/`, in `.github/scripts/`, in the two generator scripts under `docs/`, and in
two `.js` files at the repo root:

```bash
grep -rn "asked to hear" R .github/scripts docs/*.R search.js justices_network.js
```

Search for a distinctive phrase, not the whole sentence: most prose is assembled
from several string literals joined by `paste()`, so a sentence you see on the
page rarely exists as one line in the source. If that misses, drop to three or
four words. If it still misses, grep for the most unusual single word.

Two things defeat a naive grep:

- **Case.** `.panel h2` is `text-transform:uppercase` (`R/page_style.R:84`), so the
  reader sees RECENT DECISIONS while the literal is `"Recent decisions"`.
- **Composition.** "Reversed and remanded" is never written down; it is
  `"Reversed"` plus `" and remanded"` (`R/site_decisions.R:200`, `:207`). The same
  is true of every disposition label and every forecast sentence.

---

## 2. Where each page's headline and standfirst live

The kicker / headline / standfirst trio at the top of a page is what you'll want
to change most often, so it's worth having the addresses to hand. Line numbers
drift; the phrases don't.

### The front of the site

| Page | Headline | Standfirst | File |
| --- | --- | --- | --- |
| Landing `/` | `Supreme Court Report` | "Every case the Supreme Court is asked to hear…" | `.github/scripts/build_dashboards.R` :454, dek :462–463, kicker :453 |
| Landing section links | the **ten** row labels + their italic notes | — | `.github/scripts/build_dashboards.R` :238–320 |
| Landing — Likeliest grants | `Likeliest grants` | "Paid-docket cases filed in the last %d days…" | `.github/scripts/build_dashboards.R` :438, notes :392–399 and :443–445 |
| Landing — Recent decisions | `Recent decisions` | "The Court's most recent written opinions, argued or not." | heading `R/page_style.R` :832; note `.github/scripts/build_dashboards.R` :425 |
| Landing — Latest orders | `Latest orders` | "What the Court granted, sent back and turned away…" | `R/orders_list.R` :647–648 |
| Landing — Upcoming at the Court | `Upcoming at the Court` | "The next conferences, order lists and argument days…" | heading `R/page_style.R` :733; note `.github/scripts/build_dashboards.R` :413 |
| Landing — Most-Read Cases | `Most-Read Cases` | "Ranked by page views over the %d days ending…" | `.github/scripts/build_dashboards.R` :503, :505–508 |
| About | `Supreme Court Report` | "Quantifying the U.S. Supreme Court's behavior…" | `R/page_style.R` :632, dek :634–636 |
| 404 | `No page at that address` | "The link may be mistyped or out of date…" | `R/page_style.R` :578, dek :584–587 |
| Masthead wordmark | `Supreme Court <em>Report</em>` | — | `R/site_nav.R` :180 |
| Masthead section labels | **nine**: `Docket` `Conferences` `Relists` `Arguments` `Justices` `Counsel` `Funnel` `Model` `About` | — | `R/site_nav.R` :39–59 |
| Social card image | `Supreme Court Report` | "Docket analytics for the Supreme Court of the United States" | `docs/make_og_image.R` :44, :49, :53, :57 |
| Social card alt text | `Supreme Court Report - docket analytics…` | — | `R/site_meta.R` :21 |
| Feed names | `Supreme Court Report` / `Supreme Court Report: Certiorari Grants` | "Certiorari grants, conference reports and daily docket dashboards…" | `R/feeds.R` :472–474, :478–479 |
| Feed link text on index pages | `All updates` / `Certiorari grants` | `Follow by feed` | `R/page_style.R` :424, :426 |

### The dockets

| Page | Headline | Standfirst | File |
| --- | --- | --- | --- |
| Dashboards index | `The Daily Docket` | "Every petition and application, the day it arrives." | `R/scotus_dash_new.R` :637–638 |
| A day's dashboard | `The Daily Docket` | "Petitions and applications docketed **{date}** — sortable…" | `R/scotus_dash_new.R` :600–604, title :610 |
| A day with no filings | `The Daily Docket` | "No petitions or applications were docketed on {date}." | `R/scotus_dash_new.R` :486–488 |
| Cases hub `/cases/` | `Cases` | "Every docket the Court has opened since OT2017…" | `R/docket_page.R` :1682, dek :1683–1685 |
| Cases hub — Original jurisdiction | `Original jurisdiction` | "Cases between States, and the occasional State against…" | `R/docket_page.R` :1670–1672 |
| A Term's cases | `OT2025` | "{n} dockets from October Term {YYYY}." | `R/docket_page.R` :1647–1649 |
| A case page | the case caption | the posture line, then the disposition word | `R/docket_page.R` :1339; posture :1230–1236; disposition :1019–1043 |
| Forecast sentence | — | "well below the {n}% base rate…" / "about {n}× the…" | `R/cert_model.R` :1619–1620, :1637–1639, :1655–1658 |

### The conference machinery

| Page | Headline | Standfirst | File |
| --- | --- | --- | --- |
| Conferences index | `Conference Reports` | "What the Justices consider at each private conference…" | `R/conference_dash.R` :484, dek :489 |
| A conference report | `Conference of {date}` | "{n} cases distributed for this conference — sortable…" | `R/conference_dash.R` :432–435, title :441 |
| Relist Tracker | `Relist Tracker` | "{n} petitions the Justices have considered at least twice…" | `R/relist_watch.R` :202–206, title :228 |
| Orders index | `Orders` | "Every order list and miscellaneous order the Court has issued…" | `R/orders_list.R` :567–569 |
| An order list | the long date | *(no dek — the counts line stands in)* | `R/orders_list.R` :510; counts :462–466 |
| Order-page section headings | `Certiorari granted`, `Summary dispositions`, … | — | `R/orders_list.R` :81–86 |

### The merits side

| Page | Headline | Standfirst | File |
| --- | --- | --- | --- |
| Arguments index | `Oral Argument Navigator` | "Every granted case and when it is heard…" | `R/argument_nav.R` :555–556 |
| A Term's arguments | `OT2026 — Oral Arguments` | "{n} cases argued or scheduled — sortable…" | `R/argument_nav.R` :502–508, title :514 |
| Justices index | `The Justices` | "Each Term's opinions by author and kind, who joined whom…" | `R/justices.R` :1312, dek :1314 |
| A Term's Justices page | `October Term 2025` | "{n} argued cases decided by written opinion. Who wrote…" | `R/justices.R` :1281; dek :1029–1030 |
| Justices panel headings | `Opinions written`, `Who agreed with whom`, `The shape of the Term`, `The writings`, `How these are counted`, `How they write` | each has a `p.note` beneath it | `R/justices.R` :1087, :1110, :1166, :1186, :1189, :1252 |

### The explainers

| Page | Headline | Standfirst | File |
| --- | --- | --- | --- |
| The Cert Funnel | `The <em>Cert</em> Funnel` | "Nearly everything the Supreme Court is asked to do…" | `R/cert_funnel.R` :863–865, kicker :862 |
| The Counsel Table | `The Counsel Table` | "Who files certiorari petitions, whose petitions get a second look…" | `R/counsel_table.R` :1099, dek :1101–1103 |
| Methods note | `Predicting the Probability of Certiorari` | "How this site estimates the chance that a petition will be granted…" | `docs/make_methods_note.R` :287–288 |

**The two long-form pages.** `R/cert_funnel.R:862–979` is an essay in string form —
six sections with over-lines, a glossary at :876, and a methods list at :945–979.
`docs/make_methods_note.R:291–377` is the second. If you want to rewrite prose at
length, that is where the prose is. The six `/justices/` panel notes
(`R/justices.R:1088`, `:1111`, `:1167`, `:1187`, `:1253`, and the definitions at
:1190–1195) are the third-largest block and the newest.

**The table is not the inventory.** Every page also carries column headers, chip
labels, empty states, toggle labels, tooltips and a social-card description. The
biggest uncatalogued blocks: the case page's panel labels and ten-item timeline
legend (`R/docket_page.R:669–680`, :1226–1348), the `/counsel/` board headings and
method notes (`R/counsel_table.R:1123–1267`), and the two footnotes under the
conference and relist tables (`R/conference_dash.R:415–424`,
`R/relist_watch.R:208–222`). Grep, don't scroll.

---

## 3. Five rules that will bite you

### Rule 1 — type the real character, never the HTML entity

Write `—` and `–` and `…`. **Never write `&mdash;`, `&ndash;`, `&hellip;`** in a
string that goes through `htmltools`.

About half the site's *editorial surfaces* are built with `htmltools`, which
escapes the text you give it: the landing page, About, 404, every styled index
page, `/counsel/`, `/funnel/` and the `/orders/` leaves. Hand `&mdash;` to one of
those and the reader sees a literal `&amp;mdash;`. The other half pastes raw HTML
— all 56,000 case pages, the `/cases/` hub, every interactive leaf, the
`/justices/` pages, the masthead and the feeds — where an entity works. Both forms
appear *within* single files, so the behaviour is per-call-site, not per-file:
check how the string you are editing is emitted.

Two consequences the rule's simple form hides:

- **The entity check cannot stop you.** `audit_site.R:361–371` fails on 18
  double-escaped entities, but the audit is read-only and runs in its own nightly
  workflow (`.github/workflows/audit-site.yml`) against the already-published
  site. It never blocks a publish; it just turns red the next morning. It also
  samples ~25 pages (`audit_site.R:314–327`, :356–360) — nothing under
  `/justices/` or `/orders/`, nothing in `methods.html`, and 5 of 56,280 case
  pages. An entity on those pages is never reported at all.
- **Social cards decode a shorter list.** `.meta_decode()` (`R/site_meta.R:53–61`)
  handles twelve entities. An `&larr;`, `&rarr;`, `&times;` or `&bull;` in a
  headline or dek passes through undecoded and ships as `&amp;larr;` in
  `og:title` — even where the entity renders correctly on the page itself.

The same applies going the other way: **don't "fix" an existing `&mdash;` into
`--`.** A double hyphen is not an em dash; it publishes as two hyphens.
`smarten()` curls quotes and nothing else — it will not rescue one. (That was live
on the About and 404 pages until September 2026.)

One place the entity is **required**: `R/docket_page.R:1326` writes a case page's
`<title>` as `{caption} &mdash; No. {dkt}`, and the landing page's Most-Read panel
parses the caption back out with `sub("\\s*&mdash;\\s*No\\..*$", "", t)`
(`R/site_analytics.R:173`). A real em dash there leaves a trailing docket number
on every caption in that panel.

### Rule 2 — type straight quotes and let the site curl them

Write `it's` and `"quoted"`. `smarten()` (`R/page_style.R:350–367`) turns them
into `it’s` and `“quoted”` at render time. It touches **quotes only** — never
dashes, hyphens or ellipses — and it returns anything inside a tag untouched.

*Exceptions, where you must type the curly character yourself:*

- **Inside an HTML attribute.** `smarten()` skips tag internals by design, and the
  attributes here are single-quoted inside an R double-quoted string, so a
  straight `'` closes the attribute early. The one live instance is the search
  box at `R/page_style.R:285` — `placeholder='Search all cases by name or docket
  number…'`, whose `…` is a literal U+2026.
- **Inside a JavaScript string.** `R/page_style.R:567` (the 404's docket-aware
  opener) and `R/interactive_theme.R:298` (the page-size select's `All`) are prose
  inside `<script>`, which `smarten_html()` skips wholesale.
- **Wherever the renderer never calls `smarten()`.** The `/cases/` pages
  (`.write_cases_page`, `R/docket_page.R:1695`) and `original_status_word()` paste
  raw HTML with no smartening — which is why `R/original_dockets.R:143` carries a
  typed U+2019 in "Special Master’s report filed".

Never write `&rsquo;`. The one exception is `docs/make_methods_note.R:276–277`,
whose R literals are single-quoted, so an apostrophe cannot appear at all.

### Rule 3 — `%` must be doubled in a format string

If the line you're editing sits inside `sprintf()` and contains `%d`, `%s` or
`%.1f`, those are placeholders for numbers — leave them where they are. And a
literal percent sign has to be written `%%`. A stray single `%` is a runtime error
that kills the whole build.

Example, at `.github/scripts/build_dashboards.R:397`:

```r
"Paid-docket cases filed in the last %d days, against a %.1f%% base rate. "
```

Reordering the words in such a string means reordering the arguments: the
most-read note at `:505` has four positional placeholders consumed in order.

*Don't over-apply it.* `R/site_decisions.R:305` reads `sprintf("%02d", unique(y) %% 100L)`
— that `%%` is R's modulo operator sitting in sprintf's **argument**, not an
escape in the format string.

### Rule 4 — a few strings are load-bearing, not just decorative

Most words are free. These aren't:

| String | Why it's stuck |
| --- | --- |
| `R/conference_dash.R:432` — "N cases distributed…" | The conferences **index** parses the count back out of the published page with `([0-9,]+)\s+case` (`R/conference_dash.R:470`). Keep the number immediately before the word "case"; no intervening word is tolerated. |
| `R/argument_nav.R:502–508` — "N cases argued or scheduled…" | Same trick on `/arguments/`, regex `([0-9,]+)\s+(?:granted\s+)?cases?` (`R/argument_nav.R:543`). Only the word "granted" may sit between the number and "case". Both scrapes are first-match-wins over the whole page. |
| `R/argument_nav.R:405` — `Granted` `Scheduled` `Argued` `Decided` `DIG'd` `Dismissed` | Also the colour-lookup keys in `STATUS_FILL` (`R/palette.R:233–235`) and minted a second time by `classify_argument()` at `R/argument_nav.R:129–134`. Rename one in only one of the three and its cells lose their shading, silently. |
| `R/conference_dash.R:313` / `R/scotus_dash_new.R:512` — `Paid` `IFP` `Application` | Same coupling, against `TYPE_CHIPS` (`R/palette.R:219`). |
| `R/relist_watch.R:155` — `Held` | A display label *and* a lookup key, used again at :188 (palette) and :201 (the dek's "N of them apparently held" clause). The footnote at :212 explains the label and must move with it. |
| `R/docket_page.R:1686` — `Cases` | Passed as `crumb_label` and compared at `R/docket_page.R:1697` to decide whether the hub shows a middle breadcrumb. The hub's *heading* at :1682 is free; the `crumb_label` is not. |
| `R/site_nav.R:39–59` — the `long` section names | Pre-escaped HTML, pasted raw into 56k case-page footers (`R/site_nav.R:223`) and wrapped in `HTML()` on the 404 (`R/page_style.R:592–593`). Write `&amp;`, not `&`, in *these* specific strings — the opposite rule from the landing page's labels at `build_dashboards.R:238`. |
| `R/site_nav.R:39–59` — the `label` values | Also pasted raw, and width-critical: the comment at :44–49 records that nine labels need 833px against a 60rem masthead's 816px, which is why "The Funnel" became "Funnel". A longer label wraps the nav on every page. |
| `R/site_decisions.R:189` — `Per Curiam` | Exact title case, matched case-sensitively at `R/site_decisions.R:425` and :445 to pick the Court's own PDF. Re-spell it and per curiam rows link the wrong opinion. The panel's own `tolower()` check at `R/page_style.R:843` would survive, so the failure is silent. |
| `R/granted_noted.R:222–223` — `unanimous`, `unanimous in judgment` | Asserted verbatim at `R/justices.R:784`. Re-word either and the Justices pages' unanimity counts go to zero while the note still claims they cover every decision. |
| `R/orders_list.R:589`, `:592` — `Order` | Compared by string at `R/docket_page.R:1420` to suppress a redundant "· Order" on a case page's Orders row. |
| `R/feeds.R:434` — `Sitemap:` in robots.txt | Asserted by the audit (`audit_site.R:503`). Not prose; leave it alone. |

The audit's other wording-sensitive checks: `audit_site.R:143` fails on the
literal `<li>Petition for a writ` (the pre-template marker), and `:693` fails on
`NA`/`NaN`/`Inf` rendered into a counsel cell. Everything else it can fail on keys
on CSS classes and structure — `smast-wm`, `bcrumb`, `cfoot`, `ctab` — so the
class names are stuck even where the text inside them is free.

### Rule 5 — some sentences exist in more than one place

Change one of these and you have introduced a disagreement, not fixed a typo:

- **The relist definition**, three times, already drifted: `R/relist_watch.R:209–212`
  and `R/counsel_table.R:1130–1134` say "…no call for a response and no CVSG";
  `R/cert_funnel.R:954–957` omits the CVSG. A fourth, shorter gloss is in the
  funnel's glossary at `R/cert_funnel.R:879`.
- **The model disclaimer**, five times, in two grammatical numbers: "An estimate,
  not a prediction about any case." (`build_dashboards.R:398`, `:444`,
  `R/scotus_dash_new.R:597`) against "Estimates, not predictions about any case."
  (`R/conference_dash.R:423`, `R/relist_watch.R:219`).
- **The feeds have three names.** `FEED_TITLES` (`R/page_style.R:412–413`) for the
  invisible autodiscovery `<link>`, `FEED_LABELS` (:424) for the visible follow
  line, and a third phrasing in About's running prose (`R/page_style.R:668–683`).
- **`Questions Presented`** as a column header, in four files:
  `R/scotus_dash_new.R:565`, `R/conference_dash.R:400`, `R/relist_watch.R:192`, and
  `R/argument_nav.R:484`. `Grant forecast` likewise.

---

## 4. Preview before you push

R and every package are already installed. Every script uses relative paths, so
**run them from the repo root**. Render into a scratch directory — never into
`./site`.

```bash
mkdir -p /tmp/preview

# Counsel Table — ~1 second, no network
SITE_DIR=/tmp/preview Rscript .github/scripts/render_counsel.R

# Cert Funnel — borrow a committed snapshot as the live term
mkdir -p /tmp/fcases && cp data-raw/snapshot_26.rds /tmp/fcases/cases-26.rds
SITE_DIR=/tmp/preview CASES_DIR=/tmp/fcases Rscript .github/scripts/render_funnel.R

# Methods note — under 1 second; writes docs/cert_model_methods.html in place
Rscript docs/make_methods_note.R
```

`snapshot_26.rds` replaces the old `ot_2024.rds` recipe: it is a committed
full-Term fetch in the same shape, six times faster to classify, and it previews
the *current* term, which is what the funnel's live section shows. Before
trusting the funnel timing, check the baselines are fresh —
`CHECK_ONLY=1 Rscript .github/scripts/make_baselines.R`. If they are stale the
render recomputes them in-process and takes about four minutes instead of
seconds; point `BASELINES=/tmp/fbase.json` at a regenerated copy rather than
dirtying `data/`.

Serve the result over HTTP rather than double-clicking the file — the masthead
uses root-absolute links (`/dashboards/`, `/methods.html`, `/analytics.js`), which
are broken under `file://` and make the page look wrong for reasons unrelated to
your edit. Open the section, not the root: a scratch directory has no landing
page, so `http://localhost:8000/` is a directory listing.

```bash
cd /tmp/preview && python3 -m http.server 8000   # then /counsel/ or /funnel/
```

Conferences, relists and arguments need one term snapshot. `snapshot_26.rds`
works here too — the contiguity check only fires for more than one term — so the
`gh run download` preamble is optional:

```bash
mkdir -p /tmp/ccases && cp data-raw/snapshot_26.rds /tmp/ccases/cases-26.rds

SITE_DIR=/tmp/preview CASES_DIR=/tmp/ccases MIN_CONF_DATE=2026-08-01 \
  QP_MAX_NEW=0 WORD_COUNT_MAX_NEW=0 PET_SIG_MAX_NEW=0 \
  Rscript .github/scripts/render_conferences.R
```

**All three caps are needed now.** `QP_MAX_NEW=0` alone leaves `WORD_COUNT_MAX_NEW`
and `PET_SIG_MAX_NEW` at their 600 defaults (`render_conferences.R:221`, :228) and
the preview starts downloading petition PDFs. One request is unavoidable either
way: `update_court_calendar()` at `:113` has no env knob, but it is wrapped in
`tryCatch` and degrades to the held calendar.

Note that this recipe is not lightweight: `render_conferences.R:268` renders a
docket page for every docket in the snapshot. Point `SITE_DIR` at a gh-pages
checkout and the content-hash manifest suppresses that work.

### Sections that render from the published site

Orders, the Justices and search read what is already on gh-pages, so they need a
checkout rather than an empty directory. Keep it out of `/tmp` (macOS purges it)
and reset it afterwards:

```bash
git worktree add ../cert-ghpages origin/gh-pages

# Orders — render-only, no network at all
Rscript -e 'source("R/page_style.R"); source("R/orders_list.R"); render_orders("../cert-ghpages")'

# The Justices — render-from-cache
SITE_DIR=../cert-ghpages LINEUP_MAX_NEW=0 TEXT_MAX_NEW=0 \
  Rscript .github/scripts/render_justices.R

git -C ../cert-ghpages checkout -- . && git -C ../cert-ghpages clean -fd
```

Two gotchas. `render_orders()` skips any page already stamped at the current
`ORDERS_TEMPLATE_VERSION` (`R/orders_list.R:536`), so bump it or delete the target
HTML or your edit is invisible. `render_justices.R` is not fully offline even at
zero caps — `:44` always refreshes at least three Granted & Noted PDFs — but it is
`tryCatch`'d, and it hard-stops without `arguments/granted_noted.json`.

Search has no page and no render script. Its prose is in three places:
`R/page_style.R:283–286` (the box), `search.js:247` ("No matching cases.") and
`R/page_style.R:567` (the 404 fallback). It only functions over HTTP against a
checkout that has both `cases/search.json` and `/search.js`.

The landing page and the daily dashboards are the two that genuinely need a live
fetch; for those it's usually easier to push and let the daily run. To check the
Recent-decisions panel's wording without one, render the panel alone from the two
published manifests:

```r
source("R/page_style.R"); source("R/site_decisions.R")
rows <- read_decided(c("../cert-ghpages/dashboards/decided.json",
                       "../cert-ghpages/arguments/decided.json"))
writeLines(as.character(decisions_panel(rows)), "/tmp/preview/panel.html")
```

The generated writings sentence has its own one-liner:

```bash
Rscript -e 'source("R/granted_noted.R"); cat(gn_writings_phrase("Thomas (D); Alito (D); Kagan (C/J)"))'
```

Finish with the audit, which catches two things a wordsmith can break — a stray
six-digit hex outside `R/palette.R`, and a missing `cases/search.json`:

```bash
SITE_DIR=../cert-ghpages Rscript .github/scripts/audit_site.R
```

---

## 5. Publish it

Commit, **push to `main`** (a workflow runs from GitHub's copy, never your
laptop's), then dispatch. `docs/workflows.md` holds the authoritative
path-ownership table; this one is the wordsmith's view of it.

| File you edited | Section it changes | Dispatch |
| --- | --- | --- |
| `build_dashboards.R` | landing page, About, 404, feeds, `search.js` | `gh workflow run daily.yml --ref main` |
| `scotus_dash_new.R` | dashboards | `gh workflow run daily.yml --ref main` |
| `orders_list.R` | `/orders/` + the Latest orders panel | `daily.yml` — **needs a version bump** (below) |
| `feeds.R`, `search.js` | feed names, robots.txt, the search matcher | `gh workflow run daily.yml --ref main` |
| `page_style.R` | About, 404, **and every index page on the site** | `daily.yml` **and** `conferences.yml`, **and** a version bump for case pages |
| `conference_dash.R`, `relist_watch.R`, `argument_nav.R`, `argument_calendar.R` | conferences, relists, arguments | `gh workflow run conferences.yml --ref main` |
| `site_calendar.R`, `court_calendar.R` | the Upcoming rows | `conferences.yml` **then** `daily.yml` |
| `site_decisions.R`, `granted_noted.R` | Recent-decisions rows, Separate writings | `conferences.yml` **then** `daily.yml` |
| `justices.R`, `opinion_text.R`, `justices_network.js` | `/justices/` | `gh workflow run render-justices.yml --ref main -f lineup_max_new=0 -f text_max_new=0` |
| `cert_funnel.R` | the funnel | `gh workflow run render-funnel.yml --ref main` |
| `counsel_table.R` | the counsel table | `gh workflow run render-counsel.yml --ref main` |
| `docs/make_methods_note.R` | methods.html | regenerate first (below), then `daily.yml` |
| `docket_page.R`, `cert_model.R`, `original_dockets.R` | case pages | **needs a version bump** (below) |
| `site_nav.R` | the masthead on *every* page | all of the above |

Three rows changed meaning since this table was first written:

- **`page_style.R` is not just About and 404.** `styled_index_page()` is called
  from six files, so an edit there reaches the conferences, arguments, orders,
  justices and dashboards indexes too — and contributes nothing to the docket-page
  hash, so case pages need the version bump as well.
- **`site_forecast.R`, `site_analytics.R` hold no reader-facing prose at all.**
  Every string in them is a `message()`. The words those panels show live in
  `build_dashboards.R:435–448` and `:503–508`.
- **`site_calendar.R`'s labels are frozen into JSON by `conferences.yml`**
  (`render_conferences.R:106`, `:115`; `render_arguments.R:101`). The daily only
  reads those manifests. Editing the prose and dispatching `daily.yml` changes
  nothing.

Add roughly two minutes for GitHub's own Pages deploy, plus up to ten minutes of
CDN cache — hard-refresh to skip it.

Measured run times, not estimates: `daily` 5–7 min, `render-counsel` 1–2 min,
`render-justices` 2–3 min from cache (up to 31 min when it fetches),
`conferences` **32–90 min** (recent scheduled runs: 61–79), `render-funnel` 35 min
on its one successful run — the workflow's own header still predicts three
minutes; nothing supports that. Everything except `daily` and `rerender-dockets`
shares the `conference-reports` concurrency group, so they queue behind each
other. `conferences.yml` also rebuilds the funnel, the counsel table and the
Justices in its own publish job, each `continue-on-error` — so those three go out
on the next Monday run whether or not you dispatch them.

### Three special cases

**Case pages need a version bump.** Docket pages re-render only when their content
hash changes, and the hash does *not* include the page's prose. Edit wording in
`R/docket_page.R` and nothing at all will re-render. Bump `PAGE_TEMPLATE_VERSION`
at `R/docket_page.R:339` (`"v40"` → `"v41"`) in the same commit. That reaches the
current term on the next daily; the 56,280-page back catalogue needs
`gh workflow run rerender-dockets.yml --ref main` (measured 143–157 min on its
four full ten-term runs). `-f reuse_from_runs=<a recent run id>` skips the fetch,
but no such run exists in the last sixty, so its real cost is unmeasured.

Keep the assignment line's shape: `audit_site.R:29` and `audit_smoke.sh:13` both
parse it with `^PAGE_TEMPLATE_VERSION <- ` and extract `"vNN"`. Reformat it and
the audit silently compares every page against nothing.

**Order pages need their own bump.** `ORDERS_TEMPLATE_VERSION` at
`R/orders_list.R:54` (`"o3"`) gates `render_orders()` the same way, at
`R/orders_list.R:536`, across 1,047 published pages. The `/orders/` index dek and
the landing panel are rewritten unconditionally and need no bump; anything inside
`render_order_page()` does.

`JUSTICES_TEMPLATE_VERSION` (`R/justices.R:47`) looks like a third one but is
**not** a gate — every Justices page rebuilds every run. Don't bump it needlessly.

**The methods page is generated.** `methods.html` is a copy of
`docs/cert_model_methods.html`, which is written by `docs/make_methods_note.R`,
with a masthead injected afterwards (`build_dashboards.R:299–305`). Edit the
**generator**, run it, and commit both files:

```bash
Rscript docs/make_methods_note.R
git add docs/cert_model_methods.html docs/cert_model_calibration.png
```

Editing `docs/cert_model_methods.html` directly works until the next regeneration
silently throws your words away. That has already happened once — the generator
says so at `docs/make_methods_note.R:68`.

### Where the words won't move

Four archives don't re-render on a normal run, so old pages keep old wording
indefinitely:

- **Case pages** — 56,280 of them, behind `PAGE_TEMPLATE_VERSION` (above).
- **Order pages** — 1,047, behind `ORDERS_TEMPLATE_VERSION` (above).
- **Conference reports before `MIN_CONF_DATE`** — 260 of the 262 published. Reach
  them with `-f terms=24,25,26 -f min_conf_date=2025-09-01`, and note the terms
  must be contiguous or the render refuses to publish
  (`render_conferences.R:65–71`). The `min_conf_date` half is **mandatory**, not
  optional: `MIN_CONF_DATE` is derived per run as 1 August of the highest term
  fetched (`plan_terms.R:27–29`), so `-f terms=24,25,26` alone still yields
  2026-08-01 and reaches nothing older.
- **Dashboard days outside the fetch window.** Reach them with
  `-f full_term=true -f min_date=YYYY-MM-DD`. This is *not* a pure re-skin:
  forecasts recompute and dispositions update, so old numbers will move. And
  `daily.yml:138` pins `TERM_YEAR: "26"` with no dispatch input, so only OT26
  dashboards are reachable at all.

A fifth escape hatch exists for the shared `<head>` chrome of already-published
conference / dashboard / argument / relist leaves: `patch-leaf-chrome.yml`,
~1 minute, no fetching and no re-rendering. Its `dry_run` input defaults to
**true**, so pass `-f dry_run=false` to actually publish. It rewrites feed
autodiscovery and `/lib/` links, never page prose, and is explicitly not for
docket pages.

Per-Term argument pages and every `/justices/` page, by contrast, rebuild every
run.
