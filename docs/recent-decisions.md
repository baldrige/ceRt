# Spec: "Recent decisions" — a landing-page panel of cases decided by opinion

Status: **implemented** (`R/site_decisions.R`, `decisions_panel()` in
`R/page_style.R`; writer in `render_arguments.R`, reader and watch-list fetch in
`build_dashboards.R`). Written 2026-09-03 as a proposal; widened the same day to
cover the emergency docket and unargued per curiams after measuring how the
dockets record them; built the same day. The sections below are the design as
built, with the measurements that drove each rule.

Two things the build found that the proposal did not anticipate:

- **`classify_argument()` dated Trump v. Slaughter (25-332) as decided on its
  grant date.** The September grant entry also granted the companion stay
  application with a linked per curiam, and the first decision-shaped entry
  won. Fixed in `argument_nav.R`: an argued case's decision entry must come on
  or after the argument. The Navigator now shows 29 June 2026, and no other
  decided date moved.
- **The cert funnel does not know the grant form "the applications are treated
  as petitions for certiorari before judgment, and the petitions are granted"**
  (25-1083 Mullin v. Doe, 25-1084 Trump v. Miot). It holds both "pending",
  which also keeps them out of the Navigator. The decisions panel does not gate
  the argued kind on the funnel's outcome: an "Argued." entry followed by a
  decision is a merits decision whatever the grant looked like. The funnel
  grammar itself is left alone here; it is the model's training corpus and a
  change to it is its own decision.

## What it is

A third panel in the landing page's `panel_top` slot, beside "Likeliest grants"
and "Upcoming at the Court": the cases the Court has most recently decided by
written opinion, newest first, each linked to its `/cases/` page and, where
the docket links one, to the slip opinion PDF.

Three kinds of case qualify, and the row says which:

| kind | what it is | docket bucket |
| --- | --- | --- |
| **Argued** | a merits case decided after oral argument | `NN-###` grants |
| **Emergency application** | an application referred to the Court and decided with a written opinion, signed or per curiam | `NNA###` |
| **Summary reversal** | a petition granted and the judgment below reversed or vacated in the same order, with a per curiam opinion, no argument | `NN-###` |

"Likeliest grants" says what the Court might take. "Upcoming" says when it next
sits. This panel says what it just did. On an opinion day in June that is the
argued docket; in August it is the emergency docket; and it is the same panel
either way, because a reader asking "what did the Court decide" does not sort
by procedural posture first.

```
RECENT DECISIONS
The Court's most recent written opinions, argued or not.

THU      Trump v. CASA, Inc.
Jun 27   Argued · Barrett, J. · Judgment vacated · Opinion (PDF)

TUE      Noem v. Vasquez Perdomo
Sep  8   Emergency application · Stay granted · Kavanaugh, J., concurring · Opinion (PDF)

MON      Klein v. Cabrera
Jun  2   Summary reversal · Per curiam · Reversed and remanded
```

Same date gutter and two-line body as the calendar rows, so the three panels
read as one page. No new colour: the outbound PDF link is `--link`, the case
link inherits ink and colours `--accent` on hover, like `ol.cal`. The kind word
is plain text at the start of the detail line, not a chip: `TYPE_CHIPS` exist
for the dashboards' data tables, and a chip on a prose row would be the one
element on the page styled that way.

## Definition of "decided by opinion", measured against the dockets

The survey below covered OT24 and OT25: 10,614 dockets, 2,720 of them
applications. The numbers matter because the naive rule, "a docket entry that
links a `supremecourt.gov/opinions/` PDF", is wrong in both directions.

- **The link is not sufficient.** Dissents from denial and statements
  respecting denial link the same `NNpdf/` directory as merits opinions: 18 of
  the 31 unargued cert dockets with an opinion link were denials with a
  separate writing. Those are not decisions.
- **The link is not necessary.** Of 37 applications whose entry carries an
  opinion marker, only 4 link the PDF. The Court writes the per curiam opinion
  *into the docket entry* ("The application is squarely controlled by Trump v.
  Wilcox...") or flags it with "(Detached Opinion)", 32 of 37 times.

So the test is on the entry's text, per kind:

**Argued.** Unchanged from the Navigator: the docket is a `classify_petitions()`
grant and `classify_argument()` returns `status == "Decided"`. That status
already loses to `DIG'd`, and the decision grammar already tolerates the
`<a href>` anchors the JSON embeds inside "opinion of the Court". Additionally
require an author, "per curiam", or an opinion link, so a bare "Judgment
Issued" mandate entry does not qualify.

**Emergency application.** An `NNA###` docket whose disposition entry (the one
the docket page's application rule already resolves, last terminal entry wins)
contains any of:

```
(Detached Opinion)  |  per curiam  |  delivered the opinion  |
announced the judgment  |  href=...supremecourt.gov/opinions/
```

**and** is not *only* a separate writing on an otherwise bare order. An entry
whose only marker is "Justice X, dissenting" or "concurring in the denial" is
an order with a dissent, not an opinion of the Court. Measured: 16 of the 37
marked applications are that, and they are excluded. The operative words are
the ones that name the Court's own opinion: "Opinion per curiam", "The
application ... is granted. [prose]", "delivered the opinion".

The disposition itself (granted, denied, granted in part) comes from the rule
the docket page already applies in `R/docket_page.R`. Do not write a second
application grammar; that one was audited across 8.5k pages and the two-tier
self/generic form exists because the naive version published false outcomes.

**Summary reversal.** A cert or appeal docket that `classify_petitions()` files
as `gvr` **and** whose disposition entry says "Opinion per curiam" or
"opinion of the Court" or links an opinion PDF. This is what separates a per
curiam summary reversal from an ordinary GVR "for further consideration in
light of", which is an order and not an opinion even when a Justice dissents
from it in writing (25-273 is the example in the survey). Measured: 53 unargued
cert dockets carried a decision-side marker in two Terms; the "in light of"
GVRs must be filtered out of that set.

**Read the markers from the entry text with the anchors stripped, and take the
URL from the anchor, not from `Links[]`.** Checked live on 26A124 (Trump v.
California, Aug 24, 2026) and 26A203 (National Park Service v. National Trust,
Aug 31, 2026): both per curiam grants carry the slip-opinion URL only as an
`<a href>` inside the entry text, and the JSON's `Links[]` array for that entry
is empty. The anchor also splits the phrase, so the raw text reads
`<a ...>Opinion</a> per curiam` and a literal "Opinion per curiam" test misses
it. `classify_argument()` already tolerates this for argued cases; the new
marker test must do the same.

**Out of scope, still:** opinions relating to orders (dissents from denial,
statements). They are the majority of unargued dockets with an opinion link and
they are excluded by the text tests above, not by URL.

The decision date is the docket entry's date, which is the hand-down date.

## Data flow: manifests, like `upcoming.json`, from the pipeline that can see

The landing page is written by the daily, which fetches only the trailing ~50
dockets of each bucket. Measured against filing dates in the OT25 snapshot,
that window is:

| bucket | trailing 50 dockets span |
| --- | --- |
| paid | 5 days of filings |
| applications | 12 days |
| IFP | 16 days |

and the time from filing to opinion is:

| kind | median | 1st–3rd quartile |
| --- | --- | --- |
| emergency application | 25 days | 10–72 |
| summary reversal | 66 days | 55–77 |
| argued | months | |

So the daily's own fetch sees essentially none of them. This is the gap
`R/site_calendar.R` closes for the calendar, and the same answer applies: the
pipeline that knows writes a small file, the daily reads it, and staleness is
handled at read time by a date window.

**Writer:** the conferences run, which fetches the current and prior Terms in
full every Monday. `render_arguments.R` already holds the argued frame; add a
`recent_decisions(cases, tbl)` in a new `R/site_decisions.R` that produces all
three kinds from the combined frame and writes `arguments/decided.json`. One
writer, one file, because the three kinds come from the same fetched frame and
splitting them across pipelines buys nothing.

**Manifest** (`arguments/decided.json`), one row per docket, always written
even when empty, for the same reason `write_upcoming()` always writes:

| field | notes |
| --- | --- |
| `date` | `YYYY-MM-DD`, the entry date |
| `dkt` | addresses `cases/{dkt}.html` |
| `caption` | `strip_caption_roles(caption)` |
| `kind` | `argued` / `application` / `summary` |
| `author` | `"Per Curiam"`, `"Kagan"`, `"Roberts, C.J."`, or `null` |
| `disposition` | short label, see below; `null` if none matched |
| `opinion_url` | `null` when the docket has not linked one (most applications) |
| `argued` | argued date for `kind == "argued"`, else `null` |
| `term` | for the label link to `arguments/arg_{term}.html` on argued rows |

`disposition` per kind: argued and summary rows apply `JUDGMENT_RX`
(`R/counsel_table.R`) to the decision entry and map the verb to Affirmed /
Reversed / Vacated / Affirmed in part; `.last_entry()` there already picks the
operative judgment. Application rows use the docket page's application
disposition (Stay granted, Denied, Granted in part). Neither grammar is new.

The writer keeps every decision from the last **90 days**, not the six the page
shows.

**Reader:** `read_decided(paths, as_of, n = 6L, days = 45L)` in the daily,
mirroring `read_upcoming()`: skip missing or malformed files, filter to the
window, order by date descending then docket, take `n`.

**Renderer:** `decisions_panel(rows, heading = "Recent decisions", note)` in
`R/page_style.R`, returning `NULL` on zero rows. `NULL` is a legitimate state,
though with the emergency docket in scope it will be rarer than the calendar's
summer emptiness.

**Placement** in `build_dashboards.R`:

```r
panel_top = tagList(sharpest_panel, decisions, calendar)
```

Forecast first, because it is the reason to come back. Decisions second: on an
opinion day they are the news. Calendar last, because it is the furthest ahead.

## Consolidated cases

Several dockets are often decided by one opinion. Group rows by `opinion_url`
when present, otherwise by date, kind and author. The row names every docket,
each linked to its own page, separated by middots, the way an argument day
names every case it hears; "and 1 other" was removed from the calendar for a
reason that applies here too. Grouping is done in the reader so the manifest
stays flat and survives the JSON round trip without `.cal_items()`-style
normalising.

## Freshness: the part that decides whether this is worth building

The writer runs **weekly**, Monday 06:00 UTC. Argued opinions come down
Tuesday to Friday; emergency-docket opinions come down any weekday, often late
afternoon. A Thursday decision would not appear until the following Monday.
A panel called "Recent decisions" that is routinely four days behind is worse
than no panel.

The fix has a pattern in the repo already. `fetch_pending.R` fetches a named
list of old dockets a range fetch would miss. Do the same in the daily, for a
named list the weekly run writes:

1. The conferences run writes `arguments/watch.json`: dockets that could
   produce an opinion soon. Two populations, both small:
   - argued and undecided (`status == "Argued"`): zero in October, 50–60 in
     late May;
   - applications referred to the Court and not yet disposed: the docket says
     "referred to the Court" and the application rule finds no terminal
     entry. The archive has 253 referred applications across two Terms, so the
     live set at any moment is on the order of 10–30.
2. The daily reads it, fetches those dockets by name through the paced
   fetcher (`scotus_pace()` at 2 rps: under a minute at the June peak), and
   runs the kind tests above on the results.
3. Newly decided dockets are appended to the daily's own manifest,
   `dashboards/decided.json`, and `read_decided()` reads both files, deduping
   by docket with the fresher row winning. This is the two-file merge
   `read_upcoming()` already does for conferences and arguments.
4. The same fetched dockets go through `render_dockets_for()`, so the case page
   says "Decided" the same day, not the next Monday.

Summary reversals have no watch list: nothing on the docket announces that a
per curiam is coming, and they issue with the Monday order list a few days
after conference. Two options, in order of preference:

- **Accept the lag for this kind.** Order lists come out Monday at 09:30 ET;
  the weekly run finishes Monday morning UTC, so a summary reversal is at
  worst six days stale and typically appears with the *next* Monday's run.
  This is the cheap answer and the one to ship first.
- **Fetch the last conference's distributed dockets by name** in the daily's
  Monday and Tuesday runs. The conference report knows the list; it is 100–300
  dockets, so one to three minutes at the paced rate. This catches the
  summary reversals the day they issue, and as a side effect refreshes every
  docket page the order list touched, which is a larger win than this panel.
  Worth doing, but it is a change to the daily's fetch budget and should be
  its own decision.

Keep the daily's file separate from the weekly run's file. Two pipelines writing
one path on different schedules is a race, and `publish_site.sh` resolves
derived-file conflicts by union only for files it has been told about.

The daily's named fetch must **never fail the run**. Throttling on this list
costs the panel a day; it must not cost the dashboards. `fetch_pending.R`
already has the right posture.

Recommended order of work: manifest and panel from the weekly run for all
three kinds (an afternoon), then the daily watch-list fetch for argued cases
and referred applications. Do not ship the panel without the second step: on
the days it matters, a weekly refresh is stale.

## Edge cases

Handled by existing code: reargument orders leave `decided_date` NA; the
docket infixing the case number inside the judgment sentence; per curiam
opinions with no named author; embedded anchors inside "opinion of the Court";
an administrative stay granted early and the application denied later (last
terminal entry wins).

To decide in the build:

- **Opinion link posted after the entry.** Common for applications, occasional
  for argued cases. The row renders without the PDF link; both manifests are
  rewritten wholesale from what the run saw, so the link appears on the next
  run without special handling. Accept this; do not suppress the row.
- **An application decided with an opinion and later argued.** 25A312 was
  decided by a signed opinion after argument on the application itself. It
  qualifies as `application`, with the author. The argued-case rule does not
  see it because it is not a `classify_petitions()` grant, so there is no
  double count.
- **Judgment after rehearing.** `classify_argument()` takes the first decision
  entry. Rare enough to note and not code for.
- **A denial with a long per curiam explanation but no opinion.** The text
  test cannot tell "The application is denied. [two paragraphs of reasoning]"
  from a per curiam opinion, because that *is* a per curiam opinion in the
  Court's practice. Treat it as one.

## Guardrails

- **Colour:** nothing new in `:root`. `audit_site.R` fails on any hex outside
  `R/palette.R`, and the panel needs none.
- **Byte stability:** manifests carry event dates only, never a build time. A
  run that finds nothing new must write an identical file so
  `publish_site.sh` short-circuits, the constraint `R/feeds.R` documents.
- **Escaping:** captions go through `smarten()` inside `tags$`, never as raw
  HTML. Captions contain ampersands.
- **Manifest shape:** `write_json(..., dataframe = "rows", auto_unbox = TRUE)`,
  flat, one row per docket. Normalise `null` fields on read.
- **Empty is a state:** an absent file means the weekly run has not run since
  the feature shipped; an empty array means it ran and found nothing in 90
  days. Log which.
- **Grammar reuse:** three existing, audited grammars are load-bearing here
  (`classify_argument()`, the application disposition rule, `JUDGMENT_RX`).
  The only new text test is the opinion-marker test, and it should live in one
  function with the measured counts above as its comment.

## Follow-ons this unlocks

- **`decisions.xml`**, an Atom feed keyed on hand-down date. `R/feeds.R`
  already requires event-dated entries for byte stability, and the manifest
  supplies exactly that. One function.
- **The emergency docket page** (feature-ideas.md, Tier 1 item 4): the
  referred-and-pending watch list from step 1 is its "pending" table, and the
  application-kind rows are its "decided with opinion" table.
- **The opinion clock** (Tier 1 item 3): the argued-and-undecided half of the
  watch list is its input.
- **Docket-page freshness.** Step 4 above is a general fix: today a case page
  can say "Argued" or "Application pending" for up to six days after the
  Court has ruled.
