# Emergency applications

`/applications/` — every application for interim relief that is pending or was
ruled on in the last twelve months, and a by-Term table back to OT2017.
`R/applications.R`; built nightly by `render_conferences.R` (`conferences.yml`)
right after the Relist Tracker, and rebuilt by each daily run (below). In the masthead as **Interim** (the Court's own
word for this docket since 2025; the tenth link, ~27px of slack left — see
[navigation.md](navigation.md)), and on the landing page.

## Why it is built where it is

The daily fetches the trailing ~50 dockets of each bucket — about two weeks of
applications — and a stay can sit for months. Only the nightly's full-Term
`combined` holds the whole `NNA###` bucket, so the page is built there. The
by-Term history comes from the committed archives (`data-raw/ot_*.rds`), read
in the same run.

**Between nightlies (2026-10-21).** Built only there, a ruling issued during the
day waited for the 06:00 UTC run. Now the nightly also writes
`applications/applications.json` (`app_snapshot_write()`): its classified live
applications, keyed by docket. An interim row keeps the page's columns and its
Documents cell, rendered then; any other row keeps only what the by-Term count
needs (~300 KB for OT2025–26). Each daily run (`applications_daily()`, from
`build_dashboards.R`):

1. reads the snapshot (none yet → the page is left alone);
2. re-reads by name every interim application it lists as pending or deferred
   and not already in the run's fetch (13 on the test, ~6 s at 2 req/s);
3. classifies those and every application the run already holds (trailing
   window, watch list, order dockets), and lays them over the snapshot;
4. writes the snapshot back and rebuilds the page from `application_history()`,
   plus the re-read dockets' case pages.

Rebuilt from an untouched snapshot, the page is byte-identical to the nightly's
(gt's element ids aside). The archive classification makes it ~50 s a run. The
snapshot is in `publish_site.sh`'s union list, and a conflict on
`applications/index.html` keeps the run's own render, as for the scorecard.
A new application filed with one Justice reaches the page through the trailing
window; one older than the window and not yet in a snapshot waits for the
nightly, as before.

## Most applications are not the emergency docket

10,205 application dockets, OT2017 to OT2026 (measured 2026-10-20):

| kind | dockets | |
| --- | --- | --- |
| extension of time | 8,423 | counted, not shown |
| procedural (word limits, judgment forthwith, ...) | 201 | counted, not shown |
| **stay** | 901 | interim |
| **stay of execution** | 292 | interim |
| **injunction** | 198 | interim |
| **bail or release** | 67 | interim |
| **vacate a stay or injunction** | 62 | interim |
| certificate of appealability | 60 | substantive, not interim |

`application_kind()` reads the request from the opening entry ("Application
(25A123) **for a stay**, submitted to Justice Alito."). `APP_INTERIM_KINDS` is
what the page shows and what the by-Term table's interim columns count.
`classify_applications(full = ...)` reads the whole docket only for kinds that
need it: all 10,205 take ~50 s, the interim ones ~10 s.

## The outcome grammar

Submitted to one Justice → ruled on alone, or referred to the full Court →
granted / denied / dismissed / withdrawn. A denial by one Justice can be
**refiled** with another (Rule 22.4), which reopens it. So the outcome is the
**last** ruling, and a refiling after it means the application is live again.

A ruling is read from the sentence that **numbers this docket and opens with
the application** ("Application (26A40) denied by Justice Kagan.", wherever it
sits in the entry), else from the **first sentence** of an entry beginning
"Application". "Application (25A123) denied by the Court. Justice X would grant
the application." is a denial. An unnumbered **collateral** sentence (leave to
file, under seal, supplemental appendix, redacted copies) rules on nothing, and
a **granted extension** outranks a later refusal to extend further (22A209).
Beyond that, `.app_disposition()` handles:

| form | example |
| --- | --- |
| deferred to argument | 24A884 (*Trump v. CASA*), 22A444 (*Biden v. Nebraska*) |
| treated as a cert petition | 20A94, 21A222 |
| granted as to some, otherwise denied → granted in part | 25A810 |
| a ruling that is not the first sentence of its order | 24A831 (per curiam opening with the facts), 24A712 (a Justice lifting their own administrative stay) |
| "We deny the application" | 20A150 |
| withdrawn by letter; "Letter to Withdraw Reapplication" | 18A276, 22A747 |
| "lacks a quorum ... considered closed" | 23A617 |
| "Application closed." / "completed." | 17A145 |

Three traps, each one a real docket:

- **17A745 (*Rucho v. Common Cause*).** "The motion of appellees to construe
  *the application* for a stay as a jurisdictional statement ... is denied", on
  a stay the Court **granted**. The non-first-sentence fallback therefore
  requires "the application" to open its sentence (after `. `, `: `, `] `,
  `.) `, a leading clause's comma, or "that"), never to follow a verb.
- **17A1375.** "denied by Justice Alito" is listed *above* "refiled and submitted
  to Justice Alito", on the same day. Entries within a day are not in
  chronological order. On a same-day tie, a ruling that names the Justice it
  was refiled to came after the refiling.
- **21A85.** "Justice Sotomayor would grant the application to vacate stay *in
  No. 21A85* and dissents from the dismissal ... as improvidently granted"
  numbers the docket inside a dissent, on an application the Court denied. A
  numbered sentence counts only when the application opens it.
- **Collateral relief (18A1238, *Dunn v. Price*; 18A655; 18A669).** The order
  that denies a stay also grants leave to file under seal. Unnumbered, that
  sentence is ignored; this is the bug that published a capital stay as granted
  until v23.
- **`\r\n` inside entries.** The archive keeps the Court's hard line breaks, and
  `.` in a pattern does not cross them (24A587 read as pending until entries were
  whitespace-normalised).

**Coverage:** of the 984 interim applications in OT2017–OT2023, **6 read as
pending** (0.6%); each is a docket with no ruling on it at all.

### One classifier, for the tracker and the case pages

Since `PAGE_TEMPLATE_VERSION` v46, `docket_page.R`'s
`classify_application_events()` (the case pages, Recent decisions, the
decisions watch list) is a wrapper over `application_profile()`. Before that it
had its own grammar. Measured over **all 10,205** application dockets, the two
first disagreed on 95: 67 extensions and 3 collateral orders where the old
grammar was right, which were ported (numbered sentences, collateral, extension
precedence), and 25 where it was wrong, all read by hand:

| | n | case page said |
| --- | --- | --- |
| refiled after a denial, live again | 7 | Denied |
| deferred to argument | 6 | Granted / pending — the *petition* was granted |
| treated as a cert petition | 5 | Granted — likewise |
| dismissed as moot (23A745, *Trump v. United States*) | 1 | Granted |
| denied after a deferral (21A85) | 1 | Granted |
| quorum closure; withdrawn after a refiling | 2 | Denied |
| granted in part (25A810) | 1 | Denied |
| ruled on in prose (20A150, 25A515) | 2 | pending |

After the port the two agree on 10,180 and differ on exactly those 25. The
case page has two new box words for them: *Application deferred to argument*
and *Application treated as a petition*. A deferred application stays on the
decisions watch list, since its ruling comes with the argued case.

The daily re-renders only the current Term's pages on a template bump; the
back-catalog's 25 changed pages (and the new masthead link on every page) need
`rerender-dockets.yml`.

## Stale archives, and the pending cache

An archive is frozen when it is written: `ot_2024.rds` ends 2025-06-18, before
*Trump v. CASA* (decided June 27), *D.V.D.* and *McMahon* were decided. Read
as-is, the tracker's OT2024 row had the federal government winning 10 of 22
applications, against roughly 16 in fact.

The fix reuses the targeted pending fetch (`R/pending_dockets.R`). Applications
were kept out of `cases/pending.json` because the petition classifier called
almost all of them pending forever (a 496-docket list). `update_pending()` now
admits an application that `application_profile()` reads as an *interim*
application still `pending` or `deferred`. `render_conferences.R` also offers the
archive's undecided interim applications (`application_history()`'s non-live
rows). Simulated on 2026-10-20: 12 OT2024 dockets are named on the first night,
they resolve on that fetch and drop out, and the 7 abandoned OT2017–21
applications age out on the 730-day idle clock without a single request.

The first night after this ships still shows the stale OT2024 row: the cache is
written by the render step and read by the *next* run's fetch.

Only a **live** row (fetched this run) can be shown as pending on the page. An
archived "pending" docket was last read before its ruling.

## The applicant's side

Read from the caption up to the "Applicant" role, never past the "v.", by
`counsel_table.R`'s `petitioner_gov_side()`, with two corrections in
`application_side()`:

- **A State's Secretary of State reads as federal there.** `FED_PARTY_RX` matches
  "Secretary of State" and guards only "Attorney General" against a State named
  alongside it, so "Wes Allen, *Alabama* Secretary of State" (25A1229–31,
  25A1314–16) was the United States. **The Counsel Table uses the same function
  and probably carries the same error**; it is not changed here.
- **Agencies the regex does not name.** National Park Service (26A203), Forest
  Service, Army Corps, the Bureaus, and "Department of ..." at the start of the
  name.

Donald J. Trump in a personal capacity (23A745, 22A283) is correctly a private
party.

## The by-Term table

- **Term** is the docket number's (25A### is OT2025), not SCOTUSblog's
  October-to-October window, so its counts will not match theirs exactly.
- **All applications** is withheld for a closed Term with under half the median
  count: the OT2020 archive holds 178 applications against ~1,200 in its
  neighbours. Its *interim* applications are complete (147, in line with every
  other Term), so only the total is withheld.
- **Granted** counts grants in part, as a share of applications ruled on
  (withdrawn and closed excluded).

## Open questions

- **Counsel Table's State/US split** — see above.
- **Per-Justice view.** "Submitted to" is filterable; a small table of rulings
  alone vs. referrals per Justice is an easy follow-on.
