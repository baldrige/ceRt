# The Forecast Scorecard

`/scorecard/`: the site's cert-grant forecasts, written down before each
conference, held to the orders that followed. Built by `R/scorecard.R`;
reached from the landing page's section index and the "Model" breadcrumb.

| file | what it is |
| --- | --- |
| `scorecard/scorecard.json` | one row per ledger entry (`docket@conference`) with its outcome |
| `scorecard/index.html` | the page: masthead, prose, empty chart slots |
| `scorecard/scorecard.js` | draws everything in the browser from the JSON |

## Where the numbers come from

**Forecasts: the ledger, and only the ledger.** `cases/forecasts.json`
(`R/forecast_log.R`) is append-only and written only for conferences that have
not happened yet. Nothing else on the site records what was said in advance:

- The conference pages re-score past conferences with today's model and the
  case as it stands now. At the Sept 28, 2026 long conference, 9 of 342
  forecasts moved after the conference, one from 2.1% to 10.6%.
- Their embedded `Forecast` data column is **P(granted ever)**
  (`conference_dash.R`); P(granted here) is only inside the rendered cell.

Scored from the pages, the first two OT2026 conferences "expected" 23 grants
against 5. From the ledger, 8.4 against 5.

The ledger keeps a petition's *first* forecast, often weeks ahead (a median of
49 days for the 2026 long conference). It can trail what was learned later:
25-1131 was logged at 0.6% and its conference page later showed 17%. A
second, final forecast logged the morning of the conference would let the page
score both.

**Outcomes: the parsed order lists** (`orders/`, `R/orders_list.R`), so the
daily can rebuild the scorecard the day a list is released. A conference's
outcome is the first grant, GVR or denial in the orders from its date up to the
next conference; none is **carried over** (relisted, held, rescheduled, CVSG, or
only the IFP motion decided). A conference counts as **decided** once an order
*list* falls in that window. Grants can come out the afternoon of the
conference, but denials wait for the list. **Since** (the granted-ever view) is
the first disposition at any time after the conference. Habeas, mandamus,
prohibition and rehearing entries are left out.

**Case details**: caption, court below and counsel come from the conferences run's
`dist`. The daily, which has no `dist`, carries them forward from the last
`scorecard.json`, falling back to `cases/search.json` for the caption. Subjects
come from `cases/subjects.json`.

## What the page computes

Everything is scored on P(granted at this conference) against "granted at this
conference", over the latest Term with a decided conference:

- grants against expected (sum of forecasts);
- each grant's rank among its conference's logged petitions;
- AUC and Brier skill, with a bootstrap range and a plain-language verdict;
- calibration by forecast group, with Wilson intervals;
- the gains curve: grants found reading down the list in forecast order.

The yardstick is `SCORECARD_REFERENCE`: the conference model's granted-here
rolling-origin backtest (OT2019–OT2024, 11,716 rows, 346 grants), all from one
harness so the figures agree with each other. `docs/cert_model.md`'s
competing-risks table quotes AUC 0.920 for the same target from an earlier run.

## When it runs

- **Nightly** (`conferences.yml` → `render_conferences.R`): right after
  `append_forecasts()`, with `dist`. New ledger entries reach the On deck panel
  the same night.
- **Daily** (`daily.yml` → `build_dashboards.R`): after the order lists are
  parsed and rendered. A conference is scored the day its list is released.

Never fatal to either run. On a publish race, `publish_site.sh` keeps this
run's copy of the three files. Each is a pure function of the merged ledger and
order lists, so the next run of either workflow rebuilds it.

## Checks

`audit_site.R` §11 fails if any scorecard row was logged on or after its
conference, or carries an outcome for a conference that is not decided. The page
is in the build-time placeholder sample, and `PALETTE_SCORECARD` is in the
colour-source check.
