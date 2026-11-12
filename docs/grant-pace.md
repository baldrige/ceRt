# The grant pace (current Term's Oral Argument Navigator page)

Status: **built** 2026-11-12 (`R/grant_pace.R`). The current Term's page,
`arguments/arg_<yyyy>.html`, leads with a chart: the cert grants made so far in
this Term's window, day by day, against every complete past Term, with the rest
of this Term projected from them. Rebuilt with the Navigator by every
`conferences.yml` run (`render_arguments.R`). Past Terms' pages do not carry it.

## What is counted

- **A grant** is `classify_petitions()`'s `granted` outcome (cert, cert before
  judgment, probable jurisdiction noted), never a GVR. It is unioned with the
  grants in the Court's order lists (`orders/orders.json`, `granted`), and each
  docket counts once, on its earliest grant date. Neither source is complete
  alone: the order lists are fresher than any docket fetch, and the dockets
  hold the grants of the few order documents the site never read (the
  first-Monday list of 3 Oct 2022 among them).
- **The window is 1 August to 31 July** (`grant_window()`), so the long
  conference's late-September grants open a Term and the June clean-up grants
  close it. This is not the Navigator's own unit, the cases argued in a Term,
  and the panel's note says so.
- **Past Terms start at OT2018** (`GRANT_PACE_FIRST`). OT2017's window opens
  before the earliest order documents the site holds and before the 2016
  dockets its fall grants sat on, so its count would be short.

## The projection

Arithmetic, not a model. For each past Term, take what it granted between this
day of its window and 31 July; add that to this Term's count. The dashed line
is the mean of those totals day by day; the band runs from the smallest to the
largest. The summary sentence above the chart is written in R from the same
numbers, so it reads without JavaScript.

## How it is drawn

The chart is drawn in the browser (`GRANT_PACE_JS`) from a small JSON payload
in the page: an SVG scaled to a phone would shrink its 12px labels to 5px, so
the script sizes the drawing to the column and redraws on resize. Hovering
gives every Term's count on that day. Colours are the palette's tokens
(`--accent`, `--faint`, `--rule`) and `fill_palette()` placeholders; the panel
writes no hex. A `<details>` table carries the counts for anyone who cannot use
the chart.

It reaches the page through `scr_write_page(lead = )`, a slot between the
dek's rule and the table, threaded through `render_argument_nav(leads = )` and
`argument_term_page(lead = )`. Never fatal: if the grants cannot be read the
Navigator renders without the panel.
