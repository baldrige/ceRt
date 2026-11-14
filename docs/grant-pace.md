# The grant pace (Justices Term pages)

Status: **built** 2026-11-12 on the Oral Argument Navigator; **moved** 2026-11-13
to the Justices section (`R/grant_pace.R`, `R/justices.R`). Every Term page from
OT2018 on, `justices/ot<yyyy>.html`, carries a chart of the cert grants made in
that Term's window, day by day, against every other complete Term:

- **The current Term** (the window that holds today) is drawn to date, with the
  rest of the Term projected from the past Terms.
- **A finished Term** is drawn whole against the others, with no projection, and
  the summary says where it ranks.
- **OT2016 and OT2017** carry no panel (see below).

The panel sits after "The shape of the Term" and before "The writings".

## The current Term's page before its first decision

A Justices Term page used to exist only once the Term had a decision by written
opinion, so the current Term had none until December or January. Now
`render_justices()` also draws the page for the Term whose grant window is open
as soon as that Term has granted anything. Until the first decision it carries
the Court, the grant pace and a pointer to Opinion Watch (`empty = TRUE` in
`render_justices_term()`); the index lists it as "no decisions yet". The opinion
panels take over when `term_stats()` first returns a decision. Opinion Watch
links to the Term page whenever it exists ("OT2026 so far").

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
  close it. This is not the Justices pages' own unit, the cases argued in a
  Term (grants made after January are mostly argued in the next Term), and the
  panel's note says so.
- **Terms start at OT2018** (`GRANT_PACE_FIRST`). OT2017's window opens before
  the earliest order documents the site holds and before the 2016 dockets its
  fall grants sat on, so its count would be short.

## Where the grants come from

The Justices render has no dockets of its own. `render_arguments.R`, which holds
every Term's dockets, writes their grants to **`arguments/grant_dates.json`**
(`write_grant_dates()`). It merges into the file rather than replacing it, so a
run with thin dockets cannot shrink the record. `render_justices.R` reads that
file and joins the order lists' grants (`read_grant_dates()`). In
`conferences.yml` the arguments render runs first, so the file is fresh; a
standalone `render-justices.yml` run reads the last published copy plus the
current order lists.

## The projection

Arithmetic, not a model. For each past Term, take what it granted between this
day of its window and 31 July; add that to this Term's count. The dashed line
is the mean of those totals day by day; the band runs from the smallest to the
largest. The summary sentence above the chart is written in R from the same
numbers, so it reads without JavaScript.

## How it is drawn

The chart is drawn in the browser (`GRANT_PACE_JS`) from a small JSON payload
in the page (`done` switches off the projection). It is drawn in script
because an SVG scaled down to a phone would shrink its 12px labels to 5px; the
script sizes the drawing to the column and redraws on resize. Hovering gives
every Term's count on that day. Colours are the palette's tokens (`--accent`,
`--faint`, `--rule`) and `fill_palette()` placeholders; the panel writes no
hex. A `<details>` table carries the counts for anyone who cannot use the
chart. Never fatal: if the grants cannot be read, the pages render without the
panel.
