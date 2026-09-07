# The Court's argument calendars and Day Calls (`R/argument_calendar.R`)

Status: **built** 2026-09-07, with the in-chambers opinions listing folded into
the Recent decisions failsafe the same day. These are the last three items of
`docs/data-sources.md`.

## The monthly argument calendar

`/oral_arguments/argument_calendars/MonthlyArgumentCal<Month><Year>.pdf`, one
page per sitting, linked from `/oral_arguments/calendarsandlists.aspx`, posted
about two months ahead (the October 2026 sitting's on 4 August 2026) and
amended as cases are added or moved. Two weeks side by side: a weekday
heading, then each case in the order heard, "(1)", "(2)", docket and caption,
the caption wrapping under its docket; "LEGAL HOLIDAY" fills an empty day.

The one parsing decision that mattered: **the column boundary is the leftmost
start of a docket number that is not the first on its line**, not the header's
second weekday. "Monday, March 2" sits at column 52 on the header line while
the right column's "24-1234" sits at 41, and cutting on the header merged the
right column's dockets into the left column's captions.

**What it does.** `build_argument_table()` takes the calendar. A granted case
with no "SET FOR ARGUMENT" entry yet but a place on a published calendar is
Scheduled for that day. Where the docket and the calendar both give a date
and disagree, the docket wins and the run log names the case; the calendar
is a cross-check and is amended after the fact. The manifest is
`arguments/calendar.json`; sittings from six weeks ago on are re-read every
weekly run, older ones kept as held.

## The Day Call

`/oral_arguments/daycall/Day Call_MM-DD-YY.pdf`, one per argument day, posted
that morning: each advocate down the left with affiliation, city and a
parenthetical "(20 minutes – for petitioners)"; the case down the right with
"No. 24-38.", its slot, the caption, "V." and "1 hour for argument". The
boundary is where "No." sits. An older Day Call (24 February 2026) gives no
per-advocate parenthetical; there, counsel listed above the "V." line argue for
the petitioner and those below for the respondent, which is how the page is
laid out.

**What it does.** One row per advocate -- case, slot, total minutes, name,
affiliation, side, minutes -- in `arguments/daycalls.json`, every Day Call
the index links fetched once (at most 40 a run). The Navigator's "Argued by"
shows the Day Call's line ("Hurst (pet.) · Mooppan (amicus) · Hartnett
(resp.)") for a case not yet argued; the docket's own "Argued. For
petitioner: …" entry takes over once it exists. Total time and per-advocate
minutes are kept for the Counsel Table's argument boards, not yet shown.

## In-chambers opinions

`/opinions/in-chambers.aspx`, one page for every Term, the same table shape
as the slip-opinion and relating-to-orders listings. Read once by
`fetch_opinion_listing()` alongside the others, so a single Justice's opinion
on an application (the newest as of today: Navarro v. United States, 23A843,
March 2024) gets its PDF link on a Recent decisions row the way any other
opinion does. One request, only when a row lacks a link.

## Guardrails

- **Never fatal.** The index page, a calendar or a Day Call that cannot be
  fetched or parsed is logged and skipped; the weekly's other outputs are
  unaffected, and `build_argument_table()` runs without either manifest.
- **The docket wins.** The calendar only fills a date the docket lacks; the
  Day Call only names advocates the docket has not named.
- **Event dates only** in both manifests.

## Measured, 2026-09-07

Three calendars (February, October, November 2026): 6, 7 and 7 cases, every
day dated, no caption carrying another column's text. Three Day Calls
(12 and 13 January, 24 February 2026): 3, 6 and 2 advocates, every one with a
side, minutes where the Court gave them. The index page linked 24 calendars
and 56 Day Calls.
