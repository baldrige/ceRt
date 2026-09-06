# The Court's Granted & Noted List (`R/granted_noted.R`)

Status: **built** 2026-09-06. Fetched and parsed by the weekly conferences run
(`render_arguments.R`), read by the daily; a "Separate writings" column on the
Oral Argument Navigator and the same line on the landing page's Recent
decisions rows; and an audit of the argument grammar against it, below.

## What the Court publishes

One text PDF per Term at `/orders/NNgrantednotedlist.pdf`, OT16 to the
current Term, 10–12 pages, refreshed through the Term ("As of August 4, 2026").
One block per case set for argument:

```
24-351    CFX   POSTAL SERVICE V. KONAN
                Court: USCA-5                       Granted: 4/21/25
                Argument Date: 10/8/25              Decided: 2/24/26
                Author: J. Thomas                   Other: Sotomayor (D)
                Result: VACATED AND REMANDED
```

The docket line carries a three-letter **case code** (first letter C, A or Q
for certiorari, appeal or certified question; second S, F, T, M or O for the
court below; third X, Y or H for civil, criminal or habeas), the caption, and
**flags**: `*` a unanimous Court, `**` unanimous in part, `#` unanimous in
judgment, `)` a docket consolidated with the next, `)1` a footnote keying the
docket to its own court on a shared `Court: 1USCA-7; 2USCA-2` line. A
consolidated run of docket lines shares the block that follows.

Field lines carry one or two `Label: value` pairs. A value can wrap onto the
next line (`Granted: 6/30/25 (Amended / order - 7/3/25)`; a long `Other:`
list, right-aligned under its label). Orders on the case are logged as
`Order: 6/27/25 – Cases restored to the calendar for reargument`; a
dismissal as `Date: 8/11/25 – CASE DISMISSED pursuant to Rule 46.1`; a
rearguement as `Reargument Date:`. Appeals carry `Noted:` or `Juris
Postponed:` instead of `Granted:`. An application argued on the emergency
docket appears with the code `STAY` (25A312, Trump v. Cook). The file ends
with a case-code key and summary counts, which the parser stops at.

**The separate writings** are the field the docket JSON has in no structured
form. `Other: Sotomayor (D); Kagan (C/J)` names each Justice who wrote
separately and the kind: `D` dissenting, `C` concurring, `C/J` concurring in
the judgment, `C/P` in part, `D/P` dissenting in part, `C/J/P` concurring in
the judgment in part, and compounds joined by commas or `&` ("C/P, C/J/P,
D/P"). The parser reads these token by token, so a compound it has not seen
still renders as prose.

Two things the first pass got wrong, both measured: the flags can sit a space
away from the docket ("21-432 * CFX"), and a wrapped `Result:` value ran on
into the next docket line until the docket regex allowed that space -- 27
blocks in OT20 alone were lost to it. And a value must not run on into a
section heading ("CASES (ARGUMENTS) FOR 2025 TERM"), which the parser now
treats as the end of a block.

Parsed 2026-09-06, all eleven Terms: **763 rows, 691 decided**, every decided
row with a result, 640 with an author (the rest per curiam summary
dispositions, DIGs and dismissals), 549 with separate writings.

## What the site does with it

- **`arguments/granted_noted.json`**: the manifest, Term by Term. The weekly
  fetches the current and prior Terms every run (two requests) plus any Term
  the manifest lacks, so the first run pulls all eleven and later runs pull two.
  A Term's rows are replaced whole, since a Term's list is complete every time
  it is read. Event dates only.
- **The Navigator** gains a "Separate writings" column, shown on a Term's page
  when any row has one: "Thomas and Alito dissenting; Kagan concurring in the
  judgment". Joined by docket in `render_arguments.R`.
- **Recent decisions** rows on the landing page carry the same phrase after
  the disposition ("Reversed and remanded · Kagan dissenting"), through a
  `writings` field on the decisions manifest. The daily reads the manifest;
  it never fetches the list.
- The list's author fills a row's author where the docket entry did not name
  one; the list's result is kept in the manifest for the audit and not yet
  shown.

## The audit: the argument grammar against the list

`classify_argument()` reads a case's argued date, decided date and author off
its docket entries. The list states all three independently. Joined by docket
over every decided list row the Navigator holds (527 of 691; the rest are
Terms before the archive or cases outside its grants), 2026-09-06:

| field | agree | differ | one side missing |
| --- | --- | --- | --- |
| decided date | 518 (98.3%) | 4 | 5 grammar found no decision |
| author | 498 (100% where both name one) | 0 | 20 grammar / 9 list |
| argued date | 518 (99.6%) | 2 | 7 |

Before this audit the decided-date row read 516 / 6 and the argued-date row
515 / 5. The two rows it moved were the reargued cases: Louisiana v. Callais
(24-109, 24-110, argued March 2025 and again October 2025) was "decided" on
its June 2025 reargument order, a dissent's PDF attached, ten months before
its April 2026 opinion; Knick v. Township of Scott (17-647) carried its
October 2018 argument, not the January 2019 reargument. The grammar took the
FIRST "Argued." entry. It now takes the last, and the decision after it.

What remains, and why it stays:

- **Post-grant vacaturs (5, "grammar found no decision").** 17-1679, 19-825,
  19-1212, 19-1328, 20-138: a grant followed by a Munsingwear vacatur on a
  motion, a remand to dismiss as moot, or a grant vacated and the petition
  denied. The list records each as decided; the grammar's decision forms are
  the merits forms and see nothing. The same five dockets the order-list
  audit found on the cert side. The Navigator shows them "Scheduled" or
  "Granted", which is stale but not wrong about what the Court did on the
  merits.
- **20-37, 20-38 (32 days).** The list dates the vacatur order; the grammar
  dates the "Judgment Issued" that followed.
- **21-12 (365 days).** The list says decided 5/16/21 for a case argued in
  2022; the docket says 5/16/22. The list's typo.
- **23-7809 (2 days), 24-808 (1 day).** Dates that differ by the gap between
  an order and its docket entry.
- **24-872.** The list's "As of June 30, 2026" copy still shows the original
  November 4 argument date; the docket shows the December 10 reset.

## Guardrails

- **Never fatal.** A Term that cannot be fetched or parsed contributes nothing
  and says so; the weekly's other outputs are unaffected.
- **No colour, no template bump.** The column and the line use existing
  classes.
- **Append by Term, never by row.** A partial parse of a Term would otherwise
  leave last week's rows for the cases it missed.

## Follow-ons

- Show the list's result on the Navigator where the grammar has none (the
  five vacaturs), or teach the grammar the vacatur forms.
- Per-Justice separate-writing counts for the Counsel Table's argument boards.
- Two rows for a reargued case, one per sitting, the way the list itself has
  them.
