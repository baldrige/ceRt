# Audit: the cert funnel against the Court's order lists (September 2026)

Run 2026-09-05 with `.github/scripts/audit_order_lists.R` over every order
document the site holds for OT17 through OT25: 993 documents with parsed
entries, 51,305 entries, 2 October 2017 to 4 September 2026, against the
funnel's reading (`classify_petitions()`) of the same dockets in the local
snapshots (`data-raw/ot_2017.rds` … `ot_2024.rds`, `snapshot_25.rds`,
`snapshot_26.rds`, `snapshot_refresh.rds`; 55,496 dockets).

## The result

| | entries |
| --- | --- |
| list entries about a petition docket that say granted, GVR'd, denied or dismissed | 41,318 |
| of which the snapshots hold the docket and postdate the order | **40,292** |
| agree on the outcome | **40,274 (99.96%)** |
| agree on the outcome and the date | 40,273 (99.95%) |
| disagree | 18 |

| Term of the docket | compared | agree | % |
| --- | --- | --- | --- |
| OT17 | 5,935 | 5,932 | 99.95 |
| OT18 | 6,073 | 6,071 | 99.97 |
| OT19 | 5,075 | 5,069 | 99.88 |
| OT20 | 4,965 | 4,960 | 99.90 |
| OT21 | 4,062 | 4,061 | 99.98 |
| OT22 | 3,507 | 3,506 | 99.97 |
| OT23 | 3,894 | 3,894 | 100.00 |
| OT24 | 3,459 | 3,459 | 100.00 |
| OT25 | 3,307 | 3,307 | 100.00 |

The funnel's grammar holds. Of the 18 disagreements, 11 are the funnel
reporting the cert-stage outcome where the list reports a later merits-stage
one, 2 are the audit misreading a list, and **5 are real gaps**, each a single
docket in nine Terms. They are listed below with the form the grammar does not
know.

## What the audit had to learn to read a list

Most of the work was on the audit's side, not the funnel's. The first pass
scored 97.3% agreement, and every point below that was the audit reading a
section heading as the outcome. The section is a container; the entry's own
text governs:

- **"CERTIORARI DENIED" holds things that are not cert denials.** The Rule
  39.8 form ("The motion for leave to proceed in forma pauperis is denied, and
  the petition is dismissed") sits there, and the funnel rightly says
  *dismissed* -- 115 of them in OT24–25, all dated the same day. So does the
  IFP-denied-but-pending form ("... is denied. Petitioner is allowed until
  [date] to pay the docketing fee"), where the petition is still live and the
  funnel rightly says *pending*. So do denials of motions for reconsideration
  on dockets long since dismissed.
- **A motion's sentence is not the petition's.** "The motion to expedite
  consideration of the petition for a writ of certiorari is granted" grants
  nothing but the motion. The audit now reads sentence by sentence and takes
  a grant only where the petition phrase opens the sentence or follows "and"
  ("The motion ... and the petition for a writ of certiorari are granted").
- **"No." is not a sentence end.** "The petition for a writ of certiorari in
  No. 18-776 is granted" spans a period. Flattened before matching.
- **The emergency docket grants in its own words.** "The application ... is
  treated as a petition for a writ of certiorari before judgment, and the
  petition is granted" is a grant, in a miscellaneous order, in no section.
- **An appeal is granted by noting probable jurisdiction**, or by postponing
  the question of jurisdiction to the hearing. The funnel files those as
  granted; so does the audit now.
- **A grant that vacates and remands in the same order is a GVR** wherever the
  Court files it (25-162, Tennessee v. Kennedy, under CERTIORARI GRANTED).

## The 18 disagreements

**Cert-stage grant, later merits-stage disposition (11).** The list names the
docket in a summary-disposition section months after the grant: a Munsingwear
vacatur on a motion after the case was removed from the calendar (19-1212,
19-1328, 20-138, 20-37, 20-38), a remand with instructions to dismiss after
mootness (17-1679, 22-425, 18-557), a writ before judgment dismissed as
improvidently granted (21-1596), a Court-appointed amicus entry that the
audit took for a run of GVRs (18-7739), and 24-1246 (DOGE v. CREW), where the
list says granted and the funnel says GVR, and the funnel is the more precise.
The funnel reports what happened to the *petition*; these are what happened to
the *case*. Not errors.

**Audit misreads (2).** 19-1389, a motion to expedite denied, filed by the
Court in a run the parser grouped under a GVR text; and 18-7739 above.

**Real gaps in the grammar (5).**

| docket | what the docket says | what the funnel says |
| --- | --- | --- |
| 19-8695 Gutierrez v. Saenz | "The motion ... for leave to proceed in forma pauperis and the petition ... are granted. The June 12, 2020 **order** ... is vacated, and the case is remanded ..." (25 Jan 2021) | granted -- the GVR form vacates an *order*, not a *judgment* |
| 20-74 Iancu v. Luoma | "Motion to dismiss under Rule 46.2 GRANTED. The petition ... is dismissed **only with respect to** the judgment of ..." (8 Oct 2020), then GVR'd 28 Jun 2021 | dismissed -- a partial dismissal read as terminal |
| 19-825 FTC v. Credit Bureau Center | granted 9 Jul 2020; "The July 9, 2020 order granting the petition ... is vacated" (9 Nov 2020); "Petition DENIED" (3 May 2021) | granted -- a vacated grant is not unwound |
| 17-243 Abdirahman v. United States | denied 28 Jun 2018; "The petition for rehearing is granted. The order ... denying the petition ... is vacated as to petitioner ... The petition ... is granted. The judgment is vacated ..." (6 Aug 2018) | pending -- the rehearing grant resets the outcome and the GVR that follows is not recognised |
| 19-373 Walker v. United States | granted 15 Nov 2019; "It appearing that petitioner died ..., the petition ... is DISMISSED" (27 Jan 2020) | granted -- a post-grant dismissal in a form other than Rule 46 |

Five dockets in 40,292. None is worth a grammar change on its own; they are
recorded so that the next one has company. The two that could matter for a
product are 20-74 (a case the relist and hold logic would have treated as gone
for eight months while it was being held for Arthrex) and 17-243 (a docket
"pending" forever, which the pending cache ages out after two years).

## The reverse check: funnel grants the lists do not name as grants

602 funnel grants fall inside the lists' window; 30 are not matched to a grant
in any list. They sort into:

- **The Court's own broken link.** The 3 October 2022 order list -- the first
  Monday of OT22, with the Term's summer grants -- returns 404 from the Court's
  listing (`100322zor`). Nine grants dated that day (21-1333, 21-1397, 21-1436,
  21-1449, 21-1450, 21-1454, 21-1496, 21-887, 22-96) have no list to match.
- **Text the first parser truncated.** A wrapped grant like "The petitions for
  writs of certiorari in No. 20-1530, No. / 20-1531, No. 20-1778, and No.
  20-1780 are granted" had its second line read as a new docket, because it
  begins with one. Parser p2 (same PR) treats a docket-leading line with
  lowercase words as prose. Re-parsed the same night: the five West Virginia
  v. EPA dockets and the two tariffs cases (24-1287, 25-250) resolved, and the
  reverse list fell from 37 to 24. Two grants are still not located in any
  document the listing carries: 25-332 (Trump v. Slaughter, 22 Sep 2025) and
  20-1539 (18 Oct 2021).
- **Forms the audit does not read** and has no need to: "further consideration
  of the *questions* of jurisdiction *are* postponed" (17-586, 17-626), a writ
  of mandamus treated as a petition (18-557), and a few grants whose nearest
  list entry is a later motion (21-588, 22-592, 23-411, 21-1086, 21-1087),
  meaning the granting document is one the listing does not carry.

## Docket data coverage, for the record

1,034 list entries name a docket the snapshots do not hold: 938 OT16 dockets
denied in OT17 lists (before the archive begins), **92 OT24 dockets** above
the top of `ot_2024.rds`, which was taken before that Term's docket closed, 3
OT15 and 1 OT23. The OT24 gap is the one worth fixing: a `fetch_term` run for
OT24 and a refreshed snapshot.

## How to run it again

```
git archive origin/gh-pages orders/orders.json orders/data | tar -x -C /tmp/o
Rscript .github/scripts/audit_order_lists.R /tmp/o/orders /tmp/audit.md
```

The report is Markdown; the comparison frame is saved beside it as `.rds`.
