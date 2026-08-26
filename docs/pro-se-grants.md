# Pro se certiorari grants, OT2017–OT2024

How often does the Court grant **more than one** self-filed petition in a single
month, and in a single Term? Short answer: for plenary review, never twice in a
month and never twice in a Term — there were only **three** in eight Terms.
Widen "grant" to include summary dispositions and it becomes ordinary: **10 of
93 months** and **7 of 8 Terms** saw two or more.

Everything below is computed from committed data by
`.github/scripts/pro_se_grants.R`. No fetch.

## Method

| piece | choice |
| --- | --- |
| Corpus | `data-raw/ot_2017.rds` … `ot_2024.rds` — 48,985 dockets, 40,506 petitions once applications (`NNA###`) are dropped |
| Disposition | `classify_petitions()` in `R/cert_funnel.R`; `granted` = plenary merits grant, `gvr` = summary disposition in the granting order |
| Pro se | the petitioner appears as **their own attorney anywhere on the petitioner side** — any petitioner row, any attorney entry, not just counsel of record |
| Month | calendar month of the **order** that granted the petition |
| Term | reported two ways: the October Term the order fell in, and the Term encoded in the docket number |

### Why "anywhere on the petitioner side", and not `petitioner_pro_se()`

`petitioner_pro_se()` in `R/cert_model.R` tests the **counsel of record**. Run
against this question it returns **zero** pro se plenary grants — it misses all
three, and it misses them for a structural reason.

The party table on a supremecourt.gov docket is a *current snapshot*, not a
record of who filed. When the Court grants a self-filed petition the petitioner
gets merits counsel, that lawyer becomes counsel of record, and the petitioner
is demoted to a second attorney entry:

```
20-5279  Petitioner  William Dale Wooden   attys: Allon Kedem (Counsel of Record) || William Dale Wooden
18-8369  Petitioner  Arthur Lomax          attys: Brian Timothy Burgess (Counsel of Record) || Arthur J. Lomax
18-6943  Petitioner  Gregory Dean Banister attys: Brian Timothy Burgess (Counsel of Record) || Gregory Dean Banister
```

A counsel-of-record test reads all three as counselled. Sometimes the filer is
dropped from the table altogether, so even the wider test is a floor, not a
census.

**This matters beyond this question.** `pro_se` is a live feature of the cert
model, and `docs/cert_model.md` records it as "0 grants in 3,016" — a separation
so clean it needs the Firth penalty to stay finite. On the paid side that number
survives contact with this analysis (0 grants in 3,030 resolved paid pro se
petitions). On the IFP side it does not: the label flips on exactly the petitions
that get granted, so part of the observed separation is measurement, not the
Court. See **[cert_model_review_2026-07.md](cert_model_review_2026-07.md)**.

A false-negative sweep in the other direction found nothing: all 15 granted
dockets whose entries say "appointed to serve as counsel" are court-appointed
*amicus* to defend a judgment below (Sanchez-Gomez, Haymond, McGirt, Pulsifer,
Davis, Cooley, Palomar-Santiago, Gary, Twyford, Chavez-Meza, Koons, Bucklew,
Ramos, Holguin-Hernandez, Stitt), every one of them counselled at the cert stage.
The literal string "pro se" appears in the docket entries of **no** granted case.

## (a) More than one in a month

Window: order dates 2017-10 through 2025-06 — 93 calendar months, of which 80 are
months in which the Court granted anything at all. Both denominators are given
because July through September are recess and carry almost no orders.

| definition | denominator | ≥1 | **≥2** | ≥3 | max |
| --- | --- | --- | --- | --- | --- |
| plenary grants | 93 calendar months | 3 (3.2%) | **0 (0.0%)** | 0 | 1 |
| plenary grants | 80 order months | 3 (3.8%) | **0 (0.0%)** | 0 | 1 |
| + GVR / summary | 93 calendar months | 17 (18.3%) | **10 (10.8%)** | 4 (4.3%) | 5 |
| + GVR / summary | 80 order months | 17 (21.2%) | **10 (12.5%)** | 4 (5.0%) | 5 |

The ten months with two or more, counting summary dispositions:

Oct 2018 (2) · Jun 2019 (3) · **Oct 2019 (5)** · Feb 2020 (2) · Apr 2020 (2) ·
Jul 2020 (4) · Oct 2020 (2) · Jun 2022 (2) · Oct 2022 (3) · Oct 2024 (2)

Five of the ten are Octobers. That is the long-conference order list, which
disposes of a summer's accumulation in one day, and it is where a "two pro se
grants this month" headline will usually come from.

## (b) More than one in a Term

| definition | attribution | Terms ≥1 | **Terms ≥2** | max |
| --- | --- | --- | --- | --- |
| plenary | order date | 3 / 8 | **0 / 8** | 1 |
| plenary | docket number | 2 / 8 | **1 / 8** (OT2018) | 2 |
| + GVR | order date | 7 / 8 | **7 / 8** | 15 (OT2019) |
| + GVR | docket number | 7 / 8 | **6 / 8** | 13 (OT2019) |

Per Term, by the date of the order:

| Term | plenary | plenary + GVR |
| --- | --- | --- |
| OT2017 | 0 | 2 |
| OT2018 | 1 | 5 |
| OT2019 | 1 | 15 |
| OT2020 | 1 | 4 |
| OT2021 | 0 | 2 |
| OT2022 | 0 | 4 |
| OT2023 | 0 | 0 |
| OT2024 | 0 | 2 |

### The two attributions disagree on 13 of 34 cases

A petition docketed in one Term is usually disposed of in the next, so the two
labels differ whenever an order lands near the October boundary. For plenary
grants the whole difference is one case:

**Lomax** was docketed 18-8369 (OT2018) and granted 18 Oct 2019 (OT2019). Put it
with Banister by docket number and OT2018 has two pro se grants; put it with its
order date and no Term has more than one. That single case is the difference
between "it has happened once in eight Terms" and "it has never happened."

The other twelve are all summary dispositions crossing the same boundary:
17-8035, 18-8911, 18-6098, 18-8801, 19-8126, 19-8149, 20-7934, 21-6179, 21-7933,
21-8148, 23-7504, 23-7602.

Term boundaries here are the real ones — first Monday in October (2 Oct 2017,
1 Oct 2018, 7 Oct 2019, 5 Oct 2020, 4 Oct 2021, 3 Oct 2022, 2 Oct 2023, 7 Oct
2024). Ten of the thirty-four orders land in the first week of an October, and
every one of them falls on or after that Term's first Monday, so a lazier
1-October rule would happen to agree on all 34. It would not stay lucky: the gap
between 1 October and the first Monday is up to six days wide, and an order
inside it belongs to the Term that is ending, not the one about to open.

## The three plenary grants

| docket | order date | Term (order) | case |
| --- | --- | --- | --- |
| **18-6943** | 2019-06-24 | OT2018 | Banister v. Davis — Rule 59(e) motions and second-or-successive habeas |
| **18-8369** | 2019-10-18 | OT2019 | Lomax v. Ortiz-Marquez — PLRA three-strikes |
| **20-5279** | 2021-02-22 | OT2020 | Wooden v. United States — ACCA "occasions" |

All three IFP; one apiece in three consecutive Terms; none since February 2021.

## Base rates

| | resolved petitions | plenary grants | rate | GVR / summary |
| --- | --- | --- | --- | --- |
| counselled | 20,223 | 531 | 2.626% | 535 |
| pro se | 19,554 | 3 | **0.015%** | 31 |

A ~170× gap. Split by fee status, the pro se side is entirely IFP:

| | resolved | plenary grants | GVR / summary |
| --- | --- | --- | --- |
| IFP, counselled | 11,439 | 33 | 256 |
| IFP, pro se | 16,524 | **3** | 28 |
| paid, counselled | 8,784 | 498 | 279 |
| paid, pro se | 3,030 | **0** | 3 |

## The full roll — all 34 pro se dispositions

| # | docket | order date | OT (order) | OT (docket) | type | disposition | case |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | 17-6926 | 2018-05-14 | OT2017 | OT2017 | ifp | GVR / summary | Lavada Carreon v. United States |
| 2 | 17-6904 | 2018-06-11 | OT2017 | OT2017 | ifp | GVR / summary | Raymond Stern v. United States |
| 3 | 17-8035 | 2018-10-01 | OT2018 | OT2017 | ifp | GVR / summary | Erick Manners v. United States |
| 4 | 18-5184 | 2018-10-15 | OT2018 | OT2018 | ifp | GVR / summary | Ahmad Sayed Hashimi v. United States |
| 5 | **18-6943** | 2019-06-24 | OT2018 | OT2018 | ifp | **plenary grant** | Gregory Dean Banister v. Lorie Davis, Director, TDCJ |
| 6 | 18-294 | 2019-06-28 | OT2018 | OT2018 | paid | GVR / summary | Nicholas Honchariw v. County of Stanislaus, California |
| 7 | 18-7439 | 2019-06-28 | OT2018 | OT2018 | ifp | GVR / summary | Gregory M. Ward v. United States |
| 8 | 18-8911 | 2019-10-07 | OT2019 | OT2018 | ifp | GVR / summary | Gerald Humbert v. United States |
| 9 | 19-5260 | 2019-10-07 | OT2019 | OT2019 | ifp | GVR / summary | Michael Levon Jackson v. United States |
| 10 | 19-5014 | 2019-10-15 | OT2019 | OT2019 | ifp | GVR / summary | Ricardo Donate-Cardona v. United States |
| 11 | **18-8369** | 2019-10-18 | OT2019 | OT2018 | ifp | **plenary grant** | Arthur J. Lomax v. Christina Ortiz-Marquez, et al. |
| 12 | 19-5217 | 2019-10-21 | OT2019 | OT2019 | ifp | GVR / summary | Lavares Detroen Watkins v. United States |
| 13 | 19-5501 | 2019-11-12 | OT2019 | OT2019 | ifp | GVR / summary | Mandrail Jamar Woodberry v. United States |
| 14 | 19-6496 | 2020-02-24 | OT2019 | OT2019 | ifp | GVR / summary | David Elijah Smith v. United States |
| 15 | 19-6871 | 2020-02-24 | OT2019 | OT2019 | ifp | GVR / summary | Justin Vazquez v. United States |
| 16 | 19-5133 | 2020-04-20 | OT2019 | OT2019 | ifp | GVR / summary | Andrew Anthony Brown v. Barr, Attorney General |
| 17 | 19-5989 | 2020-04-27 | OT2019 | OT2019 | ifp | GVR / summary | Errol Victor, Sr. v. Louisiana |
| 18 | 19-5990 | 2020-06-08 | OT2019 | OT2019 | ifp | GVR / summary | David Alan Vogel v. United States |
| 19 | 18-6098 | 2020-07-09 | OT2019 | OT2018 | ifp | GVR / summary | Joe Johnson, Jr. v. Oklahoma |
| 20 | 18-8801 | 2020-07-09 | OT2019 | OT2018 | ifp | GVR / summary | Patrick Joseph Terry v. Oklahoma |
| 21 | 19-5417 | 2020-07-09 | OT2019 | OT2019 | ifp | GVR / summary | Travis Wayne Bentley v. Oklahoma |
| 22 | 19-6428 | 2020-07-09 | OT2019 | OT2019 | ifp | GVR / summary | Keith Elmo Davis v. Oklahoma |
| 23 | 19-8126 | 2020-10-05 | OT2020 | OT2019 | ifp | GVR / summary | Garry Wayne Wilson v. Oklahoma |
| 24 | 19-8149 | 2020-10-05 | OT2020 | OT2019 | ifp | GVR / summary | Nathaniel Lambert v. Louisiana |
| 25 | 20-5363 | 2021-01-11 | OT2020 | OT2020 | ifp | GVR / summary | Reginald Jones v. Louisiana |
| 26 | **20-5279** | 2021-02-22 | OT2020 | OT2020 | ifp | **plenary grant** | William Dale Wooden v. United States |
| 27 | 20-7934 | 2022-06-30 | OT2021 | OT2020 | ifp | GVR / summary | John Patrick Couch v. United States |
| 28 | 21-1008 | 2022-06-30 | OT2021 | OT2021 | paid | GVR / summary | Andres Mencia v. United States |
| 29 | 21-6179 | 2022-10-03 | OT2022 | OT2021 | ifp | GVR / summary | Leoncio Perez v. United States |
| 30 | 21-7933 | 2022-10-03 | OT2022 | OT2021 | ifp | GVR / summary | John Armstrong, Jr. v. United States |
| 31 | 21-8148 | 2022-10-03 | OT2022 | OT2021 | ifp | GVR / summary | Frank H. Bynes Jr. v. United States |
| 32 | 22-683 | 2023-06-26 | OT2022 | OT2022 | paid | GVR / summary | Melina Darzzete Guillen-Perez v. Garland, Attorney General |
| 33 | 23-7504 | 2024-10-07 | OT2024 | OT2023 | ifp | GVR / summary | Sergio Antonio Hood v. United States |
| 34 | 23-7602 | 2024-10-07 | OT2024 | OT2023 | ifp | GVR / summary | Melina Darzzete Guillen-Perez v. Garland, Attorney General |

Clusters, not a scatter. Rows 19–22 are all dated 9 July 2020 — the day *McGirt*
came down — and row 23 is the same wave in October; rows 17, 24 and 25 are
*Ramos* GVRs out of Louisiana; the long run of `v. United States` GVRs is
*Rehaif*, *Davis* and *Borden* working their way back down. Guillen-Perez appears
twice, on two dockets, four years apart. Only three paid pro se petitions were
ever granted anything at all in eight Terms (rows 6, 28, 32) and none of them
plenary review — the pro se story is an IFP story.

## Three classifier defects this analysis surfaced

Found by asking what a specific number meant, not by any aggregate going red.
All three fixed in `GRANT_FORMS`; the corpus-wide diff is exactly four petitions
(`granted` 532 → 534, `gvr` 565 → 566, `pending` 732 → 729).

### 1. The split IFP grant written with "and" matched nothing

`GRANT_FORMS` carried two alternatives for the split-motion grant — one
requiring `and the petition`, one requiring a bare `petition`. The Court's
fourth combination, `GRANTED, and petition for a writ of certiorari GRANTED
limited to …`, matched neither. That is how **18-6943, Banister v. Davis** is
written, so a case the Court granted, heard and decided sat at `pending`,
invisible to the funnel and dropped from the model's training corpus. One of the
three pro se plenary grants in eight Terms was the case that exposed it.

The two alternatives are now one, with `and` and `the` independently optional.

### 2. "treat the application as a petition" — only the past participle matched

The Court writes the application-treated-as-certiorari order in two voices. The
pattern matched only *"is **treated** as a petition for a writ of certiorari"*,
so *"applicants suggested this Court **treat** the application as a petition for
a writ of certiorari; doing so, the petition is granted"* fell straight through.
**24-1177 (A.A.R.P. v. Trump)** and **24-1246 (U.S. DOGE Service v. CREW)** both
sat at `pending`; both are now `gvr`, which is what they are — cert granted,
judgment below vacated, remanded, in one order.

### 3. Allen v. Milligan was filed under "summary disposition"

**21-1087** classified as `gvr` dated 2023-06-08 — the merits judgment — because
no grant form matched its order and the standalone `^Adjudged to be AFFIRMED`
rule caught the affirmance instead. It is now `granted` dated **2022-02-07**, the
day the Court granted certiorari before judgment. A fully argued,
opinion-bearing case was being counted as a summary disposition, and its date was
sixteen months late.

The cause is worth noting because it is not the wording: the old pattern needed
`granted` within 250 characters of `treated as a petition …`, and in this order
the intervening recital of the respondents' position pushes it just past the
window. The active-voice alternative added for defect 2 happens to match closer
in and rescues it. A span limit is a silent cutoff — nothing errors when it is
one clause too short.

## Caveats

- **Left edge.** Grants issued in the first months of the window can belong to
  OT2016 petitions, which are not in the archives. About 22% of grants are of a
  petition docketed in the *previous* Term, so Oct–Dec 2017 undercounts.
- **Right edge.** The OT2024 archive was fetched 2025-07-21 and OT2025 is absent
  entirely, so the last months are truncated. Neither edge moves either answer:
  all three plenary grants fall in 2019–2021.
- **The pro se flag is a floor.** It reads a current snapshot of the party
  table. Where a granted petitioner's own name was dropped rather than demoted,
  the case reads as counselled and is not here.
- **GVR ≠ merits win.** A summary disposition is the Court sending the case back
  in light of an intervening decision, not agreeing to hear it. The two
  definitions answer different questions and the honest report gives both.
