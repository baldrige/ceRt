# Pro se certiorari grants, OT2017–OT2026

How often does the Court grant **more than one** self-filed petition in a single
month, and in a single Term? It has done each exactly once, and both times were
the same event: **June 2026**, when the Court granted certiorari to William
Maxwell on the 1st and to Daniel Grand on the 30th. That is one month in 106 and
one Term in nine. Across the whole record there have been **five** pro se
plenary grants. Widen "grant" to include summary dispositions and it is ordinary:
**11 of 106 months** and **8 of 9 Terms**.

Everything below is computed by `.github/scripts/pro_se_grants.R`.

## Method

| piece | choice |
| --- | --- |
| Corpus | `data-raw/ot_2017.rds` … `ot_2024.rds` plus `data-raw/snapshot_*.rds` — 55,496 dockets, 45,552 petitions once applications (`NNA###`) are dropped |
| Why snapshots | OT2025 and OT2026 are not in the committed archives, and an OT2024 petition granted after the July 2025 archive snapshot is invisible in it. `snapshot_25` / `snapshot_26` are full-Term fetches; `snapshot_refresh` re-fetches every petition the archives still had at `pending`. They are deliberately **outside** the `ot_*.rds` glob — see below |
| Disposition | `classify_petitions()` in `R/cert_funnel.R`; `granted` = plenary merits grant, `gvr` = summary disposition in the granting order |
| Pro se | the petitioner appears as **their own attorney anywhere on the petitioner side** — any petitioner row, any attorney entry, not just counsel of record |
| Month | calendar month of the **order** that granted the petition |
| Term | reported two ways: the October Term the order fell in, and the Term encoded in the docket number |

### Why the new Terms are `snapshot_*`, not `ot_2025.rds`

Dropping an `ot_2025.rds` into `data-raw/` would silently re-base three other
things: the Cert Funnel (`render_funnel.R`), the Counsel Table
(`R/counsel_table.R`) and the cert model's training corpus
(`train_cert_model.R`) all glob `data-raw/ot_*.rds`. Each would need its
committed artifact regenerated, and the model retrained, before the site was
consistent again. That is a separate change from answering this question, so the
snapshots sit next to the archives under a name the glob does not match — the
same trick `fetch_pending.R` uses for `cases-pending.rds`.

The re-fetch was worth its six minutes on its own: it moved **23-7541, Barnes v.
Felix**, from `pending` to a GVR dated 27 May 2025, twelve days after the Court
decided the lead case. About 22% of a Term's grants are of a petition docketed
the Term before, so without the refresh OT2025's numbers would have been short.

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
25-5930  Petitioner  William Maxwell       attys: Masha Godina Hansford (Counsel of Record) || William Maxwell
25-965   Petitioner  Daniel Grand          attys: E. Joshua Rosenkranz (Counsel of Record) || Daniel Grand
```

A counsel-of-record test reads all five as counselled. Sometimes the filer is
dropped from the table altogether, so even the wider test is a floor, not a
census.

The live schema carries two fields the archives do not, and on the two OT2025
grants they corroborate the name match independently: the filer's row has **no
firm** and a **non-firm address**. Maxwell's is "FCI Beaumont, P.O. Box 26020,
Beaumont, TX"; Grand's is a house on Miramar Blvd. in University Heights, the
city he is suing. Davis Polk and Orrick, respectively, appear only as counsel of
record — after the grant.

**This matters beyond this question.** `pro_se` is a live feature of the cert
model, and `docs/cert_model.md` records it as "0 grants in 3,016" — a separation
so clean it needs the Firth penalty to stay finite. Two things have happened to
that number. On the IFP side the label flips on exactly the petitions that get
granted, so part of the separation was always measurement rather than the Court.
And on the paid side it is **no longer zero**: Grand (25-965) is one grant in
3,398 resolved paid pro se petitions. The cell that justified the penalty is not
empty any more, and the next retrain will be fitting a different shape. See
**[cert_model_review_2026-07.md](cert_model_review_2026-07.md)**.

A false-negative sweep in the other direction found nothing: all 15 granted
dockets whose entries say "appointed to serve as counsel" are court-appointed
*amicus* to defend a judgment below (Sanchez-Gomez, Haymond, McGirt, Pulsifer,
Davis, Cooley, Palomar-Santiago, Gary, Twyford, Chavez-Meza, Koons, Bucklew,
Ramos, Holguin-Hernandez, Stitt), every one of them counselled at the cert stage.
The literal string "pro se" appears in the docket entries of **no** granted case.

## (a) More than one in a month

Window: order dates 2017-10 through 2026-07 — 106 calendar months, of which 92 are
months in which the Court granted anything at all. Both denominators are given
because July through September are recess and carry almost no orders.

| definition | denominator | ≥1 | **≥2** | ≥3 | max |
| --- | --- | --- | --- | --- | --- |
| plenary grants | 106 calendar months | 4 (3.8%) | **1 (0.9%)** | 0 | 2 |
| plenary grants | 92 order months | 4 (4.3%) | **1 (1.1%)** | 0 | 2 |
| + GVR / summary | 106 calendar months | 19 (17.9%) | **11 (10.4%)** | 5 (4.7%) | 5 |
| + GVR / summary | 92 order months | 19 (20.7%) | **11 (12.0%)** | 5 (5.4%) | 5 |

The one month with two plenary grants is **June 2026**. It is also the busiest
pro se month on any definition except October 2019: four dispositions in thirty
days — Maxwell granted on the 1st, Olivarria GVR'd on the 8th, Morrison GVR'd on
the 29th, Grand granted on the 30th.

The eleven months with two or more, counting summary dispositions:

Oct 2018 (2) · Jun 2019 (3) · **Oct 2019 (5)** · Feb 2020 (2) · Apr 2020 (2) ·
Jul 2020 (4) · Oct 2020 (2) · Jun 2022 (2) · Oct 2022 (3) · Oct 2024 (2) ·
**Jun 2026 (4)**

Five of the eleven are Octobers. That is the long-conference order list, which
disposes of a summer's accumulation in one day, and it is where a "two pro se
grants this month" headline usually comes from. June 2026 is the exception: two
separate late-Term order lists, four weeks apart.

## (b) More than one in a Term

Fractions are over **OT2017–OT2025**, nine Terms. OT2026 is excluded: its
petitions are being docketed but the Term does not open until 5 October 2026, so
it contributes a denominator and no grants and would drag every rate down.

| definition | attribution | Terms ≥1 | **Terms ≥2** | max |
| --- | --- | --- | --- | --- |
| plenary | order date | 4 / 9 | **1 / 9** (OT2025) | 2 |
| plenary | docket number | 3 / 9 | **2 / 9** (OT2018, OT2025) | 2 |
| + GVR | order date | 8 / 9 | **8 / 9** | 15 (OT2019) |
| + GVR | docket number | 8 / 9 | **7 / 9** | 13 (OT2019) |

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
| OT2024 | 0 | 3 |
| **OT2025** | **2** | **4** |
| OT2026 (open) | 0 | 0 |

OT2025 is the first Term in the record with two, and it ends a four-Term drought:
between Wooden in February 2021 and Maxwell in June 2026 the Court granted no
self-filed petition plenary review at all.

### The two attributions disagree on 14 of 39 cases

A petition docketed in one Term is usually disposed of in the next, so the two
labels differ whenever an order lands near the October boundary. Among plenary
grants only one case moves:

**Lomax** was docketed 18-8369 (OT2018) and granted 18 Oct 2019 (OT2019). Put it
with Banister by docket number and OT2018 joins OT2025 as a two-grant Term; put
it with its order date and OT2025 stands alone. That single case is the whole
difference between one Term in nine and two.

The other thirteen are summary dispositions crossing the same boundary: 17-8035,
18-8911, 18-6098, 18-8801, 19-8126, 19-8149, 20-7934, 21-6179, 21-7933, 21-8148,
23-7504, 23-7541, 23-7602.

Term boundaries here are the real ones — first Monday in October (2 Oct 2017,
1 Oct 2018, 7 Oct 2019, 5 Oct 2020, 4 Oct 2021, 3 Oct 2022, 2 Oct 2023, 7 Oct
2024, 6 Oct 2025). Ten of the thirty-nine orders land in the first week of an
October, and every one of them falls on or after that Term's first Monday, so a
lazier 1-October rule would happen to agree on all 39. It would not stay lucky:
the gap between 1 October and the first Monday is up to six days wide, and an
order inside it belongs to the Term that is ending, not the one about to open.

## The five plenary grants

| docket | order date | Term | fee | case |
| --- | --- | --- | --- | --- |
| **18-6943** | 2019-06-24 | OT2018 | IFP | Banister v. Davis — Rule 59(e) motions and second-or-successive habeas |
| **18-8369** | 2019-10-18 | OT2019 | IFP | Lomax v. Ortiz-Marquez — PLRA three-strikes |
| **20-5279** | 2021-02-22 | OT2020 | IFP | Wooden v. United States — ACCA "occasions" |
| **25-5930** | 2026-06-01 | OT2025 | IFP | Maxwell v. Dinis — granted limited to one question, argument set for 2 Nov 2026 |
| **25-965** | 2026-06-30 | OT2025 | **paid** | Grand v. City of University Heights — the first paid pro se grant in the record |

The first four follow the familiar pattern: a prisoner files in forma pauperis,
the Court grants, and merits counsel is appointed. Maxwell filed from FCI
Beaumont in August 2025, was relisted once, and drew a respondent who then
supported vacatur — so the Court appointed Brian W. Barnes to defend the judgment
below as amicus.

**Grand is the outlier.** He paid the docketing fee, filed from his home in the
city he is suing, and attracted eight amicus briefs *before* the grant — Pacific
Justice Institute, the ACLJ, Agudath Israel, the U.S. Conference of Catholic
Bishops among them. Until 30 June 2026 the record held 3,398 resolved paid pro se
petitions and not one grant. It is the sort of case that ought to make anyone
reading the `pro_se` coefficient in the cert model uneasy: a self-filed petition
with an organised amicus campaign behind it is not the self-filed petition the
coefficient was fitted on.

## Base rates

| | resolved petitions | plenary grants | rate | GVR / summary |
| --- | --- | --- | --- | --- |
| counselled | 22,615 | 595 | 2.631% | 617 |
| pro se | 21,456 | 5 | **0.023%** | 34 |

A ~113× gap. Split by fee status:

| | resolved | plenary grants | rate | GVR / summary |
| --- | --- | --- | --- | --- |
| IFP, counselled | 12,683 | 37 | 0.292% | 282 |
| IFP, pro se | 18,058 | **4** | 0.022% | 31 |
| paid, counselled | 9,932 | 558 | 5.618% | 335 |
| paid, pro se | 3,398 | **1** | 0.029% | 3 |

The paid pro se cell held at zero for eight Terms and broke on the last day of
June 2026. One grant in 3,398 is not a rate anyone should quote as a rate.

Both OT2025 grants were relisted once, and in both the Court called for a
response — the ordinary engagement signals, on petitions with no lawyer behind
them.

## The full roll — all 39 pro se dispositions

All 39, in order. Five plenary grants; thirty-four summary dispositions. The Term
columns show both attributions — where they differ, the order crossed an October
boundary.

| # | docket | order date | OT (order) | OT (docket) | type | disposition | case |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | 17-6926 | 2018-05-14 | OT2017 | OT2017 | IFP | GVR / summary | Lavada Carreon, Petitioner v. United States |
| 2 | 17-6904 | 2018-06-11 | OT2017 | OT2017 | IFP | GVR / summary | Raymond Stern, Petitioner v. United States |
| 3 | 17-8035 | 2018-10-01 | OT2018 | OT2017 | IFP | GVR / summary | Erick Manners, Petitioner v. United States |
| 4 | 18-5184 | 2018-10-15 | OT2018 | OT2018 | IFP | GVR / summary | Ahmad Sayed Hashimi, Petitioner v. United States |
| 5 | **18-6943** | 2019-06-24 | OT2018 | OT2018 | IFP | **plenary grant** | Gregory Dean Banister, Petitioner v. Lorie Davis, Director, Texas Department of Criminal Justice, Correctional Institutions Division |
| 6 | 18-294 | 2019-06-28 | OT2018 | OT2018 | Paid | GVR / summary | Nicholas Honchariw, Petitioner v. County of Stanislaus, California, et al. |
| 7 | 18-7439 | 2019-06-28 | OT2018 | OT2018 | IFP | GVR / summary | Gregory M. Ward, Petitioner v. United States |
| 8 | 18-8911 | 2019-10-07 | OT2019 | OT2018 | IFP | GVR / summary | Gerald Humbert, Petitioner v. United States |
| 9 | 19-5260 | 2019-10-07 | OT2019 | OT2019 | IFP | GVR / summary | Michael Levon Jackson, Petitioner v. United States |
| 10 | 19-5014 | 2019-10-15 | OT2019 | OT2019 | IFP | GVR / summary | Ricardo Donate-Cardona, Petitioner v. United States |
| 11 | **18-8369** | 2019-10-18 | OT2019 | OT2018 | IFP | **plenary grant** | Arthur J. Lomax, Petitioner v. Christina Ortiz-Marquez, et al. |
| 12 | 19-5217 | 2019-10-21 | OT2019 | OT2019 | IFP | GVR / summary | Lavares Detroen Watkins, Petitioner v. United States |
| 13 | 19-5501 | 2019-11-12 | OT2019 | OT2019 | IFP | GVR / summary | Mandrail Jamar Woodberry, Petitioner v. United States |
| 14 | 19-6496 | 2020-02-24 | OT2019 | OT2019 | IFP | GVR / summary | David Elijah Smith, Petitioner v. United States |
| 15 | 19-6871 | 2020-02-24 | OT2019 | OT2019 | IFP | GVR / summary | Justin Vazquez, Petitioner v. United States |
| 16 | 19-5133 | 2020-04-20 | OT2019 | OT2019 | IFP | GVR / summary | Andrew Anthony Brown, Petitioner v. William P. Barr, Attorney General of the United States |
| 17 | 19-5989 | 2020-04-27 | OT2019 | OT2019 | IFP | GVR / summary | Errol Victor, Sr., Petitioner v. Louisiana |
| 18 | 19-5990 | 2020-06-08 | OT2019 | OT2019 | IFP | GVR / summary | David Alan Vogel, Petitioner v. United States |
| 19 | 18-6098 | 2020-07-09 | OT2019 | OT2018 | IFP | GVR / summary | Joe Johnson, Jr., Petitioner v. Oklahoma |
| 20 | 18-8801 | 2020-07-09 | OT2019 | OT2018 | IFP | GVR / summary | Patrick Joseph Terry, Petitioner v. Oklahoma |
| 21 | 19-5417 | 2020-07-09 | OT2019 | OT2019 | IFP | GVR / summary | Travis Wayne Bentley, Petitioner v. Oklahoma |
| 22 | 19-6428 | 2020-07-09 | OT2019 | OT2019 | IFP | GVR / summary | Keith Elmo Davis, Petitioner v. Oklahoma |
| 23 | 19-8126 | 2020-10-05 | OT2020 | OT2019 | IFP | GVR / summary | Garry Wayne Wilson, Petitioner v. Oklahoma |
| 24 | 19-8149 | 2020-10-05 | OT2020 | OT2019 | IFP | GVR / summary | Nathaniel Lambert, Petitioner v. Louisiana |
| 25 | 20-5363 | 2021-01-11 | OT2020 | OT2020 | IFP | GVR / summary | Reginald Jones, Petitioner v. Louisiana |
| 26 | **20-5279** | 2021-02-22 | OT2020 | OT2020 | IFP | **plenary grant** | William Dale Wooden, Petitioner v. United States |
| 27 | 20-7934 | 2022-06-30 | OT2021 | OT2020 | IFP | GVR / summary | John Patrick Couch, Petitioner v. United States |
| 28 | 21-1008 | 2022-06-30 | OT2021 | OT2021 | Paid | GVR / summary | Andres Mencia, Petitioner v. United States |
| 29 | 21-6179 | 2022-10-03 | OT2022 | OT2021 | IFP | GVR / summary | Leoncio Perez, Petitioner v. United States |
| 30 | 21-7933 | 2022-10-03 | OT2022 | OT2021 | IFP | GVR / summary | John Armstrong, Jr., Petitioner v. United States |
| 31 | 21-8148 | 2022-10-03 | OT2022 | OT2021 | IFP | GVR / summary | Frank H. Bynes Jr., Petitioner v. United States |
| 32 | 22-683 | 2023-06-26 | OT2022 | OT2022 | Paid | GVR / summary | Melina Darzzete Guillen-Perez, Petitioner v. Merrick B. Garland, Attorney General |
| 33 | 23-7504 | 2024-10-07 | OT2024 | OT2023 | IFP | GVR / summary | Sergio Antonio Hood, Petitioner v. United States |
| 34 | 23-7602 | 2024-10-07 | OT2024 | OT2023 | IFP | GVR / summary | Melina Darzzete Guillen-Perez, Petitioner v. Merrick B. Garland, Attorney General |
| 35 | 23-7541 | 2025-05-27 | OT2024 | OT2023 | IFP | GVR / summary | Tommy Duane Barnes, Petitioner v. Roberto Felix, Jr., et al. |
| 36 | **25-5930** | 2026-06-01 | OT2025 | OT2025 | IFP | **plenary grant** | William Maxwell, Petitioner v. Sandra Dinis, Acting Warden |
| 37 | 25-6544 | 2026-06-08 | OT2025 | OT2025 | IFP | GVR / summary | Louis Olivarria, Petitioner v. California |
| 38 | 25-6385 | 2026-06-29 | OT2025 | OT2025 | IFP | GVR / summary | Johnathan Morrison, Petitioner v. United States |
| 39 | **25-965** | 2026-06-30 | OT2025 | OT2025 | Paid | **plenary grant** | Daniel Grand, Petitioner v. City of University Heights, Ohio, et al. |

Clusters, not a scatter. Rows 19–22 are all dated 9 July 2020 — the day *McGirt*
came down — and row 23 is the same wave in October; rows 17, 24 and 25 are
*Ramos* GVRs out of Louisiana; row 35 followed *Barnes v. Felix* by twelve days;
the long run of `v. United States` GVRs is *Rehaif*, *Davis* and *Borden* working
their way back down. Guillen-Perez appears twice, on two dockets, four years
apart. Three of the four paid rows are GVRs; the fourth is Grand.

## Three classifier defects this analysis surfaced

Found by asking what a specific number meant, not by any aggregate going red.
All three fixed in `GRANT_FORMS`. Measured over the committed archives alone —
OT2017–OT2024, 40,506 petitions — the diff is exactly four: `granted` 532 → 534,
`gvr` 565 → 566, `pending` 732 → 729.

### 1. The split IFP grant written with "and" matched nothing

`GRANT_FORMS` carried two alternatives for the split-motion grant — one
requiring `and the petition`, one requiring a bare `petition`. The Court's
fourth combination, `GRANTED, and petition for a writ of certiorari GRANTED
limited to …`, matched neither. That is how **18-6943, Banister v. Davis** is
written, so a case the Court granted, heard and decided sat at `pending`,
invisible to the funnel and dropped from the model's training corpus. One of the
five pro se plenary grants in the whole record was the case that exposed it.

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
  petition docketed in the *previous* Term, so Oct–Dec 2017 undercounts. The
  right edge no longer has the same problem: `snapshot_refresh` re-fetched every
  petition the archives still had at `pending`, so an OT2024 petition disposed of
  during OT2025 is captured.
- **OT2026 is a denominator, not a Term.** 910 dockets were open when this ran
  and the Term had not begun. Its petitions face the September long conference;
  none has been granted anything. Every fraction above excludes it, and the
  per-Term table marks it "open".
- **OT2025 is treated as complete.** Formally it runs to 4 October 2026, but the
  Court's last order list of the Term was in June and nothing further issues
  under that Term's number. `PRO_SE_LAST_COMPLETE` is where that judgment lives.
- **The pro se flag is a floor.** It reads a current snapshot of the party
  table. Where a granted petitioner's own name was dropped rather than demoted,
  the case reads as counselled and is not here.
- **Self-filed is not unsupported.** Grand paid the fee and drew eight amicus
  briefs before his grant. "Pro se" describes who signed the petition, not who
  was behind it, and the two OT2025 grants are a reminder that the coefficient
  and the label can drift apart.
- **GVR ≠ merits win.** A summary disposition is the Court sending the case back
  in light of an intervening decision, not agreeing to hear it. The two
  definitions answer different questions and the honest report gives both.
- **The snapshots are a point in time.** Fetched 2026-08-26, cleanly: 5,602 +
  910 + 729 requests, zero unresolved. Re-running the fetch will move OT2026 and
  nothing else.
