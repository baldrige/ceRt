# The Counsel Table (`/counsel/`)

Seven leaderboards, from two different records: four over **petitioner's counsel
of record**, three over **who argued**. **No fetch, ever** — the inputs are
`counsel_archive_paths()` and `data-raw/arg_refresh.rds`, all committed, so the page
changes when the archives or the classifier change and at no other time.

## The inputs: OT2017–2025 (since 2026-10-21)

`counsel_archive_paths()` is the one list, read by `make_counsel_stats.R`,
`render_counsel.R` and the audit's freshness check, **most current first**,
because `counsel_petitions()` and `counsel_arguments()` both keep the first copy
of a docket:

1. `data-raw/snapshot_refresh.rds` — every petition the archives froze at
   `pending`, re-fetched in August 2026 (692 of its 729 are OT2024). Without it
   an OT2024 petition granted after `ot_2024.rds` was taken stays pending here,
   and 26 of the 64 dockets argued in OT2025 have no argument entry at all.
2. `data-raw/ot_2017.rds` … `ot_2024.rds` — the Term archives.
3. `data-raw/snapshot_25.rds` — OT2025, a full-Term fetch taken after it closed.

OT2025 is a `snapshot_*`, not an `ot_2025.rds`, because the Cert Funnel and the
cert model also glob `ot_*.rds` (see docs/pro-se-grants.md); naming it in
`COUNSEL_SNAPSHOTS` adds it to this page alone. `snapshot_26` is the Term under
way and stays out. The page's span ("OT2017–2025") is `counsel_terms()` of the
same list. The measurements below that name OT2017–2024 predate this change.

| piece | where |
| --- | --- |
| Aggregation + renderer | `R/counsel_table.R` |
| Regenerate the committed stats | `.github/scripts/make_counsel_stats.R` |
| CI render | `.github/scripts/render_counsel.R` |
| Committed summary | `data/counsel_stats.json` |
| Argument input | `data-raw/arg_refresh.rds`, refreshed by `refetch-argued.yml` |
| Workflows | `conferences.yml` (weekly, self-healing) · `render-counsel.yml` (dispatch) |

## The unit is a case, not a petition

One dispute is often docketed as several petitions — one per patent, per
consolidated appeal, per petitioner — so `counsel_cases()` collapses
`(advocate, caption)` to a single row keeping the strongest outcome and the
highest relist count.

This is 1.3% of petitions and it decides the top of a rate board. Counted as
petitions, the relist leaderboard opened with **10 filed, 10 relisted, 100%** —
eight identical *Paice LLC v. Ford* petitions docketed across three days plus two
identical *KIP CR* ones. Two disputes, each redistributed once, published as an
advocate whose every filing drew a second look. Collapsed, that advocate has
three cases and does not qualify.

Companion petitions do **not** share a docketing date, so the caption alone is the
key.

## Two floors, both measured

| floor | applies to | value |
| --- | --- | --- |
| `COUNSEL_MIN_CASES` | the volume board, and the "qualifying" count | 5 |
| `COUNSEL_MIN_RATE_CASES` | the relist board and both grant boards | 8 |

A rate needs more evidence than a count. At a floor of five, **eleven of the
twenty-five relist rows were the same shape** — 3 relisted of 5 filed, a 60% share
and a 23.1% lower bound, eleven times. A real ranking that told the reader
nothing. At eight the largest tie in the top twenty-five is two rows; ten and
twelve buy nothing further.

The grant boards take the same floor one step removed: a Wilson bound *shrinks* a
small sample, it does not rescue one. At five, 3 grants in 5 outranked 8 in 23.
It costs the government board four rows, including a 6-of-7 that would have led
it.

Rate boards rank on the **Wilson 95% lower bound** and print the raw rate in bold
beside it — the bound is the order, the rate is the claim. Ranked on the raw rate
instead, 3 grants in 10 sits above 22 in 94.

## The government split, and why it is where it is

Grant rates by petitioner, OT2017–2024, case-level:

| petitioner | cases | granted | rate |
| --- | --- | --- | --- |
| The United States | 162 | 83 | **51.2%** |
| A State | 208 | 31 | **14.9%** |
| A private party | 8,232 | 379 | **4.6%** |

**A State officer is a State, whichever side of the title the State is on.**
Until 2026-10-20 six State petitioners read as the United States: "Dave Yost,
*Ohio* Attorney General" and "Wes Allen, *Alabama* Secretary of State" carry a
title that is also federal, and `FED_PARTY_RX` guarded only against a State named
*after* it ("Attorney General of Mississippi"), with no guard on "Secretary of
State" at all. One of the six was *Merrill v. Milligan* (21-1086/1087), a grant
credited to the federal column. `STATE_OFFICER_FIRST_RX` now catches the State
named first; it matches exactly those six petitions and nothing else in
OT2017–2024. No board row moved, only the two rates above.

A pooled ranking is therefore the Solicitor General's office at the top, which
measures who the client is rather than how the petition was written.

**The split is made at the case, not the advocate.** An advocate-level cut needs a
threshold and there is none to pick: `gov_share` puts 245 of 286 qualifying
advocates at exactly zero and strings the rest out in a gradient thick with
*former* state solicitors general now in private practice. Splitting cases needs
no threshold, and an advocate who did both kinds of work appears on both boards —
which several did.

### `petitioner_gov_side()`: precise, and deliberately not recall-tuned

Measured against hand-checked ground truth: **~100% precision** (1 apparent false
positive in 547 private-bar petitions, and that one is a real government filing),
**~82% recall**. The misses are not exotic — the docket's party field carries bare
officer names (`Merrick B. Garland`), abbreviations (`FCC`, `Att'y Gen.`), source
typos (`Securites and Exchange Commission`, `Homeland Secuirty`) and offices with
no fixed written form (`Office of the United States Trustee`).

Recall is the dangerous direction: every miss moves a government case onto the
**private** board. So `COUNSEL_GOV_MAX_PRIVATE` (0.25) keeps an advocate whose
filings are a quarter or more government off the private board entirely. Split
purely by case, that board opened with Elizabeth Prelogar at 6 grants in 7 — all
seven government filings the grammar missed. Across the top of the board the
government share runs 92%, 44%, then 3%, 2%, 0%: anything from 5% to 40% draws the
same line, and a quarter is the round number inside the gap.

It costs a few real private-practice rows. That is the right direction to err — an
advocate wrongly left off a board is invisible; an advocate wrongly on top of one
is a published claim that is false.

### Adding the caption is a trap, and it is measured

Recall rises 82% → 90% and precision collapses 99.8% → **81%**, because a private
petitioner *suing* the government has the government in their caption: 50 of Raed
Gonzalez's 51 immigration petitions read as federal filings. **The party field
names the petitioner; the caption names both sides.**

## Against expectation (since 2026-10-22)

The private grant board ranks a rate, and a rate cannot say whether an advocate
wins because of what they do or because of which cases they are handed. The
"Against expectation" board holds each private-bar advocate's grants against what
the cert model expects of the same petitions **once it is told nothing about
counsel**.

**The model.** `COUNSEL_BLIND_FEATURES` = `BASELINE_FEATURES` minus
`COUNSEL_FEATURES` (the `counsel_tier` track record), nothing else changed: party
types, court below, filing speed, the dissent/split cues read from the petition,
and its length. `.github/scripts/make_counsel_expected.R` fits it on the training
corpus (`data-raw/cert_corpus.rds`, OT2017–2024) and scores every resolved paid
petition **out of term** — `loto_predict()` then `loto_platt()` — so an advocate's
own grants never set the expectation for their own cases. The petitions the corpus
cannot label — OT2025 (`snapshot_25.rds`) and the OT2024 petitions decided after
`ot_2024.rds` was taken (`snapshot_refresh.rds`) — are scored by the model fitted
on all eight Terms, calibrated on all the out-of-term predictions; they are
outside the fit, so they are out of sample too. Their petition-text cues come from
the same enrichment layer, extended to those archives (`enrich-petitions.yml`
terms `2025` and `refresh`; `enrich-word-counts.yml` with `terms_glob`
`data-raw/snapshot_25.rds,data-raw/snapshot_refresh.rds`). The script refuses to
run if their signal coverage falls more than two points below the corpus's: a
petition the enricher missed reads as "no dissent, no split" and would be
expected to lose.

**Each Term is recentred on its own grant count.** After scoring, every
petition's log-odds in a Term is shifted by one constant so the Term's expected
total equals its actual grants. Without it the model expected 63.5 OT2025 grants
against 37: the Court granted 3.1% of OT2025's resolved paid petitions (3.8–5.0%
in every earlier Term) while those petitions reported dissents, splits and
dissent citations *more* often than any earlier Term's. That deficit would have
landed on whoever filed most that Term. The shifts, in log-odds:

| OT2017 | OT2018 | OT2019 | OT2020 | OT2021 | OT2022 | OT2023 | OT2024 | OT2025 |
|---|---|---|---|---|---|---|---|---|
| +0.18 | +0.16 | −0.02 | −0.26 | −0.09 | +0.17 | −0.06 | −0.02 | −0.66 |

The question the board answers is therefore "better than other petitions *that
Term*." It reads outcomes only as a Term total, and an advocate's own grants are
a handful of a Term's ~60, so the shift they induce on their own expectation is
small and runs against them. The probabilities are
committed in `data/counsel_expected.json` (~260 KB, dkt → p) because producing them
needs the untracked corpus and eight fits; the render only reads them.

Measured 2026-10-22 on the 11,814 corpus petitions, out of term and before the
per-Term recentring below:

| | AUC | Granted | Expected |
|---|---|---|---|
| with counsel (the deployed baseline) | 0.876 | | |
| counsel-blind | 0.867 | 498 | 498.1 |

Plus 1,520 snapshot petitions (1,195 OT2025, 303 late OT2024, 22 late OT2023),
98.2% of them carrying petition signals at the current version against 90.3% of
the corpus.

**The board.** Private-side cases only, companions collapsed (granted if any was;
expectation = their mean), the `COUNSEL_GOV_MAX_PRIVATE` guard applied, at least
`COUNSEL_MIN_RATE_CASES` (8) scored cases and `COUNSEL_EXPECTED_MIN` (1) expected
grant: 49 advocates. The ratio shown is granted ÷ expected; the board is ordered by
`(granted + 2) / (expected + 2)` (`COUNSEL_EXPECTED_PRIOR`), the muted figure, so 2
grants against 0.2 cannot outrank 13 against 3.3. At k = 2 the top seven are
exactly the seven whose excess is ≥ 3.3 standard errors (Blatt, Shanmugam,
Dvoretzky, Pincus, Fisher, Geyser, Clement). The z-scores of the 49 have a
standard deviation of 1.80;
pure chance would give 1.0, so the differences are real — what they are is the
next point.

**What it cannot claim.**

- **Advocacy and case selection are confounded.** Leading advocates are often
  retained once a case already looks cert-worthy for reasons the docket does not
  record. "Above expectation" means the cases did better than the visible facts
  predicted.
- **The model reads the petition.** Whatever counsel put into its length or its
  account of a split is credited to the case, so the excess is if anything
  understated.
- **Private clients only.** The model's `pet_type` reads a warden or a named
  official as an individual — 141 of the 208 State-filed scored cases — and a
  third of the `us_fed` petitions were filed by private counsel. Against that
  baseline the SG's office (83 vs 59.5) and the States (31 vs 18.5) look like they
  outperform when the model has mistyped the client.
- **Scored petitions only, on both sides.** A petition the enricher never
  reached, or whose lower court the corpus never saw, contributes neither a grant
  nor an expectation. Until 2026-10-22 that was all of OT2025 and the late OT2024
  petitions, and the board stopped at OT2024.

**Refreshing.** After a retrain-relevant change (features, the corpus, the
signals file):

```
Rscript .github/scripts/make_counsel_expected.R   # needs data-raw/cert_corpus.rds (built if absent)
Rscript .github/scripts/make_counsel_stats.R      # the fingerprint digests the expectation file
```

## Staleness

`data/counsel_stats.json` is a committed summary of a classifier's output — the
same shape of file as `data/funnel_baselines.json`, which went stale for two weeks
and published a relist count 2.5× too high. The fix that broke it lived inside a
*function body*, so `counsel_stats_fingerprint()` deparses the eleven functions
the file is a product of, not just the tunables, plus the archive md5sums.

Three things act on it, in order of how loudly:

- **`render_counsel.R` recomputes in-process on a mismatch** (~4 min) rather than
  failing. It is `continue-on-error` inside `conferences.yml`, so a hard stop
  would be silent in exactly the case that matters *and* would leave the previous,
  wrong page live.
- **`audit_site.R` FAILs** on a stale committed copy — the page is right either
  way, but a silent 4-minute recompute on every weekly run is a cost nobody sees.
- **`CHECK_ONLY=1 make_counsel_stats.R`** exits 1, for use anywhere a gate is
  wanted.

After any change to the classifier, `counsel_key()`, the party grammar or the
constants:

```
Rscript .github/scripts/make_counsel_stats.R     # ~4 min, prints what moved
```

and commit the result.

## What the page refuses to claim

- An advocate is a **name, not a verified identity**. Rows key on first + last
  token, so `Neal K. Katyal` and `Neal Kumar Katyal` merge — and so would two
  different lawyers who share both names. Every row that merged more than one
  spelling prints them beneath the name; that visible merge is the whole
  mitigation, because **0 of 8,989** archive petitions carry a firm to
  disambiguate on.
- **Petitioner's counsel of record only.** An advocate who argued a case they did
  not petition in does not appear, and respondents' counsel — most of what the
  Supreme Court bar actually does — is invisible here.
- Paid petitions only; self-represented petitioners excluded; pending petitions
  count toward cases filed and relists but never toward a grant rate.


## The argument boards

A different population, a different source, and a different span.

The petition boards read `counsel_archive_paths()`. That is the wrong file for
arguments: **a Term's snapshot is taken before its own granted cases are argued
and decided**, so the argument and judgment entries are simply not in it.
`data-raw/arg_refresh.rds` is a re-fetch of the argued grants and is the only
current record of how OT17–23 came out. Precedence is the same as
`render_arguments.R`: refresh first, archives second, dedupe by docket.

### What the docket gives that a petition cannot

One entry carries all of it:

```
Argued. For petitioner: Jeffrey L. Fisher, Stanford, Cal.  For respondent:
Vivek Suri, Assistant to the Solicitor General, Department of Justice,
Washington, D. C.
```

That names the advocate who actually **stood up** (not merely who signed the
petition), the **side** they stood on, and — from the title they are announced
under — the **office**. The title is a far cleaner government signal than the
party-name grammar the cert boards need: `Assistant to the Solicitor General,
Department of Justice` is unambiguous, and `Solicitor General, Baton Rouge, La.`
is unambiguously a State's. Side resolution reaches all but 37 of 1,206
appearances.

### The side label names a client; the win board needs a position

These are not the same thing, and three constructions separate them:

| label | client | asks for |
| --- | --- | --- |
| `respondent in support of petitioner` | respondent | reversal |
| `respondent in support of vacatur` | respondent | vacatur |
| `petitioner in 17-1618 and respondents in 17-1623` | both | both |

A rule that only tests "petitioner" against "respondent" scores the first
correctly *by accident* and the second exactly **backwards**: measured, eight
advocates who asked the Court to vacate or reverse, and got it, were recorded as
having lost. So `.argued_side()` reads the position clause first. A label naming
both sides with no position stated is **`split`** — counted as an argument,
scored for neither, because the advocate really was on both sides of a
consolidated pair.

Amicus is tested before all of it and stays out of the win boards: no judgment
runs for or against an amicus, and a *court-appointed* one is appointed precisely
because no party will defend that position, so scoring the loss against them
would be perverse.

### Collapsing companions, and the three ways it goes wrong

VIDED companions repeat the argument entry, so the entry itself is the collapse
key — exact, not a heuristic. 522 dockets are **458 sittings** of **456 cases**.
Each refinement below was found by trying to break the previous one.

- **Squish the text.** 21A240 and 21A241 carry the same entry differing by one
  double space after `D. C.`; exact matching made them two arguments and
  double-counted all three advocates.
- **Key on the text *and the date*.** Text alone is not unique. *Biden v. Texas*
  (21-954, argued 26 Apr 2022) and *United States v. Texas* (22-58, argued 29 Nov
  2022) are different cases seven months apart whose entries are **byte-identical**
  — Prelogar for petitioners, the Texas Solicitor General for respondents — and
  collapsing on text alone merged them, losing a whole argument and its outcome.
- **Keep every argued entry, not the last.** A case can be reargued with
  different counsel. *Louisiana v. Callais* was argued in March 2025 and again in
  October; taking only the later entry erased Stuart Naifeh from the record.

Hence two identifiers. `case_id` is the dispute; `argument_id` is one sitting of
it. They differ only for a reargued case, and that difference is exactly what
lets the volume board count two appearances while the win boards count one
outcome — *Knick v. Township of Scott* was argued twice and decided once.

### Why the win boards are split by side

| arguing for | arguments | won | rate |
| --- | --- | --- | --- |
| the petitioner | 558 | 410 | **73.5%** |
| the respondent | 490 | 161 | **32.9%** |

### Those two rates do not sum to 100%, and that is not an error

They are the right comparators for the boards — an appearance-weighted board
needs an appearance-weighted baseline — but they are rates over two different
populations, not two halves of one case. The 106.3% decomposes exactly:

| | petitioner | respondent | sum |
| --- | ---: | ---: | ---: |
| one case, one outcome (426 cases) | 70.4% | 29.6% | **100.0%** |
| + weighted by advocates per side | 72.3% | 32.9% | 105.2% |
| + cases with no scored opponent | **73.5%** | **32.9%** | **106.3%** |

The weighting term is not noise. **The side that wins brings more advocates to
the lectern** — 1.34 against 1.15 when the petitioner prevails, 1.18 against 1.29
when it does not — so both rates are pulled up together.

The last term is the **14 cases whose judgment below was defended by a
court-appointed amicus**, who is deliberately unscored (no client, and appointed
precisely because no party would take the position). The petitioner-side win in
those has no counterpart on the other board.

`case_rates` carries the clean partition so the page can state how often the
Court actually reverses — 70.4% — without implying the board baselines are
halves of it. The page says in terms that the two do not add to 100 and why: a
reader who finds that discrepancy unaided has been given a reason to distrust
every other number on the page.

**The Court takes cases in order to reverse them.** Which side of the "v." an
advocate stood on therefore matters more than anything they said, and a pooled
"success at argument" ranking would largely sort advocates by that — with the
Solicitor General's office at the top, since it chooses the cases in which the
United States petitions. Two boards against two published base rates say what one
board cannot.

### The judgment below either stood or it did not

That is the whole test, and getting the grammar for it wrong was one-sided.

| disposition | order below | scores for |
| --- | --- | --- |
| `Judgment REVERSED` / `VACATED` | fell | the petitioner |
| `Judgment (is) AFFIRMED` | stood | the respondent |
| `Appeal dismissed` (mandatory jurisdiction) | stood | the respondent |
| application for a stay **granted** | fell | the applicant |
| application for a stay **denied** | stood | the respondent |
| split (`AFFIRMED as to No. 22-23; REVERSED as to No. 22-331`) | both | neither |
| writ `DISMISSED as improvidently granted` | stood, undecided | neither |

Two corrections, both found by reading the cases the grammar dropped rather than
the grammar itself:

- **The connector varies.** The first version required the verb to sit
  immediately after "Judgment", but the Court frequently writes **"Judgment is
  AFFIRMED and case REMANDED"**. The twelve dockets that dropped were
  **affirmances, every one** — a perfectly one-sided miss that inflated the
  petitioner win rate and deflated the respondent's. Fixing it moved the two
  published base rates from 75.0%/31.0% to 73.4%/33.1%.
- **A direct appeal is not disposed of by writ.** Mandatory-jurisdiction appeals
  end `Appeal dismissed`; the judgment below then stands exactly as on an
  affirmance and the appellant has lost. 18-281 (*Virginia House of Delegates*)
  is that case — dismissed for want of standing, and a loss for the appellants
  it was previously not charged to.

### Argued emergency applications

Four arguments in the window were on the **emergency docket** (`NNA###`), which
never reaches a merits judgment — but an application is granted or denied, and
that is a win or a loss for whoever moved. `applicant` is the emergency docket's
word for the petitioner's role, and `APPLICATION_RX` reads the disposition.

Two traps, both live:

- **Anchor on "application(s) for … stay", never on "granted".** These dockets
  are thick with procedural grants — `Motion for divided argument filed by
  respondents GRANTED` appears on nearly every one — and a bare granted/denied
  test scores the argument-time allocation as the outcome.
- **Gate the branch on the docket being an application.** 17 argued *certiorari*
  cases carry an ancillary stay in their history (a stay of execution, a stay
  pending cert) and every one of them also has a real merits judgment. Ungated,
  the application branch fires first and replaces the outcome with the stay
  ruling: *Glossip* (22-7466) would have been scored on `Application (22A941)
  for stay of execution` rather than on `Judgment REVERSED`.

A writ DIG'd is deliberately excluded even though the judgment below stands
there too: the Court expressly declined to decide, and the convention in
published counts is to set those aside rather than charge them to an advocate.

Amicus arguments count toward volume and never toward a record: the judgment ran
for or against the parties.

### Floors, and why the side boards are shorter

`COUNSEL_ARG_MIN` is **5**, not the 8 the petition rate boards use: arguments are
two orders of magnitude rarer than petitions (461 against 8,875), and 8 would
leave seven advocates on the respondent side.

`COUNSEL_ARG_BOARD_N` caps the two side boards at **15** rows, against 25
elsewhere. Only 19 advocates clear the floor as respondent, so a 25-cap publishes
*all* of them — and the bottom of that list is a named person at 0 wins in 7
arguments. That is a complete ranking wearing a leaderboard's clothes, and it
makes a reputational claim the data cannot carry: seven respondent-side arguments
inside one eight-year window is not a career, and the advocates it would name that
way have argued well over a hundred cases between them. The qualifying counts are
published beside the boards so the truncation is visible rather than silent.

### One name per person, across both halves

The two sources spell people differently — the petition dockets carry
"Lisa Schiavo Blatt", the argument entries "Lisa S. Blatt". A name registry built
from the union of both keys the display name and the published variant list, so
one person is never printed under two names on one page.

An advocate can appear at the lectern without appearing above it: much of the
Solicitor General's office argues cases it did not petition in, and those rows
have no petition-side counterpart at all.


## What the adversarial pass tried and could not break

Every check below was written to falsify something. The ones that found bugs are
recorded above; these are the ones that held, and they are worth keeping because
a future change should have to pass them again.

| check | result |
| --- | --- |
| Arguments missed by the anchored `^Argued\.` regex | 0 |
| Extracted "advocates" that are not names (digits, titles, single tokens) | 0 |
| Single-token counsel keys | 0 |
| Same advocate counted twice in one sitting | 0 |
| Known SG-office advocates misclassified | 0 of 14 |
| Known private-bar advocates misclassified | 0 of 11 |
| Amicus / split / unresolved appearances ever scored | 0 |
| Scored rows disagreeing with the stated stood/fell rule | **0 of 1,053** |
| Mixed-disposition appearances scored | 0 of 35 |
| Every appearance has exactly one side | 1,196 of 1,196 |

**External sanity check.** Arguments per Term come out at 67, 59, 56, 61, 57, 60,
62 for OT2018–OT2024 — the Court hears roughly 60–70 a Term. OT2017 (17) and
OT2025 (19) are partial, at the two ends of the archive window.

### One improvement tested and rejected

The 30 unresolved side labels are party names used where a role belongs
(`Navajo Nation`, `Texas`, `UTIER`), and all 30 sit on cases with a scorable
judgment — so resolving them looked worth doing. The obvious method is to match
the label against the two halves of the caption.

Measured against the appearances where the label *already* gives a side, it
agrees only **88% of the time** (22 of 25), because a party name can appear on
both sides of a caption: `appellees Virginia State Board of Elections` matches
"Virginia" in *Virginia House of Delegates*, and `student respondents` matches
"Students" in *Students for Fair Admissions*. It also resolves only 16 of the 30.

A 12% error rate on the one thing these boards must not get wrong is a bad trade
for 16 appearances out of 1,196. Rejected; the 30 stay unresolved and disclosed.
