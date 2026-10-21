# Spec: measuring the Solicitor General's recommendation after a CVSG

Status: **shipped** (2026-10-20). Labels built and checked, the predictive
comparison done, and layer (b) is served. See **Results so far** and
**Serving, as built**.

## Why

When the Court calls for the views of the Solicitor General (CVSG), the model's
only input is the flag `cvsg` (+0.71 log-odds in the at-risk model). It never
learns what the SG says. So once the SG's brief is filed, every such petition
gets the same blended number, and that number is the average of two very
different populations. 26-49 (Bird v. Iowa Migrant Movement for Justice) reads
28.4% whatever the SG recommends. In the corpus, 36 of 137 resolved CVSG'd
petitions were granted (26%). If the Court follows the SG at anything like the
rate commonly reported, the true figures are far apart, perhaps high on a
grant recommendation and single digits on a denial.

## Why the earlier "buys nothing" verdict does not settle it

`docs/cert_model_review_2026-07.md` lists "SG's post-CVSG position as a
feature" as rejected: ΔAUC +0.0000, ΔAP −0.0019. Two problems with treating
that as the answer:

1. **It measured whether the SG had answered, not what the SG recommended.**
   The note reads "0 grants among CVSG-outstanding, 36 among answered". That
   distinction is about timing (the Court never grants before the SG speaks),
   and the conference hazard already captures most of it.
2. **The yardstick could not see the effect.** CVSG'd petitions are about 1%
   of the corpus. A feature that moves only those rows cannot shift a global
   AUC or AP, however right it is on them. The question has to be scored on
   the rows it touches.

## Data on hand (measured 2026-10-20)

Paid petitions, archives OT2017–OT2024 plus `snapshot_25.rds`:

| | count |
|---|---|
| Petitions with a CVSG (`Solicitor General is invited`) | 167 |
| …with the SG's brief on the docket | 143 |
| …with a PDF link on that entry | 142 |

Per Term: 29 / 21 / 13 / 20 / 25 / 21 / 11 / 13 / 14 (OT2017 → OT2025).

The 24 without a brief are pending, stipulated dismissals (Rule 46), or regex
misses. Seen so far:

- **Entry wording.** Almost always `Brief amicus curiae of United States filed.`,
  with a trailing ` VIDED.` 31 times. Also `of United States of America`, a
  lowercase `United states`, and one typo, `United Stares` (17-1530).
- **Link order varies.** The certificate of service sometimes comes first. The
  main document is identified by its `Document_n` label (`Main Document`), not
  by position.
- **One brief can cover several dockets.** Cox v. Sony (24-171 / 24-181) is
  one PDF filed in both dockets. The recommendation can differ by docket
  (grant one, deny or hold the other), so it has to be labelled per docket.
- **Stale resolution.** `ot_2024.rds` froze on 2025-06-18. CVSG'd petitions
  decided after that date read as pending there. Resolve them from a fresh
  fetch, as `cases/pending.json` does, before measuring.

## What to extract

One record per docket, in a committed file `data-raw/sg_recommendations.json`
(keyed by docket, the same shape as `petition_signals.json`):

| field | meaning |
|---|---|
| `filed` | date of the SG brief's docket entry |
| `url` | the main document's PDF |
| `rec` | `grant`, `grant_limited`, `deny`, `gvr`, `hold`, `dismiss`, `other` |
| `quote` | the sentence the label rests on, short, from the Conclusion |
| `source` | `regex` or `hand` |
| `sg_v` | extractor version, as `sig_v` for petition signals |

**Where the recommendation lives.** The Conclusion section, in a nearly fixed
form: "The petition for a writ of certiorari should be denied." Variants to
handle:

- "granted, limited to Question 1" becomes `grant_limited`.
- "the Court should grant the petition, vacate the judgment, and remand"
  becomes `gvr`.
- "should be held pending … and then disposed of as appropriate" becomes `hold`.
- Multi-docket briefs name the docket in the sentence ("The petition in No.
  24-181 should be granted; the petition in No. 24-171 should be denied").

**Method.** Download with the existing `R/petition_signals.R` path (`pdftools`,
the same pacing), cut the text from the last `CONCLUSION` heading to the
signature block, and let a regex propose the label. Then **hand-check all
~143**. At this size, reviewing every label is cheaper than auditing a
classifier, and the label is the whole experiment. A `source = "hand"`
override always wins over a re-run of the extractor.

## Leakage rules

- **Strictly before `as_of`.** The brief counts only for conferences after its
  docket entry, the same convention as `process_features()`.
- **At-risk panel.** Conference rows before the brief carry `rec = "outstanding"`;
  rows after carry the label. Petitions with no CVSG carry `rec = "none"`.
- **Never read the SG's merits-stage brief.** A grant can be followed by a
  second United States brief at the merits stage. Only the first US brief
  after the CVSG entry counts, and only if it is filed before the
  disposition. (This is the same tripwire as amicus side; see the review doc.)

## Measurement

Run on the CVSG-answered subset (≈130 resolved rows, ≈36 grants), with
**out-of-time** evaluation: rolling origin by Term, as in `docs/cert_model.md`.
Bootstrap intervals are clustered by brief, so a multi-docket brief is not
counted twice.

1. **Descriptive.** Grant rate by `rec`, with Wilson intervals, and the
   Court's agreement rate with the SG. Report by Term and by administration:
   the recommendation may track the administration, and the Court's deference
   may too.
2. **Predictive, three contenders on the subset:**
   - (a) today's `p_grant_ever`;
   - (b) the grant rate by `rec` alone;
   - (c) a two-parameter recalibration,
     `logit p = α + β·logit p_model + γ_rec`.

   Score each by log loss, Brier and AP on the subset. (c) is the candidate to
   ship: it keeps everything the model already knows (reply, amici, split,
   counsel) and adds only what the SG said.
3. **Global non-regression.** Confirm the full-panel metrics are unchanged to
   the fourth decimal. They should be, because only CVSG rows move.

**Decision rule.** Ship (c) if its out-of-time log loss on the subset beats
(a) with a paired, brief-clustered bootstrap interval that excludes zero.
Otherwise publish the descriptive table only, on the case page and in the
CVSG tracker, and keep the model as is.

Why a recalibration layer rather than a new factor in the at-risk model:
with ~36 positive rows, a five-level factor inside a 16k-row fit is estimable
under Firth, but `fit_cert_model()`'s aliasing guard and the LOTO machinery
are tuned for the whole panel. A layer is easier to validate, easier to
remove, and easier to explain on the page. Revisit if the data grow.

The conference hazard (when the Court acts) is a separate question. A brief
recommending denial is likely denied at its first conference, while a grant
recommendation may be relisted first. Measure it only after the
`p_grant_ever` result is in, because with 4 outcomes the subset is thinner
still.

## Serving, if it ships

- **Detection.** Add `sg_brief` to `FUNNEL_PATTERNS` in `R/cert_funnel.R`,
  case-insensitive and tolerant of `of America`. This also gives the CVSG
  tracker (feature idea #2) its "response arrived" signal.
- **Live extraction.** When a CVSG'd docket gains the entry, the daily run and
  the conferences run download the PDF and extract the label into an on-site
  cache (`conferences/sg_views_cache.json`), merged over the committed file
  the same way `petition_signals_cache.json` is.
- **Unreadable brief.** If extraction fails, `rec = "unread"` and the page
  shows today's blended number. It never guesses.
- **Case page.** One line under the forecast: "The Solicitor General
  recommends denial (brief filed May 12, 2027)", linking the PDF. Bump
  `PAGE_TEMPLATE_VERSION`.
- **Docs.** Update `docs/cert_model.md`, and the review-doc row this
  supersedes.

## Results so far (2026-10-20)

Steps 1–2 done: `data-raw/sg_recommendations.json` holds 144 labelled briefs
(`R/sg_views.R`, `.github/scripts/build_sg_recommendations.R`). Every quote was read against
its label. Seven are `source = "hand"`, each with a `note`:
- 17-1175 conditional GVR;
- 18-1447 conditional hold;
- 19-520, the conditional cross-petition the brief denies;
- 19-648 hold-then-grant;
- 21-260 deny, with GVR in the alternative;
- 23-217 grant and summarily reverse;
- 23-969, an appeal (`other`).

Outcomes are from a fresh fetch of the six that the frozen archives still
showed as pending. Stipulated dismissals are excluded, since the Court did
not act on them.

| SG recommended | granted |
|---|---|
| grant (incl. limited) | 32/40 = **80%** [64–90] |
| deny | 6/90 = **7%** [3–14] |
| hold | 2/7 |
| GVR | 0/3 (2 GVR'd) |

**The Court followed a grant-or-deny recommendation in 116 of 130 cases
(89%, [82–94]).** Against the blended 26% that the `cvsg` flag alone implies,
the recommendation splits the population roughly 11 to 1.

By administration, the rate on a grant recommendation is 16/18 under Trump I,
12/17 under Biden and 4/5 under Trump II. The rate on a deny recommendation
is 2/34, 4/52 and 0/4. The cells are too thin to call a difference.

The disagreements are the known ones:
- **Granted over a deny recommendation:** Google v. Oracle, SFFA v. Harvard,
  Torres, Amgen, Cantero, Atlantic Richfield.
- **Denied over a grant recommendation:** Interactive Wearables / Tropp,
  Teva v. GSK, Mutond, Patterson, Port of Tacoma, and two limited grants.

### Predictive comparison (2026-10-20)

**Setup.**
- **Rows:** each SG-answered petition in the at-risk panel, at its first
  conference after the brief. That gives 133 petitions and 36 grants;
  OT2024's post-brief conferences are missing from the frozen archive.
- **Out of time:** for each test Term 2019–2023, the at-risk model and its
  Platt map are refitted on earlier Terms only. That leaves 85 petitions,
  22 grants and 63 distinct briefs.
- **Layers:** (b), (c) and (c1) are fitted on the earlier Terms of the
  subset.
- **Intervals:** paired bootstrap, 4,000 draws, clustered by brief.
- **Script:** `scratchpad/sgeval.R` (not committed).

| contender | log loss | Brier | AUC | AP | mean p |
|---|---|---|---|---|---|
| (a) at-risk model alone | 0.760 | 0.260 | **0.41** | 0.29 | 0.38 |
| (b) grant rate by recommendation | **0.369** | **0.109** | 0.83 | 0.70 | 0.30 |
| (c) recalibration: a + b·logit p + g_rec | 0.370 | 0.109 | 0.85 | 0.72 | 0.28 |
| (c1) offset, slope pinned to 1 | 0.456 | 0.132 | 0.85 | 0.69 | 0.34 |

Observed rate on these rows: 0.259.

Change in log loss against (a), with 95% intervals:
- (b): −0.389 [−0.578, −0.188]
- (c): −0.388 [−0.592, −0.169]
- (c1): −0.304 [−0.508, −0.091]
- (c1) is worse than (b): +0.085 [+0.004, +0.175]

**Finding: once the SG has spoken, the model's score carries no information.**
- **Ranking:** on these petitions the model ranks below chance (AUC 0.41).
  Within each recommendation it is a coin flip: AUC 0.55 among deny
  recommendations, 0.45 among grant recommendations.
- **The fitted slope:** fitted on all Terms, the slope on the model's logit is
  **−0.16 ± 0.24**.
- **What the model does see:** its own mean is *higher* on deny
  recommendations (0.33) than on grant recommendations (0.29). What it
  responds to here, mainly the reply brief (r = 0.39 with its score), does not
  separate the outcomes after the SG has weighed in.
- **Why (c1) loses:** it forces a slope of 1 onto a score that has none.

**Decision.** The rule is met by a wide margin. Because (c) is (b) with an
extra parameter estimated at zero, ship **(b)**: for an SG-answered petition,
P(grant ever) is the smoothed historical grant rate for its recommendation.
That is 78% for grant, 7% for deny and 18% for anything else, re-estimated at
each retrain. The model's number is still shown before the SG files.

Caveats:
- 85 out-of-time rows.
- The 2019 fold of (c) was trained on LOTO scores for 2017–18. This does not
  affect (a) or (b).
- The per-conference hazard (when the Court acts after the brief) was not
  measured at first. It is now; see "The per-conference split" below.

## Serving, as built

This differs from the proposal above in two ways: the layer is (b), not (c),
and the case page takes no template bump.

- **Artifact.** `data/cert_model_sg.rds` (`fit_sg_layer()`, class `sg_layer`)
  holds the smoothed rate per group (grant, deny, other), refitted by
  `train_cert_model.R` from the committed labels. `load_cert_models()` loads it
  as `models$sg`, with the labels as `models$sg_views`. When `site_dir` is
  given, the labels also include `conferences/sg_views_cache.json`, where the
  committed file wins.
- **Scoring.** `score_conference()` finds the first US brief after the CVSG
  (`find_sg_brief()`) and looks its label up by URL (`sg_view_for()`); the
  scheme is ignored, since archives hold http and fresh fetches https. If the
  brief predates `as_of` and is labelled, `p_grant_ever` becomes the group rate
  and `sg_layer` is TRUE. The view is returned as `s[["sg_view"]]` — read it
  with `[[`, since `s$sg` partial-matches `sg_layer`. The per-conference
  hazards are replaced too (below). Every consumer of `score_conference()` follows: the conference
  reports' Ever column, the relist tracker, the forecast ledger and the case
  pages.
- **Live labels.** `render_conferences.R` calls `resolve_sg_views()` (at most
  25 new briefs per run) before rendering. A failed download is retried next
  run. An unparseable Conclusion is cached with `rec = null` and keeps the
  model's number until a hand label is committed.
- **Case page.**
  - Pending: "The Solicitor General recommends denial (brief filed …)", and
    "Granted ever, given the SG's recommendation: 8%".
  - Decided: "The Solicitor General had recommended …".
  - The manifest key: the layer and labels are excluded from `model_id`, and
    the docket's SG view plus the rates are appended to that page's key only
    when it has one. So only the ~145 SG pages re-render, with no
    `PAGE_TEMPLATE_VERSION` bump. Closed Terms pick the line up on the next
    `rerender-dockets.yml` dispatch, which is incremental.
- **Publish.** `conferences/sg_views_cache.json` is in `publish_site.sh`'s
  DERIVED union list.

## The per-conference split (2026-10-21)

The layer first replaced only `p_grant_ever` and left the conference model's
hazards in place. On SG-answered petitions those hazards are no better than the
at-risk score, and the two lines could contradict each other outright. 25-828
(the SG recommends a grant) read "78% estimated chance of a grant" above "At
the Nov 6 conference: granted 10% · GVR 1.3% · relisted 10% · denied 78%". A
denial is final, so 78% denied on Friday caps the chance of ever being granted
at 22%.

What the Court has done at the conferences after an SG brief, from the labelled
briefs and the at-risk panel (`conf_outcome`; Rule 46 dismissals left out):

| SG recommends | conference after the brief | n | granted | GVR | denied | relisted |
|---|---|---|---|---|---|---|
| grant | first | 35 | 23% | 0% | 9% | 69% |
| grant | later | 33 | 61% | 0% | 12% | 27% |
| deny | first | 86 | 1% | 3% | 78% | 17% |
| deny | later | 22 | 23% | 5% | 41% | 32% |
| other | first | 10 | 0% | 10% | 10% | 80% |
| other | later | 14 | 14% | 21% | 21% | 43% |

(Raw shares; "later" pools the second conference onward.) The Court relists an
SG-recommended grant once before granting it: at the first conference two
in three are relisted, and the grant follows at the next.

**Serving.** `fit_sg_layer(views, corpus, panel)` also stores `hazard`, keyed
`<group>.<first|later>`, with `hazard_n`. Each cell is shrunk toward its
group's split over both phases, and each group toward all SG rows, by
`SG_HAZARD_PRIOR` = 4 pseudo-counts. `score_conference()` takes the split from
`sg_hazard()` whenever `sg_layer` is TRUE. A conference is "first" when no
conference date on the docket falls between the brief and it. The rates
themselves are unchanged. The page's manifest key appends `hazard` beside
`rates`, so only the SG pages re-render.

**A known incoherence, left to the clamp.** When the SG recommended denial and
the Court relisted the petition anyway, the later conference granted 20%
(smoothed). That is more than the 8% rate, which is measured from the brief
and not conditioned on surviving a conference. `score_conference()` already
clamps `p_grant_now` to `p_grant_ever`, so the page shows 8% there. The honest
fix is a rate conditioned on the phase, a change to the validated headline
that needs its own out-of-time check: 15 petitions so far. On 2026-10-21 no
pending petition was in that cell.

**Ledger.** A first forecast already logged is never rewritten, so 25-828's
first entry for 6 November keeps the conference model's 10% (scored 20
October). Its `final` block, rewritten until conference morning, carries the
split.

## Plan

| step | output |
|---|---|
| 1. Detect briefs; download and extract; propose labels | `data-raw/sg_recommendations.json` (regex labels) |
| 2. Hand-check every label | same file, `source` filled in |
| 3. Analysis script: descriptive table plus (a)/(b)/(c) | results written into this doc |
| 4. Decide per the rule above | ship (c), or descriptive-only |
| 5. If shipping: live extraction, page line, docs | PR |

Steps 1–3 touch no published page and no model artifact.
