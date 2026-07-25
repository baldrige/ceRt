# Certiorari-grant prediction model

Calibrated estimates of what the Court will do with a **paid** petition. Three
models serve two surfaces:

| Model | Answers | Where it's shown | Artifact |
|---|---|---|---|
| **Baseline** | P(granted), from the petition alone | Daily dashboards, case pages | `cert_model_baseline.rds` |
| **Conference** | P(granted / GVR'd / denied / relisted **at this conference**) | Conference reports, "Granted here" | `cert_model_conference.rds` |
| **At-risk** | P(granted **ever**) | Conference reports, "Granted ever" | `cert_model_enhanced.rds` + `_gvr.rds` |

Code: `R/cert_model.R` · training driver `.github/scripts/train_cert_model.R` ·
labels from `classify_petitions()` in `R/cert_funnel.R`.

## Why this is a calibration problem, not a classification problem

Paid petitions are granted ~4% of the time; IFP petitions ~0.1%. A model that
always says "deny" is ~96% accurate and useless. So we segment to paid petitions,
report a **probability and a lift over the base rate** rather than a yes/no,
evaluate with AUC / average precision / Brier / calibration slope, and calibrate
the scores so the numbers mean what they say. IFP petitions are not scored.

## The two questions a conference report has to answer separately

This is the central design point. A petition sitting at a conference has two
different probabilities attached to it, and they are not close:

- **P(granted at this conference)** — will the Court act on Friday?
- **P(granted ever)** — will it be granted at this conference or a later one?

At a petition's *first* conference these differ by roughly 2×: about 3% of such
petitions are granted eventually, but only ~0.8% are granted at that first
conference — the overwhelmingly likely outcomes are denial or a relist. A single
published number cannot be both, and until 2026-07 the site published one number
that was neither: it read ~7.8% where 2.9% of petitions are granted at the
conference in front of them, and understated the eventual figure 2.8× for a
first-timer.

Each column now comes from the model that measurably wins that target, on a
like-for-like rolling-origin comparison over the same rows:

| Target | Competing risks | At-risk binary |
|---|---|---|
| granted at this conference | **AUC 0.920 · AP 0.259 · Brier 0.0248** | AUC 0.900 · AP 0.223 · Brier 0.0334 |
| granted eventually | AUC 0.865 · AP 0.363 · Brier 0.0609 | **AUC 0.870 · AP 0.397 · Brier 0.0580** |
| GVR at this conference | **AUC 0.889 · AP 0.163** | — |

Each is worse at the other's job, in opposite directions. Notably, deriving
P(granted ever) by rolling the hazards forward over future conferences is
**worse** than fitting that target directly (AP 0.363 vs 0.397), so the forward
recursion (`conference_cumulative()`) exists for analysis and is *not* used in
production.

The competing-risks model is a four-way multinomial over
`{granted, gvr, denied, relisted}`, so its probabilities sum to 1 by
construction. The previous pair of independently-fitted binaries did not: they
exceeded 1 on 140 of 16,333 panel rows (max 1.43), implying a negative P(denied).
Note the conditioning: `dismissed` is **not** one of the four risks (84 rows,
0.5%), so these are probabilities conditional on the petition not being dismissed
under Rule 46.

## The leakage rules

Three, and each was violated at some point:

1. **Process features are snapshotted strictly before the as-of date.** Merits
   amicus briefs are filed *after* a grant, so counting amicus over the whole
   docket would teach the model that grants cause amicus briefs.
2. **Training and serving must measure a feature at the same point in the case's
   life.** Relists were counted over the whole docket at training but strictly
   before the as-of date at serving. A petition denied at its first conference
   whose counsel then files for rehearing picks up a second `DISTRIBUTED` entry
   *after* the denial; 695 of 11,368 paid decided petitions (6.1%) carried one,
   685 of them denials. That pushed 671 zero-relist denials into the one-relist
   cell and halved its apparent grant rate.
3. **Counsel track record counts only what had already happened.** Prior
   petitions are counted by docketing date, prior *wins* by disposition date.
   Scoring against the full sample instead lifts forward AUC from 0.882 to 0.963
   — the signature of leakage, not skill.

## Features

**Structural** (both tiers): `pet_type`, `resp_type` — entity buckets from the
caption; `court_below` — 13 circuits, `STATE`, `CAAF`, `USDC`, `FED_SPEC`,
`OTHER`; `pro_se`; `gap_fast`/`gap_na` — a hinge on days from the judgment below
to docketing.

**Petition stage adds:** `counsel_tier` — an expanding-window record of the
petitioner's counsel of record (`new` / `some` / `vet` / `won`);
`dissent_below`, `split_argued` — Rule 10 cues parsed from the petition PDF.

**Conference stage adds:** `relist_bucket`, `amicus_bucket`, `cvsg`,
`response_requested`, `response_filed`, `resp_waiver`, `reply_filed`. The
competing-risks model further adds `conf_f` (conference index) and `phase`
(position in the Term — the September long conference and the late-June clean-up
conferences behave nothing like an ordinary sitting).

Notes on specification, all of them learned the hard way:

- **Counts are bucketed, not linear.** Relists and cert-stage amicus both enter as
  factors. A linear amicus term extrapolated a 25-brief petition to +5.13
  log-odds and saturated the forecast; bucketing beats it decisively (AIC 2335 vs
  2374).
- **A CVSG redistribution is not a relist.** The case returns to conference
  because the Solicitor General's brief arrived, exactly as with a called-for
  response. Counting it gave all 124 CVSG'd petitions ≥1 relist and forced the
  CVSG coefficient to −1.10, publishing the cue backwards; correcting the grammar
  flipped it to +0.42 with no interaction term needed.
- **Separation is handled by the estimator, not by hiding cells.** `fit_logit()`
  applies a Firth penalty. `court_below == "OTHER"` (0 grants in 312 — "In re"
  original writs) used to be folded into `STATE`, the *reference* level, so those
  petitions got a court contribution of exactly zero and published ~10% where
  ~3.5% is honest. `pro_se` (0 grants in 3,016) is likewise finite only under
  Firth.
- **`elite_counsel` was removed.** It matched a fixed list of ~13 advocates and
  was worth −0.0006 AUC once `counsel_tier` existed. It had also been silently
  dead: the extractor read the live parties schema, the archives use a different
  one, so it was constant `FALSE` across all 40,506 training rows and aliased to
  an `NA` coefficient. `fit_cert_model()` now refuses to fit a model with any
  aliased coefficient.

## Relist odds

Paid, resolved, OT2017–24. `train_cert_model.R` reprints this at every retrain —
trust that over any copy pasted elsewhere.

| relists | n | granted | rate |
|---|---|---|---|
| 0 | 10,796 | 137 | 1.3% |
| 1 | 616 | 260 | 42.2% |
| 2 | 169 | 52 | 30.8% |
| 3–4 | 121 | 32 | 26.4% |
| 5+ | 104 | 16 | 15.4% |

Three separate corrections had to land before these meant anything — the
disposition snapshot, the full resolved denominator, and the CVSG grammar. The
figures published before 2026-07 (1.3 / 20.0 / 43.8 / 36.2 / 18.6%) had none of
them. The public Cert Funnel table was wrong the same way: it reported 4,421
petitions relisted at least once where the true figure is 1,841.

## Performance

Leave-one-term-out, calibrated out-of-fold. Generated by the training driver.

| Model | Target | AUC | AP | Brier | Base rate |
|---|---|---|---|---|---|
| Baseline | P(grant) | 0.863 | 0.313 | 0.0339 | 4.13% |
| At-risk | P(grant ever) | 0.875 | 0.362 | 0.0593 | 7.81% |
| GVR | P(GVR ever) | 0.822 | 0.196 | 0.0459 | 5.44% |

These are **not** comparable to figures published before 2026-07 (baseline 0.720
/ 0.804, enhanced 0.930). Those were computed on an easier target — GVRs and
dismissals excluded from the denominator — and with the Platt map fitted and
scored on the same rows, which pins the calibration slope to 1.000 by
construction and cannot detect miscalibration even in principle. The conference
tier additionally changed target entirely, from P(grant | disposed now) to
P(grant ever).

The number that moved most is not in the table: the conference-stage calibration
slope went from **0.578 to ~0.99**.

Rolling-origin evaluation (train on terms < t) reproduces leave-one-term-out to
within 0.003 AUC, so the LOTO figures are not meaningfully optimistic — but LOTO
is out-of-*fold*, not out-of-time, and should not be described as the latter.

## Uncertainty

`score_features()` returns `ci_low`/`ci_high`, a Wald interval on the linear
predictor pushed through the link and the calibrator. Measured widths:

| forecast | 95% interval width |
|---|---|
| 0.2% | 0.5 pp |
| 3.5% | 3.5 pp |
| 16.3% | 14.1 pp |
| 38.7% | 21.4 pp |

Case pages show the interval above 5%; the dashboards and conference tables keep
bare integers for scannability. A rendered `39%` means roughly 28–49%.

## Known limitations

- **The long-held tail is unsolved** ([#9](https://github.com/baldrige/ceRt/issues/9)). At 5+ relists every model fitted —
  including the current ones — over-predicts (≈20% predicted against ≈17%
  observed at k=5, ≈17% against ≈13% at k≥6). This is where `hold_signal()`
  lives and it needs its own investigation rather than another feature.
- **`gap_na` has no legal story** ([#8](https://github.com/baldrige/ceRt/issues/8)). A missing lower-court date raises the forecast
  (+0.98 log-odds) and the underlying rate agrees (6.1% vs 4.4%), but this is
  more likely a data-quality proxy than a signal. Treat with suspicion.
- **`USDC` rests on 36 rows** ([#7](https://github.com/baldrige/ceRt/issues/7)) and is the
  one level this pass made *less* conservative (8.4% → 25.0%).
- **Entity typing is heuristic** (caption regexes). A mislabelled party
  mislabels its cues; ~14.5% of respondents were mistyped before 2026-07 and the
  remainder is unmeasured.
- **Amicus side is not recoverable** at the cert stage and has been removed from
  the roadmap: of 5,604 pre-decision amicus entries, 27 name a side at all and
  **none** name the petitioner. A naive whole-docket match reads post-grant
  merits briefs and looks spectacular in-sample — a leakage tripwire, not a
  feature.
- **Issue area from the Questions Presented is deferred**, not rejected. It looks
  strong alone (+0.028 AUC) and is not significant once `counsel_tier`, `pro_se`
  and `gap` are present (drop-one CI [−0.002, +0.012]).
- **OT2024 is right-censored** and excluded from the base-rate calculation
  (`complete_terms()`), though retained in the fit. Treating the censoring more
  aggressively was measured and does not help.
- `data-raw/petition_signals.json` covers only granted-or-denied dockets, so the
  Rule 10 cues **must not** be used in any model with a GVR or dismissed class.
