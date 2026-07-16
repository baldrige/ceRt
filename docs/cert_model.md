# Certiorari-grant prediction model

A calibrated estimate of the probability that a **paid** petition for a writ of
certiorari is **granted plenary review**. Two tiers share one feature pipeline:

| Model | Target | Where it's shown | Features |
|---|---|---|---|
| **Baseline** | P(grant) | Daily petitions dashboards | Structural only — known at the petition stage |
| **Enhanced** | P(grant) | Conference reports | Baseline **＋** conference-stage process signals |
| **GVR** | P(GVR) | Conference reports (paired) | Same as enhanced; the companion "hold" risk |

Code: `R/cert_model.R` (pipeline) · `.github/scripts/train_cert_model.R`
(training driver) · artifacts `data/cert_model_{baseline,enhanced,gvr}.rds`.

## Why this is a calibration problem, not a classification problem

Paid petitions are granted ~4% of the time; IFP petitions ~0.1%. A model that
always predicts "deny" is ~96% accurate and useless. We therefore:

- **Segment to paid petitions.** IFP grants are so rare (34 in eight terms) that
  pooling lets the easy IFP negatives wash out the paid signal. IFP is not
  scored in v1.
- Report a **probability** and a **lift over the base rate**, never a yes/no.
- Evaluate with **AUC / average precision / Brier** and a **calibration table**,
  and Platt-scale the raw scores so the numbers mean what they say.

## Target and training data

- **Positive** = `outcome == "granted"` (plenary merits grant, per the
  `classify_petitions()` grammar in `cert_funnel.R`).
- **Negative** = `outcome == "denied"`.
- **Excluded** from training: GVRs, Rule 46 dismissals, and still-pending
  petitions — neither clean grants nor clean denials of the cert question. (Any
  petition is still *scored* at inference.)
- **Corpus**: `data-raw/ot_2017.rds … ot_2024.rds` — eight full terms, ~40.5k
  decided petitions, **11,368 paid / 496 grants (4.36%)**. Assembly is cached to
  `data-raw/cert_corpus.rds` (gitignored; regenerate by deleting it).

## The leakage rule (the thing that makes this correct)

Every **process** feature is snapshotted **strictly before the decision date**.
This is essential, not cosmetic: merits-stage amicus briefs are filed *after* a
grant, so counting amicus over the whole docket would teach the model that
"grants cause amicus briefs." Example: *United States v. Microsoft* (17-2) has
30+ amicus entries but only **one** preceded the cert grant. `process_features()`
counts only entries dated `< as_of` (the decision date in training; a conference
date at inference).

## Features

**Structural (baseline).** `pet_type`, `resp_type` — coarse entity buckets
(`us_fed` / `state_local` / `business` / `individual`) from the caption;
`court_below` — the 13 federal circuits, a pooled `STATE`, `FED_OTHER`;
`elite_counsel` — petitioner's counsel-of-record matches a curated Supreme Court
bar list; `related_present` — has consolidated/related dockets.

**Process (enhanced adds).** `n_relists` — true relists (Elwood's definition,
via `cert_funnel.R`); `n_amicus_cert` — cert-stage amicus count; `cvsg` — Court
invited the Solicitor General's views; `response_requested`; `response_filed`.

Notes: entity type is carried by the factors, so standalone `us_petitioner` /
`business_pet` logicals are **omitted** from the model (they duplicate a factor
level and would alias a coefficient to `NA`). Factor references are an
**individual** party and a **state** court, so a cue's log-odds reads against an
intuitive baseline. Residual levels that would separate the likelihood (an
`OTHER` court with zero grants) are lumped at fit time; the raw extractors keep
the granular levels.

**Relists are bucketed, not linear.** The grant rate is non-monotonic in relists
(0: 1%, 1: 20%, 2: 44%, 3–4: 36%, 5+: 19%), so `relist_bucket` `{0,1,2,3-4,5+}`
(reference 0) replaces the raw count. A linear term would extrapolate a
20-relist *hold* to ~99%; the bucket caps the ambiguous tail. This alone raised
enhanced AUC 0.888 → 0.930. (`related_present` was dropped — the archives carry
no `related` column, so `nzchar(NA)` made it a constant; it now returns
correctly on the live pipeline and can be re-added once archives carry it.)

## Performance (out-of-time, leave-one-term-out)

| Model | Target | AUC | Avg. precision | Brier | Base rate |
|---|---|---|---|---|---|
| Baseline | P(grant) | **0.720** | 0.202 | 0.038 | 0.044 |
| Enhanced | P(grant) | **0.930** | 0.484 | 0.029 | 0.044 |
| GVR | P(GVR) | **0.866** | 0.126 | 0.022 | 0.024 |

Forward check (train < OT2023, test OT2023, a complete term): baseline AUC
0.691. Calibration is good across all deciles (predicted vs. observed track
closely). The large baseline→enhanced jump is driven by relists: at the
conference stage the process signals are far more informative than any
structural cue.

Biggest structural signal, as the literature predicts (Tanenhaus cue theory;
Black & Owens on the SG): **US petitioner ≈ 43% grant rate vs 3.7%**.

## Holds and GVR risk

A petition relisted conference after conference — far beyond the 1–3 relists of a
case under active grant consideration — is being **held**, typically pending a
lead case on the same question. Empirically a held (≥5-relist) petition resolves
**~15% granted, ~20% GVR'd, ~65% denied**: a hold predicts *deferral*, most often
toward a GVR, **not** a plenary grant. This is why the relist effect *falls* in
the 5+ bucket for the grant model but *stays high* for the GVR model.

So held petitions are shown with **both** scores — a low P(grant) and an elevated
P(GVR) — rather than a single misleading number. `hold_signal()` flags a hold via
(1) serial relisting (≥6), or (2) the sharper **companion-linkage** tier: the
docket's `related` field cross-references a companion (`"Vide, NN-NNN"`) that has
already been granted. Tier 2 needs the live `related` field (absent from the
training archives), so it activates at inference, supplied with the set of
dockets granted so far. `score_disposition()` returns `p_grant`, `p_gvr`, and
`held` together — the interface the conference reports call.

## Using the model

```r
source("R/cert_funnel.R"); source("R/cert_model.R")
m <- readRDS("data/cert_model_baseline.rds")
s <- score_case(m, caption, lower, parties, date, lower_date, related)
s$prob   # calibrated P(grant);  s$lift = prob / base_rate;  s$cues = signed log-odds
```

`score_case()` returns the probability, the lift over the base rate, and a
`cues` table — the signed per-feature log-odds contributions that power the
"which cues fired" explanation. For a conference read, `score_disposition(grant_model,
gvr_model, …, granted_dockets)` returns `p_grant`, `p_gvr`, and the `held` flag
together.

## Limitations / next phases

- **No lower-court dissent / vote split** yet (Rule 10's core signal). Not in the
  docket JSON; Phase 3 would enrich via CourtListener.
- **Entity typing is heuristic** (caption regexes); a mislabeled party mislabels
  its cues.
- **OT2024 is right-censored** in its snapshot (late petitions undecided), so it
  is a pessimistic forward-test term.
- **Amicus side** (petitioner vs. respondent vs. neither) is not yet
  distinguished; v1 counts all cert-stage amicus as the salience signal.
- Issue area (from the QP text) is not yet a feature.
