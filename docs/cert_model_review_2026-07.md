# Cert model review — July 2026

The record of a full review of the certiorari model: what was wrong, what was
measured and rejected, and what remains open. `cert_model.md` documents the model
as it now stands; this documents how it got there, so the same ground is not
covered twice.

Evidence base: seven audit passes over the code, eight experiments on the
11,368-row disposition corpus and a purpose-built 16,333-row (petition ×
conference) panel, and an adversarial verification pass. 36 findings survived a
refutation attempt (9 confirmed, 27 partly true, **0 refuted outright**); ~38
were never verified because the pass ran out of budget, so anything below not
marked as measured should be treated as unconfirmed.

## The four defects that mattered

All four shared a signature worth internalising: **none of them errored, and none
were visible in aggregate metrics.** AUC could not see any of them. They were
found by asking what a specific number meant and checking it against data.

### 1. `elite_counsel` was dead for months while documented as a driver

`petitioner_counsel()` read `parties$type` / `$firm` / `$counsel_of_record` — the
schema `build_parties()` produces in the live pipeline. Every term archive uses
`names` / `attys` / `party`. So the extractor returned `""` for all 48,985 archive
dockets, the feature was constant `FALSE` across all 40,506 training rows, and
its coefficient aliased to `NA`. `score_features()` does `beta[is.na(beta)] <- 0`,
so at serve time — where the live schema *does* populate those columns — a Clement
& Murphy petition and a pro se petition scored bit-identically.

The same class of bug had already been found once (`related_present`, noted in the
old docs) and was not generalised. Guard added: `fit_cert_model()` now fails on
any aliased coefficient.

### 2. The conference column answered a different question than its label

Trained on features frozen at the disposition date; served at every conference a
petition was distributed for. It therefore estimated **P(grant | the Court acts on
this petition at this conference)** while the page called it a "calibrated model
estimate of plenary certiorari".

It was well calibrated *for the conditional it was trained on* — which is what
made it hard to see. Measured against what a reader assumes, the out-of-time
calibration slope was **0.578**.

### 3. Relists were counted on two different clocks

Training used a whole-docket count; serving counted strictly before the as-of
date. A petition denied at its first conference whose counsel files for rehearing
picks up a second `DISTRIBUTED` entry weeks *after* the denial — 695 of 11,368
paid decided petitions (6.1%), 685 of them denials. That moved 671 zero-relist
denials into the one-relist cell.

This also corrupted the public Cert Funnel page, which shares the count: it
reported **4,421** petitions relisted at least once where the truth is **1,841**,
and put the value of a first relist at 6.1% where it is 19.6%.

### 4. `lump_model_levels()` hid a separated cell inside the reference level

`court_below == "OTHER"` is 0 grants in 312 — "In re" original writs with no
court below. Lumping recoded it to `STATE`, which is the *reference* level, so
those petitions received a court contribution of exactly zero and were scored as
ordinary state-court cases: ~10% published where ~3.5% is honest, across 1,721
live-served rows. A pooled AUC cannot detect this; 300-odd rows carrying one
grant have no leverage.

Removed. Separation is now handled where it belongs, by a Firth penalty in
`fit_logit()`. That decision was made for `court_below` and turned out to be
load-bearing for `pro_se` (0 grants in 3,016), which was added later.

## Measured and rejected

Recorded so it is not re-litigated. Each was tested, not assumed.

| Proposal | Verdict |
|---|---|
| Ridge / glmnet | LOTO ΔAUC +0.0007 baseline, −0.0001 enhanced. The rolling-origin "win" is entirely a small-training-window artifact: the gap closes from +0.024 at n=1,041 to +0.001 at n=10,407, which is where we sit. |
| Collapsing / partial-pooling the 13 circuits | Predictively a coin flip (LOTO 0.9285 vs 0.9301), and `mgcv::gam` breaks the `strip_glm()`/`model.matrix()` serving path. Keep as an interpretive caution: only 15 of 78 pairwise circuit contrasts reach \|z\|>1.96. |
| Fixing `elite_counsel` as a *feature* | Worth −0.0006 AUC once `counsel_tier` exists. Keep the extractor (counsel_tier is built on it) and the guard; drop the feature. |
| `dissent_argued` / `enbanc_dissent` | Drop-one −0.0034 AUC but **+0.0033 AP** — removing them improves average precision. |
| QP issue area | Strong alone (+0.028 AUC), not significant jointly (drop-one CI [−0.002, +0.012], P=0.928) once `counsel_tier`, `pro_se` and `gap` are present. Highest infrastructure cost of anything proposed. Deferred, not rejected — retest now that those have shipped. |
| Censoring / IPCW correction | Dropping the censored term *costs* ~2.4% AP. Censoring is close to ignorable conditional on relists, which the model already conditions on. |
| Amicus side (petitioner vs respondent) | **Dead at the source.** Of 5,604 pre-decision amicus entries, 27 name a side and *none* name the petitioner; the side-naming form is a merits-stage entry. Removed from the roadmap. A naive whole-docket match reads post-grant merits briefs and looks spectacular in-sample — a leakage tripwire, not a feature. |
| SG's post-CVSG position as a feature | The raw contingency is dramatic (0 grants among CVSG-outstanding, 36 among answered) and it still buys nothing: ΔAUC +0.0000, ΔAP −0.0019. n is too small. |
| Term trend | The grant rate does not trend (4.07–5.14% across eight terms). Nothing to extrapolate. |
| Conference-calendar position in the terminal model | Null (a long-conference flag moves AUC by 0.0001). It *is* real for the hazard — the conference-level gap is 3.95× — which is re-timing, so `phase` belongs in the competing-risks model and nowhere else. |
| Expanding-window OOF as the calibrator basis | Worst of seven calibration methods (slope 1.366). |
| Rolling-origin instead of LOTO | Reproduces LOTO to within 0.003 AUC. Not worth rebuilding the evaluation — but LOTO is out-of-*fold*, not out-of-time, and the docs said otherwise. |
| Platt-vs-refit scale mismatch | Real in principle, ≤4pp in practice, direction conservative. Term-to-term drift dwarfs it. |
| Alternative calibrators (7 tested) | Total spread 0.0004 Brier. Not where the problem was. |

## Known issues

Tracked, not forgotten. All three ship in the current model.

| | issue |
|---|---|
| `USDC` moved 8.4% → 25.0% on 36 corpus rows — the one number this pass made *less* conservative | [#7](https://github.com/baldrige/ceRt/issues/7) |
| `gap_na` (+0.98 log-odds) predicts well with no legal explanation; likely a data-quality proxy | [#8](https://github.com/baldrige/ceRt/issues/8) |
| The 5+ relist tail over-predicts in every model fitted; needs the hold state modelled, not another feature | [#9](https://github.com/baldrige/ceRt/issues/9) |

## Other open questions

- **Entity typing remains heuristic.** ~14.5% of respondents were mistyped before
  this pass (bare state names, federal AGs, universities, unions typed as
  `individual`); the residual rate after the fix is unmeasured.
- **`hold_signal()` tier 2 is now reachable but unvalidated.** It never fired
  before, so there is no history of it doing anything useful.

## Process notes

Seven bugs were introduced and caught *during* this work, every one by reading
output rather than code. Three would have shipped plausible-but-wrong results
rather than errors:

- a `counsel_tier` factor that silently collapsed to a single level, which would
  have reported a null result for the strongest feature in the review — the exact
  failure mode as `elite_counsel`;
- an unnamed probability matrix from `predict.multinom` (because `strip_multinom()`
  drops `fitted.values`, which is where it sources column names) — positional
  indexing would have made "granted" read as "denied" on a public page;
- a cue-centring fix that made 313 of 400 case pages assert that *represented*
  litigants were self-represented. Centring is mathematically right and fixed the
  circuit artifact (68.9% → 0.5% of explanations led by a circuit dummy), but it
  broke an invariant the phrase table depended on: that a nonzero contribution
  implies the factor is present.

Two consecutive attempts to benchmark the competing-risks model against the
binary were invalid in *opposite* directions (first compared a rolling-origin
model against an all-terms artifact; second misaligned predictions through
`model_frame()`'s `drop_na()`, collapsing AUC to 0.57) before the third was
trustworthy.

The practical conclusions:

1. **Guards should assert content, not intent.** `n_distinct(counsel_tier) > 1`,
   `identical(model$lev, CONF_LEVELS)`, `rowSums(p) == 1`, no aliased
   coefficients. Comments do not catch anything.
2. **A number being well calibrated does not mean it is the right number.** The
   conference model was correctly estimating a quantity nobody wanted.
3. **Aggregate metrics are blind to small, high-leverage cells.** Three of the
   four defects were invisible to AUC.
4. **Check the comparator as hard as the proposal.** Half the invalid results
   here came from a flawed baseline, not a flawed treatment.
5. **A fix can be worse than the bug.** Centring and `strip_multinom()` both
   introduced defects while correcting real problems.
