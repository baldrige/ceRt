# Subject areas

Every docket with readable questions presented gets a subject area: one of the
14 `issueArea` categories of the [Supreme Court Database](https://scdb.la.psu.edu/)
(Criminal Procedure, Civil Rights, First Amendment, Due Process, Privacy,
Attorneys, Unions, Economic Activity, Judicial Power, Federalism, Interstate
Relations, Federal Taxation, Miscellaneous, Private Action). It appears as a
"Subject area" row in the case page's Case panel and as a filterable column on
the daily docket. The public explanation is the About page's `#subject-areas`
section (`write_about_page()` in `R/page_style.R`).

## How a label is made

| step | where |
| --- | --- |
| Input: caption (`cases/search.json`) + questions presented (the three `qp_cache.json`, merged as `render_dockets_for()` merges them) | `classify_site.py` |
| Trailing front matter cut ("TABLE OF CONTENTS", "LIST OF PARTIES", ...) | `readable.py: clean_qp()` |
| Unreadable QPs skipped -- OCR of handwriting, cover pages, tables of contents (~6% of the cache) | `readable.py: readable()` |
| Classification: Jev (TypeSafe), a pick-one over the 14 areas with each area's definition as the option's description | `classify.py`, `areas.py` |
| Stored: area, confidence, runner-up, input hash, and which model + definitions produced it | `cases/subjects.json` |
| Shown only at confidence ≥ `SUBJECT_MIN_CONFIDENCE` (0.7) | `R/subject_area.R` |

The classifier is Python because Jev's only client is (`pydantic-ai-slim[typesafe]`,
pinned in `requirements.txt`; the model is pinned to `jev-1.13.0` in `classify.py`).
R calls it through `refresh_subjects()`, which is never fatal: without Python,
the package or the `TYPESAFE_API_KEY` secret it logs a line and the pages render
with the labels already on file.

Runs are incremental. An entry is reused while its input hash and its producing
model + definitions (`by`) are unchanged, so the daily pays for new QPs only.
**Editing `areas.py` re-labels everything** on the next run: the definitions
file's hash is part of `by`. Run `subject-areas.yml` after such an edit rather
than letting the daily's `max_new` cap (2000) spread it over several days, then
re-render the pages.

## Where the definitions came from, and how well it works

Jev takes no labelled examples; the docstrings in `areas.py` are all it learns
the categories from. They paraphrase the Database's codebook, including its
counter-intuitive conventions, each confirmed in the Database's own data:
habeas corpus and the Second Amendment are Criminal Procedure (*Heller*, *Bruen*,
*Rahimi*, *Wolford*); deportation, section 1983 suits and tribal authority are
Civil Rights; takings, forfeiture, vagueness and personal jurisdiction are Due
Process; employment arbitration is Unions; *Bivens*, FTCA and FSIA suits are
Economic Activity; standing, mootness and agency review are Judicial Power;
Commerce Clause limits on federal power are Federalism (*Lopez*, *Comstock*).

Measured 2026-09 (scripts in `.github/scripts/subject_area/`):

| check | n | accuracy | macro-F1 | top-2 |
| --- | --- | --- | --- | --- |
| Database coding, dev Terms OT17–21 (definitions tuned here) | 309 | 79.6% | 0.71 | 91% |
| Database coding, test Terms OT22–25 (held out) | 264 | 77–80% | 0.74–0.76 | 92% |
| Hand-labelled sample, ungranted OT25–26 petitions | 139 | 80.6% | 0.78 | 96% |
| — of which paid / IFP | 72 / 67 | 75.0% / 86.6% | | |

Baselines on the same test Terms: always "Economic Activity" 25%; TF-IDF +
logistic regression trained on the dev Terms 56% (macro-F1 0.20).

Things the numbers say:

- **The confidence runs high.** Picks at ≥ 0.9 average 0.98 but are right ~88%
  of the time. The 0.7 threshold was read off the measured coverage table (≈84%
  of cases labelled, ≈84–86% of those right), not off the model's own numbers.
- **Confidence does not catch garbage.** On the 11 unreadable sample inputs it
  ran 0.32 to 1.0. That is why `readable()` exists and runs first.
- **Most disagreements are not fixable from the QP.** The Database codes the
  decision, so a case dismissed as moot or decided on standing is Judicial Power
  whatever the petition asked. Economic Activity cases with procedural questions
  are the largest single confusion (→ Judicial Power).
- **Rewording moves borderline cases.** Jev is nearly deterministic (262/264
  identical picks on a repeat), but any edit to `areas.py` flips a handful of
  close calls; adding the Second Amendment and Commerce Clause rules moved six
  held-out cases, none of them about either rule. Judge an edit on the dev
  Terms, then check the test Terms once.

## Re-running the evaluation

```
pip install -r requirements.txt scikit-learn
# Database coding: SCDB case-centered, organized by docket (scdb.la.psu.edu, 2026 release 01)
python eval_scdb.py --scdb SCDB_2026_01_caseCentered_Docket.csv \
    --qp qp_arguments.json qp_conferences.json qp_dashboards.json --split dev [--baseline]
# Hand-labelled sample (label = null marks an unreadable QP)
python eval_sample.py --sample sample_labelled.json --label-field claude_label --out out/
python eval_sample.py --sample sample_labelled.json --reviewed out/review_sample.csv --out out/
```

The key is read from `TYPESAFE_API_KEY`, else `~/.typesafe_key` (or `.txt`).
