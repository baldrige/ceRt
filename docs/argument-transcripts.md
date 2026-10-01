# Oral-argument transcripts

One page per argument at `arguments/<yyyy>/<docket>.html`: the Court's
recording with its transcript, each Justice's questioning, and the bench's
**post-argument lean** — the chance the petitioner prevails, read from how the
Justices divided their words. Linked from the Navigator's "Argument" column and
from the case page's "Argued" line.

## How it is published

The weekly `conferences.yml` run, in `render_arguments.R`, before the Navigator
and the docket pages (both link the result):

1. **`update_transcripts()`** (`R/argument_transcript.R`) reads the Court's
   transcript feeds for OT2017 on, downloads each transcript the site lacks —
   newest first, `TRANSCRIPTS_MAX_NEW` a run (default 150, so the back-catalogue
   fills over four runs) — parses it, and writes `arguments/<yyyy>/<docket>.json`
   plus the index `arguments/transcripts.json`. The PDFs are not kept. Bump
   `TX_PARSER_VERSION` to re-parse everything after a parser change.
2. **`render_argument_readers()`** (`R/argument_reader.R`) writes every page,
   the shared `arguments/reader.js`, and `arguments/readers.json`
   (docket → page). Each argument's judgment is cached in the index once known;
   one the run's dockets do not carry is fetched by name (`fetch_judgment()`,
   `JUDGMENTS_MAX_FETCH` a run, default 120) — an OT2017 case docketed in 2015,
   or a decision since the last range fetch. The Justices' votes come from the
   Justices section's `justices/lineups.json`.
3. The lean is scored with the coefficients in **`data/argument_lean.json`**
   (`R/argument_lean.R`), which `train_argument_lean.R` writes. Retrain after a
   Term's decisions are in: `transcript_parse.R` → `transcript_backtest.R` →
   `train_argument_lean.R`, then commit the JSON. Its stored leave-one-Term-out
   figures are what the page quotes.

Page details:

- The Term chart sorts each argument by its disposition (`argument_disposition()`):
  petitioner won, respondent won, **dismissed** (a DIG — disposed of, no
  winner), or awaiting decision; original actions are not plotted.
- The lean, bench table and Term chart are in the HTML; only the transcript and
  player need script (`reader.js` fetches the JSON beside the page).
- The audio is the Court's own MP3
  (`supremecourt.gov/media/audio/mp3files/<docket>.mp3`), played in place. A
  docket argued in two Terms has one MP3 URL, the later argument's, so the
  earlier argument's page links out instead.
- **Line times come from the recording** once `align-arguments.yml` has run on
  the argument (below); until then they are an even-rate estimate, marked "≈"
  and labelled as estimated. The estimate ran 15–28 s *ahead* of the audio in
  Slaughter's first six minutes and varied turn to turn — back-and-forth
  questioning carries more pause per word than a prepared opening — so no single
  correction fixes it.

## Timing each line to the recording

`align-arguments.yml` (every six hours; `.github/scripts/align_arguments.py`):

1. **Plan** counts transcripts not yet aligned under `ALIGN_VERSION`. Empty
   queue, and the run ends there.
2. **Four shards** each take up to `per_shard` arguments (default 12) within a
   `budget` of minutes (default 150): current and previous Terms first, then
   back through the catalogue, newest argument first. Each downloads the
   Court's MP3, runs `faster-whisper` (`tiny.en`, int8, CPU) for word
   timestamps, and matches them to the transcript:
   - **anchors**: four-word phrases that occur exactly once in both texts,
     kept only where they run forward in both (longest increasing chain), so a
     repeated phrase cannot pull the clock backwards;
   - **gaps** between consecutive anchors matched word by word;
   - every transcript word then gets a time by interpolation between matched
     words, and each turn's start is its first word's time.
   Slaughter (2.5 h): 91% of transcript words matched, last line at 9,026 s of a
   9,030 s recording, Justice Thomas's first question within 0.1 s of a
   separate check; 4.5 min of CPU on a desktop. The recognised text is never
   published — it is used only to anchor the Court's own words to the clock.
3. **Publish** writes the aligned JSON (`turns[].t`, plus `align`:
   `{v, ok, model, matched, duration}`) to gh-pages. Below 30% matched the
   argument is marked `ok: false` and not retried until `ALIGN_VERSION` is
   bumped.

A docket argued in two Terms has one MP3 URL (the later argument's), so its
earlier argument is never queued. A parser bump (`TX_PARSER_VERSION`) rewrites
the JSON without times, and the next alignment run re-times it.
- Case pages carry the link because `readers[[dkt]]` is in their render key, so
  a case page re-renders the run its argument page first appears.

## The research scripts

- **`R/argument_transcript.R`** — finds each Term's transcript PDFs (the Court's
  transcript RSS feed via `fetch_media_feed()`, the index scrape as fallback),
  downloads them paced, and parses one into speaker turns: who spoke, in which
  advocate's segment, how many words. `tx_bench()` sums the Justices' turns and
  words by side.
- **`.github/scripts/transcript_parse.R`** downloads and parses OT2017–OT2025 and
  prints a parse-quality report. **`.github/scripts/transcript_backtest.R`**
  backtests the bench's questioning against outcomes. Both write only to
  gitignored `data-raw/` files.

## How the parser reads a transcript

The Heritage Reporting layout is stable from OT2017 through OT2025: numbered
body lines 1–25 per page (page numbers and furniture are unnumbered, so keeping
only numbered lines drops them), the body opens at `P R O C E E D I N G S` and
closes at `(Whereupon ... submitted.)`, after which comes a word index.

- **Speaker labels** are capitals ending in a colon (`JUSTICE KAGAN:`,
  `GENERAL PRELOGAR:`, `MR. DUPREE:`). Matching is case-sensitive so that
  "Mr. Chief Justice, and may it please the Court:" stays speech.
- **Segments** open at `ORAL ARGUMENT OF …` / `REBUTTAL ARGUMENT OF …`, which
  run on in capitals ("ON BEHALF OF THE PETITIONERS", "FOR THE UNITED STATES, AS
  AMICUS CURIAE, SUPPORTING RESPONDENTS"). Every Justice turn in a segment is
  counted as directed at that segment's side.
- **Side rules** (`tx_header_side()`): support beats party ("respondents
  supporting petitioners" is the petitioner's side); "in support of the judgment
  below" / "affirmance" is the respondent's; a party header that names no role
  ("on behalf of the United States") takes lectern order — first principal
  segment petitioner, later respondent, rebuttal petitioner. An amicus
  supporting neither party counts for neither side.

Gotchas found on the way, each now handled:

- **"Mc" names.** `MR. McCONNELL:` and `ORAL ARGUMENT OF S. MICHAEL McCOLLOCH`
  fail a capitals test, which silently folded 22-859's entire respondent
  argument into the petitioner's. Name prefixes (`Mc`, `Mac`, `De`, …) are raised
  before matching.
- **Reporter typos** in Justice labels (`SOTOYMAYOR`, `CHIEF JUSTICE ROBERT`) are
  snapped to the roster by edit distance ≤ 2.
- **"IN SUPPORT OF"** is as common as "SUPPORTING" in amicus headers.

Quality, OT2017–OT2025 (552 transcripts): all parse; 551 identify both sides;
1–3% of Justice turns per Term fall in no side's segment, almost all of them
neutral-amicus segments, which is correct. The exceptions are a handful of
consolidated arguments where both parties are styled petitioners (e.g. 19-422).

## Does the questioning predict the outcome?

Outcome: the lead docket's judgment entry (`judgment_of()`, `JUDGMENT_RULES`) —
reversed or vacated, in whole or in part, is a petitioner win, affirmed a loss,
a dismissal no outcome; an argued application's "granted / denied by the Court" (or "referred to the
Court are granted") counts as its applicant's win or loss; an original action
has no petitioner or respondent and is left out. 528 of 552 argued cases have one; 71% are
petitioner wins. (Rules `j2`, 2026-10-01: the first version read only capitals,
missing "Judgment is affirmed", and took a respondent's *motion* to dismiss as
improvidently granted for a DIG; refitting on the corrected outcomes moved every
figure below by under a point.) Per-Justice votes come from the
published `justices/lineups.json` through `decision_votes()`. Every fit is
**leave-one-Term-out**, so no Term's outcomes inform its own forecast.

### Case level (n = 511, party segments only)

| | accuracy | Brier |
| --- | --- | --- |
| petitioner always wins | 70.5% | 0.210 |
| raw rule: side with more Justice **turns** loses | 55.0% | — |
| raw rule: side with more Justice **words** loses | 57.1% | — |
| logistic on log(resp/pet) turns and words | **72.0%** | **0.197** |

- **Words carry the signal; turn counts do not** once words are in the model
  (words z = 4.1; turns n.s.).
- The raw rules lose to the baseline because petitioners draw more words by
  construction — they go first and have rebuttal. A fitted intercept absorbs that.
- Accuracy barely moves because the base rate is 71% and the model rarely
  calls a respondent win. The forecast itself is well calibrated and spreads
  usefully:

| forecast quintile | median words to resp ÷ to pet | mean forecast | petitioner won |
| --- | --- | --- | --- |
| 1 | 0.51 | 51% | 54% |
| 2 | 0.80 | 66% | 67% |
| 3 | 0.98 | 72% | 70% |
| 4 | 1.22 | 78% | 78% |
| 5 | 1.70 | 85% | 83% |

When the bench gave the petitioner about twice the words it gave the
respondent, the petitioner won just over half the time; when the respondent got
1.7×, the petitioner won 83%.

Counting amicus segments toward the side they support makes it worse (Brier
0.204 vs base 0.207): the SG's amicus time is questioned differently.

### Justice level (4,410 votes)

Each Justice's vote from their **own** words to each side plus the whole
bench's, with a per-Justice intercept, against each Justice's own
petitioner-vote rate:

| | accuracy | Brier |
| --- | --- | --- |
| Justice's own petitioner rate | 63.5% | 0.232 |
| + own imbalance + bench imbalance | **67.4%** | **0.208** |

Both terms are strong (own z = 14, bench z = 11). The gain is largest for
Breyer, Jackson, Ginsburg, Sotomayor and Gorsuch (Brier −12 to −17%) and
smallest for Barrett, Kavanaugh and Thomas (−5%). Thomas asked nothing in 35%
of these cases, nearly all before the 2020 telephone format.

### Reading

The signal is real, stable across Terms, and calibrated — but at the case level
it is a refinement of a strong base rate, not a lever that flips many calls. It
is more informative Justice by Justice. A Navigator column would be honest as
"post-argument lean", shown as a probability with the base rate beside it,
never as a call.

Open: per-Justice "pleasantness"/tone measures; interruptions (turns ending in
`--`); whether the seriatim round (OT2021–) changes the signal; the synced
reader (forced alignment against the Court's audio).
