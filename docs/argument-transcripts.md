# Oral-argument transcripts

One page per argument at `arguments/<yyyy>/<docket>.html`: the Court's
recording with its transcript, each Justice's questioning, and the bench's
**post-argument lean** — the chance the petitioner prevails, read from how the
Justices divided their words. Linked from the Navigator's "Argument" column and
from the case page's "Argued" line.

## How it is published

**The day of the argument** (`daily.yml`, `build_dashboards.R`): the daily reads
the current Term's transcript feed, parses any transcript the site lacks,
fetches its docket by name, writes just those argument pages
(`render_argument_readers(only_keys = ...)`) and `arguments/recent.json`, and
-- in a `daily.yml` step **after the publish**, on the build step's
`new_args` output -- dispatches `align-arguments.yml` so the line times follow
within the hour. (It used to dispatch from inside the build, which raced the
publish: on 6 Oct 2026 the aligner checked gh-pages before 25-498 was pushed,
found "Transcripts waiting: 0", and the argument waited for the schedule.) The
homepage's **Recent arguments** panel (`arguments_panel()`, `R/page_style.R`)
reads `recent.json`: arguments of the last three weeks, with advocates, the
lean and a "Listen and read" link. The Court posts a transcript the afternoon of
the argument, so the 20:33 UTC daily (or a Hermes-triggered one) carries it.
What waits for the nightly conferences run (06:00 UTC): the Term chart on the Term's *other* argument
pages, and the Navigator's link.

**The argument's date** is the feed's posting date -- the day of argument --
unless the Court's argument calendar (`arguments/calendar.json`) lists the
docket in that Term, in which case the calendar's date wins
(`render_argument_readers()`, written back to the index and the transcript
JSON, and the page re-rendered). The feed is not always right: OT2026's first
transcript (Suncor, 25-170, argued 5 Oct 2026) went into the feed's empty
placeholder item and kept its date, 4 Aug, so the page read "argued August 4"
and the homepage's three-week window dropped it. Older Terms are outside the
calendar manifest and keep the feed's date, which was right for them.

**Every night** (weekly until 2026-10-06), the full build — `conferences.yml`, in `render_arguments.R`,
before the Navigator and the docket pages (both link the result):

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
- A playback-speed control (1×–2×) sets the audio's own `playbackRate`, so the
  follow-along keeps step at any speed; the choice is remembered in the browser.
- The lean, bench table and Term chart are in the HTML; only the transcript and
  player need script (`reader.js` fetches the JSON beside the page).
- The audio is the Court's own MP3, played in place:
  `supremecourt.gov/media/audio/mp3files/<docket>.mp3` for a docket's first
  argument, `<docket>_2.mp3` for its second (24-109 is OT2024, 24-109_2 is
  Louisiana v. Callais reargued in OT2025), and `<n>-Orig.mp3` for an original
  action (141-Orig, not the API's 22O141). `argument_mp3()` in R and
  `mp3_url()` in the aligner hold the same rule. Two exceptions the rule cannot
  see: a case first argued before OT2017 (the count starts there), and the
  Court's older naming for OT2017–OT2018 reargued cases, `<docket>rearg.mp3`
  (15-1204, 15-1498, 17-647). When an argument fails to match, the aligner tries
  the docket's other recordings (`_2`, `_3`, `rearg`) and records the one that
  matched in `align.url`; the page plays that one.
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
   bumped. An argument with **no recording to download** — the daily
   dispatches the aligner the moment it publishes a transcript, and the MP3
   may not be up yet — is left unwritten, so the next run tries again, for
   `AWAIT_AUDIO_DAYS` (14) after its transcript was posted; only after that is
   a missing recording failed. Before this, a run that beat the MP3 marked the
   argument failed against the right URL with its alternatives tried, which
   the queue never revisits.

Two lessons from the first full pass (426 of 437 aligned, 2026-10-02):

- **The MP3 name.** The first version used the bare docket number for every
  argument, so the six original actions (files are `<n>-Orig.mp3`) matched 0%
  and the Callais reargument was matched against the OT2024 recording (3%).
  With the names above, both align at 91%.
- **Telephone audio.** The four OT2020 failures (18-1259, 19-351, 19-422,
  19-547, 22–27% matched) were the silence filter: it discarded much of the
  remote-argument audio as non-speech — 19-351 kept 4,065 words of 14,082.
  An argument under 80% matched is transcribed again without the filter and the
  better result kept (19-351: 89%). `align.vad` records which. The threshold was
  60% at first, so about 60 telephone arguments (May 2020 and OT2020) aligned at
  32-80% with the filter on and were never retried; they are re-queued (no
  `align.vad`, under 80%), and 19-783 went from 32% to 91%.

A failed argument is re-queued when the recording it was tried against
(`align.url`) is not the one the current rules would use, so a naming fix
retries the failures without re-running the aligned. A parser bump (`TX_PARSER_VERSION`) rewrites
the JSON without times, and the next alignment run re-times it.
- Case pages carry the link because `readers[[dkt]]` is in their render key, so
  a case page re-renders the run its argument page first appears -- provided
  that run renders the docket at all. The nightly conferences run does; the daily now renders
  each new argument's dockets straight after their argument pages
  (`build_dashboards.R`). Before that, a docket outside the daily's trailing
  fetch kept no link until the weekly (25-170 and 25-735, 5 Oct 2026).

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
  below" / "affirmance" is the respondent's; applicants are the applicant
  (petitioner) side. An amicus supporting neither party counts for neither side.
- **A header that names a party, not a role** ("on behalf of the Federal
  parties", "... of Michelle Cochran") is placed by `R/argument_sides.R`, below.

### Sides when the header names no role

26 of 552 arguments have one. The parser used to fall back on lectern order —
first to argue is the petitioner, everyone after the respondent — and about 14
came out wrong: in a consolidated argument the first to argue is the petitioner
of *some* docket, not necessarily the page's (24-1287 Learning Resources v.
Trump: the Solicitor General went first as 25-250's petitioner, so the page
counted the government as its petitioner), and with several parties a side,
"everyone after the first" put the House against California and Smith & Nephew
against the United States.

`resolve_argument_sides()` now places them, at render time and in the backtest:

1. **Title parties** of the docket and its consolidated companions (the JSON's
   "Vide" links): the page's docket fixes the frame, and a companion joins it
   through a title party already placed (the President is 25-250's petitioner
   and 24-1287's respondent, so 25-250's respondent is on 24-1287's petitioner
   side). Not the full party lists: they record formal roles, not alignment —
   24-1287 lists the States among its respondents, beside the President they
   sued.
2. Each header's party matched to a placed party by **name**, **initials**
   ("FCC") or **class** (federal, State, tribal, private).
3. **Argument order** for the rest: each side argues as a block, so the side
   changes once along the parties' segments. An advocate after the second block
   begins is in it; one with only a single side in view before them is the
   other side; one exactly at the boundary is ambiguous.
4. A rebuttal takes its advocate's side.

Anything still unplaced — the House in California v. Texas, an advocate in ZF
Automotive whose header names no one — leaves the argument without a lean.
Resolved sides are cached in the transcript index (`sides_v`, `SIDES_RULES`) and
written back into the transcript JSON, so the page's segment labels agree with
its lean.

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
| raw rule: side with more Justice **turns** loses | 54.6% | — |
| raw rule: side with more Justice **words** loses | 56.8% | — |
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
| 1 | 0.52 | 52% | 54% |
| 2 | 0.80 | 66% | 68% |
| 3 | 0.98 | 72% | 70% |
| 4 | 1.22 | 78% | 78% |
| 5 | 1.66 | 85% | 82% |

When the bench gave the petitioner about twice the words it gave the
respondent, the petitioner won just over half the time; when the respondent got
1.7×, the petitioner won 82%.

Counting amicus segments toward the side they support makes it worse (Brier
0.207 vs base 0.209): the SG's amicus time is questioned differently.

### Justice level (4,408 votes)

Each Justice's vote from their **own** words to each side plus the whole
bench's, with a per-Justice intercept, against each Justice's own
petitioner-vote rate:

| | accuracy | Brier |
| --- | --- | --- |
| Justice's own petitioner rate | 63.4% | 0.232 |
| + own imbalance + bench imbalance | **67.5%** | **0.208** |

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
