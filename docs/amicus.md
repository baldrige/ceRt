# Friends of the Court (`/counsel/amicus.html`)

Who files amicus briefs, at which stage, on which side, beside whom, and who
writes them. The Counsel section's second page: the masthead has no room for an
eleventh link (CLAUDE.md, *Colour*), and amicus filing is advocacy, so it sits
under `/counsel/` and the Counsel Table links to it.

| piece | where |
| --- | --- |
| Extraction, statistics, renderer | `R/amicus.R` |
| The amici in the docket JSON | `build_amici()` in `R/scotus_dash_new.R` → an `amici` column |
| Regenerate the committed stats | `.github/scripts/make_amicus_stats.R` |
| CI render | `.github/scripts/render_amicus.R` |
| Committed summary | `data/amicus_stats.json` |
| Counsel + fresh merits input | `data-raw/amici_refresh.rds`, written by `refetch-amici.yml` (`refetch_amici.R`) |
| Workflows | `conferences.yml` (nightly, self-healing) · `render-counsel.yml` (dispatch) |

## A brief is whatever the case pages say it is

Every number starts from `brief_cover()` (`R/docket_page.R`), the classifier
that colours each case page's timeline, called with the same grant date
(`docket_granted_on()`, factored out of `docket_page()` for this) and the same
Rule 37 split (`resp_merits_brief_on()`). A case page and this page cannot
disagree about whether an entry is an amicus brief, which stage it came at, or
which side's schedule it was filed on. `amicus_stats_fingerprint()` digests
those function bodies, so a change to the case pages' grammar marks the
committed stats stale.

Only entries that *open* "Brief amicus/amici curiae …" or "Amicus brief of …"
are candidates. That excludes, by construction, `Argued.` lines, scheduling
orders and motion papers that mention an amicus. The appointee's own brief
does match the pattern in one of its forms ("Brief amicus curiae of
Court-appointed amicus curiae …") and is dropped by its key.

Excluded: emergency applications and original actions (`PETITION_TYPES` only),
rejected tenders, motions for leave, and the Court-appointed amicus defending a
judgment below (a lawyer the Court appointed, not a filer).

## Sides, and why there is no win rate

The docket rarely states a side. Rule 37 does, by deadline: petitioner **or
neither party** a week after the petitioner's merits brief, respondent a week
after the respondent's. Measured on the 6,766 merits amici in
`arg_refresh.rds`, against the few whose PDF filename names a side:

| timing says | filename: resp | pet | neither |
| --- | --- | --- | --- |
| respondent | 89 | 5 | 1 |
| petitioner / neither | 3 | 85 | 16 |

The respondent bucket is clean (the 5 "pet" are proof-of-service files, not
briefs). The petitioner bucket is a sixth neither-party. So:

- the page labels the bucket **Pet./neither**, never "for the petitioner";
- the merits board reports the **case**: on the petitioner's schedule, how
  often the judgment below *fell*; on the respondent's, how often it *stood*.
  It never scores the filer, because a neither-party brief has no side to win.

Filenames name a side on ~3% of briefs, so they cannot replace the timing. The
exact side is on the PDF cover.

## Filers

The lead name before "et al." — `amicus_filer()`, keyed by `amicus_key()`
(case, punctuation, a leading "The", a corporate suffix, and "et al." set
aside: the raw strings split the Chamber of Commerce four ways). Every merged
spelling is printed under the name, as on the Counsel Table.

- **The United States** (`AMICUS_US_KEYS`) is off every board and has its own
  panel. Its cert-stage brief usually answers a CVSG (`FUNNEL_PATTERNS$cvsg`
  on the same docket); the page counts how many.
- **States** (`amicus_is_state()`): delete every State name and the joining
  words ("and", "other", "additional", "governor", numbers…); a State brief is
  one with nothing left. "Washington Legal Foundation" leaves "legal
  foundation" and is not a State. Each brief is credited to the first State
  named; "et al." co-signers are on the cover, not the docket.
- **Consolidated cases**: a brief docketed on each companion ("VIDED") counts
  once — same filer, date, stage and side.

Cases are dated by **docket year** (the `24-` in `24-1287`), not argument
Term: every docket of a year is in that year's archive, so each row is
complete except for briefing still under way, which the page counts as
"undecided": a merits case with no judgment yet. A dismissal (DIG, Rule 46,
moot) is decided, though no side won. Until 2026-10-30 the count ran 2–9 a
year for long-finished Terms. Most of those were denied petitions that
`docket_granted_on()` dated a false grant for (v49, docs/docket-pages.md), and
the rest were dismissals. Three stragglers remain: 18-557 ("The case is
remanded … with instructions to vacate", no judgment wording), 19-373
(dismissed on the petitioner's death, mid-sentence), and 24-1016, which is
genuinely undecided.

## The front door

Paid petitions only: 14.0% of 2,403 decided paid petitions with a cert-stage
amicus brief were granted, against 2.1% of 10,931 without (2026-10-29, archives
alone). Pooled with IFP the comparison was 13.2% against 0.6% — a comparison
of dockets, not of support, because IFP petitions almost never draw amici. The page says plainly
that the contrast is selection, not effect, and the filer board (≥8 decided
petitions, Wilson lower bound) answers only "of the petitions this filer
supported, how many were taken?".

## Counsel for the amici

The docket JSON has a third attorney block, `Other`, with one row per amicus
attorney: `PartyName`, `Attorney`, `Title` (firm), `IsCounselofRecord`. The
fetcher dropped it until `build_amici()`. It is kept as its own `amici`
column, **not** appended to `parties`: `parties` is a cert-model input
(`petition_features()`) and is hashed into every case page's manifest key, so
amici there would move live scores and re-render the back-catalog. The HTML-era
archives kept `party == "Other"` rows, but without firms; the page reads only
the `amici` column.

`amicus_match_counsel()` joins a brief's filer to the block's party: exact key,
then `party_score()`'s distinctive-word overlap, and a tie is no match.
Measured on six large dockets (22-451, 19-1392, 20-843, 23-719, 24-154,
25-332): **441 of 451 filers matched (98%)**; the misses are mostly "A and B"
entries the block lists as two parties.

Counsel whose firm *is* the filer (Cato's litigators on Cato's brief) are
in-house and kept off the firm board. So are two kinds of `Title` that name no
firm, matched on the keyed title:

- **placeholders** (`AMICUS_GENERIC_FIRM_RX`): "Attorney at Law", "N/A",
  "Self" … — solo lawyers' filler, which had merged 80 unrelated lawyers into
  one "firm" ranked 18th;
- **government law offices** (`AMICUS_GOV_FIRM_RX`): Solicitors General,
  Attorneys General (including the OCR'd "Attomey"), departments of justice or
  law, general counsel's offices, county/city attorneys, corporation counsel,
  "State of …". Matched on the title, not on the filer being a State: a State
  that hires Consovoy McCarthy keeps that brief on Consovoy's line (an earlier
  cut by filer kind took it from 146 to 130). All 180 matched titles were
  checked by hand on 2026-10-29; none is a private firm.

The counsel-of-record board lists everyone.

### The refresh

`refetch-amici.yml` re-fetches every archived docket with an amicus entry
(~3,100; about half an hour at 2 req/s) into `data-raw/amici_refresh.rds`,
which `amicus_input_paths()` reads first. It also fixes the merits stage of
recent years: `ot_2024.rds` was taken before most OT2024 grants were briefed
(111 respondent-side merits amici against ~400 in a normal year). Without it
the page renders from the archives with no counsel boards, and says so.

After it lands on `main`:

```
git pull
Rscript .github/scripts/make_amicus_stats.R
```

and commit `data/amicus_stats.json`. Until then each render recomputes in-process
(13 minutes locally), and the audit fails "amicus stats fresh".

## Open

- **Motion papers on the case pages.** Since v48 (#281, `COURT_APPOINTED_BRIEF_RX`)
  `brief_cover()` no longer paints non-briefs that mention a Court-appointed
  amicus dark green, but "Response to motion …", "Reply in support of motion …"
  and "Opposition to motion to dismiss or affirm" still take a brief's colour
  (24-621 shows them tan/yellow). A motion-papers rule is a separate change;
  this page is unaffected, since its entry pattern never admits them.
- **Exact side and full co-signer lists** are on the PDF cover. Reading the
  first page of ~8,000 merits briefs would replace the timing heuristic and
  name every State on a multistate brief.
- **Emergency applications** now draw amici too; they are out of scope here
  because `brief_cover()`'s stage logic is about petitions.
