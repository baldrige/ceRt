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
| Committed summary | `data/amicus_stats.json`, and the per-brief table the Term-range pages are built from, `data/amicus_briefs.rds` |
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

**Each brief belongs to the Term in which it was filed for** (since
2026-10-30; until then, to the docket year):
- a cert-stage brief belongs to the Term the case was docketed (the `24-` in
  `24-1287`);
- a merits brief belongs to the Term of the first argument on or after it;
- a merits brief in a case not yet argued belongs to the Term it was briefed
  for, with the boundary at 1 July.

The rule, the floors that scale with a range, and the range pages under
`/counsel/terms/` are in **[counsel-table.md](counsel-table.md#term-ranges-since-2026-10-30)**.
The span runs from OT2017 to the Term under way.

OT2017's merits row is incomplete: the archives begin with the `17-` dockets,
so the OT2016 cases argued in OT2017 are missing, and the page says so. The
Term under way is still being briefed, which the page counts as "undecided": a
merits case with no judgment yet. A dismissal (DIG, Rule 46,
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

## The coalition network

Above the two pair tables, the Coalitions section draws every same-side pairing
the allies table is cut from. Each node is a frequent filer, meaning at least
`AMICUS_PAIR_FILER_MIN` merits briefs. Each edge is a pair that met on the same
Rule 37 schedule in at least `AMICUS_PAIR_MIN` cases. On 2026-10-30 that was 76
filers and 353 pairings.
`amicus_network()` computes it into `data/amicus_stats.json` (`network`), and
`.amicus_network_svg()` draws it.

- **Communities:** Louvain (`.amicus_louvain()`), weighted by shared cases and
  run in a fixed node order, so the same data always gives the same groups. On
  2026-10-30 it found five groups, with modularity 0.37:
  - Cato and the Chamber of Commerce with the business and libertarian
    foundations;
  - ACLJ, Becket, the Catholic bishops and CLS;
  - CAC, NACDL, the ACLU, Public Citizen and NAACP LDF;
  - the IP bar's four associations;
  - Eagle Forum and the Immigration Reform Law Institute.

  The page names each group by its most connected members, never with a
  political label, and says the grouping is inferred from co-filing alone.
  Groups past `AMICUS_NET_GROUPS` (4) share `--net-other`. The hues are
  `PALETTE_NETWORK` in `R/palette.R`.
- **Layout:** Fruchterman-Reingold in base R (`.amicus_fr()`); igraph is not on
  the runners. Each connected piece is laid out on its own
  (`.amicus_layout()`). The largest piece fills the left 82% of the panel and
  the rest stack in the right-hand column. Laid out together, the IP bar's
  four associations, which have no tie to anyone else, were pushed to a corner
  and squeezed everything else into what was left.
- **Drawn at build time** as inline SVG, so the picture needs no script. Labels
  go to the strongest filers plus each group's strongest, wherever a label fits
  without covering another label or a large circle. The one inline script
  (`AMICUS_NETWORK_JS`) only lights a filer's partners on hover or tap and shows
  its top three in a tip. The pair tables beneath are the text equivalent.
- **"Same side" means the same schedule.** A brief on the petitioner's schedule
  may support neither party (see *Sides*), so a tie means "filed in the same
  window", as the allies table does.

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
