# The original-jurisdiction docket (22O###)

Status: **built** (`R/original_dockets.R`; fetch in `fetch_originals.R` and
the daily; pages, the `/cases/` hub, the Argument Navigator, the sitemap).
Written 2026-09-04 from a probe of the Court's docket API and a survey of every
docket it returned. What follows is the design as built and the measurements
behind each rule.

## What the Court publishes

Cases between States, and the occasional State against the United States or a
private party, begin in this Court under Article III's grant of original
jurisdiction. The Court numbers them **`22O###`**: a fixed `22O` prefix and a
sequence number that runs across the whole original docket, not per Term.

Measured on 2026-09-04 by requesting the docket JSON for `22O1` through
`22O200`:

| finding | value |
| --- | --- |
| numbers that return a docket | **44 of 200** |
| lowest / highest | `22O1` (Wisconsin v. Illinois, filed 1922) / `22O164` (Iowa and Montana v. Arizona, filed 27 Aug 2026) |
| `sJsonTerm` on every one | the sentinel `"1922"` |
| `sJsonCaseType` | `"Original"` on 43; **absent** on `22O138` |
| still active after a decree | `22O65` (Texas v. New Mexico, filed 1974): decree 1988, opinion 2020, filings in Aug 2026 |

Two consequences drive the fetch design. The numbering is **sparse** (seven of
the first eighty-two numbers exist), so `binary_search_max()`, which assumes a
contiguous bucket, cannot enumerate it, and a Term-matrix range fetch is the
wrong tool. And the `22` is **not a Term**: `.docket_term()` returns NA for
these, the `/cases/` hub lists them in a section of their own, and the sitemap
gives them a child of their own rather than filing them under OT22.

## What they are not: petitions

Before this work a `22O###` docket had no path into the site, but had one been
fetched it would have been typed **`paid`** -- `funnel_case_type()` fell through
to the sequence-number rule, and `TYPE_MAP` had no entry for `"Original"`, so
the JSON type came out NA and every page-side default was `paid`. A State-v-
State water case would have been scored for cert.

So `funnel_case_type()` now returns **`orig`**, tested before the number rule,
and every petition-side consumer filters on `PETITION_TYPES` (`paid`, `ifp`)
rather than on "not an application":

| consumer | guard |
| --- | --- |
| `classify_petitions()` (funnel, model corpus, Navigator grants, page classifier) | `type %in% PETITION_TYPES` |
| `conference_distributions()` (conference reports, relist tracker, forecast log) | drops `O\d+$` dockets before anything else |
| `update_pending()` (the by-name straggler list) | paid/IFP only |
| `update_grants_cache()` (the grants feed) | skips `[AO]\d+$` -- "Motion for leave to file a bill of complaint is GRANTED" is not a cert grant |
| `build_case()` | types by docket number first, because `22O138` has no JSON type |

Original actions **are** distributed for conference -- 41 of the 44 carry a
DISTRIBUTED entry -- and a motion for leave can be relisted for months (No. 158
was distributed for six conferences). They are kept out of the conference
reports for now because their motions are not in the petition grammar and a
conference report is the product. Giving them a row treatment there is the
obvious next step; see Follow-ons.

## The lifecycle, measured over the 44 dockets

```
motion for leave to file a bill of complaint filed        42
  -> leave DENIED                                           24   (the usual end)
  -> leave GRANTED                                          16
       -> Special Master appointed                          13
       -> report received and ordered filed                 14
       -> exceptions filed; set for argument; Argued.       16
       -> exceptions sustained/overruled; opinion           16
       -> decree entered                                    14
       -> complaint or claims dismissed                      4
```

`classify_original_events()` returns one of six outcomes with a date, and the
dated stages behind it. Resolution:

1. **Latest wins** among decree, decision, dismissal and leave denied -- the
   docket keeps going after a decision. No. 142 (Florida v. Georgia) was decided
   and remanded in 2018 and dismissed in 2021; it is dismissed. No. 65 has a
   1988 decree and a 2020 opinion on exceptions; it is decided.
2. **A dismissal counts only where there is no decree.** After a decree the
   Court tidies up -- No. 141's "the United States' claims in this case are
   hereby dismissed with prejudice" came a fortnight after its 2026 decree --
   and the decree is still the judgment.
3. **A denied motion for leave ends a case only when no motion for leave was
   ever granted.** No. 1 has a 1980 decree and a later denied motion for leave
   to file an amended complaint; it is a decree.
4. Ties go to the earlier name in the candidate vector: No. 142 was decided and
   dismissed in one entry, and "Dismissed" is what happened.

Classified: decided 8, decree 13, leave denied 21, leave granted 1 (No. 161,
Nebraska v. Colorado, granted 29 Jun 2026), pending 1 (No. 164).

The disposition box on the case page says where a granted motion has got to:
"Leave to file granted", then "Before the Special Master" once one is appointed,
then "Special Master's report filed". A granted motion for leave is the merits
boundary for brief-cover colouring, the way a cert grant is for a petition.

**Known limitation: a case argued twice keeps its first argument.** No. 141 was
argued in January 2018 and again in March 2024, on successive interim reports.
`classify_argument()` takes the first "Argued." entry and the first decision
after it, so the page's argument block and the Navigator row say 2018. The
disposition box is right (decree, May 2026) because the original classifier
takes the latest. Fixing this means teaching the argument grammar about
multiple arguments, which is a change to every page, and is deferred. A second
gap in the same grammar: it does not know "EXCEPTION SUSTAINED / OVERRULED" as
a decision form, so No. 129 (Virginia v. Maryland, 2003) shows "Argued" in the
Navigator while its box says "Decree entered".

## Fetching

Two lists, both from **`cases/original.json`**, a manifest keyed by docket
(`{caption, last_event, outcome}`) that the weekly conferences run rewrites
wholesale from what it fetched, carrying forward anything it did not see. Only
the weekly writes it; the daily reads it. Like `cases/pending.json`, it is
deliberately **not** in `publish_site.sh`'s union list -- a docket's
`last_event` has to be able to move, and only one workflow writes the file.

| run | list | size |
| --- | --- | --- |
| weekly (`fetch_originals.R`, on the pending-fetch runner) | every known docket + `ORIG_PROBE_ABOVE` (25) numbers past the highest | ~70 requests |
| weekly, first run ever (no manifest) | `22O1..22O200` once | 200 requests, ~2 min |
| daily (`build_dashboards.R`) | dockets with activity in `ORIG_LIVE_IDLE_DAYS` (730) + `ORIG_PROBE_DAILY` (3) numbers | 13 today |

"Live" is about activity, not outcome, because of No. 65. A probe past the
highest number is expected to 404 and does not count against the fetch. The
artifact is `cases-original.rds`, named so it does **not** match
`^cases-\d{2}\.rds$`: the funnel and the counsel table load Terms, and
`render_conferences.R`'s contiguity check ignores it. `render_arguments.R`
loads it by name. Both fetches are never-fatal.

## Where they appear

- **`/cases/22O###.html`** -- the same page template; posture "Original
  jurisdiction"; the disposition box from the lifecycle above; no forecast; the
  argument block only where the case was set for argument or argued (a denied
  motion with a dissent's PDF linked, No. 158, reads as "decided" to the
  argument grammar, and there was no argument to report).
- **`/cases/`** hub -- an "Original jurisdiction" section listing all of them,
  newest first, after the current Term's buckets. Never on an `otNN` page.
- **Oral Argument Navigator** -- an original action argued in a Term the
  Navigator already covers (OT17 on) gets a row; its "grant" is the order
  granting leave. Original actions argued in 1962 do not create an OT1961 page.
- **Recent decisions** -- an argued original action decided by opinion takes
  the argued kind through the existing rule. The listing failsafe rewrites the
  Court's "141, Orig." to `22O141`.
- **Sitemap** -- `sitemap-cases-original.xml`.

## Guardrails

- **No colour, no template bump.** Nothing new in `:root`; existing pages'
  markup is unchanged, so `PAGE_TEMPLATE_VERSION` stays. Original pages are new.
- **The type guard is a whitelist.** `PETITION_TYPES` exists so the next docket
  kind (there is an `In re` form, and there are `M` motions) does not fall
  through to `paid` either.
- **Byte stability.** `original.json` carries event dates only.

## Follow-ons

- Conference-report rows for motions for leave (they are distributed, and
  relisted), with their own outcome words.
- Multiple arguments in `classify_argument()`; "EXCEPTION SUSTAINED" as a
  decision form.
- A landing-page mention when a motion for leave is granted or denied, which is
  news in its own right and happens a few times a Term.
