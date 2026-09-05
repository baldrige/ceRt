# supremecourt.gov: the machine-readable sources, covered and not

An inventory of every structured or text-extractable data stream on
supremecourt.gov, what the site reads today, and what is still on the table.
Surveyed 2026-09-04 by fetching each endpoint and inspecting its shape; the
"not clean" section is measured, not assumed. Update this file when a stream
is adopted or the Court changes one.

## Read today

| stream | endpoint | what it gives | read by |
| --- | --- | --- | --- |
| **Docket JSON** | `/rss/cases/JSON/<docket>.json` | every docket: parties, counsel, every entry with date, text and document links; `sJsonCaseType` (Paid / IFP / Application / Original) | `R/scotus_dash_new.R`; everything downstream |
| **Order lists** | `/orders/ordersofthecourt/NN` → `/orders/courtorders/*.pdf` | one Term's order documents (date, kind, PDF); PDF text with a fixed grammar of sections, dockets, captions and order prose | `R/orders_list.R` (the daily); `docs/order-lists.md` |
| **Slip-opinion RSS** | `/rss/slipopinion_rss.aspx?TYear=NN` | caption and docket, author or per curiam, PDF, posting time, opinion type and citation as categories, and the Reporter's holding summary as the description | `R/site_decisions.R`: opinion URLs and the holding line on Recent decisions |
| **Opinion listings** | `/opinions/slipopinion/NN` (fallback), `/opinions/relatingtoorders/NN` | HTML tables of docket, date, PDF, author code, citation | the Recent decisions failsafe in `R/site_decisions.R` |
| **Hermes transfer feed** | `/rss/hermes_transfer.xml` | the files the Court's internal system just pushed, with timestamps; the files themselves are not served | `watch-court.yml`: a change trigger that dispatches the daily |
| **Questions Presented PDFs** | `/qp/NN-NNNNNqp.pdf` | the QP as granted, typeset text | `R/qp_extract.R` |
| **Argument transcripts index** | `/oral_arguments/argument_transcript/YYYY` | docket → transcript PDF | `attach_media()` in `R/argument_nav.R` |
| **Argument audio** | `/oral_arguments/audio/YYYY/<docket>` | stable per-case URL | same |

## Not read yet, clean, worth having

Ranked by what each adds, from the 2026-09-04 survey. The first two on the
original list -- the Hermes-feed change trigger and the slip-opinion RSS --
were built on 2026-09-05 and have moved to the table above. One thing the
trigger's first week of logs should settle: the order list's `ZOR.xml` was
transferred the afternoon *before* it was posted (Sep 3, 13:34 ET for the
Sep 4 list), so the feed may lead an order list rather than announce it; the
Monday 14:03 UTC daily slot is the floor for those.

1. **Granted & Noted List.** `/orders/NNgrantednotedlist.pdf`, ~10 text pages
   per Term, one block per argued case: docket, court below, grant date,
   argument date, decision date, author, **separate writings with their kind**
   ("Other: Jackson (D)"), result. An authoritative cross-check for the
   Navigator's decided dates and the Counsel Table's judgments; the
   separate-writings field is structured nowhere else.
2. **Argument audio and transcript RSS.** `/rss/argument_audio_rss.aspx?TYear=NN`
   and `argument_transcripts_rss.aspx`: the same data the Navigator scrapes,
   as a feed with posting times.
3. **Monthly argument calendars.** `/oral_arguments/argument_calendars/MonthlyArgumentCal<Month><Year>.pdf`,
   text PDFs listing each argument day's dockets, published ~2 months ahead
   (the October 2026 sitting appeared 4 Aug 2026). Agrees with the dockets'
   "SET FOR ARGUMENT" entries; a cross-check.
4. **Day Call.** `/oral_arguments/daycall/Day Call_MM-DD-YY.pdf`, one per
   argument day: each advocate's name, city, side, and the time allotted.
   Cleaner than parsing "Argued. For petitioner: …" if the Counsel Table ever
   wants argument time.
5. **In-chambers opinions.** `/opinions/in-chambers.aspx`, same table shape as
   the other listings; rare (last: 23A843). A one-line addition to the
   failsafe's listing kinds.

## Not clean, or not worth it

- **The Term court calendar PDF** (`/oral_arguments/2026TermCourtCalendar.pdf`)
  has no text layer. It would be the best source for future conference dates,
  which the site infers from distribution entries on dockets.
- **The Journal** (`/orders/journal/JnlNN.pdf`): one enormous PDF per Term.
- **Press releases** (`/publicinfo/press/pressreleases.aspx`): prose; the
  "Summer Order Lists" release does announce future order dates.
- **The case distribution schedule**: a Court publication (paper-due and
  distribution dates per conference), not found at any URL tried
  (`/casedistributionschedule.aspx`, `/orders/…`, `/casehand/…`, `/docket/…`).
  If located, the best source for future conference dates.
- **The Court's 3 October 2022 order list** returns 404 from its own listing
  (`100322zor`); the first Monday of OT22 is the one order document the site
  cannot hold.

## Docket JSON fields not yet used

`sJsonCreationDate` (when the JSON was generated -- a freshness stamp),
`QPLink` (the QP PDF for granted cases, also derivable from the docket
number), `RelatedCaseNumber` (used only for companion detection).
