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
| **Opinion listings** | `/opinions/slipopinion/NN`, `/opinions/relatingtoorders/NN` | HTML tables of docket, date, PDF, author code, citation | the Recent decisions failsafe in `R/site_decisions.R` |
| **Questions Presented PDFs** | `/qp/NN-NNNNNqp.pdf` | the QP as granted, typeset text | `R/qp_extract.R` |
| **Argument transcripts index** | `/oral_arguments/argument_transcript/YYYY` | docket → transcript PDF | `attach_media()` in `R/argument_nav.R` |
| **Argument audio** | `/oral_arguments/audio/YYYY/<docket>` | stable per-case URL | same |

## Not read yet, clean, worth having

Ranked by what each adds, from the 2026-09-04 survey.

1. **A change trigger: the Hermes transfer feed.** `/rss/hermes_transfer.xml`
   is a 1 KB RSS feed naming the files the Court's internal system just pushed,
   with timestamps: on 4 Sep 2026 it listed `090426ZOR.xml` (the order list)
   and `26A274.xml` (the per curiam), stamped 15:14 ET. The XML files it names
   are not served at any path tried (`/rss/…`, `/orders/courtorders/…`,
   `/opinions/…`), so the content is unusable -- but the feed itself is a
   "something changed" signal. Polling it and dispatching the daily on a new
   item would catch an afternoon opinion or a Monday order list within minutes
   instead of at the next fixed cron (today: over an hour later for 26A274).
   Every landing-page panel benefits at once.
2. **Slip-opinion RSS.** `/rss/slipopinion_rss.aspx?TYear=NN`, back to OT17:
   proper RSS with the caption and docket in the title, the author or "per
   curiam", the PDF link, the posting time, the opinion type and the U.S.
   Reports citation as categories, and a one-paragraph **holding summary**
   written by the Reporter's office as the description. Cleaner than the HTML
   listing the failsafe parses, and the summary and citation could go on the
   Recent decisions rows.
3. **Granted & Noted List.** `/orders/NNgrantednotedlist.pdf`, ~10 text pages
   per Term, one block per argued case: docket, court below, grant date,
   argument date, decision date, author, **separate writings with their kind**
   ("Other: Jackson (D)"), result. An authoritative cross-check for the
   Navigator's decided dates and the Counsel Table's judgments; the
   separate-writings field is structured nowhere else.
4. **Argument audio and transcript RSS.** `/rss/argument_audio_rss.aspx?TYear=NN`
   and `argument_transcripts_rss.aspx`: the same data the Navigator scrapes,
   as a feed with posting times.
5. **Monthly argument calendars.** `/oral_arguments/argument_calendars/MonthlyArgumentCal<Month><Year>.pdf`,
   text PDFs listing each argument day's dockets, published ~2 months ahead
   (the October 2026 sitting appeared 4 Aug 2026). Agrees with the dockets'
   "SET FOR ARGUMENT" entries; a cross-check.
6. **Day Call.** `/oral_arguments/daycall/Day Call_MM-DD-YY.pdf`, one per
   argument day: each advocate's name, city, side, and the time allotted.
   Cleaner than parsing "Argued. For petitioner: …" if the Counsel Table ever
   wants argument time.
7. **In-chambers opinions.** `/opinions/in-chambers.aspx`, same table shape as
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
