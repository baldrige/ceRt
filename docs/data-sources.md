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
| **Hermes transfer feed** | `/rss/hermes_transfer.xml` | the files the Court's internal system just pushed, with timestamps; the files themselves are not served. What it carries: [below](#the-hermes-feed-what-it-carries) | the court watcher (`aws/watcher/watcher.py`; `watch-court.yml` before 2026-09-29): a hint alongside the direct sources it now polls |
| **Granted & Noted List** | `/orders/NNgrantednotedlist.pdf`, OT16 on | per argued case: code, court below, grant, argument and decision dates, author, separate writings with their kind, result, unanimity flags | `R/granted_noted.R`: the Navigator's "Separate writings" column, the same line on Recent decisions, and the argument-grammar audit |
| **Monthly argument calendars** | `/oral_arguments/argument_calendars/MonthlyArgumentCal<Month><Year>.pdf` | each sitting's cases by day and order, ~2 months ahead | `R/argument_calendar.R`: schedules a case the docket has not set; cross-checks the rest |
| **The home-page calendar** | `/` (a Telerik RadCalendar; another month is a postback with `__EVENTARGUMENT=n:k`) | every marked day, September 2021 to the end of the announced Term: conferences, argument and non-argument days, holidays by name; order-list days only in retrospect | `R/court_calendar.R`: future conference dates, and the order lists expected from them (`conferences/court_calendar.json`, the landing page's calendar) |
| **Day Calls** | `/oral_arguments/daycall/Day Call_MM-DD-YY.pdf` | each argument day's advocates with side, affiliation and minutes | `R/argument_calendar.R`: the Navigator's "Argued by" before the argument |
| **In-chambers opinions** | `/opinions/in-chambers.aspx` | a single Justice's opinion on an application, all Terms on one page | the Recent decisions failsafe in `R/site_decisions.R` |
| **Questions Presented PDFs** | `/qp/NN-NNNNNqp.pdf` | the QP as granted, typeset text | `R/qp_extract.R` |
| **Argument audio and transcript feeds** | `/rss/argument_audio_rss.aspx?TYear=NN`, `/rss/argument_transcripts_rss.aspx?TYear=NN` (OT17 on) | one item per argued case: caption and docket, the audio page or transcript PDF, and when it was posted | `attach_media()` in `R/argument_nav.R`, since 2026-09-06: a link is offered only once the Court has posted the file |
| **Argument transcripts index** | `/oral_arguments/argument_transcript/YYYY` | docket → transcript PDF | the fallback for a Term whose feed is down |
| **Argument audio** | `/oral_arguments/audio/YYYY/<docket>` | stable per-case URL | the fallback for a Term whose feed is down |

### The Hermes feed: what it carries

The feed is the Court's **orders and the written opinions that go with them**
-- not docket activity. Every change to it, of any kind, does the same thing:
the watcher dispatches `daily.yml` (unless one was dispatched under 10 minutes
ago or a daily is live, in which case the next 5-minute poll tries again). From
the ten transfers `watch-court.yml` logged (5-29 Sep 2026) and the feed itself
(5-7 Oct 2026):

| file | what it was | seen |
| --- | --- | --- |
| `MMDDYYzor.xml` | a regular order list | `090426ZOR`, `100526ZOR` |
| `MMDDYYzr.xml`, `zr1`, `zr2`... | a miscellaneous order, one file per order that day | `090826zr`, `091026zr` and `zr1`, `100126zr` (the three grants) -- every one is in `orders/orders.json` as a `misc` order of that date |
| `26A###.xml` | the full Court's order on an emergency application referred to it | 26A274 granted (4 Sep); 26A305, 26A308, 26A388 (14 and 25 Sep); 26A428 (30 Sep) -- each the same day as "Application ... referred to the Court" and its disposition |
| a petition docket, `25-7499.xml` | a written opinion on an order: Justice Sotomayor's statement respecting the denial in *Mulkey v. Alabama*, published with the 5 Oct list | file dated 30 Sep, transferred 5 Oct |

**Not on it:** argument transcripts and audio (the feed did not move through
the arguments of 5 and 6 Oct 2026), ordinary docket activity (filings,
distributions, the docket JSON), Day Calls, argument calendars. **Not yet
observed:** merits opinions -- none were issued in the window. The written
opinions on orders above suggest slip opinions arrive the same way, as
docket-numbered files; confirm on the first opinion day.

**Two timestamps, and neither is "posted".** An item's `pubDate` is the
file's modification time; the channel's `pubDate` is the last transfer.
`100526ZOR.xml` is dated 1 Oct 15:13 ET, but it was not in the feed on 1 Oct
(the channel then read 1 Oct 16:44 with only `100126zr`, `zr1` and
`26A428`): it arrived with the channel stamp of 5 Oct 10:05 ET, **35 minutes
after** the 9:30 release. So the feed does not reliably lead an order list,
and the Monday 14:03 UTC daily is still the one that carries it.

**Why the watcher no longer relies on it** (October 2026): it misses argument
transcripts and audio entirely and trails the order list, so the court watcher
polls the order lists page, the slip-opinion and argument feeds and the
opinions-relating-to-orders page directly, keyed by content (that page's
`Last-Modified` moves with no new document on it), with Hermes kept as a hint.

**The feed holds only the last two or three files**, so it is no record:
`watch-court.yml`'s logs (expiring) were the only history, and the AWS
watcher logs a fingerprint, not the items.

## Not read yet, clean, worth having

Nothing remains here as of 2026-09-07: every clean stream the survey found is
in the table above. The list below is kept as the record of what was found
and in what order it was taken up.

Ranked by what each adds, from the 2026-09-04 survey. The first two on the
original list -- the Hermes-feed change trigger and the slip-opinion RSS --
were built on 2026-09-05 and have moved to the table above. The question this
left -- whether the feed leads an order list (the Sep 4 list's `ZOR.xml` was
dated Sep 3, 13:34 ET) -- is answered above: that date is the file's, not the
transfer's, and the 5 Oct list reached the feed 35 minutes after release.

The Granted & Noted List was built on 2026-09-06 and has moved to the table
above (`docs/granted-noted.md`).

The argument audio and transcript feeds were adopted on 2026-09-06 and have
moved to the table above.

1. **Monthly argument calendars.** `/oral_arguments/argument_calendars/MonthlyArgumentCal<Month><Year>.pdf`,
   text PDFs listing each argument day's dockets, published ~2 months ahead
   (the October 2026 sitting appeared 4 Aug 2026). Agrees with the dockets'
   "SET FOR ARGUMENT" entries; a cross-check.
2. **Day Call.** `/oral_arguments/daycall/Day Call_MM-DD-YY.pdf`, one per
   argument day: each advocate's name, city, side, and the time allotted.
   Cleaner than parsing "Argued. For petitioner: …" if the Counsel Table ever
   wants argument time.
3. **In-chambers opinions.** `/opinions/in-chambers.aspx`, same table shape as
   the other listings; rare (last: 23A843). A one-line addition to the
   failsafe's listing kinds.

## Not clean, or not worth it

- **The Term court calendar PDF** (`/oral_arguments/2026TermCourtCalendar.pdf`)
  has no text layer. Superseded: the home-page calendar above is the same data
  as text.
- **The Journal** (`/orders/journal/JnlNN.pdf`): one enormous PDF per Term.
- **Press releases** (`/publicinfo/press/pressreleases.aspx`): prose; the
  "Summer Order Lists" release (around 1 July) is the only place the Court
  announces order-list dates, and the three summer lists are the ones
  `expected_order_lists()` cannot infer, since they follow no conference.
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
