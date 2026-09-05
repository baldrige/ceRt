# The Court's order lists (`orders/`)

Status: **built** (`R/orders_list.R`; fetched, parsed and rendered by the
daily; a "Latest orders" panel on the landing page). Written 2026-09-04 from
the Court's OT25 listing and four sample documents parsed end to end.

## What the Court publishes

`/orders/ordersofthecourt/NN` lists one Term's order documents, newest first,
as a run of `<div>`s: a date, a kind, and a PDF link. OT25 on 2026-09-04:

| kind | count | when |
| --- | --- | --- |
| Order List (`MMDDYYzor_hash.pdf`) | 34 | the Monday after a conference, and the summer lists |
| Miscellaneous Order (`MMDDYYzr[N]_hash.pdf`) | 76 | any day the Court acts outside an order list: a stay, an execution, a grant on an application |

The PDFs have a text layer with a fixed grammar. Page 1 opens with the
citation and the date; then centred capitalised section headings, in the
Court's order:

```
ORDERS IN PENDING CASES
CERTIORARI -- SUMMARY DISPOSITIONS      (the GVRs)
CERTIORARI GRANTED
CERTIORARI DENIED
HABEAS CORPUS DENIED
MANDAMUS DENIED / PROHIBITION DENIED
REHEARINGS DENIED
ATTORNEY DISCIPLINE
```

Under each heading, one line per docket -- the number, then the caption in
caps -- and, indented, the order's prose. One prose block can close a **run**
of dockets ("The petitions for writs of certiorari are denied." after 820 of
them on the first Monday of OT25), and a `)` before or after a caption brackets
dockets the Court disposes of together. A related docket sits on its own line
in parentheses under the caption (`(26A209)`, the stay application beside a
capital petition). The list ends where any attached opinions begin: a
`SUPREME COURT OF THE UNITED STATES` running head or a `THOMAS, J.,
dissenting` header. Those are not parsed; the docket pages already link them.

Measured on four documents (a first-Monday list, a January list, the last June
list, and a one-docket miscellaneous order): 910, 418, 147 and 1 entries, every
docket assigned to its section, every run of dockets carrying its order text,
and captions intact. The bracket was the one surprise: it sits **after** the
caption on some lines, and is stripped from either end.

## Why it is worth having

The docket JSON records every one of these orders as an entry on its docket,
and the site reads those. But the order list is the Court's own statement of a
day's work, posted the morning it happens, and it is the one place a Monday's
grants, GVRs and denials sit together. It is also the audit source for the cert
funnel's grammar: what the list says was granted is what was granted, and a
docket the funnel calls "pending" that the list calls denied is a bug with a
name.

## What the site does with it

**The daily owns `orders/`.** On every run it:

1. reads the listing for the current and prior Term (two requests; the prior
   Term is needed across the October boundary),
2. downloads only the PDFs `orders/orders.json` does not hold -- none on an
   ordinary day, one on a Monday, ~110 on the first run for a Term, bounded by
   `ORDERS_MAX_NEW` (default 250) -- and parses each with `pdftools`,
3. writes each document's entries to `orders/data/<stem>.json` and its summary
   (date, kind, counts, the granted dockets, the GVR'd dockets) to the manifest,
4. renders `orders/index.html` and one page per document, incrementally by
   `ORDERS_TEMPLATE_VERSION`, and
5. puts a **"Latest orders"** panel on the landing page: documents from the
   last 21 days, at most four, each with the day, a link to its page, the
   headline counts (granted, GVRs, denied), and the granted cases by name, each
   linked to its docket page. NULL when nothing is in the window, which is the
   normal state for stretches of the summer, and the slot collapses.

The document's identity is the PDF's **stem** (`090426zor`, `072826zr2`): the
Court's file names carry a hash that could change on a re-post, and a date is
not unique -- two miscellaneous orders on one day are `zr1` and `zr2`. Page
names are dates: `orders/2026-09-04.html`, `orders/2026-07-28-misc2.html`.

Captions on the pages and the panel are the site's own (from `search.json`)
where the docket has a page, linked to it; otherwise the Court's caps, unlinked.
A denial section, which is one sentence over hundreds of dockets, is listed
tight with the sentence once at the end; a section where dockets carry their
own orders is listed with room.

Not in the masthead: the nav is at its seven-link limit (`docs/navigation.md`),
and orders are reached from the landing panel and the sitemap.

## Backfill

Dispatch the daily with `orders_terms=23,24` (any Terms the listing serves;
OT17 on) and `orders_max_new` as needed. Each Term is ~110 documents, ~4 MB of
PDF, downloaded once and never again -- only the parsed entries are kept.

## Guardrails

- **Never fatal.** A throttled listing costs the panel a run; a document that
  fails to parse is logged and retried next run (it is not written to the
  manifest).
- **No colour.** The page's CSS uses the palette tokens only.
- **Append-only per key.** `orders/orders.json` only gains documents, and only
  the daily writes it, so it needs no place in `publish_site.sh`'s union list.
- **Byte stability.** The manifest carries event dates and counts only, never a
  build time; the `rendered` field is the template version.

## Follow-ons

- Cross-check `classify_petitions()` against the granted and denied sections
  and log every disagreement: the audit this data exists for.
- Link each docket page to the order list that disposed of it.
- A grants feed entry from the list, hours before the docket JSON re-export.
- Parse the separate writings attached to a list (dissents from denial) into
  the Recent decisions grammar's "separate writing" signal.
