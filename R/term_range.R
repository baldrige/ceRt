# term_range.R ------------------------------------------------------------------
# Term ranges for the Counsel section: which Term a fact belongs to, the floors a
# shorter range is held to, and the picker that moves between ranges.
#
# A FACT BELONGS TO THE TERM IN WHICH IT HAPPENED.
#
#   * A petition, and an amicus brief at the petition stage, belong to the Term
#     the case was DOCKETED -- the 24- of 24-1287 is OT2024. The Clerk's docket
#     year and the October Term coincide, which is why the Term archives
#     (ot_2024.rds) hold exactly the 24- dockets.
#   * An argument belongs to the Term it was ARGUED in, from its date; and a
#     merits amicus brief and the judgment it is read against go with the
#     argument they were filed for. A case docketed 24- and granted in January
#     2025 is argued, briefed and decided in OT2025.
#
# So one case can sit in two Terms, and does whenever it is granted late in a
# Term: its cert-stage briefs in the one, its merits briefs in the next.
#
# Coverage follows from that. The archives start at the 17- dockets, so OT2017's
# ARGUMENTS hold only the few 17- cases argued that Term (18, measured
# 2026-10-30, against ~60 in a full Term): the 16- dockets argued in OT2017 were
# never fetched. Each page states this rather than letting OT2017 read as a
# quiet Term.

suppressPackageStartupMessages(library(tidyverse))

# The Term of a docket number: "24-1287" -> 2024. NA for an application or
# anything else without a two-digit docket year.
term_of_docket <- function(dkt) {
  2000L + suppressWarnings(as.integer(str_match(dkt, "^(\\d{2})-")[, 2]))
}

# The October Term a date falls in. A Term opens on the first Monday in October
# and runs until the next one opens, so September belongs to the Term before:
# the special session of 9 September 2009 (Citizens United) is OT2008.
term_of_date <- function(d) {
  d <- as.Date(d)
  as.integer(lubridate::year(d) - (lubridate::month(d) < 10))
}

# An argument entry: "Argued. For petitioner: ..." -- with the docket number
# prefixed on a consolidated case ("21-588 (21A85) Argued. ...").
ARGUED_ENTRY_RX <- regex("^([0-9A-Z-]+\\s+(\\([0-9A-Z]+\\)\\s+)?)?Argued\\.", ignore_case = TRUE)

# Every argument date on a docket.
argued_dates_of <- function(ev) {
  if (!is.data.frame(ev) || !"Proceedings and Orders" %in% names(ev)) return(as.Date(character()))
  hit <- str_detect(coalesce(ev[["Proceedings and Orders"]], ""), ARGUED_ENTRY_RX)
  suppressWarnings(lubridate::mdy(ev$Date[hit]))
}

# The Term a merits-stage brief belongs to: that of the first argument on or
# after it; else -- a case not yet argued, or never argued -- the Term it was
# briefed for. Merits briefing runs through the summer into the October
# sittings, so for that case the boundary is 1 July rather than October. That
# is one Term early for a case granted in January and set over to the next
# Term, until it is argued; the argument date then decides.
term_of_merits_brief <- function(brief_date, argued_dates) {
  argued_dates <- sort(argued_dates[!is.na(argued_dates)])
  vapply(seq_along(brief_date), function(i) {
    b <- brief_date[i]
    if (is.na(b)) return(NA_integer_)
    a <- argued_dates[argued_dates >= b]
    if (length(a)) return(term_of_date(a[1]))
    as.integer(lubridate::year(b) - (lubridate::month(b) < 7))
  }, integer(1))
}

# ---- floors -----------------------------------------------------------------

# The span the section's floors were set on: OT2017-OT2025. Every floor on both
# pages was chosen by looking at boards over these nine Terms (the reasons are
# beside each constant), so that is the span a floor means what it says on.
TERM_FLOOR_SPAN <- 9L

# A floor scaled to a range of `n` Terms: in proportion, never above the
# full-span floor and never below `lo`. A floor of 8 decided petitions over nine
# Terms is about one a Term; over two Terms it is 2, and the board stays a
# board instead of emptying. `lo` is each floor's own minimum, below which a
# rate or a pairing says nothing at all.
scale_floor <- function(floor, n, lo = 1, span = TERM_FLOOR_SPAN) {
  if (n >= span) return(floor)
  # A count rounds to a whole number; a continuous floor (expected grants) does not.
  if (is.integer(floor)) as.integer(max(lo, round(floor * n / span)))
  else max(lo, round(floor * n / span, 2))
}

# ---- ranges -----------------------------------------------------------------

# Every contiguous range within [from, to], as a tibble of from/to.
term_ranges <- function(from, to) {
  t <- seq.int(from, to)
  tidyr::expand_grid(from = t, to = t) |> filter(from <= to)
}

term_range_label <- function(from, to) {
  if (from == to) paste0("OT", from) else paste0("OT", from, "–OT", to)
}

# The directory a range's pages live in, under /counsel/terms/. The full span
# has none: it is the section's own /counsel/ pages.
term_range_slug <- function(from, to) {
  if (from == to) paste0("OT", from) else paste0("OT", from, "-OT", to)
}

# Where a page of the section lives for a range. `page` is "index.html" or
# "amicus.html"; `span` is that page's own full span, which maps to the
# canonical page. A range is clamped to the page's span, so a link from an
# amicus range reaching OT2026 lands on the Counsel Table's last Term.
term_range_href <- function(page, from, to, span) {
  from <- max(from, span[1]); to <- min(to, span[2])
  if (from > to) { from <- span[1]; to <- span[2] }
  file <- if (page == "index.html") "" else page
  if (from == span[1] && to == span[2]) return(paste0("/counsel/", file))
  paste0("/counsel/terms/", term_range_slug(from, to), "/", file)
}

# ---- the picker ---------------------------------------------------------------

TERM_PICKER_CSS <- "
  .tpick{display:flex;flex-wrap:wrap;align-items:center;gap:.4rem .6rem;font-size:.9rem;
    color:var(--ink-soft);margin:.2rem 0 1.4rem;padding:.55rem .75rem;
    border:1px solid var(--rule);background:var(--panel)}
  .tpick label{display:inline-flex;align-items:center;gap:.35rem}
  .tpick select{font:inherit;color:var(--ink);background:var(--field);border:1px solid var(--rule);
    border-radius:2px;padding:.15rem .3rem}
  .tpick button{font:inherit;color:var(--paper);background:var(--ink);border:0;border-radius:2px;
    padding:.2rem .7rem;cursor:pointer}
  .tpick .all{margin-left:auto;color:var(--link)}
  .tpick .now{color:var(--ink);font-weight:600}
"

# The picker for one page: two Term selects and a button, plus a link back to
# the full span from a range page. It needs its script to navigate; without
# one the reader keeps the page they are on, which is the full span unless
# they arrived at a range by a link.
term_picker_html <- function(page, from, to, span) {
  opts <- function(sel) paste0("<option value='", seq.int(span[1], span[2]), "'",
                               ifelse(seq.int(span[1], span[2]) == sel, " selected", ""),
                               ">OT", seq.int(span[1], span[2]), "</option>", collapse = "")
  full <- from == span[1] && to == span[2]
  paste0(
    "<form class='tpick' id='tpick' data-page='", page, "' data-span='", span[1], "-", span[2], "'",
    " onsubmit='return false'>",
    "<span>Terms: <span class='now'>", term_range_label(from, to), "</span></span>",
    "<label>from <select id='tp-from' aria-label='First Term'>", opts(from), "</select></label>",
    "<label>to <select id='tp-to' aria-label='Last Term'>", opts(to), "</select></label>",
    "<button type='button' id='tp-go'>Show</button>",
    if (!full) paste0("<a class='all' href='", term_range_href(page, span[1], span[2], span),
                      "'>All Terms</a>") else "",
    "</form>",
    "<script>", TERM_PICKER_JS, "</script>")
}

TERM_PICKER_JS <- "
(function(){
  var f=document.getElementById('tpick'); if(!f) return;
  var page=f.getAttribute('data-page'), sp=f.getAttribute('data-span').split('-').map(Number);
  var a=document.getElementById('tp-from'), b=document.getElementById('tp-to');
  function slug(x,y){return x===y?'OT'+x:'OT'+x+'-OT'+y;}
  function go(){var x=+a.value,y=+b.value; if(x>y){var t=x;x=y;y=t;}
    var file=page==='index.html'?'':page;
    location.href=(x===sp[0]&&y===sp[1])?'/counsel/'+file:'/counsel/terms/'+slug(x,y)+'/'+file;}
  document.getElementById('tp-go').addEventListener('click',go);
  [a,b].forEach(function(s){s.addEventListener('keydown',function(e){if(e.key==='Enter') go();});});
})();
"

# The first Term whose arguments the archives hold completely. OT2017's hold
# only the 17- cases argued that Term (see the header).
TERM_ARGUMENTS_FROM <- 2018L

num_or_na <- function(x) if (is.null(x) || !length(x)) NA_real_ else suppressWarnings(as.numeric(x[[1]]))

# What a page needs to know about its range, from its stats' totals: NULL for a
# stats file written before ranges existed, which renders without a picker.
.term_nav <- function(tot) {
  if (is.null(tot$term_from) || is.null(tot$span_from)) return(NULL)
  from <- as.integer(tot$term_from); to <- as.integer(tot$term_to)
  span <- as.integer(c(tot$span_from, tot$span_to))
  # The other page's span, which the range renderer adds: the two pages' spans
  # differ (the amicus page reaches the Term under way), and a link across has
  # to be clamped to the page it lands on.
  other <- if (is.null(tot$other_from)) span else as.integer(c(tot$other_from, tot$other_to))
  list(from = from, to = to, span = span, other = other, span_n = span[2] - span[1] + 1L,
       part = !(from == span[1] && to == span[2]))
}

# A link to the other page of the section for the same range, clamped to that
# page's span; `default` (its canonical page) from a full-span page.
.term_href <- function(rng, page, default) {
  if (is.null(rng) || !rng$part) return(default)
  term_range_href(page, rng$from, rng$to, rng$other)
}

# The sentence a page adds when its range reaches OT2017's merits stage, which
# the archives hold only in part. `what` names the thing ("arguments", "merits
# briefs").
.term_coverage_note <- function(rng, what) {
  if (is.null(rng) || rng$from >= TERM_ARGUMENTS_FROM) return("")
  paste0(" <strong>OT2017&rsquo;s ", what, " are incomplete:</strong> the archive ",
         "begins with the cases docketed in OT2017, so the OT2016 cases heard in ",
         "OT2017 — most of that Term&rsquo;s arguments — are not here.")
}

# The <head> additions for a range page: it is a view of the canonical page,
# so search engines are pointed there and asked not to index the range itself.
term_range_head <- function(page, span) {
  file <- if (page == "index.html") "" else page
  paste0("<meta name='robots' content='noindex,follow'>",
         "<link rel='canonical' href='https://supremecourt.report/counsel/", file, "'>")
}
