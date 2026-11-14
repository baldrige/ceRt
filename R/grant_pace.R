# The grant pace: cert grants made in a Term's window, day by day, against every
# other complete Term's, with the rest of the current Term projected from what
# the past Terms granted after the same day. Shown on each Justices Term page
# from OT2018 on (R/justices.R): the current Term's carries the projection, a
# finished Term's its whole line against the others.
#
# A grant is classify_petitions()'s "granted" (cert, cert before judgment,
# probable jurisdiction noted; never a GVR), unioned with the CERTIORARI GRANTED
# entries of the Court's order lists (orders/orders.json, R/orders_list.R). Each
# fills the other's gaps: the order lists are fresher than any docket fetch, and
# the dockets hold the grants of the few order documents the site never read
# (the first-Monday list of 3 Oct 2022 among them). A docket counts once, on its
# earliest grant date. The docket side is written by render_arguments.R, the
# run that holds the dockets, to arguments/grant_dates.json; the Justices render
# has no dockets and reads that file.
#
# The window is 1 August to 31 July, so the long conference's late-September
# grants open a Term and the June clean-up grants close it. That is NOT the
# Justices pages' own unit -- the cases argued in a Term -- and the panel says so.
#
# The projection is arithmetic, not a model: today's count plus what each past
# Term granted between this day of its window and the end; the dashed line is
# their mean and the band runs from the slowest to the fastest.

GRANT_PACE_FIRST <- 2018L   # the first window the site's data covers whole
NUM_WORDS <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
               "eleven", "twelve")
ORDINAL_WORDS <- c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth",
                   "tenth", "eleventh", "twelfth")

# Window year of a date: the Term whose August-to-July window holds it.
grant_window <- function(d) as.integer(ifelse(as.integer(format(d, "%m")) >= 8L,
                                              as.integer(format(d, "%Y")),
                                              as.integer(format(d, "%Y")) - 1L))

GRANT_DATES_FILE <- "arguments/grant_dates.json"   # under SITE_DIR

# The order lists' grants: one row per granted docket, dkt and date.
orders_grants <- function(orders_json) {
  empty <- data.frame(dkt = character(), date = as.Date(character()), stringsAsFactors = FALSE)
  if (is.null(orders_json) || !file.exists(orders_json)) return(empty)
  man <- tryCatch(jsonlite::fromJSON(orders_json, simplifyVector = FALSE), error = function(e) NULL)
  o <- do.call(rbind, lapply(man, function(v) if (length(v$granted))
    data.frame(dkt = vapply(v$granted, function(x) as.character(x$dkt), ""),
               date = as.Date(v$date), stringsAsFactors = FALSE)))
  if (is.null(o)) empty else o
}

# Earliest date per docket.
.first_grant <- function(g) { g <- g[!is.na(g$date), ]; g <- g[order(g$date), ]; g[!duplicated(g$dkt), ] }

# One row per granted docket: dkt, date. `cases` is a docket frame for
# classify_petitions(); `orders_json` the path to orders/orders.json (optional).
grant_dates <- function(cases, orders_json = NULL) {
  g <- classify_petitions(cases)
  g <- g[g$outcome %in% "granted" & !is.na(g$outcome_date), c("dkt", "outcome_date")]
  g <- data.frame(dkt = g$dkt, date = as.Date(g$outcome_date), stringsAsFactors = FALSE)
  .first_grant(rbind(g, orders_grants(orders_json)))
}

# Merged into what the file already holds, never replacing it: a grant does not
# un-happen, so a run whose dockets are thin cannot shrink the record.
write_grant_dates <- function(site_dir, gd) {
  path <- file.path(site_dir, GRANT_DATES_FILE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  old <- read_grant_dates(site_dir, orders = FALSE)
  g <- .first_grant(rbind(old, gd[, c("dkt", "date")]))
  writeLines(jsonlite::toJSON(list(dkt = g$dkt, date = format(g$date))), path, useBytes = TRUE)
  invisible(nrow(g))
}

# The docket grants from GRANT_DATES_FILE, joined (unless orders = FALSE) with
# the order lists' grants, which are fresher than any docket fetch.
read_grant_dates <- function(site_dir, orders = TRUE) {
  path <- file.path(site_dir, GRANT_DATES_FILE)
  g <- data.frame(dkt = character(), date = as.Date(character()), stringsAsFactors = FALSE)
  if (file.exists(path)) {
    j <- jsonlite::fromJSON(path, simplifyVector = TRUE)
    if (length(j$dkt)) g <- data.frame(dkt = as.character(j$dkt), date = as.Date(j$date), stringsAsFactors = FALSE)
  }
  if (orders) g <- rbind(g, orders_grants(file.path(site_dir, "orders", "orders.json")))
  .first_grant(g)
}

# The panel's HTML for the window of `term` (a four-digit year), or "" when the
# Term is before GRANT_PACE_FIRST, not yet open, has granted nothing, or has no
# other Term to compare with. The open window (the one holding `as_of`) is drawn
# to date and projected to July; a finished one is drawn whole, against every
# other finished Term.
grant_pace_panel <- function(grants, term = grant_window(as_of), as_of = Sys.Date()) {
  open <- grant_window(as_of)
  cur <- as.integer(term)
  if (cur < GRANT_PACE_FIRST || cur > open) return("")
  done <- cur < open
  start <- as.Date(sprintf("%d-08-01", cur))
  end_day <- 364L                                    # 31 July (30 July in a leap window)
  today <- if (done) end_day else as.integer(as_of - start)
  w <- grant_window(grants$date)
  day <- as.integer(grants$date - as.Date(sprintf("%d-08-01", w)))
  others <- setdiff(seq.int(GRANT_PACE_FIRST, open - 1L), cur)
  keep <- w %in% c(others, cur) & day <= end_day & grants$date <= as_of
  if (!length(others) || !any(w[keep] == cur)) return("")
  terms <- lapply(setNames(c(others, cur), c(others, cur)), function(y) sort(day[keep & w == y]))

  cum <- function(y, d) sum(terms[[as.character(y)]] <= d)
  now <- cum(cur, today)
  tot <- vapply(others, function(y) length(terms[[as.character(y)]]), 1L)
  ot <- function(y) paste0("OT", y)
  nw <- function(n) if (n >= 1 && n <= length(NUM_WORDS)) NUM_WORDS[n] else as.character(n)
  span <- paste0(ot(min(c(others, cur))), "&ndash;", ot(max(c(others, cur))))

  if (done) {
    # Where the Term ranks among all of them, busiest first; a tie says so.
    n_lt <- sum(tot < now); n_gt <- sum(tot > now); L <- length(others)
    tied <- n_lt + n_gt < L
    of_all <- sprintf("of the %s Terms from %s to %s", nw(L + 1L), ot(min(c(others, cur))), ot(max(c(others, cur))))
    cmp <- if (n_gt == 0) paste(if (tied) "tied for the most" else "the most", of_all) else
           if (n_lt == 0) paste(if (tied) "tied for the fewest" else "the fewest", of_all) else
           paste0(if (tied) "tied for " else "", "the ", ORDINAL_WORDS[n_gt + 1L], "-most ", of_all)
    summary <- sprintf(paste0(
      "The Court granted %d petitions between August 1, %d, and July 31, %d, %s. ",
      "The other %s granted between %d and %d (%.1f on average)."),
      now, cur, cur + 1L, cmp, nw(L), min(tot), max(tot), mean(tot))
    key <- paste0("<span><i></i>", ot(cur), "</span><span><i class='gp-past'></i>the other Terms, ", span, "</span>")
    yrs <- sort(c(others, cur), decreasing = TRUE)
    rows <- paste(vapply(yrs, function(y) sprintf("<tr%s><td>%s</td><td>%d</td></tr>",
      if (y == cur) " class='gp-cur'" else "", ot(y), length(terms[[as.character(y)]])), character(1)), collapse = "")
    thead <- "<tr><th>Term</th><th>Grants</th></tr>"
    as_of_l <- "July 31"
  } else {
    by_now <- vapply(others, cum, 1L, d = today)
    rest <- tot - by_now
    proj <- now + mean(rest)
    rank_word <- if (now < min(by_now)) "fewer than in any of the last" else
                 if (now > max(by_now)) "more than in any of the last" else "within the range of the last"
    as_of_l <- paste(format(as_of, "%B"), as.integer(format(as_of, "%d")))
    summary <- sprintf(paste0(
      "The Court has granted %d petitions since the window opened on August 1: %s %s Terms by %s, ",
      "which granted between %d and %d (%.1f on average). If the rest of the Term runs like an average past one, ",
      "it ends near %d grants; like the slowest or the fastest, %d or %d."),
      now, rank_word, nw(length(others)), as_of_l, min(by_now), max(by_now), mean(by_now),
      round(proj), now + min(rest), now + max(rest))
    key <- paste0("<span><i></i>", ot(cur), ", so far</span><span><i class='gp-dash'></i>projected</span>",
      "<span><i class='gp-band'></i>range, if it runs like a past Term</span>",
      "<span><i class='gp-past'></i>", ot(min(others)), "&ndash;", ot(max(others)), "</span>")
    rows <- paste0(
      sprintf("<tr class='gp-cur'><td>%s</td><td>%d</td><td>&asymp; %d projected</td></tr>", ot(cur), now, round(proj)),
      paste0(sprintf("<tr><td>%s</td><td>%d</td><td>%d</td></tr>", ot(rev(others)), rev(by_now), rev(tot)), collapse = ""))
    thead <- paste0("<tr><th>Term</th><th>By ", as_of_l, "</th><th>Whole Term</th></tr>")
  }
  data <- jsonlite::toJSON(list(terms = terms, cur = as.character(cur), today = today, end = end_day,
                                start = format(start), asOf = as_of_l, done = done), auto_unbox = TRUE)

  paste0(
    "<section class='jx gpace' id='grants' aria-labelledby='gp-h'>",
    "<style>", fill_palette(GRANT_PACE_CSS), "</style>",
    "<h2 id='gp-h'>The grant pace <span class='tag'>Order lists &amp; dockets</span></h2>",
    "<p class='gp-sum'>", summary, "</p>",
    "<div class='gp-key'>", key, "</div>",
    "<div class='gp-chart' id='gp-chart'><div class='gp-tip' id='gp-tip' hidden></div></div>",
    "<details class='gp-tbl'><summary>The counts, Term by Term</summary><table>",
    "<thead>", thead, "</thead><tbody>", rows, "</tbody></table></details>",
    "<p class='gp-note'>Cumulative cert grants, counted by the date of the order from August 1 to July 31, ",
    "so the late-September grants open a Term and the June ones close it; GVRs are not counted. ",
    "The Justices pages otherwise count the cases argued in a Term: the grants of the autumn and early winter ",
    "are argued in it, and most of those made after January in the next.",
    if (!done) " The projection adds to this Term's count what each past Term granted after the same day; it is arithmetic, not a forecast." else "",
    "</p>",
    "<script>var GRANT_PACE=", data, ";</script><script>", GRANT_PACE_JS, "</script>",
    "</section>")
}

GRANT_PACE_CSS <- "
.gpace{max-width:52rem}
.gp-sum{margin:0 0 .8rem;max-width:46rem;color:var(--ink-soft);line-height:1.5}
.gp-key{display:flex;flex-wrap:wrap;gap:.3rem 1.1rem;font-size:.84rem;color:var(--ink-soft);margin:0 0 .3rem}
.gp-key i{display:inline-block;width:1.3rem;height:0;vertical-align:.3rem;margin-right:.4rem;border-top:2.5px solid var(--accent)}
.gp-key i.gp-dash{border-top-style:dashed;border-top-width:2px}
.gp-key i.gp-band{height:.7rem;border:0;vertical-align:-.05rem;background:rgba(@accent:rgb@,.13)}
.gp-key i.gp-past{border-top:1.5px solid rgba(@faint:rgb@,.55)}
.gp-chart{position:relative}
.gp-chart svg{display:block;width:100%;height:auto;overflow:visible}
.gp-chart .gp-bandfill{fill:rgba(@accent:rgb@,.13);stroke:none}
.gp-chart svg text{font-family:'Newsreader',Georgia,serif;fill:var(--faint);font-size:12px}
.gp-tip{position:absolute;pointer-events:none;background:var(--panel);border:1px solid var(--rule);
  box-shadow:0 6px 18px rgba(@ink:rgb@,.12);padding:.5rem .65rem;font-size:.82rem;line-height:1.35;min-width:10rem;z-index:5}
.gp-tip b{display:block;margin:0 0 .3rem}
.gp-tip table{border-collapse:collapse;width:100%}
.gp-tip td{padding:0;font-variant-numeric:tabular-nums}
.gp-tip td+td{text-align:right;padding-left:.8rem}
.gp-tip tr.gp-cur td{color:var(--accent);font-weight:600}
.gp-tip .gp-r{color:var(--faint);font-size:.76rem;margin-top:.25rem}
.gp-tbl{margin:.6rem 0 0;font-size:.9rem}
.gp-tbl summary{color:var(--accent);cursor:pointer;font-style:italic}
.gp-tbl table{border-collapse:collapse;margin:.5rem 0 0;font-variant-numeric:tabular-nums}
.gp-tbl th{font-weight:600;font-size:.72rem;letter-spacing:.12em;text-transform:uppercase;color:var(--faint);text-align:right;padding:.3rem .7rem;border-bottom:1px solid var(--ink)}
.gp-tbl td{text-align:right;padding:.3rem .7rem;border-bottom:1px solid var(--rule)}
.gp-tbl th:first-child,.gp-tbl td:first-child{text-align:left;padding-left:0}
.gp-tbl tr.gp-cur td{color:var(--accent);font-weight:600}
.gp-note{font-size:.84rem;font-style:italic;color:var(--faint);margin:.7rem 0 0;max-width:46rem}
"

# Drawn in the browser so the chart can size its text to the column (an SVG
# scaled down to a phone shrinks 12px labels to 5px). Plain ES5-ish, no library.
GRANT_PACE_JS <- r"---(
(function () {
  var G = window.GRANT_PACE, box = document.getElementById('gp-chart'), tip = document.getElementById('gp-tip');
  if (!G || !box) return;
  var NS = 'http://www.w3.org/2000/svg', T = G.terms, CUR = G.cur, TODAY = G.today, END = G.end, DONE = !!G.done;
  var past = Object.keys(T).filter(function (y) { return y !== CUR; });
  var start = new Date(G.start + 'T00:00:00Z');
  function cum(y, d) { var n = 0, a = T[y]; for (var i = 0; i < a.length && a[i] <= d; i++) n++; return n; }
  var now = cum(CUR, TODAY);
  function proj(d) {
    var inc = past.map(function (y) { return cum(y, d) - cum(y, TODAY); });
    return { mean: now + inc.reduce(function (a, b) { return a + b; }, 0) / inc.length,
             lo: now + Math.min.apply(null, inc), hi: now + Math.max.apply(null, inc) };
  }
  function fmt(d) { return new Date(start.getTime() + d * 864e5).toLocaleDateString('en-US', { month: 'short', day: 'numeric', timeZone: 'UTC' }); }
  function el(n, a, p) { var e = document.createElementNS(NS, n); for (var k in a) e.setAttribute(k, a[k]); if (p) p.appendChild(e); return e; }
  var ymax = 10 * Math.ceil(Math.max.apply(null, Object.keys(T).map(function (y) { return T[y].length; }).concat(DONE ? [] : [proj(END).hi])) / 10);
  var svg;
  function draw() {
    if (svg) svg.remove();
    var W = Math.max(300, box.clientWidth), narrow = W < 560, H = Math.round(narrow ? W * 0.9 : W * 0.5);
    var M = { t: 14, r: narrow ? 46 : 60, b: 28, l: 30 };
    function X(d) { return M.l + (W - M.l - M.r) * d / END; }
    function Y(v) { return H - M.b - (H - M.t - M.b) * v / ymax; }
    svg = el('svg', { viewBox: '0 0 ' + W + ' ' + H, role: 'img',
      'aria-label': 'Cumulative cert grants by day of the Term: OT' + CUR + ' against the other Terms, OT' + past[0] + ' to OT' + past[past.length - 1] + '.' });
    box.insertBefore(svg, tip);
    for (var v = 0; v <= ymax; v += 10) {
      el('line', { x1: M.l, x2: W - M.r, y1: Y(v), y2: Y(v), stroke: 'var(--rule)', 'stroke-opacity': .6, 'stroke-width': 1 }, svg);
      if (v % 20 === 0) el('text', { x: M.l - 7, y: Y(v) + 4, 'text-anchor': 'end' }, svg).textContent = v;
    }
    ['Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul'].forEach(function (m, i) {
      var d = Math.round((Date.UTC(start.getUTCFullYear(), 7 + i, 1) - start) / 864e5);
      el('line', { x1: X(d), x2: X(d), y1: H - M.b, y2: H - M.b + 4, stroke: 'var(--rule)' }, svg);
      if (!narrow || i % 2 === 0) el('text', { x: X(d) + 2, y: H - M.b + 17 }, svg).textContent = m;
    });
    el('line', { x1: M.l, x2: W - M.r, y1: Y(0), y2: Y(0), stroke: 'var(--faint)' }, svg);
    var up = '', dn = '', mean = '';
    if (!DONE) for (var d = TODAY; d <= END; d++) { var p = proj(d), c = d === TODAY ? 'M' : 'L';
      up += c + X(d) + ',' + Y(p.hi); dn = 'L' + X(d) + ',' + Y(p.lo) + dn; mean += c + X(d) + ',' + Y(p.mean); }
    if (!DONE) el('path', { d: up + dn + 'Z', 'class': 'gp-bandfill' }, svg);
    function step(y, upTo) { var s = 'M' + X(0) + ',' + Y(0), n = 0;
      T[y].forEach(function (dd) { if (dd <= upTo) s += 'H' + X(dd) + 'V' + Y(++n); });
      return s + 'H' + X(upTo); }
    past.forEach(function (y) { el('path', { d: step(y, END), fill: 'none', stroke: 'var(--faint)', 'stroke-opacity': .55, 'stroke-width': 1.5 }, svg); });
    if (!DONE) el('path', { d: mean, fill: 'none', stroke: 'var(--accent)', 'stroke-width': 2, 'stroke-dasharray': '5 4' }, svg);
    el('path', { d: step(CUR, TODAY), fill: 'none', stroke: 'var(--accent)', 'stroke-width': 2.75 }, svg);
    el('circle', { cx: X(TODAY), cy: Y(now), r: 4.5, fill: 'var(--accent)', stroke: 'var(--paper)', 'stroke-width': 2 }, svg);
    if (!DONE) { var lab = el('text', { x: X(TODAY) + 9, y: Y(now) + 4 }, svg);
      lab.textContent = 'OT' + CUR + ' · ' + now; lab.style.fill = 'var(--accent)'; lab.style.fontWeight = 600; }
    var ends = past.map(function (y) { return { t: 'OT' + y.slice(2) + ' ' + T[y].length, y: Y(T[y].length) }; });
    if (DONE) ends.push({ t: 'OT' + CUR.slice(2) + ' ' + now, y: Y(now), acc: true });
    else { var pe = proj(END); ends.push({ t: '≈' + Math.round(pe.mean), y: Y(pe.mean), acc: true }); }
    ends.sort(function (a, b) { return a.y - b.y; });
    for (var k = 0; k < 30; k++) for (var i = 1; i < ends.length; i++) {
      var gap = ends[i].y - ends[i - 1].y; if (gap < 13) { var s = (13 - gap) / 2; ends[i].y += s; ends[i - 1].y -= s; } }
    ends.forEach(function (e) { var t = el('text', { x: X(END) + 6, y: e.y + 4 }, svg); t.textContent = e.t;
      if (e.acc) { t.style.fill = 'var(--accent)'; t.style.fontWeight = 600; } });
    if (!DONE) { el('line', { x1: X(TODAY), x2: X(TODAY), y1: M.t, y2: H - M.b, stroke: 'var(--rule)', 'stroke-dasharray': '2 3' }, svg);
      el('text', { x: X(TODAY) + 4, y: M.t + 10 }, svg).textContent = G.asOf; }
    var cross = el('line', { y1: M.t, y2: H - M.b, stroke: 'var(--faint)', visibility: 'hidden' }, svg);
    var hit = el('rect', { x: M.l, y: M.t, width: W - M.l - M.r, height: H - M.t - M.b, fill: 'transparent' }, svg);
    function move(ev) {
      var r = svg.getBoundingClientRect(), px = (ev.clientX - r.left) * W / r.width;
      var d = Math.max(0, Math.min(END, Math.round((px - M.l) / (W - M.l - M.r) * END)));
      cross.setAttribute('x1', X(d)); cross.setAttribute('x2', X(d)); cross.setAttribute('visibility', 'visible');
      var rows = past.map(function (y) { return [y, cum(y, d)]; }).sort(function (a, b) { return b[1] - a[1]; });
      var p = DONE ? null : proj(d), head = d <= TODAY ? '<tr class="gp-cur"><td>OT' + CUR + '</td><td>' + cum(CUR, d) + '</td></tr>'
        : '<tr class="gp-cur"><td>OT' + CUR + ', projected</td><td>≈ ' + Math.round(p.mean) + '</td></tr>';
      tip.innerHTML = '<b>' + fmt(d) + '</b><table>' + head + rows.map(function (r) { return '<tr><td>OT' + r[0] + '</td><td>' + r[1] + '</td></tr>'; }).join('') +
        '</table>' + (d > TODAY ? '<div class="gp-r">Projected range ' + p.lo + '–' + p.hi + '</div>' : '');
      tip.hidden = false;
      var bx = box.getBoundingClientRect(), tw = tip.offsetWidth, left = ev.clientX - bx.left + 14;
      if (left + tw > bx.width) left = ev.clientX - bx.left - tw - 14;
      tip.style.left = Math.max(0, left) + 'px'; tip.style.top = Math.max(0, ev.clientY - bx.top - 40) + 'px';
    }
    hit.addEventListener('pointermove', move); hit.addEventListener('pointerdown', move);
    hit.addEventListener('pointerleave', function () { tip.hidden = true; cross.setAttribute('visibility', 'hidden'); });
  }
  draw();
  var rt; window.addEventListener('resize', function () { clearTimeout(rt); rt = setTimeout(draw, 120); });
})();
)---"
