# The grant pace: cert grants made so far in the current Term's window, day by
# day, against every complete past Term, with the rest of the Term projected
# from what those Terms granted after the same day. Shown above the table on the
# current Term's Oral Argument Navigator page (R/argument_nav.R).
#
# A grant is classify_petitions()'s "granted" (cert, cert before judgment,
# probable jurisdiction noted; never a GVR), unioned with the CERTIORARI GRANTED
# entries of the Court's order lists (orders/orders.json, R/orders_list.R). Each
# fills the other's gaps: the order lists are fresher than any docket fetch, and
# the dockets hold the grants of the few order documents the site never read
# (the first-Monday list of 3 Oct 2022 among them). A docket counts once, on its
# earliest grant date.
#
# The window is 1 August to 31 July, so the long conference's late-September
# grants open a Term and the June clean-up grants close it. That is NOT the
# Navigator's own unit -- the cases argued in a Term -- and the panel says so.
#
# The projection is arithmetic, not a model: today's count plus what each past
# Term granted between this day of its window and the end; the dashed line is
# their mean and the band runs from the slowest to the fastest.

GRANT_PACE_FIRST <- 2018L   # the first window the site's data covers whole
NUM_WORDS <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
               "eleven", "twelve")

# Window year of a date: the Term whose August-to-July window holds it.
grant_window <- function(d) as.integer(ifelse(as.integer(format(d, "%m")) >= 8L,
                                              as.integer(format(d, "%Y")),
                                              as.integer(format(d, "%Y")) - 1L))

# One row per granted docket: dkt, date. `cases` is a docket frame for
# classify_petitions(); `orders_json` the path to orders/orders.json (optional).
grant_dates <- function(cases, orders_json = NULL) {
  g <- classify_petitions(cases)
  g <- g[g$outcome %in% "granted" & !is.na(g$outcome_date), c("dkt", "outcome_date")]
  g <- data.frame(dkt = g$dkt, date = as.Date(g$outcome_date), stringsAsFactors = FALSE)
  if (!is.null(orders_json) && file.exists(orders_json)) {
    man <- tryCatch(jsonlite::fromJSON(orders_json, simplifyVector = FALSE), error = function(e) NULL)
    o <- do.call(rbind, lapply(man, function(v) if (length(v$granted))
      data.frame(dkt = vapply(v$granted, function(x) as.character(x$dkt), ""),
                 date = as.Date(v$date), stringsAsFactors = FALSE)))
    if (!is.null(o)) g <- rbind(g, o)
  }
  g <- g[order(g$date), ]
  g[!duplicated(g$dkt), ]
}

# The panel's HTML for the window that holds `as_of`, or "" when there is no
# past Term to compare with or nothing granted yet.
grant_pace_panel <- function(grants, as_of = Sys.Date()) {
  cur <- grant_window(as_of)
  start <- as.Date(sprintf("%d-08-01", cur))
  end_day <- 364L                                    # 31 July (30 July in a leap window)
  today <- as.integer(as_of - start)
  w <- grant_window(grants$date)
  day <- as.integer(grants$date - as.Date(sprintf("%d-08-01", w)))
  past <- seq.int(GRANT_PACE_FIRST, cur - 1L)
  keep <- w %in% c(past, cur) & day <= end_day & !(w == cur & grants$date > as_of)
  if (!length(past) || !any(w[keep] == cur)) return("")
  terms <- lapply(setNames(c(past, cur), c(past, cur)), function(y) sort(day[keep & w == y]))

  cum <- function(y, d) sum(terms[[as.character(y)]] <= d)
  now <- cum(cur, today)
  by_now <- vapply(past, cum, 1L, d = today)
  tot <- vapply(past, function(y) length(terms[[as.character(y)]]), 1L)
  rest <- tot - by_now
  proj <- now + mean(rest)
  ot <- function(y) paste0("OT", y)
  rank_word <- if (now < min(by_now)) "fewer than in any of the last" else
               if (now > max(by_now)) "more than in any of the last" else "within the range of the last"
  as_of_l <- paste(format(as_of, "%B"), as.integer(format(as_of, "%d")))

  summary <- sprintf(paste0(
    "The Court has granted %d petitions since the window opened on August 1: %s %s Terms by %s, ",
    "which granted between %d and %d (%.1f on average). If the rest of the Term runs like an average past one, ",
    "it ends near %d grants; like the slowest or the fastest, %d or %d."),
    now, rank_word, if (length(past) <= length(NUM_WORDS)) NUM_WORDS[length(past)] else length(past), as_of_l, min(by_now), max(by_now), mean(by_now),
    round(proj), now + min(rest), now + max(rest))

  rows <- paste0(
    sprintf("<tr class='gp-cur'><td>%s</td><td>%d</td><td>&asymp; %d projected</td></tr>", ot(cur), now, round(proj)),
    paste0(sprintf("<tr><td>%s</td><td>%d</td><td>%d</td></tr>", ot(rev(past)), rev(by_now), rev(tot)), collapse = ""))
  data <- jsonlite::toJSON(list(terms = terms, cur = as.character(cur), today = today, end = end_day,
                                start = format(start), asOf = as_of_l), auto_unbox = TRUE)

  paste0(
    "<section class='gpace' aria-labelledby='gp-h'>",
    "<style>", fill_palette(GRANT_PACE_CSS), "</style>",
    "<h2 id='gp-h'>The grant pace</h2>",
    "<p class='gp-sum'>", summary, "</p>",
    "<div class='gp-key'><span><i></i>", ot(cur), ", so far</span><span><i class='gp-dash'></i>projected</span>",
    "<span><i class='gp-band'></i>range, if it runs like a past Term</span>",
    "<span><i class='gp-past'></i>", ot(min(past)), "&ndash;", ot(max(past)), "</span></div>",
    "<div class='gp-chart' id='gp-chart'><div class='gp-tip' id='gp-tip' hidden></div></div>",
    "<details class='gp-tbl'><summary>The counts, Term by Term</summary><table>",
    "<thead><tr><th>Term</th><th>By ", as_of_l, "</th><th>Whole Term</th></tr></thead><tbody>", rows,
    "</tbody></table></details>",
    "<p class='gp-note'>Cumulative cert grants, counted by the date of the order from August 1 to July 31, ",
    "so the late-September grants open a Term and the June ones close it. That is not the table's unit ",
    "below, which is the cases argued in the Term. GVRs are not counted. The projection adds to this Term's ",
    "count what each past Term granted after the same day; it is arithmetic, not a forecast.</p>",
    "<script>var GRANT_PACE=", data, ";</script><script>", GRANT_PACE_JS, "</script>",
    "</section>")
}

GRANT_PACE_CSS <- "
.gpace{margin:0 0 2.2rem;max-width:52rem}
.gpace h2{font:600 .78rem/1 'Newsreader',Georgia,serif;letter-spacing:.22em;text-transform:uppercase;color:var(--accent);margin:0 0 .5rem}
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
  var NS = 'http://www.w3.org/2000/svg', T = G.terms, CUR = G.cur, TODAY = G.today, END = G.end;
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
  var ymax = 10 * Math.ceil(Math.max.apply(null, past.map(function (y) { return T[y].length; }).concat([proj(END).hi])) / 10);
  var svg;
  function draw() {
    if (svg) svg.remove();
    var W = Math.max(300, box.clientWidth), narrow = W < 560, H = Math.round(narrow ? W * 0.9 : W * 0.5);
    var M = { t: 14, r: narrow ? 46 : 60, b: 28, l: 30 };
    function X(d) { return M.l + (W - M.l - M.r) * d / END; }
    function Y(v) { return H - M.b - (H - M.t - M.b) * v / ymax; }
    svg = el('svg', { viewBox: '0 0 ' + W + ' ' + H, role: 'img',
      'aria-label': 'Cumulative cert grants by day of the Term: OT' + CUR + ' against OT' + past[0] + ' to OT' + past[past.length - 1] + '.' });
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
    for (var d = TODAY; d <= END; d++) { var p = proj(d), c = d === TODAY ? 'M' : 'L';
      up += c + X(d) + ',' + Y(p.hi); dn = 'L' + X(d) + ',' + Y(p.lo) + dn; mean += c + X(d) + ',' + Y(p.mean); }
    el('path', { d: up + dn + 'Z', 'class': 'gp-bandfill' }, svg);
    function step(y, upTo) { var s = 'M' + X(0) + ',' + Y(0), n = 0;
      T[y].forEach(function (dd) { if (dd <= upTo) s += 'H' + X(dd) + 'V' + Y(++n); });
      return s + 'H' + X(upTo); }
    past.forEach(function (y) { el('path', { d: step(y, END), fill: 'none', stroke: 'var(--faint)', 'stroke-opacity': .55, 'stroke-width': 1.5 }, svg); });
    el('path', { d: mean, fill: 'none', stroke: 'var(--accent)', 'stroke-width': 2, 'stroke-dasharray': '5 4' }, svg);
    el('path', { d: step(CUR, TODAY), fill: 'none', stroke: 'var(--accent)', 'stroke-width': 2.75 }, svg);
    el('circle', { cx: X(TODAY), cy: Y(now), r: 4.5, fill: 'var(--accent)', stroke: 'var(--paper)', 'stroke-width': 2 }, svg);
    var lab = el('text', { x: X(TODAY) + 9, y: Y(now) + 4 }, svg);
    lab.textContent = 'OT' + CUR + ' · ' + now; lab.style.fill = 'var(--accent)'; lab.style.fontWeight = 600;
    var ends = past.map(function (y) { return { t: 'OT' + y.slice(2) + ' ' + T[y].length, y: Y(T[y].length) }; });
    var pe = proj(END); ends.push({ t: '≈' + Math.round(pe.mean), y: Y(pe.mean), acc: true });
    ends.sort(function (a, b) { return a.y - b.y; });
    for (var k = 0; k < 30; k++) for (var i = 1; i < ends.length; i++) {
      var gap = ends[i].y - ends[i - 1].y; if (gap < 13) { var s = (13 - gap) / 2; ends[i].y += s; ends[i - 1].y -= s; } }
    ends.forEach(function (e) { var t = el('text', { x: X(END) + 6, y: e.y + 4 }, svg); t.textContent = e.t;
      if (e.acc) { t.style.fill = 'var(--accent)'; t.style.fontWeight = 600; } });
    el('line', { x1: X(TODAY), x2: X(TODAY), y1: M.t, y2: H - M.b, stroke: 'var(--rule)', 'stroke-dasharray': '2 3' }, svg);
    el('text', { x: X(TODAY) + 4, y: M.t + 10 }, svg).textContent = G.asOf;
    var cross = el('line', { y1: M.t, y2: H - M.b, stroke: 'var(--faint)', visibility: 'hidden' }, svg);
    var hit = el('rect', { x: M.l, y: M.t, width: W - M.l - M.r, height: H - M.t - M.b, fill: 'transparent' }, svg);
    function move(ev) {
      var r = svg.getBoundingClientRect(), px = (ev.clientX - r.left) * W / r.width;
      var d = Math.max(0, Math.min(END, Math.round((px - M.l) / (W - M.l - M.r) * END)));
      cross.setAttribute('x1', X(d)); cross.setAttribute('x2', X(d)); cross.setAttribute('visibility', 'visible');
      var rows = past.map(function (y) { return [y, cum(y, d)]; }).sort(function (a, b) { return b[1] - a[1]; });
      var p = proj(d), head = d <= TODAY ? '<tr class="gp-cur"><td>OT' + CUR + '</td><td>' + cum(CUR, d) + '</td></tr>'
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
