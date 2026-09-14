/* justices_network.js -- the agreement network on each Term's Justices page.
 *
 * Served at /justices/network.js (render_justices() copies it beside the
 * pages). The page carries the Term's data in <script type="application/json"
 * id="jx-net">: the Justices in seniority order, the two agreement matrices
 * (same side of the judgment; joined the same opinion in full), the pair
 * denominators, each Justice's share of decisions in the majority, and for
 * each a portrait URL with its detected face geometry (data/portraits/
 * crops.json). The measure radios (#mx-judg / #mx-full) already drive the
 * CSS-toggled matrix tables; this script listens to the same radios so the
 * picture follows them.
 *
 * Layout: a spring model over all 36 pairs -- rest length grows with
 * disagreement, plus a repulsion that keeps a tight bloc legible -- seeded on
 * a circle in seniority order and iterated to rest, so the same matrix always
 * draws the same picture. Circle radius is the share in the majority (60% to
 * 100% mapped onto 14 to 24px). The portrait is scaled so the face spans 85%
 * of the circle and shifted so the nose sits at its centre. Labels go on the
 * far side of the node from the picture's centre; a collision pass nudges any
 * label that would sit on a circle or another label further out, and a label
 * that had to travel gets a leader line back to its circle. The threshold
 * slider starts at the measure's floor -- the weakest pair, rounded down --
 * so every connection is drawn by default and the slider only hides.
 */
(function () {
  'use strict';
  var el = document.getElementById('jx-net'), svg = document.getElementById('jx-svg'), stage = document.getElementById('jx-stage');
  if (!el || !svg || !stage) return;
  var D = JSON.parse(el.textContent);
  var K = D.short.length, W = 860, H = 560, CX = W / 2, CY = H / 2;
  var tip = document.getElementById('jx-tip'), slider = document.getElementById('jx-thr'), thrv = document.getElementById('jx-thrv');
  var measure = document.getElementById('mx-full') && document.getElementById('mx-full').checked ? 'full' : 'judg';
  var thr = 0.5, pos = { judg: null, full: null };
  var ACC = D.accent_rgb || '156,14,58';

  function pct(v) { return Math.round(100 * v) + '%'; }
  function floorOf(M) { var m = 1; for (var a = 0; a < K; a++) for (var b = 0; b < K; b++) if (a !== b && M[a][b] != null && M[a][b] < m) m = M[a][b]; return Math.floor(100 * m); }

  function force(M) {
    var p = [], i, j, it;
    for (i = 0; i < K; i++) { var a0 = -Math.PI / 2 + 2 * Math.PI * i / K; p.push({ x: CX + 200 * Math.cos(a0), y: CY + 200 * Math.sin(a0) }); }
    for (it = 0; it < 900; it++) {
      var step = 0.02 + 0.08 * (1 - it / 900);
      for (i = 0; i < K; i++) { p[i].fx = 0; p[i].fy = 0; }
      for (i = 0; i < K; i++) for (j = i + 1; j < K; j++) {
        var dx = p[j].x - p[i].x, dy = p[j].y - p[i].y, d = Math.sqrt(dx * dx + dy * dy) || 1;
        var a = M[i][j] == null ? 0.5 : M[i][j];
        var rest = 120 + 440 * Math.pow(1 - a, 1.4);
        var f = (d - rest) * 0.9, rep = -30000 / (d * d);
        var fx = (f + rep) * dx / d, fy = (f + rep) * dy / d;
        p[i].fx += fx; p[i].fy += fy; p[j].fx -= fx; p[j].fy -= fy;
      }
      for (i = 0; i < K; i++) { p[i].x += p[i].fx * step; p[i].y += p[i].fy * step; }
    }
    var mx = 0, my = 0; for (i = 0; i < K; i++) { mx += p[i].x; my += p[i].y; } mx /= K; my /= K;
    var r = 0; for (i = 0; i < K; i++) r = Math.max(r, Math.hypot(p[i].x - mx, p[i].y - my));
    var s = Math.min((W / 2 - 110) / r, (H / 2 - 70) / r, 1.6);
    return p.map(function (q) { return { x: CX + (q.x - mx) * s, y: CY + (q.y - my) * s }; });
  }

  function draw() {
    var M = D[measure];
    var p = pos[measure] || (pos[measure] = force(M));
    var out = [], i, j;
    var edges = [];
    for (i = 0; i < K; i++) for (j = i + 1; j < K; j++) { var a = M[i][j]; if (a != null && a >= thr) edges.push({ i: i, j: j, a: a }); }
    edges.sort(function (x, y) { return x.a - y.a; });
    var lo = thr, hi = 0.95;
    edges.forEach(function (e) {
      var t = Math.max(0, Math.min(1, (e.a - lo) / (hi - lo)));
      out.push("<line class='jx-edge' data-i='" + e.i + "' data-j='" + e.j + "' x1='" + p[e.i].x.toFixed(1) + "' y1='" + p[e.i].y.toFixed(1) + "' x2='" + p[e.j].x.toFixed(1) + "' y2='" + p[e.j].y.toFixed(1) + "' stroke-width='" + (1.5 + 6.5 * t).toFixed(1) + "' style='opacity:" + (0.18 + 0.72 * t).toFixed(2) + "'><title>" + D.short[e.i] + " and " + D.short[e.j] + ": " + pct(e.a) + " of " + D.n[e.i][e.j] + " decisions</title></line>");
    });
    var R = [], L = [];
    for (i = 0; i < K; i++) R.push(14.4 + 9.6 * Math.max(0, ((D.in_maj[i] == null ? 0.6 : D.in_maj[i]) - 0.6) / 0.4));
    for (i = 0; i < K; i++) {
      var ang = Math.atan2(p[i].y - CY, p[i].x - CX);
      var w = Math.max(7.6 * D.short[i].length, 7.0 * (pct(D.in_maj[i] || 0).length + 12));
      L.push({ i: i, ang: ang, dist: R[i] + 8, w: w, h: 30 });
    }
    function box(lab) {
      var cx = p[lab.i].x + Math.cos(lab.ang) * lab.dist, cy = p[lab.i].y + Math.sin(lab.ang) * lab.dist, c = Math.cos(lab.ang), s = Math.sin(lab.ang);
      return { x: c > 0.35 ? cx : c < -0.35 ? cx - lab.w : cx - lab.w / 2, y: s > 0.35 ? cy : s < -0.35 ? cy - lab.h : cy - lab.h / 2, w: lab.w, h: lab.h, anchor: c > 0.35 ? 'start' : c < -0.35 ? 'end' : 'middle' };
    }
    function hits(a, b) { return !(a.x + a.w < b.x || b.x + b.w < a.x || a.y + a.h < b.y || b.y + b.h < a.y); }
    function hitsCircle(a, k) { var cx = Math.max(a.x, Math.min(p[k].x, a.x + a.w)), cy = Math.max(a.y, Math.min(p[k].y, a.y + a.h)); return Math.hypot(cx - p[k].x, cy - p[k].y) < R[k] + 3; }
    for (var pass = 0; pass < 12; pass++) {
      var moved = false;
      for (i = 0; i < K; i++) {
        var bi = box(L[i]), bad = false;
        for (j = 0; j < K; j++) if (j !== i && hitsCircle(bi, j)) { bad = true; break; }
        if (!bad) for (j = 0; j < i; j++) if (hits(bi, box(L[j]))) { bad = true; break; }
        if (bad) { L[i].dist += 9; moved = true; }
      }
      if (!moved) break;
    }
    var defs = "<defs>" + D.short.map(function (s, k) { return "<clipPath id='jxclip" + k + "'><circle r='" + (R[k] - 1.5).toFixed(1) + "'/></clipPath>"; }).join('') + "</defs>";
    for (i = 0; i < K; i++) {
      var b = box(L[i]), r = R[i], pr = D.portraits[D.short[i]];
      var tx = b.anchor === 'start' ? b.x : b.anchor === 'end' ? b.x + b.w : b.x + b.w / 2;
      var leader = '';
      if (L[i].dist > r + 22) {
        var ex = Math.cos(L[i].ang), ey = Math.sin(L[i].ang);
        leader = "<line class='jx-leader' x1='" + (ex * (r + 3)).toFixed(1) + "' y1='" + (ey * (r + 3)).toFixed(1) + "' x2='" + (ex * (L[i].dist - 4)).toFixed(1) + "' y2='" + (ey * (L[i].dist - 4)).toFixed(1) + "'/>";
      }
      var img = '';
      if (pr && pr.url && pr.fw) { var s = (0.85 * 2 * r) / pr.fw; img = "<image href='" + pr.url + "' x='" + (-pr.nx * s).toFixed(1) + "' y='" + (-pr.ny * s).toFixed(1) + "' width='" + (pr.iw * s).toFixed(1) + "' height='" + (pr.ih * s).toFixed(1) + "' preserveAspectRatio='none' clip-path='url(#jxclip" + i + ")'/>"; }
      out.push("<g class='jx-node' data-i='" + i + "' transform='translate(" + p[i].x.toFixed(1) + "," + p[i].y.toFixed(1) + ")'>" + leader +
        "<circle r='" + r.toFixed(1) + "'><title>" + D.names[i] + ": in the majority in " + pct(D.in_maj[i] || 0) + " of decisions</title></circle>" + img +
        "<circle class='ring' r='" + r.toFixed(1) + "'/>" +
        "<text text-anchor='" + b.anchor + "' x='" + (tx - p[i].x).toFixed(1) + "' y='" + (b.y + 13 - p[i].y).toFixed(1) + "'>" + D.short[i] + "</text>" +
        "<text class='sub' text-anchor='" + b.anchor + "' x='" + (tx - p[i].x).toFixed(1) + "' y='" + (b.y + 26 - p[i].y).toFixed(1) + "'>" + pct(D.in_maj[i] || 0) + " in majority</text></g>");
    }
    svg.innerHTML = defs + out.join('');
    bind();
  }

  function showTip(ev, text) { var r = stage.getBoundingClientRect(); tip.textContent = text; tip.style.left = (ev.clientX - r.left) + 'px'; tip.style.top = (ev.clientY - r.top) + 'px'; tip.classList.add('on'); }
  function bind() {
    var nodes = svg.querySelectorAll('.jx-node'), edges = svg.querySelectorAll('.jx-edge');
    function clear() { nodes.forEach(function (m) { m.classList.remove('dim', 'hot'); }); edges.forEach(function (e) { e.classList.remove('dim', 'hot'); }); tip.classList.remove('on'); }
    nodes.forEach(function (n) {
      var i = +n.getAttribute('data-i');
      n.addEventListener('mouseenter', function (ev) {
        nodes.forEach(function (m) { m.classList.add('dim'); }); n.classList.remove('dim'); n.classList.add('hot');
        edges.forEach(function (e) { var a = +e.getAttribute('data-i'), b = +e.getAttribute('data-j'); if (a === i || b === i) { e.classList.add('hot'); nodes[a === i ? b : a].classList.remove('dim'); } else e.classList.add('dim'); });
        var M = D[measure], best = [], k; for (k = 0; k < K; k++) if (k !== i && M[i][k] != null) best.push([M[i][k], k]); best.sort(function (x, y) { return y[0] - x[0]; });
        if (best.length >= 2) showTip(ev, D.names[i] + ' · closest: ' + D.short[best[0][1]] + ' ' + pct(best[0][0]) + ', ' + D.short[best[1][1]] + ' ' + pct(best[1][0]) + ' · farthest: ' + D.short[best[best.length - 1][1]] + ' ' + pct(best[best.length - 1][0]));
      });
      n.addEventListener('mouseleave', clear);
    });
    edges.forEach(function (e) {
      e.addEventListener('mouseenter', function (ev) { var a = +e.getAttribute('data-i'), b = +e.getAttribute('data-j'); edges.forEach(function (x) { x.classList.add('dim'); }); e.classList.add('hot'); showTip(ev, D.short[a] + ' and ' + D.short[b] + ': ' + pct(D[measure][a][b]) + ' of ' + D.n[a][b] + ' decisions'); });
      e.addEventListener('mouseleave', clear);
    });
  }
  stage.addEventListener('mousemove', function (ev) { if (tip.classList.contains('on')) { var r = stage.getBoundingClientRect(); tip.style.left = (ev.clientX - r.left) + 'px'; tip.style.top = (ev.clientY - r.top) + 'px'; } });

  function setFloor() { var f = floorOf(D[measure]); slider.min = f; slider.value = f; thr = f / 100; thrv.textContent = f; }
  document.querySelectorAll('input[name=mx]').forEach(function (r) { r.addEventListener('change', function () { measure = this.id === 'mx-full' ? 'full' : 'judg'; setFloor(); draw(); }); });
  slider.addEventListener('input', function () { thr = +this.value / 100; thrv.textContent = this.value; draw(); });
  setFloor(); draw();
})();
