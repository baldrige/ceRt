/* search.js -- case search for supremecourt.report.
 *
 * Served at /search.js (build_dashboards.R copies it beside analytics.js) and
 * loaded by every page that carries the search box (the landing page, the
 * /cases/ index, the 404). The page sets window.SCR_SEARCH = {json, prefix}
 * before this loads: where the index is, and what to put before "<docket>.html"
 * in a result link, both RELATIVE TO THAT PAGE.
 *
 * The index (cases/search.json) is one object, docket -> caption, ~56,000
 * entries, fetched the first time the box takes focus. It is turned into
 * normalised tokens once, here, which costs ~150 ms and keeps the JSON as the
 * docket pages already write it.
 *
 * Matching, in layers (docs/search.md has the probes and timings):
 *   1. normalise: lower-case, strip diacritics and punctuation, "v."/"vs"/
 *      "versus" become a separator, "&" becomes "and";
 *   2. a docket-shaped query ("24-1046", "24 1046", "no. 241046", "25A312")
 *      matches dockets by their characters alone, exact first, then prefix;
 *   3. every query token must PREFIX-match some caption token, in any order
 *      ("genalo black", "gen bla", "black genalo" all find Genalo v. Black);
 *   4. a token of five letters or more that prefix-matches nothing may match
 *      a caption token within one edit (two at eight letters or more), so
 *      "Skermetti" finds Skrmetti -- run only when the exact pass came up
 *      short, and only against tokens of similar length;
 *   5. rank: exact docket, docket prefix, caption that starts with the query,
 *      all tokens in order, all tokens anywhere, then typo matches; ties go to
 *      the newest Term, which the old file-order search got backwards.
 *
 * Exposed as SCRSearch for the Node probe harness (.github/scripts/
 * search_probe.js); the DOM wiring at the bottom runs only in a browser.
 */
(function (root) {
  'use strict';

  // Combining diacritical marks, U+0300-U+036F, built from char codes so this
  // file stays pure ASCII whatever edits it.
  var COMBINING = new RegExp('[' + String.fromCharCode(0x300) + '-' + String.fromCharCode(0x36f) + ']', 'g');
  var MIDDOT = String.fromCharCode(0xb7);
  var NOISE = { et: 1, al: 1, inc: 1, llc: 1, llp: 1, ltd: 1, corp: 1, co: 1, the: 1, of: 1, a: 1, an: 1, and: 1, on: 1, for: 1, to: 1, by: 1, in: 1, re: 1, ex: 1, rel: 1, his: 1, her: 1, its: 1, their: 1, as: 1, official: 1, capacity: 1 };

  // Long forms the captions use beside the short forms readers type. Applied
  // to BOTH sides: the caption keeps its word and gains the short form as an
  // extra token, so "Dept" and "Department" both prefix-match it; "u.s.",
  // "us" and "united states" all become the token "us" and the caption keeps
  // "united states" too.
  var ALIAS = [
    [/\bunited states\b/g, 'united states us'], [/\bu\.?\s?s\.?(?=\s|$)/g, 'us'],
    [/\bdepartment\b/g, 'department dept'], [/\battorney\b/g, 'attorney atty'],
    [/\bgeneral\b/g, 'general gen'], [/\bboard\b/g, 'board bd'],
    [/\bcommissioner\b/g, 'commissioner commr'], [/\bsecretary\b/g, 'secretary sec'],
    [/\bcorporation\b/g, 'corporation corp'], [/\bcompany\b/g, 'company co'],
    [/\bassociation\b/g, 'association assn'], [/\bnational\b/g, 'national natl'],
    [/\binternational\b/g, 'international intl'], [/\bincorporated\b/g, 'incorporated inc']
  ];
  function normalise(s) {
    s = String(s).toLowerCase();
    if (s.normalize) s = s.normalize('NFD').replace(COMBINING, '');
    s = s.replace(/&/g, ' and ')
         .replace(/\b(v|vs|versus)\b\.?/g, ' ')
         .replace(/\bu\.\s?s\.(?=\s|$)/g, ' us ')
         .replace(/[^a-z0-9]+/g, ' ')
         .replace(/\s+/g, ' ').trim();
    for (var i = 0; i < ALIAS.length; i++) s = s.replace(ALIAS[i][0], ALIAS[i][1]);
    return s.replace(/\s+/g, ' ').trim();
  }
  function tokens(s) { return s ? s.split(' ') : []; }

  // Docket text as the docket pages key it, without punctuation: "24-1046" ->
  // "241046", "25A312" -> "25a312", "22O141" -> "22o141".
  function docketKey(s) { return String(s).toLowerCase().replace(/^no\.?\s*/, '').replace(/[^a-z0-9]/g, ''); }
  function looksLikeDocket(q) { return /^(no\.?\s*)?\d{2}\s*[-\s]?\s*[ao]?\s*\d*$/i.test(q.trim()); }
  function termOf(d) { var m = /^(\d{2})/.exec(d); return m ? parseInt(m[1], 10) : -1; }
  function numOf(d) { var m = /(\d+)$/.exec(d); return m ? parseInt(m[1], 10) : 0; }

  // Optimal-string-alignment distance with an early exit once `max` is passed.
  function editDistance(a, b, max) {
    var la = a.length, lb = b.length;
    if (Math.abs(la - lb) > max) return max + 1;
    var prev2 = null, prev = [], cur, i, j;
    for (j = 0; j <= lb; j++) prev[j] = j;
    for (i = 1; i <= la; i++) {
      cur = [i]; var rowMin = i;
      for (j = 1; j <= lb; j++) {
        var cost = a.charCodeAt(i - 1) === b.charCodeAt(j - 1) ? 0 : 1;
        var v = Math.min(prev[j] + 1, cur[j - 1] + 1, prev[j - 1] + cost);
        if (prev2 && i > 1 && j > 1 && a.charCodeAt(i - 1) === b.charCodeAt(j - 2) && a.charCodeAt(i - 2) === b.charCodeAt(j - 1))
          v = Math.min(v, prev2[j - 2] + 1);
        cur[j] = v; if (v < rowMin) rowMin = v;
      }
      if (rowMin > max) return max + 1;
      prev2 = prev; prev = cur;
    }
    return prev[lb];
  }

  // The index: E, one record per case, plus two lookups the typo pass needs so
  // it never scans the 56,000 captions -- T maps each distinct token to the
  // records that contain it, and L buckets the distinct tokens by first letter
  // and length, which is all a one- or two-edit neighbour can differ in.
  function build(index) {
    var E = [], keys = Object.keys(index), T = {}, L = {}, i, j, d, c;
    for (i = 0; i < keys.length; i++) {
      d = keys[i]; c = index[d];
      var n = normalise(c), t = tokens(n);
      // Bucket weight breaks a tie the way a reader expects: a paid petition
      // over an IFP one over an application. "Skrmetti" alone should reach
      // United States v. Skrmetti before the applications against the same
      // Attorney General two Terms later.
      var b = /a/i.test(d) ? -3 : /o/i.test(d) ? 1 : numOf(d) >= 5000 ? 0 : 4;
      E.push({ d: d, c: c, k: docketKey(d), n: n, t: t, j: ' ' + t.join(' ') + ' ', term: termOf(d), num: numOf(d), b: b });
      for (j = 0; j < t.length; j++) {
        var tk = t[j];
        if (!T[tk]) { T[tk] = []; var bk = tk.charAt(0) + tk.length; (L[bk] || (L[bk] = [])).push(tk); }
        T[tk].push(i);
      }
    }
    E.T = T; E.L = L;
    return E;
  }

  // Distinct caption tokens within `max` edits of `tok`, same first letter.
  function neighbours(E, tok) {
    var max = tok.length >= 8 ? 2 : 1, out = [], len, k, bucket, i;
    for (len = tok.length - max; len <= tok.length + max; len++) {
      bucket = E.L[tok.charAt(0) + len]; if (!bucket) continue;
      for (i = 0; i < bucket.length; i++) {
        k = bucket[i]; if (k === tok) continue;
        if (editDistance(tok, k, max) <= max) out.push(k);
      }
    }
    return out;
  }

  // Score one entry against a parsed query; null when it does not match.
  // The prefix pass runs on e.j, the tokens joined with a space either side,
  // so "does any token start with tok" is one indexOf(' ' + tok) per query
  // token rather than a loop over the caption's tokens -- the difference
  // between ~45 ms and ~15 ms for a one-word query over 56,000 captions.
  // `alts` holds, per query token, the tokens that satisfy it: the token itself
  // (a prefix match) and, on the typo pass, its edit-distance neighbours (a
  // whole-token match). `order` is the token indices most selective first --
  // the longest token -- so a caption that misses it is dropped after one
  // indexOf rather than three.
  function scoreText(e, alts, order, qn) {
    var pos = [], fuzzy = 0, exact = 0, i, k, at, a, tok;
    for (k = 0; k < order.length; k++) {
      i = order[k]; a = alts[i]; tok = a[0]; at = e.j.indexOf(' ' + tok);
      if (at >= 0) {
        if (e.j.charAt(at + tok.length + 1) === ' ') exact++;
      } else {
        for (var m = 1; m < a.length; m++) {
          at = e.j.indexOf(' ' + a[m] + ' ');
          if (at >= 0) { fuzzy++; break; }
        }
      }
      if (at < 0) return null;
      pos[i] = at;
    }
    var inOrder = true;
    for (i = 1; i < pos.length; i++) if (pos[i] <= pos[i - 1]) { inOrder = false; break; }
    var s = 60 + e.b;
    if (e.n.indexOf(qn) === 0) s += 15;
    if (inOrder) s += 10;
    s += 5 * exact;
    s -= 12 * fuzzy;
    return s;
  }

  function query(E, q, limit) {
    limit = limit || 40;
    var raw = String(q).trim(), out = [], i, e;
    if (!raw || !E) return out;
    var qn = normalise(raw), qt = tokens(qn);
    // Noise words count only when they are all the reader typed.
    var qk = []; for (i = 0; i < qt.length; i++) if (!NOISE[qt[i]]) qk.push(qt[i]);
    if (qk.length) qt = qk;
    var dk = looksLikeDocket(raw) ? docketKey(raw) : null;
    var alts = qt.map(function (t) { return [t]; });
    var order = qt.map(function (t, k) { return k; }).sort(function (a, b) { return qt[b].length - qt[a].length; });
    for (i = 0; i < E.length; i++) {
      e = E[i];
      if (dk) {
        if (e.k === dk) { out.push({ e: e, s: 100 }); continue; }
        if (e.k.indexOf(dk) === 0) { out.push({ e: e, s: 90 }); continue; }
      }
      if (qt.length) { var s = scoreText(e, alts, order, qn); if (s !== null) out.push({ e: e, s: s }); }
    }
    // The typo pass, only when the exact pass came up short. Neighbours are
    // found among the DISTINCT tokens (a few thousand comparisons), and only
    // the records that contain one are scored again -- never the whole index.
    if (out.length < 10 && qt.length && !dk && E.T) {
      var cand = {}, any = false;
      for (i = 0; i < qt.length; i++) {
        if (qt[i].length < 5) continue;
        var nb = neighbours(E, qt[i]);
        for (var m = 0; m < nb.length; m++) {
          alts[i].push(nb[m]); any = true;
          var recs = E.T[nb[m]]; for (var r = 0; r < recs.length; r++) cand[recs[r]] = 1;
        }
      }
      if (any) {
        var seen = {}; for (i = 0; i < out.length; i++) seen[out[i].e.d] = 1;
        for (var key in cand) {
          e = E[key]; if (seen[e.d]) continue;
          var f = scoreText(e, alts, order, qn); if (f !== null) out.push({ e: e, s: f });
        }
      }
    }
    out.sort(function (a, b) { return b.s - a.s || b.e.term - a.e.term || a.e.num - b.e.num || (a.e.d < b.e.d ? -1 : 1); });
    return out.slice(0, limit);
  }

  // Wrap each caption word that a query token prefix-matches (or, for a typo
  // match, that the scorer accepted) in <mark>. Display only; never re-parsed.
  function esc(s) { return s.replace(/[&<>]/g, function (c) { return { '&': '&amp;', '<': '&lt;', '>': '&gt;' }[c]; }); }
  function highlight(caption, qt) {
    return caption.split(/(\s+)/).map(function (w) {
      if (!w.trim()) return w;
      var n = normalise(w); if (!n) return esc(w);
      for (var i = 0; i < qt.length; i++) {
        var tok = qt[i];
        if (n.indexOf(tok) === 0 || (tok.length >= 5 && Math.abs(n.length - tok.length) <= 2 && editDistance(tok, n, tok.length >= 8 ? 2 : 1) <= (tok.length >= 8 ? 2 : 1)))
          return '<mark>' + esc(w) + '</mark>';
      }
      return esc(w);
    }).join('');
  }

  var api = { normalise: normalise, tokens: tokens, build: build, query: query, editDistance: editDistance, highlight: highlight, looksLikeDocket: looksLikeDocket };
  if (typeof module !== 'undefined' && module.exports) { module.exports = api; return; }
  root.SCRSearch = api;

  // ---- browser wiring -------------------------------------------------------
  var cfg = root.SCR_SEARCH || { json: 'cases/search.json', prefix: 'cases/' };
  var q = document.getElementById('cq'), r = document.getElementById('cres');
  if (!q || !r) return;
  var E = null, timer, active = -1, last = [];
  function load() {
    if (E) return;
    q.classList.add('loading');
    fetch(cfg.json).then(function (x) { return x.json(); }).then(function (j) {
      E = build(j); q.classList.remove('loading'); run();
    }).catch(function () { q.classList.remove('loading'); });
  }
  function render(hits, qt) {
    active = -1; last = hits;
    if (!hits.length) { r.innerHTML = "<li class='cnone'>No matching cases.</li>"; return; }
    r.innerHTML = hits.map(function (h, i) {
      var e = h.e, term = e.term >= 0 ? ' ' + MIDDOT + ' OT' + (2000 + e.term) : '';
      return "<li id='cres-" + i + "' role='option'><a href='" + cfg.prefix + e.d + ".html'>" +
        "<span class='cd'>No. " + esc(e.d) + term + "</span>" + highlight(e.c, qt) + "</a></li>";
    }).join('');
  }
  function run() {
    var s = q.value.trim();
    if (!s || !E) { r.innerHTML = ''; last = []; active = -1; q.removeAttribute('aria-activedescendant'); return; }
    var qn = normalise(s), qt = tokens(qn).filter(function (t) { return !NOISE[t]; });
    if (!qt.length) qt = tokens(qn);
    render(query(E, s, 40), qt);
  }
  function setActive(i) {
    var items = r.querySelectorAll('li[role=option]');
    if (!items.length) return;
    if (active >= 0 && items[active]) items[active].classList.remove('act');
    active = (i + items.length) % items.length;
    items[active].classList.add('act');
    q.setAttribute('aria-activedescendant', items[active].id);
    if (items[active].scrollIntoView) items[active].scrollIntoView({ block: 'nearest' });
  }
  q.addEventListener('focus', load);
  q.addEventListener('input', function () { if (!E) load(); clearTimeout(timer); timer = setTimeout(run, 90); });
  q.addEventListener('keydown', function (ev) {
    if (ev.key === 'ArrowDown') { ev.preventDefault(); setActive(active + 1); }
    else if (ev.key === 'ArrowUp') { ev.preventDefault(); setActive(active - 1); }
    else if (ev.key === 'Enter') {
      var a = active >= 0 ? r.querySelectorAll('li[role=option] a')[active] : r.querySelector('li[role=option] a');
      if (a) { ev.preventDefault(); location.href = a.getAttribute('href'); }
    }
    else if (ev.key === 'Escape') { q.value = ''; run(); }
  });
})(typeof window !== 'undefined' ? window : this);
