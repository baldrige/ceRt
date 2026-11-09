/* qp.js -- search over the Questions Presented, and "similar questions".
 *
 * Served at /qp.js (R/qp_index.R copies it beside search.js) and loaded by two
 * kinds of page:
 *
 *   /questions/        the search page. It carries #qpq, and this file loads
 *                      /questions/text/<tt>.json one Term at a time, newest
 *                      first, searching what has arrived so far.
 *   /cases/<d>.html    a case page whose petition has a Question Presented. It
 *                      carries <section id='qpsim' data-d='<docket>' hidden>,
 *                      and this file fills it from /questions/near/<tt>-<b>.json
 *                      (~10 KB) or leaves it hidden.
 *
 * The data is built by R/qp_index.R; docs/qp-search.md has the formats, the
 * probes and the timings. There is no index in the usual sense: a Term is
 * 300-1,400 KB gzipped of plain text, and scanning all of it for a word takes
 * a few milliseconds, so the "index" is the text, folded once on arrival.
 *
 * Matching:
 *   1. fold both sides: lower-case, strip diacritics, everything that is not a
 *      letter or digit becomes a space. "922(g)(1)" is "922 g 1" on both.
 *   2. each run of non-space characters in the query is a term. A term that
 *      folds to several tokens ("922(g)(1)", "u.s.c.") is a phrase; so is
 *      anything in quotation marks. A phrase in quotation marks must match
 *      whole words; anything else also matches a longer word it begins.
 *   3. every term must appear. A handful of function words are ignored unless
 *      they are all the reader typed. When no question has every term and
 *      there are at least two, the questions with all but one are returned
 *      instead, flagged `partial`, and the page says so.
 *   4. rank: BM25 over the terms, plus a little for a whole-word match, for
 *      the terms standing close together and for the query appearing verbatim;
 *      ties go to the newest Term.
 *
 * Exposed as SCRQuestions for the Node probe harness (.github/scripts/
 * qp_probe.js); the DOM wiring at the bottom runs only in a browser.
 */
(function (root) {
  'use strict';

  var COMBINING = new RegExp('[' + String.fromCharCode(0x300) + '-' + String.fromCharCode(0x36f) + ']', 'g');
  var MIDDOT = String.fromCharCode(0xb7), ELL = String.fromCharCode(0x2026);
  // Curly double quotes, so a phrase pasted from a brief still reads as one.
  var CURLY = new RegExp('[' + String.fromCharCode(0x201c) + String.fromCharCode(0x201d) + ']', 'g');
  var NOISE = { the: 1, of: 1, a: 1, an: 1, to: 1, 'in': 1, and: 1, or: 1, is: 1, are: 1, by: 1, 'for': 1, on: 1, whether: 1, does: 1, 'do': 1, can: 1, may: 1, that: 1, under: 1, be: 1, it: 1, as: 1, at: 1 };

  function fold(s) {
    s = String(s).toLowerCase();
    if (s.normalize) s = s.normalize('NFD').replace(COMBINING, '');
    return s.replace(/[^a-z0-9]+/g, ' ').trim();
  }
  function esc(s) { return String(s).replace(/[&<>"']/g, function (c) { return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]; }); }
  function termOf(d) { var m = /^(\d{2})/.exec(d); return m ? 2000 + parseInt(m[1], 10) : -1; }
  function numOf(d) { var m = /(\d+)$/.exec(d); return m ? parseInt(m[1], 10) : 0; }

  // One shard row, [docket, caption, subject, granted, text], as a record. `n`
  // is the folded text with a space either side, so "a word starting with tok"
  // is indexOf(' ' + tok) and "the whole word tok" is indexOf(' ' + tok + ' ').
  function makeDoc(row) {
    return { d: row[0], c: row[1], a: row[2] || '', g: row[3] ? 1 : 0, x: row[4],
             n: ' ' + fold(row[4]) + ' ', term: termOf(row[0]), num: numOf(row[0]) };
  }

  function parse(q) {
    var re = /"([^"]*)"|(\S+)/g, m, terms = [], dropped = [], src = String(q).replace(CURLY, '"');
    while ((m = re.exec(src))) {
      var quoted = m[1] != null, f = fold(quoted ? m[1] : m[2]);
      if (!f) continue;
      var multi = f.indexOf(' ') >= 0;
      var t = { s: f, toks: f.split(' '), exact: quoted, phrase: quoted || multi,
                needle: ' ' + f + (quoted ? ' ' : '') };
      if (!quoted && !multi && NOISE[f]) dropped.push(t); else terms.push(t);
    }
    if (!terms.length) terms = dropped;
    return terms;
  }

  function passes(doc, f) {
    if (!f) return true;
    if (f.term && doc.d.slice(0, 2) !== f.term) return false;
    if (f.subject && doc.a !== f.subject) return false;
    if (f.granted && !doc.g) return false;
    return true;
  }

  // Search `docs` for the parsed `terms`. Returns {hits: [{e, s}], total,
  // partial}, best first. With no terms it is a browse: every document the filters pass,
  // newest first.
  function search(docs, terms, opts) {
    opts = opts || {};
    var f = opts.filter, i, k, e, out = [], nT = terms.length;
    if (!nT) {
      for (i = 0; i < docs.length; i++) if (passes(docs[i], f)) out.push({ e: docs[i], s: 0 });
      out.sort(byNewest);
      return { hits: out, total: out.length };
    }
    var df = [], cand = [], near = [], pool = 0, lenSum = 0;
    for (k = 0; k < nT; k++) df[k] = 0;
    for (i = 0; i < docs.length; i++) {
      e = docs[i];
      if (!passes(e, f)) continue;
      pool++; lenSum += e.n.length;
      var tf = [], whole = [], first = [], got = 0;
      for (k = 0; k < nT; k++) {
        var nd = terms[k].needle, at = e.n.indexOf(nd), c = 0, w = 0, p0 = at;
        while (at >= 0 && c < 8) {
          c++;
          if (terms[k].exact || e.n.charAt(at + nd.length) === ' ') w++;
          at = e.n.indexOf(nd, at + nd.length - (terms[k].exact ? 1 : 0));
        }
        if (c) { df[k]++; got++; }
        tf[k] = c; whole[k] = w; first[k] = p0;
      }
      if (got === nT) cand.push({ e: e, tf: tf, whole: whole, first: first });
      else if (nT > 1 && got === nT - 1 && !cand.length) near.push({ e: e, tf: tf, whole: whole, first: first });
    }
    var partial = !cand.length && near.length > 0;
    if (partial) cand = near;
    var avg = pool ? lenSum / pool : 1, K = 1.2, B = 0.6;
    var joined = ' ' + terms.map(function (t) { return t.s; }).join(' ');
    for (i = 0; i < cand.length; i++) {
      var cnd = cand[i], s = 0, lo = Infinity, hi = -1, ordered = true;
      for (k = 0; k < nT; k++) {
        if (!cnd.tf[k]) continue;
        var idf = Math.log(1 + (pool - df[k] + 0.5) / (df[k] + 0.5));
        s += idf * (cnd.tf[k] * (K + 1)) / (cnd.tf[k] + K * (1 - B + B * cnd.e.n.length / avg));
        if (cnd.whole[k]) s += 0.15 * idf;
        if (cnd.first[k] < lo) lo = cnd.first[k];
        if (cnd.first[k] > hi) hi = cnd.first[k];
        if (k && cnd.first[k] <= cnd.first[k - 1]) ordered = false;
      }
      if (nT > 1 && !partial) {
        if (hi - lo <= 90) s += ordered ? 1.0 : 0.6;
        if (cnd.e.n.indexOf(joined) >= 0) s += 2.0;
      }
      out.push({ e: cnd.e, s: s });
    }
    if (opts.sort === 'new') out.sort(byNewest);
    else out.sort(function (a, b) { return b.s - a.s || byNewest(a, b); });
    return { hits: out, total: out.length, partial: partial };
  }
  function byNewest(a, b) { return b.e.term - a.e.term || b.e.num - a.e.num; }

  // Does the displayed word `w` satisfy one of the query terms? A word's folded
  // form can be several tokens ("922(g)(1)," -> "922 g 1"), so the test is on
  // the whole folded word first and then on its tokens.
  function wordHits(w, terms) {
    var fw = fold(w); if (!fw) return false;
    var parts = fw.split(' ');
    for (var k = 0; k < terms.length; k++) {
      var t = terms[k];
      if (t.phrase) {
        if (!t.exact && t.toks.length > 1 && fw.indexOf(t.s) === 0) return true;
        for (var j = 0; j < t.toks.length; j++)
          if (t.toks[j].length >= 3 && parts.indexOf(t.toks[j]) >= 0) return true;
        if (t.exact && t.toks.length === 1 && parts.indexOf(t.s) >= 0) return true;
      } else {
        for (var p = 0; p < parts.length; p++) if (parts[p].indexOf(t.s) === 0) return true;
      }
    }
    return false;
  }

  // About `span` words of the text around the first word a term matches, as
  // HTML with the matches in <mark>. With no terms, the opening of the text.
  function snippet(text, terms, span) {
    span = span || 46;
    var words = String(text).split(/\s+/), hit = [], firstHit = -1, i;
    if (terms && terms.length)
      for (i = 0; i < words.length; i++) { hit[i] = wordHits(words[i], terms); if (hit[i] && firstHit < 0) firstHit = i; }
    var from = firstHit < 0 ? 0 : Math.max(0, firstHit - Math.floor(span / 3));
    var to = Math.min(words.length, from + span);
    if (to - from < span) from = Math.max(0, to - span);
    var out = [];
    for (i = from; i < to; i++) out.push(hit[i] ? '<mark>' + esc(words[i]) + '</mark>' : esc(words[i]));
    return (from > 0 ? ELL + ' ' : '') + out.join(' ') + (to < words.length ? ' ' + ELL : '');
  }

  // The file a case page reads its neighbours from.
  function nearFile(d, bucket) {
    var m = /^(\d{2})-(\d+)$/.exec(d);
    return m ? m[1] + '-' + Math.floor(parseInt(m[2], 10) / (bucket || 100)) + '.json' : null;
  }

  var api = { fold: fold, parse: parse, makeDoc: makeDoc, search: search, snippet: snippet, nearFile: nearFile, esc: esc };
  if (typeof module !== 'undefined' && module.exports) { module.exports = api; return; }
  root.SCRQuestions = api;

  // ---- browser wiring -------------------------------------------------------
  var cfg = root.SCR_QP || { base: '/questions/', cases: '/cases/' };
  function getJSON(u) { return fetch(u).then(function (x) { if (!x.ok) throw new Error(u + ' ' + x.status); return x.json(); }); }
  function meta(e, extra) {
    return 'No. ' + esc(e.d) + ' ' + MIDDOT + ' OT' + e.term + (e.a ? ' ' + MIDDOT + ' ' + esc(e.a) : '') +
      (e.g ? ' ' + MIDDOT + " <span class='qg'>Review granted</span>" : '') + (extra ? ' ' + MIDDOT + ' ' + extra : '');
  }

  // ---- a case page: "Similar questions" -------------------------------------
  var sim = document.getElementById('qpsim');
  if (sim) {
    var sd = sim.getAttribute('data-d'), sf = nearFile(sd);
    if (sf) getJSON(cfg.base + 'near/' + sf).then(function (j) {
      var e = j[sd]; if (!e || !e.top || !e.top.length) return;
      var shownSame = 0, html = e.top.map(function (r) {
        var same = r[3] & 2; if (same) shownSame++;
        return "<li><a href='" + cfg.cases + esc(r[0]) + ".html'>" + esc(r[1]) + "</a> <span class='qm'>No. " + esc(r[0]) +
          ' ' + MIDDOT + ' OT' + termOf(r[0]) + (r[3] & 1 ? ' ' + MIDDOT + " <span class='qg'>Review granted</span>" : '') +
          (same ? ' ' + MIDDOT + ' nearly the same words' : '') + '</span></li>';
      }).join('');
      var more = [];
      if (e.same > shownSame) more.push(e.same + ' petitions ask this in nearly the same words.');
      if (e.n > e.top.length) more.push("<a href='" + cfg.base + '?like=' + encodeURIComponent(sd) + "'>" +
        (e.k && e.k < e.n ? 'The ' + e.k + ' closest of ' + e.n : 'All ' + e.n) + ' similar questions</a>');
      more.push("<a href='" + cfg.base + "'>Search the questions presented</a>");
      sim.querySelector('ul').innerHTML = html;
      sim.querySelector('.qpsim-more').innerHTML = more.join(' ' + MIDDOT + ' ');
      sim.hidden = false;
    }).catch(function () { /* no neighbours file for this docket: stay hidden */ });
  }

  // ---- the search page -------------------------------------------------------
  var q = document.getElementById('qpq');
  if (!q) return;
  var res = document.getElementById('qpres'), stat = document.getElementById('qpstat'),
      moreBtn = document.getElementById('qpmore'), likeBox = document.getElementById('qplike'),
      selTerm = document.getElementById('qpterm'), selSubj = document.getElementById('qpsubj'),
      chkGr = document.getElementById('qpgr'), selSort = document.getElementById('qpsort');
  var META = null, shards = {}, loading = {}, docs = [], byDocket = {}, shown = 30, timer, likeOf = null, PAGE = 30;

  function allTerms() { return META ? META.terms.map(function (t) { return ('0' + t.t).slice(-2); }) : []; }
  function wanted() { return selTerm.value ? [selTerm.value] : allTerms(); }
  function loadShard(tt) {
    if (shards[tt]) return Promise.resolve();
    if (loading[tt]) return loading[tt];
    loading[tt] = getJSON(cfg.base + 'text/' + tt + '.json').then(function (rows) {
      var list = rows.map(makeDoc);
      shards[tt] = list;
      for (var i = 0; i < list.length; i++) { docs.push(list[i]); byDocket[list[i].d] = list[i]; }
    }).catch(function () { shards[tt] = []; });
    return loading[tt];
  }
  // Newest first, one at a time: the Terms a reader most likely wants are
  // searchable after the first few hundred kilobytes, and each arrival re-runs
  // the query so the list fills in rather than waiting on the oldest Term.
  function ensure(list, each) {
    var chain = Promise.resolve();
    list.forEach(function (tt) { chain = chain.then(function () { return loadShard(tt); }).then(each); });
    return chain;
  }
  function start() {
    if (META) return Promise.resolve();
    if (start.p) return start.p;
    start.p = getJSON(cfg.base + 'meta.json').then(function (m) { META = m; });
    return start.p;
  }

  function state() {
    return { q: q.value.trim(), term: selTerm.value, subject: selSubj.value, granted: chkGr.checked, sort: selSort.value };
  }
  function writeURL(s) {
    var p = [];
    if (likeOf) p.push('like=' + encodeURIComponent(likeOf));
    else {
      if (s.q) p.push('q=' + encodeURIComponent(s.q));
      if (s.term) p.push('term=' + s.term);
      if (s.subject) p.push('subject=' + encodeURIComponent(s.subject));
      if (s.granted) p.push('granted=1');
      if (s.sort) p.push('sort=' + s.sort);
    }
    if (root.history && history.replaceState) history.replaceState(null, '', location.pathname + (p.length ? '?' + p.join('&') : ''));
  }
  function readURL() {
    var out = {}, parts = location.search.replace(/^\?/, '').split('&');
    for (var i = 0; i < parts.length; i++) {
      var kv = parts[i].split('='); if (!kv[0]) continue;
      try { out[kv[0]] = decodeURIComponent((kv[1] || '').replace(/\+/g, ' ')); } catch (err) { out[kv[0]] = ''; }
    }
    return out;
  }

  function item(h, terms, extra) {
    var e = h.e;
    return "<li><a class='qt' href='" + cfg.cases + esc(e.d) + ".html'>" + esc(e.c) + '</a>' +
      "<div class='qm'>" + meta(e, extra) + '</div>' +
      "<p class='qx'>" + snippet(e.x, terms) + '</p></li>';
  }
  function span(list) {
    var ts = list.map(function (tt) { return 2000 + parseInt(tt, 10); }).sort();
    return !ts.length ? '' : ts[0] === ts[ts.length - 1] ? 'OT' + ts[0] : 'OT' + ts[0] + ' to OT' + ts[ts.length - 1];
  }
  function run() {
    if (likeOf) return;
    var s = state(), terms = parse(s.q), want = wanted();
    var filtered = s.subject || s.granted;
    writeURL(s);
    if (!terms.length && !filtered) {
      res.innerHTML = ''; moreBtn.hidden = true;
      stat.textContent = s.q ? 'Add a word that is not just a connective.' : '';
      return;
    }
    var have = want.filter(function (tt) { return shards[tt]; }), pending = want.length - have.length;
    var r = search(docs, terms, { filter: { term: s.term, subject: s.subject, granted: s.granted }, sort: s.sort });
    var n = r.total, msg = n.toLocaleString() + (n === 1 ? ' question' : ' questions') +
      (terms.length ? (n === 1 ? ' matches' : ' match') : '') + (have.length ? ' in ' + span(have) : '');
    if (r.partial) msg = 'No question has every one of those words' + (have.length ? ' in ' + span(have) : '') +
      '. ' + n.toLocaleString() + (n === 1 ? ' has' : ' have') + ' all but one';
    if (pending) msg += ' ' + MIDDOT + ' still loading ' + pending + (pending === 1 ? ' Term' : ' Terms') + ELL;
    stat.textContent = msg;
    res.innerHTML = r.hits.slice(0, shown).map(function (h) { return item(h, terms); }).join('');
    moreBtn.hidden = n <= shown;
    if (!moreBtn.hidden) moreBtn.textContent = 'Show ' + Math.min(PAGE, n - shown) + ' more';
  }
  function go() { shown = PAGE; start().then(function () { run(); return ensure(wanted(), run); }); }

  // "Questions similar to No. X": the stored neighbours of one petition. Only
  // the Terms those neighbours sit in are loaded.
  function like(d) {
    likeOf = d; writeURL({});
    var tt = d.slice(0, 2);
    stat.textContent = 'Loading' + ELL;
    start().then(function () { return getJSON(cfg.base + 'like/' + tt + '.json'); }).then(function (j) {
      var nb = j[d] || [], need = {}; need[tt] = 1;
      nb.forEach(function (r) { need[r[0].slice(0, 2)] = 1; });
      return ensure(Object.keys(need).sort().reverse(), function () {}).then(function () {
        var src = byDocket[d], same = (META && META.same) || 75;
        likeBox.innerHTML = '<p>Questions worded like the one in <a href="' + cfg.cases + esc(d) + '.html">' +
          esc(src ? src.c : 'No. ' + d) + '</a>' + (src ? ', No. ' + esc(d) : '') + ":</p>" +
          (src ? "<p class='qx'>" + snippet(src.x, null, 70) + '</p>' : '') +
          "<p><a href='" + cfg.base + "'>Back to the search</a></p>";
        likeBox.hidden = false;
        var hits = nb.filter(function (r) { return byDocket[r[0]]; });
        stat.textContent = hits.length ? hits.length + (hits.length === 1 ? ' similar question' : ' similar questions') + ', closest first'
          : 'No petition here asks a question in similar words.';
        res.innerHTML = hits.map(function (r) {
          return item({ e: byDocket[r[0]] }, null, r[1] >= same ? 'nearly the same words' : '');
        }).join('');
        moreBtn.hidden = true;
      });
    }).catch(function () { stat.textContent = 'Could not load the similar questions for No. ' + d + '.'; });
  }

  var init = readURL();
  if (init.like && /^\d{2}-\d+$/.test(init.like)) { like(init.like); }
  else {
    if (init.q) q.value = init.q;
    if (init.term) selTerm.value = init.term;
    if (init.subject) selSubj.value = init.subject;
    if (init.granted) chkGr.checked = true;
    if (init.sort) selSort.value = init.sort;
    if (init.q || init.subject || init.granted) go();
  }
  function changed() { if (likeOf) { likeOf = null; likeBox.hidden = true; } clearTimeout(timer); timer = setTimeout(go, 140); }
  q.addEventListener('focus', function () { start().then(function () { ensure(wanted(), function () {}); }); });
  q.addEventListener('input', changed);
  [selTerm, selSubj, chkGr, selSort].forEach(function (el) { el.addEventListener('change', changed); });
  moreBtn.addEventListener('click', function () { shown += PAGE; run(); });
})(typeof window !== 'undefined' ? window : this);
