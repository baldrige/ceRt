// arguments/reader.js -- written by R/argument_reader.R. The transcript and
// player on an argument page: loads {dkt}.json beside the page, renders the
// turns by segment, plays the Court's recording, and follows along.
(function () {
  var main = document.getElementById('main'); if (!main) return;
  var tx = document.getElementById('rd-tx'), audio = document.getElementById('rd-audio');
  var now = document.getElementById('rd-now'), clock = document.getElementById('rd-clock');
  var prog = document.querySelector('#rd-prog div'), play = document.getElementById('rd-play');
  var icon = document.getElementById('rd-icon'), flt = document.getElementById('rd-flt');
  var turns = [], els = [], times = [], cur = -1, filterJ = null;
  function esc(s) { return String(s == null ? '' : s).replace(/[&<>"]/g, function (c) { return {'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;'}[c]; }); }
  function mmss(s) { s = Math.max(0, Math.floor(s)); var h = Math.floor(s / 3600), m = Math.floor(s % 3600 / 60), ss = ('0' + s % 60).slice(-2); return h ? h + ':' + ('0' + m).slice(-2) + ':' + ss : m + ':' + ss; }
  function who(t) {
    if (t.r === 'j') return t.sp === 'Roberts' ? 'Chief Justice Roberts' : 'Justice ' + t.sp;
    return t.sp.replace(/^GENERAL /, 'General ').replace(/^(MR|MS|MRS|MISS)\. /, function (m, a) { return a.charAt(0) + a.slice(1).toLowerCase() + '. '; })
      .replace(/([A-Z])([A-Z'-]+)$/, function (m, a, b) { return a + b.toLowerCase(); });
  }
  function side(s) { return s === 'pet' ? "<span class='pet'>for the petitioners</span>" : s === 'resp' ? "<span class='resp'>for the respondents</span>" : '<span>for neither side</span>'; }
  var aligned = false;
  function setTimes() {
    if (aligned) return;
    if (!audio || !isFinite(audio.duration) || !audio.duration) return;
    var total = turns.reduce(function (a, t) { return a + t.w; }, 0) || 1, spw = audio.duration / total, acc = 0;
    times = turns.map(function (t) { var s = acc * spw; acc += t.w; return s; });
    els.forEach(function (el, i) { var sp = el.querySelector('.who span'); if (sp) sp.textContent = '≈ ' + mmss(times[i]); });
  }
  function setFilter(j) {
    filterJ = j;
    flt.querySelectorAll('button').forEach(function (b) { b.setAttribute('aria-pressed', String((b.dataset.j || null) === j)); });
    document.querySelectorAll('.bench tbody tr').forEach(function (tr) { tr.classList.toggle('sel', tr.dataset.j === j); });
    els.forEach(function (el) { el.classList.toggle('dim', !!j && el.dataset.sp !== j); });
    if (j) { var f = els.find(function (el) { return el.dataset.sp === j; }); if (f) tx.scrollTop = f.offsetTop - tx.offsetTop - 40; }
  }
  fetch(main.dataset.json).then(function (r) { if (!r.ok) throw new Error(r.status); return r.json(); }).then(function (d) {
    turns = d.turns; var segs = {}; (d.segments || []).forEach(function (s) { segs[s.segment] = s; });
    var html = '', last = null;
    turns.forEach(function (t, i) {
      if (t.s !== last) {
        last = t.s; var s = segs[t.s];
        html += s ? "<div class='seg'><b>" + (s.rebuttal ? 'Rebuttal' : 'Argument') + ' · ' + esc(s.advocate) + '</b>' + side(s.side) + '</div>'
                  : "<div class='seg'><b>Opening</b></div>";
      }
      html += "<div class='turn" + (t.r === 'j' ? ' j' : '') + "' data-i='" + i + "' data-sp='" + esc(t.sp) + "'><div class='who'>" + esc(who(t)) +
              (audio ? '<span></span>' : '') + '</div><p>' + esc(t.x) + '</p></div>';
    });
    tx.innerHTML = html; els = Array.prototype.slice.call(tx.querySelectorAll('.turn'));
    var nj = turns.filter(function (t) { return t.r === 'j'; }).length;
    document.getElementById('rd-count').textContent = turns.length + ' turns · ' + nj + ' from the bench';
    var js = []; turns.forEach(function (t) { if (t.r === 'j' && js.indexOf(t.sp) < 0) js.push(t.sp); });
    flt.innerHTML = "<span class='fine' style='margin-right:.2rem'>Pick out</span><button data-j='' aria-pressed='true'>Everyone</button>" +
      js.map(function (n) { return "<button data-j='" + esc(n) + "' aria-pressed='false'>" + esc(n) + '</button>'; }).join('');
    flt.querySelectorAll('button').forEach(function (b) { b.addEventListener('click', function () { setFilter(b.dataset.j || null); }); });
    els.forEach(function (el) { el.addEventListener('click', function () {
      if (!audio) return; if (!times.length) setTimes(); if (!times.length) return;
      audio.currentTime = times[+el.dataset.i]; audio.play().catch(function () {}); }); });
    // Aligned to the recording (.github/scripts/align_arguments.py): each line
    // carries its own start time. Otherwise the even-rate estimate, marked ≈.
    aligned = !!(d.align && d.align.ok) && turns.every(function (t) { return typeof t.t === 'number'; });
    if (aligned) {
      times = turns.map(function (t) { return t.t; });
      els.forEach(function (el, i) { var sp = el.querySelector('.who span'); if (sp) sp.textContent = mmss(times[i]); });
      var sub = document.getElementById('rd-sub'); if (sub && audio) sub.textContent = 'The Court’s recording · each line timed to the audio';
    }
    if (audio) { if (audio.readyState >= 1) setTimes(); audio.addEventListener('loadedmetadata', setTimes); }
  }).catch(function () { tx.innerHTML = "<p class='fine'>The transcript did not load. It is on the Court’s site, linked below.</p>"; });
  document.querySelectorAll('.bench tbody tr').forEach(function (tr) { tr.addEventListener('click', function () { setFilter(filterJ === tr.dataset.j ? null : tr.dataset.j); }); });
  if (!audio) return;
  play.addEventListener('click', function () { if (audio.paused) audio.play().catch(function () {}); else audio.pause(); });
  audio.addEventListener('play', function () { icon.setAttribute('d', 'M3 1.5h3.5v13H3zM9.5 1.5H13v13H9.5z'); play.setAttribute('aria-label', 'Pause'); });
  audio.addEventListener('pause', function () { icon.setAttribute('d', 'M3 1.5v13l11-6.5z'); play.setAttribute('aria-label', 'Play'); });
  audio.addEventListener('error', function () { now.innerHTML = "The recording did not load — <a href='https://www.supremecourt.gov/oral_arguments/audio/'>listen on the Court’s site</a>"; play.disabled = true; });
  document.getElementById('rd-prog').addEventListener('click', function (e) {
    if (!audio.duration) return; var r = this.getBoundingClientRect(); audio.currentTime = audio.duration * (e.clientX - r.left) / r.width; });
  audio.addEventListener('timeupdate', function () {
    var t = audio.currentTime; clock.textContent = mmss(t);
    if (audio.duration) prog.style.width = (100 * t / audio.duration) + '%';
    if (!times.length) return;
    var i = 0; while (i + 1 < times.length && times[i + 1] <= t) i++;
    if (i === cur) return;
    if (els[cur]) els[cur].classList.remove('now');
    cur = i; var el = els[i]; if (!el) return; el.classList.add('now');
    now.textContent = who(turns[i]) + ' — ' + turns[i].x.slice(0, 90) + (turns[i].x.length > 90 ? '…' : '');
    var top = el.offsetTop - tx.offsetTop;
    if (!filterJ && (top < tx.scrollTop + 30 || top > tx.scrollTop + tx.clientHeight - 80)) tx.scrollTop = top - 60;
  });
})();

