/* live.js -- the landing page's "Live now" panel: the Court's own live audio of
 * an oral argument, shown only while the Court is actually streaming one.
 *
 * Served at /live.js (build_dashboards.R copies it beside search.js). The page
 * carries the panel hidden (live_argument_panel(), R/page_style.R), with one
 * <ol data-day="YYYY-MM-DD"> per upcoming argument day naming that day's cases
 * in the order the Court hears them. This script does nothing at all unless
 * today, in Washington, is one of those days; then, from 9:30 a.m. to 4 p.m. ET,
 * it reads the Court's stream playlist once a minute and shows the panel while
 * the stream is live.
 *
 * "Live" is read from the playlist, never assumed from the calendar: the Court
 * starts late, recesses between cases and ends early. A first look counts the
 * stream live when its newest segment is under FRESH_S seconds old (the
 * playlist dates each segment); after that, a stream is live while its media
 * sequence keeps advancing, which does not depend on the reader's clock. A 404
 * (the backup stream returns one while unused) or a playlist that has stopped
 * moving hides the panel -- unless the reader is listening, in which case the
 * player is left alone to finish.
 *
 * The stream is the Court's (supremecourt.gov/oral_arguments/live.aspx, which
 * lists these two URLs), served by Akamai with Access-Control-Allow-Origin: *.
 * It is undocumented: if it moves, the panel simply never appears. Playback
 * uses the browser's own HLS where it has one (Safari, iOS, Android) and
 * otherwise hls.js from cdnjs, loaded only when the reader presses play.
 * See docs/live-argument.md.
 */
(function () {
  'use strict';

  var STREAMS = [
    'https://scotus_stream.akamaized.net/hls/live/2032703/oa_staging/master.m3u8',
    'https://scotus_stream.akamaized.net/hls/live/2032703-b/oa_staging/master.m3u8'
  ];
  var HLS_JS = 'https://cdnjs.cloudflare.com/ajax/libs/hls.js/1.6.16/hls.light.min.js';
  var HLS_SRI = 'sha512-yiuu1qUWP6u4jOyav6Pfnjrluji/KVkLli7eTy43Jr3wmwHrQ72c577qWk8fnIwDlQcpDJbi1lgqnOTulx3SEQ==';
  var FRESH_S = 120;               // newest segment younger than this = live (first look only)
  var POLL_MS = 60000;
  var OPEN_MIN = 9 * 60 + 30;      // 9:30 a.m. ET: the Court sits at 10
  var CLOSE_MIN = 16 * 60;         // 4 p.m. ET: no argument runs this late

  var box = document.getElementById('live-arg');
  if (!box || !window.fetch) return;

  // Today's date and the minute of the day in Washington, whatever the reader's zone.
  function etNow() {
    var p = {};
    new Intl.DateTimeFormat('en-US', { timeZone: 'America/New_York', year: 'numeric', month: '2-digit',
      day: '2-digit', hour: '2-digit', minute: '2-digit', hourCycle: 'h23' })
      .formatToParts(new Date()).forEach(function (x) { p[x.type] = x.value; });
    return { date: p.year + '-' + p.month + '-' + p.day, min: parseInt(p.hour, 10) * 60 + parseInt(p.minute, 10) };
  }

  var now = etNow();
  var list = box.querySelector('ol[data-day="' + now.date + '"]');
  if (!list || now.min >= CLOSE_MIN) return;
  list.hidden = false;

  var go = document.getElementById('live-go'), audio = document.getElementById('live-audio');
  var stat = document.getElementById('live-stat');
  var src = null, seq = {}, timer = null, hls = null;

  // "2026-10-05T11:17:59.000-0400": Date.parse wants the offset as -04:00.
  function parseDate(s) { return Date.parse(s.replace(/([+-]\d{2})(\d{2})$/, '$1:$2')); }

  function getText(url) {
    return fetch(url, { cache: 'no-store' }).then(function (r) { return r.ok ? r.text() : null; });
  }

  // One look at a stream: resolves {live, url}. A master playlist (variants)
  // is followed to its first variant, which is the one that carries segments.
  function probe(url) {
    return getText(url).then(function (t) {
      if (t && /#EXT-X-STREAM-INF/.test(t)) {
        var v = t.split(/\r?\n/).filter(function (l) { return l && l.charAt(0) !== '#'; })[0];
        return v ? getText(new URL(v, url).href) : null;
      }
      return t;
    }).then(function (t) {
      if (!t || !/#EXTM3U/.test(t)) return { live: false, url: url };
      var m = /#EXT-X-MEDIA-SEQUENCE:(\d+)/.exec(t), n = m ? +m[1] : null;
      var prev = seq[url]; seq[url] = n;
      if (/#EXT-X-ENDLIST/.test(t)) return { live: false, url: url };
      if (prev != null && n != null) return { live: n > prev, url: url };
      var d = t.match(/#EXT-X-PROGRAM-DATE-TIME:([^\r\n]+)/g);
      if (!d) return { live: false, url: url };   // undated: wait for the sequence to move
      var last = parseDate(d[d.length - 1].split(':').slice(1).join(':'));
      return { live: !isNaN(last) && (Date.now() - last) / 1000 < FRESH_S, url: url };
    }).catch(function () { return { live: false, url: url }; });
  }

  function listening() { return audio && !audio.paused && !audio.ended; }

  function check() {
    var t = etNow();
    if (t.date !== now.date || t.min >= CLOSE_MIN) { if (!listening()) box.hidden = true; return; }
    if (t.min < OPEN_MIN) { timer = setTimeout(check, Math.min(POLL_MS * 15, (OPEN_MIN - t.min) * 60000)); return; }
    probe(STREAMS[0]).then(function (a) { return a.live ? a : probe(STREAMS[1]); }).then(function (r) {
      if (r.live) {
        if (!src || !listening()) src = r.url;
        box.hidden = false;
        if (stat) stat.textContent = '';
      } else if (!listening()) {
        box.hidden = true;
      }
      timer = setTimeout(check, POLL_MS);
    });
  }

  function loadHls() {
    if (window.Hls) return Promise.resolve(window.Hls);
    return new Promise(function (ok, no) {
      var s = document.createElement('script');
      s.src = HLS_JS; s.integrity = HLS_SRI; s.crossOrigin = 'anonymous'; s.referrerPolicy = 'no-referrer';
      s.onload = function () { ok(window.Hls); }; s.onerror = no;
      document.head.appendChild(s);
    });
  }

  function start() {
    if (!src || !audio) return;
    go.disabled = true;
    if (stat) stat.textContent = 'Connecting…';
    var played = function () { go.hidden = true; audio.hidden = false; if (stat) stat.textContent = ''; };
    var failed = function () {
      go.disabled = false;
      if (stat) stat.textContent = 'The stream did not start. Try again, or listen on the Court’s site.';
    };
    // hls.js wherever the browser has Media Source (desktop browsers, iOS 17.1+
    // through ManagedMediaSource), as the Court's own player does
    // (overrideNative): Chrome 154 answers "maybe" for native HLS and then
    // would not play this stream. The browser's own HLS only where there is no
    // MSE (older iPhones), and as the fallback if hls.js fails.
    var nativeOk = !!audio.canPlayType('application/vnd.apple.mpegurl');
    function native() {
      if (!nativeOk) return Promise.reject(new Error('no native HLS'));
      if (hls) { hls.destroy(); hls = null; }
      audio.src = src;
      return audio.play();
    }
    if (!(window.MediaSource || window.ManagedMediaSource)) { native().then(played, failed); return; }
    loadHls().then(function (Hls) {
      if (!Hls || !Hls.isSupported()) throw new Error('no MSE');
      if (hls) hls.destroy();
      hls = new Hls({ liveSyncDurationCount: 3 });
      hls.on(Hls.Events.ERROR, function (e, d) { if (d && d.fatal) { hls.destroy(); hls = null; failed(); } });
      hls.loadSource(src);
      hls.attachMedia(audio);
      return audio.play();
    }).then(played).catch(function () { native().then(played, failed); });
  }

  if (go) go.addEventListener('click', start);
  check();
})();
