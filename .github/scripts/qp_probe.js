/* qp_probe.js -- probes for the Questions Presented search and the similar-
 * question lists, run against a built index:
 *
 *   node .github/scripts/qp_probe.js site/questions
 *
 * Each search probe names a query and a case it must find within `top`
 * results; each neighbour probe names two petitions that ask the same question
 * and must appear in each other's stored list. A miss fails (exit 1); a query
 * slower than 80 ms warns. docs/qp-search.md records a run.
 *
 * With --explore "<query>" it prints the top ten for that query instead.
 */
'use strict';
var fs = require('fs'), path = require('path');
var Q = require(path.join(__dirname, '..', '..', 'qp.js'));

var dir = process.argv[2];
if (!dir) { console.error('usage: node qp_probe.js <site>/questions [--explore "<query>"]'); process.exit(2); }
var read = function (f) { return JSON.parse(fs.readFileSync(path.join(dir, f), 'utf8')); };
var meta = read('meta.json'), docs = [], like = {}, t0 = Date.now();
meta.terms.forEach(function (t) {
  var tt = ('0' + t.t).slice(-2);
  read('text/' + tt + '.json').forEach(function (r) { docs.push(Q.makeDoc(r)); });
  var l = read('like/' + tt + '.json'); for (var d in l) like[d] = l[d];
});
var byDocket = {}; docs.forEach(function (e) { byDocket[e.d] = e; });
console.log(docs.length + ' questions, ' + meta.terms.length + ' Terms, folded in ' + (Date.now() - t0) + ' ms');

var ex = process.argv.indexOf('--explore');
if (ex > 0) {
  var r = Q.search(docs, Q.parse(process.argv[ex + 1]), {});
  console.log(r.total + ' match');
  r.hits.slice(0, 10).forEach(function (h) { console.log('  ' + h.s.toFixed(2) + '  ' + h.e.d + '  ' + h.e.c.slice(0, 60)); });
  process.exit(0);
}

// query, a docket it must find, within how many results (0 = at any rank),
// filter (optional). A null docket means the query must match nothing.
var SEARCH = [
  ['chevron overrule', '22-451', 10],                          // Loper Bright
  ['chevron overrule', '22-1219', 10],                         // Relentless
  ['chevron deference', '22-451', 0, { granted: true }],
  ['922(g)(1) bruen', '25-5519', 0],                           // a boilerplate question: found, however deep
  ['almendarez-torres', '25-6007', 0],
  ['Almendarez Torres overrule', '25-6007', 0],
  ['mcgirt overruled', '21-429', 0],                           // Oklahoma v. Castro-Huerta
  ['"habitual residence" hague', '18-935', 5],                 // Monasky v. Taglieri
  ['rooker-feldman', '25-197', 0],
  ['Rooker Feldman', '25-197', 0],
  ['transgender athletes', '24-38', 3],                        // Little v. Hecox
  ['section 230', '21-1333', 3, { granted: true }],            // Gonzalez v. Google
  ['"fair use" copyright', '21-869', 10],                      // Warhol v. Goldsmith
  ['race admissions harvard', '20-1199', 3],                   // SFFA v. Harvard
  ['bump stock machinegun', '22-976', 5],                      // Garland v. Cargill
  ['mifepristone', '23-235', 5],                               // FDA v. Alliance
  ['presidential immunity criminal prosecution', '23-939', 3], // Trump v. United States
  ['1983 malicious prosecution', '23-50', 5],                  // Chiaverini v. City of Napoleon
  ['dormant commerce clause', '21-468', 0, { term: '21' }],    // National Pork Producers, by Term
  ['xyzzyqq', null, 0]
];
// two petitions that ask the same question
var NEAR = [
  ['22-451', '22-1219'],   // Loper Bright / Relentless
  ['20-1199', '21-707'],   // SFFA v. Harvard / v. UNC
  ['24-38', '24-43'],      // Little v. Hecox / West Virginia v. B.P.J.
  ['22-277', '22-555'],    // Moody v. NetChoice / NetChoice v. Paxton
  ['22-506', '22-535'],    // Biden v. Nebraska / Department of Education v. Brown
  ['23-719', '23-696']     // Trump v. Anderson and its companion
];

var bad = 0, slow = 0;
SEARCH.forEach(function (p) {
  var t = Date.now(), r = Q.search(docs, Q.parse(p[0]), { filter: p[3] }), ms = Date.now() - t;
  var rank = -1; for (var i = 0; i < r.hits.length; i++) if (r.hits[i].e.d === p[1]) { rank = i + 1; break; }
  var ok = p[1] === null ? r.total === 0 : rank > 0 && (p[2] === 0 || rank <= p[2]);
  // A filter must hold for every hit, not only for the one being looked for.
  if (p[3]) r.hits.forEach(function (h) {
    if ((p[3].granted && !h.e.g) || (p[3].term && h.e.d.slice(0, 2) !== p[3].term)) ok = false;
  });
  if (!ok) bad++; if (ms > 80) slow++;
  console.log((ok ? '  ok   ' : '  FAIL ') + JSON.stringify(p[0]) + ' -> ' + (p[1] || 'nothing') +
    ': rank ' + (rank < 0 ? '-' : rank) + ' of ' + r.total + ', ' + ms + ' ms' + (ms > 80 ? ' (slow)' : ''));
});
NEAR.forEach(function (p) {
  var rk = function (a, b) { var l = like[a] || []; for (var i = 0; i < l.length; i++) if (l[i][0] === b) return i + 1; return -1; };
  var ab = rk(p[0], p[1]), ba = rk(p[1], p[0]), ok = ab > 0 && ba > 0;
  if (!ok) bad++;
  console.log((ok ? '  ok   ' : '  FAIL ') + p[0] + ' <-> ' + p[1] + ': rank ' + ab + ' and ' + ba);
});
// When no question has every word, the search falls back to all-but-one and
// says so; it must not come back empty-handed or pretend to a full match.
var fb = Q.search(docs, Q.parse('affirmative action admissions xyzzyqq'), {});
var fb2 = Q.search(docs, Q.parse('race admissions xyzzyqq'), {});
var fbOk = fb.total === 0 && fb2.partial === true && fb2.total > 0;
if (!fbOk) bad++;
console.log((fbOk ? '  ok   ' : '  FAIL ') + 'all-but-one fallback: ' + fb2.total + ' partial, and none when two words miss');
// A snippet must mark what the query matched and must never emit raw markup.
var s = Q.snippet('Whether 18 U.S.C. § 922(g)(1) <b>violates</b> the Second Amendment.', Q.parse('922(g)(1) "second amendment"'));
var snipOk = /<mark>922\(g\)\(1\)<\/mark>/.test(s) && /<mark>Second<\/mark> <mark>Amendment\.<\/mark>/.test(s) && s.indexOf('<b>') < 0;
if (!snipOk) bad++;
console.log((snipOk ? '  ok   ' : '  FAIL ') + 'snippet marks matches and escapes markup: ' + s);
console.log(bad ? bad + ' probe(s) FAILED' : 'all probes passed' + (slow ? ' (' + slow + ' slow)' : ''));
process.exit(bad ? 1 : 0);
