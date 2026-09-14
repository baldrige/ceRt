#!/usr/bin/env node
// search_probe.js -- run the case search matcher (search.js) against the real
// index and a list of probes, and report rank and timing.
//
//   node .github/scripts/search_probe.js [path/to/search.json]
//
// Default index: site/cases/search.json (the gh-pages checkout). Each probe is
// a query and the docket it should find; the report is the rank at which that
// docket appears (1 = top) and the time the query took over the full index.
// A probe that misses or ranks past 3 is a FAIL; a query over 50 ms is a WARN.
// Numbers from each run go in docs/search.md.

'use strict';
const fs = require('fs');
const path = require('path');
const S = require(path.join(__dirname, '..', '..', 'search.js'));

const file = process.argv[2] || path.join('site', 'cases', 'search.json');
if (!fs.existsSync(file)) { console.error('no index at ' + file); process.exit(2); }
const raw = JSON.parse(fs.readFileSync(file, 'utf8'));

let t0 = process.hrtime.bigint();
const E = S.build(raw);
const buildMs = Number(process.hrtime.bigint() - t0) / 1e6;
console.log(`index: ${E.length} cases, built in ${buildMs.toFixed(0)} ms`);

// [query, docket it must find, what the probe exercises]
const PROBES = [
  ['24-1046',            '24-1046', 'exact docket'],
  ['24 1046',            '24-1046', 'docket with a space'],
  ['241046',             '24-1046', 'docket without the hyphen'],
  ['No. 24-1046',        '24-1046', 'docket with "No."'],
  ['25A312',             '25A312',  'application docket'],
  ['25a312',             '25A312',  'application docket, lower case'],
  ['Wolford',            '24-1046', 'one surname'],
  ['wolford lopez',      '24-1046', 'two parties, no v.'],
  ['lopez wolford',      '24-1046', 'parties reversed'],
  ['Wolford v Lopez',    '24-1046', 'v without the period'],
  ['wolf lop',           '24-1046', 'prefixes of both parties'],
  ['Skrmetti',           '23-477',  'one surname, current-Term case'],
  ['Skermetti',          '23-477',  'typo: inserted letter'],
  ['Skrmeti',            '23-477',  'typo: dropped letter'],
  ['Loper Bright',       '22-451',  'two-word party'],
  ['Loper Brigth',       '22-451',  'typo in the second word (one transposition)'],
  ['Trump v. Cook',      '25A312',  'caption with the period'],
  ['trump cook',         '25A312',  'same, bare'],
  ['Genalo Black',       '25-886',  'the case this Term'],
  ['Carr v. Saul',       '19-1442', 'exact caption'],
  ['Bostock',            '17-1618', 'the docket the list once glued a digit to'],
  ['Diaz Colon',         '26-5122', 'diacritics dropped (the caption reads Díaz-Colón)'],
  ['Relentless',         '22-1219', 'companion docket has its own page'],
  ['Landor',             '23-1197', 'single distinctive surname'],
  ['United States v. Skrmetti', '23-477', 'full caption with United States'],
  ['US v Skrmetti',      '23-477',  '"US" for United States'],
  ['Dept of Commerce',   '22-1219', 'abbreviated department'],
  ['FCC',                null,      'initialism (any hit, must not error)'],
  ['xyzzyqq',            null,      'no match (must return nothing)'],
  ['24-',                null,      'docket prefix (any OT24 hit)'],
];

let fails = 0, warns = 0;
for (const [q, want, why] of PROBES) {
  t0 = process.hrtime.bigint();
  const hits = S.query(E, q, 40);
  const ms = Number(process.hrtime.bigint() - t0) / 1e6;
  let rank = want ? hits.findIndex(h => h.e.d === want) + 1 : (hits.length ? 1 : 0);
  let status = 'ok  ';
  if (want && (rank === 0 || rank > 3)) { status = 'FAIL'; fails++; }
  if (want === null && q === 'xyzzyqq' && hits.length) { status = 'FAIL'; fails++; }
  if (ms > 50) { if (status === 'ok  ') status = 'WARN'; warns++; }
  const top = hits[0] ? `${hits[0].e.d} ${hits[0].e.c.slice(0, 48)}` : '(none)';
  console.log(`  [${status}] ${q.padEnd(28)} rank ${String(rank || '-').padStart(2)}  ${ms.toFixed(1).padStart(6)} ms  top: ${top}   -- ${why}`);
}
console.log(`\n${PROBES.length} probes: ${fails} fail, ${warns} slow`);
process.exit(fails ? 1 : 0);
