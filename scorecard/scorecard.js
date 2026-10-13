// scorecard/scorecard.js -- written by R/scorecard.R; edit it there. Draws the
// Forecast Scorecard from scorecard.json in the browser. Everything is computed
// here from the per-petition rows, so the JSON stays a plain ledger-plus-outcome
// file that any reader can audit.
(function () {
"use strict";
function init(DATA) {
const REF = DATA.reference;
// Which forecast to score: the FIRST one logged (default), or the FINAL one made
// the morning of the conference. Chosen by the URL's #final, so the choice is
// linkable; switching reloads the page rather than redrawing it in place.
const HAS_FINAL = DATA.cases.some(x => x.p_final != null && x.outcome);
const BASIS = HAS_FINAL && location.hash === "#final" ? "final" : "first";
if (BASIS === "final") {
  DATA = Object.assign({}, DATA, { cases: DATA.cases.filter(x => x.p_final != null).map(x => Object.assign({}, x, {
    p: x.p_final, p_gvr: x.p_final_gvr, p_ever: x.p_final_ever, scored_on: x.final_on,
    model: x.final_model, relists: x.final_relists ?? x.relists })) });
}
if (HAS_FINAL) {
  const bp = document.getElementById("basispick");
  bp.hidden = false;
  bp.innerHTML = `<span>Score the</span>` + [["first", "First forecast"], ["final", "Final forecast"]].map(([k, l]) =>
    `<button type="button" data-b="${k}" aria-pressed="${k === BASIS}">${l}</button>`).join("");
  bp.addEventListener("click", e => { const b = e.target.dataset?.b; if (b && b !== BASIS) { location.hash = b; location.reload(); } });
}
const OUT = {
  granted: { label: "Granted", row: "Granted" },
  gvr:     { label: "GVR'd", row: "GVR'd" },
  carried: { label: "Carried over", row: "Carried over" },
  pending: { label: "Still pending", row: "Still pending" },
  denied:  { label: "Denied", row: "Denied" }
};
const ORDER = ["granted", "gvr", "carried", "denied"];
const ORDER_EVER = ["granted", "gvr", "pending", "denied"];
let mode = "here";
const val = x => mode === "here" ? x.p : x.p_ever;
const res = x => mode === "here" ? x.outcome : (x.outcome ? x.ever : null);
const daysAhead = x => Math.round((Date.parse(x.conf) - Date.parse(x.scored_on)) / 864e5);
const $ = id => document.getElementById(id);
const esc = s => String(s ?? "").replace(/[&<>"]/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
// The ledger stores five decimals, so a forecast under 0.0005% is logged as 0.
const pct = (p, d) => p == null ? "—" : p < 0.00001 ? "<0.001%" : (p < 0.001 ? (p * 100).toFixed(3) : p < 0.01 ? (p * 100).toFixed(2) : (p * 100).toFixed(d ?? 1)) + "%";
const MONTHS = ["Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec."];
const fdate = s => { const [y, m, d] = s.split("-").map(Number); return `${MONTHS[m - 1]} ${d}`; };
const fdateY = s => fdate(s) + ", " + s.slice(0, 4);
const caseUrl = d => `/cases/${d}.html`;

// "Todd Blanche, Attorney General v. Izuchukwu Ozurumba" -> "Blanche v. Ozurumba"
const ORG = /\b(Inc|LLC|L\.L\.C|Corp|Corporation|Company|Co|County|State|States|United|Department|Board|City|Alliance|Service|Bank|Court|Commission|University|Association|Church|Tribe|Tribes|Nation|Ltd|N\.A|Institute|Center|Agency|Office|Republic|Exchange|Parish|School|District|Committee|Authority|Union|Trust|Fund|Partners|Group|Holdings|Services|Systems|Technologies|Pharmaceuticals|Energy|Missionaries|Society|Foundation|Council|Village|Town|Township)\b/;
function party(x) {
  x = x.replace(/,.*$/, "").replace(/\bet al\.?/, "").trim();
  if (/^In re /i.test(x)) return x;
  const w = x.split(/\s+/).filter(t => !/^(Jr|Sr|II|III|IV)\.?$/.test(t));
  if (w.length <= 4 && !ORG.test(x)) return w[w.length - 1];
  return x.length > 34 ? x.slice(0, 32).trim() + "…" : x;
}
const short = c => { const s = c.split(" v. "); return s.length < 2 ? party(c) : party(s[0]) + " v. " + party(s.slice(1).join(" v. ")); };

// The page shows one Term; the on-deck panel reads every upcoming conference.
const CONFS = DATA.conferences.filter(c => c.term === DATA.term_year);
// Decided, and with rows to score: on the final basis, a conference held before
// finals were logged has none.
const decided = CONFS.filter(c => c.status === "decided" && DATA.cases.some(x => x.conf === c.date));
const CASES = DATA.cases.filter(x => CONFS.some(c => c.date === x.conf));
// rank within conference among scored petitions
for (const c of CONFS) {
  const sc = CASES.filter(x => x.conf === c.date && x.p != null).sort((a, b) => b.p - a.p);
  sc.forEach((x, i) => { x.rank = i + 1; x.of = sc.length; });
}
const scoredDecided = CASES.filter(x => x.outcome && x.p != null);

// ---------- metrics ----------
function metrics(rows) {
  const n = rows.length, y = rows.map(r => r.outcome === "granted" ? 1 : 0), p = rows.map(r => r.p);
  const ev = y.reduce((a, b) => a + b, 0), expd = p.reduce((a, b) => a + b, 0);
  const brier = n ? rows.reduce((s, r, i) => s + (p[i] - y[i]) ** 2, 0) / n : NaN;
  const base = n ? ev / n : NaN;
  const skill = base > 0 && base < 1 ? 1 - brier / (base * (1 - base)) : NaN;
  const ll = n ? -rows.reduce((s, r, i) => { const q = Math.min(Math.max(p[i], 1e-6), 1 - 1e-6); return s + (y[i] ? Math.log(q) : Math.log(1 - q)); }, 0) / n : NaN;
  let auc = NaN;
  if (ev > 0 && ev < n) {
    const idx = p.map((v, i) => [v, y[i]]).sort((a, b) => a[0] - b[0]);
    let r = 1, sumPos = 0;
    for (let i = 0; i < idx.length;) {
      let j = i; while (j + 1 < idx.length && idx[j + 1][0] === idx[i][0]) j++;
      const avg = (r + r + (j - i)) / 2;
      for (let k = i; k <= j; k++) if (idx[k][1]) sumPos += avg;
      r += j - i + 1; i = j + 1;
    }
    auc = (sumPos - ev * (ev + 1) / 2) / (ev * (n - ev));
  }
  return { n, ev, expd, brier, skill, ll, auc, base };
}
function wilson(k, n) {
  if (!n) return [0, 0];
  const z = 1.96, ph = k / n, d = 1 + z * z / n, c = ph + z * z / (2 * n), m = z * Math.sqrt(ph * (1 - ph) / n + z * z / (4 * n * n));
  return [Math.max(0, (c - m) / d), Math.min(1, (c + m) / d)];
}
if (!decided.length) {
  $("asof").textContent = `${DATA.term}: no conference scored yet. Results appear the day its order list is released.`;
  deck();
  return;
}
const M = metrics(scoredDecided);

// ---------- header + tiles ----------
const lastOrder = decided.flatMap(c => c.orders).map(o => o.date).sort().pop();
$("asof").textContent = `${DATA.term} · ${decided.length} conference${decided.length === 1 ? "" : "s"} scored, through the orders of ${lastOrder ? fdateY(lastOrder) : "—"} · data built ${DATA.generated.replace("T", " ").replace("Z", " UTC")}`;
const allDecided = CASES.filter(x => x.outcome);
const grantsAll = allDecided.filter(x => x.outcome === "granted");
const grantsScored = grantsAll.filter(x => x.p != null);
const pctiles = grantsScored.map(x => x.rank / x.of).sort((a, b) => a - b);
const medPct = pctiles.length ? pctiles[Math.floor((pctiles.length - 1) / 2)] : null;
const aheads = scoredDecided.map(daysAhead).sort((a, b) => a - b), medAhead = aheads.length ? aheads[Math.floor(aheads.length / 2)] : "—";
// Hours from the final forecast to 13:30 UTC on conference day (9:30 a.m. EDT; an hour short under EST).
const hrs = scoredDecided.filter(x => x.final_at).map(x => (Date.parse(x.conf + "T13:30:00Z") - Date.parse(x.final_at)) / 36e5).sort((a, b) => a - b);
const medHours = hrs.length ? Math.round(hrs[Math.floor(hrs.length / 2)]) : "—";
const top10 = grantsScored.filter(x => x.rank / x.of <= 0.10).length;
// Bootstrap over logged petitions: a rough interval for the live AUC and Brier skill.
function boot(rows, B) {
  let seed = 7; const rnd = () => (seed = (seed * 1664525 + 1013904223) >>> 0) / 4294967296;
  const a = [], k = [];
  for (let b = 0; b < B; b++) {
    const r = rows.map(() => rows[Math.floor(rnd() * rows.length)]);
    const m = metrics(r);
    if (!isNaN(m.auc)) a.push(m.auc);
    if (!isNaN(m.skill)) k.push(m.skill);
  }
  const q = (v, p) => { v.sort((x, y) => x - y); return v[Math.min(v.length - 1, Math.floor(p * v.length))]; };
  return { auc: [q(a, .025), q(a, .975)], skill: [q(k, .025), q(k, .975)] };
}
const CI = boot(scoredDecided, 600);

// The scale a reader needs to judge a number: where "no better than chance" sits,
// where perfect sits, the backtest, and the live figure with its interval.
function scale(lo, hi, marks, live, ci, ref) {
  const W = 220, x = v => 6 + (Math.min(Math.max(v, lo), hi) - lo) / (hi - lo) * (W - 12);
  let g = `<line x1="${x(lo)}" x2="${x(hi)}" y1="12" y2="12" stroke="var(--grid)" stroke-width="6" stroke-linecap="round"/>`;
  for (const [v, l] of marks) g += `<line x1="${x(v)}" x2="${x(v)}" y1="7" y2="17" stroke="var(--faint)" stroke-width="1"/><text x="${x(v)}" y="30" text-anchor="${v === lo ? "start" : v === hi ? "end" : "middle"}">${l}</text>`;
  if (ci) g += `<line x1="${x(ci[0])}" x2="${x(ci[1])}" y1="12" y2="12" stroke="var(--ink-soft)" stroke-width="2" opacity=".55"/>`;
  g += `<path d="M${x(ref) - 4.5},12 l4.5,-4.5 l4.5,4.5 l-4.5,4.5z" fill="var(--panel)" stroke="var(--ink)" stroke-width="1.3"/>`;
  if (!isNaN(live)) g += `<circle cx="${x(live)}" cy="12" r="4.5" fill="var(--ink)" stroke="var(--panel)" stroke-width="1.5"/>`;
  return `<svg class="scale" viewBox="0 0 ${W} 34" role="img" aria-label="scale">${g}</svg>`;
}
const VERDICT_ICON = { good: "✓", mixed: "!", bad: "✕" };
const verdict = (cls, text) => `<span class="verdict ${cls}"><i aria-hidden="true">${VERDICT_ICON[cls]}</i>${text}</span>`;
function aucVerdict(v, ci) {
  if (isNaN(v)) return ["mixed", "Not enough grants yet"];
  const band = v >= .9 ? "Excellent" : v >= .8 ? "Good" : v >= .7 ? "Fair" : v >= .6 ? "Weak" : "No better than chance";
  const cls = v >= .8 ? "good" : v >= .7 ? "mixed" : "bad";
  const vs = ci[0] <= REF.auc && REF.auc <= ci[1] ? "in line with the backtest" : v < REF.auc ? "below the backtest" : "above the backtest";
  return [cls, `${band}, ${vs}`];
}
function skillVerdict(v, ci) {
  if (isNaN(v)) return ["mixed", "Not enough grants yet"];
  if (v <= 0) return ["bad", "No better than the base rate"];
  const vs = ci[0] <= REF.brier_skill && REF.brier_skill <= ci[1] ? "in line with the backtest" : v < REF.brier_skill ? "below the backtest" : "above the backtest";
  return ci[0] > 0 ? ["good", `Better than the base rate, ${vs}`]
                   : ["mixed", `Ahead of the base rate, not yet clearly; ${vs}`];
}
const [aCls, aTxt] = aucVerdict(M.auc, CI.auc), [sCls, sTxt] = skillVerdict(M.skill, CI.skill);
const r2 = v => v.toFixed(2).replace("-", "−");

const tiles = [
  { k: "Grants", v: `${grantsScored.length}<small> of ${M.expd.toFixed(1)} expected</small>`,
    s: `Granted at their conference, among ${M.n.toLocaleString()} logged paid petitions. Expected is the sum of the forecasts.` },
  { k: "In the top tenth", v: `${top10}<small> of ${grantsScored.length} grants</small>`,
    s: `Grants ranked in the top 10% of their conference. Backtest: ${Math.round(REF.top10_recall * 100)}%.` },
  { k: "Median grant", v: medPct == null ? "—" : `top ${Math.max(1, Math.round(medPct * 100))}%`,
    s: "Where the typical grant ranked among the petitions logged for its conference." },
  { k: "AUC · ranking", v: isNaN(M.auc) ? "—" : M.auc.toFixed(3),
    x: verdict(aCls, aTxt) + scale(.5, 1, [[.5, "0.5 chance"], [.8, "0.8"], [1, "1.0 perfect"]], M.auc, CI.auc, REF.auc),
    s: `How often a granted petition is ranked above one that wasn't. Live ● with its range (${r2(CI.auc[0])}–${r2(CI.auc[1])}); backtest ◇ ${REF.auc.toFixed(3)}.` },
  { k: "Brier skill · accuracy", v: isNaN(M.skill) ? "—" : r2(M.skill),
    x: verdict(sCls, sTxt) + scale(-.2, .5, [[-.2, "−0.2"], [0, "0 base rate"], [.5, "0.5"]], M.skill, CI.skill, REF.brier_skill),
    s: `How much closer the forecasts come than giving every petition the average chance. Above 0 is better; range ${r2(CI.skill[0])} to ${r2(CI.skill[1])}; backtest ◇ ${r2(REF.brier_skill)}.` },
  BASIS === "final"
    ? { k: "Final forecast", v: `${medHours}<small> hours ahead</small>`, s: "Median time between the final forecast and the Justices' 9:30 a.m. start on conference day." }
    : { k: "Logged ahead", v: `${medAhead}<small> days</small>`, s: "Median time between a forecast's entry in the ledger and its conference." }
];
$("tiles").innerHTML = tiles.map(t => `<div class="tile"><span class="k">${t.k}</span><span class="v">${t.v}</span>${t.x || ""}<span class="s">${t.s}</span></div>`).join("");

// ---------- tooltip helpers ----------
function tipHTML(x) {
  const o = x.outcome ? OUT[x.outcome].label + (x.decided ? `, ${fdate(x.decided)}` : "") : "Awaiting conference";
  const rows = [
    ["Conference", fdateY(x.conf)],
    ["At this conference", `${pct(x.p)} · rank ${x.rank} of ${x.of}`],
    ["Ever", pct(x.p_ever)],
    ["Outcome here", o + (x.cvsg ? " · CVSG" : "")],
    ["Since", x.ever ? (x.ever === "pending" ? "still pending" : OUT[x.ever].label + (x.ever_date ? `, ${fdate(x.ever_date)}` : "")) : "—"],
    ["Logged", `${fdate(x.scored_on)}, ${daysAhead(x)} days ahead${x.superseded ? " (re-scored)" : ""}`],
    ["Prior relists", x.relists ?? "—"]
  ];
  if (x.court) rows.push(["Below", x.court]);
  if (x.subject) rows.push(["Subject", x.subject]);
  return `<b>${esc(short(x.caption))}</b><div style="color:var(--faint);margin-bottom:.3rem">${esc(x.dkt)}</div>` +
    rows.map(r => `<div class="row"><span>${r[0]}</span><span>${esc(r[1])}</span></div>`).join("");
}
function placeTip(tip, wrap, evt) {
  const b = wrap.getBoundingClientRect();
  let x = evt.clientX - b.left + 14, y = evt.clientY - b.top + 14;
  tip.hidden = false;
  const w = tip.offsetWidth, h = tip.offsetHeight;
  if (x + w > b.width) x = Math.max(0, evt.clientX - b.left - w - 14);
  if (y + h > b.height + 40) y = Math.max(0, evt.clientY - b.top - h - 14);
  tip.style.left = x + "px"; tip.style.top = y + "px";
}

// ---------- dot plot ----------
let confSel = decided.length ? decided[decided.length - 1].date : "all";
$("confpick").innerHTML = [...decided.map(c => [c.date, fdateY(c.date) + (c.date.slice(5) >= "09-20" && c.date.slice(5) <= "10-02" ? " (long conference)" : "")]), ["all", "All scored conferences"]]
  .map(([k, l]) => `<button type="button" id="cp-${k}" data-k="${k}" aria-pressed="${k === confSel}">${l}</button>`).join("");
$("legend").innerHTML = ["granted", "gvr", "carried", "denied"].map(k => `<span><i style="background:var(--o-${k})"></i>${k === "carried" ? "Carried over / still pending" : OUT[k].label}</span>`).join("") +
  `<span><i style="border:1px solid var(--ink);width:.75rem;height:.75rem"></i>CVSG (ringed)</span>`;

function hash(s) { let h = 2166136261; for (const c of s) { h ^= c.charCodeAt(0); h = Math.imul(h, 16777619); } return ((h >>> 0) % 10000) / 10000; }
let pinned = null;
function drawDots() {
  const rows = CASES.filter(x => x.outcome && (confSel === "all" || x.conf === confSel));
  const scored = rows;
  const RO = mode === "here" ? ORDER : ORDER_EVER;
  const W = 900, L = 150, R = 24, T = 20;
  const rowH = { granted: 74, gvr: 64, carried: 110, pending: 110, denied: 130 };
  const lo = 1e-5, hi = 1;
  const x = p => L + (Math.log10(Math.max(p, lo)) - Math.log10(lo)) / (Math.log10(hi) - Math.log10(lo)) * (W - L - R);
  let y0 = T, g = "", labels = [];
  const ticks = [1e-4, 1e-3, 1e-2, 0.1, 0.5];
  const H = T + RO.reduce((a, k) => a + rowH[k], 0) + 34;
  for (const t of ticks) g += `<line class="gridline" x1="${x(t)}" x2="${x(t)}" y1="${T}" y2="${H - 30}"/><text x="${x(t)}" y="${H - 12}" text-anchor="middle">${pct(t, 0).replace(".00%", "%").replace(".0%", "%")}</text>`;
  // Forecasts logged as 0 (under 0.0005%) sit on the floor of the axis.
  g += `<text x="${x(lo)}" y="${H - 12}" text-anchor="middle">≤0.001%</text>`;
  for (const k of RO) {
    const h = rowH[k], mid = y0 + h / 2, inRow = rows.filter(r => res(r) === k), sc = inRow;
    g += `<line class="axis" x1="${L}" x2="${W - R}" y1="${y0 + h}" y2="${y0 + h}"/>`;
    g += `<text class="ink" x="0" y="${mid - 2}" font-size="13">${OUT[k].label}</text><text x="0" y="${mid + 14}">${sc.length} petition${sc.length === 1 ? "" : "s"}</text>`;
    const spread = k === "granted" || k === "gvr" ? 0.25 : 0.8;
    for (const r of sc) {
      const cy = mid + (hash(r.dkt + r.conf) - 0.5) * (h - 18) * spread;
      const rad = k === "denied" ? 3.6 : k === "granted" ? 6 : 4.8;
      const i = CASES.indexOf(r);
      g += `<circle class="dot ${k}${pinned === i ? " on" : ""}" data-i="${i}" cx="${x(val(r)).toFixed(1)}" cy="${cy.toFixed(1)}" r="${rad}" tabindex="0" role="button" aria-label="${esc(short(r.caption))}, ${pct(val(r))}, ${OUT[k].label}"/>`;
      if (r.cvsg && mode === "here") g += `<circle class="cvsg" cx="${x(val(r)).toFixed(1)}" cy="${cy.toFixed(1)}" r="${rad + 3.5}" pointer-events="none"/>`;
      if (k === "granted") labels.push({ x: x(val(r)), y: cy, t: `${short(r.caption)}` });
    }
    y0 += h;
  }
  // direct labels on grants: stacked above the row, right-aligned to their dot
  labels.sort((a, b) => b.x - a.x).forEach((l, j) => {
    const ly = T + 12 + j * 13 - 2;
    const anchor = l.x > W - 180 ? "end" : "start";
    const tx = anchor === "end" ? l.x : l.x + 4;
    g += `<line class="lead" x1="${l.x}" x2="${l.x}" y1="${ly + 3}" y2="${l.y - 6}"/><text class="soft" x="${tx}" y="${ly}" text-anchor="${anchor}" font-size="11">${esc(l.t)}</text>`;
  });
  const svg = $("dots");
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
  svg.style.minWidth = "640px";
  svg.innerHTML = g;
  $("dotsnote").textContent = mode === "here"
    ? `${rows.length} logged paid petitions; IFP petitions carry no forecast and are not shown. Horizontal axis: the logged chance of a grant at this conference, log scale. Carried over means relisted, held, rescheduled or a CVSG.`
    : `${rows.length} logged paid petitions. Horizontal axis: the logged chance the petition is ever granted, log scale. Rows are what has happened since the conference; still-pending petitions have not been decided.`;
}
const dotsWrap = $("dots").parentElement.parentElement;
function showCard(i) {
  const x = CASES[i];
  $("card").hidden = false;
  $("card").innerHTML = `<h3>${esc(x.caption)}</h3><dl>
    <dt>Docket</dt><dd><a href="${caseUrl(x.dkt)}" target="_blank" rel="noopener">${esc(x.dkt)}</a></dd>
    <dt>Conference</dt><dd>${fdateY(x.conf)}</dd>
    <dt>At this conference</dt><dd>${pct(x.p)}, rank ${x.rank} of ${x.of} · GVR ${pct(x.p_gvr)}</dd>
    <dt>Ever granted</dt><dd>${pct(x.p_ever)}</dd>
    <dt>Logged</dt><dd>${fdateY(x.scored_on)}, ${daysAhead(x)} days before the conference${x.superseded ? " · re-scored after a code fix, still before the conference" : ""} · model ${esc(x.model || "—")}</dd>
    <dt>Outcome</dt><dd>${x.outcome ? OUT[x.outcome].label + (x.decided ? ` (${fdateY(x.decided)})` : "") : "—"}${x.cvsg ? " · Solicitor General invited" : ""}</dd>
    <dt>Since</dt><dd>${x.ever === "pending" ? "Still pending" : x.ever ? OUT[x.ever].label + (x.ever_date ? ` (${fdateY(x.ever_date)})` : "") : "—"}</dd>
    <dt>Prior relists</dt><dd>${x.relists ?? "—"}</dd>
    ${x.court ? `<dt>Court below</dt><dd>${esc(x.court)}</dd>` : ""}
    ${x.subject ? `<dt>Subject</dt><dd>${esc(x.subject)}</dd>` : ""}
    ${x.counsel ? `<dt>Counsel</dt><dd>${esc(x.counsel)}</dd>` : ""}</dl>`;
}
$("dots").addEventListener("pointermove", e => {
  const i = e.target.dataset?.i;
  if (i == null) { $("tip").hidden = true; return; }
  $("tip").innerHTML = tipHTML(CASES[+i]); placeTip($("tip"), dotsWrap, e);
});
$("dots").addEventListener("pointerleave", () => $("tip").hidden = true);
$("dots").addEventListener("click", e => { const i = e.target.dataset?.i; if (i != null) { pinned = +i; showCard(pinned); drawDots(); } });
$("dots").addEventListener("keydown", e => { const i = e.target.dataset?.i; if (i != null && (e.key === "Enter" || e.key === " ")) { e.preventDefault(); pinned = +i; showCard(pinned); drawDots(); } });
$("modepick").addEventListener("click", e => {
  const m = e.target.dataset?.m; if (!m) return;
  mode = m; document.querySelectorAll("#modepick button").forEach(b => b.setAttribute("aria-pressed", b.dataset.m === m));
  $("tip").hidden = true; drawDots();
});
$("confpick").addEventListener("click", e => {
  const k = e.target.dataset?.k; if (!k) return;
  confSel = k; document.querySelectorAll("#confpick button").forEach(b => b.setAttribute("aria-pressed", b.dataset.k === k));
  $("tip").hidden = true; drawDots();
});

// ---------- grant ranks ----------
$("ranks").innerHTML = grantsAll.sort((a, b) => a.conf.localeCompare(b.conf) || (a.rank ?? 1e9) - (b.rank ?? 1e9)).map(x => {
  const left = x.p == null ? 0 : (x.rank - 1) / Math.max(1, x.of - 1) * 100;
  return `<li class="${x.p == null ? "unscored" : ""}"><span class="case"><a href="${caseUrl(x.dkt)}" target="_blank" rel="noopener">${esc(short(x.caption))}</a></span>
    <span class="num">${x.p == null ? "IFP, not scored" : `#${x.rank} of ${x.of}`}</span>
    <span class="bar"><i style="left:calc(${left}% - 2px)"></i></span>
    <span class="meta">${esc(x.dkt)} · ${fdate(x.conf)} conference · ${x.p == null ? `${x.relists} prior relists` : `forecast ${pct(x.p)} · top ${Math.max(1, Math.ceil(x.rank / x.of * 100))}%`}</span></li>`;
}).join("");

// ---------- gains ----------
function drawGains() {
  const rows = [...scoredDecided].sort((a, b) => b.p - a.p), n = rows.length, ev = rows.filter(r => r.outcome === "granted").length;
  const W = 420, H = 300, L = 44, R = 14, T = 12, B = 36;
  const x = v => L + v * (W - L - R), y = v => T + (1 - v) * (H - T - B);
  let g = "";
  for (const t of [0, .25, .5, .75, 1]) {
    g += `<line class="gridline" x1="${x(0)}" x2="${x(1)}" y1="${y(t)}" y2="${y(t)}"/><text x="${L - 6}" y="${y(t) + 4}" text-anchor="end">${t * 100}%</text>`;
    g += `<text x="${x(t)}" y="${H - B + 16}" text-anchor="middle">${t * 100}%</text>`;
  }
  g += `<text x="${x(.5)}" y="${H - 4}" text-anchor="middle">share of petitions, highest forecast first</text>`;
  g += `<line x1="${x(0)}" y1="${y(0)}" x2="${x(1)}" y2="${y(1)}" stroke="var(--rule)" stroke-dasharray="4 4"/>`;
  let pts = [[0, 0]], c = 0;
  rows.forEach((r, i) => { if (r.outcome === "granted") { pts.push([i / n, c / ev]); c++; pts.push([(i + 1) / n, c / ev]); } });
  pts.push([1, 1]);
  g += `<path d="M${pts.map(p => `${x(p[0]).toFixed(1)},${y(p[1]).toFixed(1)}`).join("L")}" fill="none" stroke="var(--o-granted)" stroke-width="2" stroke-linejoin="round"/>`;
  // backtest reference: top-10% recall
  g += `<line x1="${x(.1)}" x2="${x(.1)}" y1="${y(0)}" y2="${y(1)}" stroke="var(--ref)" stroke-width="1" stroke-dasharray="2 3"/>`;
  g += `<path d="M${x(.1) - 5},${y(REF.top10_recall)} l5,-5 l5,5 l-5,5z" fill="var(--panel)" stroke="var(--ref)" stroke-width="1.5"/>`;
  g += `<line id="gx" x1="0" x2="0" y1="${y(0)}" y2="${y(1)}" stroke="var(--ink-soft)" stroke-width="1" visibility="hidden"/>`;
  g += `<rect id="ghit" x="${x(0)}" y="${y(1)}" width="${x(1) - x(0)}" height="${y(0) - y(1)}" fill="transparent"/>`;
  const svg = $("gains"); svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.innerHTML = g;
  const wrap = svg.parentElement;
  svg.addEventListener("pointermove", e => {
    const b = svg.getBoundingClientRect(), sx = (e.clientX - b.left) / b.width * W;
    const f = Math.min(1, Math.max(0, (sx - L) / (W - L - R)));
    if (sx < L || sx > W - R) { $("tip2").hidden = true; $("gx").setAttribute("visibility", "hidden"); return; }
    const k = Math.max(1, Math.round(f * n)), found = rows.slice(0, k).filter(r => r.outcome === "granted").length;
    $("gx").setAttribute("x1", x(k / n)); $("gx").setAttribute("x2", x(k / n)); $("gx").setAttribute("visibility", "visible");
    $("tip2").innerHTML = `<div class="row"><span>Top</span><span>${k} of ${n} petitions (${Math.round(k / n * 100)}%)</span></div><div class="row"><span>Grants found</span><span>${found} of ${ev}</span></div><div class="row"><span>Lowest forecast</span><span>${pct(rows[k - 1].p)}</span></div>`;
    placeTip($("tip2"), wrap, e);
  });
  svg.addEventListener("pointerleave", () => { $("tip2").hidden = true; $("gx").setAttribute("visibility", "hidden"); });
  const firstAll = rows.findIndex((r, i) => rows.slice(0, i + 1).filter(z => z.outcome === "granted").length === ev);
  $("gainsnote").textContent = (ev ? `All ${ev} scored grants were in the top ${Math.ceil((firstAll + 1) / n * 100)}% of the ${n} scored petitions. ` : "No scored grants yet. ") +
    `◇ The backtest found ${Math.round(REF.top10_recall * 100)}% of grants in the top 10%.`;
}

// ---------- calibration ----------
function drawCalib() {
  const bins = [[0, .005], [.005, .02], [.02, .05], [.05, .1], [.1, .2], [.2, 1.01]];
  const lab = ["under 0.5%", "0.5–2%", "2–5%", "5–10%", "10–20%", "20% and up"];
  const data = bins.map(([a, b], i) => {
    const r = scoredDecided.filter(x => x.p >= a && x.p < b), k = r.filter(x => x.outcome === "granted").length;
    return { lab: lab[i], n: r.length, k, pred: r.length ? r.reduce((s, x) => s + x.p, 0) / r.length : null, obs: r.length ? k / r.length : null, ci: wilson(k, r.length) };
  });
  const W = 900, L = 120, R = 150, T = 24, rowH = 40, H = T + data.length * rowH + 30;
  const top = 0.6, sx = v => L + Math.sqrt(Math.min(v, top) / top) * (W - L - R);
  let g = "";
  for (const t of [0, .01, .05, .1, .2, .4, .6]) g += `<line class="gridline" x1="${sx(t)}" x2="${sx(t)}" y1="${T - 8}" y2="${H - 26}"/><text x="${sx(t)}" y="${H - 10}" text-anchor="middle">${t * 100}%</text>`;
  g += `<text x="${L}" y="${T - 12}">forecast group</text><text x="${W - R + 12}" y="${T - 12}">granted / petitions</text>`;
  data.forEach((d, i) => {
    const cy = T + i * rowH + rowH / 2;
    g += `<text class="ink" x="${L - 12}" y="${cy + 4}" text-anchor="end" font-size="12">${d.lab}</text>`;
    g += `<text class="num" x="${W - R + 12}" y="${cy + 4}" font-size="12">${d.k} / ${d.n}</text>`;
    g += `<rect data-b="${i}" x="0" y="${cy - rowH / 2}" width="${W}" height="${rowH}" fill="transparent"/>`;
    if (!d.n) return;
    g += `<line x1="${sx(d.ci[0])}" x2="${sx(d.ci[1])}" y1="${cy}" y2="${cy}" stroke="var(--o-granted)" stroke-width="2" stroke-linecap="round" opacity=".45" pointer-events="none"/>`;
    g += `<path d="M${sx(d.pred) - 6},${cy} l6,-6 l6,6 l-6,6z" fill="var(--panel)" stroke="var(--ink)" stroke-width="1.5" pointer-events="none"/>`;
    g += `<circle cx="${sx(d.obs)}" cy="${cy}" r="5" fill="var(--o-granted)" stroke="var(--paper)" stroke-width="1.5" pointer-events="none"/>`;
  });
  const svg = $("calib"); svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.style.minWidth = "600px"; svg.innerHTML = g;
  const wrap = svg.parentElement.parentElement;
  svg.addEventListener("pointermove", e => {
    const b = e.target.dataset?.b; if (b == null) { $("tip3").hidden = true; return; }
    const d = data[+b];
    $("tip3").innerHTML = `<b>Forecast ${d.lab}</b><div class="row"><span>Petitions</span><span>${d.n}</span></div><div class="row"><span>Average forecast</span><span>${pct(d.pred)}</span></div><div class="row"><span>Granted</span><span>${d.k} (${pct(d.obs)})</span></div><div class="row"><span>95% interval</span><span>${pct(d.ci[0])} – ${pct(d.ci[1])}</span></div><div class="row"><span>Expected grants</span><span>${d.n ? (d.pred * d.n).toFixed(1) : "—"}</span></div>`;
    placeTip($("tip3"), wrap, e);
  });
  svg.addEventListener("pointerleave", () => $("tip3").hidden = true);
  $("calibnote").textContent = `Horizontal axis on a square-root scale so the low groups are readable. In the backtest the conference forecasts ran slightly over-dispersed (calibration slope ${REF.cal_slope}): the highest forecasts a little too high, the lowest a little too low.`;
}

// ---------- ledger table ----------
function ledger() {
  const head = `<thead><tr><th>Conference</th><th>Status</th><th>Paid logged</th><th>Granted</th><th>Expected grants</th><th>GVR'd</th><th>Carried over</th><th>Denied</th><th>Brier</th><th>AUC</th><th>Log loss</th><th>Granted ever, so far</th><th>Expected ever</th></tr></thead>`;
  const row = (label, sub, status, rows, all) => {
    const m = metrics(rows), cnt = k => all.filter(x => x.outcome === k).length;
    return `<tr><td>${label}${sub ? `<span class="sub">${sub}</span>` : ""}</td><td>${status}</td><td>${m.n}</td>
      <td>${cnt("granted")}${cnt("granted") - m.ev ? `<span class="sub">${m.ev} scored</span>` : ""}</td><td>${m.expd.toFixed(1)}</td><td>${cnt("gvr")}</td><td>${cnt("carried")}</td><td>${cnt("denied")}</td>
      <td>${isNaN(m.brier) ? "—" : m.brier.toFixed(4)}</td><td>${isNaN(m.auc) ? "—" : m.auc.toFixed(3)}</td><td>${isNaN(m.ll) ? "—" : m.ll.toFixed(4)}</td>
      <td>${all.filter(x => x.ever === "granted").length}<span class="sub">${all.filter(x => x.ever === "pending").length} pending</span></td><td>${all.reduce((s, x) => s + (x.p_ever || 0), 0).toFixed(1)}</td></tr>`;
  };
  const body = decided.map(c => {
    const all = CASES.filter(x => x.conf === c.date && x.outcome), sc = all.filter(x => x.p != null);
    const ah = all.map(daysAhead).sort((a, b) => a - b);
    const sub = `logged ${ah[0]}–${ah[ah.length - 1]} days ahead${c.superseded ? ` · ${c.superseded} re-scored` : ""} · orders ${[...new Set(c.orders.filter(o => CASES.some(x => x.conf === c.date && x.decided === o.date)).map(o => fdate(o.date)))].join(", ")}`;
    return row(fdateY(c.date), sub, `<span class="pill decided">Scored</span>`, sc, all);
  }).join("");
  const all = CASES.filter(x => x.outcome);
  const foot = row(`${DATA.term} to date`, "", "", scoredDecided, all) +
    `<tr><td>Backtest<span class="sub">${REF.scheme}</span></td><td></td><td>${REF.rows.toLocaleString()}</td><td>${REF.events}</td><td></td><td></td><td></td><td></td><td>${REF.brier.toFixed(4)}</td><td>${REF.auc.toFixed(3)}</td><td>${REF.logloss.toFixed(4)}</td><td></td><td></td></tr>`;
  $("ledger").innerHTML = head + `<tbody>${body}</tbody><tfoot>${foot}</tfoot>`;
}

// ---------- on deck ----------
function deck() {
  const up = DATA.conferences.filter(c => c.status !== "decided");
  $("deck").innerHTML = up.length ? up.map(c => {
    const rows = DATA.cases.filter(x => x.conf === c.date), sc = rows.filter(x => x.p != null).sort((a, b) => b.p - a.p);
    const exp = sc.reduce((s, x) => s + x.p, 0);
    return `<div><h3>${fdateY(c.date)}</h3><div class="ui" style="font-size:.8rem;color:var(--faint)">${sc.length} paid petitions logged · ${exp.toFixed(1)} grants expected here${c.status === "awaiting" ? " · held, orders pending" : ""}</div>
      <ol>${sc.slice(0, 5).map(x => `<li><a href="${caseUrl(x.dkt)}" target="_blank" rel="noopener">${esc(short(x.caption))}</a> <span>${pct(x.p)} here · ${pct(x.p_ever)} ever${x.relists ? ` · relisted ${x.relists}×` : ""}</span></li>`).join("")}</ol></div>`;
  }).join("") : `<p class="ui">No conferences scheduled.</p>`;
}

drawDots(); drawGains(); drawCalib(); ledger(); deck();
}

fetch("/scorecard/scorecard.json", { cache: "no-cache" })
  .then(r => { if (!r.ok) throw new Error("HTTP " + r.status); return r.json(); })
  .then(init)
  .catch(err => {
    const a = document.getElementById("asof");
    if (a) a.textContent = "The scorecard data could not be loaded. Try reloading the page.";
    console.error("scorecard:", err);
  });
})();

