// Documentation: the soil-temperature sensor screen (R/ts-screen.R). Static:
// reads only data/code.json (the R code; the screen's constants are read from
// it) and static/docs/de-hai-screen.json (the R screen run on the raw DE-Hai
// record by scripts/make-docs-examples.R). The simulations re-implement the
// screen's rules in JavaScript (ports of the R functions named below).
import { header, loadJSON, baseLayout, PLOT_CONFIG, fmt, VERDICT_COLORS } from "./common.js";
import { loadCode, renderSnippets, renderConstants, rConstant, rng, mean, median, finite, mod, num } from "./docs-common.js";

header("docs-sensor-screen.html");
const code = await loadCode();
renderSnippets(code);
renderConstants(code);
const K = (n) => rConstant(code, `TS_SCREEN_${n}`);
const C = Object.fromEntries(["AIRLIKE_AMP", "AIRLIKE_LAG", "FLAT_AMP", "STUCK_FRAC", "LEADS_H", "COVERAGE_MIN", "DEPTH_RATIO", "YEAR_Z",
  "YEAR_MIN_DEV", "MIN_METRICS", "CHANGE_MIN_SIDE", "CHANGE_MIN_YEARS", "STUCK_PERIOD_H", "STUCK_MERGE_H"].map((n) => [n, K(n)]));
const METRICS = Object.keys(C.YEAR_MIN_DEV);
const real = await loadJSON("static/docs/de-hai-screen.json");
const SENSORS = Object.keys(real.sensors);
const BLUES = ["#08306b", "#2171b5", "#4292c6", "#6baed6", "#9ecae1"];

const bind = (id, f = (v) => v) => {
  const el = document.getElementById(id), out = document.getElementById(`${id}-o`);
  const show = () => { if (out) out.textContent = f(el.value); };
  show();
  el.addEventListener("input", show);
  return el;
};
const pill = (v) => `<span class="pill" style="background:${VERDICT_COLORS[v] || "#8c959f"}">${v}</span>`;
const C2PI = 24 / (2 * Math.PI);

// ---------------------------------------------------------------- ports of R/ts-screen.R
// R's round() (halves to even).
const rround = (x) => (Math.abs(x % 1) === 0.5 ? 2 * Math.round(x / 2) : Math.round(x));
// ts_peak_hour()
function peakHour(value, hour) {
  let s = 0, c = 0, n = 0;
  value.forEach((v, i) => {
    if (!finite(v)) return;
    const th = 2 * Math.PI * hour[i] / 24;
    s += v * Math.sin(th); c += v * Math.cos(th); n++;
  });
  return n < 12 ? NaN : mod(Math.atan2(s, c) / (2 * Math.PI) * 24, 24);
}
// ts_wrap_lag()
const wrapLag = (x) => mod(x + 12, 24) - 12;
// mean by time of day, at the sorted unique times of day
function byHour(v, hour) {
  const hours = [...new Set(hour)].sort((a, b) => a - b), pos = new Map(hours.map((h, i) => [h, i]));
  const s = hours.map(() => 0), n = hours.map(() => 0);
  v.forEach((x, i) => { if (finite(x)) { s[pos.get(hour[i])] += x; n[pos.get(hour[i])]++; } });
  return { hours, mean: s.map((x, i) => (n[i] ? x / n[i] : NaN)) };
}
// ts_diurnal_diagnostics()
function diurnalDiagnostics(day, hour, ts, ta, minObs) {
  const days = new Map();
  day.forEach((d, i) => { if (!days.has(d)) days.set(d, []); days.get(d).push(i); });
  const range = (idx, v) => { const w = idx.map((i) => v[i]).filter(finite); return w.length >= minObs ? Math.max(...w) - Math.min(...w) : NaN; };
  const aTs = [], aTa = [];
  for (const idx of days.values()) {
    const a = range(idx, ts), b = range(idx, ta);
    if (finite(a) && finite(b)) { aTs.push(a); aTa.push(b); }
  }
  const out = { amp_ts: NaN, amp_ta: NaN, amp_ratio: NaN, n_amp_days: aTs.length, lag_h: NaN, zd_amp: NaN, zd_lag: NaN };
  if (aTs.length < 5) return out;
  out.amp_ts = median(aTs); out.amp_ta = median(aTa); out.amp_ratio = out.amp_ts / out.amp_ta;
  const mts = byHour(ts, hour), mta = byHour(ta, hour);
  out.wrapped = wrapLag(peakHour(mts.mean, mts.hours) - peakHour(mta.mean, mta.hours));
  let lag = out.wrapped;
  const zdAmp = finite(out.amp_ratio) && out.amp_ratio > 0 ? -Math.log(out.amp_ratio) : NaN;
  if (finite(zdAmp) && finite(lag)) lag += 24 * rround((zdAmp * C2PI - lag) / 24);
  Object.assign(out, { lag_h: lag, zd_amp: zdAmp, zd_lag: lag / C2PI });
  return out;
}
// the rules of ts_screen_series() that use only the diurnal diagnostics
function diurnalRules(d) {
  const ratio = d.zd_lag / d.zd_amp;
  return {
    bad: { airlike: d.amp_ratio > C.AIRLIKE_AMP && Math.abs(d.lag_h) < C.AIRLIKE_LAG, flat: d.amp_ratio < C.FLAT_AMP, leads: d.lag_h < C.LEADS_H },
    suspect: { nonconductive: d.zd_amp > 0.05 && (d.zd_lag <= 0 || ratio > C.DEPTH_RATIO || ratio < 1 / C.DEPTH_RATIO) },
  };
}
const fired = (r) => [...Object.entries(r.bad), ...Object.entries(r.suspect)].filter(([, v]) => v).map(([k]) => k);
const verdictOf = (r) => (Object.values(r.bad).some(Boolean) ? "BAD" : Object.values(r.suspect).some(Boolean) ? "SUSPECT" : "GOOD");
// ts_runs()
function tsRuns(x, zeroBand = 0.5, exempt = true) {
  const runs = [];
  let first = 0;
  for (let i = 1; i <= x.length; i++) {
    const same = i < x.length && finite(x[i]) && finite(x[i - 1]) && x[i] === x[i - 1];
    if (!same) { runs.push({ first, last: i - 1, length: i - first }); first = i; }
  }
  return exempt ? runs.filter((r) => finite(x[r.first]) && Math.abs(x[r.first]) >= zeroBand) : runs;
}
// ts_changepoint(): the strongest split, and every split's scores (for display)
function changepoint(Y) {
  const n = Y.length, scan = [];
  if (n < C.CHANGE_MIN_YEARS) return { best: null, scan };
  let best = null;
  for (let k = C.CHANGE_MIN_SIDE; k <= n - C.CHANGE_MIN_SIDE; k++) {
    const split = METRICS.map((m) => {
      const a = Y.slice(0, k).map((r) => r[m]), b = Y.slice(k).map((r) => r[m]);
      const ma = median(a), mb = median(b), d = mb - ma;
      const spread = 1.4826 * median([...a.map((v) => Math.abs(v - ma)), ...b.map((v) => Math.abs(v - mb))]);
      return { metric: m, before: ma, after: mb, d, score: Math.abs(d) / Math.max(spread, 1e-9), agrees: Math.abs(d) > C.YEAR_MIN_DEV[m] };
    });
    const ok = split.filter((s) => s.agrees && s.score > C.YEAR_Z).sort((p, q) => q.score - p.score);
    const score = ok.reduce((a, s) => a + s.score, 0);
    scan.push({ year: Y[k].growing_year, split, counts: ok.length >= C.MIN_METRICS, score });
    if (ok.length < C.MIN_METRICS) continue;
    if (!best || score > best.score) {
      best = { year: Y[k].growing_year, metric: ok.map((s) => s.metric).join(","), before: ok[0].before, after: ok[0].after, score };
    }
  }
  return { best, scan };
}
// ts_changepoints()
function changepoints(Y) {
  const found = changepoint(Y).best;
  if (!found) return [];
  return [...changepoints(Y.filter((r) => r.growing_year < found.year)), found, ...changepoints(Y.filter((r) => r.growing_year >= found.year))];
}
// the yearly part of ts_screen_series(): segments, odd years, and the windows
// (as ranges of growing years)
function yearlyScreen(yearly) {
  const screenable = yearly.map((r) => METRICS.every((m) => finite(r[m])) && r.n_amp_days >= 20);
  const changes = changepoints(yearly.filter((_, i) => screenable[i]));
  const segment = yearly.map((r) => 1 + changes.filter((c) => r.growing_year >= c.year).length);
  const outlier = yearly.map(() => ""), reference = {};
  for (const seg of [...new Set(segment)]) {
    const inSeg = yearly.map((_, i) => screenable[i] && segment[i] === seg);
    if (inSeg.filter(Boolean).length < 3) continue;
    const odd = METRICS.map((m) => {
      const v = yearly.map((r) => r[m]), vs = v.filter((_, i) => inSeg[i]);
      const med = median(vs);
      const s = 1.4826 * (1.4826 * median(vs.map((x) => Math.abs(x - med))));   // 1.4826 * stats::mad()
      reference[`${seg} ${m}`] = med;
      return v.map((x, i) => inSeg[i] && Math.abs(x - med) > C.YEAR_MIN_DEV[m] && (s === 0 || Math.abs(x - med) / Math.max(s, 1e-9) > C.YEAR_Z));
    });
    yearly.forEach((_, i) => {
      const hit = METRICS.filter((m, j) => odd[j][i]);
      if (hit.length >= C.MIN_METRICS) outlier[i] = hit.map((m) => `shift_${m}`).join(",");
    });
  }
  const windows = [];
  if (changes.length) {
    const last = changes[changes.length - 1];
    windows.push({ years: [yearly[0].growing_year, last.year - 1], scale: "changepoint",
      rule: [...new Set(changes.flatMap((c) => c.metric.split(",")).map((m) => `changepoint_${m}`))].join(","), value: last.before, reference: last.after });
  }
  const flagged = yearly.map((r, i) => ({ ...r, outlier: outlier[i], segment: segment[i] })).filter((r) => r.outlier);
  let group = [];
  const flush = () => {
    if (!group.length) return;
    const rule = [...new Set(group.flatMap((r) => r.outlier.split(",")))], metric = rule[0].replace(/^shift_/, "");
    windows.push({ years: [group[0].growing_year, group[group.length - 1].growing_year], scale: "year", rule: rule.join(","),
      value: median(group.map((r) => r[metric])), reference: reference[`${group[0].segment} ${metric}`] });
    group = [];
  };
  for (const r of flagged) { if (group.length && r.growing_year !== group[group.length - 1].growing_year + 1) flush(); group.push(r); }
  flush();
  return { screenable, changes, segment, outlier, windows };
}
// ts_screen_mask()
const screenMask = (time, windows) => time.map((t) => !windows.some((w) => t >= w.start && t <= w.end));

// ---------------------------------------------------------------- the conduction basis
bind("b-z"); bind("b-d");
function drawBasis() {
  const z = num("b-z"), d = num("b-d"), zd = z / d;
  const t = Array.from({ length: 6 * 48 }, (_, i) => i / 2 + 0.25), hour = t.map((x) => x % 24), day = t.map((x) => Math.floor(x / 24));
  const ta = t.map((x) => 18 + 6 * Math.cos(2 * Math.PI * (x - 15) / 24));
  const ts = t.map((x) => 17 + 6 * Math.exp(-zd) * Math.cos(2 * Math.PI * (x - 15) / 24 - zd));
  const dg = diurnalDiagnostics(day, hour, ts, ta, 38);
  document.getElementById("b-readout").innerHTML = `z/d = <b>${fmt(zd, 2)}</b>: range ratio <b>${fmt(Math.exp(-zd), 3)}</b>, true lag <b>${fmt(zd * C2PI, 2)} h</b>.
    The screen: range ratio ${fmt(dg.amp_ratio, 3)}, peak-hour difference wrapped into [−12, 12): ${fmt(dg.wrapped, 2)} h, unwrapped lag
    <b>${fmt(dg.lag_h, 2)} h</b>; implied depths ẑ<sub>amp</sub> ${fmt(dg.zd_amp, 2)}, ẑ<sub>lag</sub> ${fmt(dg.zd_lag, 2)}.`;
  Plotly.react("b-plot", [
    { x: t.slice(0, 96), y: ta.slice(0, 96), name: "air", line: { color: "#cf222e" } },
    { x: t.slice(0, 96), y: ts.slice(0, 96), name: `soil at ${z} cm`, line: { color: "#0969da", width: 2.5 } },
  ], baseLayout({ xaxis: { title: "hour", dtick: 6 }, yaxis: { title: "temperature (°C)" }, legend: { orientation: "h", y: -0.25 } }), PLOT_CONFIG);
}
for (const id of ["b-z", "b-d"]) document.getElementById(id).addEventListener("input", drawBasis);
drawBasis();
{
  const di = real.sensors[SENSORS[0]].diurnal;
  Plotly.newPlot("b-real", [{ x: di.map((r) => r.hour), y: di.map((r) => r.ta), name: "air", line: { color: "#cf222e" } },
    ...SENSORS.map((s, i) => ({ x: real.sensors[s].diurnal.map((r) => r.hour), y: real.sensors[s].diurnal.map((r) => r.ts), name: s,
      line: { color: BLUES[i], width: 2 } }))],
  baseLayout({ title: { text: `${real.site}: growing-season mean diurnal cycles`, font: { size: 13 } }, xaxis: { title: "hour (record's clock)", dtick: 6 },
    yaxis: { title: "temperature (°C)" }, legend: { orientation: "h", y: -0.25 } }), PLOT_CONFIG);
  const cols = ["amp_ratio", "lag_h", "zd_amp", "zd_lag", "stuck_frac", "coverage"];
  document.getElementById("b-table").innerHTML = `<table class="plain"><tr><th>sensor</th>${cols.map((c) => `<th>${c}</th>`).join("")}<th>ẑ<sub>lag</sub> / ẑ<sub>amp</sub></th><th>verdict</th><th>flags</th></tr>
    ${SENSORS.map((s) => { const r = real.sensors[s].summary[0]; return `<tr><td><code>${s}</code></td>${cols.map((c) => `<td class="num">${fmt(r[c], 3)}</td>`).join("")}
      <td class="num">${fmt(r.zd_lag / r.zd_amp, 2)}</td><td>${pill(r.verdict)}</td><td>${r.flags}</td></tr>`; }).join("")}</table>
    <p class="small">The R screen (<code>ts_screen_series()</code>) of each sensor's measured readings, ${real.site}, growing season fixed to day of year
    ${real.season[0]}–${real.season[1]}. The deepest sensor's lag is unwrapped beyond 24 h, as its range ratio implies a depth of
    ${fmt(real.sensors[SENSORS[4]].summary[0].zd_amp, 1)} damping depths.</p>`;
}

// ---------------------------------------------------------------- peak hour demo
bind("p-a2"); bind("p-noise");
let pSeed = 5;
function drawPeak() {
  const r = rng(pSeed), a2 = num("p-a2"), sdv = num("p-noise");
  const hour = Array.from({ length: 48 }, (_, i) => i / 2 + 0.25);
  const clean = hour.map((h) => Math.cos(2 * Math.PI * (h - 14) / 24) + a2 * Math.cos(4 * Math.PI * (h - 15.5) / 24));
  const v = clean.map((x) => x + sdv * r.norm());
  const hmax = hour[v.indexOf(Math.max(...v))], hp = peakHour(v, hour);
  Plotly.react("p-plot", [
    { x: hour, y: v, mode: "lines+markers", name: "mean diurnal cycle", marker: { size: 4 }, line: { color: "#0969da" } },
    { x: [hmax, hmax], y: [-2, 2.5], mode: "lines", name: `hour of maximum: ${fmt(hmax, 2)}`, line: { color: "#cf222e", dash: "dot" } },
    { x: [hp, hp], y: [-2, 2.5], mode: "lines", name: `first-harmonic peak: ${fmt(hp, 2)}`, line: { color: "#1a7f37", width: 2.5 } },
  ], baseLayout({ xaxis: { title: "hour", dtick: 3, range: [0, 24] }, yaxis: { title: "value (arbitrary units)", range: [-2, 2.5] },
    legend: { orientation: "h", y: -0.3 } }), PLOT_CONFIG);
}
for (const id of ["p-a2", "p-noise"]) document.getElementById(id).addEventListener("input", drawPeak);
document.getElementById("p-seed").addEventListener("click", () => { pSeed++; drawPeak(); });
drawPeak();

// ---------------------------------------------------------------- the plane
const NDAYS = 60;
const simAir = (() => {
  const r = rng(3), n = NDAYS * 48, wd = [], amp = [];
  let w = 0;
  for (let d = 0; d <= NDAYS; d++) { w = 0.7 * w + 2.5 * Math.sqrt(1 - 0.49) * r.norm(); wd.push(w); amp.push(5 + 1.5 * r.unif()); }
  const t = Array.from({ length: n }, (_, i) => i / 2 + 0.25), hour = t.map((x) => x % 24), day = t.map((x) => Math.floor(x / 24));
  const wt = t.map((x) => { const f = x / 24 - 0.5, i = Math.max(0, Math.floor(f)), g = Math.min(1, Math.max(0, f - i)); return wd[i] * (1 - g) + wd[i + 1] * g; });
  const ta = t.map((x, i) => 17 + wt[i] + amp[day[i]] * Math.cos(2 * Math.PI * (x - 15) / 24) + 0.3 * r.norm());
  return { t, hour, day, wt, amp, ta };
})();
function simSensor({ zd = 0, shift = 0, w = 0, air = false }) {
  const r = rng(17), A = simAir;
  return A.t.map((x, i) => {
    if (air) return A.ta[i] + 0.2 * r.norm();
    const tt = x - shift, dd = Math.max(0, Math.floor(tt / 24));
    const soil = 16 + A.wt[i] * Math.exp(-zd / 2.2) + A.amp[Math.min(dd, NDAYS - 1)] * Math.exp(-zd) * Math.cos(2 * Math.PI * (tt - 15) / 24 - zd) + 0.02 * r.norm();
    return (1 - w) * soil + w * (6 + 0.55 * A.ta[i]);
  });
}
const EXAMPLES = [
  ["shallow sensor (0.5 damping depths)", { zd: 0.5 }],
  ["sensor at 1.5 damping depths", { zd: 1.5 }],
  ["deep sensor (3 damping depths)", { zd: 3 }],
  ["very deep sensor (4.5 damping depths)", { zd: 4.5 }],
  ["sensor reporting air temperature", { air: true }],
  ["regression on air temperature (6 + 0.55 TA)", { w: 1 }],
  ["shallow sensor, time stamps 3 h early", { zd: 0.5, shift: -3 }],
];
const simDiag = (series) => diurnalDiagnostics(simAir.day, simAir.hour, series, simAir.ta, 38);
const examples = new Map(EXAMPLES.map(([name, p]) => { const s = simSensor(p); return [name, { series: s, diag: simDiag(s) }]; }));
bind("u-z"); bind("u-shift"); bind("u-w");
let selected = EXAMPLES[0][0];

function planeRegions() {
  const X0 = 0.005, X1 = 1.6, Y0 = -8, Y1 = 28, cond = (x) => -Math.log(x) * C2PI;
  const xs = Array.from({ length: 80 }, (_, i) => Math.exp(Math.log(X0) + (Math.log(Math.exp(-0.05)) - Math.log(X0)) * i / 79));
  const box = (x0, x1, y0, y1, color, name) => ({ x: [x0, x1, x1, x0, x0], y: [y0, y0, y1, y1, y0], fill: "toself", fillcolor: color,
    line: { width: 0 }, mode: "lines", name, hoverinfo: "name", showlegend: false });
  const label = (x, y, text, color) => ({ x: [x], y: [y], mode: "text", text: [text], textfont: { color, size: 12 }, hoverinfo: "skip", showlegend: false });
  return [
    { x: [...xs, ...[...xs].reverse(), xs[0]], y: [...xs.map((x) => Math.min(60, C.DEPTH_RATIO * cond(x))), ...[...xs].reverse().map((x) => cond(x) / C.DEPTH_RATIO), C.DEPTH_RATIO * cond(xs[0])],
      fill: "toself", fillcolor: "rgba(26,127,55,0.12)", line: { width: 0 }, mode: "lines", name: `conductive: depths agree within ×${C.DEPTH_RATIO}`, hoverinfo: "name", showlegend: false },
    box(X0, C.FLAT_AMP, Y0, Y1, "rgba(140,149,159,0.3)", `flat: range ratio < ${C.FLAT_AMP}`),
    box(X0, X1, Y0, C.LEADS_H, "rgba(207,34,46,0.15)", `leads air: lag < ${C.LEADS_H} h`),
    box(C.AIRLIKE_AMP, X1, -C.AIRLIKE_LAG, C.AIRLIKE_LAG, "rgba(191,135,0,0.45)", `air-like: ratio > ${C.AIRLIKE_AMP}, |lag| < ${C.AIRLIKE_LAG} h`),
    { x: xs.concat([1]), y: xs.map(cond).concat([0]), mode: "lines", line: { color: "#1a7f37", width: 2 }, name: "conduction: lag = −ln(ratio)·24/2π", hoverinfo: "name", showlegend: false },
    label(0.012, 24, "flat", "#57606a"), label(0.08, -5, "leads air", "#cf222e"), label(1.15, 2.2, "air-like", "#9a6700"),
    label(0.25, 14, "non-conductive", "#9a6700"), label(0.04, 1.2, "non-conductive", "#9a6700"), label(0.02, 15.5, "conduction", "#1a7f37"),
  ];
}
function drawPlane() {
  const user = simSensor({ zd: num("u-z"), shift: num("u-shift"), w: num("u-w") });
  examples.set("your sensor", { series: user, diag: simDiag(user) });
  const pts = [...examples.entries()];
  const color = (d) => VERDICT_COLORS[verdictOf(diurnalRules(d))];
  const traces = planeRegions();
  traces.push({ x: pts.map(([, e]) => e.diag.amp_ratio), y: pts.map(([, e]) => e.diag.lag_h), customdata: pts.map(([n]) => n), mode: "markers",
    marker: { size: pts.map(([n]) => (n === "your sensor" ? 16 : 11)), symbol: pts.map(([n]) => (n === "your sensor" ? "star" : "circle")),
      color: pts.map(([, e]) => color(e.diag)), line: { color: "#000", width: 1 } },
    hovertext: pts.map(([n, e]) => `${n}<br>ratio ${fmt(e.diag.amp_ratio, 3)}, lag ${fmt(e.diag.lag_h, 2)} h<br>${fired(diurnalRules(e.diag)).join(", ") || "no rule fires"}`),
    hoverinfo: "text", name: "simulated", showlegend: false });
  traces.push({ x: SENSORS.map((s) => real.sensors[s].summary[0].amp_ratio), y: SENSORS.map((s) => real.sensors[s].summary[0].lag_h), customdata: SENSORS,
    mode: "markers+text", text: SENSORS.map((s) => s.replace("TS_F_MDS_", "DE-Hai ")), textposition: "middle right", textfont: { size: 10 },
    marker: { size: 11, symbol: "diamond", color: SENSORS.map((s) => VERDICT_COLORS[real.sensors[s].summary[0].verdict]), line: { color: "#000", width: 1 } },
    hovertext: SENSORS.map((s) => { const r = real.sensors[s].summary[0]; return `${real.site} ${s} (R screen)<br>ratio ${fmt(r.amp_ratio, 3)}, lag ${fmt(r.lag_h, 2)} h<br>${r.verdict} ${r.flags}`; }),
    hoverinfo: "text", showlegend: false });
  Plotly.react("plane-plot", traces, baseLayout({ xaxis: { title: "range ratio (series / air)", type: "log", range: [Math.log10(0.005), Math.log10(1.6)] },
    yaxis: { title: "lag behind air (h)", range: [-8, 28], zeroline: true }, margin: { l: 60, r: 10, t: 20, b: 50 } }), PLOT_CONFIG);
  drawDetail();
}
function drawDetail() {
  let ts, ta, hours, title, text;
  if (SENSORS.includes(selected)) {
    const s = real.sensors[selected], r = s.summary[0];
    hours = s.diurnal.map((x) => x.hour); ts = s.diurnal.map((x) => x.ts); ta = s.diurnal.map((x) => x.ta);
    title = `${real.site} ${selected} (real; R screen)`;
    text = `range ratio <b>${fmt(r.amp_ratio, 3)}</b>, lag <b>${fmt(r.lag_h, 2)} h</b>, ẑ<sub>amp</sub> ${fmt(r.zd_amp, 2)}, ẑ<sub>lag</sub> ${fmt(r.zd_lag, 2)};
      R verdict ${pill(r.verdict)} ${r.flags ? `flags: <code>${r.flags}</code>` : ""}`;
  } else {
    const e = examples.get(selected), mts = byHour(e.series, simAir.hour), mta = byHour(simAir.ta, simAir.hour), rules = diurnalRules(e.diag);
    hours = mts.hours; ts = mts.mean; ta = mta.mean; title = `${selected} (simulated)`;
    text = `range ratio <b>${fmt(e.diag.amp_ratio, 3)}</b>, lag <b>${fmt(e.diag.lag_h, 2)} h</b> (wrapped ${fmt(e.diag.wrapped, 2)} h),
      ẑ<sub>amp</sub> ${fmt(e.diag.zd_amp, 2)}, ẑ<sub>lag</sub> ${fmt(e.diag.zd_lag, 2)}; diurnal rules: ${pill(verdictOf(rules))}
      ${fired(rules).length ? `<code>${fired(rules).join(",")}</code>` : "none fires"} <span class="small">(the stuck, coverage and yearly rules are not simulated here)</span>`;
  }
  Plotly.react("plane-detail", [{ x: hours, y: ta, name: "air", line: { color: "#cf222e" } }, { x: hours, y: ts, name: "series", line: { color: "#0969da", width: 2.5 } }],
    baseLayout({ title: { text: title, font: { size: 13 } }, xaxis: { title: "hour", dtick: 6 }, yaxis: { title: "mean (°C)" }, legend: { orientation: "h", y: -0.3 },
      margin: { l: 60, r: 10, t: 40, b: 50 } }), PLOT_CONFIG);
  document.getElementById("plane-text").innerHTML = text;
}
for (const id of ["u-z", "u-shift", "u-w"]) document.getElementById(id).addEventListener("input", () => { selected = "your sensor"; drawPlane(); });
drawPlane();
document.getElementById("plane-plot").on("plotly_click", (e) => { const c = e.points[0].customdata; if (c) { selected = c; drawDetail(); } });

// ---------------------------------------------------------------- stuck runs
{
  const r5 = real.sensors[SENSORS[4]].summary[0];
  document.getElementById("s-real").textContent = `${SENSORS[4]}, has ${fmt(100 * r5.stuck_frac, 1)}% of its values in runs (stuck_frac ${fmt(r5.stuck_frac, 3)} > ${C.STUCK_FRAC}) and is BAD as stuck (${r5.flags})`;
}
bind("s-snowt"); bind("s-days");
const SEASON = [120, 280];
function drawStuck() {
  const r = rng(23), n = 365 * 48, res = Number(document.getElementById("s-res").value), snow = document.getElementById("s-snow").checked;
  const snowT = num("s-snowt"), stuckDays = num("s-days");
  const doy = Array.from({ length: n }, (_, i) => Math.floor(i / 48) + 1), x = Array.from({ length: n }, (_, i) => i / 48 + 1);
  // weather: AR(1) of half-hourly steps with a 2-day time scale (sd 1.5 °C);
  // under snow, a slow drift (3-day time scale, sd 0.03 °C)
  const pw = Math.exp(-1 / 96), pd = Math.exp(-1 / 144);
  let w = 0, drift = 0;
  const raw = x.map((t, i) => {
    w = pw * w + 1.5 * Math.sqrt(1 - pw * pw) * r.norm();
    drift = pd * drift + 0.03 * Math.sqrt(1 - pd * pd) * r.norm();
    const d = doy[i];
    if (snow && (d <= 75 || d >= 335)) return snowT + drift;
    const season = 9 - 9 * Math.cos(2 * Math.PI * (d - 20) / 365);
    return season + w + 1.5 * (season / 18) * Math.cos(2 * Math.PI * ((t % 1) * 24 - 17) / 24);
  });
  const q = raw.map((v) => Number((Math.round(v / res) * res).toFixed(3)));
  const s0 = 189 * 48 + 20, s1 = Math.min(n, s0 + Math.round(stuckDays * 48));
  for (let i = s0 + 1; i < s1; i++) q[i] = q[s0];
  const season = doy.map((d) => d >= SEASON[0] && d <= SEASON[1]);
  const runs = tsRuns(q), counted = runs.filter((u) => u.length >= 6);
  const exemptRuns = tsRuns(q, 0.5, false).filter((u) => u.length >= 6 && Math.abs(q[u.first]) < 0.5);
  const stuckFrac = counted.reduce((a, u) => a + u.length, 0) / n;
  const stuckMax = Math.max(...runs.map((u) => u.length)) * 0.5;
  const long = runs.filter((u) => u.length * 0.5 >= C.STUCK_PERIOD_H);
  const inSeason = long.filter((u) => mean(season.slice(u.first, u.last + 1).map(Number)) > 0.5);
  const windows = [];
  for (const u of inSeason) {
    const prev = windows[windows.length - 1];
    if (prev && (u.first - prev.last) * 0.5 <= C.STUCK_MERGE_H) { prev.last = u.last; prev.hours += u.length * 0.5; }
    else windows.push({ first: u.first, last: u.last, hours: u.length * 0.5 });
  }
  const mark = (rr) => { const y = q.map(() => NaN); for (const u of rr) for (let i = u.first; i <= u.last; i++) y[i] = q[i]; return y; };
  const shapes = [{ type: "rect", xref: "x", yref: "paper", x0: SEASON[0], x1: SEASON[1] + 1, y0: 0, y1: 1, fillcolor: "rgba(26,127,55,0.07)", line: { width: 0 } },
    ...windows.map((u) => ({ type: "rect", xref: "x", yref: "paper", x0: x[u.first] - 0.5, x1: x[u.last] + 0.5, y0: 0, y1: 1, fillcolor: "rgba(207,34,46,0.3)", line: { width: 0 } }))];
  Plotly.react("s-plot", [
    { x, y: q, name: "soil temperature", line: { color: "#0969da", width: 0.8 } },
    { x, y: mark(counted), name: "runs ≥ 6 records (counted)", line: { color: "#fb8500", width: 3 } },
    { x, y: mark(exemptRuns), name: "runs within ±0.5 °C (exempt)", line: { color: "#57606a", width: 3 } },
  ], baseLayout({ shapes, xaxis: { title: "day of year" }, yaxis: { title: "soil temperature (°C)" }, legend: { orientation: "h", y: -0.25 } }), PLOT_CONFIG);
  const longOut = long.length - inSeason.length;
  document.getElementById("s-readout").innerHTML = `stuck_frac <b>${fmt(stuckFrac, 3)}</b> (${stuckFrac > C.STUCK_FRAC ? `&gt; ${C.STUCK_FRAC}: ${pill("BAD")} stuck` : `≤ ${C.STUCK_FRAC}: the stuck rule does not fire`});
    longest run (stuck_max_h) <b>${fmt(stuckMax, 1)} h</b>; runs of ≥ ${C.STUCK_PERIOD_H} h: ${long.length}, of which ${longOut} mostly outside the growing season (no window);
    stuck-period windows: <b>${windows.length}</b>${windows.length ? ` (${windows.map((u) => `day ${fmt(x[u.first], 1)}–${fmt(x[u.last], 1)}, ${fmt(u.hours, 1)} h`).join("; ")}): ${pill("SUSPECT")} stuck_periods` : ""}.`;
}
for (const id of ["s-snow", "s-snowt", "s-days", "s-res"]) document.getElementById(id).addEventListener("input", drawStuck);
drawStuck();

// ---------------------------------------------------------------- yearly screen
const ySel = document.getElementById("y-data");
for (const s of SENSORS) ySel.insertAdjacentHTML("beforeend", `<option value="${s}">${real.site} ${s} (R's yearly diagnostics)</option>`);
bind("y-n"); bind("y-k");
let ySeed = 41;
const SIM_IDS = ["y-n", "y-k", "y-seed", "y-odd", ...METRICS.flatMap((m) => [`y-s-${m}`, `y-n-${m}`])];
function yearlyData() {
  if (ySel.value !== "sim") return real.sensors[ySel.value].yearly.map((r) => ({ ...r }));
  const n = num("y-n"), k = Math.min(num("y-k"), n - 1), odd = num("y-odd"), r = rng(ySeed);
  document.getElementById("y-k").max = n - 1;
  const base = { offset: -2.5, log_amp: Math.log(0.2), lag_h: 2.5 }, oddShift = { offset: 2, log_amp: 0.5, lag_h: 2 };
  return Array.from({ length: n }, (_, i) => {
    const row = { growing_year: 2001 + i, n_amp_days: 80 };
    for (const m of METRICS) row[m] = base[m] + (i >= k ? num(`y-s-${m}`) : 0) + num(`y-n-${m}`) * r.norm() + (odd === i + 1 ? oddShift[m] : 0);
    return row;
  });
}
function drawYearly() {
  const isSim = ySel.value === "sim";
  for (const id of SIM_IDS) document.getElementById(id).disabled = !isSim;
  const Y = yearlyData(), res = yearlyScreen(Y);
  const years = Y.map((r) => r.growing_year);
  const titles = { offset: "offset (°C)", log_amp: "log_amp", lag_h: "lag_h (h)" };
  const traces = [], shapes = [];
  METRICS.forEach((m, j) => {
    const ax = j ? `${j + 1}` : "";
    traces.push({ x: years, y: Y.map((r) => r[m]), xaxis: `x${ax}`, yaxis: `y${ax}`, mode: "lines+markers", showlegend: false, line: { color: "#8c959f", width: 1 },
      marker: { size: 8, color: Y.map((r, i) => (res.outlier[i] ? "#cf222e" : !res.screenable[i] ? "#d0d7de" : "#0969da")) },
      hovertext: Y.map((r, i) => `${r.growing_year}: ${m} ${fmt(r[m], 3)}${res.outlier[i] ? `<br>${res.outlier[i]}` : ""}`), hoverinfo: "text" });
    for (const seg of [...new Set(res.segment)]) {
      const ys = years.filter((_, i) => res.segment[i] === seg && res.screenable[i]);
      if (!ys.length) continue;
      const med = median(Y.filter((_, i) => res.segment[i] === seg && res.screenable[i]).map((r) => r[m]));
      traces.push({ x: [ys[0] - 0.4, ys[ys.length - 1] + 0.4], y: [med, med], xaxis: `x${ax}`, yaxis: `y${ax}`, mode: "lines", showlegend: false,
        line: { color: "#1a7f37", width: 2 }, hoverinfo: "skip" });
    }
  });
  for (const c of res.changes) shapes.push({ type: "line", xref: "x", yref: "paper", x0: c.year - 0.5, x1: c.year - 0.5, y0: 0, y1: 1, line: { color: "#cf222e", dash: "dash" } });
  for (const w of res.windows) {
    shapes.push({ type: "rect", xref: "x", yref: "paper", x0: w.years[0] - 0.5, x1: w.years[1] + 0.5, y0: 0, y1: 1, fillcolor: "rgba(207,34,46,0.1)", line: { width: 0 } });
  }
  const dom = [[0.7, 1], [0.37, 0.63], [0, 0.3]];
  const layout = { shapes, showlegend: false, margin: { l: 60, r: 10, t: 20, b: 50 } };
  METRICS.forEach((m, j) => {
    const ax = j ? `${j + 1}` : "";
    layout[`xaxis${ax}`] = { anchor: `y${ax}`, matches: j ? "x" : undefined, showticklabels: j === 2, title: j === 2 ? "growing year" : undefined };
    layout[`yaxis${ax}`] = { domain: dom[j], anchor: `x${ax}`, title: titles[m] };
  });
  Plotly.react("y-series", traces, baseLayout(layout), PLOT_CONFIG);

  const scan = changepoint(Y.filter((_, i) => res.screenable[i])).scan;
  const colors = { offset: "#8250df", log_amp: "#0969da", lag_h: "#bf8700" };
  const st = METRICS.map((m) => ({ x: scan.map((s) => s.year), y: scan.map((s) => Math.max(0.01, s.split.find((p) => p.metric === m).score)), name: m, mode: "lines+markers",
    line: { color: colors[m] }, marker: { size: 9, color: colors[m], symbol: scan.map((s) => (s.split.find((p) => p.metric === m).agrees ? "circle" : "circle-open")) },
    hovertext: scan.map((s) => { const p = s.split.find((q) => q.metric === m); return `split before ${s.year}: ${m} D = ${fmt(p.d, 3)}, score ${fmt(p.score, 2)}${p.agrees ? "" : " (below the minimum shift)"}`; }),
    hoverinfo: "text" }));
  st.push({ x: scan.filter((s) => s.counts).map((s) => s.year), y: scan.filter((s) => s.counts).map((s) => s.score), mode: "markers", name: "split counts (summed score)",
    marker: { symbol: "star", size: 14, color: "#cf222e" } });
  Plotly.react("y-scores", st, baseLayout({ xaxis: { title: "first year of the later segment" }, yaxis: { title: "score |D| / S", type: "log" },
    shapes: [{ type: "line", xref: "paper", x0: 0, x1: 1, yref: "y", y0: C.YEAR_Z, y1: C.YEAR_Z, line: { color: "#000", dash: "dot" } }],
    legend: { orientation: "h", y: -0.2 }, margin: { l: 60, r: 10, t: 20, b: 50 } }), PLOT_CONFIG);

  const nScreen = res.screenable.filter(Boolean).length;
  const changeText = res.changes.length ? res.changes.map((c) => `<b>${c.year}</b> (${c.metric}; ${fmt(c.before, 2)} → ${fmt(c.after, 2)}, score ${fmt(c.score, 1)})`).join(", ")
    : nScreen < C.CHANGE_MIN_YEARS ? `none (only ${nScreen} screenable years, fewer than ${C.CHANGE_MIN_YEARS})` : "none";
  const odd = Y.filter((_, i) => res.outlier[i]).map((r) => `${r.growing_year} (${res.outlier[Y.indexOf(r)]})`);
  const flags = [res.changes.length ? "changepoint" : "", res.windows.some((w) => w.scale === "year") ? "bad_years" : ""].filter(Boolean);
  let html = `Changepoints: ${changeText}. Odd years: ${odd.length ? odd.join(", ") : "none"}.
    ${flags.length ? `${pill("SUSPECT")} <code>${flags.join(",")}</code>` : "No yearly flag."}`;
  if (res.windows.length) {
    html += `<table class="plain"><tr><th>years</th><th>scale</th><th>rule</th><th>value</th><th>reference</th></tr>${res.windows.map((w) =>
      `<tr><td>${w.years[0]}–${w.years[1]}</td><td>${w.scale}</td><td><code>${w.rule}</code></td><td class="num">${fmt(w.value, 3)}</td><td class="num">${fmt(w.reference, 3)}</td></tr>`).join("")}</table>`;
  }
  if (!isSim) {
    const s = real.sensors[ySel.value];
    const rOdd = s.yearly.filter((r) => r.outlier).map((r) => `${r.growing_year} (${r.outlier})`);
    const rSeg = [...new Set(s.yearly.map((r) => r.segment))].length;
    const rWin = (s.windows || []).filter((w) => w.scale !== "period");
    html += `<br><span class="small">The R screen of the same data: ${rSeg} segment${rSeg > 1 ? "s" : ""}, odd years ${rOdd.length ? rOdd.join(", ") : "none"},
      ${rWin.length} changepoint/year window${rWin.length === 1 ? "" : "s"}${rWin.length ? ` (${rWin.map((w) => `${w.start.slice(0, 4)}–${w.end.slice(0, 4)} ${w.rule}`).join("; ")})` : ""}.</span>`;
  }
  document.getElementById("y-readout").innerHTML = html;
}
for (const id of SIM_IDS.filter((i) => i !== "y-seed")) document.getElementById(id).addEventListener("input", drawYearly);
ySel.addEventListener("change", drawYearly);
document.getElementById("y-seed").addEventListener("click", () => { ySeed++; drawYearly(); });
drawYearly();

// ---------------------------------------------------------------- rules table
{
  const dev = C.YEAR_MIN_DEV;
  const rows = [
    ["airlike", "BAD", `amp_ratio &gt; ${C.AIRLIKE_AMP} and |lag_h| &lt; ${C.AIRLIKE_LAG} h`, "reports air temperature, or sits at the surface"],
    ["flat", "BAD", `amp_ratio &lt; ${C.FLAT_AMP}`, "no daily cycle"],
    ["stuck", "BAD", `stuck_frac &gt; ${C.STUCK_FRAC}`, "share of the values in runs of ≥ 6 equal values (outside ±0.5 °C), whole record"],
    ["leads", "BAD", `lag_h &lt; ${C.LEADS_H} h`, "daily peak before air's"],
    ["coverage", "BAD", `coverage &lt; ${C.COVERAGE_MIN}, or no value at all`, "share of the record with a value"],
    ["nonconductive", "SUSPECT", `zd_amp &gt; 0.05 and (zd_lag ≤ 0, or zd_lag / zd_amp outside [1/${C.DEPTH_RATIO}, ${C.DEPTH_RATIO}])`,
      "damping and delay imply different depths (e.g. a regression on air temperature: damped, not delayed)"],
    ["changepoint", "SUSPECT", `a changepoint window: ≥ ${C.MIN_METRICS} of offset, log_amp, lag_h shift (score &gt; ${C.YEAR_Z}; |D| &gt; ${dev.offset} °C, ${dev.log_amp}, ${dev.lag_h} h),
      ≥ ${C.CHANGE_MIN_YEARS} screenable years, ≥ ${C.CHANGE_MIN_SIDE} per side`, "the record before the latest changepoint"],
    ["bad_years", "SUSPECT", `an odd-year window: ≥ ${C.MIN_METRICS} metrics deviate from the segment median by more than their minimum shift and ${C.YEAR_Z} spreads`, "runs of odd growing years"],
    ["stuck_periods", "SUSPECT", `a run of ≥ ${C.STUCK_PERIOD_H} h, mostly in the growing season (runs within ${C.STUCK_MERGE_H} h merged)`, "a stuck logger in the season"],
  ];
  document.getElementById("rules-table").innerHTML = `<table class="plain"><tr><th>flag</th><th>level</th><th>fires if</th><th>meaning</th></tr>
    ${rows.map(([f, l, rule, what]) => `<tr><td><code>${f}</code></td><td>${pill(l)}</td><td>${rule}</td><td>${what}</td></tr>`).join("")}</table>
    <p class="small">Thresholds read from the <code>TS_SCREEN_*</code> constants of <code>R/ts-screen.R</code>; the 0.05 damping floor, the 6-record run
    length and the ±0.5 °C zero band are written in the code itself. Missing diagnostics never fire a rule (R's <code>isTRUE()</code>).</p>`;
  document.getElementById("real-verdicts").innerHTML = `<table class="plain"><tr><th>${real.site} sensor</th><th>verdict</th><th>flags</th><th>bad_fraction</th></tr>
    ${SENSORS.map((s) => { const r = real.sensors[s].summary[0]; return `<tr><td><code>${s}</code></td><td>${pill(r.verdict)}</td><td><code>${r.flags}</code></td><td class="num">${fmt(r.bad_fraction, 3)}</td></tr>`; }).join("")}</table>`;
}

// ---------------------------------------------------------------- windows and the mask
{
  const W = [
    { site_ID: "XX-Toy", column: "TS_original", start: "2010-01-01 00:15:00", end: "2011-12-31 23:45:00", scale: "changepoint", rule: "changepoint_offset,changepoint_lag_h", value: -0.62, reference: -2.41 },
    { site_ID: "XX-Toy", column: "TS_original", start: "2014-01-01 00:15:00", end: "2014-12-31 23:45:00", scale: "year", rule: "shift_log_amp,shift_lag_h", value: -0.35, reference: -1.45 },
    { site_ID: "XX-Toy", column: "TS_original", start: "2015-07-02 10:15:00", end: "2015-07-12 08:45:00", scale: "period", rule: "stuck", value: 239, reference: C.STUCK_PERIOD_H },
  ];
  const cols = Object.keys(W[0]);
  document.getElementById("w-csv").textContent = [cols.map((c) => `"${c}"`).join(","),
    ...W.map((w) => cols.map((c) => (typeof w[c] === "number" ? w[c] : `"${w[c]}"`)).join(","))].join("\n");
  const parse = (s) => new Date(`${s.replace(" ", "T")}Z`);
  const windows = W.map((w) => ({ start: parse(w.start), end: parse(w.end) }));
  const time = [], y = [];
  for (let t = Date.UTC(2010, 0, 1, 12, 15); t < Date.UTC(2017, 0, 1); t += 864e5) {
    const d = new Date(t), doy = (t - Date.UTC(d.getUTCFullYear(), 0, 1)) / 864e5;
    time.push(d); y.push(9 - 9 * Math.cos(2 * Math.PI * (doy - 20) / 365) + (d.getUTCFullYear() < 2012 ? 2 : 0));
  }
  const keep = screenMask(time, windows);
  Plotly.newPlot("w-plot", [
    { x: time, y: y.map((v, i) => (keep[i] ? v : NaN)), name: "kept (mask TRUE)", line: { color: "#0969da" } },
    { x: time, y: y.map((v, i) => (keep[i] ? NaN : v)), name: "in a bad window (mask FALSE)", line: { color: "#cf222e" } },
  ], baseLayout({ xaxis: { title: "time" }, yaxis: { title: "toy daily soil temperature (°C)" }, legend: { orientation: "h", y: -0.3 },
    shapes: windows.map((w) => ({ type: "rect", xref: "x", yref: "paper", x0: w.start, x1: w.end, y0: 0, y1: 1, fillcolor: "rgba(207,34,46,0.12)", line: { width: 0 } })) }),
  PLOT_CONFIG);
  const out = keep.filter((k) => !k).length;
  document.getElementById("w-readout").innerHTML = `<code>ts_screen_mask(timestamp, windows)</code> on daily records 2010–2016: ${out} of ${keep.length} records in a window,
    so <code>bad_fraction</code> = ${fmt(out / keep.length, 3)} (the share of the series' non-missing records inside windows).`;
}
