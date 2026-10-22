// Documentation: the candidate soil-temperature columns, the estimators, their
// cross-validation and ranking (R/ts-columns.R, R/ts-ranking.R). Static: reads
// only data/code.json (the R code) and static/docs/ (raw-data examples).
import { header, loadJSON, baseLayout, PLOT_CONFIG, fmt } from "./common.js";
import { loadCode, renderSnippets, renderConstants, rConstant, rStrings, escapeHTML, rng, mean, sd, cor, lstsq, rollMean,
  num, finite, CYCLIC } from "./docs-common.js";

header("docs-ts-estimators.html");
const code = await loadCode();
renderSnippets(code);
renderConstants(code);

const bind = (id, fmtv = (v) => v) => {
  const el = document.getElementById(id), out = document.getElementById(`${id}-o`);
  const show = () => { if (out) out.textContent = fmtv(el.value); };
  show();
  el.addEventListener("input", show);
  return el;
};
const rmse = (p, o) => { let s = 0, n = 0; for (let i = 0; i < p.length; i++) if (finite(p[i]) && finite(o[i])) { s += (p[i] - o[i]) ** 2; n++; } return Math.sqrt(s / n); };

// ---------------------------------------------------------------- memory features
// ts_memory_features() on records grouped into days (dayIndex: consecutive
// calendar days from 0), with yday and the time of day of each record.
function memoryFeatures(ta, dayIndex, yday, hour) {
  const nd = Math.max(...dayIndex) + 1;
  const byDay = Array.from({ length: nd }, () => []);
  ta.forEach((v, i) => byDay[dayIndex[i]].push(v));
  const day = byDay.map((v) => mean(v));
  const amp = byDay.map((v) => { const w = v.filter(finite); return w.length >= 4 ? Math.max(...w) - Math.min(...w) : NaN; });
  const m3 = rollMean(day, 3), m7 = rollMean(day, 7), m30 = rollMean(day, 30);
  return ta.map((v, i) => {
    const d = dayIndex[i], a = 2 * Math.PI * yday[i] / 365.25, h = 2 * Math.PI * hour[i] / 24;
    return [v, day[d], d > 0 ? day[d - 1] : NaN, d > 0 ? amp[d - 1] : NaN, m3[d], m7[d], m30[d],
      Math.sin(a), Math.cos(a), Math.sin(2 * a), Math.cos(2 * a), Math.sin(h), Math.cos(h)];
  });
}

// ---------------------------------------------------------------- conduction simulation
const YEAR_H = 8760, NYEARS = 3;
const weather = (() => {
  const r = rng(7);
  return Array.from({ length: 12 }, () => ({ P: 24 * Math.exp(Math.log(2) + r.unif() * Math.log(15)), psi: 2 * Math.PI * r.unif() }));
})();
function conduction(zcm, kappa, wsd) {
  const z = zcm / 100, k = kappa * 1e-6, aw = wsd * Math.sqrt(2 / weather.length);
  const comps = [{ A: 9, P: 365.25 * 24, psi: 2 * Math.PI * (15 * 24) / (365.25 * 24) + Math.PI },
    { A: 5, P: 24, psi: 2 * Math.PI * 15 / 24 }, ...weather.map((w) => ({ A: aw, P: w.P, psi: w.psi }))];
  for (const c of comps) c.d = Math.sqrt(2 * k / (2 * Math.PI / (c.P * 3600)));
  const n = NYEARS * YEAR_H, ta = new Float64Array(n), ts = new Float64Array(n);
  for (let t = 0; t < n; t++) {
    let a = 9, s = 9;
    for (const c of comps) {
      const ph = 2 * Math.PI * t / c.P - c.psi;
      a += c.A * Math.cos(ph);
      s += c.A * Math.exp(-z / c.d) * Math.cos(ph - z / c.d);
    }
    ta[t] = a; ts[t] = s;
  }
  return { ta: Array.from(ta), ts: Array.from(ts), dDay: comps[1].d, dYear: comps[0].d };
}
const MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
const MONTH_DOY = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];
const monthSel = document.getElementById("c-month");
MONTHS.forEach((m, i) => monthSel.insertAdjacentHTML("beforeend", `<option value="${i}"${i === 3 ? " selected" : ""}>${m} (4 weeks)</option>`));
const cz = bind("c-z"), ck = bind("c-k"), cw = bind("c-w");
const simHours = Array.from({ length: NYEARS * YEAR_H }, (_, t) => t);
const simDay = simHours.map((t) => Math.floor(t / 24));
const simYday = simDay.map((d) => (d % 365) + 1);
const simHour = simHours.map((t) => t % 24);

function drawConduction() {
  const s = conduction(num("c-z"), num("c-k"), num("c-w"));
  const zd = num("c-z") / 100 / s.dDay;
  // linear (TA > 0) and lm_memory, in sample
  const pos = s.ta.map((v, i) => i).filter((i) => s.ta[i] > 0);
  const lin = lstsq(pos.map((i) => [s.ta[i]]), pos.map((i) => s.ts[i]));
  const predLin = s.ta.map((v) => lin.predict([v]));
  const F = memoryFeatures(s.ta, simDay, simYday, simHour);
  const ok = F.map((f, i) => i).filter((i) => F[i].every(finite));
  const mem = lstsq(ok.map((i) => F[i]), ok.map((i) => s.ts[i]));
  const predMem = F.map((f) => (f.every(finite) ? mem.predict(f) : NaN));
  document.getElementById("c-readout").innerHTML =
    `Daily cycle at ${num("c-z")} cm: range × <b>${fmt(Math.exp(-zd), 2)}</b>, peak <b>${fmt(zd * 24 / (2 * Math.PI), 1)} h</b> later
    (damping depth ${fmt(100 * s.dDay, 1)} cm). Seasonal cycle: amplitude × <b>${fmt(Math.exp(-num("c-z") / 100 / s.dYear), 2)}</b>,
    <b>${fmt(num("c-z") / 100 / s.dYear * 365.25 / (2 * Math.PI), 1)} days</b> later (damping depth ${fmt(s.dYear, 2)} m).
    Hourly RMSE: <code>linear</code> <b>${fmt(rmse(predLin, s.ts), 2)} °C</b>, <code>lm_memory</code> <b>${fmt(rmse(predMem, s.ts), 2)} °C</b>.`;
  const start = YEAR_H + MONTH_DOY[num("c-month")] * 24, idx = Array.from({ length: 28 * 24 }, (_, i) => start + i);
  const x = idx.map((t) => (t - YEAR_H) / 24 + 1);
  Plotly.react("c-series", [
    { x, y: idx.map((t) => s.ta[t]), name: "air", line: { color: "#cf222e", width: 1 } },
    { x, y: idx.map((t) => s.ts[t]), name: `soil, ${num("c-z")} cm`, line: { color: "#0969da", width: 2 } },
    { x, y: idx.map((t) => predLin[t]), name: "linear (TS ~ TA, TA > 0)", line: { color: "#8c959f", width: 1, dash: "dot" } },
  ], baseLayout({ xaxis: { title: "day of year (simulated year 2)" }, yaxis: { title: "temperature (°C)" },
    legend: { orientation: "h", y: -0.25 } }), PLOT_CONFIG);
  const days = Array.from({ length: 365 }, (_, d) => 365 + d);
  const dm = (v) => days.map((d) => mean(v.slice(d * 24, d * 24 + 24)));
  const taD = dm(s.ta), tsD = dm(s.ts);
  const lo = Math.min(...taD), hi = Math.max(...taD);
  Plotly.react("c-loop", [
    { x: taD, y: tsD, mode: "markers", name: "daily means", marker: { color: days.map((d) => d - 364), colorscale: CYCLIC, cmin: 1, cmax: 365,
      size: 5, colorbar: { title: { text: "day of year" }, thickness: 12 } }, text: days.map((d) => `day ${d - 364}`), hoverinfo: "text+x+y" },
    { x: [lo, hi], y: [lin.predict([lo]), lin.predict([hi])], mode: "lines", name: "linear", line: { color: "#000", dash: "dot" } },
  ], baseLayout({ xaxis: { title: "air temperature, daily mean (°C)" }, yaxis: { title: "soil temperature, daily mean (°C)" },
    showlegend: false }), PLOT_CONFIG);
}
for (const el of [cz, ck, cw, monthSel]) el.addEventListener("change", drawConduction);
for (const el of [cz, ck, cw]) el.addEventListener("input", drawConduction);
drawConduction();

// ---------------------------------------------------------------- DE-Hai
const hh = await loadJSON("static/docs/de-hai-halfhourly.json");
const daily = await loadJSON("static/docs/de-hai-daily.json");
const SENSORS = ["TS_F_MDS_1", "TS_F_MDS_2", "TS_F_MDS_3", "TS_F_MDS_4", "TS_F_MDS_5"];
const blues = ["#08306b", "#2171b5", "#4292c6", "#6baed6", "#9ecae1"];
Plotly.newPlot("r-series", [{ x: hh.time, y: hh.TA, name: "air (TA_F_MDS)", line: { color: "#cf222e", width: 1 } },
  ...SENSORS.map((s, i) => ({ x: hh.time, y: hh[s], name: s, line: { color: blues[i], width: 1.5 } }))],
baseLayout({ title: { text: `${hh.site}, half-hourly, June 2016`, font: { size: 13 } }, xaxis: { title: "time" },
  yaxis: { title: "temperature (°C)" }, legend: { orientation: "h", y: -0.25 } }), PLOT_CONFIG);

const dYday = daily.date.map((d) => { const t = new Date(`${d}T00:00:00Z`); return Math.floor((t - Date.UTC(t.getUTCFullYear(), 0, 1)) / 864e5) + 1; });
const dYear = daily.date.map((d) => Number(d.slice(0, 4)));
const dTA7 = rollMean(daily.TA, 7), dTA30 = rollMean(daily.TA, 30);
function drawRealLoop() {
  const s = document.getElementById("r-sensor").value;
  const ok = daily.TA.map((v, i) => i).filter((i) => finite(daily[s][i]) && finite(daily.TA[i]));
  const pos = ok.filter((i) => daily.TA[i] > 0);
  const lin = lstsq(pos.map((i) => [daily.TA[i]]), pos.map((i) => daily[s][i]));
  const feat = (i) => [daily.TA[i], dTA7[i], dTA30[i], Math.sin(2 * Math.PI * dYday[i] / 365.25), Math.cos(2 * Math.PI * dYday[i] / 365.25)];
  const mem = lstsq(ok.map(feat), ok.map((i) => daily[s][i]));
  document.getElementById("r-readout").innerHTML = `RMSE of daily means, in sample: TS ~ TA (TA &gt; 0) ${fmt(rmse(ok.map((i) => lin.predict([daily.TA[i]])), ok.map((i) => daily[s][i])), 2)} °C;
    least squares on daily TA, its 7- and 30-day means and the annual harmonic ${fmt(rmse(ok.map((i) => mem.predict(feat(i))), ok.map((i) => daily[s][i])), 2)} °C`;
  const lo = Math.min(...daily.TA.filter(finite)), hi = Math.max(...daily.TA.filter(finite));
  Plotly.react("r-loop", [
    { x: ok.map((i) => daily.TA[i]), y: ok.map((i) => daily[s][i]), mode: "markers", text: ok.map((i) => daily.date[i]), hoverinfo: "text+x+y",
      marker: { color: ok.map((i) => dYday[i]), colorscale: CYCLIC, cmin: 1, cmax: 366, size: 4, opacity: 0.7,
        colorbar: { title: { text: "day of year" }, thickness: 12 } } },
    { x: [lo, hi], y: [lin.predict([lo]), lin.predict([hi])], mode: "lines", line: { color: "#000", dash: "dot" } },
  ], baseLayout({ title: { text: `${daily.site} daily means ${dYear[0]}–${dYear[dYear.length - 1]}, ${s}`, font: { size: 13 } },
    xaxis: { title: "air temperature (°C)" }, yaxis: { title: "soil temperature (°C)" }, showlegend: false }), PLOT_CONFIG);
}
document.getElementById("r-sensor").addEventListener("change", drawRealLoop);
drawRealLoop();

// memory features of DE-Hai, 2014
{
  const idx = daily.date.map((d, i) => i).filter((i) => dYear[i] === 2014);
  const x = idx.map((i) => daily.date[i]);
  Plotly.newPlot("m-plot", [
    { x, y: idx.map((i) => daily.TA[i]), name: "TA_day", line: { color: "#cf222e", width: 1 } },
    { x, y: idx.map((i) => dTA7[i]), name: "TA_m7", line: { color: "#bf8700", width: 1.5 } },
    { x, y: idx.map((i) => dTA30[i]), name: "TA_m30", line: { color: "#8250df", width: 2 } },
    { x, y: idx.map((i) => daily.TS_F_MDS_1[i]), name: "soil (TS_F_MDS_1), daily mean", line: { color: "#0969da", width: 2.5 } },
  ], baseLayout({ title: { text: "DE-Hai 2014: memory features and the shallowest sensor", font: { size: 13 } },
    xaxis: { title: "date" }, yaxis: { title: "temperature (°C)" }, legend: { orientation: "h", y: -0.2 } }), PLOT_CONFIG);
}

// ---------------------------------------------------------------- estimators table
{
  const body = code.ts_estimators.code;
  const freezing = (body.match(/above_freezing <- function\(d\) (.+)$/m) || [, "?"])[1].replace(/d\$/g, "");
  const memory = rStrings(code, "TS_MEMORY_PREDICTORS");
  const predictors = (expr) => {
    const parts = [];
    for (const m of expr.matchAll(/"([^"]+)"|(TS_MEMORY_PREDICTORS)/g)) {
      parts.push(m[1] ? `<code>${m[1]}</code>` : `the ${memory.length} memory predictors (<code>TS_MEMORY_PREDICTORS</code>)`);
    }
    return parts.join(" + ");
  };
  const subset = (s) => (s === "NULL" ? "all real readings"
    : `real readings with <code>${escapeHTML(s.replace(/^function\(d\) /, "").replace(/above_freezing\(d\)|^above_freezing$/g, `(${freezing})`).replace(/d\$/g, ""))}</code>`);
  const rows = [...body.matchAll(/^\s+(\w+) = list\(kind = "(\w+)", predictors = (.+), subset = (.+)\),?$/gm)];
  document.getElementById("est-table").innerHTML = `<table class="plain"><tr><th>estimator</th><th>column</th><th>model</th><th>predictors</th><th>trained on</th></tr>
    ${rows.map(([, name, kind, pred, sub]) => `<tr><td><code>${name}</code></td><td><code>TS_${name}</code></td>
      <td>${kind === "rf" ? "random forest (ranger)" : "least squares (lm)"}</td><td>${predictors(pred)}</td><td>${subset(sub)}</td></tr>`).join("")}</table>
    <p class="small">The memory predictors: ${memory.map((m) => `<code>${m}</code>`).join(", ")}. <code>NETRAD</code> is the site's net
    radiation (<code>site_settings.csv</code> <code>netrad</code>), gap-filled like air temperature; where a site has none, the
    estimators using it have no predictions.</p>`;
}

// ---------------------------------------------------------------- cross-validation simulation
const cvTau = bind("cv-tau"), cvSig = bind("cv-sig"), cvK = bind("cv-k");
let cvSeed = 11;
function knnPredict(F, y, train, test, k) {
  const out = new Map();
  for (const t of test) {
    const bd = [], bv = [];
    for (const r of train) {
      let d = 0;
      for (let j = 0; j < F[t].length; j++) d += (F[t][j] - F[r][j]) ** 2;
      if (bd.length < k || d < bd[bd.length - 1]) {
        let p = bd.length < k ? bd.length : k - 1;
        if (bd.length < k) { bd.push(0); bv.push(0); }
        while (p > 0 && bd[p - 1] > d) { bd[p] = bd[p - 1]; bv[p] = bv[p - 1]; p--; }
        bd[p] = d; bv[p] = y[r];
      }
    }
    out.set(t, mean(bv));
  }
  return out;
}
function lsPredict(F, y, train, test) {
  const m = lstsq(train.map((i) => F[i]), train.map((i) => y[i]));
  return new Map(test.map((t) => [t, m.predict(F[t])]));
}
function cvData(kind) {
  if (kind === "real") {
    const rows = daily.TA.map((v, i) => i).filter((i) => finite(daily.TS_F_MDS_1[i]) && finite(dTA30[i]));
    return { date: rows.map((i) => daily.date[i]), year: rows.map((i) => dYear[i]), ta: rows.map((i) => daily.TA[i]), m7: rows.map((i) => dTA7[i]), m30: rows.map((i) => dTA30[i]),
      yday: rows.map((i) => dYday[i]), ts: rows.map((i) => daily.TS_F_MDS_1[i]), anomaly: null, train: [...new Set(rows.map((i) => dYear[i]))], test: [] };
  }
  const r = rng(cvSeed), years = 12, n = years * 365, tau = num("cv-tau"), sig = num("cv-sig");
  const ta = [], ts = [], an = [];
  let w = 0, m = 9, a = 0;
  const pw = Math.exp(-1 / 3), pa = Math.exp(-1 / tau);
  for (let d = 0; d < n; d++) {
    w = pw * w + 3 * Math.sqrt(1 - pw * pw) * r.norm();
    a = pa * a + sig * Math.sqrt(1 - pa * pa) * r.norm();
    const t = 9 - 9 * Math.cos(2 * Math.PI * ((d % 365) - 15) / 365) + w;
    m += (t - m) / 10;
    ta.push(t); an.push(a); ts.push(1 + 0.9 * m + a + 0.2 * r.norm());
  }
  const year = ta.map((_, d) => 1 + Math.floor(d / 365));
  return { year, ta, m7: rollMean(ta, 7), m30: rollMean(ta, 30), yday: ta.map((_, d) => (d % 365) + 1), ts, anomaly: an,
    train: Array.from({ length: 8 }, (_, i) => i + 1), test: [9, 10, 11, 12] };
}
function drawCV() {
  const kind = document.getElementById("cv-data").value;
  for (const id of ["cv-tau", "cv-sig", "cv-seed"]) document.getElementById(id).disabled = kind === "real";
  const D = cvData(kind), k = num("cv-k");
  const raw = D.ta.map((_, i) => [D.ta[i], D.m7[i], D.m30[i], Math.sin(2 * Math.PI * D.yday[i] / 365.25), Math.cos(2 * Math.PI * D.yday[i] / 365.25)]);
  const trainRows = raw.map((_, i) => i).filter((i) => D.train.includes(D.year[i]));
  const testRows = raw.map((_, i) => i).filter((i) => D.test.includes(D.year[i]));
  const mu = raw[0].map((_, j) => mean(trainRows.map((i) => raw[i][j]))), s = raw[0].map((_, j) => sd(trainRows.map((i) => raw[i][j])));
  const F = raw.map((r) => r.map((v, j) => (v - mu[j]) / s[j]));
  const models = { "least squares": (tr, te) => lsPredict(F, D.ts, tr, te), [`${k} nearest neighbours`]: (tr, te) => knnPredict(F, D.ts, tr, te, k) };
  const r = rng(cvSeed + 1);
  const shuffled = trainRows.map((i) => [r.unif(), i]).sort((a, b) => a[0] - b[0]).map((x) => x[1]);
  const nf = D.train.length;
  const randomFolds = Array.from({ length: nf }, (_, f) => shuffled.filter((_, j) => j % nf === f));
  const yearFolds = D.train.map((y) => trainRows.filter((i) => D.year[i] === y));
  const oof = (fit, folds) => {
    const p = new Map();
    for (const fold of folds) {
      const held = new Set(fold);
      for (const [t, v] of fit(trainRows.filter((i) => !held.has(i)), fold)) p.set(t, v);
    }
    return p;
  };
  const err = (p, rows) => rmse(rows.map((i) => p.get(i)), rows.map((i) => D.ts[i]));
  const res = {}, keep = {};
  for (const [name, fit] of Object.entries(models)) {
    const pr = oof(fit, randomFolds), py = oof(fit, yearFolds);
    res[name] = { random: err(pr, trainRows), year: err(py, trainRows), fresh: testRows.length ? err(fit(trainRows, testRows), testRows) : null };
    keep[name] = { pr, py };
  }
  const names = Object.keys(models);
  const bars = [["random", "random split (8 folds of days)", "#bf8700"], ["year", "leave one year out", "#0969da"]];
  if (testRows.length) bars.push(["fresh", "new years (true error)", "#1a7f37"]);
  Plotly.react("cv-bars", bars.map(([key, label, color]) => ({ type: "bar", name: label, x: names, y: names.map((n) => res[n][key]),
    marker: { color }, text: names.map((n) => fmt(res[n][key], 2)), textposition: "outside" })),
  baseLayout({ barmode: "group", yaxis: { title: "RMSE of daily means (°C)", rangemode: "tozero" }, xaxis: { title: "model" },
    legend: { orientation: "h", y: -0.25 } }), PLOT_CONFIG);
  const [a, b] = names, by = (key) => (res[a][key] < res[b][key] ? a : b);
  document.getElementById("cv-readout").innerHTML = `Better model by the random split: <b>${by("random")}</b>; by leave one year out:
    <b>${by("year")}</b>${testRows.length ? `; on new years (the truth): <b>${by("fresh")}</b>` : ""}.
    The random split flatters the nearest-neighbour model by ${fmt(100 * (1 - res[b].random / res[b].year), 0)}% of its leave-one-year-out RMSE,
    least squares by ${fmt(100 * (1 - res[a].random / res[a].year), 0)}%.`;
  const showYear = kind === "real" ? 2014 : 3;
  const rows = trainRows.filter((i) => D.year[i] === showYear);
  const knn = keep[names[1]];
  const x = rows.map((i) => (kind === "real" ? D.date[i] : D.yday[i]));
  const traces = [
    { x, y: rows.map((i) => D.ts[i]), name: "sensor", line: { color: "#000", width: 2 } },
    { x, y: rows.map((i) => knn.pr.get(i)), name: "k-NN, random split", line: { color: "#bf8700", width: 1.2 } },
    { x, y: rows.map((i) => knn.py.get(i)), name: "k-NN, year held out", line: { color: "#0969da", width: 1.2 } },
  ];
  Plotly.react("cv-series", traces, baseLayout({ title: { text: kind === "real" ? "DE-Hai 2014, sensor 1" : "simulated year 3", font: { size: 13 } },
    xaxis: { title: kind === "real" ? "date" : "day of year" }, yaxis: { title: "soil temperature, daily mean (°C)" },
    legend: { orientation: "h", y: -0.25 } }), PLOT_CONFIG);
}
for (const el of [cvTau, cvSig, cvK]) el.addEventListener("change", drawCV);
document.getElementById("cv-data").addEventListener("change", drawCV);
document.getElementById("cv-seed").addEventListener("click", () => { cvSeed += 1; drawCV(); });
drawCV();

// ---------------------------------------------------------------- ranking calculator
const margin = rConstant(code, "TS_MARGIN");
const estimators = [...code.ts_estimators.code.matchAll(/^\s+(\w+) = list\(kind/gm)].map((m) => `TS_${m[1]}`);
const metricDefs = [...code.TS_RANK_METRICS.code.matchAll(/(\w+) = function\(q\) (abs\(q\$\w+(?: - [\d.]+)?\)|q\$\w+)/g)].map(([, name, expr]) => {
  const m = expr.match(/^(abs\()?q\$(\w+)(?: - ([\d.]+))?\)?$/);
  const field = m[2], minus = m[3] ? Number(m[3]) : 0, abs = Boolean(m[1]);
  return { name, expr, field, score: (v) => (finite(v) ? (abs ? Math.abs(v - minus) : v) : NaN) };
});
const fields = [...new Set(metricDefs.map((m) => m.field))];
document.getElementById("rank-metrics").innerHTML = `<table class="plain"><tr><th>metric</th><th>score (R)</th><th>better</th></tr>
  ${metricDefs.map((m) => `<tr><td><code>${m.name}</code></td><td><code>${escapeHTML(m.expr)}</code></td><td>${m.name === "r2" ? "higher" : "lower"}</td></tr>`).join("")}</table>`;
document.getElementById("ref-cols").innerHTML = rStrings(code, "TS_REFERENCE_COLUMNS").map((c) => `<code>${c}</code>`).join(", ");
const DEFAULTS = {
  TS_linear: [2.10, 0.40, 0.88, 1.25, 1.10, 0.35, 0.95, 2.5, 0.45],
  TS_lm_ta: [2.05, 0.05, 0.88, 1.10, 1.08, 0.33, 0.85, 2.5, 0.10],
  TS_lm_ta_recent: [],
  TS_lm_ta_netrad: [1.95, 0.06, 0.89, 1.08, 1.02, 0.33, 0.80, 2.0, 0.11],
  TS_rf_ta: [1.90, -0.08, 0.90, 0.97, 1.00, 0.34, 0.70, 1.5, 0.12],
  TS_rf_ta_netrad: [1.70, 0.07, 0.92, 0.96, 0.92, 0.31, 0.55, 1.0, 0.10],
  TS_lm_memory: [1.20, 0.04, 0.96, 0.98, 0.70, 0.30, 0.30, 0.5, 0.06],
  TS_rf_memory: [1.05, -0.03, 0.97, 0.97, 0.62, 0.29, 0.25, 0.5, 0.05],
  TS_rf_memory_netrad: [1.10, 0.09, 0.965, 0.96, 0.64, 0.32, 0.20, 0.0, 0.09],
};
const DEFAULT_FIELDS = ["rmse", "bias", "r2", "sd_ratio", "cell_sd", "year_sd", "d_amp", "d_lag", "d_offset"];
document.getElementById("calc").innerHTML = `<table class="plain calc"><tr><th>column</th>${fields.map((f) => `<th>${f}</th>`).join("")}</tr>
  ${estimators.map((e) => `<tr><td><code>${e}</code></td>${fields.map((f) => {
    const v = (DEFAULTS[e] || [])[DEFAULT_FIELDS.indexOf(f)];
    return `<td><input type="number" step="any" data-e="${e}" data-f="${f}" value="${v ?? ""}"></td>`;
  }).join("")}</tr>`).join("")}</table>
  <p class="small">An empty row is an estimator without values at this site (e.g. <code>TS_lm_ta_recent</code> where the sensor has
  no data after 2021, fewer than ${rConstant(code, "TS_MIN_TRAIN")} training records).</p>`;

// rank_ts_columns() for one site
function rankSite(q) {
  const out = [];
  for (const m of metricDefs) {
    const v = estimators.map((e) => m.score(q[e][m.field]));
    const ok = v.map(finite);
    if (!ok.some(Boolean)) continue;
    const higher = m.name === "r2";
    const score = v.map((x) => (higher ? -x : x));
    const valid = score.filter((_, i) => ok[i]).sort((a, b) => a - b);
    const third = valid[Math.min(3, valid.length) - 1];
    estimators.forEach((e, i) => {
      const rank = ok[i] ? 1 + valid.filter((s) => s < score[i]).length : null;
      const close = higher ? score[i] <= third + margin.r2 : score[i] <= third + margin.relative * Math.abs(third);
      out.push({ metric: m.name, column: e, value: v[i], rank, top: ok[i] && (rank === 1 || close) });
    });
  }
  return out;
}
function drawCalc() {
  const q = Object.fromEntries(estimators.map((e) => [e, {}]));
  for (const el of document.querySelectorAll("#calc input")) q[el.dataset.e][el.dataset.f] = el.value === "" ? NaN : Number(el.value);
  const ranking = rankSite(q);
  const metrics = [...new Set(ranking.map((r) => r.metric))];
  const cell = (e, m) => ranking.find((r) => r.column === e && r.metric === m);
  document.getElementById("calc-out").innerHTML = `<table class="plain calc"><tr><th>column</th>${metrics.map((m) => `<th>${m}</th>`).join("")}<th>top under</th></tr>
    ${estimators.map((e) => `<tr><td><code>${e}</code></td>${metrics.map((m) => {
      const r = cell(e, m);
      if (!r || r.rank === null) return `<td class="na">–</td>`;
      return `<td class="${r.rank === 1 ? "best" : r.top ? "top" : ""}" title="score ${fmt(r.value, 3)}">${fmt(r.value, 3)} <span class="small">#${r.rank}</span></td>`;
    }).join("")}<td class="num">${ranking.filter((r) => r.column === e && r.top).length}</td></tr>`).join("")}</table>
    <p class="small">Each cell: the metric's value (as <code>TS_RANK_METRICS</code> computes it) and the rank. Dark green: best;
    light green: top (among the three best, or within the margin of the third best).</p>`;
  const winners = new Set(ranking.filter((r) => r.top).map((r) => r.column));
  const ref = rStrings(code, "TS_REFERENCE_COLUMNS");
  const selected = [...ref, ...estimators.filter((e) => winners.has(e) && !ref.includes(e))];
  const dropped = estimators.filter((e) => !selected.includes(e));
  document.getElementById("calc-selected").innerHTML = `<p class="note">${selected.map((c) => `<code>${c}</code>`).join(" ")}
    ${dropped.length ? `<br><span class="small">not selected (never top here): ${dropped.map((c) => `<code>${c}</code>`).join(" ")}</span>` : ""}</p>`;
}
document.getElementById("calc").addEventListener("input", drawCalc);
drawCalc();
