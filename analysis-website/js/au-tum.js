import { header, loadJSON, baseLayout, PLOT_CONFIG, fmt, tasClass, CLASS_COLORS } from "./common.js";

header("au-tum.html");

const COLORS = { F15_VUT: "#0969da", F15_CUT: "#54aeff", SHUT_CUT: "#bf8700", TERN: "#1a7f37" };
const SHORT = { F15_VUT: "FLUXNET2015 VUT", F15_CUT: "FLUXNET2015 CUT", SHUT_CUT: "Shuttle v1.3 CUT", TERN: "TERN L6" };
const d = await loadJSON("data/au_tum_sources.json");
const options = await loadJSON("data/au_tum_options.json").catch(() => ({}));
const streams = Object.keys(d.streams);

// ---- streams table
document.getElementById("streams").innerHTML = `<table class="plain"><tr><th>stream</th><th>NEE column</th><th>file</th>
  <th>period</th><th class="num">hours</th><th class="num">night records (step 01's selection)</th></tr>${streams.map((s) => {
  const x = d.streams[s];
  return `<tr><td><span class="pill" style="background:${COLORS[s]}">${SHORT[s]}</span></td><td><code>${x.nee_column}</code></td>
    <td class="small">${x.file}</td><td>${x.start} – ${x.end}</td><td class="num">${x.hours.toLocaleString()}</td>
    <td class="num">${x.n_night.toLocaleString()}</td></tr>`;
}).join("")}</table>`;

// ---- monthly time series (from the daily values)
function monthly(s, key) {
  const x = d.daily[s];
  const acc = new Map();
  x.date.forEach((date, i) => {
    const m = date.slice(0, 7);
    const v = x[key][i];
    if (v === null) return;
    const w = key === "night" ? x.n_night[i] : 1;
    const a = acc.get(m) || [0, 0];
    acc.set(m, [a[0] + v * w, a[1] + w]);
  });
  const months = [...acc.keys()].sort();
  const minWeight = key === "night" ? 10 : 20;
  return { x: months.map((m) => `${m}-15`), y: months.map((m) => (acc.get(m)[1] >= minWeight ? acc.get(m)[0] / acc.get(m)[1] : null)) };
}
function drawSeries() {
  const key = document.getElementById("series-var").value;
  const traces = streams.map((s) => ({ type: "scatter", mode: "lines", name: SHORT[s], line: { color: COLORS[s], width: 1.5 },
    connectgaps: false, ...monthly(s, key) }));
  const ylab = key === "night" ? "monthly mean night NEE, observations (µmol m⁻² s⁻¹)" : "monthly mean NEE, all hours, gap-filled (µmol m⁻² s⁻¹)";
  Plotly.newPlot("series", traces, baseLayout({ xaxis: { title: "date" }, yaxis: { title: ylab }, legend: { orientation: "h", y: 1.1 },
    shapes: [{ type: "line", xref: "x", yref: "paper", x0: "2015-01-01", x1: "2015-01-01", y0: 0, y1: 1, line: { dash: "dot", color: "#888" } }] }), PLOT_CONFIG);
}
document.getElementById("series-var").addEventListener("change", drawSeries);
drawSeries();

// ---- annual metrics
const ANNUAL = {
  n_night: "night records selected (per year)", night_nee: "mean night NEE of the selected records (µmol m⁻² s⁻¹)",
  night_ts: "mean soil temperature of the selected records (°C)", ustar_p05: "5% quantile of u* of the selected records (m s⁻¹)",
  r10: "R10 from ln(NEE) = a + b·TS (µmol m⁻² s⁻¹)", q10: "Q10 from ln(NEE) = a + b·TS", nee_sum: "annual NEE, years ≥ 95% complete (g C m⁻²)",
  ts_mean: "mean soil temperature, all hours (°C)", swc_mean: "mean soil water, all hours (%)",
};
const annualSel = document.getElementById("annual-var");
for (const [k, v] of Object.entries(ANNUAL)) annualSel.insertAdjacentHTML("beforeend", `<option value="${k}">${v}</option>`);
function drawAnnual() {
  const key = annualSel.value;
  const traces = streams.map((s) => ({ type: "scatter", mode: "lines+markers", name: SHORT[s], line: { color: COLORS[s] },
    x: d.annual[s].map((r) => r.year), y: d.annual[s].map((r) => r[key]) }));
  Plotly.newPlot("annual", traces, baseLayout({ xaxis: { title: "year", dtick: 2 }, yaxis: { title: ANNUAL[key] }, legend: { orientation: "h", y: 1.1 } }), PLOT_CONFIG);
}
annualSel.addEventListener("change", drawAnnual);
drawAnnual();

// ---- pairs
const pairSel = document.getElementById("pair");
d.pairs.forEach((p, i) => pairSel.insertAdjacentHTML("beforeend", `<option value="${i}">${SHORT[p.a]} vs ${SHORT[p.b]} (${p.years.join("–")})</option>`));
pairSel.value = String(d.pairs.findIndex((p) => p.a === "F15_VUT" && p.b === "TERN"));
function drawPair() {
  const p = d.pairs[Number(pairSel.value)];
  const s = p.sample;
  const lim = [-5, 30];
  Plotly.newPlot("pair-scatter", [
    { type: "scattergl", mode: "markers", x: s.x, y: s.y, marker: { size: 4, color: s.ts, colorscale: "Viridis", opacity: 0.6,
      colorbar: { title: { text: `soil temperature, ${SHORT[p.a]} (°C)` }, thickness: 12 } }, name: "hours both select", hovertemplate: "%{x:.2f}, %{y:.2f}<extra></extra>" },
    { type: "scatter", mode: "lines", x: lim, y: lim, line: { color: "#000", dash: "dash", width: 1 }, name: "1:1", hoverinfo: "skip" },
  ], baseLayout({ title: { text: `Night NEE where both select the hour (sample of ${s.x.length.toLocaleString()} of ${p.night.n.toLocaleString()})`, font: { size: 13 } },
    xaxis: { title: `${SHORT[p.a]} NEE (µmol m⁻² s⁻¹)`, range: lim }, yaxis: { title: `${SHORT[p.b]} NEE (µmol m⁻² s⁻¹)`, range: lim }, showlegend: false }), PLOT_CONFIG);
  const u = p.by_ustar;
  Plotly.newPlot("pair-ustar", [{ type: "bar", x: u.lower.map((l, i) => `${l}–${u.upper[i]}`), y: u.diff, marker: { color: COLORS[p.b] },
    text: u.n.map((n) => `n=${n}`), textposition: "outside", hovertemplate: "%{x} m/s: %{y:.2f} (%{text})<extra></extra>" }],
  baseLayout({ title: { text: "Mean night difference by turbulence", font: { size: 13 } },
    xaxis: { title: `u* bin, ${SHORT[p.a]} (m s⁻¹)` }, yaxis: { title: `NEE ${SHORT[p.b]} − ${SHORT[p.a]} (µmol m⁻² s⁻¹)` } }), PLOT_CONFIG);
  const sel = p.selection;
  Plotly.newPlot("pair-selection", [
    { type: "bar", name: "both", x: sel.year, y: sel.both, marker: { color: "#8c959f" } },
    { type: "bar", name: `only ${SHORT[p.a]}`, x: sel.year, y: sel.only_a, marker: { color: COLORS[p.a] } },
    { type: "bar", name: `only ${SHORT[p.b]}`, x: sel.year, y: sel.only_b, marker: { color: COLORS[p.b] } },
  ], baseLayout({ barmode: "stack", title: { text: "Which night hours each stream selects", font: { size: 13 } },
    xaxis: { title: "year", dtick: 2 }, yaxis: { title: "night records" }, legend: { orientation: "h", y: -0.25 } }), PLOT_CONFIG);
}
pairSel.addEventListener("change", drawPair);
drawPair();

// ---- agreement table
const row = (label, a) => `<td>${label}</td><td class="num">${a.n?.toLocaleString() ?? "–"}</td><td class="num">${fmt(a.bias, 3)}</td>
  <td class="num">${fmt(a.rmse, 3)}</td><td class="num">${fmt(a.r, 3)}</td><td class="num">${fmt(a.slope, 3)}</td>`;
document.getElementById("agreement").innerHTML = `<table class="plain"><tr><th>a</th><th>b</th><th>years</th><th>variable</th><th class="num">n</th>
  <th class="num">bias (b − a)</th><th class="num">RMSE</th><th class="num">r</th><th class="num">slope</th></tr>${d.pairs.map((p) => {
  const vars = [["night NEE (both select)", p.night], ...Object.entries(p.drivers)];
  return vars.map(([k, a], i) => `<tr>${i === 0 ? `<td rowspan="${vars.length}">${SHORT[p.a]}</td><td rowspan="${vars.length}">${SHORT[p.b]}</td>
    <td rowspan="${vars.length}">${p.years.join("–")}</td>` : ""}${row(k, a)}</tr>`).join("");
}).join("")}</table>`;

// ---- respiration curves
const periodSel = document.getElementById("period");
const periods = [...new Set(streams.flatMap((s) => Object.keys(d.curves[s])))].sort();
for (const p of periods) periodSel.insertAdjacentHTML("beforeend", `<option>${p}</option>`);
function drawCurves() {
  const period = periodSel.value;
  const traces = streams.filter((s) => d.curves[s][period]).map((s) => ({ type: "scatter", mode: "lines+markers", name: SHORT[s],
    line: { color: COLORS[s] }, x: d.curves[s][period].ts, y: d.curves[s][period].nee, text: d.curves[s][period].n.map((n) => `n=${n}`),
    hovertemplate: "%{x} °C: %{y:.2f} (%{text})<extra></extra>" }));
  Plotly.newPlot("curves", traces, baseLayout({ xaxis: { title: "soil temperature, 1 °C bins (°C)" },
    yaxis: { title: "mean night NEE of the selected records (µmol m⁻² s⁻¹)" }, legend: { orientation: "h", y: 1.1 } }), PLOT_CONFIG);
}
periodSel.addEventListener("change", drawCurves);
drawCurves();

// ---- TAS under each candidate source
const OPTION_LABELS = { published: "FLUXNET2015 only (published, 2001–2014)", F15_TERN: "FLUXNET2015, then TERN L6 (2001–2026)",
  TERN: "TERN L6 only (2002–2026)" };
const optIds = Object.keys(OPTION_LABELS).filter((o) => options[o]);
if (!optIds.length) {
  document.getElementById("tas").innerHTML = `<p class="note">The fits of each candidate source have not been run yet
    (<code>scripts/au-tum-source-options.R</code>).</p>`;
} else {
  const rows = optIds.map((o) => {
    const x = options[o];
    const t = (m) => (x[`outcome_${m}`] || [])[0] || {};
    return { o, inputs: x.inputs, total: t("total"), direct: t("direct") };
  });
  const cls = (r) => { const c = tasClass(r.TAS, r.TASp); return `<span class="pill" style="background:${CLASS_COLORS[c]}">${c}</span>`; };
  document.getElementById("tas-table").innerHTML = `<table class="plain"><tr><th>source</th><th class="num">night records</th><th class="num">years</th>
    <th>control year</th><th class="num">tStart–tEnd (°C)</th>${["total", "direct"].map((m) => `<th class="num">${m} TAS (± se)</th><th>p, class</th>`).join("")}</tr>
    ${rows.map((r) => `<tr><td>${OPTION_LABELS[r.o]}</td><td class="num">${r.inputs.n_night.toLocaleString()}</td>
      <td class="num">${r.inputs.n_years} (${r.inputs.first_year}–${r.inputs.last_year})</td><td>${r.inputs.control_year}</td>
      <td class="num">${fmt(r.inputs.tStart, 3)}–${fmt(r.inputs.tEnd, 3)}</td>
      ${["total", "direct"].map((m) => `<td class="num">${fmt(r[m].TAS, 4)} ± ${fmt(r[m].TAS_se, 4)}</td><td>${fmt(r[m].TASp, 3)} ${cls(r[m])}</td>`).join("")}</tr>`).join("")}</table>`;
  const traces = ["total", "direct"].map((m, i) => ({ type: "scatter", mode: "markers", name: `${m} TAS`,
    x: rows.map((r) => OPTION_LABELS[r.o]), y: rows.map((r) => r[m].TAS),
    error_y: { type: "data", array: rows.map((r) => 1.96 * (r[m].TAS_se ?? 0)), visible: true },
    marker: { size: 10, symbol: i ? "diamond" : "circle" }, offsetgroup: m }));
  Plotly.newPlot("tas-plot", traces, baseLayout({ xaxis: { title: "AU-Tum's record" }, yaxis: { title: "TAS (± 1.96 se)", zeroline: true },
    scattermode: "group", legend: { orientation: "h", y: 1.1 } }), PLOT_CONFIG);
}
