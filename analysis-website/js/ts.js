import { header, loadJSON, table, params, setParams, baseLayout, PLOT_CONFIG, numberFormatter, loadScreen, siteMaps, VERDICT_COLORS, fmt } from "./common.js";

header("ts.html");
const runs = await loadJSON("data/ts_runs.json");
const sets = await loadJSON("data/resultsets.json");
const label = (id) => (sets.find((s) => s.id === id) || { label: id }).label;
const runSel = document.getElementById("run");
if (!runs.length) {
  document.getElementById("selected").innerHTML = `<p class="note">No run with soil-temperature columns yet.</p>`;
}
for (const id of runs) runSel.insertAdjacentHTML("beforeend", `<option value="${id}">${label(id)}</option>`);
const METRICS = {
  rmse: ["RMSE vs sensor (°C, out of fold)", false], bias: ["bias vs sensor (°C)", "diverging"], r2: ["r² vs sensor", true],
  sd_ratio: ["sd ratio (column / sensor)", false], cell_sd: ["within-cell error sd (°C)", false], year_sd: ["sd of yearly bias (°C)", false],
  amp_ratio: ["daily range / air temperature's", false], lag_h: ["peak-hour lag after air temperature (h)", "diverging"],
  offset: ["mean − air temperature (°C)", "diverging"], stuck: ["share in stuck runs", false], spikes: ["share of > 5 °C steps", false],
  coverage: ["share of record usable", true], n_night: ["nighttime fit records", true], n_years: ["years kept", true],
};
const metricSel = document.getElementById("metric");
for (const [k, [name]] of Object.entries(METRICS)) metricSel.insertAdjacentHTML("beforeend", `<option value="${k}">${name}</option>`);
const p = params();
runSel.value = p.get("run") || runs[runs.length - 1];
metricSel.value = p.get("metric") || "rmse";
let tbl;

async function draw() {
  const run = runSel.value, metric = metricSel.value;
  setParams({ run, metric });
  const d = await loadJSON(`data/ts/${run}.json`);
  const q = d.quality || [];
  const selected = (d.methods_selected || []).map((r) => r.column ?? Object.values(r)[0]);
  document.getElementById("selected").innerHTML = selected.length
    ? `<div class="note"><b>Carried forward to the fits (${selected.length}):</b> ${selected.map((c) => `<code>${c}</code>`).join(" ")}</div>` : "";

  // top counts per estimator and metric
  const rk = d.ranking || [];
  const metrics = [...new Set(rk.map((r) => r.metric))];
  const cols = [...new Set(rk.map((r) => r.column))];
  Plotly.newPlot("tops", metrics.map((m) => ({ type: "bar", name: m, x: cols,
    y: cols.map((c) => rk.filter((r) => r.metric === m && r.column === c && r.top).length) })),
  baseLayout({ barmode: "group", xaxis: { title: "estimator" }, yaxis: { title: "sites where best or close to the top 3" },
    legend: { orientation: "h", y: -0.35, title: { text: "metric" } } }), PLOT_CONFIG);

  // heatmap
  const sites = [...new Set(q.map((r) => r.site_ID))];
  const columns = [...new Set(q.map((r) => r.column))];
  const val = new Map(q.map((r) => [`${r.site_ID}|${r.column}`, r[metric]]));
  const z = sites.map((s) => columns.map((c) => val.get(`${s}|${c}`) ?? null));
  const [, kind] = METRICS[metric];
  const flat = z.flat().filter((v) => v !== null);
  const vmax = Math.max(...flat.map(Math.abs), 1e-9);
  Plotly.newPlot("heat", [{ type: "heatmap", x: columns, y: sites, z, hoverongaps: false,
    colorscale: kind === "diverging" ? "RdBu" : "Viridis", reversescale: kind === false,
    zmin: kind === "diverging" ? -vmax : undefined, zmax: kind === "diverging" ? vmax : undefined,
    colorbar: { title: { text: METRICS[metric][0] }, thickness: 12 } }],
  baseLayout({ margin: { l: 80, r: 20, t: 20, b: 140 }, xaxis: { tickangle: -45, title: "soil-temperature column" },
    yaxis: { autorange: "reversed", tickfont: { size: 9 }, title: "site" },
    plot_bgcolor: "#e5e5e5", height: Math.max(500, sites.length * 11 + 200) }), PLOT_CONFIG);
  document.getElementById("heat").on("plotly_click", (e) => { location.href = `site.html?site=${e.points[0].y}`; });

  const f = numberFormatter(3);
  const tcols = [
    { title: "Site", field: "site_ID", frozen: true, formatter: (c) => `<a href="site.html?site=${c.getValue()}">${c.getValue()}</a>` },
    { title: "Column", field: "column" },
    ...Object.entries(METRICS).map(([k, [name]]) => ({ title: k, field: k, formatter: f, hozAlign: "right", headerFilter: false, headerTooltip: name })),
  ];
  if (tbl) tbl.replaceData(q); else tbl = table("table", q, tcols, { height: "520px" });
}
for (const el of [runSel, metricSel]) el.addEventListener("change", draw);
if (runs.length) draw();

// The sensor screen (input data only; independent of the run selector above).
const screen = await loadScreen();
if (screen) {
  const sites = await loadJSON("data/sites.json");
  const v = screen.data.validation || [];
  const count = (pred) => v.filter(pred).length;
  const stat = (n, k) => `<div class="stat"><div class="v">${n}</div><div class="k">${k}</div></div>`;
  document.getElementById("screenstats").innerHTML = ["GOOD", "SUSPECT", "BAD"].map((x) => stat(count((r) => r.verdict === x), `sensor ${x}`)).join("") +
    stat(count((r) => r.verdict !== "GOOD" && r.manuscript_acted), "not GOOD, manuscript corrected it") +
    stat(count((r) => r.verdict !== "GOOD" && !r.manuscript_acted), "not GOOD, manuscript kept it") +
    stat(count((r) => r.verdict === "GOOD" && r.manuscript_acted), "GOOD, manuscript corrected it");
  siteMaps("screengeo", "screenclim", sites.filter((s) => screen.sensor.has(s.site_ID)), {
    category: (s) => screen.sensor.get(s.site_ID).verdict, label: "sensor", colors: VERDICT_COLORS,
    onClick: (id) => { location.href = `site.html?site=${id}#screen`; } });
  const pct = (c) => (c.getValue() === null ? "–" : `${Math.round(100 * c.getValue())}%`);
  const link = (c) => `<a href="site.html?site=${c.getValue()}#screen">${c.getValue()}</a>`;
  const verdict = (c) => `<span style="color:${VERDICT_COLORS[c.getValue()] || "#000"};font-weight:600">${c.getValue()}</span>`;
  table("screentable", v, [
    { title: "Site", field: "site_ID", formatter: link, frozen: true },
    { title: "screened", field: "screened" },
    { title: "verdict", field: "verdict", formatter: verdict },
    { title: "flags", field: "flags" },
    { title: "bad share", field: "bad_fraction", formatter: pct, hozAlign: "right", headerFilter: false },
    { title: "bad windows", field: "windows", widthGrow: 2 },
    { title: "manuscript: step 01 replaces", field: "step01_replaced", formatter: pct, hozAlign: "right", headerFilter: false },
    { title: "manuscript: fits replace", field: "manuscript_replaced", formatter: pct, hozAlign: "right", headerFilter: false },
    { title: "regression", field: "ts_regression" },
    { title: "sensor from", field: "sensor_first_year", hozAlign: "right" },
    { title: "years removed", field: "year_removed" },
    { title: "manuscript corrected", field: "manuscript_acted", formatter: "tickCross", headerFilter: "tickCross" },
  ], { height: "520px" });
  table("windowstable", screen.data.windows || [], [
    { title: "Site", field: "site_ID", formatter: link },
    { title: "column", field: "column" },
    { title: "start", field: "start" },
    { title: "end", field: "end" },
    { title: "scale", field: "scale" },
    { title: "rule", field: "rule" },
    { title: "value", field: "value", formatter: numberFormatter(3), hozAlign: "right", headerFilter: false },
    { title: "reference", field: "reference", formatter: numberFormatter(3), hozAlign: "right", headerFilter: false },
  ], { height: "420px" });
} else {
  document.getElementById("screenstats").innerHTML = `<p class="note">No run with the sensor screen yet.</p>`;
}
