// Soil temperature in detail: the documentation pages' explanations and
// simulations (docs-sensor-screen.js, docs-ts-estimators.js, run on this
// page's elements) and the main run's results: the sensor screen, the quality
// of every soil-temperature source, and the estimators' ranking.
import { header, loadJSON, loadMain, table, params, setParams, baseLayout, PLOT_CONFIG, numberFormatter, loadScreen, siteMaps,
  VERDICT_COLORS, SITES, siteCell, siteHTML, columnLabel, shortLabel } from "./common.js";

header("soil-temperature.html");

// sections: fold/unfold all; a link to a folded section opens it
const sections = [...document.querySelectorAll("details.section")];
document.getElementById("fold-all").addEventListener("click", (e) => { e.preventDefault(); sections.forEach((d) => { d.open = false; }); });
document.getElementById("unfold-all").addEventListener("click", (e) => { e.preventDefault(); sections.forEach((d) => { d.open = true; }); });
const openTarget = () => {
  const el = location.hash && document.getElementById(location.hash.slice(1));
  if (el) { const d = el.closest("details.section"); if (d && !d.open) { d.open = true; el.scrollIntoView(); } }
};
window.addEventListener("hashchange", openTarget);
openTarget();

const main = await loadMain();
const VERDICTS = { GOOD: "good", SUSPECT: "suspect", BAD: "bad" };
const SCREENED = { TS_original: "sensor", TS_real: "sensor's real readings", TS_reconstructed: "repaired sensor", TS_manuscript: "default" };

// ---- the sensor screen of every site
const screen = await loadScreen(main.run);
if (screen) {
  const v = screen.data.validation || [];
  const count = (pred) => v.filter(pred).length;
  const stat = (n, k) => `<div class="stat"><div class="v">${n}</div><div class="k">${k}</div></div>`;
  document.getElementById("screenstats").innerHTML = stat(v.length, "sites with a soil sensor") +
    ["GOOD", "SUSPECT", "BAD"].map((x) => stat(count((r) => r.verdict === x), `sensor ${VERDICTS[x]}`)).join("");
  siteMaps("screengeo", "screenclim", [...SITES.values()].filter((s) => screen.sensor.has(s.site_ID)), {
    category: (s) => VERDICTS[screen.sensor.get(s.site_ID).verdict] ?? "–", categories: ["good", "suspect", "bad"],
    label: "sensor", colors: { good: VERDICT_COLORS.GOOD, suspect: VERDICT_COLORS.SUSPECT, bad: VERDICT_COLORS.BAD },
    onClick: (id) => { location.href = `site.html?site=${id}`; } });
  const pct = (c) => (c.getValue() === null ? "–" : `${Math.round(100 * c.getValue())}%`);
  const verdict = (c) => `<span style="color:${VERDICT_COLORS[c.getValue()] || "#000"};font-weight:600">${VERDICTS[c.getValue()] ?? c.getValue()}</span>`;
  table("screentable", v, [
    { title: "Site", field: "site_ID", formatter: siteCell, frozen: true },
    { title: "series screened", field: "screened", formatter: (c) => SCREENED[c.getValue()] ?? c.getValue() },
    { title: "verdict", field: "verdict", formatter: verdict },
    { title: "flags", field: "flags" },
    { title: "share in bad windows", field: "bad_fraction", formatter: pct, hozAlign: "right", headerFilter: false },
    { title: "bad windows", field: "windows", widthGrow: 2 },
  ], { height: "480px" });
  table("windowstable", screen.data.windows || [], [
    { title: "Site", field: "site_ID", formatter: siteCell },
    { title: "series", field: "column", formatter: (c) => SCREENED[c.getValue()] ?? c.getValue() },
    { title: "start", field: "start" },
    { title: "end", field: "end" },
    { title: "scale", field: "scale" },
    { title: "rule", field: "rule" },
    { title: "value", field: "value", formatter: numberFormatter(3), hozAlign: "right", headerFilter: false },
    { title: "reference", field: "reference", formatter: numberFormatter(3), hozAlign: "right", headerFilter: false },
  ], { height: "420px" });
}

// ---- quality of every source, and the ranking
const ts = await loadJSON(`data/ts/${main.run}.json`);
const METRICS = {
  rmse: ["RMSE against the sensor (°C, out of fold)", false], bias: ["bias against the sensor (°C)", "diverging"],
  r2: ["r² against the sensor", true], sd_ratio: ["sd ratio (source / sensor)", false], cell_sd: ["within-cell error sd (°C)", false],
  year_sd: ["sd of the yearly bias (°C)", false], amp_ratio: ["median daily range / air temperature's", false],
  lag_h: ["peak-hour lag after air temperature (h)", "diverging"], offset: ["mean − air temperature (°C)", "diverging"],
  stuck: ["share in stuck runs", false], spikes: ["share of steps > 2 °C per hour", false],
  coverage: ["share of the record usable", true], n_night: ["nighttime records for the fits", true], n_years: ["years kept", true],
};
const metricSel = document.getElementById("metric");
metricSel.innerHTML = Object.entries(METRICS).map(([k, [name]]) => `<option value="${k}">${name}</option>`).join("");
metricSel.value = params().get("metric") || "rmse";
const q = ts.quality || [];
let tbl;
function drawQuality() {
  const metric = metricSel.value;
  setParams({ metric });
  const sites = [...new Set(q.map((r) => r.site_ID))];
  const columns = [...new Set(q.map((r) => r.column))];
  const val = new Map(q.map((r) => [`${r.site_ID}|${r.column}`, r[metric]]));
  const z = sites.map((s) => columns.map((c) => val.get(`${s}|${c}`) ?? null));
  const [, kind] = METRICS[metric];
  const vmax = Math.max(...z.flat().filter((x) => x !== null).map(Math.abs), 1e-9);
  Plotly.react("heat", [{ type: "heatmap", x: columns, y: sites, z, hoverongaps: false,
    text: sites.map((s) => columns.map((c) => `${s}<br>${columnLabel(c, false)}`)), hovertemplate: "%{text}<br>%{z:.3g}<extra></extra>",
    colorscale: kind === "diverging" ? "RdBu" : "Viridis", reversescale: kind === false,
    zmin: kind === "diverging" ? -vmax : undefined, zmax: kind === "diverging" ? vmax : undefined,
    colorbar: { title: { text: METRICS[metric][0] }, thickness: 12 } }],
  baseLayout({ margin: { l: 80, r: 20, t: 20, b: 230 }, xaxis: { tickangle: -45, title: "soil-temperature source",
    tickvals: columns, ticktext: columns.map(shortLabel) },
  yaxis: { autorange: "reversed", tickfont: { size: 9 }, title: "site" },
  plot_bgcolor: "#e5e5e5", height: Math.max(500, sites.length * 11 + 280) }), PLOT_CONFIG);
  const f = numberFormatter(3);
  const tcols = [
    { title: "Site", field: "site_ID", frozen: true, formatter: siteCell },
    { title: "source", field: "column", formatter: (c) => columnLabel(c.getValue(), false) },
    ...Object.entries(METRICS).map(([k, [name]]) => ({ title: k, field: k, formatter: f, hozAlign: "right", headerFilter: false, headerTooltip: name })),
  ];
  if (!tbl) tbl = table("table", q, tcols, { height: "480px" });
}
metricSel.addEventListener("change", drawQuality);
drawQuality();
document.getElementById("heat").on("plotly_click", (e) => { location.href = `site.html?site=${e.points[0].y}`; });

const rk = ts.ranking || [];
const metrics = [...new Set(rk.map((r) => r.metric))];
const cols = [...new Set(rk.map((r) => r.column))];
Plotly.newPlot("tops", metrics.map((m) => ({ type: "bar", name: METRICS[m]?.[0] ?? m, x: cols.map(shortLabel),
  y: cols.map((c) => rk.filter((r) => r.metric === m && r.column === c && r.top).length) })),
baseLayout({ barmode: "group", xaxis: { title: "estimator", tickangle: -30 }, yaxis: { title: "sites where among the best" },
  legend: { orientation: "h", y: -0.6, title: { text: "metric" } }, height: 560, margin: { b: 220 } }), PLOT_CONFIG);
const selected = (ts.methods_selected || []).map((r) => r.column ?? Object.values(r)[0]);
document.getElementById("selected").innerHTML = selected.length
  ? `<div class="note"><b>Sources kept (${selected.length}):</b> ${selected.map((c) => columnLabel(c, false)).join("; ")}</div>` : "";

// ---- the explanations and simulations
await import("./docs-sensor-screen.js");
await import("./docs-ts-estimators.js");
