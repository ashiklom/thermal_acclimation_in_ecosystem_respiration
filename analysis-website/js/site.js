import { header, loadJSON, table, fmt, params, setParams, baseLayout, PLOT_CONFIG, CLASS_COLORS, tasClass, numberFormatter, siteMaps, loadScreen, VERDICT_COLORS, siteLabel, isNew } from "./common.js";

header("site.html");
const sites = await loadJSON("data/sites.json");
const sets = await loadJSON("data/resultsets.json");
const tas = Object.fromEntries(await Promise.all(sets.map(async (s) => [s.id, await loadJSON(`data/tas/${s.id}.json`)])));
const siteyearCache = {};
const getSiteyear = async (id) => (siteyearCache[id] ??= await loadJSON(`data/siteyear/${id}.json`));

const siteSel = document.getElementById("site");
for (const s of sites) siteSel.insertAdjacentHTML("beforeend", `<option value="${s.site_ID}">${siteLabel(s.site_ID)}</option>`);
const setSel = document.getElementById("set");
for (const s of sets) setSel.insertAdjacentHTML("beforeend", `<option value="${s.id}">${s.label}</option>`);
const p = params();
siteSel.value = p.get("site") || sites[0].site_ID;
setSel.value = p.get("set") || "published";
document.getElementById("model").value = p.get("model") || "total";
let tbl;
const screen = await loadScreen();
const COLUMN_COLORS = { TS_original: "#0969da", TS_real: "#54aeff", TS_reconstructed: "#8250df", TS_manuscript: "#bf3989" };

function drawScreen(site) {
  const box = document.getElementById("screenverdicts");
  if (!screen) { box.innerHTML = `<p class="note">No run with the sensor screen yet.</p>`; return; }
  const d = screen.data;
  const summary = (d.summary || []).filter((r) => r.site_ID === site);
  box.innerHTML = `<table class="plain"><tr><th>column</th><th>verdict</th><th>flags</th><th>range ratio</th><th>lag (h)</th>
    <th>offset from air (°C)</th><th>coverage</th><th>bad share</th></tr>${summary.map((r) => `<tr><td>${r.column}</td>
    <td style="color:${VERDICT_COLORS[r.verdict]};font-weight:600">${r.verdict}</td><td>${r.flags || ""}</td><td>${fmt(r.amp_ratio, 3)}</td>
    <td>${fmt(r.lag_h, 2)}</td><td>${fmt(r.mean_offset, 2)}</td><td>${fmt(r.coverage, 2)}</td><td>${fmt(r.bad_fraction, 2)}</td></tr>`).join("")}</table>`;
  const diurnal = (d.diurnal || []).filter((r) => r.site_ID === site);
  const cols = [...new Set(diurnal.map((r) => r.column))];
  const air = diurnal.filter((r) => r.column === cols[0]);
  Plotly.newPlot("diurnal", [
    { type: "scatter", mode: "lines", name: "air", x: air.map((r) => r.hour), y: air.map((r) => r.ta), line: { color: "#888", dash: "dash" } },
    ...cols.map((c) => { const g = diurnal.filter((r) => r.column === c); return { type: "scatter", mode: "lines", name: c, x: g.map((r) => r.hour), y: g.map((r) => r.ts), line: { color: COLUMN_COLORS[c] } }; }),
  ], baseLayout({ title: { text: "Mean diurnal cycle, growing season", font: { size: 13 } }, xaxis: { title: "hour of day", dtick: 3 },
    yaxis: { title: "temperature (°C)" }, legend: { orientation: "h", y: -0.3 } }), PLOT_CONFIG);
  const yearly = (d.yearly || []).filter((r) => r.site_ID === site);
  const windows = (d.windows || []).filter((r) => r.site_ID === site && r.scale !== "period");
  const shapes = windows.map((w) => ({ type: "rect", xref: "x", yref: "paper", x0: Number(w.start.slice(0, 4)) - 0.5, x1: Number(w.end.slice(0, 4)) + 0.5,
    y0: 0, y1: 1, fillcolor: w.scale === "changepoint" ? "rgba(191,135,0,0.12)" : "rgba(207,34,46,0.12)", line: { width: 0 } }));
  Plotly.newPlot("yearly", ["amp_ratio", "lag_h", "offset"].flatMap((m, i) => [...new Set(yearly.map((r) => r.column))].map((c) => {
    const g = yearly.filter((r) => r.column === c);
    return { type: "scatter", mode: "lines+markers", name: `${c}`, legendgroup: c, showlegend: i === 0, xaxis: "x", yaxis: i ? `y${i + 1}` : "y",
      x: g.map((r) => r.growing_year), y: g.map((r) => r[m]), line: { color: COLUMN_COLORS[c] },
      marker: { size: g.map((r) => (r.outlier ? 10 : 5)) }, text: g.map((r) => r.outlier || ""), hoverinfo: "x+y+text" };
  })), baseLayout({ title: { text: "Yearly diagnostics (shaded: bad windows)", font: { size: 13 } }, shapes,
    grid: { rows: 3, columns: 1, pattern: "coupled" }, xaxis: { title: "growing year" },
    yaxis: { title: "range ratio" }, yaxis2: { title: "lag (h)" }, yaxis3: { title: "offset (°C)" }, legend: { orientation: "h", y: -0.25 }, height: 520 }), PLOT_CONFIG);
  const w = (d.windows || []).filter((r) => r.site_ID === site);
  document.getElementById("screenwindows").innerHTML = w.length ? `<table class="plain"><tr><th>column</th><th>start</th><th>end</th><th>scale</th><th>rule</th><th>value</th><th>reference</th></tr>${
    w.map((r) => `<tr><td>${r.column}</td><td>${r.start}</td><td>${r.end}</td><td>${r.scale}</td><td>${r.rule}</td><td>${fmt(r.value, 3)}</td><td>${fmt(r.reference, 3)}</td></tr>`).join("")}</table>`
    : `<p class="small">No bad windows.</p>`;
}

async function draw() {
  const site = siteSel.value;
  const set = setSel.value;
  const model = document.getElementById("model").value;
  setParams({ site, set, model });
  const meta = sites.find((s) => s.site_ID === site);
  document.getElementById("title").innerHTML = isNew(site) ? `${site} <span class="small">(added ${meta.added}, FLUXNET shuttle)</span>` : site;
  document.getElementById("meta").innerHTML = [
    `${meta.IGBP}, ${meta.Climate_class}`, `${fmt(meta.LAT, 3)}°, ${fmt(meta.LONG, 3)}°, ${meta.ELEV} m`,
    `MAT ${fmt(meta.MAT, 1)} °C, MAP ${meta.MAP} mm`, `data: ${meta.source}`,
    `estimated TS: ${meta.estimate_Ts}`, `measured SWC: ${meta.SWC_use}`,
    meta.ts_regression && meta.ts_regression !== "none" ? `TS from TA regression (${meta.ts_regression})` : "",
  ].filter(Boolean).join(" · ");

  drawScreen(site);

  // the site alone, on the map and in climate space
  const runRow = (tas[set]?.[model] || []).find((r) => r.site_ID === site);
  siteMaps("geo", "clim", [meta], { category: () => (runRow ? tasClass(runRow.TAS, runRow.TASp) : "NA"),
    label: `TAS class (${model})`, focus: true });

  // TAS by run, both models.
  const traces = ["total", "direct"].map((m, i) => {
    const pts = sets.map((s) => ({ s, r: (tas[s.id][m] || []).find((r) => r.site_ID === site) })).filter((x) => x.r);
    return { type: "scatter", mode: "markers", name: m, x: pts.map((x) => x.s.label), y: pts.map((x) => x.r.TAS),
      error_y: { type: "data", array: pts.map((x) => x.r.TAS_se ?? 0), visible: true },
      text: pts.map((x) => `${x.s.label}<br>TAS ${fmt(x.r.TAS)} (p ${fmt(x.r.TASp, 3)}, ${tasClass(x.r.TAS, x.r.TASp)})`), hoverinfo: "text",
      marker: { size: 10, symbol: i ? "diamond" : "circle", color: pts.map((x) => CLASS_COLORS[tasClass(x.r.TAS, x.r.TASp)]), line: { width: 1, color: "#000" } } };
  });
  Plotly.newPlot("tas", traces, baseLayout({ xaxis: { title: "run" }, yaxis: { title: "TAS (°C⁻¹)", zeroline: true }, legend: { orientation: "h", y: -0.35, title: { text: "model" } } }), PLOT_CONFIG);

  // Window-year detail for the chosen run.
  const sy = ((await getSiteyear(set))[model] || []).filter((r) => r.site_ID === site);
  const windows = [...new Set(sy.map((r) => r.window))];
  const wtr = windows.map((w) => {
    const g = sy.filter((r) => r.window === w && r.lnRatio !== null);
    return { type: "scatter", mode: "markers", name: w, x: g.map((r) => r.TS), y: g.map((r) => r.lnRatio),
      text: g.map((r) => `window ${w}, ${r.growing_year}<br>n=${r.nobsv}, +${r.extend_days} d<br>ln ratio ${fmt(r.lnRatio, 3)}`), hoverinfo: "text" };
  });
  Plotly.newPlot("windows", wtr, baseLayout({ xaxis: { title: "Window-year mean soil temperature (°C)" },
    yaxis: { title: "ln(ER_ref / ER_ref, control year)" }, legend: { title: { text: "window (DOY)" } } }), PLOT_CONFIG);

  const f = numberFormatter(4);
  const cols = ["growing_year", "window", "nobsv", "extend_days", "alpha", "beta", "C0", "Hs", "k2", "TS", "ERref", "lnRatio"]
    .map((k) => ({ title: k, field: k, formatter: typeof sy[0]?.[k] === "number" && !["growing_year", "nobsv", "extend_days"].includes(k) ? f : undefined }));
  if (tbl) tbl.replaceData(sy); else tbl = table("siteyears", sy, cols, { height: "360px" });
}
for (const id of ["site", "set", "model"]) document.getElementById(id).addEventListener("change", draw);
draw();
