import { header, loadJSON, table, fmt, params, setParams, baseLayout, PLOT_CONFIG, CLASS_COLORS, tasClass, numberFormatter } from "./common.js";

header("site.html");
const sites = await loadJSON("data/sites.json");
const sets = await loadJSON("data/resultsets.json");
const tas = Object.fromEntries(await Promise.all(sets.map(async (s) => [s.id, await loadJSON(`data/tas/${s.id}.json`)])));
const siteyearCache = {};
const getSiteyear = async (id) => (siteyearCache[id] ??= await loadJSON(`data/siteyear/${id}.json`));

const siteSel = document.getElementById("site");
for (const s of sites) siteSel.insertAdjacentHTML("beforeend", `<option value="${s.site_ID}">${s.site_ID}</option>`);
const setSel = document.getElementById("set");
for (const s of sets) setSel.insertAdjacentHTML("beforeend", `<option value="${s.id}">${s.label}</option>`);
const p = params();
siteSel.value = p.get("site") || sites[0].site_ID;
setSel.value = p.get("set") || "published";
document.getElementById("model").value = p.get("model") || "total";
let tbl;

async function draw() {
  const site = siteSel.value;
  const set = setSel.value;
  const model = document.getElementById("model").value;
  setParams({ site, set, model });
  const meta = sites.find((s) => s.site_ID === site);
  document.getElementById("title").textContent = site;
  document.getElementById("meta").innerHTML = [
    `${meta.IGBP}, ${meta.Climate_class}`, `${fmt(meta.LAT, 3)}°, ${fmt(meta.LONG, 3)}°, ${meta.ELEV} m`,
    `MAT ${fmt(meta.MAT, 1)} °C, MAP ${meta.MAP} mm`, `data: ${meta.source}`,
    `estimated TS: ${meta.estimate_Ts}`, `measured SWC: ${meta.SWC_use}`,
    meta.ts_regression && meta.ts_regression !== "none" ? `TS from TA regression (${meta.ts_regression})` : "",
  ].filter(Boolean).join(" · ");

  // TAS by run, both models.
  const traces = ["total", "direct"].map((m, i) => {
    const pts = sets.map((s) => ({ s, r: (tas[s.id][m] || []).find((r) => r.site_ID === site) })).filter((x) => x.r);
    return { type: "scatter", mode: "markers", name: m, x: pts.map((x) => x.s.label), y: pts.map((x) => x.r.TAS),
      error_y: { type: "data", array: pts.map((x) => x.r.TAS_se ?? 0), visible: true },
      text: pts.map((x) => `${x.s.label}<br>TAS ${fmt(x.r.TAS)} (p ${fmt(x.r.TASp, 3)}, ${tasClass(x.r.TAS, x.r.TASp)})`), hoverinfo: "text",
      marker: { size: 10, symbol: i ? "diamond" : "circle", color: pts.map((x) => CLASS_COLORS[tasClass(x.r.TAS, x.r.TASp)]), line: { width: 1, color: "#000" } } };
  });
  Plotly.newPlot("tas", traces, baseLayout({ yaxis: { title: "TAS (°C⁻¹)", zeroline: true }, legend: { orientation: "h", y: -0.25 } }), PLOT_CONFIG);

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
