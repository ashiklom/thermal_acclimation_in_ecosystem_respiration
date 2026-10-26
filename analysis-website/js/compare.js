import { header, loadJSON, compare, summarise, table, fmt, params, setParams, baseLayout, PLOT_CONFIG, CLASS_COLORS, numberFormatter, siteCell, filterSites, siteSymbol, siteLabel, siteHTML } from "./common.js";

header("compare.html");
const sets = await loadJSON("data/resultsets.json");
const label = (id) => (sets.find((s) => s.id === id) || { label: id }).label;
const cache = {};
const getTas = async (id) => (cache[id] ??= await loadJSON(`data/tas/${id}.json`));

const p = params();
for (const id of ["a", "b"]) {
  const sel = document.getElementById(id);
  for (const s of sets) sel.insertAdjacentHTML("beforeend", `<option value="${s.id}">${s.label}</option>`);
}
document.getElementById("a").value = p.get("a") || "published";
document.getElementById("b").value = p.get("b") || sets[sets.length - 1].id;
document.getElementById("model").value = p.get("model") || "total";
if (p.get("threshold")) document.getElementById("threshold").value = p.get("threshold");
let tbl;

async function draw() {
  const a = document.getElementById("a").value;
  const b = document.getElementById("b").value;
  const model = document.getElementById("model").value;
  const threshold = Number(document.getElementById("threshold").value);
  setParams({ a, b, model, threshold });
  const rows = filterSites(compare(await getTas(a), await getTas(b), model, threshold).filter((r) => r.TAS_new !== null && r.TAS_ref !== null));
  const s = summarise(rows);
  const stat = (v, k) => `<div class="stat"><div class="v">${v}</div><div class="k">${k}</div></div>`;
  document.getElementById("stats").innerHTML = [
    stat(s.n, "sites in both"), stat(fmt(s.median_abs, 5), "median |ΔTAS|"), stat(fmt(s.max_abs, 5), "max |ΔTAS|"),
    stat(s.changed, `|ΔTAS| ≥ ${threshold}`), stat(s.transitions, "class changes"), stat(s.flips.length, "significant sign flips"),
  ].join("");
  document.getElementById("flipnote").innerHTML = s.flips.length
    ? `<div class="note"><span class="flip">Significant sign flips:</span> ${s.flips.map((x) => siteHTML(x)).join(", ")}</div>` : "";

  const lim = Math.max(...rows.flatMap((r) => [Math.abs(r.TAS_ref), Math.abs(r.TAS_new)])) * 1.1 || 0.1;
  const groups = Object.keys(CLASS_COLORS).map((c) => {
    const g = rows.filter((r) => r.class_new === c);
    return { type: "scatter", mode: "markers", name: `run: ${c}`, x: g.map((r) => r.TAS_ref), y: g.map((r) => r.TAS_new),
      error_y: { type: "data", array: g.map((r) => r.se ?? 0), visible: true, thickness: 0.6, width: 0, color: "#bbb" },
      text: g.map((r) => `${siteLabel(r.site_ID)}<br>ref ${fmt(r.TAS_ref)} (${r.class_ref})<br>run ${fmt(r.TAS_new)} (${r.class_new})<br>Δ ${fmt(r.dTAS, 5)}`),
      hoverinfo: "text", customdata: g.map((r) => r.site_ID),
      marker: { color: CLASS_COLORS[c], symbol: g.map((r) => siteSymbol(r.site_ID)), size: g.map((r) => (r.flip ? 13 : 8)),
        line: { width: g.map((r) => (r.flip ? 2 : 0.5)), color: "#000" } } };
  });
  const line = { type: "scatter", mode: "lines", x: [-lim, lim], y: [-lim, lim], line: { dash: "dash", color: "#666" }, showlegend: false, hoverinfo: "skip" };
  const band = { type: "scatter", x: [-lim, lim, lim, -lim], y: [-lim - threshold, lim - threshold, lim + threshold, -lim + threshold], fill: "toself",
    fillcolor: "rgba(140,149,159,0.25)", line: { width: 0 }, showlegend: false, hoverinfo: "skip" };
  Plotly.newPlot("scatter", [band, line, ...groups], baseLayout({
    xaxis: { title: `TAS (°C⁻¹), ${label(a)}`, range: [-lim, lim], zeroline: true }, yaxis: { title: `TAS (°C⁻¹), ${label(b)}`, range: [-lim, lim], zeroline: true },
    legend: { orientation: "h", y: -0.2 },
  }), PLOT_CONFIG);
  document.getElementById("scatter").on("plotly_click", (e) => { location.href = `site.html?site=${e.points[0].customdata}`; });

  // ΔTAS in units of the estimate's standard error where runs report it (TAS_se), else ΔTAS itself.
  const withSe = rows.filter((r) => r.d_over_se !== null);
  const useSe = withSe.length >= rows.length / 2;
  Plotly.newPlot("hist", [{ type: "histogram", x: useSe ? withSe.map((r) => r.d_over_se) : rows.map((r) => r.dTAS), nbinsx: 40, marker: { color: "#0969da" } }],
    baseLayout({ xaxis: { title: useSe ? "ΔTAS / standard error of TAS" : "ΔTAS (no standard errors in these runs)" }, yaxis: { title: "sites" } }), PLOT_CONFIG);

  const f = numberFormatter(5);
  const cols = [
    { title: "Site", field: "site_ID", formatter: siteCell },
    { title: "TAS ref", field: "TAS_ref", formatter: f, hozAlign: "right", headerFilter: false },
    { title: "TAS run", field: "TAS_new", formatter: f, hozAlign: "right", headerFilter: false },
    { title: "ΔTAS", field: "dTAS", formatter: f, hozAlign: "right", headerFilter: false },
    { title: "SE", field: "se", formatter: f, hozAlign: "right", headerFilter: false },
    { title: "Δ/SE", field: "d_over_se", formatter: numberFormatter(2), hozAlign: "right", headerFilter: false },
    { title: "p ref", field: "p_ref", formatter: numberFormatter(3), hozAlign: "right", headerFilter: false },
    { title: "p run", field: "p_new", formatter: numberFormatter(3), hozAlign: "right", headerFilter: false },
    { title: "class change", field: "transition" },
    { title: "flip", field: "flip", formatter: "tickCross", headerFilter: "tickCross" },
    { title: "changed", field: "changed", formatter: "tickCross", headerFilter: "tickCross" },
  ];
  if (tbl) tbl.replaceData(rows); else tbl = table("table", rows, cols, { height: "520px" });
}
for (const id of ["a", "b", "model", "threshold"]) document.getElementById(id).addEventListener("change", draw);
draw();
