import { header, loadJSON, loadMain, table, params, setParams, baseLayout, PLOT_CONFIG, numberFormatter, siteCell, SITES,
  tempSourcePicker, tempSource, columnLabel, PREDICTORS } from "./common.js";

header("drivers.html");
const main = await loadMain();
const index = await loadJSON("data/drivers.json");
const ids = new Set(index.map((e) => e.id));
document.getElementById("trs").value = params().get("trs") || "TAS_direct";
tempSourcePicker("controls", draw);
document.getElementById("trs").addEventListener("change", draw);
// the random forests' predictors (R/step03-drivers.R driver_analysis())
const VARS = { elev: "ELEV", mat: "MATA", lai: "LAI", soc: "SOC", warm_rate: "warm_rate", ELEV: "ELEV", MATA: "MATA", LAI: "LAI",
  SOC: "SOC" };
const varLabel = (v) => PREDICTORS[VARS[v] ?? v] ?? v;
const RESPONSES = { TAS_direct: "direct TAS", TAS_tot: "total TAS", TAS_total: "total TAS", TAS_app: "apparent TAS" };
const increase = (r, col) => (r[col] !== null && r.NEE_night_mod_p ? r[col] / r.NEE_night_mod_p - 1 : null);
const mean = (v) => { const x = v.filter((y) => y !== null && Number.isFinite(y)); return x.length ? x.reduce((s, y) => s + y, 0) / x.length : null; };
const pct = (x) => (x === null || !Number.isFinite(x) ? "–" : `${(100 * x).toFixed(1)} %`);
let tbl;

async function draw() {
  const column = tempSource(), trs = document.getElementById("trs").value;
  setParams({ trs });
  const id = `${main.run}--${column}`;
  if (!ids.has(id)) {
    document.getElementById("note").innerHTML = `<p class="note">No driver analysis with ${columnLabel(column)}.</p>`;
    return;
  }
  document.getElementById("note").innerHTML = "";
  const T = await loadJSON(`data/drivers/${id}.json`);
  const fut = (T.future || []).map((r) => ({ ...r, without: increase(r, "NEE_night_mod_f"), with: increase(r, "NEE_night_mod_fa") }))
    .filter((r) => [r.without, r.with].every((x) => x !== null && Number.isFinite(x)));
  const stat = (v, k) => `<div class="stat"><div class="v">${v}</div><div class="k">${k}</div></div>`;
  document.getElementById("stats").innerHTML = [
    stat((T.drivers || []).length, "sites in the driver analysis"),
    stat(pct(mean(fut.map((r) => r.without))), `mean future increase, short-term response only (${fut.length} sites)`),
    stat(pct(mean(fut.map((r) => r.with))), "mean future increase, with total TAS"),
  ].join("");

  const vi = (T.varimp || []).filter((r) => r.TRS_type === trs).sort((a, b) => b.varImp - a.varImp);
  // horizontal bars, most important on top, so the predictor names have room
  Plotly.react("varimp", [{ type: "bar", orientation: "h", y: vi.map((r) => varLabel(r.var)), x: vi.map((r) => r.varImp),
    marker: { color: "#0969da" }, hovertemplate: "%{y}: %{x:.1f}<extra></extra>" }],
    baseLayout({ title: { text: `Variable importance, ${RESPONSES[trs]}`, font: { size: 13 } },
      xaxis: { title: "% increase in MSE when permuted" }, yaxis: { autorange: "reversed", automargin: true } }), PLOT_CONFIG);

  const lim = Math.max(...fut.flatMap((r) => [Math.abs(r.without), Math.abs(r.with ?? 0)])) * 1.1 || 0.1;
  const tas = fut.map((r) => r.TAS_tot);
  const tmax = Math.max(...tas.filter(Number.isFinite).map(Math.abs), 1e-6);
  Plotly.react("future", [
    { type: "scatter", mode: "lines", x: [-lim, lim], y: [-lim, lim], line: { dash: "dash", color: "#999" }, showlegend: false, hoverinfo: "skip" },
    { type: "scatter", mode: "markers", x: fut.map((r) => r.without), y: fut.map((r) => r.with), customdata: fut.map((r) => r.site_ID),
      text: fut.map((r) => `<b>${r.site_ID}</b> ${SITES.get(r.site_ID)?.site_name || ""}<br>without TAS ${pct(r.without)}<br>with total TAS ${pct(r.with)}` +
        `<br>total TAS ${numberFormatter(4)({ getValue: () => r.TAS_tot })}`), hoverinfo: "text", showlegend: false,
      marker: { size: 8, color: tas, colorscale: "RdBu", cmin: -tmax, cmax: tmax, line: { width: 0.5, color: "#333" },
        colorbar: { title: { text: "total TAS" }, thickness: 12 } } },
  ], baseLayout({ title: { text: "Future nighttime respiration increase", font: { size: 13 } },
    xaxis: { title: "short-term temperature response only", tickformat: ".0%" },
    yaxis: { title: "with total TAS", tickformat: ".0%" } }), PLOT_CONFIG);
  const fdiv = document.getElementById("future");
  if (!fdiv.dataset.bound) { fdiv.on("plotly_click", (e) => { const s = e.points[0].customdata; if (s) location.href = `site.html?site=${s}`; }); fdiv.dataset.bound = "1"; }

  const resp = trs === "TAS_tot" ? ["TAS_tot", "TAS_total"] : [trs];
  const pd = (T.partial || []).filter((r) => resp.includes(r.TRS_type));
  const vars = [...new Set(pd.map((r) => r.var))];
  const root = document.getElementById("partial");
  root.innerHTML = vars.map((v) => `<div id="pd-${v}" class="plot" style="min-height:300px"></div>`).join("");
  for (const v of vars) {
    const rows = pd.filter((r) => r.var === v).sort((a, b) => a.x - b.x);
    Plotly.newPlot(`pd-${v}`, [
      { type: "scatter", mode: "lines", x: rows.map((r) => r.x), y: rows.map((r) => r.y950), line: { width: 0 }, showlegend: false, hoverinfo: "skip" },
      { type: "scatter", mode: "lines", x: rows.map((r) => r.x), y: rows.map((r) => r.y050), fill: "tonexty", fillcolor: "rgba(9,105,218,0.2)",
        line: { width: 0 }, showlegend: false, hoverinfo: "skip" },
      { type: "scatter", mode: "lines", x: rows.map((r) => r.x), y: rows.map((r) => r.y), line: { color: "#0969da" }, showlegend: false },
    ], baseLayout({ xaxis: { title: varLabel(v) }, yaxis: { title: `${RESPONSES[trs]} (per °C)` }, margin: { l: 60, r: 10, t: 20, b: 45 } }), PLOT_CONFIG);
  }

  const rows = fut.map((r) => ({ site_ID: r.site_ID, name: SITES.get(r.site_ID)?.site_name || "", TAS_tot: r.TAS_tot,
    warming: r.TSmin_c_gs, without: r.without, with: r.with }));
  const f = (c) => pct(c.getValue());
  const cols = [
    { title: "Site", field: "site_ID", formatter: siteCell },
    { title: "Name", field: "name" },
    { title: "Total TAS", field: "TAS_tot", formatter: numberFormatter(4), hozAlign: "right", headerFilter: false },
    { title: "Warming of minimum soil temperature, growing season (°C)", field: "warming", formatter: numberFormatter(2), hozAlign: "right", headerFilter: false },
    { title: "Increase, short-term response only", field: "without", formatter: f, hozAlign: "right", headerFilter: false },
    { title: "Increase with total TAS", field: "with", formatter: f, hozAlign: "right", headerFilter: false },
  ];
  if (tbl) tbl.replaceData(rows); else tbl = table("table", rows, cols, { height: "440px" });
}
draw();
