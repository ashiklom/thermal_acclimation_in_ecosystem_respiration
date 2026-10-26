import { isNew, NEW_SYMBOL, header, loadJSON, table, params, setParams, baseLayout, PLOT_CONFIG, numberFormatter, fmt, siteCell, filterSites, siteSymbol, siteLabel, includeNew } from "./common.js";

header("drivers.html");
const index = await loadJSON("data/drivers.json");
const byId = new Map(index.map((e) => [e.id, e]));
// with the added sites excluded, an analysis over all sites is shown by its
// counterpart run over the manuscript's sites (same code and data; the
// forests refitted without the added sites)
const resolve = (id) => (!includeNew() && byId.get(id)?.original_sites_id) || id;
const cache = {};
const get = async (id) => (cache[id] ??= await loadJSON(`data/drivers/${id}.json`));
for (const id of ["a", "b"]) for (const e of index) document.getElementById(id).insertAdjacentHTML("beforeend", `<option value="${e.id}">${e.label}</option>`);
const p = params();
document.getElementById("a").value = p.get("a") || "published";
document.getElementById("b").value = p.get("b") || index[index.length - 1].id;
document.getElementById("trs").value = p.get("trs") || "TAS_direct";
let tbl;
const PREDICTORS = { elev: "elevation (m)", mata: "mean annual air temperature (°C)", mat: "mean annual air temperature (°C)",
  lai: "leaf area index (m² m⁻²)", soc: "soil organic carbon", warm_rate: "warming rate" };
const RESPONSES = { TAS_direct: "direct TAS", TAS_tot: "total TAS", TAS_total: "total TAS", TAS_app: "apparent TAS" };

const increase = (rows, col) => rows.map((r) => (r[col] !== null && r.NEE_night_mod_p ? r[col] / r.NEE_night_mod_p - 1 : null));
const mean = (v) => { const x = v.filter((y) => y !== null && Number.isFinite(y)); return x.length ? x.reduce((s, y) => s + y, 0) / x.length : null; };

async function draw() {
  const a = document.getElementById("a").value, b = document.getElementById("b").value, trs = document.getElementById("trs").value;
  setParams({ a, b, trs });
  const [ra, rb] = [resolve(a), resolve(b)];
  // (copies: the cached tables stay whole for the added-sites section)
  const [A, B] = (await Promise.all([get(ra), get(rb)])).map((T) => ({ ...T, future: filterSites(T.future || []), n_future: (T.future || []).length }));
  const swapped = [[a, ra], [b, rb]].filter(([x, y]) => x !== y);
  document.getElementById("swapnote").innerHTML = swapped.length
    ? `<div class="note">Added sites excluded: showing ${swapped.map(([x, y]) => `<b>${byId.get(y)?.label ?? y}</b> for ${byId.get(x)?.label ?? x}`).join("; ")} (the manuscript's sites only).</div>`
    : (!includeNew() && [A, B].some((T) => T.future.length < (T.n_future ?? 0)) ? `<div class="note">Added sites excluded from the figures; this analysis has no run over the manuscript's sites only, so its forests include them.</div>` : "");
  const stat = (v, k) => `<div class="stat"><div class="v">${v}</div><div class="k">${k}</div></div>`;
  const pct = (x) => (x === null ? "–" : `${(100 * x).toFixed(2)}%`);
  document.getElementById("stats").innerHTML = [
    stat(`${pct(mean(increase(A.future || [], "NEE_night_mod_f")))} → ${pct(mean(increase(B.future || [], "NEE_night_mod_f")))}`, "mean future increase, no thermal response"),
    stat(`${pct(mean(increase(A.future || [], "NEE_night_mod_fa")))} → ${pct(mean(increase(B.future || [], "NEE_night_mod_fa")))}`, "mean future increase, with total TAS"),
  ].join("");

  const vi = (T) => (T.varimp || []).filter((r) => r.TRS_type === trs);
  const vars = [...new Set([...vi(A), ...vi(B)].map((r) => r.var))];
  const val = (T, v) => (vi(T).find((r) => r.var === v) || {}).varImp ?? null;
  Plotly.newPlot("varimp", [
    { type: "bar", name: "reference", x: vars, y: vars.map((v) => val(A, v)), marker: { color: "#8c959f" } },
    { type: "bar", name: "analysis", x: vars, y: vars.map((v) => val(B, v)), marker: { color: "#0969da" } },
  ], baseLayout({ title: { text: `Variable importance, ${trs}`, font: { size: 13 } }, xaxis: { title: "predictor" },
    yaxis: { title: "% increase in MSE" }, barmode: "group" }), PLOT_CONFIG);

  const fa = new Map((A.future || []).map((r) => [r.site_ID, r]));
  const both = (B.future || []).filter((r) => fa.has(r.site_ID)).map((r) => ({ site_ID: r.site_ID, ref: increase([fa.get(r.site_ID)], "NEE_night_mod_fa")[0], new: increase([r], "NEE_night_mod_fa")[0],
    TAS_ref: fa.get(r.site_ID).TAS_tot, TAS_new: r.TAS_tot, TSmin_c_gs: r.TSmin_c_gs }));
  const lim = Math.max(...both.flatMap((r) => [Math.abs(r.ref ?? 0), Math.abs(r.new ?? 0)])) * 1.1 || 0.1;
  Plotly.newPlot("future", [
    { type: "scatter", mode: "lines", x: [-lim, lim], y: [-lim, lim], line: { dash: "dash", color: "#666" }, showlegend: false, hoverinfo: "skip" },
    { type: "scatter", mode: "markers", x: both.map((r) => r.ref), y: both.map((r) => r.new), text: both.map((r) => `${siteLabel(r.site_ID)}<br>ref ${pct(r.ref)}<br>new ${pct(r.new)}`), hoverinfo: "text",
      customdata: both.map((r) => r.site_ID), marker: { size: 8, color: "#0969da", symbol: both.map((r) => siteSymbol(r.site_ID)) }, showlegend: false },
  ], baseLayout({ title: { text: "Future nighttime respiration increase, with total TAS", font: { size: 13 } },
    xaxis: { title: "increase with total TAS, reference", tickformat: ".0%" },
    yaxis: { title: "increase with total TAS, analysis", tickformat: ".0%" } }), PLOT_CONFIG);
  document.getElementById("future").on("plotly_click", (e) => { location.href = `site.html?site=${e.points[0].customdata}`; });

  await drawAdded(b, trs, pct);

  const pd = (T) => (T.partial || []).filter((r) => r.TRS_type === trs);
  const pvars = [...new Set([...pd(A), ...pd(B)].map((r) => r.var))];
  const root = document.getElementById("partial");
  root.innerHTML = pvars.map((v) => `<div id="pd-${v}" class="plot" style="min-height:300px"></div>`).join("");
  const band = (rows, color, fill, name) => [
    { type: "scatter", mode: "lines", x: rows.map((r) => r.x), y: rows.map((r) => r.y950), line: { width: 0 }, showlegend: false, hoverinfo: "skip" },
    { type: "scatter", mode: "lines", x: rows.map((r) => r.x), y: rows.map((r) => r.y050), fill: "tonexty", fillcolor: fill, line: { width: 0 }, showlegend: false, hoverinfo: "skip" },
    { type: "scatter", mode: "lines", x: rows.map((r) => r.x), y: rows.map((r) => r.y), line: { color }, name },
  ];
  for (const v of pvars) {
    const ra = pd(A).filter((r) => r.var === v).sort((x, y) => x.x - y.x), rb = pd(B).filter((r) => r.var === v).sort((x, y) => x.x - y.x);
    Plotly.newPlot(`pd-${v}`, [...band(ra, "#666", "rgba(140,149,159,0.3)", "reference"), ...band(rb, "#0969da", "rgba(9,105,218,0.2)", "analysis")],
      baseLayout({ title: { text: v, font: { size: 12 } }, xaxis: { title: PREDICTORS[v.toLowerCase()] || v },
        yaxis: { title: `partial dependence of ${RESPONSES[trs] || trs}` }, margin: { l: 60, r: 10, t: 30, b: 40 },
        showlegend: false }), PLOT_CONFIG);
  }

  const f = numberFormatter(4);
  const cols = [
    { title: "Site", field: "site_ID", formatter: siteCell },
    { title: "TAS_tot ref", field: "TAS_ref", formatter: f, hozAlign: "right", headerFilter: false },
    { title: "TAS_tot", field: "TAS_new", formatter: f, hozAlign: "right", headerFilter: false },
    { title: "soil Tmin change, GS (°C)", field: "TSmin_c_gs", formatter: numberFormatter(2), hozAlign: "right", headerFilter: false },
    { title: "increase ref", field: "ref", formatter: (c) => pct(c.getValue()), hozAlign: "right", headerFilter: false },
    { title: "increase", field: "new", formatter: (c) => pct(c.getValue()), hozAlign: "right", headerFilter: false },
  ];
  if (tbl) tbl.replaceData(both); else tbl = table("table", both, cols, { height: "420px" });
}
for (const id of ["a", "b", "trs"]) document.getElementById(id).addEventListener("change", draw);
draw();

// The added sites' effect on steps 03-04: analysis `b` over all sites vs its
// run over the manuscript's sites only (whatever the toggle).
async function drawAdded(b, trs, pct) {
  const sec = document.getElementById("added");
  const orig = byId.get(b)?.original_sites_id;
  if (!orig) { sec.style.display = "none"; return; }
  sec.style.display = "";
  const [All, Orig] = [await get(b), await get(orig)];
  const fut = All.future || [];
  const groups = { "manuscript's sites": fut.filter((r) => !isNew(r.site_ID)), "all sites": fut, "added sites": fut.filter((r) => isNew(r.site_ID)) };
  const stat = (v, k) => `<div class="stat"><div class="v">${v}</div><div class="k">${k}</div></div>`;
  document.getElementById("addedstats").innerHTML = Object.entries(groups).map(([k, rows]) =>
    stat(`${pct(mean(increase(rows, "NEE_night_mod_f")))} / ${pct(mean(increase(rows, "NEE_night_mod_fa")))}`,
      `${k} (${rows.length}): mean future increase without / with total TAS`)).join("") +
    stat(`${pct(mean(increase(Orig.future || [], "NEE_night_mod_fa")))}`, "with total TAS, the manuscript's-sites run");
  const vi = (T) => (T.varimp || []).filter((r) => r.TRS_type === trs);
  const vars = [...new Set([...vi(Orig), ...vi(All)].map((r) => r.var))];
  const val = (T, v) => (vi(T).find((r) => r.var === v) || {}).varImp ?? null;
  Plotly.newPlot("addedvarimp", [
    { type: "bar", name: "manuscript's sites", x: vars, y: vars.map((v) => val(Orig, v)), marker: { color: "#8c959f" } },
    { type: "bar", name: "all sites", x: vars, y: vars.map((v) => val(All, v)), marker: { color: "#0969da" } },
  ], baseLayout({ title: { text: `Variable importance, ${trs}: manuscript's vs all sites`, font: { size: 13 } },
    xaxis: { title: "predictor" }, yaxis: { title: "% increase in MSE" }, barmode: "group", legend: { orientation: "h", y: -0.25 } }), PLOT_CONFIG);
  const drv = All.drivers || [];
  const col = trs === "TAS_direct" ? "TAS" : trs === "TAS_app" ? null : "TAS_tot";
  const tasOf = (r) => (col ? r[col] : (r.TAS_tot ?? NaN) - (r.TAS ?? NaN));
  Plotly.newPlot("addedtas", ["manuscript's sites", "added sites"].map((k, i) => {
    const rows = drv.filter((r) => isNew(r.site_ID) === (i === 1)).filter((r) => Number.isFinite(tasOf(r)));
    return { type: "box", name: k, y: rows.map(tasOf), text: rows.map((r) => r.site_ID), boxpoints: "all", jitter: 0.4, pointpos: 0,
      marker: { size: 5, symbol: i ? NEW_SYMBOL : "circle", color: i ? "#0969da" : "#8c959f" }, hovertemplate: "%{text}: %{y:.4f}<extra></extra>" };
  }), baseLayout({ title: { text: `${RESPONSES[trs] || trs} of the sites`, font: { size: 13 } }, yaxis: { title: "TAS (°C⁻¹)", zeroline: true }, showlegend: false }), PLOT_CONFIG);
  const skill = All.new_sites_effect || [];
  document.getElementById("addedskill").innerHTML = skill.length ? `<table class="plain"><tr><th>TAS</th><th>forest fitted to (n)</th><th>scored on added sites (n)</th>
    <th>r² on added sites</th><th>r</th><th>RMSE</th><th>bias</th><th>OOB r², manuscript's sites</th><th>mean ± sd, manuscript's</th><th>mean ± sd, added</th></tr>${
    skill.map((r) => `<tr><td>${RESPONSES[r.type] || r.type}</td><td>${r.n_original}</td><td>${r.n_added}</td><td>${fmt(r.r2, 2)}</td><td>${fmt(r.r, 2)}</td>
      <td>${fmt(r.rmse, 4)}</td><td>${fmt(r.bias, 4)}</td><td>${fmt(r.oob_r2_original, 2)}</td><td>${fmt(r.mean_original, 4)} ± ${fmt(r.sd_original, 4)}</td>
      <td>${fmt(r.mean_added, 4)} ± ${fmt(r.sd_added, 4)}</td></tr>`).join("")}</table>` : "";
}
