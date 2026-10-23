import { header, loadJSON, compare, summarise, table, fmt, params, setParams, baseLayout, PLOT_CONFIG, CLASS_COLORS,
  numberFormatter, tasClass } from "./common.js";

header("update.html");
const update = await loadJSON("data/update.json");
const sets = await loadJSON("data/resultsets.json");
const label = (id) => (sets.find((s) => s.id === id) || { label: id }).label;
const cache = {};
const getTas = async (id) => (cache[id] ??= await loadJSON(`data/tas/${id}.json`));
const getTs = async (id) => (cache[`ts:${id}`] ??= await loadJSON(`data/ts/${id}.json`).catch(() => null));

if (!update.runs || !update.runs.length) {
  document.querySelector("main").insertAdjacentHTML("beforeend", `<p class="note">No latest-data run saved yet.</p>`);
  throw new Error("no update runs");
}
const runSel = document.getElementById("run");
for (const r of update.runs) runSel.insertAdjacentHTML("beforeend", `<option value="${r.run}">${label(r.run)}</option>`);
const p = params();
runSel.value = p.get("run") || update.runs[update.runs.length - 1].run;
document.getElementById("model").value = p.get("model") || "total";
if (p.get("threshold")) document.getElementById("threshold").value = p.get("threshold");
const stat = (v, k) => `<div class="stat"><div class="v">${v}</div><div class="k">${k}</div></div>`;
let siteTable, fillsTable, columnsTable;

async function draw() {
  const run = runSel.value, model = document.getElementById("model").value;
  const threshold = Number(document.getElementById("threshold").value);
  setParams({ run, model, threshold });
  const entry = update.runs.find((r) => r.run === run);
  const ref = entry.reference ?? update.reference;
  const [pub, old, now] = [await getTas("published"), await getTas(ref), await getTas(run)];
  const method = new Map(compare(pub, old, model, threshold).map((r) => [r.site_ID, r]));
  const data = compare(old, now, model, threshold);
  const inputs = new Map((entry.inputs || []).map((r) => [r.site_ID, r]));
  const sm = summarise([...method.values()].filter((r) => r.dTAS !== null));
  const sd = summarise(data.filter((r) => r.dTAS !== null));
  document.getElementById("stats").innerHTML = [
    stat(`${fmt(sm.median_abs, 4)} / ${fmt(sd.median_abs, 4)}`, "median |ΔTAS|: method / data"),
    stat(`${sm.changed} / ${sd.changed}`, `sites |ΔTAS| ≥ ${threshold}: method / data`),
    stat(`${sm.transitions} / ${sd.transitions}`, "class changes: method / data"),
    stat(`${sm.flips.length} / ${sd.flips.length}`, "significant sign flips: method / data"),
    stat(`${data.filter((r) => r.TAS_new === null && r.TAS_ref !== null).length}`, "sites without a TAS on the latest data"),
    stat(`${inputs.size}`, "sites whose TAS inputs changed"),
  ].join("");
  const flips = [...sm.flips.map((s) => `${s} (method)`), ...sd.flips.map((s) => `${s} (data)`)];
  document.getElementById("flipnote").innerHTML = flips.length
    ? `<div class="note"><span class="flip">Significant sign flips:</span> ${flips.join(", ")}</div>` : "";

  // method vs data effect
  const rows = data.filter((r) => r.dTAS !== null).map((r) => ({ ...r, method: method.get(r.site_ID)?.dTAS ?? null }))
    .filter((r) => r.method !== null);
  const groups = Object.keys(CLASS_COLORS).map((c) => {
    const g = rows.filter((r) => r.class_new === c);
    return { type: "scatter", mode: "markers", name: `latest data: ${c}`, x: g.map((r) => r.method), y: g.map((r) => r.dTAS),
      text: g.map((r) => `${r.site_ID}<br>method Δ ${fmt(r.method, 5)}<br>data Δ ${fmt(r.dTAS, 5)}<br>${r.class_ref} → ${r.class_new}`),
      hoverinfo: "text", customdata: g.map((r) => r.site_ID),
      marker: { color: CLASS_COLORS[c], size: g.map((r) => (r.flip ? 13 : 8)), line: { width: g.map((r) => (r.flip ? 2 : 0.5)), color: "#000" } } };
  });
  const lim = Math.max(...rows.flatMap((r) => [Math.abs(r.method), Math.abs(r.dTAS)])) * 1.1 || 0.05;
  Plotly.newPlot("effects", groups, baseLayout({
    xaxis: { title: "method effect: ΔTAS, refactored − published (°C⁻¹)", range: [-lim, lim], zeroline: true },
    yaxis: { title: "data effect: ΔTAS, latest − published data (°C⁻¹)", range: [-lim, lim], zeroline: true },
    legend: { orientation: "h", y: -0.2 } }), PLOT_CONFIG);
  const withInputs = rows.filter((r) => inputs.has(r.site_ID) && inputs.get(r.site_ID).n_night_ref);
  Plotly.newPlot("records", [{ type: "scatter", mode: "markers",
    x: withInputs.map((r) => 100 * (inputs.get(r.site_ID).n_night_new / inputs.get(r.site_ID).n_night_ref - 1)), y: withInputs.map((r) => r.dTAS),
    text: withInputs.map((r) => { const i = inputs.get(r.site_ID); return `${r.site_ID}<br>night records ${i.n_night_ref} → ${i.n_night_new}<br>years ${i.n_years_ref} → ${i.n_years_new}<br>data Δ ${fmt(r.dTAS, 5)}`; }),
    hoverinfo: "text", customdata: withInputs.map((r) => r.site_ID), marker: { color: withInputs.map((r) => CLASS_COLORS[r.class_new]), size: 8, line: { width: 0.5, color: "#333" } } }],
  baseLayout({ xaxis: { title: "change in night-time records (%)", zeroline: true }, yaxis: { title: "data effect: ΔTAS (°C⁻¹)", zeroline: true } }), PLOT_CONFIG);
  for (const id of ["effects", "records"]) document.getElementById(id).on("plotly_click", (e) => { location.href = `site.html?site=${e.points[0].customdata}`; });

  // fit matrix: data effect by column
  const [tsOld, tsNew] = [await getTs(ref), await getTs(run)];
  if (tsOld && tsNew && tsNew[`outcome_${model}`]) {
    const key = (r) => `${r.column}|${r.site_ID}`;
    const o = new Map(tsOld[`outcome_${model}`].map((r) => [key(r), r]));
    const cols = [...new Set(tsNew[`outcome_${model}`].map((r) => r.column))];
    const per = cols.map((c) => {
      const pairs = tsNew[`outcome_${model}`].filter((r) => r.column === c).map((r) => [o.get(key(r)), r]).filter(([a, b]) => a && a.TAS !== null && b.TAS !== null);
      const d = pairs.map(([a, b]) => b.TAS - a.TAS);
      const cl = pairs.map(([a, b]) => [tasClass(a.TAS, a.TASp), tasClass(b.TAS, b.TASp)]);
      const sorted = d.map(Math.abs).sort((x, y) => x - y);
      return { column: c, d, sites: pairs.map(([, b]) => b.site_ID), n: d.length, median_abs: sorted.length ? sorted[Math.floor(sorted.length / 2)] : null,
        changed: d.filter((x) => Math.abs(x) >= threshold).length, transitions: cl.filter(([a, b]) => a !== b).length,
        flips: cl.filter(([a, b]) => (a === "sig-" && b === "sig+") || (a === "sig+" && b === "sig-")).length };
    });
    Plotly.newPlot("columns", per.map((c) => ({ type: "box", name: c.column, y: c.d, text: c.sites, boxpoints: "all", jitter: 0.4, pointpos: 0,
      marker: { size: 3 }, hovertemplate: "%{text}: %{y:.4f}<extra></extra>" })),
    baseLayout({ showlegend: false, xaxis: { title: "soil-temperature column" }, yaxis: { title: "data effect: ΔTAS, latest − published data (°C⁻¹)", zeroline: true } }), PLOT_CONFIG);
    const ccols = [{ title: "column", field: "column" }, { title: "sites", field: "n", hozAlign: "right", headerFilter: false },
      { title: "median |ΔTAS|", field: "median_abs", formatter: numberFormatter(4), hozAlign: "right", headerFilter: false },
      { title: `|ΔTAS| ≥ threshold`, field: "changed", hozAlign: "right", headerFilter: false },
      { title: "class changes", field: "transitions", hozAlign: "right", headerFilter: false },
      { title: "sign flips", field: "flips", hozAlign: "right", headerFilter: false }];
    if (columnsTable) columnsTable.replaceData(per); else columnsTable = table("columns-table", per, ccols, { height: "auto" });
  } else {
    document.getElementById("columns").innerHTML = `<p class="note">This run has no fit matrix.</p>`;
  }

  // per-site table
  const siteRows = data.map((r) => {
    const m = method.get(r.site_ID) || {};
    const i = inputs.get(r.site_ID) || {};
    return { site_ID: r.site_ID, published: m.TAS_ref ?? null, refactored: r.TAS_ref, latest: r.TAS_new, method: m.dTAS ?? null, data: r.dTAS,
      classes: `${m.class_ref ?? "NA"} → ${r.class_ref} → ${r.class_new}`, flip: r.flip, inputs: i.changed || "",
      n_night: i.n_night_ref !== undefined ? `${i.n_night_ref} → ${i.n_night_new}` : "",
      years: i.n_years_ref !== undefined ? `${i.n_years_ref} → ${i.n_years_new}` : "" };
  });
  const f = numberFormatter(4);
  const cols = [
    { title: "Site", field: "site_ID", formatter: (c) => `<a href="site.html?site=${c.getValue()}">${c.getValue()}</a>` },
    { title: "published", field: "published", formatter: f, hozAlign: "right", headerFilter: false },
    { title: "refactored", field: "refactored", formatter: f, hozAlign: "right", headerFilter: false },
    { title: "latest data", field: "latest", formatter: f, hozAlign: "right", headerFilter: false },
    { title: "Δ method", field: "method", formatter: f, hozAlign: "right", headerFilter: false },
    { title: "Δ data", field: "data", formatter: f, hozAlign: "right", headerFilter: false },
    { title: "class: published → refactored → latest", field: "classes" },
    { title: "data flip", field: "flip", formatter: "tickCross", headerFilter: "tickCross" },
    { title: "inputs changed", field: "inputs" },
    { title: "night records", field: "n_night" },
    { title: "years", field: "years" },
  ];
  if (siteTable) siteTable.replaceData(siteRows); else siteTable = table("table", siteRows, cols, { height: "520px" });

  // fills
  const fills = entry.fills || [];
  if (fills.length) {
    const fcols = [{ title: "Site", field: "site_ID" }, { title: "product", field: "product" }, { title: "variable", field: "variable" },
      { title: "records filled", field: "n", hozAlign: "right", headerFilter: false }];
    if (fillsTable) fillsTable.replaceData(fills); else fillsTable = table("fills", fills, fcols, { height: "360px" });
  } else {
    document.getElementById("fills").innerHTML = `<p class="note">This run has no record of filled soil drivers (run before the fill).</p>`;
  }

  // ERA5 sensitivity
  const near = (update.era5_nearest || []).find((e) => e.reference === run);
  const offsets = entry.era5_offsets || [];
  if (near) {
    const rowsE = compare(await getTas(near.reference), await getTas(near.run), model, threshold)
      .filter((r) => r.TAS_ref !== null && r.TAS_new !== null && (cache[near.run][model] || []).some((x) => x.site_ID === r.site_ID));
    const se = summarise(rowsE);
    document.getElementById("era5-stats").innerHTML = [stat(se.n, "ERA5 sites"), stat(fmt(se.median_abs, 5), "median |ΔTAS|"),
      stat(se.changed, `|ΔTAS| ≥ ${threshold}`), stat(se.transitions, "class changes"), stat(se.flips.length, "significant sign flips")].join("");
    const off = new Map(offsets.map((o) => [o.site_ID ?? o.name, o.offset]));
    const limE = Math.max(...rowsE.flatMap((r) => [Math.abs(r.TAS_ref), Math.abs(r.TAS_new)])) * 1.1 || 0.05;
    Plotly.newPlot("era5-scatter", [
      { type: "scatter", mode: "lines", x: [-limE, limE], y: [-limE, limE], line: { dash: "dash", color: "#666" }, showlegend: false, hoverinfo: "skip" },
      { type: "scatter", mode: "markers", x: rowsE.map((r) => r.TAS_ref), y: rowsE.map((r) => r.TAS_new),
        text: rowsE.map((r) => `${r.site_ID}<br>offset ${fmt(off.get(r.site_ID), 2)} pp<br>Δ ${fmt(r.dTAS, 5)}<br>${r.transition}`), hoverinfo: "text",
        marker: { size: 9, color: rowsE.map((r) => off.get(r.site_ID) ?? 0), colorscale: "RdBu", cmid: 0, line: { width: 0.5, color: "#333" },
          colorbar: { title: { text: "offset (pp)" }, thickness: 12 } } }],
    baseLayout({ xaxis: { title: `TAS, offset-corrected (°C⁻¹)`, range: [-limE, limE] }, yaxis: { title: "TAS, uncorrected nearest cell (°C⁻¹)", range: [-limE, limE] }, showlegend: false }), PLOT_CONFIG);
    Plotly.newPlot("era5-offsets", [{ type: "scatter", mode: "markers", x: rowsE.map((r) => off.get(r.site_ID) ?? null), y: rowsE.map((r) => r.dTAS),
      text: rowsE.map((r) => r.site_ID), hovertemplate: "%{text}: offset %{x:.2f}, Δ %{y:.5f}<extra></extra>", marker: { size: 8, color: "#0969da" } }],
    baseLayout({ xaxis: { title: "ERA5 offset applied (percentage points of soil water)", zeroline: true }, yaxis: { title: "ΔTAS, uncorrected − corrected (°C⁻¹)", zeroline: true } }), PLOT_CONFIG);
  } else {
    const others = (update.era5_nearest || []).map((e) => label(e.reference));
    document.getElementById("era5-stats").innerHTML = `<p class="note">No ERA5 sensitivity run for this run${others.length
      ? ` (one exists for: ${others.join(", ")}; choose it above)` : ""}.</p>`;
    for (const id of ["era5-scatter", "era5-offsets"]) Plotly.purge(id);
  }
}
for (const id of ["run", "model", "threshold"]) document.getElementById(id).addEventListener("change", draw);
draw();
