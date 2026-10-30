import { header, loadJSON, table, params, setParams, baseLayout, PLOT_CONFIG, numberFormatter, tasClass, fmt, loadScreen, VERDICT_COLORS, siteCell, filterSites, siteSymbol, siteLabel, siteHTML } from "./common.js";

header("matrix.html");
const runs = await loadJSON("data/ts_runs.json");
const sets = await loadJSON("data/resultsets.json");
const label = (id) => (sets.find((s) => s.id === id) || { label: id }).label;
const data = {};
for (const id of runs) data[id] = await loadJSON(`data/ts/${id}.json`);
const withFits = runs.filter((id) => (data[id].outcome_total || []).length || (data[id].ta_outcome_total || []).length);
const runSel = document.getElementById("run");
if (!withFits.length) {
  document.querySelector("main").insertAdjacentHTML("beforeend", `<p class="note">No run with the fit matrix yet.</p>`);
  throw new Error("no runs");
}
for (const id of withFits) runSel.insertAdjacentHTML("beforeend", `<option value="${id}">${label(id)}</option>`);
const p = params();
runSel.value = p.get("run") || withFits[withFits.length - 1];
document.getElementById("model").value = p.get("model") || "total";
document.getElementById("sensor").value = p.get("sensor") || "all";
document.getElementById("group").value = p.get("group") || "all";
const screen = await loadScreen();
const verdictOf = (site) => screen?.sensor.get(site)?.verdict || "NA";
let tbl, colTbl;

function fillRef(columns) {
  const sel = document.getElementById("ref");
  const keep = sel.value || p.get("ref") || "TS_manuscript";
  sel.innerHTML = columns.map((c) => `<option value="${c}">${c}</option>`).join("");
  sel.value = columns.includes(keep) ? keep : columns[0];
}

async function draw() {
  const run = runSel.value, model = document.getElementById("model").value;
  // soil-temperature columns, then the air-temperature ones (R/ta-columns.R)
  const group = document.getElementById("group").value;
  const rows = [...(group !== "ta" ? data[run][`outcome_${model}`] || [] : []),
    ...(group !== "ts" ? data[run][`ta_outcome_${model}`] || [] : [])];
  const columns = [...new Set(rows.map((r) => r.column))];
  fillRef(columns);
  const ref = document.getElementById("ref").value;
  const sensor = document.getElementById("sensor").value;
  setParams({ run, model, ref, sensor, group });
  const keep = (s) => sensor === "all" || (sensor === "notBAD" ? verdictOf(s) !== "BAD" : verdictOf(s) === sensor);
  const sites = filterSites([...new Set(rows.map((r) => r.site_ID))].filter(keep).sort());
  const get = new Map(rows.map((r) => [`${r.site_ID}|${r.column}`, r]));
  const cls = (r) => (r ? tasClass(r.TAS, r.TASp) : "NA");

  // per site: classes over columns, flips against the reference
  const siteRows = sites.map((s) => {
    const rr = get.get(`${s}|${ref}`);
    const cr = cls(rr);
    const out = { site_ID: s, sensor: verdictOf(s), ref_TAS: rr?.TAS ?? null, ref_class: cr, flips: [], changes: [] };
    const vals = [];
    for (const c of columns) {
      const r = get.get(`${s}|${c}`);
      if (!r || r.TAS === null) continue;
      vals.push(r.TAS);
      const k = cls(r);
      if ((cr === "sig-" && k === "sig+") || (cr === "sig+" && k === "sig-")) out.flips.push(c);
      else if (k !== cr && cr !== "NA") out.changes.push(c);
    }
    out.min = vals.length ? Math.min(...vals) : null;
    out.max = vals.length ? Math.max(...vals) : null;
    out.n_pos = columns.filter((c) => cls(get.get(`${s}|${c}`)) === "sig+").length;
    out.n_neg = columns.filter((c) => cls(get.get(`${s}|${c}`)) === "sig-").length;
    out.flip_columns = out.flips.join(", ");
    out.change_columns = out.changes.join(", ");
    return out;
  }).sort((a, b) => b.flips.length - a.flips.length || b.changes.length - a.changes.length);

  const stat = (v, k) => `<div class="stat"><div class="v">${v}</div><div class="k">${k}</div></div>`;
  const nflip = siteRows.filter((r) => r.flips.length).length, nchg = siteRows.filter((r) => r.changes.length || r.flips.length).length;
  const both = siteRows.filter((r) => r.n_pos && r.n_neg).length;
  document.getElementById("stats").innerHTML = [stat(sites.length, "sites"), stat(columns.length, "columns"),
    stat(nchg, `sites whose class changes vs ${ref}`), stat(nflip, "sites with a significant sign flip"),
    stat(both, "sites significantly + under one column and − under another")].join("");
  const flipSites = siteRows.filter((r) => r.flips.length);
  document.getElementById("interpretation").innerHTML = flipSites.length
    ? `<div class="note"><span class="flip">Significant sign flips against ${ref}:</span> ${flipSites.map((r) => `${siteHTML(r.site_ID)} (${r.flips.join(", ")})`).join("; ")}</div>` : "";

  // heatmap
  const order = siteRows.map((r) => r.site_ID);
  const z = order.map((s) => columns.map((c) => get.get(`${s}|${c}`)?.TAS ?? null));
  const vmax = Math.max(...z.flat().filter((v) => v !== null).map(Math.abs), 1e-6);
  const marks = { x: [], y: [], text: [], color: [] };
  const flipsXY = { x: [], y: [] };
  for (const s of order) for (const c of columns) {
    const r = get.get(`${s}|${c}`); const k = cls(r);
    if (k === "sig+" || k === "sig-") { marks.x.push(c); marks.y.push(s); marks.text.push(k === "sig+" ? "▲" : "▼"); }
    if (siteRows.find((x) => x.site_ID === s).flips.includes(c)) { flipsXY.x.push(c); flipsXY.y.push(s); }
  }
  const hover = order.map((s) => columns.map((c) => { const r = get.get(`${s}|${c}`); return r ? `${s} · ${c}<br>TAS ${fmt(r.TAS)} (p ${fmt(r.TASp, 3)})` : `${s} · ${c}<br>no fit`; }));
  Plotly.newPlot("heat", [
    { type: "heatmap", x: columns, y: order, z, text: hover, hoverinfo: "text", colorscale: "RdBu", zmin: -vmax, zmax: vmax, colorbar: { title: { text: "TAS" }, thickness: 12 } },
    { type: "scatter", mode: "text", x: marks.x, y: marks.y, text: marks.text, textfont: { size: 9, color: "#111" }, hoverinfo: "skip", showlegend: false },
    { type: "scatter", mode: "markers", x: flipsXY.x, y: flipsXY.y, marker: { symbol: "square-open", size: 14, color: "#d1242f", line: { width: 2 } }, hoverinfo: "skip", showlegend: false },
  ], baseLayout({ margin: { l: 80, r: 20, t: 20, b: 150 }, xaxis: { tickangle: -45, title: "temperature column" },
    yaxis: { autorange: "reversed", tickfont: { size: 9 }, title: "site", tickvals: order, ticktext: order.map(siteLabel) },
    plot_bgcolor: "#e5e5e5", height: Math.max(450, order.length * 12 + 220) }), PLOT_CONFIG);
  document.getElementById("heat").on("plotly_click", (e) => { if (e.points[0].y) location.href = `site.html?site=${e.points[0].y}`; });

  // each column vs reference: summary, then small multiples
  const others = columns.filter((c) => c !== ref);
  const isFlipPair = (x, y) => (x === "sig-" && y === "sig+") || (x === "sig+" && y === "sig-");
  const colRows = others.map((c) => {
    const pts = sites.map((s) => [get.get(`${s}|${ref}`), get.get(`${s}|${c}`)])
      .filter(([a, b]) => a && b && a.TAS !== null && b.TAS !== null);
    const x = pts.map(([a]) => a.TAS), y = pts.map(([, b]) => b.TAS), n = pts.length;
    const mean = (v) => v.reduce((t, u) => t + u, 0) / v.length;
    const mx = mean(x), my = mean(y);
    const sxy = x.reduce((t, u, i) => t + (u - mx) * (y[i] - my), 0);
    const r = sxy / Math.sqrt(x.reduce((t, u) => t + (u - mx) ** 2, 0) * y.reduce((t, u) => t + (u - my) ** 2, 0));
    const ad = y.map((v, i) => Math.abs(v - x[i])).sort((a, b) => a - b);
    return {
      column: c, n, r: n > 2 ? r : null, mean_ref: n ? mx : null, mean_col: n ? my : null, mean_d: n ? my - mx : null,
      median_abs_d: n ? ad[Math.floor(n / 2)] : null,
      changes: pts.filter(([a, b]) => cls(a) !== cls(b)).length,
      flips: pts.filter(([a, b]) => isFlipPair(cls(a), cls(b))).length,
      n_pos: pts.filter(([, b]) => cls(b) === "sig+").length, n_neg: pts.filter(([, b]) => cls(b) === "sig-").length,
    };
  });
  const f3 = numberFormatter(3), f4 = numberFormatter(4);
  const colCols = [
    { title: "column", field: "column" },
    { title: "sites", field: "n", hozAlign: "right", headerFilter: false },
    { title: `r with ${ref}`, field: "r", formatter: f3, hozAlign: "right", headerFilter: false },
    { title: `mean TAS (${ref})`, field: "mean_ref", formatter: f4, hozAlign: "right", headerFilter: false },
    { title: "mean TAS (column)", field: "mean_col", formatter: f4, hozAlign: "right", headerFilter: false },
    { title: "mean ΔTAS", field: "mean_d", formatter: f4, hozAlign: "right", headerFilter: false },
    { title: "median |ΔTAS|", field: "median_abs_d", formatter: f4, hozAlign: "right", headerFilter: false },
    { title: "# sig+", field: "n_pos", hozAlign: "right", headerFilter: false },
    { title: "# sig−", field: "n_neg", hozAlign: "right", headerFilter: false },
    { title: "class changes", field: "changes", hozAlign: "right", headerFilter: false },
    { title: "sign flips", field: "flips", hozAlign: "right", headerFilter: false },
  ];
  if (colTbl) colTbl.replaceData(colRows); else colTbl = table("columns", colRows, colCols, { height: "auto" });
  const root = document.getElementById("pairs");
  root.className = "grid2";
  root.innerHTML = others.map((c) => `<div id="pair-${c}" class="plot" style="min-height:280px"></div>`).join("");
  for (const c of others) {
    const pts = sites.map((s) => [get.get(`${s}|${ref}`), get.get(`${s}|${c}`), s]).filter(([a, b]) => a && b && a.TAS !== null && b.TAS !== null);
    const lim = Math.max(...pts.flatMap(([a, b]) => [Math.abs(a.TAS), Math.abs(b.TAS)]), 1e-3) * 1.1;
    const isFlip = ([a, b]) => { const x = cls(a), y = cls(b); return (x === "sig-" && y === "sig+") || (x === "sig+" && y === "sig-"); };
    Plotly.newPlot(`pair-${c}`, [
      { type: "scatter", mode: "lines", x: [-lim, lim], y: [-lim, lim], line: { dash: "dash", color: "#999" }, hoverinfo: "skip", showlegend: false },
      { type: "scatter", mode: "markers", x: pts.map(([a]) => a.TAS), y: pts.map(([, b]) => b.TAS), text: pts.map(([a, b, s]) => `${siteLabel(s)}<br>${ref} ${fmt(a.TAS)} (${cls(a)})<br>${c} ${fmt(b.TAS)} (${cls(b)})`),
        hoverinfo: "text", customdata: pts.map(([, , s]) => s),
        marker: { size: pts.map((x) => (isFlip(x) ? 11 : 7)), color: pts.map((x) => (isFlip(x) ? "#d1242f" : "#0969da")), symbol: pts.map(([, , s]) => siteSymbol(s)),
          line: { width: 0.5, color: "#333" } }, showlegend: false },
    ], baseLayout({ title: { text: c, font: { size: 12 } }, xaxis: { title: `TAS (°C⁻¹), ${ref}`, range: [-lim, lim] }, yaxis: { title: `TAS (°C⁻¹), ${c}`, range: [-lim, lim] }, margin: { l: 55, r: 10, t: 30, b: 40 } }), PLOT_CONFIG);
    document.getElementById(`pair-${c}`).on("plotly_click", (e) => { location.href = `site.html?site=${e.points[0].customdata}`; });
  }

  const f = numberFormatter(4);
  const cols = [
    { title: "Site", field: "site_ID", formatter: siteCell },
    { title: "sensor", field: "sensor", formatter: (c) => `<span style="color:${VERDICT_COLORS[c.getValue()] || "#000"};font-weight:600">${c.getValue()}</span>` },
    { title: `TAS (${ref})`, field: "ref_TAS", formatter: f, hozAlign: "right", headerFilter: false },
    { title: "class", field: "ref_class" },
    { title: "min TAS", field: "min", formatter: f, hozAlign: "right", headerFilter: false },
    { title: "max TAS", field: "max", formatter: f, hozAlign: "right", headerFilter: false },
    { title: "# sig+", field: "n_pos", hozAlign: "right", headerFilter: false },
    { title: "# sig−", field: "n_neg", hozAlign: "right", headerFilter: false },
    { title: "significant sign flips", field: "flip_columns" },
    { title: "other class changes", field: "change_columns" },
  ];
  if (tbl) tbl.replaceData(siteRows); else tbl = table("table", siteRows, cols, { height: "520px" });
}
for (const id of ["run", "model", "ref", "sensor", "group"]) document.getElementById(id).addEventListener("change", draw);
draw();
