import { header, table, params, setParams, baseLayout, PLOT_CONFIG, numberFormatter, tasClass, fmt, loadScreen, VERDICT_COLORS,
  siteCell, siteHTML, loadMain, tempSourcePicker, tempSource, columnLabel, shortLabel, SITES } from "./common.js";

header("matrix.html");
const main = await loadMain();
const p = params();
document.getElementById("model").value = p.get("model") || "total";
document.getElementById("sensor").value = p.get("sensor") || "all";
const screen = await loadScreen(main.run);
const verdictOf = (site) => screen?.sensor.get(site)?.verdict || "NA";
const VERDICTS = { GOOD: "good", SUSPECT: "suspect", BAD: "bad", NA: "–" };
const name = (s) => SITES.get(s)?.site_name || "";
let tbl, colTbl;

async function draw() {
  const model = document.getElementById("model").value;
  // every soil- and air-temperature source; the reference is the selected one
  const ref = tempSource();
  const columns = main.columns;
  const rows = main.tas[model].filter((r) => columns.includes(r.column));
  const sensor = document.getElementById("sensor").value;
  setParams({ model, sensor });
  const keep = (s) => sensor === "all" || (sensor === "notBAD" ? verdictOf(s) !== "BAD" : verdictOf(s) === sensor);
  const sites = [...new Set(rows.map((r) => r.site_ID))].filter(keep).sort();
  const get = new Map(rows.map((r) => [`${r.site_ID}|${r.column}`, r]));
  const cls = (r) => (r ? tasClass(r.TAS, r.TASp) : "NA");

  // per site: classes over columns, flips against the reference
  const siteRows = sites.map((s) => {
    const rr = get.get(`${s}|${ref}`);
    const cr = cls(rr);
    const out = { site_ID: s, name: name(s), sensor: verdictOf(s), ref_TAS: rr?.TAS ?? null, ref_class: cr, flips: [], changes: [] };
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
    out.flip_columns = out.flips.map(shortLabel).join(", ");
    out.change_columns = out.changes.map(shortLabel).join(", ");
    return out;
  }).sort((a, b) => b.flips.length - a.flips.length || b.changes.length - a.changes.length);

  const stat = (v, k) => `<div class="stat"><div class="v">${v}</div><div class="k">${k}</div></div>`;
  const nflip = siteRows.filter((r) => r.flips.length).length, nchg = siteRows.filter((r) => r.changes.length || r.flips.length).length;
  const both = siteRows.filter((r) => r.n_pos && r.n_neg).length;
  document.getElementById("stats").innerHTML = [stat(sites.length, "sites"), stat(columns.length, "temperature sources"),
    stat(nchg, `sites whose class changes against ${columnLabel(ref)}`), stat(nflip, "sites with a significant sign flip"),
    stat(both, "sites significantly + with one source and − with another")].join("");
  const flipSites = siteRows.filter((r) => r.flips.length);
  document.getElementById("interpretation").innerHTML = flipSites.length
    ? `<div class="note"><span class="flip">Significant sign flips against ${columnLabel(ref)}:</span> ${flipSites.map((r) => `${siteHTML(r.site_ID)} (${r.flips.map(shortLabel).join(", ")})`).join("; ")}</div>` : "";

  // heatmap: sites grouped by their soil sensor's screen, a labelled gap between groups
  const GROUPS = [["GOOD", "Soil sensor good"], ["SUSPECT", "Soil sensor suspect"], ["BAD", "Soil sensor bad"],
    ["NA", "No soil sensor screened"]];
  const GAP = 3;
  const ys = new Map(), labels = [], boxes = [];
  let y = 0;
  for (const [v, text] of GROUPS) {
    const members = siteRows.filter((r) => (VERDICTS[r.sensor] ? r.sensor : "NA") === v).map((r) => r.site_ID);
    if (!members.length) continue;
    y += GAP;
    labels.push({ text: `<b>${text}</b> (${members.length})`, y: y - 1.1 });
    boxes.push([y, y + members.length - 1]);
    for (const s of members) ys.set(s, y++);
  }
  const order = [...ys.keys()];
  const yOf = (s) => ys.get(s);
  const z = order.map((s) => columns.map((c) => get.get(`${s}|${c}`)?.TAS ?? null));
  // colour range: the 98th percentile of |TAS| (the few larger values saturate)
  const abs = z.flat().filter((v) => v !== null).map(Math.abs).sort((a, b) => a - b);
  const vmax = Math.max(abs[Math.floor(0.98 * (abs.length - 1))] ?? 0, 1e-6);
  const marks = { x: [], y: [], text: [] };
  const flipsXY = { x: [], y: [] };
  const flipsOf = new Map(siteRows.map((r) => [r.site_ID, r.flips]));
  for (const s of order) for (const c of columns) {
    const k = cls(get.get(`${s}|${c}`));
    if (k === "sig+" || k === "sig-") { marks.x.push(c); marks.y.push(yOf(s)); marks.text.push(k === "sig+" ? "▲" : "▼"); }
    if (flipsOf.get(s).includes(c)) { flipsXY.x.push(c); flipsXY.y.push(yOf(s)); }
  }
  const hover = order.map((s) => columns.map((c) => { const r = get.get(`${s}|${c}`); return r ? `${s} ${name(s)}<br>${columnLabel(c, false)}<br>TAS ${fmt(r.TAS)} (p ${fmt(r.TASp, 3)})` : `${s}<br>${columnLabel(c, false)}<br>no estimate`; }));
  // one heatmap per group (so the gaps stay empty), on one colour scale
  const heat = boxes.map(([y0, y1], i) => {
    const idx = order.map((s, j) => j).filter((j) => yOf(order[j]) >= y0 && yOf(order[j]) <= y1);
    return { type: "heatmap", x: columns, y: idx.map((j) => yOf(order[j])), z: idx.map((j) => z[j]), text: idx.map((j) => hover[j]),
      hoverinfo: "text", coloraxis: "coloraxis", xgap: 0, ygap: 0 };
  });
  const nrow = y;
  const height = Math.round(nrow * 12 + 260);
  document.getElementById("heat").style.height = `${height}px`;
  Plotly.react("heat", [
    ...heat,
    { type: "scatter", mode: "text", x: marks.x, y: marks.y, text: marks.text, textfont: { size: 9, color: "#111" }, hoverinfo: "skip", showlegend: false },
    { type: "scatter", mode: "markers", x: flipsXY.x, y: flipsXY.y, marker: { symbol: "square-open", size: 14, color: "#d1242f", line: { width: 2 } }, hoverinfo: "skip", showlegend: false },
  ], baseLayout({ margin: { l: 80, r: 20, t: 10, b: 10 },
    coloraxis: { colorscale: [[0, "#053061"], [0.25, "#4393c3"], [0.5, "#f7f7f7"], [0.75, "#d6604d"], [1, "#67001f"]], cmin: -vmax, cmax: vmax, colorbar: { title: { text: "TAS" }, thickness: 12, ticks: "outside", len: 400, lenmode: "pixels", y: 1, yanchor: "top" } },
    xaxis: { tickangle: -45, title: "temperature source", tickvals: columns, ticktext: columns.map(shortLabel), side: "bottom",
      range: [-0.5, columns.length - 0.5], showgrid: false, zeroline: false },
    yaxis: { range: [nrow - 0.5, 0.3], tickfont: { size: 9 }, tickvals: order.map(yOf), ticktext: order, showgrid: false, zeroline: false },
    // grey behind each group (a missing estimate), white in the gaps
    shapes: boxes.map(([y0, y1]) => ({ type: "rect", layer: "below", xref: "x", yref: "y", x0: -0.5, x1: columns.length - 0.5,
      y0: y0 - 0.5, y1: y1 + 0.5, fillcolor: "#bdbdbd", line: { width: 0 } })),
    annotations: labels.map((l) => ({ text: l.text, xref: "paper", x: 0, xanchor: "left", y: l.y, yref: "y", yanchor: "middle",
      showarrow: false, font: { size: 12 } })),
    plot_bgcolor: "#fff", height }), PLOT_CONFIG);
  const heatDiv = document.getElementById("heat");
  heatDiv.siteAt = (yv) => order.find((s) => yOf(s) === yv);
  if (!heatDiv.dataset.bound) {
    heatDiv.on("plotly_click", (e) => { const s = heatDiv.siteAt(e.points[0].y); if (s) location.href = `site.html?site=${s}`; });
    heatDiv.dataset.bound = "1";
  }

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
    { title: "temperature source", field: "column", formatter: (c) => columnLabel(c.getValue(), false) },
    { title: "sites", field: "n", hozAlign: "right", headerFilter: false },
    { title: "r with the reference", field: "r", formatter: f3, hozAlign: "right", headerFilter: false },
    { title: "mean TAS (reference)", field: "mean_ref", formatter: f4, hozAlign: "right", headerFilter: false },
    { title: "mean TAS (source)", field: "mean_col", formatter: f4, hozAlign: "right", headerFilter: false },
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
      { type: "scatter", mode: "markers", x: pts.map(([a]) => a.TAS), y: pts.map(([, b]) => b.TAS), text: pts.map(([a, b, s]) => `${s} ${name(s)}<br>${shortLabel(ref)}: ${fmt(a.TAS)} (${cls(a)})<br>${shortLabel(c)}: ${fmt(b.TAS)} (${cls(b)})`),
        hoverinfo: "text", customdata: pts.map(([, , s]) => s),
        marker: { size: pts.map((x) => (isFlip(x) ? 11 : 7)), color: pts.map((x) => (isFlip(x) ? "#d1242f" : "#0969da")),
          line: { width: 0.5, color: "#333" } }, showlegend: false },
    ], baseLayout({ title: { text: columnLabel(c, false), font: { size: 12 } }, xaxis: { title: `TAS (°C⁻¹), ${shortLabel(ref)}`, range: [-lim, lim] }, yaxis: { title: `TAS (°C⁻¹), ${shortLabel(c)}`, range: [-lim, lim] }, margin: { l: 55, r: 10, t: 30, b: 40 } }), PLOT_CONFIG);
    document.getElementById(`pair-${c}`).on("plotly_click", (e) => { location.href = `site.html?site=${e.points[0].customdata}`; });
  }

  const f = numberFormatter(4);
  const cols = [
    { title: "Site", field: "site_ID", formatter: siteCell },
    { title: "Name", field: "name" },
    { title: "soil sensor", field: "sensor", formatter: (c) => `<span style="color:${VERDICT_COLORS[c.getValue()] || "#000"};font-weight:600">${VERDICTS[c.getValue()] ?? c.getValue()}</span>` },
    { title: "TAS (reference)", field: "ref_TAS", formatter: f, hozAlign: "right", headerFilter: false },
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
tempSourcePicker("controls", draw, { label: "Reference temperature source" });
for (const id of ["model", "sensor"]) document.getElementById(id).addEventListener("change", draw);
draw();
