import { header, loadJSON, table, fmt, params, setParams, baseLayout, PLOT_CONFIG, numberFormatter } from "./common.js";

header("flux-products.html");
const data = await loadJSON("data/flux_products.json");
if (!data.summary) {
  document.querySelector("main").insertAdjacentHTML("beforeend", `<p class="note">No comparison saved yet
    (<code>scripts/compare-flux-products.R</code>).</p>`);
  throw new Error("no flux-products data");
}
const NETWORK_COLORS = { "FLUXNET-format": "#0969da", AmeriFlux: "#bf3989" };
const STREAMS = {
  PUB_VUT: ["published, VUT", "#0969da", "solid"], PUB_CUT: ["published, CUT", "#54aeff", "dot"],
  MS: ["manuscript (AmeriFlux BASE, its u* filter)", "#8250df", "solid"],
  SHUT_VUT: ["shuttle, VUT", "#bf8700", "solid"], SHUT_CUT: ["shuttle, CUT", "#d4a72c", "dot"],
};
const p = params();
const el = (id) => document.getElementById(id);
el("comparison").value = p.get("comparison") || "VUT";
el("stat").value = p.get("stat") || "diff";
el("network").value = p.get("network") || "all";
let picked = p.get("site") || null;
let tbl;
const cache = {};

const value = (r, stat) => (stat === "rel" ? (r.rel_diff ?? null) !== null ? 100 * r.rel_diff : null : r.diff ?? null);
const statLabel = (stat) => (stat === "rel" ? "relative difference, shuttle − reference (%)" : "mean difference, shuttle − reference (µmol m⁻² s⁻¹)");
const quantile = (v, q) => { const s = [...v].sort((a, b) => a - b); const i = (s.length - 1) * q; const lo = Math.floor(i); return s.length ? s[lo] + (s[Math.ceil(i)] - s[lo]) * (i - lo) : null; };

function rows() {
  const cmp = el("comparison").value, net = el("network").value;
  return data.summary.filter((r) => r.comparison === cmp && (net === "all" || r.network === net));
}

function drawSummary() {
  const cmp = el("comparison").value, stat = el("stat").value;
  const rs = rows().filter((r) => value(r, stat) !== null && r.n_both > 0).sort((a, b) => value(a, stat) - value(b, stat));
  const v = rs.map((r) => value(r, stat));
  const stat_ = (x, k) => `<div class="stat"><div class="v">${x}</div><div class="k">${k}</div></div>`;
  const unit = stat === "rel" ? "%" : "";
  el("stats").innerHTML = [
    stat_(rs.length, "sites compared"), stat_(`${fmt(quantile(v, 0.5), 3)}${unit}`, "median site difference"),
    stat_(`${fmt(quantile(v, 0.25), 3)} – ${fmt(quantile(v, 0.75), 3)}${unit}`, "middle 50% of sites"),
    stat_(`${v.filter((x) => x < 0).length} / ${v.filter((x) => x > 0).length}`, "sites shuttle lower / higher"),
    stat_(rows().filter((r) => r.status && r.status !== "ok").length, "sites not compared or partly"),
  ].join("");
  const yearly = new Map();
  for (const y of data.siteyear.filter((y) => y.comparison === cmp)) {
    const val = stat === "rel" ? (y.mean_ref ? 100 * y.diff / y.mean_ref : null) : y.diff;
    if (val === null || val === undefined) continue;
    if (!yearly.has(y.site_ID)) yearly.set(y.site_ID, []);
    yearly.get(y.site_ID).push([y.year, val, y.n_both]);
  }
  const q = (s, k) => quantile((yearly.get(s) || []).filter((x) => x[2] > 0).map((x) => x[1]), k);
  const trace = (net) => {
    const g = rs.filter((r) => r.network === net);
    return { type: "scatter", mode: "markers", name: net, x: g.map((r) => r.site_ID), y: g.map((r) => value(r, stat)),
      error_y: { type: "data", symmetric: false, array: g.map((r) => Math.max(0, (q(r.site_ID, 0.75) ?? value(r, stat)) - value(r, stat))),
        arrayminus: g.map((r) => Math.max(0, value(r, stat) - (q(r.site_ID, 0.25) ?? value(r, stat)))), thickness: 1, width: 0, color: "#999" },
      text: g.map((r) => `${r.site_ID} (${r.years})<br>${r.reference} vs ${r.shuttle}<br>n = ${r.n_both}<br>${fmt(value(r, stat), 3)}`),
      hoverinfo: "text", customdata: g.map((r) => r.site_ID), marker: { color: NETWORK_COLORS[net], size: 8 } };
  };
  Plotly.newPlot("sites-plot", ["FLUXNET-format", "AmeriFlux"].map(trace), baseLayout({
    xaxis: { title: "site", categoryorder: "array", categoryarray: rs.map((r) => r.site_ID), tickangle: -90, tickfont: { size: 9 } },
    yaxis: { title: statLabel(stat), zeroline: true }, legend: { orientation: "h", y: 1.08 }, margin: { l: 70, r: 20, t: 30, b: 90 } }), PLOT_CONFIG);
  el("sites-plot").on("plotly_click", (e) => pick(e.points[0].customdata));

  const years = [...new Set(data.siteyear.map((y) => y.year))].sort((a, b) => a - b);
  const z = rs.map((r) => years.map((yr) => { const x = (yearly.get(r.site_ID) || []).find((y) => y[0] === yr && y[2] > 0); return x ? x[1] : null; }));
  const lim = quantile(z.flat().filter((x) => x !== null).map(Math.abs), 0.95) || 1;
  Plotly.newPlot("heatmap", [{ type: "heatmap", x: years, y: rs.map((r) => r.site_ID), z, zmin: -lim, zmid: 0, zmax: lim,
    colorscale: "RdBu", reversescale: false, hoverongaps: false, colorbar: { title: { text: stat === "rel" ? "%" : "µmol m⁻² s⁻¹" }, thickness: 12 },
    hovertemplate: "%{y} %{x}: %{z:.2f}<extra></extra>" }],
  baseLayout({ height: Math.max(420, 14 * rs.length + 120), xaxis: { title: "year", dtick: 2 },
    yaxis: { title: "site", autorange: "reversed", tickfont: { size: 9 } }, margin: { l: 70, r: 20, t: 20, b: 50 } }), PLOT_CONFIG);
  el("heatmap").on("plotly_click", (e) => pick(e.points[0].y));
}

function drawTable() {
  const f2 = numberFormatter(2), f3 = numberFormatter(3);
  const cols = [
    { title: "show", field: "site_ID", headerSort: false, headerFilter: false, hozAlign: "center", width: 60,
      formatter: (c) => `<input type="radio" name="flux-site" ${c.getValue() === picked ? "checked" : ""}>`,
      cellClick: (e, c) => pick(c.getValue()) },
    { title: "Site", field: "site_ID", formatter: (c) => `<a href="site.html?site=${c.getValue()}">${c.getValue()}</a>` },
    { title: "network", field: "network" },
    { title: "reference", field: "reference_product" },
    { title: "shuttle product", field: "shuttle_file" },
    { title: "years", field: "years" },
    { title: "matched n", field: "n_both", hozAlign: "right", headerFilter: false },
    { title: "ref mean", field: "mean_ref", formatter: f2, hozAlign: "right", headerFilter: false },
    { title: "Δ matched", field: "diff", formatter: f3, hozAlign: "right", headerFilter: false },
    { title: "Δ matched (%)", field: "rel_pct", formatter: numberFormatter(1), hozAlign: "right", headerFilter: false },
    { title: "r", field: "r", formatter: f3, hozAlign: "right", headerFilter: false },
    { title: "slope", field: "slope", formatter: f2, hozAlign: "right", headerFilter: false },
    { title: "day slope", field: "day_slope", formatter: f2, hozAlign: "right", headerFilter: false },
    { title: "Δ own", field: "diff_own", formatter: f3, hozAlign: "right", headerFilter: false },
    { title: "n own: ref / shuttle", field: "n_own", headerFilter: false },
    { title: "status", field: "status" },
  ];
  const rs = rows().map((r) => ({ ...r, rel_pct: r.rel_diff !== null && r.rel_diff !== undefined ? 100 * r.rel_diff : null,
    n_own: r.n_ref_own !== undefined && r.n_ref_own !== null ? `${r.n_ref_own} / ${r.n_shuttle_own}` : "" }));
  if (tbl) tbl.replaceData(rs); else tbl = table("table", rs, cols, { height: "480px", initialSort: [{ column: "diff", dir: "asc" }] });
}

async function drawSeries() {
  if (!picked) {
    el("series-title").innerHTML = "Choose a site in the table.";
    return;
  }
  const s = data.summary.filter((r) => r.site_ID === picked);
  const m = (cache[picked] ??= await loadJSON(`data/flux_products/${picked}.json`).catch(() => ({})));
  el("series-title").innerHTML = `<b>${picked}</b> (${s[0]?.network ?? ""}): reference ${s[0]?.reference_product ?? "–"}; shuttle
    ${s[0]?.shuttle_file ?? "none"}. ${s.map((r) => `${r.comparison}: Δ ${fmt(r.diff, 3)} µmol m⁻² s⁻¹ (${r.status})`).join("; ")}`;
  const traces = Object.keys(STREAMS).filter((k) => m[k]).map((k) => ({ type: "scatter", mode: "lines", name: STREAMS[k][0],
    x: m[k].month.map((x) => `${x}-15`), y: m[k].nee.map((v, i) => (m[k].n[i] >= 10 ? v : null)), connectgaps: false,
    line: { color: STREAMS[k][1], dash: STREAMS[k][2], width: 1.5 } }));
  Plotly.newPlot("series", traces, baseLayout({ xaxis: { title: "date" }, yaxis: { title: "monthly mean night NEE, selected records (µmol m⁻² s⁻¹)" },
    legend: { orientation: "h", y: 1.12 } }), PLOT_CONFIG);
  const ys = data.siteyear.filter((y) => y.site_ID === picked);
  Plotly.newPlot("yearly", ["VUT", "CUT"].map((c) => {
    const g = ys.filter((y) => y.comparison === c);
    return { type: "bar", name: `${c}: matched`, x: g.map((y) => y.year), y: g.map((y) => y.diff), marker: { color: c === "VUT" ? "#bf8700" : "#d4a72c" },
      text: g.map((y) => `n=${y.n_both}`), hovertemplate: "%{x}: %{y:.2f} (%{text})<extra></extra>" };
  }), baseLayout({ barmode: "group", xaxis: { title: "year", dtick: 2 }, yaxis: { title: "mean difference, shuttle − reference (µmol m⁻² s⁻¹)", zeroline: true },
    legend: { orientation: "h", y: 1.12 } }), PLOT_CONFIG);
}

function pick(site) {
  picked = site;
  setParams({ site });
  if (tbl) tbl.getRows().forEach((r) => r.reformat());
  drawSeries();
  el("sec-series").scrollIntoView({ behavior: "smooth" });
}

function draw() {
  setParams({ comparison: el("comparison").value, stat: el("stat").value, network: el("network").value });
  drawSummary();
  drawTable();
}
for (const id of ["comparison", "stat", "network"]) el(id).addEventListener("change", draw);
draw();
drawSeries();
