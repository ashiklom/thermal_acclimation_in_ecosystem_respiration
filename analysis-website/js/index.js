import { header, SITES, loadMain, siteMaps, table, fmt, tasClass, tempSourcePicker, tempSource, columnLabel, CLASS_COLORS,
  PREDICTORS, MODEL_LABELS, DIVERGING, baseLayout, PLOT_CONFIG, params, setParams, siteCell, numberFormatter } from "./common.js";

header("index.html");
const main = await loadMain();
const sites = [...SITES.values()];
const open = (id) => { location.href = `site.html?site=${id}`; };
const p = params();
for (const [k, v] of [["colour", "direct"], ["model", "direct"], ["predictor", "LAI"]]) {
  if (k === "predictor") {
    document.getElementById(k).innerHTML = Object.entries(PREDICTORS).map(([c, l]) => `<option value="${c}">${l}</option>`).join("");
  }
  document.getElementById(k).value = p.get(k) || v;
}
tempSourcePicker("controls", draw);
for (const id of ["colour", "model", "predictor"]) document.getElementById(id).addEventListener("change", draw);
let tbl;

function draw() {
  const column = tempSource();
  const colour = document.getElementById("colour").value;
  const model = document.getElementById("model").value;
  const predictor = document.getElementById("predictor").value;
  setParams({ colour, model, predictor });
  const total = main.get("total", column), direct = main.get("direct", column);
  const fitted = sites.filter((s) => total.has(s.site_ID) || direct.has(s.site_ID));
  document.getElementById("nsites").textContent = new Set(main.tas.total.map((r) => r.site_ID)).size;
  const source = columnLabel(column);

  // maps
  const byModel = { total, direct };
  if (colour === "total" || colour === "direct") {
    siteMaps("geo", "clim", sites, { value: (s) => byModel[colour].get(s.site_ID)?.TAS ?? null, colorscale: DIVERGING, legendDiv: "maplegend", label: `${MODEL_LABELS[colour]} (${source})`, onClick: open,
      missing: column.startsWith("TS_") ? "no estimate (e.g. no soil temperature)" : "no estimate" });
  } else {
    const category = { IGBP: (s) => s.IGBP, climate: (s) => s.Climate_class, network: (s) => s.network }[colour];
    const label = { IGBP: "Vegetation type", climate: "Climate class", network: "Data source" }[colour];
    siteMaps("geo", "clim", sites, { category, label, colors: null, onClick: open, legendDiv: "maplegend" });
  }

  // table
  const rows = sites.map((s) => {
    const t = total.get(s.site_ID) || {}, d = direct.get(s.site_ID) || {};
    return { site_ID: s.site_ID, name: s.site_name || "", IGBP: s.IGBP, climate: s.Climate_class, source: s.source_label,
      total: t.TAS ?? null, total_se: t.TAS_se ?? null, total_p: t.TASp ?? null,
      direct: d.TAS ?? null, direct_se: d.TAS_se ?? null, direct_p: d.TASp ?? null,
      years: t.n_years ?? d.n_years ?? null };
  });
  const sig = (field) => (cell) => {
    const r = cell.getRow().getData();
    const c = tasClass(r[field], r[`${field}_p`]);
    return `<span style="color:${c === "n.s." ? "inherit" : CLASS_COLORS[c]}">${fmt(cell.getValue(), 4)}</span>`;
  };
  const columns = [
    { title: "Site", field: "site_ID", formatter: siteCell, frozen: true },
    { title: "Name", field: "name" },
    { title: "IGBP", field: "IGBP" },
    { title: "Climate", field: "climate" },
    { title: "Data source", field: "source" },
    { title: "Total TAS", field: "total", formatter: sig("total"), hozAlign: "right", sorter: "number" },
    { title: "SE", field: "total_se", formatter: numberFormatter(4), hozAlign: "right" },
    { title: "p", field: "total_p", formatter: numberFormatter(3), hozAlign: "right" },
    { title: "Direct TAS", field: "direct", formatter: sig("direct"), hozAlign: "right", sorter: "number" },
    { title: "SE", field: "direct_se", formatter: numberFormatter(4), hozAlign: "right" },
    { title: "p", field: "direct_p", formatter: numberFormatter(3), hozAlign: "right" },
    { title: "Years", field: "years", hozAlign: "right" },
  ];
  if (tbl) tbl.setData(rows);
  else tbl = table("table", rows, columns, { onRowClick: (r) => open(r.site_ID), height: "480px",
    initialSort: [{ column: "site_ID", dir: "asc" }] });

  // TAS against a predictor
  const est = byModel[model];
  const pts = fitted.map((s) => ({ s, r: est.get(s.site_ID), x: s[predictor] }))
    .filter((q) => q.r && q.r.TAS !== null && q.x !== null && q.x !== undefined);
  const traces = ["sig-", "n.s.", "sig+"].map((c) => {
    const g = pts.filter((q) => tasClass(q.r.TAS, q.r.TASp) === c);
    return { type: "scatter", mode: "markers", name: { "sig-": "significantly negative", "n.s.": "not significant",
      "sig+": "significantly positive" }[c], x: g.map((q) => q.x), y: g.map((q) => q.r.TAS),
    error_y: { type: "data", array: g.map((q) => q.r.TAS_se), thickness: 0.8, width: 0, color: `${CLASS_COLORS[c]}66` },
    marker: { color: CLASS_COLORS[c], size: 7 }, customdata: g.map((q) => q.s.site_ID),
    hovertemplate: g.map((q) => `<b>${q.s.site_ID}</b> ${q.s.site_name || ""}<br>${q.s.IGBP}, ${q.s.Climate_class}` +
      `<br>%{x:.3g}<br>TAS %{y:.4f}<extra></extra>`) };
  });
  if (pts.length > 2) {
    const n = pts.length, mx = pts.reduce((a, q) => a + q.x, 0) / n, my = pts.reduce((a, q) => a + q.r.TAS, 0) / n;
    const sxy = pts.reduce((a, q) => a + (q.x - mx) * (q.r.TAS - my), 0), sxx = pts.reduce((a, q) => a + (q.x - mx) ** 2, 0);
    const syy = pts.reduce((a, q) => a + (q.r.TAS - my) ** 2, 0);
    const b = sxy / sxx, xs = [Math.min(...pts.map((q) => q.x)), Math.max(...pts.map((q) => q.x))];
    traces.push({ type: "scatter", mode: "lines", x: xs, y: xs.map((x) => my + b * (x - mx)), hoverinfo: "skip",
      line: { color: "#333", dash: "dash", width: 1.5 }, name: `linear fit (r = ${(sxy / Math.sqrt(sxx * syy)).toFixed(2)}, n = ${n})` });
  }
  Plotly.react("scatter", traces, baseLayout({ xaxis: { title: PREDICTORS[predictor], zeroline: false },
    yaxis: { title: `${MODEL_LABELS[model]} (${source}, per °C)`, zeroline: true, zerolinecolor: "#999" },
    legend: { orientation: "h", y: -0.2 } }), PLOT_CONFIG);
  const div = document.getElementById("scatter");
  if (!div.dataset.bound) {
    div.on("plotly_click", (e) => { const id = e.points[0].customdata; if (id) open(id); });
    div.dataset.bound = "1";
  }
}
draw();
