// Shared helpers for the analysis website: data loading, navigation, TAS
// classes and comparisons, site maps, tables.

export const PAGES = [
  ["index.html", "Overview"],
  ["compare.html", "Compare runs"],
  ["stages.html", "Method stages"],
  ["drivers.html", "Drivers & scenario"],
  ["ts.html", "Soil temperature"],
  ["matrix.html", "TAS by column"],
  ["site.html", "Site explorer"],
  ["update.html", "Data update"],
  ["flux-products.html", "Records vs shuttle"],
  ["au-tum.html", "AU-Tum sources"],
  ["docs-ts-estimators.html", "Docs: TS estimators"],
  ["docs-sensor-screen.html", "Docs: sensor screen"],
];

// Threshold for "TAS changed" (refactor-v2-log.md, S9): 4 x the robust sd of
// the full reproduction run vs the published TAS (total 1.2e-3, direct 1.7e-3).
export const DEFAULT_THRESHOLD = 1.7e-3;

export const CLASS_COLORS = { "sig-": "#2166ac", "n.s.": "#8c959f", "sig+": "#b2182b", "NA": "#d0d7de" };

// Soil-temperature sensor screen verdicts (R/ts-screen.R).
export const VERDICT_COLORS = { GOOD: "#1a7f37", SUSPECT: "#bf8700", BAD: "#cf222e", NA: "#d0d7de" };

// The latest run's sensor screen: { run, data, sensor: Map(site -> the
// sensor's validation row) }, or null if no run has one.
export async function loadScreen(run) {
  const runs = await loadJSON("data/ts_screen_runs.json").catch(() => []);
  if (!runs.length) return null;
  const id = run && runs.includes(run) ? run : runs[runs.length - 1];
  const data = await loadJSON(`data/ts_screen/${id}.json`);
  return { run: id, runs, data, sensor: new Map((data.validation || []).map((r) => [r.site_ID, r])) };
}

export async function loadJSON(path) {
  const r = await fetch(path, { cache: "no-cache" });
  if (!r.ok) throw new Error(`${path}: ${r.status}`);
  return r.json();
}

export function header(active) {
  const links = PAGES.map(([href, label]) =>
    `<a href="${href}"${href === active ? ' class="active"' : ""}>${label}</a>`).join("");
  document.body.insertAdjacentHTML("afterbegin",
    `<header><span class="title">Thermal acclimation · refactor-v2</span><nav>${links}</nav></header>`);
}

export function params() {
  return new URLSearchParams(window.location.search);
}

export function setParams(obj) {
  const p = params();
  for (const [k, v] of Object.entries(obj)) p.set(k, v);
  history.replaceState(null, "", `${location.pathname}?${p.toString()}`);
}

export function fmt(x, digits = 4) {
  if (x === null || x === undefined || Number.isNaN(x)) return "–";
  if (Math.abs(x) > 0 && Math.abs(x) < 1e-3) return x.toExponential(2);
  return Number(x).toFixed(digits);
}

export function tasClass(tas, p, alpha = 0.05) {
  if (tas === null || p === null || tas === undefined || p === undefined) return "NA";
  if (p >= alpha) return "n.s.";
  return tas < 0 ? "sig-" : "sig+";
}

// Compare result set `b` with reference `a` for one model: one row per site.
export function compare(a, b, model, threshold = DEFAULT_THRESHOLD) {
  const ra = new Map((a[model] || []).map((r) => [r.site_ID, r]));
  const rb = new Map((b[model] || []).map((r) => [r.site_ID, r]));
  const sites = [...new Set([...ra.keys(), ...rb.keys()])];
  return sites.map((site) => {
    const x = ra.get(site) || {};
    const y = rb.get(site) || {};
    const d = (y.TAS ?? null) !== null && (x.TAS ?? null) !== null ? y.TAS - x.TAS : null;
    const cx = tasClass(x.TAS ?? null, x.TASp ?? null);
    const cy = tasClass(y.TAS ?? null, y.TASp ?? null);
    const flip = (cx === "sig-" && cy === "sig+") || (cx === "sig+" && cy === "sig-");
    const se = y.TAS_se ?? x.TAS_se ?? null;
    return {
      site_ID: site,
      TAS_ref: x.TAS ?? null, TAS_new: y.TAS ?? null, dTAS: d,
      se, d_over_se: d !== null && se ? d / se : null,
      p_ref: x.TASp ?? null, p_new: y.TASp ?? null,
      class_ref: cx, class_new: cy,
      transition: cx === cy ? "" : `${cx} → ${cy}`,
      flip,
      changed: d === null ? (x.TAS ?? null) !== (y.TAS ?? null) : Math.abs(d) >= threshold,
      nwindow_ref: x.nwindow ?? null, nwindow_new: y.nwindow ?? null,
    };
  }).sort((p, q) => (q.flip - p.flip) || ((q.transition !== "") - (p.transition !== "")) ||
    (Math.abs(q.dTAS ?? 0) - Math.abs(p.dTAS ?? 0)));
}

export function summarise(rows) {
  const d = rows.map((r) => r.dTAS).filter((v) => v !== null).map(Math.abs).sort((a, b) => a - b);
  const median = d.length ? d[Math.floor(d.length / 2)] : null;
  return {
    n: rows.length,
    median_abs: median,
    max_abs: d.length ? d[d.length - 1] : null,
    changed: rows.filter((r) => r.changed).length,
    transitions: rows.filter((r) => r.transition !== "").length,
    flips: rows.filter((r) => r.flip).map((r) => r.site_ID),
  };
}

export const PLOT_CONFIG = { responsive: true, displaylogo: false, topojsonURL: "vendor/topojson/" };

export function baseLayout(extra = {}) {
  const layout = Object.assign({
    margin: { l: 60, r: 20, t: 30, b: 50 },
    font: { family: "-apple-system, Helvetica, Arial, sans-serif", size: 12 },
    hovermode: "closest",
    plot_bgcolor: "#fff",
  }, extra);
  // Plotly.js 3 shows axis titles only as { text }, not as plain strings; and
  // long titles need room
  for (const [key, axis] of Object.entries(layout)) {
    if (/^[xy]axis\d*$/.test(key) && axis) {
      if (typeof axis.title === "string") axis.title = { text: axis.title };
      if (axis.title && axis.automargin === undefined) axis.automargin = true;
    }
  }
  return layout;
}

// Two site maps, geographic and climate space (mean annual temperature from
// the flux record vs annual precipitation, over Whittaker biomes), with
// points coloured by `value(site)` (numeric) or `category(site)`. With
// `focus`, for one site: the map zoomed in and centred on it, and the site
// labelled in both.
export async function siteMaps(geoDiv, climDiv, sites, { value, category, label = "", colorscale = "RdBu", symmetric = true, onClick, focus = false, colors = CLASS_COLORS } = {}) {
  const biomes = await loadJSON("static/whittaker_biomes.json");
  const text = sites.map((s) => `${s.site_ID}<br>${s.IGBP}, ${s.Climate_class}<br>${label}: ${value ? fmt(value(s)) : category(s)}`);
  let marker;
  if (value) {
    const v = sites.map(value);
    const vmax = Math.max(...v.filter((x) => x !== null && !Number.isNaN(x)).map(Math.abs), 1e-9);
    marker = { color: v, colorscale, cmin: symmetric ? -vmax : undefined, cmax: symmetric ? vmax : undefined,
      size: 9, line: { width: 0.5, color: "#333" }, colorbar: { title: { text: label }, thickness: 12 } };
  } else {
    const cats = sites.map(category);
    marker = { color: cats.map((c) => colors[c] || "#8c959f"), size: 9, line: { width: 0.5, color: "#333" } };
  }
  if (focus) Object.assign(marker, { size: 14, line: { width: 1.5, color: "#000" } });
  const mode = focus ? "markers+text" : "markers";
  const labels = { text: sites.map((s) => s.site_ID), textposition: "top center", textfont: { size: 12 } };
  const geo = [{ type: "scattergeo", lon: sites.map((s) => s.LONG), lat: sites.map((s) => s.LAT), hovertext: text, hoverinfo: "text",
    customdata: sites.map((s) => s.site_ID), mode, marker, ...(focus ? labels : {}) }];
  const geoLayout = { projection: { type: "natural earth" }, showland: true, landcolor: "#f0f0f0", showcountries: true, countrycolor: "#ccc" };
  if (focus && sites.length) {
    Object.assign(geoLayout, { center: { lat: sites[0].LAT, lon: sites[0].LONG }, projection: { type: "natural earth", scale: 8,
      rotation: { lon: sites[0].LONG } }, showcoastlines: true, coastlinecolor: "#999", showlakes: true, lakecolor: "#dbe9f6",
      showrivers: true, rivercolor: "#dbe9f6", showsubunits: true, subunitcolor: "#ddd" });
  }
  Plotly.newPlot(geoDiv, geo, baseLayout({ geo: geoLayout, margin: { l: 0, r: 0, t: 10, b: 0 } }), PLOT_CONFIG);
  const shapes = biomes.map((b, i) => ({ type: "scatter", x: b.precp_cm.map((p) => p * 10), y: b.temp_c, fill: "toself",
    mode: "lines", line: { width: 0.5, color: "#999" }, fillcolor: `hsla(${(i * 47) % 360}, 35%, 85%, 0.45)`,
    name: b.biome, hoverinfo: "name", showlegend: false }));
  const clim = shapes.concat([{ type: "scatter", mode, x: sites.map((s) => s.MAP), y: sites.map((s) => s.MAT), hovertext: text,
    hoverinfo: "text", customdata: sites.map((s) => s.site_ID), marker, showlegend: false, ...(focus ? labels : {}) }]);
  Plotly.newPlot(climDiv, clim, baseLayout({
    xaxis: { title: "Mean annual precipitation (mm)" }, yaxis: { title: "Mean annual air temperature (°C)" },
  }), PLOT_CONFIG);
  if (onClick) {
    for (const div of [geoDiv, climDiv]) {
      document.getElementById(div).on("plotly_click", (e) => { const id = e.points[0].customdata; if (id) onClick(id); });
    }
  }
}

export function table(div, rows, columns, { onRowClick, height = "420px", initialSort } = {}) {
  const t = new Tabulator(`#${div}`, {
    data: rows, columns, layout: "fitDataStretch", height, initialSort,
    columnDefaults: { headerFilter: "input", headerFilterPlaceholder: "filter" },
  });
  if (onRowClick) t.on("rowClick", (e, row) => onRowClick(row.getData()));
  return t;
}

export const numberFormatter = (digits = 4) => (cell) => fmt(cell.getValue(), digits);
