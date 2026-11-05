// Shared helpers for the analysis website: data loading, navigation, TAS
// classes and comparisons, site maps, tables.

// The main pages (results of the latest run) and, in the header's Archive menu,
// the pages that documented the refactor run by run.
export const PAGES = [
  ["index.html", "Overview"],
  ["method.html", "How TAS is estimated"],
  ["site.html", "Site explorer"],
  ["matrix.html", "TAS by temperature source"],
  ["soil-temperature.html", "Soil temperature in detail"],
  ["drivers.html", "Drivers of TAS"],
];
export const ARCHIVE_PAGES = [
  ["archive-overview.html", "Overview (result sets)"],
  ["compare.html", "Compare runs"],
  ["stages.html", "Method stages"],
  ["archive-drivers.html", "Drivers & scenario"],
  ["ts.html", "Soil temperature columns"],
  ["archive-matrix.html", "TAS by column"],
  ["archive-site.html", "Site explorer"],
  ["update.html", "Data update"],
  ["flux-products.html", "Records vs shuttle"],
  ["au-tum.html", "AU-Tum sources"],
  ["docs-ts-estimators.html", "Docs: TS estimators"],
  ["docs-sensor-screen.html", "Docs: sensor screen"],
];
// On an archive page (set by header()): the added-sites toggle, marks and
// triangles; on the main pages every site is shown alike.
const archiveMode = () => document.body?.dataset.archive === "1";

// The temperature columns TAS is estimated with (R/ts-columns.R, R/ta-columns.R),
// as the main pages name them. SIMPLE_SOURCES are offered without "Advanced".
export const COLUMN_LABELS = {
  TS_original: "Soil temperature (sensor)",
  TS_reconstructed: "Soil temperature (repaired sensor)",
  TS_manuscript: "Soil temperature (reconstructed or regression)",
  TS_linear: "Soil temperature (linear on air temperature, TA > 0)",
  TS_lm_ta: "Soil temperature (linear on air temperature)",
  TS_lm_ta_recent: "Soil temperature (linear on air temperature, 2022 onward)",
  TS_lm_ta_netrad: "Soil temperature (linear on air temperature + net radiation)",
  TS_rf_ta: "Soil temperature (random forest on air temperature)",
  TS_rf_ta_netrad: "Soil temperature (random forest on air temperature + net radiation)",
  TS_lm_memory: "Soil temperature (linear on lagged air temperature)",
  TS_rf_memory: "Soil temperature (random forest on lagged air temperature)",
  TS_rf_memory_netrad: "Soil temperature (random forest on lagged air temperature + net radiation)",
  TA_measured: "Air temperature (measured)",
  TA_mem3d: "Air temperature (3-day running mean)",
  TA_mem7d: "Air temperature (7-day running mean)",
  TA_mem30d: "Air temperature (30-day running mean)",
};
export const SIMPLE_SOURCES = { TS_manuscript: "Soil temperature", TA_measured: "Air temperature" };
export const DEFAULT_SOURCE = "TS_manuscript";
export const SOIL_COLUMNS = Object.keys(COLUMN_LABELS).filter((c) => c.startsWith("TS_"));
export const AIR_COLUMNS = Object.keys(COLUMN_LABELS).filter((c) => c.startsWith("TA_"));
export const isAdvanced = () => localStorage.getItem("thermal.tempAdvanced") === "1";
// The label of a column: the simple name where "Advanced" is off and it has one.
export const columnLabel = (c, simple = !isAdvanced()) => (simple && SIMPLE_SOURCES[c]) || COLUMN_LABELS[c] || c;
// A colour per column (soil: warm/brown hues, air: blues), for line plots.
export const COLUMN_COLORS = Object.fromEntries([
  ...SOIL_COLUMNS.map((c, i) => [c, ["#8c510a", "#bf812d", "#b2182b", "#e08214", "#7f3b08", "#d6604d", "#f4a582",
    "#9e0142", "#d53e4f", "#a6761d", "#e6ab02", "#666666"][i]]),
  ...AIR_COLUMNS.map((c, i) => [c, ["#2166ac", "#4393c3", "#92c5de", "#053061"][i]]),
]);

// The temperature source of the TAS the main pages show: URL `temp=`, else the
// last choice (localStorage), else soil temperature.
export function tempSource() {
  const v = params().get("temp");
  if (v && COLUMN_LABELS[v]) return v;
  const s = localStorage.getItem("thermal.tempSource");
  return s && COLUMN_LABELS[s] ? s : DEFAULT_SOURCE;
}
function setTempSource(c) {
  localStorage.setItem("thermal.tempSource", c);
  setParams({ temp: c });
}

// The shared temperature-source control: a select (soil or air temperature;
// with "Advanced", every column) inside `container` (an element or id), calling
// onChange(column) on every change, labelled `label`. Returns { get, set }.
export function tempSourcePicker(container, onChange, { label = "Temperature source" } = {}) {
  const el = typeof container === "string" ? document.getElementById(container) : container;
  el.insertAdjacentHTML("beforeend", `<span class="temppicker"><label>${label} <select></select></label>
    <label class="adv" title="Every soil- and air-temperature column TAS was estimated with">
    <input type="checkbox"> Advanced</label></span>`);
  const span = el.lastElementChild;
  const select = span.querySelector("select");
  const box = span.querySelector("input");
  const fill = () => {
    const adv = box.checked;
    const opt = (c) => `<option value="${c}">${columnLabel(c, !adv)}</option>`;
    select.innerHTML = adv
      ? `<optgroup label="Soil temperature">${SOIL_COLUMNS.map(opt).join("")}</optgroup>` +
        `<optgroup label="Air temperature">${AIR_COLUMNS.map(opt).join("")}</optgroup>`
      : Object.keys(SIMPLE_SOURCES).map(opt).join("");
    select.value = tempSource();
  };
  box.checked = isAdvanced() || !SIMPLE_SOURCES[tempSource()];
  localStorage.setItem("thermal.tempAdvanced", box.checked ? "1" : "0");
  fill();
  setTempSource(select.value);
  select.addEventListener("change", () => { setTempSource(select.value); onChange(select.value); });
  box.addEventListener("change", () => {
    localStorage.setItem("thermal.tempAdvanced", box.checked ? "1" : "0");
    const before = tempSource();
    if (!box.checked && !SIMPLE_SOURCES[before]) setTempSource(DEFAULT_SOURCE);
    fill();
    // the labels change with the mode even where the column does not
    onChange(tempSource());
  });
  return { get: () => select.value, set: (c) => { setTempSource(c); fill(); } };
}

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

// Sites added after the manuscript's 117 (the site table's `added`, e.g. the
// FLUXNET-shuttle sites): marked NEW_MARK in tables and drawn as NEW_SYMBOL in
// figures (same colours), and left out of figures, tables and statistics when
// the header's toggle excludes them (URL parameter `new=0`, remembered across
// pages). Steps 03-04 are refitted without them, see drivers.js.
export const NEW_SYMBOL = "triangle-up";
export const NEW_MARK = "*";
export const SITES = new Map((await loadJSON("data/sites.json").catch(() => [])).map((s) => [s.site_ID, s]));
export const isNew = (id) => Boolean(SITES.get(id)?.added);
export const ADDED_DATES = [...new Set([...SITES.values()].map((s) => s.added).filter(Boolean))].sort();
// Added sites without soil temperature, fitted only with the air-temperature
// columns (R/ta-columns.R): TA_ONLY_MARK after NEW_MARK.
export const TA_ONLY_MARK = "†";
export const isTaOnly = (id) => Boolean(SITES.get(id)?.ta_only);

export function includeNew() {
  if (!archiveMode()) return true;
  const v = new URLSearchParams(window.location.search).get("new");
  if (v !== null) return v !== "0";
  return localStorage.getItem("thermal.includeNew") !== "0";
}

// Rows whose site (field `key`) the toggle keeps.
export function filterSites(rows, key = "site_ID") {
  return includeNew() ? rows : rows.filter((r) => !isNew(typeof r === "string" ? r : r[key]));
}

export const siteSymbol = (id, base = "circle") => (archiveMode() && isNew(id) ? NEW_SYMBOL : base);
const marks = (id) => (archiveMode() ? (isNew(id) ? NEW_MARK : "") + (isTaOnly(id) ? TA_ONLY_MARK : "") : "");
export const siteLabel = (id) => `${id}${marks(id)}`;
const newTitle = (id) => `added ${SITES.get(id)?.added} (FLUXNET shuttle)` +
  (isTaOnly(id) ? "; no soil temperature: air-temperature columns only" : "");
export const siteHTML = (id, href = `${archiveMode() ? "archive-" : ""}site.html?site=${id}`) =>
  `<a href="${href}">${id}</a>${marks(id) ? `<sup class="newsite" title="${newTitle(id)}">${marks(id)}</sup>` : ""}`;
// Tabulator formatter for a site-ID column.
export const siteCell = (cell) => siteHTML(cell.getValue());

export function header(active) {
  const archive = ARCHIVE_PAGES.some(([href]) => href === active);
  if (archive) document.body.dataset.archive = "1";
  const links = PAGES.map(([href, label]) =>
    `<a href="${href}"${href === active ? ' class="active"' : ""}>${label}</a>`).join("");
  const archiveLinks = ARCHIVE_PAGES.map(([href, label]) =>
    `<a href="${href}"${href === active ? ' class="active"' : ""}>${label}</a>`).join("");
  const menu = `<details class="archive"><summary>Archive</summary>
    <div class="menu"><p class="small">How the analysis was refactored and checked, run by run</p>${archiveLinks}</div></details>`;
  const nNew = [...SITES.values()].filter((s) => s.added).length;
  const nTaOnly = [...SITES.values()].filter((s) => s.ta_only).length;
  const toggle = archive && nNew ? `<label class="newtoggle" title="Sites added ${ADDED_DATES.join(", ")} from the FLUXNET shuttle: ` +
    `triangles in figures, ${NEW_MARK} in tables; excluded, they are left out of every figure, table and statistic, ` +
    `and steps 03-04 show the run over the manuscript's sites"><input type="checkbox" id="include-new"${includeNew() ? " checked" : ""}> ` +
    `include the ${nNew} added sites (▲, ${NEW_MARK})</label>` : "";
  const title = archive ? `<a class="title" href="index.html">Thermal acclimation of ecosystem respiration</a>` +
    `<span class="archivetag">Archive: refactor-v2</span>` :
    `<a class="title" href="index.html">Thermal acclimation of ecosystem respiration</a>`;
  document.body.insertAdjacentHTML("afterbegin", `<header>${title}<nav>${links}</nav>${toggle}${menu}</header>`);
  // close the archive menu on a click elsewhere (it opens over the page)
  const details = document.querySelector("header details.archive");
  document.addEventListener("click", (e) => { if (!details.contains(e.target)) details.open = false; });
  document.getElementById("include-new")?.addEventListener("change", (e) => {
    localStorage.setItem("thermal.includeNew", e.target.checked ? "1" : "0");
    setParams({ new: e.target.checked ? "1" : "0" });
    location.reload();
  });
  if (archive && nNew) {
    const note = `<p class="small footnote"><sup class="newsite">${NEW_MARK}</sup> / ▲ site added ${ADDED_DATES.join(", ")} ` +
      `from the FLUXNET shuttle (${nNew} sites; not in the manuscript); ` +
      `${includeNew() ? "included" : "excluded"} here (toggle at the top).` +
      (nTaOnly ? ` <sup class="newsite">${NEW_MARK}${TA_ONLY_MARK}</sup> added site without soil temperature ` +
        `(${nTaOnly} sites), fitted only with the air-temperature columns.` : "") + `</p>`;
    // (module scripts run once the page is parsed: <main> exists)
    document.querySelector("main")?.insertAdjacentHTML("beforeend", note);
  }
  watchPlainTables();
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
      // (a site in only one of the two, e.g. an added site, is not "changed")
      changed: d === null ? (x.TAS ?? null) !== (y.TAS ?? null) && ra.has(site) && rb.has(site) : Math.abs(d) >= threshold,
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

// A categorical palette (colour-blind safe first, then more), for categories
// without colours of their own.
export const PALETTE = ["#0072b2", "#e69f00", "#009e73", "#cc79a7", "#56b4e9", "#d55e00", "#f0e442", "#000000",
  "#8c564b", "#7f7f7f", "#bcbd22", "#17becf", "#9467bd", "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#e377c2"];
export const NO_VALUE_COLOR = "#c8ccd1";
// A diverging scale for TAS: blue (negative) to red (positive), white at zero.
export const DIVERGING = [[0, "#053061"], [0.2, "#2166ac"], [0.4, "#92c5de"], [0.5, "#f7f7f7"], [0.6, "#f4a582"],
  [0.8, "#b2182b"], [1, "#67001f"]];

// A legend shared by the two site maps, centred under both (in `div`): a
// horizontal colour bar for values, or one swatch per category; and the
// sites without a value.
function mapLegend(div, { label, colorscale, vmax, missing, nMissing, categories }) {
  const el = typeof div === "string" ? document.getElementById(div) : div;
  const swatch = (color, text) => `<span class="swatchitem"><span class="dot" style="background:${color}"></span>${text}</span>`;
  let bar = "";
  if (colorscale) {
    const ticks = [-1, -0.5, 0, 0.5, 1].map((f) => `<span style="left:${50 + 50 * f}%">${fmt(f * vmax, 2)}</span>`).join("");
    bar = `<div class="colorbar"><div class="cbtitle">${label}</div>` +
      `<div class="cbbar" style="background:linear-gradient(to right, ${colorscale.map(([q, c]) => `${c} ${100 * q}%`).join(", ")})"></div>` +
      `<div class="cbticks">${ticks}</div></div>`;
  }
  const cats = (categories || []).map(([c, color]) => swatch(color, c)).join("");
  el.innerHTML = `<div class="maplegend${bar ? " stacked" : ""}">${bar}${cats}${nMissing ? swatch(NO_VALUE_COLOR, `${missing} (${nMissing})`) : ""}</div>`;
}

// Two site maps, geographic and climate space (mean annual temperature vs
// annual precipitation, over Whittaker biomes), with points coloured by
// `value(site)` (numeric; sites without a value grey, as `missing`) or
// `category(site)` (colours from `colors`, else PALETTE in order of
// `categories`, with a legend). With `focus`, for one site: the map zoomed in
// and centred on it, and the site labelled in both (and shown whatever the
// added-sites toggle). With `legendDiv`, one legend for both maps goes there
// (mapLegend()) instead of a colour bar and legend in each plot; a numeric
// colorscale must then be an array of [position, colour].
export async function siteMaps(geoDiv, climDiv, sites, { value, category, categories, label = "", colorscale = "RdBu",
  reversescale = false, symmetric = true, onClick, focus = false, colors = CLASS_COLORS, filter = !focus,
  missing = "no estimate", hover = () => "", legendDiv = null } = {}) {
  const biomes = await loadJSON("static/whittaker_biomes.json");
  if (filter) sites = filterSites(sites);
  const name = (s) => s.site_name ? ` ${s.site_name}` : "";
  const text = (s) => `<b>${siteLabel(s.site_ID)}</b>${name(s)}<br>${s.IGBP}, ${s.Climate_class}<br>${label}: ` +
    `${value ? fmt(value(s)) : category(s)}${hover(s)}`;
  const mode = focus ? "markers+text" : "markers";
  const base = { size: 9, line: { width: 0.5, color: "#333" } };
  if (focus) Object.assign(base, { size: 14, line: { width: 1.5, color: "#000" } });
  // groups of sites, each one trace in both plots
  const groups = [];
  if (value) {
    const has = sites.filter((s) => value(s) !== null && value(s) !== undefined && !Number.isNaN(value(s)));
    const none = sites.filter((s) => !has.includes(s));
    const v = has.map(value);
    const vmax = Math.max(...v.map(Math.abs), 1e-9);
    groups.push({ sites: has, name: label, showlegend: false, marker: { ...base, color: v, colorscale, reversescale,
      cmin: symmetric ? -vmax : undefined, cmax: symmetric ? vmax : undefined,
      ...(legendDiv ? { showscale: false } : { colorbar: { title: { text: label }, thickness: 12 } }) } });
    if (none.length) groups.push({ sites: none, name: missing, showlegend: true, marker: { ...base, color: NO_VALUE_COLOR } });
    if (legendDiv) mapLegend(legendDiv, { label, colorscale, vmax, missing, nMissing: none.length });
  } else {
    const cats = categories || [...new Set(sites.map(category))].sort();
    cats.forEach((c, i) => {
      const g = sites.filter((s) => category(s) === c);
      if (g.length) groups.push({ sites: g, name: `${c} (${g.length})`, showlegend: true,
        marker: { ...base, color: colors?.[c] || PALETTE[i % PALETTE.length] } });
    });
    if (legendDiv) mapLegend(legendDiv, { categories: groups.map((g) => [g.name, g.marker.color]) });
  }
  const trace = (g, extra) => ({ hovertext: g.sites.map(text), hoverinfo: "text", customdata: g.sites.map((s) => s.site_ID), mode,
    name: g.name, marker: { ...g.marker, symbol: g.sites.map((s) => siteSymbol(s.site_ID)) },
    ...(focus ? { text: g.sites.map((s) => s.site_ID), textposition: "top center", textfont: { size: 12 } } : {}), ...extra });
  const geo = groups.map((g) => trace(g, { type: "scattergeo", lon: g.sites.map((s) => s.LONG), lat: g.sites.map((s) => s.LAT),
    showlegend: false }));
  const geoLayout = { projection: { type: "natural earth" }, showland: true, landcolor: "#f0f0f0", showcountries: true, countrycolor: "#ccc" };
  if (focus && sites.length) {
    Object.assign(geoLayout, { center: { lat: sites[0].LAT, lon: sites[0].LONG }, projection: { type: "natural earth", scale: 8,
      rotation: { lon: sites[0].LONG } }, showcoastlines: true, coastlinecolor: "#999", showlakes: true, lakecolor: "#dbe9f6",
      showrivers: true, rivercolor: "#dbe9f6", showsubunits: true, subunitcolor: "#ddd" });
  }
  Plotly.newPlot(geoDiv, geo, baseLayout({ geo: geoLayout, margin: { l: 0, r: 0, t: 10, b: 0 }, showlegend: false }), PLOT_CONFIG);
  const shapes = biomes.map((b, i) => ({ type: "scatter", x: b.precp_cm.map((p) => p * 10), y: b.temp_c, fill: "toself",
    mode: "lines", line: { width: 0.5, color: "#999" }, fillcolor: `hsla(${(i * 47) % 360}, 35%, 85%, 0.45)`,
    name: b.biome, hoverinfo: "name", showlegend: false }));
  const clim = shapes.concat(groups.map((g) => trace(g, { type: "scatter", x: g.sites.map((s) => s.MAP), y: g.sites.map((s) => s.MAT),
    showlegend: !focus && !legendDiv && g.showlegend })));
  Plotly.newPlot(climDiv, clim, baseLayout({
    xaxis: { title: "Mean annual precipitation (mm)" }, yaxis: { title: "Mean annual air temperature (°C)" },
    legend: { orientation: "h", y: -0.18, font: { size: 11 } },
  }), PLOT_CONFIG);
  if (onClick) {
    for (const div of [geoDiv, climDiv]) {
      document.getElementById(div).on("plotly_click", (e) => { const id = e.points[0].customdata; if (id) onClick(id); });
    }
  }
}

// csvName: the downloaded file's name without ".csv" (a string, or a function
// called at download time); by default the page's and the table's names.
export function table(div, rows, columns, { onRowClick, height = "420px", initialSort, csvName } = {}) {
  const t = new Tabulator(`#${div}`, {
    data: rows, columns, layout: "fitDataStretch", height, initialSort,
    columnDefaults: { headerFilter: "input", headerFilterPlaceholder: "filter" },
  });
  if (onRowClick) t.on("rowClick", (e, row) => onRowClick(row.getData()));
  const name = () => (typeof csvName === "function" ? csvName() : csvName) || `${pageName()}-${div}`;
  csvButton(document.getElementById(div), `tabulator-${div}`,
    "Download the rows shown (after the filters, in the current order) as CSV, with unrounded values",
    () => t.download("csv", `${name()}.csv`, { bom: true }));
  return t;
}

// ---------------------------------------------------------------- CSV downloads
const pageName = () => (location.pathname.split("/").pop() || "index.html").replace(/\.html$/, "");

// A "Download CSV" button just before `el` (once per key: tables redrawn into
// the same place keep their button, whose click calls the current onClick).
function csvButton(el, key, title, onClick) {
  let bar = el.previousElementSibling;
  if (!bar?.classList.contains("csv-tools") || bar.dataset.key !== key) {
    bar = document.createElement("div");
    bar.className = "csv-tools";
    bar.dataset.key = key;
    bar.innerHTML = `<button type="button" class="csv-download">Download CSV</button>`;
    el.before(bar);
  }
  const btn = bar.querySelector("button");
  btn.title = title;
  btn.onclick = onClick;
}

const csvField = (v) => {
  const s = v === null || v === undefined ? "" : String(v);
  return /[",\r\n]/.test(s) ? `"${s.replaceAll('"', '""')}"` : s;
};
export function downloadCSV(name, header, rows) {
  const text = [header, ...rows].map((r) => r.map(csvField).join(",")).join("\r\n");
  const url = URL.createObjectURL(new Blob(["﻿", text, "\r\n"], { type: "text/csv;charset=utf-8" }));
  const a = Object.assign(document.createElement("a"), { href: url, download: `${name}.csv` });
  document.body.append(a);
  a.click();
  a.remove();
  setTimeout(() => URL.revokeObjectURL(url), 0);
}

// A results table written as HTML (<table class="plain" data-csv="NAME">) as rows
// of text: row- and column-spans repeated into every cell they cover, "–" (no
// value) empty, thousands separators and typographic minus signs removed. A
// <th data-csv-split="a|b"> splits its column's cells ("x ± y", "p class") into
// the columns a and b.
function plainTableRows(tbl) {
  const grid = [];
  [...tbl.rows].forEach((tr, i) => {
    grid[i] ??= [];
    let j = 0;
    for (const cell of tr.cells) {
      while (grid[i][j] !== undefined) j++;
      for (let di = 0; di < cell.rowSpan; di++) {
        for (let dj = 0; dj < cell.colSpan; dj++) (grid[i + di] ??= [])[j + dj] = cell;
      }
      j += cell.colSpan;
    }
  });
  const text = (cell) => {
    const s = (cell?.textContent ?? "").replace(/\s+/g, " ").trim().replaceAll("−", "-");
    if (s === "–") return "";
    return /^-?\d{1,3}(,\d{3})+$/.test(s) ? s.replaceAll(",", "") : s;
  };
  const head = grid[0] || [];
  const out = grid.map((row, i) => row.flatMap((cell, j) => {
    const split = head[j]?.dataset.csvSplit?.split("|");
    if (!split) return [text(cell)];
    if (i === 0) return split;
    const parts = text(cell).split(/\s*±\s*|\s+/);
    return split.map((_, k) => (parts.length === split.length ? parts[k] : k ? "" : text(cell)));
  }));
  return { header: out[0] || [], rows: out.slice(1) };
}

function plainTableButtons() {
  for (const tbl of document.querySelectorAll("table.plain[data-csv]")) {
    if (tbl.dataset.csvReady) continue;
    tbl.dataset.csvReady = "1";
    csvButton(tbl, `plain-${tbl.dataset.csv}`, "Download this table as CSV", () => {
      const { header, rows } = plainTableRows(tbl);
      downloadCSV(`${pageName()}-${tbl.dataset.csv}`, header, rows);
    });
  }
}
// Buttons for the results tables pages write now and later (set up by header()).
let plainObserver;
function watchPlainTables() {
  if (plainObserver) return;
  plainTableButtons();
  plainObserver = new MutationObserver(plainTableButtons);
  plainObserver.observe(document.body, { childList: true, subtree: true });
}

export const numberFormatter = (digits = 4) => (cell) => fmt(cell.getValue(), digits);

// The site characteristics and driver-analysis predictors (R/step03-drivers.R)
// TAS is plotted against, with units, as sites.json has them.
export const PREDICTORS = {
  LAI: "Leaf area index (MODIS, m² m⁻²)",
  EVI: "Enhanced vegetation index (MODIS)",
  NDVI: "Normalized difference vegetation index (MODIS)",
  GPP: "Gross primary productivity (MODIS)",
  SOC: "Soil organic carbon (t ha⁻¹)",
  MATA: "Mean annual air temperature (°C)",
  SSTA: "Seasonal variation of air temperature (°C)",
  IATA: "Inter-annual variation of air temperature (°C)",
  DRTA: "Daily range of air temperature (°C)",
  warm_rate: "Warming rate (°C per year)",
  MAP: "Mean annual precipitation (mm)",
  ELEV: "Elevation (m)",
  NEE: "Mean annual net ecosystem exchange (g C m⁻²)",
  NEE_night: "Mean annual nighttime NEE (g C m⁻²)",
};
export const MODEL_LABELS = { direct: "Direct TAS", total: "Total TAS" };

// The main run's TAS: { run, label, columns, get(model, column) -> Map(site -> row) }.
export async function loadMain() {
  const main = await loadJSON("data/main.json");
  const maps = {};
  main.get = (model, column) => (maps[`${model}|${column}`] ??= new Map(main.tas[model]
    .filter((r) => r.column === column).map((r) => [r.site_ID, r])));
  return main;
}

// TAS ± SE with a significance mark (p < 0.05).
export const tasText = (r) => (r && r.TAS !== null && r.TAS !== undefined
  ? `${fmt(r.TAS, 4)} ± ${fmt(r.TAS_se, 4)}${r.TASp < 0.05 ? " *" : ""}` : "–");
// Links to the site's network pages and data product.
export const siteLinks = (s) => (s.links || []).map((l) => `<a href="${l.url}" target="_blank" rel="noopener">${l.label}</a>`).join("");
// A short column label for crowded axes: "Soil: sensor", "Air: 3-day running mean".
export const shortLabel = (c) => (COLUMN_LABELS[c] || c).replace(/^Soil temperature \((.*)\)$/, "Soil: $1")
  .replace(/^Air temperature \((.*)\)$/, "Air: $1");
