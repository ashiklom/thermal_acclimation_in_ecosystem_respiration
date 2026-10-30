import { header, SITES, loadJSON, loadMain, siteMaps, table, fmt, params, setParams, tempSourcePicker, tempSource,
  columnLabel, COLUMN_COLORS, SOIL_COLUMNS, AIR_COLUMNS, baseLayout, PLOT_CONFIG, tasText, siteLinks, isAdvanced } from "./common.js";
import { drawWalkthrough, windowLabel } from "./walkthrough.js";

header("site.html");
const main = await loadMain();
const ids = [...SITES.keys()].sort();
const p = params();
const select = document.getElementById("site");
select.innerHTML = ids.map((id) => `<option value="${id}">${id}${SITES.get(id).site_name ? ` · ${SITES.get(id).site_name}` : ""}</option>`).join("");
select.value = ids.includes(p.get("site")) ? p.get("site") : (ids.includes("DE-Hai") ? "DE-Hai" : ids[0]);
document.getElementById("period").value = p.get("period") || "night";
document.getElementById("model").value = p.get("model") || "total";
tempSourcePicker("controls", () => { drawTemperature(); drawResults(); drawWalk(); });

let site, temp, fits, shown = { soil: new Set(), air: new Set() }, tbl;

async function load() {
  const id = select.value;
  setParams({ site: id });
  site = SITES.get(id);
  [temp, fits] = await Promise.all([loadJSON(`data/site-temperature/${id}.json`).catch(() => null),
    loadJSON(`data/site/${id}.json`).catch(() => null)]);
  document.getElementById("title").textContent = `${id}${site.site_name ? ` — ${site.site_name}` : ""}`;
  const ns = (x, pos, neg) => `${Math.abs(x).toFixed(3)}° ${x >= 0 ? pos : neg}`;
  document.getElementById("meta").innerHTML = [`${site.IGBP} (vegetation type)`, `${site.Climate_class} (Köppen climate)`,
    `${ns(site.LAT, "N", "S")}, ${ns(site.LONG, "E", "W")}`, site.ELEV !== null ? `${Math.round(site.ELEV)} m` : null,
    site.MAT !== null ? `mean annual air temperature ${fmt(site.MAT, 1)} °C` : null,
    site.MAP !== null ? `precipitation ${Math.round(site.MAP)} mm` : null,
    `data: ${site.source_label}`].filter(Boolean).join(" · ");
  document.getElementById("links").innerHTML = siteLinks(site);
  document.getElementById("citation").innerHTML = site.citation ? `Data citation: ${site.citation}` : "";
  siteMaps("geo", "clim", [site], { category: () => site.IGBP, label: "Vegetation type", colors: { [site.IGBP]: "#0969da" }, focus: true });
  shown = { soil: new Set(["TS_manuscript"]), air: new Set(["TA_measured"]) };
  drawTemperature();
  drawResults();
  drawWalk();
}

function drawTemperature() {
  const period = document.getElementById("period").value;
  setParams({ period });
  const selected = tempSource();
  for (const [kind, columns, div, picks] of [["soil", SOIL_COLUMNS, "soil", "soilpicks"], ["air", AIR_COLUMNS, "air", "airpicks"]]) {
    const values = temp?.[period] || {};
    const have = columns.filter((c) => values[c]);
    if (have.includes(selected)) shown[kind].add(selected);
    document.getElementById(picks).innerHTML = have.map((c) => `<label><input type="checkbox" value="${c}"${shown[kind].has(c) ? " checked" : ""}>` +
      `<span class="swatch" style="background:${COLUMN_COLORS[c]}"></span>${columnLabel(c, false)}</label>`).join("");
    document.getElementById(picks).querySelectorAll("input").forEach((box) => box.addEventListener("change", () => {
      if (box.checked) shown[kind].add(box.value); else shown[kind].delete(box.value);
      drawTemperature();
    }));
    const traces = have.filter((c) => shown[kind].has(c)).map((c) => ({ type: "scattergl", mode: "lines", x: temp.date, y: values[c],
      name: columnLabel(c, false), line: { color: COLUMN_COLORS[c], width: c === selected ? 2.8 : 1 },
      opacity: c === selected ? 1 : 0.75, hovertemplate: `${columnLabel(c, false)}<br>%{x}: %{y:.1f} °C<extra></extra>` }));
    const layout = baseLayout({ margin: { l: 60, r: 20, t: 10, b: 40 }, showlegend: false, hovermode: "x unified",
      yaxis: { title: `${kind === "soil" ? "Soil" : "Air"} temperature (°C)` }, xaxis: { type: "date" } });
    if (!have.length) {
      layout.annotations = [{ text: kind === "soil" ? "No soil temperature at this site: its TAS uses air temperature only."
        : "No air-temperature series", showarrow: false, xref: "paper", yref: "paper", x: 0.5, y: 0.5 }];
    }
    const el = document.getElementById(div);
    if (el._fullLayout?.xaxis && temp?.date?.length) layout.xaxis.range = el._fullLayout.xaxis.range;
    Plotly.react(div, traces, layout, PLOT_CONFIG);
  }
  syncAxes();
}

// zooming one temperature plot zooms the other
let syncing = false;
function syncAxes() {
  for (const [a, b] of [["soil", "air"], ["air", "soil"]]) {
    const el = document.getElementById(a);
    if (el.dataset.synced) continue;
    el.dataset.synced = "1";
    el.on("plotly_relayout", (e) => {
      if (syncing) return;
      const update = e["xaxis.autorange"] ? { "xaxis.autorange": true }
        : e["xaxis.range[0]"] !== undefined ? { "xaxis.range": [e["xaxis.range[0]"], e["xaxis.range[1]"]] }
          : e["xaxis.range"] ? { "xaxis.range": e["xaxis.range"] } : null;
      if (!update) return;
      syncing = true;
      Plotly.relayout(b, update).finally(() => { syncing = false; });
    });
  }
}

function drawResults() {
  const selected = tempSource();
  const rows = [...SOIL_COLUMNS, ...AIR_COLUMNS].map((c) => {
    const t = main.get("total", c).get(site.site_ID), d = main.get("direct", c).get(site.site_ID);
    return { column: c, label: columnLabel(c, false), total: tasText(t), direct: tasText(d), years: t?.n_years ?? d?.n_years ?? null,
      windows: t?.nwindow ?? null, control: t?.control_year ?? null, has: Boolean(t || d) };
  }).filter((r) => r.has);
  const bold = (cell) => (cell.getRow().getData().column === selected ? `<b>${cell.getValue() ?? "–"}</b>` : (cell.getValue() ?? "–"));
  const columns = [
    { title: "Temperature source", field: "label", formatter: bold, headerFilter: false },
    { title: "Total TAS", field: "total", formatter: bold, hozAlign: "right", headerFilter: false, headerSort: false },
    { title: "Direct TAS", field: "direct", formatter: bold, hozAlign: "right", headerFilter: false, headerSort: false },
    { title: "Years", field: "years", hozAlign: "right", headerFilter: false },
    { title: "Windows", field: "windows", hozAlign: "right", headerFilter: false },
    { title: "Control year", field: "control", hozAlign: "right", headerFilter: false },
  ];
  if (tbl) tbl.destroy();
  tbl = table("results", rows, columns, { height: false });
}

function drawWalk() {
  const column = tempSource();
  const model = document.getElementById("model").value;
  setParams({ model });
  const tas = main.get(model, column).get(site.site_ID);
  const sel = document.getElementById("window");
  const res = drawWalkthrough({ curveDiv: "curves", ratioDiv: "ratios", anomalyDiv: "anomalies", site: fits, column, model,
    window: sel.value || p.get("window"), tas, temperatureLabel: `${columnLabel(column)} (°C)` });
  sel.innerHTML = res.windows.map((w) => `<option value="${w}">${windowLabel(w)}</option>`).join("");
  if (res.window) sel.value = res.window;
  document.getElementById("walknote").innerHTML = res.windows.length ? ""
    : `<div class="note">No fits for ${columnLabel(column)} at this site${column.startsWith("TS_") && SITES.get(site.site_ID).ta_only
      ? " (it has no soil temperature; choose air temperature)" : ""}.</div>`;
}

select.addEventListener("change", () => { document.getElementById("window").innerHTML = ""; load(); });
document.getElementById("period").addEventListener("change", drawTemperature);
document.getElementById("model").addEventListener("change", drawWalk);
document.getElementById("window").addEventListener("change", drawWalk);
load();
