import { header, loadJSON, compare, summarise, table, params, setParams, numberFormatter, siteCell, filterSites } from "./common.js";

header("stages.html");
const stages = await loadJSON("data/stages.json");
const sets = await loadJSON("data/resultsets.json");
const label = (id) => (sets.find((s) => s.id === id) || { label: id }).label;
const cache = {};
const getTas = async (id) => (cache[id] ??= await loadJSON(`data/tas/${id}.json`));
const p = params();
if (p.get("threshold")) document.getElementById("threshold").value = p.get("threshold");

const stat = (v, k) => `<div class="stat"><div class="v">${v}</div><div class="k">${k}</div></div>`;
const tables = [];

async function draw() {
  const threshold = Number(document.getElementById("threshold").value);
  setParams({ threshold });
  const root = document.getElementById("stages");
  root.innerHTML = stages.length ? "" : `<p class="note">No method stage has a saved run yet.</p>`;
  tables.length = 0;
  for (const st of stages) {
    const text = await fetch(`stages/${st.stage}.html`).then((r) => (r.ok ? r.text() : ""));
    const [ref, run] = [await getTas(st.reference), await getTas(st.run)];
    const byModel = Object.fromEntries(["total", "direct"].map((m) => [m, filterSites(compare(ref, run, m, threshold))]));
    const inputs = filterSites(st.inputs || []);
    const affected = new Set(inputs.map((r) => r.site_ID));
    const sec = document.createElement("section");
    sec.innerHTML = `<h2 id="${st.stage}">${st.stage}: ${st.title}</h2>${text}
      <p class="small">Run <b>${label(st.run)}</b> against <b>${label(st.reference)}</b> ·
        <a href="compare.html?a=${st.reference}&b=${st.run}&model=total&threshold=${threshold}">compare total</a> ·
        <a href="compare.html?a=${st.reference}&b=${st.run}&model=direct&threshold=${threshold}">compare direct</a></p>`;
    const stats = document.createElement("div");
    stats.className = "stats";
    const parts = [stat(st.inputs ? affected.size : "–", "sites with changed inputs")];
    for (const m of ["total", "direct"]) {
      const s = summarise(byModel[m].filter((r) => r.TAS_new !== null && r.TAS_ref !== null));
      parts.push(stat(s.changed, `${m}: |ΔTAS| ≥ ${threshold}`), stat(s.transitions, `${m}: class changes`),
        stat(s.flips.length, `${m}: significant sign flips`));
    }
    stats.innerHTML = parts.join("");
    sec.appendChild(stats);
    const flips = ["total", "direct"].flatMap((m) => summarise(byModel[m]).flips.map((x) => `${x} (${m})`));
    if (flips.length) sec.insertAdjacentHTML("beforeend", `<div class="note"><span class="flip">Significant sign flips:</span> ${flips.join(", ")}</div>`);
    const unexpected = ["total", "direct"].flatMap((m) => byModel[m].filter((r) => r.changed && st.inputs && !affected.has(r.site_ID)).map((r) => `${r.site_ID} (${m})`));
    // (a stage that changes no input changes the fits themselves: every site may move)
    if (unexpected.length && affected.size) sec.insertAdjacentHTML("beforeend", `<div class="note">TAS changed although the inputs did not: ${unexpected.join(", ")}</div>`);

    // one row per site whose inputs or TAS changed
    const tas = (m, site) => byModel[m].find((r) => r.site_ID === site) || {};
    const sites = [...new Set([...affected, ...["total", "direct"].flatMap((m) => byModel[m].filter((r) => r.changed || r.transition).map((r) => r.site_ID))])];
    const rows = sites.map((site) => {
      const inp = inputs.find((r) => r.site_ID === site) || {};
      const t = tas("total", site), d = tas("direct", site);
      return { site_ID: site, changed: inp.changed || "", ...inp,
        dTAS_total: t.dTAS ?? null, trans_total: t.transition || "", flip_total: !!t.flip,
        dTAS_direct: d.dTAS ?? null, trans_direct: d.transition || "", flip_direct: !!d.flip };
    }).sort((a, b) => (b.flip_total + b.flip_direct) - (a.flip_total + a.flip_direct) || Math.abs(b.dTAS_total ?? 0) - Math.abs(a.dTAS_total ?? 0));
    const div = document.createElement("div");
    div.id = `table-${st.stage}`;
    sec.appendChild(div);
    root.appendChild(sec);
    const pair = (name, field, digits = 3) => ({ title: name, columns: [
      { title: "ref", field: `${field}_ref`, formatter: numberFormatter(digits), hozAlign: "right", headerFilter: false },
      { title: "new", field: `${field}_new`, formatter: numberFormatter(digits), hozAlign: "right", headerFilter: false }] });
    const cols = [
      { title: "Site", field: "site_ID", frozen: true, formatter: siteCell },
      { title: "inputs changed", field: "changed" },
      { title: "ΔTAS total", field: "dTAS_total", formatter: numberFormatter(5), hozAlign: "right", headerFilter: false },
      { title: "class total", field: "trans_total" },
      { title: "ΔTAS direct", field: "dTAS_direct", formatter: numberFormatter(5), hozAlign: "right", headerFilter: false },
      { title: "class direct", field: "trans_direct" },
      pair("gStart", "gStart", 0), pair("gEnd", "gEnd", 0), pair("tStart", "tStart"), pair("tEnd", "tEnd"),
      pair("windows", "nwindow", 0), pair("night rows", "n_night", 0), pair("control year", "control_year", 0),
      pair("mean NEE_daytime", "NEE_daytime_mean"),
    ];
    if (rows.length) tables.push(table(div.id, rows, cols, { height: rows.length > 12 ? "420px" : undefined }));
    else div.innerHTML = `<p class="small">No site changed.</p>`;
  }
  if (location.hash) document.querySelector(location.hash)?.scrollIntoView();
}
document.getElementById("threshold").addEventListener("change", draw);
draw();
