import { header, loadJSON, compare, summarise, siteMaps, table, fmt, tasClass, params, setParams, loadScreen, VERDICT_COLORS, filterSites } from "./common.js";

header("index.html");
const sites = await loadJSON("data/sites.json");
const sets = await loadJSON("data/resultsets.json");
const tas = Object.fromEntries(await Promise.all(sets.map(async (s) => [s.id, await loadJSON(`data/tas/${s.id}.json`)])));

// Result-set table, each compared with the published estimates.
const rows = sets.map((s) => {
  const out = { id: s.id, label: s.label, stage: s.stage, description: s.description, sha: s.git_sha,
    profile: [].concat(s.fit_profile || []).join(","), n_sites: s.n_sites_added ? `${s.n_sites} (${s.n_sites_added} added)` : s.n_sites };
  for (const model of ["total", "direct"]) {
    const sum = summarise(filterSites(compare(tas.published, tas[s.id], model).filter((r) => r.TAS_new !== null && r.TAS_ref !== null)));
    out[`${model}_median`] = sum.median_abs;
    out[`${model}_changed`] = sum.changed;
    out[`${model}_flips`] = sum.flips.length;
  }
  return out;
});
const f = (cell) => fmt(cell.getValue(), 5);
table("sets", rows, [
  { title: "Stage", field: "stage", width: 90 },
  { title: "Result set", field: "label" },
  { title: "Sites", field: "n_sites", hozAlign: "right" },
  { title: "Profile", field: "profile" },
  { title: "Commit", field: "sha" },
  { title: "Total: median |ΔTAS|", field: "total_median", formatter: f, hozAlign: "right" },
  { title: "changed", field: "total_changed", hozAlign: "right" },
  { title: "flips", field: "total_flips", hozAlign: "right" },
  { title: "Direct: median |ΔTAS|", field: "direct_median", formatter: f, hozAlign: "right" },
  { title: "changed", field: "direct_changed", hozAlign: "right" },
  { title: "flips", field: "direct_flips", hozAlign: "right" },
  { title: "Description", field: "description", formatter: "textarea", widthGrow: 3 },
], { height: "auto", onRowClick: (r) => { location.href = `compare.html?a=published&b=${r.id}`; } });

// Maps.
const screen = await loadScreen();
const setSel = document.getElementById("set");
for (const s of sets) setSel.insertAdjacentHTML("beforeend", `<option value="${s.id}">${s.label}</option>`);
const p = params();
setSel.value = p.get("set") || sets[sets.length - 1].id;
document.getElementById("model").value = p.get("model") || "total";
document.getElementById("colour").value = p.get("colour") || "tas";

function draw() {
  const set = setSel.value;
  const model = document.getElementById("model").value;
  const colour = document.getElementById("colour").value;
  setParams({ set, model, colour });
  const bySite = new Map((tas[set][model] || []).map((r) => [r.site_ID, r]));
  const ref = new Map((tas.published[model] || []).map((r) => [r.site_ID, r]));
  const shown = sites.filter((s) => bySite.has(s.site_ID));
  const opts = { onClick: (id) => { location.href = `site.html?site=${id}`; } };
  if (colour === "tas") Object.assign(opts, { value: (s) => bySite.get(s.site_ID).TAS, label: `TAS (${model})` });
  if (colour === "delta") Object.assign(opts, { value: (s) => (bySite.get(s.site_ID).TAS ?? NaN) - (ref.get(s.site_ID)?.TAS ?? NaN), label: "ΔTAS" });
  if (colour === "class") Object.assign(opts, { category: (s) => tasClass(bySite.get(s.site_ID).TAS, bySite.get(s.site_ID).TASp), label: "class" });
  if (colour === "verdict") Object.assign(opts, { category: (s) => screen?.sensor.get(s.site_ID)?.verdict || "NA", label: "sensor", colors: VERDICT_COLORS });
  siteMaps("geo", "clim", shown, opts);
}
for (const id of ["set", "model", "colour"]) document.getElementById(id).addEventListener("change", draw);
draw();
