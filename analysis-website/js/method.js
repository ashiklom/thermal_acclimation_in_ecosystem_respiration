import { header, SITES, loadJSON, loadMain, baseLayout, PLOT_CONFIG, fmt, tempSourcePicker, tempSource, columnLabel,
  MODEL_LABELS, params, setParams } from "./common.js";
import { rng, lstsq, mean } from "./docs-common.js";
import { drawWalkthrough } from "./walkthrough.js";

header("method.html");

// ---- simulation
const WINDOWS = 8, PER_WINDOW = 40, Q = 0.07, G = 0.3;
let seed = 7;
const val = (id) => Number(document.getElementById(`s-${id}`).value);
for (const id of ["direct", "indirect", "years", "tsd", "noise"]) {
  const el = document.getElementById(`s-${id}`);
  const show = () => { document.getElementById(`o-${id}`).textContent = el.value; };
  show();
  el.addEventListener("input", () => { show(); simulate(); });
}
document.getElementById("s-redraw").addEventListener("click", () => { seed += 1; simulate(); });

function simulate() {
  const r = rng(seed);
  const direct = val("direct"), indirect = val("indirect"), years = val("years"), tsd = val("tsd"), noise = val("noise");
  // substrate (daytime uptake) anomaly per °C of window-year warmth, so that its
  // effect on ln respiration, G per unit, is `indirect` per °C
  const coupling = indirect / G;
  const fits = { total: [], direct: [] };
  const records = [];
  for (let w = 0; w < WINDOWS; w++) {
    const tref = 8 + 10 * Math.sin((Math.PI * (w + 0.5)) / WINDOWS);
    const cells = [];
    for (let y = 0; y < years; y++) {
      const anomaly = tsd * r.norm();
      const ty = tref + anomaly;
      const t = [], p = [], lr = [];
      for (let i = 0; i < PER_WINDOW; i++) {
        const ti = ty + 3 * r.norm();
        const pi = coupling * anomaly + r.norm();
        t.push(ti); p.push(pi);
        lr.push(0.5 + Q * (ti - tref) + G * pi + direct * anomaly + noise * r.norm());
      }
      if (w === Math.floor(WINDOWS / 2)) records.push({ y, ty, t, lr });
      // total model: ln R ~ T; direct model: ln R ~ T + substrate; each at the reference
      const tot = lstsq(t.map((x) => [x - tref]), lr);
      const dir = lstsq(t.map((x, i) => [x - tref, p[i]]), lr);
      cells.push({ w, y, ty, tref, tot: tot.beta, total: tot.predict([0]), direct: dir.predict([0, 0]) });
    }
    for (const model of ["total", "direct"]) {
      const control = cells[0][model];
      for (const c of cells) fits[model].push({ w, y: c.y, x: c.ty, ln: c[model] - control });
    }
    if (w === Math.floor(WINDOWS / 2)) records.forEach((rec, i) => { rec.fit = cells[i]; });
  }
  // TAS: slope within windows (the regression with a window fixed effect)
  const slope = (rows) => {
    const anom = [];
    for (let w = 0; w < WINDOWS; w++) {
      const g = rows.filter((q) => q.w === w), mx = mean(g.map((q) => q.x)), my = mean(g.map((q) => q.ln));
      for (const q of g) anom.push({ x: q.x - mx, y: q.ln - my });
    }
    const b = anom.reduce((a, q) => a + q.x * q.y, 0) / anom.reduce((a, q) => a + q.x * q.x, 0);
    return { b, anom };
  };
  const est = { total: slope(fits.total), direct: slope(fits.direct) };
  document.getElementById("s-readout").innerHTML =
    `True direct TAS <b>${fmt(direct, 3)}</b>, true total TAS (direct + indirect) <b>${fmt(direct + indirect, 3)}</b> · ` +
    `estimated: direct <b style="color:#1a7f37">${fmt(est.direct.b, 3)}</b>, total <b style="color:#d55e00">${fmt(est.total.b, 3)}</b>`;

  const tys = records.map((q) => q.ty), lo = Math.min(...tys), hi = Math.max(...tys);
  const shade = (t) => `hsl(${hi > lo ? 230 - (230 * (t - lo)) / (hi - lo) : 120}, 65%, 45%)`;
  const tref = records[0].fit.tref;
  const traces = records.flatMap((q) => {
    const xs = [Math.min(...q.t), Math.max(...q.t)];
    return [{ type: "scatter", mode: "markers", x: q.t, y: q.lr.map(Math.exp), marker: { color: shade(q.ty), size: 4, opacity: 0.5 },
      hoverinfo: "skip", showlegend: false },
    { type: "scatter", mode: "lines", x: xs, y: xs.map((x) => Math.exp(q.fit.tot[0] + q.fit.tot[1] * (x - tref))),
      line: { color: shade(q.ty), width: 2 }, name: `year ${q.y + 1}: ${fmt(q.ty, 1)} °C`, hoverinfo: "name", showlegend: false }];
  });
  Plotly.react("s-records", traces, baseLayout({ title: { text: "One window, every year", font: { size: 13 } },
    xaxis: { title: "Temperature (°C)" }, yaxis: { title: "Nighttime respiration", rangemode: "tozero" },
    shapes: [{ type: "line", x0: tref, x1: tref, yref: "paper", y0: 0, y1: 1, line: { dash: "dot", color: "#666" } }] }), PLOT_CONFIG);
  const colors = { total: "#d55e00", direct: "#1a7f37" };
  const st = ["total", "direct"].flatMap((m) => {
    const xs = est[m].anom.map((q) => q.x), x0 = Math.min(...xs), x1 = Math.max(...xs);
    return [{ type: "scatter", mode: "markers", x: xs, y: est[m].anom.map((q) => q.y), name: `${MODEL_LABELS[m]} model`,
      marker: { color: colors[m], size: 6, opacity: 0.6 } },
    { type: "scatter", mode: "lines", x: [x0, x1], y: [x0 * est[m].b, x1 * est[m].b], line: { color: colors[m], width: 2.5 },
      name: `${MODEL_LABELS[m]} ${fmt(est[m].b, 3)}` }];
  });
  Plotly.react("s-slopes", st, baseLayout({ title: { text: "Every window-year, within windows", font: { size: 13 } },
    xaxis: { title: "Temperature relative to the window's mean (°C)" }, yaxis: { title: "ln ratio relative to the window's mean" },
    legend: { orientation: "h", y: -0.25 } }), PLOT_CONFIG);
}
simulate();

// ---- real examples
const main = await loadMain();
document.getElementById("model").value = params().get("model") || "total";
tempSourcePicker("controls", examples);
document.getElementById("model").addEventListener("change", examples);

async function examples() {
  const column = tempSource(), model = document.getElementById("model").value;
  setParams({ model });
  const rows = [...main.get(model, column).values()].filter((r) => r.TAS !== null && r.TAS_se && (r.n_years ?? 0) >= 6);
  const t = (r) => r.TAS / r.TAS_se;
  const sig = rows.filter((r) => r.TASp < 0.05);
  const picks = [
    ["Strong negative TAS", sig.filter((r) => r.TAS < 0).sort((a, b) => t(a) - t(b))[0]],
    ["TAS near zero", [...rows].sort((a, b) => Math.abs(a.TAS) - Math.abs(b.TAS))[0]],
    ["Strong positive TAS", sig.filter((r) => r.TAS > 0).sort((a, b) => t(b) - t(a))[0]],
  ].filter(([, r]) => r);
  const div = document.getElementById("examples");
  div.innerHTML = picks.map(([title, r], i) => {
    const s = SITES.get(r.site_ID);
    return `<div class="example"><h3>${title}: <a href="site.html?site=${r.site_ID}&model=${model}">${r.site_ID}</a>` +
      ` <span class="small">${s?.site_name || ""} · ${s?.IGBP}, ${s?.Climate_class} · ${r.n_years} years, ${r.nwindow} windows</span></h3>` +
      `<div class="grid2"><div id="ex-curves-${i}" class="plot"></div><div id="ex-anom-${i}" class="plot"></div></div></div>`;
  }).join("") || `<p class="note">No site qualifies with ${columnLabel(column)}.</p>`;
  for (const [i, [, r]] of picks.entries()) {
    const fits = await loadJSON(`data/site/${r.site_ID}.json`);
    drawWalkthrough({ curveDiv: `ex-curves-${i}`, anomalyDiv: `ex-anom-${i}`, site: fits, column, model, tas: r,
      temperatureLabel: `${columnLabel(column)} (°C)` });
  }
}
examples();
