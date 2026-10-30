// How one site's TAS comes out of its window-year fits (data/site/<site>.json,
// R/step02-tas.R estimate_tas()), drawn for the site explorer and the method
// page's examples:
//   curves: in one window, each year's fitted respiration–temperature curve
//     through its reference respiration ERref at the window's reference
//     temperature (the same in every year);
//   ratios: each window-year's ln(ERref / ERref of the control year) against
//     that year's mean temperature in the window, coloured by window;
//   anomalies: the same, each relative to its window's mean (the window fixed
//     effect of the TAS regression), with the TAS slope.
import { baseLayout, PLOT_CONFIG, fmt, MODEL_LABELS } from "./common.js";

const finite = (x) => x !== null && x !== undefined && Number.isFinite(x);

// The rows of one column and model: [{growing_year, window, TS, alpha, ...}].
export function fitRows(site, model, column) {
  const t = site?.[model]?.[column];
  if (!t) return [];
  const n = t.growing_year.length;
  return Array.from({ length: n }, (_, i) => Object.fromEntries(Object.keys(t).map((k) => [k, t[k][i]])));
}

const windowStart = (w) => Number(String(w).split("_")[0]);
export const windowLabel = (w) => { const [a, b] = String(w).split("_"); return `day ${a}–${b}`; };

// The window's reference temperature: where the year's total-model curve
// exp(αT + βT²)·C0 equals its ERref (the root nearest the year's mean
// temperature), the median over the years; else the years' mean temperature.
export function referenceTemperature(totalRows, rows) {
  const roots = totalRows.filter((r) => finite(r.alpha) && finite(r.beta) && finite(r.C0) && finite(r.ERref) && r.ERref > 0)
    .map((r) => {
      const c = -Math.log(r.ERref / r.C0), disc = r.alpha ** 2 - 4 * r.beta * c;
      if (disc < 0 || r.beta === 0) return null;
      const xs = [(-r.alpha + Math.sqrt(disc)) / (2 * r.beta), (-r.alpha - Math.sqrt(disc)) / (2 * r.beta)];
      return xs.sort((a, b) => Math.abs(a - r.TS) - Math.abs(b - r.TS))[0];
    }).filter(finite).sort((a, b) => a - b);
  if (roots.length) return roots[Math.floor(roots.length / 2)];
  const ts = rows.map((r) => r.TS).filter(finite);
  return ts.length ? ts.reduce((a, b) => a + b, 0) / ts.length : null;
}

export function windows(rows) {
  return [...new Set(rows.filter((r) => finite(r.lnRatio)).map((r) => r.window))].sort((a, b) => windowStart(a) - windowStart(b));
}

const COOL_WARM = [[0, "#2166ac"], [0.5, "#f7f7f7"], [1, "#b2182b"]];
function interp(scale, t) {
  const hex = (h) => [1, 3, 5].map((i) => parseInt(h.slice(i, i + 2), 16));
  for (let i = 1; i < scale.length; i++) {
    if (t <= scale[i][0]) {
      const [a, b] = [hex(scale[i - 1][1]), hex(scale[i][1])], f = (t - scale[i - 1][0]) / (scale[i][0] - scale[i - 1][0]);
      return `rgb(${a.map((x, j) => Math.round(x + f * (b[j] - x))).join(",")})`;
    }
  }
  return scale[scale.length - 1][1];
}

// Draws the three plots into curveDiv, ratioDiv and anomalyDiv for one model
// and window (the middle window if not given or without fits). `site` is the
// site's fits file, `tas` the column's outcome row. Returns { windows, window }.
export function drawWalkthrough({ curveDiv, ratioDiv, anomalyDiv, site, column, model, window, tas, temperatureLabel = "Temperature (°C)" }) {
  const rows = fitRows(site, model, column);
  const total = fitRows(site, "total", column);
  const ws = windows(rows);
  const empty = (div, msg) => Plotly.react(div, [], baseLayout({ annotations: [{ text: msg, showarrow: false, xref: "paper",
    yref: "paper", x: 0.5, y: 0.5 }], xaxis: { visible: false }, yaxis: { visible: false } }), PLOT_CONFIG);
  if (!ws.length) {
    for (const d of [curveDiv, ratioDiv, anomalyDiv]) if (d) empty(d, "no window-year fits");
    return { windows: ws, window: null };
  }
  if (!ws.includes(window)) window = ws[Math.floor(ws.length / 2)];

  // (a) curves of one window
  if (curveDiv) {
    const inWindow = rows.filter((r) => r.window === window && finite(r.alpha) && finite(r.ERref));
    const tr = referenceTemperature(total.filter((r) => r.window === window), inWindow);
    const ts = inWindow.map((r) => r.TS).filter(finite);
    const lo = Math.min(tr, ...ts) - 4, hi = Math.max(tr, ...ts) + 4;
    const grid = Array.from({ length: 60 }, (_, i) => lo + (i * (hi - lo)) / 59);
    const tmin = Math.min(...ts), tmax = Math.max(...ts);
    const shade = (t) => interp(COOL_WARM, tmax > tmin ? (t - tmin) / (tmax - tmin) : 0.5);
    const curve = (r, t) => r.ERref * Math.exp(r.alpha * (t - tr) + r.beta * (t * t - tr * tr));
    const traces = inWindow.map((r) => ({ type: "scatter", mode: "lines", x: grid, y: grid.map((t) => curve(r, t)),
      line: { color: shade(r.TS), width: 1.8 }, name: String(r.growing_year), hoverinfo: "name+y", showlegend: false }));
    traces.push({ type: "scatter", mode: "markers", x: inWindow.map(() => tr), y: inWindow.map((r) => r.ERref),
      marker: { color: inWindow.map((r) => shade(r.TS)), size: 9, line: { width: 1, color: "#333" } }, name: "ERref",
      text: inWindow.map((r) => `${r.growing_year}: mean ${fmt(r.TS, 1)} °C in the window<br>ERref ${fmt(r.ERref, 3)}` +
        `<br>ln ratio ${fmt(r.lnRatio, 3)}`), hoverinfo: "text", showlegend: false });
    traces.push({ type: "scatter", mode: "markers", x: inWindow.map((r) => r.TS), y: inWindow.map((r) => curve(r, r.TS)),
      marker: { symbol: "line-ns-open", size: 10, color: inWindow.map((r) => shade(r.TS)), line: { width: 2 } },
      hoverinfo: "skip", showlegend: false });
    Plotly.react(curveDiv, traces, baseLayout({
      title: { text: `${windowLabel(window)}: each year's fitted curve (${model} model)`, font: { size: 13 } },
      xaxis: { title: temperatureLabel }, yaxis: { title: "Nighttime respiration (µmol m⁻² s⁻¹)", rangemode: "tozero" },
      shapes: [{ type: "line", x0: tr, x1: tr, yref: "paper", y0: 0, y1: 1, line: { dash: "dot", color: "#666", width: 1 } }],
      annotations: [{ x: tr, yref: "paper", y: 0.02, text: " reference temperature", showarrow: false, xanchor: "left", yanchor: "bottom", font: { size: 11, color: "#666" } }],
    }), PLOT_CONFIG);
  }

  // (b) ln ratio against temperature, (c) the same within windows
  const ok = rows.filter((r) => finite(r.lnRatio) && finite(r.TS));
  // windows coloured through the season, purple (first) to orange (last)
  const color = (w) => `hsl(${270 - (ws.length > 1 ? ws.indexOf(w) / (ws.length - 1) : 0) * 245}, 70%, 45%)`;
  const byWindow = ws.map((w) => ({ w, rs: ok.filter((r) => r.window === w) }));
  if (ratioDiv) {
    Plotly.react(ratioDiv, byWindow.map(({ w, rs }) => ({ type: "scatter", mode: "markers", x: rs.map((r) => r.TS),
      y: rs.map((r) => r.lnRatio), name: windowLabel(w), marker: { color: color(w), size: w === window ? 10 : 7,
        line: { width: w === window ? 1.5 : 0, color: "#000" } },
      text: rs.map((r) => `${windowLabel(w)}, ${r.growing_year}`), hovertemplate: "%{text}<br>%{x:.1f} °C, ln ratio %{y:.3f}<extra></extra>" })),
    baseLayout({ title: { text: "Every window and year", font: { size: 13 } }, xaxis: { title: `Mean temperature in the window that year (°C)` },
      yaxis: { title: "ln(ERref / ERref in the control year)", zeroline: true }, showlegend: false }), PLOT_CONFIG);
  }
  if (anomalyDiv) {
    const anom = byWindow.flatMap(({ w, rs }) => {
      if (rs.length < 2) return [];
      const mx = rs.reduce((a, r) => a + r.TS, 0) / rs.length, my = rs.reduce((a, r) => a + r.lnRatio, 0) / rs.length;
      return rs.map((r) => ({ w, x: r.TS - mx, y: r.lnRatio - my, year: r.growing_year }));
    });
    const traces = byWindow.map(({ w }) => {
      const a = anom.filter((q) => q.w === w);
      return { type: "scatter", mode: "markers", x: a.map((q) => q.x), y: a.map((q) => q.y), name: windowLabel(w),
        marker: { color: color(w), size: 7 }, text: a.map((q) => `${windowLabel(w)}, ${q.year}`),
        hovertemplate: "%{text}<br>%{x:+.1f} °C, %{y:+.3f}<extra></extra>", showlegend: false };
    });
    if (tas && finite(tas.TAS) && anom.length) {
      const xs = anom.map((q) => q.x), x0 = Math.min(...xs), x1 = Math.max(...xs);
      const band = (s) => [x0, x1].map((x) => (tas.TAS + s * (tas.TAS_se || 0)) * x);
      traces.push({ type: "scatter", mode: "lines", x: [x0, x1, x1, x0], y: [...band(1), ...band(-1).reverse()], fill: "toself",
        fillcolor: "#0002", line: { width: 0 }, hoverinfo: "skip", showlegend: false });
      traces.push({ type: "scatter", mode: "lines", x: [x0, x1], y: [x0, x1].map((x) => tas.TAS * x), line: { color: "#000", width: 2 },
        name: "TAS", hoverinfo: "skip", showlegend: false });
    }
    const title = tas && finite(tas.TAS) ? `${MODEL_LABELS[model]} = ${fmt(tas.TAS, 4)} ± ${fmt(tas.TAS_se, 4)} per °C (p = ${fmt(tas.TASp, 3)})`
      : `${MODEL_LABELS[model]}: no estimate`;
    Plotly.react(anomalyDiv, traces, baseLayout({ title: { text: title, font: { size: 13 } },
      xaxis: { title: "Temperature relative to the window's mean (°C)", zeroline: true },
      yaxis: { title: "ln ratio relative to the window's mean", zeroline: true } }), PLOT_CONFIG);
  }
  return { windows: ws, window };
}
