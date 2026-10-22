// Shared helpers of the documentation pages (docs-*.html): R code snippets
// from data/code.json (built from R/*.R by build.py, never from results),
// R constants read from that code, and small numerical tools for the pages'
// simulations.

import { loadJSON } from "./common.js";

export const loadCode = () => loadJSON("data/code.json");

export function escapeHTML(s) {
  return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

// One definition's source as a collapsible block.
export function codeBlock(code, name, open = false) {
  const c = code[name];
  if (!c) return `<p class="note">No R definition named <code>${escapeHTML(name)}</code> (data/code.json is stale?)</p>`;
  return `<details class="code"${open ? " open" : ""}><summary><code>${escapeHTML(name)}</code>
    <span class="small">${c.file}, line ${c.line}</span></summary><pre><code>${escapeHTML(c.code)}</code></pre></details>`;
}

// Fill every element with data-code="name1 name2 ..." with those definitions
// (open if the element has data-open).
export function renderSnippets(code) {
  for (const el of document.querySelectorAll("[data-code]")) {
    el.innerHTML = el.dataset.code.split(/\s+/).filter(Boolean).map((n) => codeBlock(code, n, el.hasAttribute("data-open"))).join("");
  }
}

// The value of an R constant defined as a number (`X <- 0.85`), or as named
// numbers (`c(a = 1, b = 2)` or `list(a = 1, b = 2)`, returned as an object).
export function rConstant(code, name) {
  const c = code[name];
  if (!c) throw new Error(`no R definition ${name}`);
  const line = c.code.split("\n").find((l) => l.startsWith(`${name} <- `));
  const expr = c.code.slice(c.code.indexOf(line) + name.length + 4).trim();
  if (/^-?[\d.]+(e-?\d+)?L?$/.test(expr)) return Number(expr.replace(/L$/, ""));
  const m = expr.match(/^(?:c|list)\(([\s\S]*)\)$/);
  if (!m) throw new Error(`${name}: not a number or named numbers: ${expr}`);
  const out = {};
  for (const part of m[1].split(",")) {
    const [k, v] = part.split("=").map((x) => x.trim());
    out[k] = Number(v);
  }
  return out;
}

// Write a constant's value into every <span data-const="NAME"> (or
// data-const="NAME.key" for named numbers).
export function renderConstants(code) {
  for (const el of document.querySelectorAll("[data-const]")) {
    const [name, key] = el.dataset.const.split(".");
    const v = rConstant(code, name);
    el.textContent = key ? v[key] : v;
  }
}

// ---- numerics

// Seeded uniform random numbers (mulberry32) and normal deviates.
export function rng(seed) {
  let a = seed >>> 0;
  const unif = () => {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
  const norm = () => Math.sqrt(-2 * Math.log(1 - unif())) * Math.cos(2 * Math.PI * unif());
  return { unif, norm };
}

export const finite = (x) => x !== null && x !== undefined && Number.isFinite(x);
export const mean = (v) => { const w = v.filter(finite); return w.length ? w.reduce((a, b) => a + b, 0) / w.length : NaN; };
export function median(v) {
  const w = v.filter(finite).sort((a, b) => a - b);
  if (!w.length) return NaN;
  const h = Math.floor(w.length / 2);
  return w.length % 2 ? w[h] : (w[h - 1] + w[h]) / 2;
}
// Sample standard deviation (as R's sd()).
export function sd(v) {
  const w = v.filter(finite);
  if (w.length < 2) return NaN;
  const m = mean(w);
  return Math.sqrt(w.reduce((a, b) => a + (b - m) ** 2, 0) / (w.length - 1));
}
export function cor(x, y) {
  const mx = mean(x), my = mean(y);
  let sxy = 0, sxx = 0, syy = 0;
  for (let i = 0; i < x.length; i++) { sxy += (x[i] - mx) * (y[i] - my); sxx += (x[i] - mx) ** 2; syy += (y[i] - my) ** 2; }
  return sxy / Math.sqrt(sxx * syy);
}
// R's x %% m (result has the sign of m).
export const mod = (x, m) => ((x % m) + m) % m;

// Least squares with an intercept: coefficients of y ~ X (rows of X: one per
// observation), by the normal equations and Gaussian elimination.
export function lstsq(X, y) {
  const p = X[0].length + 1;
  const A = Array.from({ length: p }, () => new Float64Array(p + 1));
  for (let i = 0; i < X.length; i++) {
    const r = [1, ...X[i]];
    for (let j = 0; j < p; j++) {
      for (let k = 0; k < p; k++) A[j][k] += r[j] * r[k];
      A[j][p] += r[j] * y[i];
    }
  }
  for (let j = 0; j < p; j++) A[j][j] += 1e-9;
  for (let c = 0; c < p; c++) {
    let piv = c;
    for (let r = c + 1; r < p; r++) if (Math.abs(A[r][c]) > Math.abs(A[piv][c])) piv = r;
    [A[c], A[piv]] = [A[piv], A[c]];
    for (let r = 0; r < p; r++) {
      if (r === c) continue;
      const f = A[r][c] / A[c][c];
      for (let k = c; k <= p; k++) A[r][k] -= f * A[c][k];
    }
  }
  const beta = A.map((row, j) => row[p] / row[j]);
  return { beta, predict: (x) => beta[0] + x.reduce((a, v, j) => a + beta[j + 1] * v, 0) };
}

// Right-aligned running mean over the last k values that are finite
// (partial windows at the start), as zoo::rollapply(partial = TRUE) with a
// mean of the available values.
export function rollMean(v, k) {
  return v.map((_, i) => mean(v.slice(Math.max(0, i - k + 1), i + 1)));
}

// Number input / slider helper: the element's numeric value.
export const num = (id) => Number(document.getElementById(id).value);

// The quoted strings of an R definition's value (e.g. a character vector
// constant), in order.
export function rStrings(code, name) {
  const c = code[name];
  if (!c) throw new Error(`no R definition ${name}`);
  const body = c.code.slice(c.code.indexOf(`${name} <- `) + name.length + 4);
  return [...body.matchAll(/"([^"]*)"/g)].map((m) => m[1]);
}

// A cyclic colour scale (for day of year).
export const CYCLIC = Array.from({ length: 13 }, (_, i) => [i / 12, `hsl(${(i * 30 + 220) % 360}, 65%, 48%)`]);
