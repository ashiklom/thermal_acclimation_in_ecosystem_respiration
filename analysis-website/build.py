"""Build the analysis website's data files.

Reads (paths relative to the repository root):
  data/site_info_new.csv               site metadata (all sites; `added`: sites added after the
                                       manuscript's), with data/site_info_overrides.csv and
                                       data/site_info_excluded.csv
  data/outcome_*.csv                   the published TAS estimates ("published")
  results/<id>/manifest.json           each saved result set (scripts/save-results.R)
  results/<id>/{outcome,siteyear}_{total,direct}.csv
  results/<id>/tas_inputs_summary.csv   (optional) each site's fit settings and input hashes
  data-proc[-original]/...             the outputs of the default (latest-data) project, then
                                       the original project's, whichever has the file:
  data-proc/RespirationData/*_ac.parquet   (optional) for mean annual air temperature
  analysis-website/stages/<stage>.html     what each method stage changes and why
  data-proc/flux-products/{summary,siteyear,monthly}.csv  (optional) the manuscript's records
                                       against the shuttle's (scripts/compare-flux-products.R)
  data-proc/au-tum-sources.json        (optional) AU-Tum's candidate flux records compared
                                       (scripts/compare-au-tum-sources.py)
  data-proc/au-tum-options/<option>/   (optional) AU-Tum's TAS under each candidate source
                                       (scripts/au-tum-source-options.R)
  results/<id>/ts_{columns_summary,quality,ranking,methods_selected}.csv  (optional)
                                       the soil-temperature columns of a run and their quality
  results/<id>/ta_{columns_summary,outcome_*}.csv  (optional) the air-temperature columns of a
                                       run (R/ta-columns.R) and their TAS

Writes analysis-website/data/:
  sites.json                 one record per site
  resultsets.json            the result sets, published first, then by stage
  tas/<id>.json              site-level TAS, per model
  siteyear/<id>.json         site-year table, per model
  stages.json                the method stages with a saved run, each with the run it is
                             compared with and the sites whose TAS inputs changed
  ts/<id>.json               soil-temperature columns of a run: per-site summary and quality,
                             estimator ranking, the selected columns, and (fit matrix) every
                             column's TAS
  ts_screen_runs.json, ts_screen/<id>.json   the soil-temperature sensor screen (summary,
                             yearly diagnostics, bad windows, diurnal cycles, the
                             manuscript's own corrections beside it)
  drivers.json, drivers/<id>.json   steps 03-04 tables (driver data, variable importance,
                             partial dependence, future respiration) of the published analysis
                             and of every fit set of every run
  update.json                the data update (S18, S19): each latest-data run (id with "update") with
                             the run on the published data it is compared with (that of its own
                             stage, else the latest S17 run), the sites
                             whose TAS inputs changed between them, the soil drivers filled from
                             older products (source_fills.csv), the ERA5 extension offsets, and
                             the ERA5 nearest-cell sensitivity runs (ids containing
                             "era5-nearest") with the run they are compared with
  flux_products.json         the manuscript's flux records against the FLUXNET-archive shuttle's
                             ONEFlux products (scripts/compare-flux-products.R): per site and
                             comparison (VUT, CUT) summary, and per site-year differences
  flux_products/<site>.json  monthly means of each stream's selected night-time NEE, by stream
  au_tum_sources.json        AU-Tum's candidate flux records compared (a copy)
  au_tum_options.json        AU-Tum's TAS inputs summary, outcome and site-year tables per
                             candidate source, by option
  code.json                  the source of every top-level R definition in R/*.R, by name:
                             {file, line, code}, for the documentation pages' code snippets
                             (built from the code alone, never from results)

Code snippets (build_code()): a top-level definition is a line starting at column 0 with
`name <- `. Its source runs from the roxygen block (`#'` lines) directly above it to the end
of the expression: the line where the brackets ((), [], {}) opened since the `<-` are all
closed again and the line does not end in an operator or comma (brackets inside strings and
comments are ignored). For a function that is the closing brace at column 0; for a one-line
constant, that line. `line` is the line of `name <- ` (1-based); a name defined twice keeps
its first definition (with a warning).

Run with `pixi run website-build`.
"""

import json
import math
import pathlib
import re
import shutil

import pandas as pd
import pyarrow.parquet as pq

SITE = pathlib.Path(__file__).resolve().parent
ROOT = SITE.parent
OUT = SITE / "data"

STAGE_ORDER = ["published", "S5", "S6", "S7", "S8", "S9", "S10", "S11", "S12", "S13",
               "S14", "S15", "S16", "S17", "S18", "S19", "S20", "S22"]


def records(df):
    """DataFrame -> list of dicts with NaN as null."""
    out = df.astype(object).where(pd.notna(df), None).to_dict(orient="records")
    for row in out:
        for k, v in row.items():
            if isinstance(v, float) and (math.isnan(v) or math.isinf(v)):
                row[k] = None
    return out


def write_json(obj, path):
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w") as f:
        json.dump(obj, f, separators=(",", ":"))


def proc_path(*parts):
    """A workflow output: the default (latest-data) project's data-proc/ if it has it, else the
    original project's data-proc-original/ (the published records' analyses live there)."""
    for d in ("data-proc", "data-proc-original"):
        path = ROOT.joinpath(d, *parts)
        if path.exists():
            return path
    return ROOT.joinpath("data-proc", *parts)


def mean_annual_temperature(site_id):
    path = proc_path("RespirationData", f"{site_id}_ac.parquet")
    if not path.exists():
        return None
    ac = pq.read_table(path, columns=["YEAR", "TA"]).to_pandas()
    yearly = ac.groupby("YEAR")["TA"].mean()
    return float(yearly.mean()) if len(yearly) else None


def build_sites():
    info = pd.read_csv(ROOT / "data" / "site_info_new.csv", encoding="utf-8-sig")
    # the sites without soil temperature: air-temperature columns only (R/ta-columns.R)
    info["ta_only"] = ~info["soil_temperature"]
    info["added"] = info["added"].fillna("")
    keep = ["site_ID", "LAT", "LONG", "ELEV", "IGBP", "Climate_class", "MAP", "source",
            "estimate_Ts", "SWC_use", "added", "ta_only"]
    sites = info[keep].copy()
    overrides = ROOT / "data" / "site_info_overrides.csv"
    if overrides.exists():
        o = pd.read_csv(overrides)
        sites["overrides"] = [", ".join(f"{r.field} {r.declared} → {r.used} ({r.reason})" for r in o[o.site_ID == s].itertuples())
                              for s in sites["site_ID"]]
    excluded = ROOT / "data" / "site_info_excluded.csv"
    write_json(records(pd.read_csv(excluded)) if excluded.exists() else [], OUT / "sites_excluded.json")
    settings_path = ROOT / "data" / "site_settings.csv"
    if settings_path.exists():
        sites = sites.merge(pd.read_csv(settings_path), on="site_ID", how="left")
    sites["MAT"] = [mean_annual_temperature(s) for s in sites["site_ID"]]
    write_json(records(sites), OUT / "sites.json")
    return sites


def read_set(directory, total, direct, siteyear_total, siteyear_direct):
    def read(path):
        return pd.read_csv(path) if path.exists() else None
    return {
        "total": read(directory / total),
        "direct": read(directory / direct),
        "siteyear_total": read(directory / siteyear_total),
        "siteyear_direct": read(directory / siteyear_direct),
    }


def build_resultsets():
    sets = [{
        "id": "published",
        "label": "Published",
        "stage": "published",
        "description": "The manuscript's TAS estimates (data/outcome_*.csv), from the original code.",
        "git_sha": "c3f7d57",
        "fit_profile": "full",
        "data": "published step-01 outputs",
        "tables": read_set(ROOT / "data", "outcome_temp.csv", "outcome_temp_water_gpp.csv",
                           "outcome_siteyear_temp.csv", "outcome_siteyear_temp_water_gpp.csv"),
    }]
    for manifest_path in sorted((ROOT / "results").glob("*/manifest.json")):
        manifest = json.loads(manifest_path.read_text())
        d = manifest_path.parent
        manifest["tables"] = read_set(d, "outcome_total.csv", "outcome_direct.csv",
                                      "siteyear_total.csv", "siteyear_direct.csv")
        sets.append(manifest)

    def order(s):
        stage = s.get("stage") or ""
        return (STAGE_ORDER.index(stage) if stage in STAGE_ORDER else len(STAGE_ORDER), s["id"])
    sets.sort(key=order)

    index = []
    for s in sets:
        tables = s.pop("tables")
        tas = {m: records(tables[m]) for m in ("total", "direct") if tables[m] is not None}
        siteyear = {m: records(tables["siteyear_" + m]) for m in ("total", "direct")
                    if tables["siteyear_" + m] is not None}
        write_json(tas, OUT / "tas" / f"{s['id']}.json")
        write_json(siteyear, OUT / "siteyear" / f"{s['id']}.json")
        s["n_sites"] = max((len(v) for v in tas.values()), default=0)
        index.append(s)
    write_json(index, OUT / "resultsets.json")
    return index


# Method stages (one section each on stages.html), in order.
STAGES = [
    ("S11", "tStart/tEnd from the climatology rule at every site"),
    ("S12", "gStart bounded by a day of year, not a row number"),
    ("S13", "Daily daytime NEE keyed by calendar date"),
    ("S14", "14-day non-overlapping windows"),
]
# Fields of tas_inputs_summary.csv compared between runs.
INPUT_FIELDS = ["gStart", "gEnd", "tStart", "tEnd", "control_year", "n_night", "n_years",
                "nwindow", "NEE_daytime_mean", "night_hash", "ac_hash", "ac_day_hash"]


def input_changes(ref_dir, new_dir):
    """Sites whose TAS inputs differ between two runs: one row per site, with the changed
    fields and the reference and new values of the numeric ones."""
    paths = [ref_dir / "tas_inputs_summary.csv", new_dir / "tas_inputs_summary.csv"]
    if not all(p.exists() for p in paths):
        return None
    ref, new = (pd.read_csv(p).set_index("site_ID") for p in paths)
    rows = []
    for site in sorted(set(ref.index) | set(new.index)):
        if site not in ref.index or site not in new.index:
            rows.append({"site_ID": site, "changed": "site added" if site in new.index else "site dropped"})
            continue
        changed = []
        for f in INPUT_FIELDS:
            a, b = ref.at[site, f], new.at[site, f]
            same = (pd.isna(a) and pd.isna(b)) or (
                abs(a - b) <= 1e-9 * max(1.0, abs(a)) if isinstance(a, float) and isinstance(b, float) else a == b)
            if not same:
                changed.append(f)
        if changed:
            row = {"site_ID": site, "changed": ", ".join(f.replace("_hash", " data") for f in changed)}
            for f in INPUT_FIELDS:
                if not f.endswith("_hash"):
                    row[f + "_ref"], row[f + "_new"] = ref.at[site, f], new.at[site, f]
            rows.append(row)
    return records(pd.DataFrame(rows))


def build_stages(sets):
    """For each method stage with a saved run: that run, the run before it (the latest saved
    run of an earlier stage) and the sites whose inputs changed."""
    order = {s["id"]: i for i, s in enumerate(sets)}
    stages = []
    for stage, title in STAGES:
        runs = [s for s in sets if s.get("stage") == stage]
        if not runs:
            continue
        run = runs[-1]
        earlier = [s for s in sets[:order[run["id"]]] if s.get("stage") not in (stage, "published")]
        ref = earlier[-1] if earlier else sets[0]
        stages.append({
            "stage": stage, "title": title, "run": run["id"], "reference": ref["id"],
            "inputs": input_changes(ROOT / "results" / ref["id"], ROOT / "results" / run["id"]),
        })
    write_json(stages, OUT / "stages.json")
    return stages


def build_ts(sets):
    """Soil-temperature column tables of the runs that have them, and the air-temperature
    columns' (ta_*: their summary and TAS) where the run has those."""
    out = []
    for s in sets:
        d = ROOT / "results" / s["id"]
        paths = {k: d / f"ts_{k}.csv" for k in ("columns_summary", "quality", "ranking", "methods_selected",
                                                 "outcome_total", "outcome_direct")}
        paths.update({f"ta_{k}": d / f"ta_{k}.csv" for k in ("columns_summary", "outcome_total", "outcome_direct")})
        if not paths["columns_summary"].exists():
            continue
        data = {k: records(pd.read_csv(p)) for k, p in paths.items() if p.exists()}
        write_json(data, OUT / "ts" / f"{s['id']}.json")
        out.append(s["id"])
    write_json(out, OUT / "ts_runs.json")
    return out


STEP0304_FILES = {"drivers": "acclimation_data.csv", "varimp": "varImp_plot.csv", "partial": "partial_plot.csv",
                  "future": "acclimation_data_future_ssp245.csv"}


def build_drivers(sets):
    """Steps 03-04 outputs: the published tables, and every fit set of every run that has
    them (results/<id>/step03-04/<fit set>/)."""
    entries = [{"id": "published", "label": "Published (original/data)", "dir": ROOT / "data"}]
    for s in sets:
        for sub, what in (("step03-04", "TAS"), ("step03-04-ts", "soil temperature"), ("step03-04-ta", "air temperature")):
            d = ROOT / "results" / s["id"] / sub
            if d.exists():
                for fs in sorted(p.name for p in d.iterdir() if p.is_dir()):
                    entry = {"id": f"{s['id']}--{fs}", "label": f"{s['label']}: {what} {fs}", "dir": d / fs}
                    # the same analysis over the manuscript's sites only (the run's
                    # original_sites_run, scripts/save-results.R), for the added-sites toggle
                    orig = s.get("original_sites_run")
                    if orig and (ROOT / "results" / orig / sub / fs).exists():
                        entry["original_sites_id"] = f"{orig}--{fs}"
                    entries.append(entry)
    index = []
    for e in entries:
        tables = {k: records(pd.read_csv(e["dir"] / f)) for k, f in STEP0304_FILES.items() if (e["dir"] / f).exists()}
        if not tables:
            continue
        # forests fitted to the manuscript's sites, scored on the added ones (scripts/new-sites-effect.R)
        effect = e["dir"].parent.parent / "new_sites_effect.csv"
        if effect.exists():
            ne = pd.read_csv(effect)
            ne = ne[ne["set"] == f"{e['dir'].parent.name}/{e['dir'].name}"]
            if len(ne):
                tables["new_sites_effect"] = records(ne)
        write_json(tables, OUT / "drivers" / f"{e['id']}.json")
        index.append({k: e[k] for k in ("id", "label", "original_sites_id") if k in e})
    write_json(index, OUT / "drivers.json")
    return index


SCREEN_FILES = {"summary": "ts_screen.csv", "yearly": "ts_screen_yearly.csv", "windows": "ts_screen_windows.csv",
                "diurnal": "ts_screen_diurnal.csv", "validation": "ts_screen_validation.csv"}


def build_screen(sets):
    """The soil-temperature sensor screen of every run that has it."""
    out = []
    for s in sets:
        d = ROOT / "results" / s["id"]
        if not (d / SCREEN_FILES["summary"]).exists():
            continue
        data = {k: records(pd.read_csv(d / f)) for k, f in SCREEN_FILES.items() if (d / f).exists()}
        write_json(data, OUT / "ts_screen" / f"{s['id']}.json")
        out.append(s["id"])
    write_json(out, OUT / "ts_screen_runs.json")
    return out


def build_update(sets):
    """The data update: latest-data runs against the latest run on the published data."""
    def latest_data(s):
        return "update" in s["id"] and "era5-nearest" not in s["id"]
    runs = [s for s in sets if latest_data(s)]
    nearest = [s for s in sets if "era5-nearest" in s["id"]]
    published = [s for s in sets if s.get("stage") in ("S17", "S19") and not latest_data(s) and "era5" not in s["id"]]
    if not runs or not published:
        write_json({}, OUT / "update.json")
        return None

    def reference(run):
        same = [s for s in published if s.get("stage") == run.get("stage")]
        return (same or [s for s in published if s.get("stage") == "S17"] or published)[-1]["id"]

    def optional(run_id, name):
        path = ROOT / "results" / run_id / name
        return records(pd.read_csv(path)) if path.exists() else None

    out = {"reference": reference(runs[-1]), "runs": [{
        "run": r["id"], "reference": reference(r),
        "inputs": input_changes(ROOT / "results" / reference(r), ROOT / "results" / r["id"]),
        "fills": optional(r["id"], "source_fills.csv"), "era5_offsets": optional(r["id"], "era5_extension_offsets.csv"),
    } for r in runs], "era5_nearest": [{"run": n["id"], "reference": [r for r in runs if r.get("stage") == n.get("stage")][-1]["id"]}
                                       for n in nearest if any(r.get("stage") == n.get("stage") for r in runs)]}
    write_json(out, OUT / "update.json")
    return out


def build_flux_products():
    """The manuscript's flux records against the shuttle's ONEFlux products."""
    d = proc_path("flux-products")
    if not (d / "summary.csv").exists():
        write_json({}, OUT / "flux_products.json")
        return None
    out = {"summary": records(pd.read_csv(d / "summary.csv")),
           "siteyear": records(pd.read_csv(d / "siteyear.csv")) if (d / "siteyear.csv").exists() else []}
    write_json(out, OUT / "flux_products.json")
    if (d / "monthly.csv").exists():
        monthly = pd.read_csv(d / "monthly.csv")
        for site, g in monthly.groupby("site_ID"):
            write_json({s: {"month": h["month"].tolist(), "nee": [round(v, 3) for v in h["nee"]], "n": h["n"].tolist()}
                        for s, h in g.groupby("stream")}, OUT / "flux_products" / f"{site}.json")
    return out


def build_au_tum():
    """AU-Tum's candidate flux records and its TAS under each candidate source."""
    src = proc_path("au-tum-sources.json")
    if src.exists():
        shutil.copy(src, OUT / "au_tum_sources.json")
    options = {}
    d = proc_path("au-tum-options")
    for summary in sorted(d.glob("*/tas_inputs_summary.csv")) if d.exists() else []:
        entry = {"inputs": records(pd.read_csv(summary))[0]}
        for kind in ("outcome", "siteyear"):
            for model in ("total", "direct"):
                f = summary.parent / f"{kind}_{model}.csv"
                if f.exists():
                    entry[f"{kind}_{model}"] = records(pd.read_csv(f))
        options[summary.parent.name] = entry
    write_json(options, OUT / "au_tum_options.json")
    return options


DEFINITION = re.compile(r"^([A-Za-z.][A-Za-z0-9._]*) <- ")
CONTINUES = re.compile(r"(\|>|%[^%]*%|<-|[-+*/^&|=,~(\[{])\s*$")
CLOSERS = {")": "(", "]": "[", "}": "{"}


def code_only(line):
    """A line of R without its comment and string contents (brackets in them don't count)."""
    out, quote, i = [], None, 0
    while i < len(line):
        c = line[i]
        if quote:
            if c == "\\":
                i += 1
            elif c == quote:
                quote = None
                out.append(c)
        elif c in "\"'`":
            quote = c
            out.append(c)
        elif c == "#":
            break
        else:
            out.append(c)
        i += 1
    return "".join(out), quote


def definition_end(lines, start):
    """Index of the last line of the definition starting at lines[start]."""
    depth, quote = 0, None
    for i in range(start, len(lines)):
        if quote:  # a string spanning lines: skip to its end
            j = lines[i].find(quote)
            if j < 0:
                continue
            text, quote = code_only(lines[i][j + 1:])
        else:
            text, quote = code_only(lines[i] if i > start else lines[i].split("<-", 1)[1])
        for c in text:
            if c in "([{":
                depth += 1
            elif c in CLOSERS:
                depth -= 1
        if depth <= 0 and not quote and not CONTINUES.search(text):
            return i
    return len(lines) - 1


def build_code():
    """The source of every top-level R definition in R/*.R (see the module docstring)."""
    code = {}
    for path in sorted((ROOT / "R").glob("*.R")):
        lines = path.read_text().splitlines()
        i = 0
        while i < len(lines):
            m = DEFINITION.match(lines[i])
            if not m:
                i += 1
                continue
            first = i
            while first > 0 and lines[first - 1].startswith("#'"):
                first -= 1
            last = definition_end(lines, i)
            name = m.group(1)
            if name in code:
                print(f"warning: {name} defined again in {path.name}:{i + 1}; keeping {code[name]['file']}:{code[name]['line']}")
            else:
                code[name] = {"file": f"R/{path.name}", "line": i + 1, "code": "\n".join(lines[first:last + 1])}
            i = last + 1
    write_json(code, OUT / "code.json")
    return code


def copy_plotly():
    try:
        import plotly
    except ImportError:
        print("plotly (python) not installed: vendor/plotly.min.js not refreshed")
        return
    src = pathlib.Path(plotly.__file__).parent / "package_data" / "plotly.min.js"
    if src.exists():
        shutil.copy(src, SITE / "vendor" / "plotly.min.js")


def main():
    OUT.mkdir(exist_ok=True)
    sites = build_sites()
    sets = build_resultsets()
    stages = build_stages(sets)
    build_ts(sets)
    build_drivers(sets)
    build_screen(sets)
    build_update(sets)
    build_flux_products()
    build_au_tum()
    code = build_code()
    copy_plotly()
    print(f"{len(sites)} sites, {len(sets)} result sets, {len(stages)} method stages, "
          f"{len(code)} R definitions -> {OUT}")


if __name__ == "__main__":
    main()
