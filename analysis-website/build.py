"""Build the analysis website's data files.

Reads (paths relative to the repository root):
  data/site_info.csv                   site metadata
  data/outcome_*.csv                   the published TAS estimates ("published")
  results/<id>/manifest.json           each saved result set (scripts/save-results.R)
  results/<id>/{outcome,siteyear}_{total,direct}.csv
  results/<id>/tas_inputs_summary.csv   (optional) each site's fit settings and input hashes
  data-proc/RespirationData/*_ac.parquet   (optional) for mean annual air temperature
  analysis-website/stages/<stage>.html     what each method stage changes and why
  results/<id>/ts_{columns_summary,quality,ranking,methods_selected}.csv  (optional)
                                       the soil-temperature columns of a run and their quality

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

Run with `pixi run website-build`.
"""

import json
import math
import pathlib
import shutil

import pandas as pd
import pyarrow.parquet as pq

SITE = pathlib.Path(__file__).resolve().parent
ROOT = SITE.parent
OUT = SITE / "data"

STAGE_ORDER = ["published", "S5", "S6", "S7", "S8", "S9", "S10", "S11", "S12", "S13",
               "S14", "S15", "S16", "S17", "S18"]


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


def mean_annual_temperature(site_id):
    path = ROOT / "data-proc" / "RespirationData" / f"{site_id}_ac.parquet"
    if not path.exists():
        return None
    ac = pq.read_table(path, columns=["YEAR", "TA"]).to_pandas()
    yearly = ac.groupby("YEAR")["TA"].mean()
    return float(yearly.mean()) if len(yearly) else None


def build_sites():
    info = pd.read_csv(ROOT / "data" / "site_info.csv", encoding="utf-8-sig")
    keep = ["site_ID", "LAT", "LONG", "ELEV", "IGBP", "Climate_class", "MAP", "source",
            "estimate_Ts", "SWC_use"]
    sites = info[keep].copy()
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
    """Soil-temperature column tables of the runs that have them."""
    out = []
    for s in sets:
        d = ROOT / "results" / s["id"]
        paths = {k: d / f"ts_{k}.csv" for k in ("columns_summary", "quality", "ranking", "methods_selected",
                                                 "outcome_total", "outcome_direct")}
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
        for sub, what in (("step03-04", "TAS"), ("step03-04-ts", "soil temperature")):
            d = ROOT / "results" / s["id"] / sub
            if d.exists():
                for fs in sorted(p.name for p in d.iterdir() if p.is_dir()):
                    entries.append({"id": f"{s['id']}--{fs}", "label": f"{s['label']}: {what} {fs}", "dir": d / fs})
    index = []
    for e in entries:
        tables = {k: records(pd.read_csv(e["dir"] / f)) for k, f in STEP0304_FILES.items() if (e["dir"] / f).exists()}
        if not tables:
            continue
        write_json(tables, OUT / "drivers" / f"{e['id']}.json")
        index.append({"id": e["id"], "label": e["label"]})
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
    copy_plotly()
    print(f"{len(sites)} sites, {len(sets)} result sets, {len(stages)} method stages -> {OUT}")


if __name__ == "__main__":
    main()
