"""Build the analysis website's data files.

Reads (paths relative to the repository root):
  data/site_info.csv                   site metadata
  data/outcome_*.csv                   the published TAS estimates ("published")
  results/<id>/manifest.json           each saved result set (scripts/save-results.R)
  results/<id>/{outcome,siteyear}_{total,direct}.csv
  data-proc/RespirationData/*_ac.parquet   (optional) for mean annual air temperature

Writes analysis-website/data/:
  sites.json                 one record per site
  resultsets.json            the result sets, published first, then by stage
  tas/<id>.json              site-level TAS, per model
  siteyear/<id>.json         site-year table, per model

Run with `pixi run -e website website-build`.
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
    copy_plotly()
    print(f"{len(sites)} sites, {len(sets)} result sets -> {OUT}")


if __name__ == "__main__":
    main()
