"""MODIS at the sites the published AppEEARS extractions lack (sites added
later), from the ORNL DAAC MODIS web service, written in the layout of those
extractions (../original-data/towers-*-061-results.csv) so that
spectral_indices() (R/step03-drivers.R) reads them alongside.

Products (collection 6.1), at the pixel containing the site (kmAboveBelow =
kmLeftRight = 0, as AppEEARS's point samples):
  MOD15A2H    Fpar_500m, Lai_500m, FparLai_QC
  MYD17A2HGF  Gpp_500m, Psn_QC_500m
  MOD13Q1     250 m NDVI, EVI, VI_Quality. The service has no MOD13A2 (1 km,
              the published extraction's); MOD13Q1 is the same algorithm at
              250 m, written under MOD13A2's column names (source_product says
              which). EVI and NDVI are carried, not driver-analysis predictors.

Values are scaled as AppEEARS writes them; each QC's MODLAND bits are decoded
to AppEEARS's description strings (MOD15A2H, MYD17A2HGF: bit 0; MOD13: bits
0-1). Dates are written m/d/yy, as in the published files.

Each site's download is cached (<out-dir>/cache/<product>/<site>.csv), so a
rerun fetches only the sites not yet cached; the output files are rebuilt from
the cache for every site of the tables that the published extractions lack
(by default data/site_info_new.csv).

Usage (from the repository root; needs internet):
  python scripts/download-modis-ornl.py [--site-info TABLE [TABLE ...]]
      [--sites A,B] [--out-dir data-raw/modis] [--workers 4]
  python scripts/download-modis-ornl.py --check US-Kon,DE-Hai,...
      fetch these published sites too and compare with the AppEEARS files
      (writes <out-dir>/check-ornl-vs-appeears.csv)
"""

import argparse
import concurrent.futures
import pathlib
import sys
import time

import numpy as np
import pandas as pd
import requests

API = "https://modis.ornl.gov/rst/api/v1"
START, END = pd.Timestamp("2000-01-01"), pd.Timestamp("2023-12-31")  # the published period
ORIGINAL = pathlib.Path("..", "original-data")
MAX_DATES = 10  # the service's limit per request

PRODUCTS = {
    "MOD15A2H": {
        "file": "towers-new-MOD15A2H-061-results.csv",
        "published": "towers-MOD15A2H-061-results.csv",
        "values": {"Fpar_500m": "MOD15A2H_061_Fpar_500m", "Lai_500m": "MOD15A2H_061_Lai_500m"},
        "qc": ("FparLai_QC", "MOD15A2H_061_FparLai_QC"),
        "modland_bits": 1,
        "modland": {0: "Good quality (main algorithm with or without saturation)",
                    1: "Other Quality (back-up algorithm or fill values)"},
    },
    "MYD17A2HGF": {
        "file": "towers-new-MYD17A2HGF-061-results.csv",
        "published": "towers-MYD17A2HGF-061-results.csv",
        "values": {"Gpp_500m": "MYD17A2HGF_061_Gpp_500m"},
        "qc": ("Psn_QC_500m", "MYD17A2HGF_061_Psn_QC_500m"),
        "modland_bits": 1,
        "modland": {0: "Good quality (main algorithm with or without saturation)",
                    1: "Other quality (back-up algorithm or fill values)"},
    },
    "MOD13Q1": {
        "file": "towers-new-MOD13Q1-061-results.csv",
        "published": "towers-MOD13A2-061-results.csv",
        "values": {"250m_16_days_EVI": "MOD13A2_061__1_km_16_days_EVI",
                   "250m_16_days_NDVI": "MOD13A2_061__1_km_16_days_NDVI"},
        "qc": ("250m_16_days_VI_Quality", "MOD13A2_061__1_km_16_days_VI_Quality"),
        "modland_bits": 2,
        "modland": {0: "VI produced, good quality", 1: "VI produced, but check other QA",
                    2: "Pixel produced, but most probably cloudy",
                    3: "Pixel not produced due to other reasons than clouds"},
    },
}

session = requests.Session()
session.headers["Accept"] = "application/json"


def get(url, params=None, tries=8):
    for i in range(tries):
        try:
            r = session.get(url, params=params, timeout=120)
            if r.status_code == 200:
                return r.json()
            if r.status_code not in (429, 500, 502, 503, 504):
                sys.exit(f"{r.url}: HTTP {r.status_code}: {r.text[:300]}")
        except requests.RequestException as e:
            if i == tries - 1:
                raise
            print(f"retry ({e.__class__.__name__}): {url}", file=sys.stderr, flush=True)
        time.sleep(min(2 ** i, 120))
    sys.exit(f"{url}: no answer after {tries} tries")


def scales(product):
    bands = get(f"{API}/{product}/bands")["bands"]
    return {b["band"]: float(b["scale_factor"]) for b in bands if b.get("scale_factor") not in (None, "", "Not Available")}


def fetch_site(product, site, lat, lon, cache):
    """One site's values and QC of a product over the published period, cached."""
    if cache.exists():
        return pd.read_csv(cache)
    spec = PRODUCTS[product]
    dates = get(f"{API}/{product}/dates", {"latitude": lat, "longitude": lon})["dates"]
    dates = [d for d in dates if START <= pd.Timestamp(d["calendar_date"]) <= END]
    wanted = set(spec["values"]) | {spec["qc"][0]}
    rows = []
    for k in range(0, len(dates), MAX_DATES):
        chunk = dates[k:k + MAX_DATES]
        d = get(f"{API}/{product}/subset", {"latitude": lat, "longitude": lon, "startDate": chunk[0]["modis_date"],
                                           "endDate": chunk[-1]["modis_date"], "kmAboveBelow": 0, "kmLeftRight": 0})
        for s in d["subset"]:
            if s["band"] in wanted:
                rows.append({"date": s["calendar_date"], "tile": s["tile"], "band": s["band"], "value": s["data"][0]})
    raw = pd.DataFrame(rows).pivot_table(index=["date", "tile"], columns="band", values="value", aggfunc="first").reset_index()
    cache.parent.mkdir(parents=True, exist_ok=True)
    raw.to_csv(cache, index=False)
    print(f"{product} {site}: {len(raw)} dates", flush=True)
    return raw


def appeears_layout(product, raw, site, scale):
    """A site's raw download in the layout of the AppEEARS extraction."""
    spec = PRODUCTS[product]
    out = pd.DataFrame({"Category": site.IGBP, "ID": site.site_ID, "Latitude": site.LAT, "Longitude": site.LONG,
                        "Date": [f"{t.month}/{t.day}/{t.strftime('%y')}" for t in pd.to_datetime(raw["date"])],
                        "MODIS_Tile": raw["tile"]})
    for band, col in spec["values"].items():
        out[col] = np.round(raw[band].astype(float) * scale.get(band, 1.0), 6)
    qc_band, qc_col = spec["qc"]
    qc = raw[qc_band].astype("Int64")
    modland = qc % (2 ** spec["modland_bits"])
    out[qc_col] = qc
    out[f"{qc_col}_MODLAND"] = [f"0b{int(m):0{spec['modland_bits']}b}" if pd.notna(m) else "" for m in modland]
    out[f"{qc_col}_MODLAND_Description"] = [spec["modland"].get(int(m), "") if pd.notna(m) else "" for m in modland]
    out["source_product"] = f"{product}.061 (ORNL DAAC MODIS web service)"
    return out


def run(sites, out_dir, workers, cache_name="cache"):
    """Every product for these sites (site_ID, LAT, LONG, IGBP): AppEEARS-layout tables by product."""
    scale = {p: scales(p) for p in PRODUCTS}
    jobs = {}
    with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as pool:
        for p in PRODUCTS:
            for site in sites.itertuples():
                cache = out_dir / cache_name / p / f"{site.site_ID}.csv"
                jobs[(p, site.site_ID)] = pool.submit(fetch_site, p, site.site_ID, site.LAT, site.LONG, cache)
        tables = {p: [] for p in PRODUCTS}
        for site in sites.itertuples():
            for p in PRODUCTS:
                tables[p].append(appeears_layout(p, jobs[(p, site.site_ID)].result(), site, scale[p]))
    return {p: pd.concat(t, ignore_index=True) for p, t in tables.items()}


def check(tables, out_dir):
    """Compare the ORNL downloads of published sites with the AppEEARS extractions."""
    lines = []
    for p, spec in PRODUCTS.items():
        pub = pd.read_csv(ORIGINAL / spec["published"], low_memory=False)
        new = tables[p]
        m = new.merge(pub, on=["ID", "Date"], suffixes=("_ornl", "_appeears"))
        qc_col = f"{spec['qc'][1]}_MODLAND_Description"
        for col in spec["values"].values():
            for site, g in m.groupby("ID"):
                a, b = g[f"{col}_ornl"].astype(float), g[f"{col}_appeears"].astype(float)
                ok = a.notna() & b.notna()
                lines.append({"product": p, "variable": col, "site": site, "n_matched": int(ok.sum()),
                              "identical": float((np.abs(a[ok] - b[ok]) < 1e-6).mean()),
                              "r": float(np.corrcoef(a[ok], b[ok])[0, 1]) if ok.sum() > 2 else np.nan,
                              "bias": float((a[ok] - b[ok]).mean()),
                              "median_rel_diff": float((np.abs(a[ok] - b[ok]) / np.abs(b[ok]).where(b[ok] != 0)).median()),
                              "qc_identical": float((g[f"{qc_col}_ornl"] == g[f"{qc_col}_appeears"]).mean())})
    out = pd.DataFrame(lines)
    out.to_csv(out_dir / "check-ornl-vs-appeears.csv", index=False)
    with pd.option_context("display.width", 200, "display.max_rows", 200):
        print(out.to_string(index=False))
    mod = out[out["product"] != "MOD13Q1"]
    if (mod["identical"] < 1).any() or (mod["qc_identical"] < 1).any():
        print("\nWARNING: ORNL's MOD15A2H/MYD17A2HGF differ from the AppEEARS extractions at some records "
              "(see the table above)", file=sys.stderr)
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--site-info", nargs="+", default=["data/site_info_new.csv"])
    ap.add_argument("--sites")
    ap.add_argument("--out-dir", default="data-raw/modis")
    ap.add_argument("--workers", type=int, default=4)
    ap.add_argument("--check")
    a = ap.parse_args()
    out_dir = pathlib.Path(a.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    info = pd.concat([pd.read_csv(p, encoding="utf-8-sig")[["site_ID", "LAT", "LONG", "IGBP"]] for p in a.site_info],
                     ignore_index=True)
    assert not info.site_ID.duplicated().any()

    if a.check:
        published = pd.read_csv(ORIGINAL / "site_info.csv", encoding="utf-8-sig")[["site_ID", "LAT", "LONG", "IGBP"]]
        sites = published[published.site_ID.isin(a.check.split(","))]
        check(run(sites, out_dir, a.workers, cache_name="cache-check"), out_dir)
        return

    have = set(pd.read_csv(ORIGINAL / PRODUCTS["MOD15A2H"]["published"], usecols=["ID"])["ID"])
    sites = info[~info.site_ID.isin(have)]
    if a.sites:
        sites = sites[sites.site_ID.isin(a.sites.split(","))]
    print(f"{len(sites)} sites without published MODIS", flush=True)
    tables = run(sites, out_dir, a.workers)
    for p, spec in PRODUCTS.items():
        t = tables[p]
        t.to_csv(out_dir / spec["file"], index=False)
        qc = f"{spec['qc'][1]}_MODLAND_Description"
        good = t[qc].isin([spec["modland"][0]] + ([spec["modland"][1]] if p == "MOD13Q1" else []))
        none = sorted(set(t.ID) - set(t.ID[good]))
        print(f"{p}: {len(t)} rows, {t.ID.nunique()} sites -> {out_dir / spec['file']}"
              + (f"; no good-quality value at {', '.join(none)}" if none else ""), flush=True)


if __name__ == "__main__":
    main()
