"""Daily ERA5-Land top-layer soil water (%) at every site, for extending
ERA5_daily_swc_1990_2024_allsites.csv past 2024 (refactor-v2 stage S18).

Reads volumetric soil water of layer 1 (swvl1, 0-7 cm, m3/m3) from ECMWF's
analysis-ready (ARCO) ERA5-Land store, at the grid cell nearest each site
(data/site_info.csv LAT/LONG), averages it per UTC day and converts it to %
(x 100), the units of the published file. Needs internet access and the
Climate Data Store key `cds_api_key` in _creds.toml (stops at once
otherwise).

Usage (from the repository root):
  python scripts/download-era5-swc.py --start 2024-01-01 --out data-raw/era5/ERA5_daily_swc_2024_latest.csv
  [--end YYYY-MM-DD]   last day (default: the store's last complete day)
  [--sites A,B]        only these sites

Writes a CSV with columns date, name, SWC (the published file's), sites in
site_info order, then by date.
"""

import argparse
import pathlib
import sys
import tomllib

import pandas as pd
import xarray as xr

STORE = "https://arco.datastores.ecmwf.int/cadl-arco-geo-005/arco/reanalysis_era5_land/sfc-soil-water/geoChunked.zarr"


def cds_key(path="_creds.toml"):
    p = pathlib.Path(path)
    if not p.exists():
        sys.exit(f"ERA5 download needs {p.resolve()} with cds_api_key; it is missing.")
    creds = tomllib.loads(p.read_text())
    if "cds_api_key" not in creds:
        sys.exit(f"{p} lacks cds_api_key")
    return creds["cds_api_key"]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--start", required=True)
    ap.add_argument("--end")
    ap.add_argument("--sites")
    ap.add_argument("--out", required=True)
    a = ap.parse_args()

    sites = pd.read_csv("data/site_info.csv", encoding="utf-8-sig")[["site_ID", "LAT", "LONG"]]
    if a.sites:
        keep = a.sites.split(",")
        missing = set(keep) - set(sites.site_ID)
        if missing:
            sys.exit(f"unknown sites: {sorted(missing)}")
        sites = sites[sites.site_ID.isin(keep)]

    ds = xr.open_zarr(STORE, consolidated=True,
                      storage_options={"headers": {"Authorization": f"Bearer {cds_key()}"}})
    swc = ds["swvl1"]
    last_day = pd.Timestamp(swc.time.values.max()).normalize() - pd.Timedelta(days=1)
    end = min(pd.Timestamp(a.end), last_day) if a.end else last_day
    lon = sites.LONG.to_numpy()
    if float(swc.longitude.max()) > 180:  # store on 0..360
        lon = lon % 360
    cells = swc.sel(latitude=xr.DataArray(sites.LAT.to_numpy(), dims="site"),
                    longitude=xr.DataArray(lon, dims="site"), method="nearest")
    daily = cells.sel(time=slice(pd.Timestamp(a.start), end + pd.Timedelta(hours=23))).resample(time="1D").mean()
    values = daily.compute()

    out = values.to_dataframe(name="SWC").reset_index()
    out["name"] = sites.site_ID.to_numpy()[out["site"].to_numpy()]
    out["date"] = out["time"].dt.strftime("%Y-%m-%d")
    out["SWC"] = out["SWC"] * 100
    empty = out.groupby("name")["SWC"].apply(lambda v: v.isna().all())
    if empty.any():
        print(f"WARNING: no land value (sea cell?) at {', '.join(empty[empty].index)}", file=sys.stderr)
    order = {s: i for i, s in enumerate(sites.site_ID)}
    out = out.sort_values(["name", "date"], key=lambda c: c.map(order) if c.name == "name" else c)
    pathlib.Path(a.out).parent.mkdir(parents=True, exist_ok=True)
    out[["date", "name", "SWC"]].to_csv(a.out, index=False)
    print(f"{len(sites)} sites, {a.start} to {end.date()}: {a.out}")


if __name__ == "__main__":
    main()
