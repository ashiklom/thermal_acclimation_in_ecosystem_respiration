"""Daily ERA5-Land top-layer soil water (%) at every site, for extending
ERA5_daily_swc_1990_2024_allsites.csv past 2024 (refactor-v2 stage S18).

Reads volumetric soil water of layer 1 (swvl1, 0-7 cm, m3/m3) from ECMWF's
analysis-ready (ARCO) ERA5-Land store, at the grid cell nearest each site
(data/site_info.csv LAT/LONG), averages it per UTC day and converts it to %
(x 100), the units of the published file. Where that cell is sea (no soil
water at any of a sample of times, e.g. IT-Noe on the coast), the nearest
land cell (one with soil water) within MAX_RADIUS cells is used instead,
with a warning naming it and its distance. Needs internet access and the
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
import math
import pathlib
import sys
import tomllib

import pandas as pd
import xarray as xr

STORE = "https://arco.datastores.ecmwf.int/cadl-arco-geo-005/arco/reanalysis_era5_land/sfc-soil-water/geoChunked.zarr"
# how far (in grid cells) to look for land around a sea cell
MAX_RADIUS = 5
# times sampled to tell land (soil water) from sea (none)
N_SAMPLE_TIMES = 24


def haversine_km(lat1, lon1, lat2, lon2):
    p1, p2 = math.radians(lat1), math.radians(lat2)
    dp, dl = p2 - p1, math.radians(((lon2 - lon1 + 180) % 360) - 180)
    a = math.sin(dp / 2) ** 2 + math.cos(p1) * math.cos(p2) * math.sin(dl / 2) ** 2
    return 2 * 6371.0 * math.asin(math.sqrt(a))


def land_cells(swc, sites, lon):
    """Per site, the (latitude, longitude) of the cell to read: the nearest
    cell, or, where that is sea, the nearest land cell around it."""
    sample = list(range(0, swc.sizes["time"], max(1, swc.sizes["time"] // N_SAMPLE_TIMES)))
    lats = swc.latitude.values
    lons = swc.longitude.values
    out = []
    for (site, lat), x in zip(sites[["site_ID", "LAT"]].itertuples(index=False), lon):
        i = int(abs(lats - lat).argmin())
        j = int(abs(lons - x).argmin())
        block = swc.isel(time=xr.DataArray(sample, dims="t"),
                         latitude=slice(max(i - MAX_RADIUS, 0), i + MAX_RADIUS + 1),
                         longitude=[(j + d) % len(lons) for d in range(-MAX_RADIUS, MAX_RADIUS + 1)]).compute()
        land = block.notnull().any("t")
        here = land.sel(latitude=lats[i], longitude=lons[j]).item()
        if here:
            out.append((lats[i], lons[j]))
            continue
        best = None
        for la in land.latitude.values:
            for lo in land.longitude.values:
                if land.sel(latitude=la, longitude=lo).item():
                    d = haversine_km(lat, x, float(la), float(lo))
                    if best is None or d < best[0]:
                        best = (d, float(la), float(lo))
        if best is None:
            sys.exit(f"{site}: no land cell within {MAX_RADIUS} cells of ({lat}, {x})")
        print(f"WARNING: {site}: nearest ERA5-Land cell ({lats[i]:.2f}, {lons[j]:.2f}) is sea; using the nearest land "
              f"cell ({best[1]:.2f}, {best[2]:.2f}), {best[0]:.1f} km from the site", file=sys.stderr)
        out.append((best[1], best[2]))
    return out


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
    chosen = land_cells(swc, sites, lon)
    # one site at a time: the store is chunked by location, so each site's
    # read covers long time series; all at once does not fit in memory
    frames = []
    for (site, (la, lo)) in zip(sites.site_ID, chosen):
        cell = swc.sel(latitude=la, longitude=lo).sel(time=slice(pd.Timestamp(a.start), end + pd.Timedelta(hours=23)))
        daily = cell.resample(time="1D").mean().compute()
        frames.append(pd.DataFrame({"date": pd.to_datetime(daily.time.values).strftime("%Y-%m-%d"),
                                    "name": site, "SWC": daily.values * 100}))
        print(f"{site}: {len(daily.time)} days", flush=True)
    out = pd.concat(frames, ignore_index=True)
    empty = out.groupby("name")["SWC"].apply(lambda v: v.isna().all())
    if empty.any():
        print(f"WARNING: no land value (sea cell?) at {', '.join(empty[empty].index)}", file=sys.stderr)
    pathlib.Path(a.out).parent.mkdir(parents=True, exist_ok=True)
    out[["date", "name", "SWC"]].to_csv(a.out, index=False)
    print(f"{len(sites)} sites, {a.start} to {end.date()}: {a.out}")


if __name__ == "__main__":
    main()
