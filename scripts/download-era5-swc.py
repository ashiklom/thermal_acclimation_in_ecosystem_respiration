#!/usr/bin/env -S uv run --script
#
# /// script
# requires-python = ">=3.13"
# dependencies = [
#   "aiohttp",
#   "dask[array,distributed]",
#   "fsspec",
#   "pandas",
#   "requests",
#   "xarray",
#   "zarr"
# ]
# ///

# uv run --with-requirements scripts/download-era5-swc.py --with ipython -- ipython

# Usage:
#   uv run scripts/download-era5-swc.py                     extend to the store's end
#   uv run scripts/download-era5-swc.py --through 2026-08-31
#   uv run scripts/download-era5-swc.py --full              re-extract everything
#
# Incremental by default. Sites already in data-raw/ERA5_daily_swc.csv are
# extended from the day after their last row; sites in site_info.csv but not in
# the file are extracted over the whole range. When there is nothing new to
# add, the file is left untouched -- not rewritten with the same contents --
# because the pipeline tracks it by hash and a rewrite would be free, but
# anything keyed on mtime would not be.

import argparse
import os
from pathlib import Path

import pandas as pd
import tomllib
import xarray as xr
from dask.distributed import Client, LocalCluster

# Threads, not processes. The work is thousands of small HTTPS range requests
# against the zarr store, so it is network-bound rather than CPU-bound, and
# threads share one authenticated session instead of shipping every chunk
# across a process boundary. Process-based workers also cannot start here at
# all: dask launches its nannies with `spawn`, which re-imports this module,
# and this script runs at module level rather than under a `__main__` guard.
START = pd.Timestamp("1990-01-01")
OUT = Path("data-raw/ERA5_daily_swc.csv")

parser = argparse.ArgumentParser(description="Extract daily ERA5-Land layer-1 soil water at every site.")
parser.add_argument("--through", help="Last date to extract (YYYY-MM-DD). Default: the store's last day.")
parser.add_argument("--full", action="store_true", help="Ignore the existing file and re-extract everything.")
args = parser.parse_args()

n_workers = int(os.environ.get("SLURM_CPUS_PER_TASK", os.cpu_count() or 4))
cluster = LocalCluster(n_workers=n_workers, processes=False)
dclient = Client(cluster)

with open("_creds.toml", "r") as f:
    creds = tomllib.loads(f.read())

CDS_API_KEY = creds["cds_api_key"]

site_info = pd.read_csv("data-core/site_info.csv")

# Sites whose tower coordinate falls in an ERA5-Land cell that is masked as sea,
# mapped to the coordinate of the nearest cell that is land. ERA5-Land is
# defined only over land, so taking the nearest cell for these sites returns
# NaN for the entire record rather than an approximation of it.
#
# Regenerate an entry with `scripts/find-coastal-land-pixel.py <site_ID>`; the
# comment is the distance from the tower to the substituted cell.
COASTAL_SITES = {
    "IT-Noe": (40.70, 8.20),  # nearest land cell, 11.2 km from the tower
}

unknown_coastal = set(COASTAL_SITES) - set(site_info["site_ID"])
if unknown_coastal:
    raise ValueError(
        "COASTAL_SITES names sites that are not in data-core/site_info.csv: "
        f"{', '.join(sorted(unknown_coastal))}"
    )


chunks = "geo"
soil_water_url = f"https://arco.datastores.ecmwf.int/cadl-arco-{chunks}-005/arco/reanalysis_era5_land/sfc-soil-water/{chunks}Chunked.zarr"

dat = xr.open_zarr(
    soil_water_url,
    consolidated=True,
    storage_options = {
        "headers": {"Authorization": f"Bearer {CDS_API_KEY}"}
        }
)


# The last *complete* day in the store. ERA5-Land runs a few days behind real
# time and the store's final day can be partial, so it is dropped: a daily mean
# over half a day of hours would be written once and then never corrected by a
# later incremental run.
store_end = pd.Timestamp(dat["time"].values.max()).normalize() - pd.Timedelta(days=1)
end = min(pd.Timestamp(args.through), store_end) if args.through else store_end
if args.through and pd.Timestamp(args.through) > store_end:
    print(f"ERA5-Land store ends {store_end.date()}; clipping --through {args.through} to that.")


def extract_daily(site_ids, lats, lons, start):
    """Daily mean layer-1 soil water at the nearest cell to each coordinate.

    Returns a wide frame indexed by date with one column per site ID.

    NOTE: `swvl1` is ERA5-Land volumetric soil water for layer 1 (0-7 cm), in
    m3/m3. It is returned here in that native unit; the analysis convention is
    percent (0-100), and `read_era5_swc()` in R/total_tas.R does the conversion.
    """
    if len(site_ids) == 0 or start > end:
        return pd.DataFrame(index=pd.DatetimeIndex([], name="time"))
    extracted_ds = (dat
                    .sel(latitude=xr.DataArray(lats, dims="site"),
                         longitude=xr.DataArray(lons, dims="site"),
                         method="nearest")
                    # `end` is a date; the slice has to reach its last hour.
                    .sel(time=slice(start, end + pd.Timedelta(hours=23)))
                    .assign_coords(site=xr.DataArray(site_ids, dims="site")))
    daily = extracted_ds["swvl1"].resample(time="1D").mean()
    return daily.to_pandas()


def extract_group(sites, start):
    """`extract_daily()` over `sites`, split into regular and coastal ones."""
    sites = site_info.loc[site_info["site_ID"].isin(sites)]
    coastal_mask = sites["site_ID"].isin(COASTAL_SITES)
    # Regular sites: the tower coordinate is inside the land mask, so the
    # nearest cell is the right cell.
    regular = sites.loc[~coastal_mask]
    daily_regular = extract_daily(
        regular["site_ID"].to_numpy(),
        regular["LAT"].to_numpy(),
        regular["LONG"].to_numpy(),
        start,
    )
    # Coastal sites: identical extraction, but off the substituted land
    # coordinate instead of the tower coordinate.
    coastal = sites.loc[coastal_mask, "site_ID"].to_numpy()
    daily_coastal = extract_daily(
        coastal,
        [COASTAL_SITES[s][0] for s in coastal],
        [COASTAL_SITES[s][1] for s in coastal],
        start,
    )
    # A substituted coordinate that is still in the water would hand the
    # analysis layer a silently empty column, so refuse to write one.
    still_sea = [s for s in coastal if daily_coastal[s].isna().all()]
    if still_sea:
        raise ValueError(
            "COASTAL_SITES coordinates are still outside the ERA5-Land land mask "
            f"for: {', '.join(still_sea)}. Re-run "
            "`scripts/find-coastal-land-pixel.py` for these sites."
        )
    wide = pd.concat([daily_regular, daily_coastal], axis=1)
    wide.index.name = "time"
    wide.columns.name = "site"
    long = wide.stack()
    long.name = "SWC"
    return long.reset_index()


existing = None
if OUT.exists() and not args.full:
    existing = pd.read_csv(OUT, parse_dates=["time"])

all_sites = site_info["site_ID"].tolist()
pieces = []
if existing is None:
    new_sites, old_sites = all_sites, []
else:
    # A site whose rows are all NaN counts as missing. That is what a sea cell
    # extracted before its COASTAL_SITES entry existed looks like, and
    # extending it would only append good days to a record that stays empty.
    have = set(existing.loc[existing["SWC"].notna(), "site"])
    new_sites = [s for s in all_sites if s not in have]
    old_sites = [s for s in all_sites if s in have]
    # One extension start for all existing sites. The file is always written
    # whole, so its sites share an end date; a per-site start would only matter
    # after a hand edit, and the drop_duplicates below covers that.
    last = existing.loc[existing["site"].isin(old_sites), "time"].max()
    old_start = last + pd.Timedelta(days=1)
    if old_sites and old_start <= end:
        print(f"Extending {len(old_sites)} site(s) from {old_start.date()} to {end.date()}.")
        pieces.append(extract_group(old_sites, old_start))

if new_sites:
    print(f"Extracting {len(new_sites)} new site(s) from {START.date()} to {end.date()}.")
    pieces.append(extract_group(new_sites, START))

if not pieces:
    print(f"{OUT} already covers every site through {end.date()}; nothing to do.")
    raise SystemExit(0)

frames = ([existing] if existing is not None else []) + pieces
daily_long = (pd.concat(frames, ignore_index=True)
              .drop_duplicates(subset=["time", "site"], keep="last"))
# Only sites still in site_info, in site_info order, then by date -- so the
# layout does not depend on the order in which sites were added.
order = {s: i for i, s in enumerate(all_sites)}
daily_long = daily_long.loc[daily_long["site"].isin(order)]
daily_long = (daily_long
              .assign(_o=daily_long["site"].map(order))
              .sort_values(["time", "_o"])
              .drop(columns="_o"))

# float32 is what ERA5 stores and what the file has always held; without the
# cast, rows read back from the existing file are written out as float64 and
# every old value changes its last digits.
daily_long["SWC"] = daily_long["SWC"].astype("float32")

# Write beside the target and rename, so an interrupted run never leaves a
# truncated file that the next incremental run would then extend.
tmp = OUT.with_suffix(".csv.part")
daily_long.to_csv(tmp, index=False, date_format="%Y-%m-%d")
tmp.replace(OUT)
print(f"Wrote {OUT}: {daily_long['site'].nunique()} site(s), through {daily_long['time'].max().date()}.")
