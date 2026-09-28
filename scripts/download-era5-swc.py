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

import os

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

is_coastal = site_info["site_ID"].isin(COASTAL_SITES)

chunks = "geo"
soil_water_url = f"https://arco.datastores.ecmwf.int/cadl-arco-{chunks}-005/arco/reanalysis_era5_land/sfc-soil-water/{chunks}Chunked.zarr"

dat = xr.open_zarr(
    soil_water_url,
    consolidated=True,
    storage_options = {
        "headers": {"Authorization": f"Bearer {CDS_API_KEY}"}
        }
)


def extract_daily(site_ids, lats, lons):
    """Daily mean layer-1 soil water at the nearest cell to each coordinate.

    Returns a wide frame indexed by date with one column per site ID.

    NOTE: `swvl1` is ERA5-Land volumetric soil water for layer 1 (0-7 cm), in
    m3/m3. It is returned here in that native unit; the analysis convention is
    percent (0-100), and `read_era5_swc()` in R/total_tas.R does the conversion.
    """
    extracted_ds = (dat
                    .sel(latitude=xr.DataArray(lats, dims="site"),
                         longitude=xr.DataArray(lons, dims="site"),
                         method="nearest")
                    .sel(time=slice("1990-01-01", "2026-07-01"))
                    .assign_coords(site=xr.DataArray(site_ids, dims="site")))
    daily = extracted_ds["swvl1"].resample(time="1D").mean()
    return daily.to_pandas()


# Regular sites: the tower coordinate is inside the land mask, so the nearest
# cell is the right cell.
regular = site_info.loc[~is_coastal]
daily_regular = extract_daily(
    regular["site_ID"].to_numpy(),
    regular["LAT"].to_numpy(),
    regular["LONG"].to_numpy(),
)

# Coastal sites: identical extraction, but off the substituted land coordinate
# instead of the tower coordinate.
coastal = site_info.loc[is_coastal, "site_ID"].to_numpy()
daily_coastal = extract_daily(
    coastal,
    [COASTAL_SITES[s][0] for s in coastal],
    [COASTAL_SITES[s][1] for s in coastal],
)

# A substituted coordinate that is still in the water would hand the analysis
# layer a silently empty column, so refuse to write one.
still_sea = [s for s in coastal if daily_coastal[s].isna().all()]
if still_sea:
    raise ValueError(
        "COASTAL_SITES coordinates are still outside the ERA5-Land land mask "
        f"for: {', '.join(still_sea)}. Re-run "
        "`scripts/find-coastal-land-pixel.py` for these sites."
    )

# Reindexed to the site_info order so the output layout does not depend on how
# the sites were split across the two branches.
daily_pd = (pd.concat([daily_regular, daily_coastal], axis=1)
            .reindex(columns=site_info["site_ID"].to_numpy()))

daily_pd.columns.name = "site"

daily_long = daily_pd.stack()
daily_long.name = "SWC"

daily_long.to_csv("data-raw/ERA5_daily_swc.csv")
