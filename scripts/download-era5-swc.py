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

if "SLURM_CPUS_PER_TASK" in os.environ:
    cluster = LocalCluster(n_workers=int(os.environ["SLURM_CPUS_PER_TASK"]))
    dclient = Client(cluster)
else:
    dclient = Client()

with open("_creds.toml", "r") as f:
    creds = tomllib.loads(f.read())

CDS_API_KEY = creds["cds_api_key"]

site_info = pd.read_csv("data-core/site_info.csv")
site_lat = xr.DataArray(site_info["LAT"], dims="site")
site_lon = xr.DataArray(site_info["LONG"], dims="site")
site_dim = xr.DataArray(site_info["site_ID"], dims="site")

# Special case -- too close to shore; need to find the closest non-water pixel.
it_noe = site_info["site_ID"] == "IT-Noe"
it_noe_lat = site_info.loc[it_noe, "LAT"].item()
it_noe_lon = site_info.loc[it_noe, "LONG"].item()

chunks = "geo"
soil_water_url = f"https://arco.datastores.ecmwf.int/cadl-arco-{chunks}-005/arco/reanalysis_era5_land/sfc-soil-water/{chunks}Chunked.zarr"

dat = xr.open_zarr(
    soil_water_url,
    consolidated=True,
    storage_options = {
        "headers": {"Authorization": f"Bearer {CDS_API_KEY}"}
        }
)

extracted_ds = (dat
                .sel(latitude=site_lat, longitude=site_lon, method="nearest")
                .sel(time=slice("1990-01-01", "2026-07-01"))
                .assign_coords(site=site_dim))

# NOTE: `swvl1` is ERA5-Land volumetric soil water for layer 1 (0-7 cm), in
# m3/m3. It is written out here in that native unit; the analysis convention is
# percent (0-100), and `read_era5_swc()` in R/total_tas.R does the conversion.
daily = extracted_ds["swvl1"].resample(time="1D").mean()

daily_pd = daily.to_pandas()

daily_pd.columns.name = "site"

daily_long = daily_pd.stack()
daily_long.name = "SWC"

daily_long.to_csv("data-raw/ERA5_daily_swc.csv")
