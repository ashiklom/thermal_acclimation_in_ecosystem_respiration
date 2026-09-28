#!/usr/bin/env -S uv run --script
#
# /// script
# requires-python = ">=3.13"
# dependencies = [
#   "aiohttp",
#   "fsspec",
#   "numpy",
#   "pandas",
#   "xarray",
#   "zarr"
# ]
# ///
#
# For a site whose nearest ERA5-Land cell is sea, find the nearest cell that is
# land, and print the coordinates to use instead.
#
#   ./scripts/find-coastal-land-pixel.py IT-Noe
#
# Results go to stdout only. Paste the reported latitude/longitude into
# COASTAL_SITES in scripts/download-era5-swc.py.
#
# Why the neighbourhood search and not a land-sea mask: the CADS ARCO store
# publishes exactly one ERA5-Land group, `sfc-soil-water` (swvl1-swvl4). There
# is no `lsm` variable and no invariants group alongside it, and the bucket
# refuses prefix listing, so there is nothing to discover by walking it. That is
# not much of a loss -- ERA5-Land is *defined* only on land, so a cell being
# all-NaN in swvl1 is the land-sea mask, at exactly the grid and resolution we
# need it at, rather than a 0.25-degree ERA5 mask resampled onto it.

import argparse
import sys

import numpy as np
import pandas as pd
import tomllib
import xarray as xr

SOIL_WATER_URL = (
    "https://arco.datastores.ecmwf.int/cadl-arco-geo-005/arco/"
    "reanalysis_era5_land/sfc-soil-water/geoChunked.zarr"
)

# Enough scattered timesteps to distinguish a sea cell (NaN for the whole
# record) from a land cell that happens to be missing at one instant.
N_TIME_SAMPLES = 24

EARTH_RADIUS_KM = 6371.0088


def haversine_km(lat1, lon1, lat2, lon2):
    """Great-circle distance in km between scalars/arrays of degrees."""
    lat1, lon1, lat2, lon2 = (np.radians(np.asarray(x, dtype=float))
                              for x in (lat1, lon1, lat2, lon2))
    dlat = lat2 - lat1
    dlon = lon2 - lon1
    h = np.sin(dlat / 2) ** 2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon / 2) ** 2
    return 2 * EARTH_RADIUS_KM * np.arcsin(np.sqrt(h))


def to_store_longitude(lon, store_lon):
    """Put a site longitude in the same convention as the store's axis."""
    if float(store_lon.min()) >= 0.0 and lon < 0.0:
        return lon + 360.0
    if float(store_lon.max()) <= 180.0 and lon > 180.0:
        return lon - 360.0
    return lon


def neighbourhood_indices(axis, target, radius, wrap):
    """Indices of the `2 * radius + 1` cells centred on the nearest to target.

    Longitude wraps around the globe; latitude is clipped at the poles, so a
    site near a pole simply gets a shorter list rather than an index error.
    """
    centre = int(np.abs(axis - target).argmin())
    offsets = np.arange(-radius, radius + 1)
    if wrap:
        return (centre + offsets) % axis.size
    return np.unique(np.clip(centre + offsets, 0, axis.size - 1))


def candidate_cells(dat, site_lat, site_lon, radius):
    """Sample swvl1 on a square neighbourhood, one row per candidate cell."""
    lat_axis = dat["latitude"].values
    lon_axis = dat["longitude"].values
    lon_target = to_store_longitude(site_lon, lon_axis)

    lat_idx = neighbourhood_indices(lat_axis, site_lat, radius, wrap=False)
    lon_idx = neighbourhood_indices(lon_axis, lon_target, radius, wrap=True)

    # Spread the samples across the whole record rather than taking the first
    # N hours, so a cell is only called sea if it is missing throughout.
    n_time = dat.sizes["time"]
    time_idx = np.unique(
        np.linspace(0, n_time - 1, min(N_TIME_SAMPLES, n_time)).astype(int)
    )

    block = (dat["swvl1"]
             .isel(latitude=lat_idx, longitude=lon_idx, time=time_idx)
             .load())

    rows = []
    for i, lat in enumerate(block["latitude"].values):
        for j, lon in enumerate(block["longitude"].values):
            values = block.isel(latitude=i, longitude=j).values
            n_valid = int(np.isfinite(values).sum())
            rows.append({
                "latitude": float(lat),
                "longitude": float(lon),
                "distance_km": float(haversine_km(site_lat, lon_target, lat, lon)),
                "n_sampled": values.size,
                "n_valid": n_valid,
                "is_land": n_valid > 0,
                "mean_swvl1": (float(np.nanmean(values)) if n_valid else float("nan")),
            })

    return pd.DataFrame(rows).sort_values("distance_km").reset_index(drop=True)


def report(site_id, site_lat, site_lon, cells):
    nearest = cells.iloc[0]
    land = cells[cells["is_land"]]

    print(f"=== {site_id} ({site_lat:.4f}, {site_lon:.4f}) ===")
    print(f"{len(cells)} candidate cells, {len(land)} on land")
    print()
    with pd.option_context("display.width", 200, "display.max_rows", None):
        print(cells.to_string(index=False, float_format=lambda x: f"{x:.4f}"))
    print()

    if nearest["is_land"]:
        print(
            "Nearest cell is land -- this site does not need a coastal "
            "override. Its all-NA data, if any, has another cause."
        )
        return False

    if land.empty:
        print(
            f"No land cell within the sampled neighbourhood (radius "
            f"{(int(np.sqrt(len(cells))) - 1) // 2}). Re-run with a larger "
            f"--radius."
        )
        return False

    best = land.iloc[0]
    print(
        f"Nearest cell ({nearest['latitude']:.2f}, {nearest['longitude']:.2f}) "
        f"is sea: 0 of {int(nearest['n_sampled'])} sampled timesteps are finite."
    )
    print(
        f"Nearest land cell is ({best['latitude']:.2f}, "
        f"{best['longitude']:.2f}), {best['distance_km']:.1f} km away, "
        f"mean swvl1 {best['mean_swvl1']:.3f} m3/m3."
    )
    print()
    print("For COASTAL_SITES in scripts/download-era5-swc.py:")
    print(
        f'    "{site_id}": ({best["latitude"]:.2f}, {best["longitude"]:.2f}),'
        f"  # nearest land cell, {best['distance_km']:.1f} km from the tower"
    )
    return True


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "sites", nargs="*", default=["IT-Noe"],
        help="Site IDs from data-core/site_info.csv (default: IT-Noe)"
    )
    parser.add_argument(
        "--radius", type=int, default=1,
        help="Half-width of the search neighbourhood in grid cells. The "
             "default of 1 gives the nearest cell plus its 8 neighbours."
    )
    parser.add_argument(
        "--site-info", default="data-core/site_info.csv",
        help="Path to the site metadata table"
    )
    args = parser.parse_args()

    with open("_creds.toml", "r") as f:
        cds_api_key = tomllib.loads(f.read())["cds_api_key"]

    site_info = pd.read_csv(args.site_info).set_index("site_ID")
    missing = [s for s in args.sites if s not in site_info.index]
    if missing:
        parser.error(f"Not in {args.site_info}: {', '.join(missing)}")

    dat = xr.open_zarr(
        SOIL_WATER_URL,
        consolidated=True,
        storage_options={"headers": {"Authorization": f"Bearer {cds_api_key}"}},
    )

    for k, site_id in enumerate(args.sites):
        if k:
            print()
        row = site_info.loc[site_id]
        cells = candidate_cells(
            dat, float(row["LAT"]), float(row["LONG"]), args.radius
        )
        report(site_id, float(row["LAT"]), float(row["LONG"]), cells)


if __name__ == "__main__":
    sys.exit(main())
