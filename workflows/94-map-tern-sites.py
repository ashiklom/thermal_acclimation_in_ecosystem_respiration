#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.10"
# dependencies = ["terndata.flux>=1.0.7", "pandas"]
# ///

"""Generate the FLUXNET-code to TERN-native-site mapping."""

from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd
import terndata.flux as flux


DEFAULT_SITE_INFO = Path("data/site_info.csv")
DEFAULT_OUTPUT = Path("data/tern_fluxnet_site_mapping.csv")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("-f", "--site-info", type=Path, default=DEFAULT_SITE_INFO)
    parser.add_argument("-o", "--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("-s", "--sites", nargs="+", help="FLUXNET codes to map (default: AU-* entries).")
    parser.add_argument("--max-distance-km", type=float, default=5.0)
    return parser.parse_args()


def distance_km(lat1: float, lon1: float, lat2: float, lon2: float) -> float:
    from math import asin, cos, radians, sin, sqrt

    dlat = radians(lat2 - lat1)
    dlon = radians(lon2 - lon1)
    a = sin(dlat / 2) ** 2 + cos(radians(lat1)) * cos(radians(lat2)) * sin(dlon / 2) ** 2
    return 6371.0088 * 2 * asin(sqrt(a))


def main() -> None:
    args = parse_args()
    site_info = pd.read_csv(args.site_info, dtype={"site_ID": str})
    requested = set(args.sites or site_info.loc[site_info.site_ID.str.startswith("AU-"), "site_ID"])
    site_info = site_info[site_info.site_ID.isin(requested)].copy()
    if site_info.empty:
        raise ValueError("No requested FLUXNET sites were found in site_info.csv")

    tern_sites = flux.get_sites()[["site", "latitude", "longitude"]]
    rows = []
    for record in site_info.itertuples():
        candidates = tern_sites.assign(
            distance_km=[distance_km(record.LAT, record.LONG, lat, lon)
                         for lat, lon in zip(tern_sites.latitude, tern_sites.longitude)]
        ).sort_values("distance_km")
        match = candidates.iloc[0]
        matched = float(match.distance_km) <= args.max_distance_km
        rows.append({
            "fluxnet_site": record.site_ID,
            "tern_site": match.site if matched else "",
            "fluxnet_lat": record.LAT,
            "fluxnet_lon": record.LONG,
            "tern_lat": match.latitude if matched else "",
            "tern_lon": match.longitude if matched else "",
            "distance_km": match.distance_km,
            "matched": matched,
        })

    args.output.parent.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(rows).to_csv(args.output, index=False)
    print(f"Wrote {args.output}")
    print(pd.DataFrame(rows).to_string(index=False))


if __name__ == "__main__":
    main()
