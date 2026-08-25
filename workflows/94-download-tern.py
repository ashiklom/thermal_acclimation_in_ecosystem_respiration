#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.10"
# dependencies = ["terndata.flux>=1.0.7", "pandas"]
# ///

"""Download and normalize TERN OzFlux data for the project workflows."""

from __future__ import annotations

import argparse
import logging
from pathlib import Path

import pandas as pd
import terndata.flux as flux


LOGGER = logging.getLogger(__name__)
DEFAULT_OUTPUT = Path("data-raw/TERN")
DEFAULT_SITE_INFO = Path("data-core/site_info.csv")
DEFAULT_MAPPING = Path("data-core/tern_fluxnet_site_mapping.csv")
VARIABLES = [
    "Fco2", "Fco2_QCFlag", "Ta", "Ta_QCFlag", "Ts", "Ts_QCFlag",
    "Sws", "Sws_QCFlag", "Fsd", "Fsd_QCFlag", "Fn", "Fn_QCFlag",
    "ustar", "ustar_QCFlag",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("-s", "--sites", nargs="+", help="TERN site names.")
    parser.add_argument("-o", "--overwrite", action="store_true")
    parser.add_argument("-d", "--output-dir", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("-f", "--site-info", type=Path, default=DEFAULT_SITE_INFO)
    parser.add_argument("-m", "--mapping", type=Path, default=DEFAULT_MAPPING)
    parser.add_argument("--version", help="TERN data version (default: latest available).")
    parser.add_argument("--processing-level", default="L3", choices=["L3", "L4", "L5", "L6"])
    return parser.parse_args()


def sites(requested: list[str] | None, site_info: Path) -> list[str]:
    if requested:
        return list(dict.fromkeys(requested))
    table = pd.read_csv(site_info, usecols=["site_ID"])
    return table.loc[table.site_ID.str.startswith("AU-"), "site_ID"].drop_duplicates().tolist()


def resolve_site(code: str, mapping: Path) -> tuple[str, str]:
    table = pd.read_csv(mapping, dtype=str)
    match = table[table.fluxnet_site == code]
    if len(match) != 1 or match.iloc[0].matched.lower() != "true":
        raise ValueError(
            f"No unambiguous TERN mapping for {code}; run workflows/94-map-tern-sites.py"
        )
    return code, match.iloc[0].tern_site


def latest_version(site: str) -> str:
    versions = flux.get_versions(site)
    if not versions:
        raise RuntimeError(f"TERN returned no versions for {site}")
    return max(versions)


def normalize(site: str, version: str | None, processing_level: str) -> pd.DataFrame:
    version = version or latest_version(site)
    available = set(flux.get_variables(site, version, processing_level))
    missing = sorted(set(VARIABLES) - available)
    if missing:
        raise RuntimeError(f"{site} {version} {processing_level} is missing variables: {missing}")

    dataset = flux.get_subset(
        site, version, processing_level, VARIABLES, missing_as_nan=True
    )
    frame = dataset[VARIABLES].to_dataframe().reset_index()
    frame = frame.drop(columns=["latitude", "longitude"], errors="ignore")
    frame = frame.rename(columns={
        "time": "TIMESTAMP_END",
        "Fco2": "NEE_VUT_REF",
        "Fco2_QCFlag": "NEE_VUT_REF_QC",
        "Ta": "TA_F_MDS",
        "Ta_QCFlag": "TA_F_MDS_QC",
        "Ts": "TS_F_MDS_1",
        "Ts_QCFlag": "TS_F_MDS_1_QC",
        "Sws": "SWC_F_MDS_1",
        "Sws_QCFlag": "SWC_F_MDS_1_QC",
        "Fsd": "SW_IN_F_MDS",
        "Fsd_QCFlag": "SW_IN_F_MDS_QC",
        "Fn": "NETRAD",
        "Fn_QCFlag": "NETRAD_QC",
        "ustar": "USTAR",
        "ustar_QCFlag": "USTAR_QC",
    })
    frame["TIMESTAMP_END"] = pd.to_datetime(frame["TIMESTAMP_END"])
    frame["TIMESTAMP_START"] = frame["TIMESTAMP_END"] - pd.Timedelta(minutes=30)
    frame["NIGHT"] = (frame["SW_IN_F_MDS"] <= 20).where(frame["SW_IN_F_MDS"].notna())
    frame["NIGHT"] = frame["NIGHT"].fillna(False).astype(int)
    frame["TIMESTAMP_START"] = frame["TIMESTAMP_START"].dt.strftime("%Y%m%d%H%M")
    frame["TIMESTAMP_END"] = frame["TIMESTAMP_END"].dt.strftime("%Y%m%d%H%M")
    return frame.sort_values("TIMESTAMP_START").reset_index(drop=True)


def main() -> None:
    args = parse_args()
    logging.basicConfig(level=logging.INFO, format="%(message)s")
    args.output_dir.mkdir(parents=True, exist_ok=True)
    for code in sites(args.sites, args.site_info):
        fluxnet_code, tern_site = resolve_site(code, args.mapping)
        site_dir = args.output_dir / fluxnet_code
        site_dir.mkdir(parents=True, exist_ok=True)
        output = site_dir / f"{fluxnet_code}_TERN_{args.processing_level}_FLUXNET_HH.csv"
        if output.exists() and not args.overwrite:
            LOGGER.info("Skipping %s (already exists)", output)
            continue
        LOGGER.info("Downloading %s (%s)", fluxnet_code, tern_site)
        normalize(tern_site, args.version, args.processing_level).to_csv(
            output, index=False, na_rep="-9999"
        )
        LOGGER.info("Wrote %s", output)


if __name__ == "__main__":
    main()
