#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.10"
# dependencies = ["icoscp", "pandas"]
# ///

"""Download ICOS ETC level-2 ecosystem data for one or more stations."""

from __future__ import annotations

import argparse
import logging
from pathlib import Path

import pandas as pd
from icoscp.dobj import Dobj
from icoscp_core.icos import meta


LOGGER = logging.getLogger(__name__)
STATION_URI = "http://meta.icos-cp.eu/resources/stations/ES_{site}"
DATATYPE_URI = "http://meta.icos-cp.eu/resources/cpmeta/{datatype}"
OUTPUT_DIR = Path(
    "data-raw/SiteData/Ecosystem final quality (L2) product in ETC-Archive format - release 2025-1/unzip"
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Download ICOS ETC level-2 ecosystem data in FLUXNET-style CSV format."
    )
    parser.add_argument(
        "-s",
        "--sites",
        nargs="+",
        help="ICOS site IDs to download, for example FR-FBn FR-Fon.",
    )
    parser.add_argument(
        "-f",
        "--site-list",
        type=Path,
        default=Path("~/Downloads/stations.csv").expanduser(),
        help="CSV containing ICOS station metadata (default: ~/Downloads/stations.csv).",
    )
    parser.add_argument(
        "-o",
        "--overwrite",
        action="store_true",
        help="Replace existing normalized CSV files.",
    )
    parser.add_argument(
        "-d",
        "--output-dir",
        type=Path,
        default=OUTPUT_DIR,
        help=f"Output directory (default: {OUTPUT_DIR}).",
    )
    return parser.parse_args()


def station_ids(site_list: Path, requested: list[str] | None) -> list[str]:
    if requested:
        return list(dict.fromkeys(requested))

    if not site_list.is_file():
        raise FileNotFoundError(f"Station list not found: {site_list}")

    stations = pd.read_csv(site_list, usecols=["Id"])
    ids = stations["Id"].astype(str).str.extract(r"ES_([A-Z]{2}-[^,]+)$")[0]
    return ids.dropna().drop_duplicates().tolist()


def get_product(site: str, datatype: str) -> pd.DataFrame:
    station_uri = STATION_URI.format(site=site)
    objects = meta.list_data_objects(
        datatype=DATATYPE_URI.format(datatype=datatype), station=station_uri
    )
    if not objects:
        raise RuntimeError(f"No {datatype} level-2 product found for {site}")

    latest = objects[0]
    LOGGER.info("  %s: %s", datatype, latest.filename)
    return Dobj(latest.uri).data


def normalize(site: str) -> pd.DataFrame:
    fluxnet = get_product(site, "etcL2Fluxnet")
    meteo = get_product(site, "etcL2Meteo")
    meteosens = get_product(site, "etcL2Meteosens")

    fluxnet = fluxnet.rename(
        columns={
            "TA_F": "TA_F_MDS",
            "TA_F_QC": "TA_F_MDS_QC",
            "SW_IN_F": "SW_IN_F_MDS",
            "SW_IN_F_QC": "SW_IN_F_MDS_QC",
        }
    )
    required_fluxnet = [
        "TIMESTAMP",
        "TA_F_MDS",
        "TA_F_MDS_QC",
        "SW_IN_F_MDS",
        "SW_IN_F_MDS_QC",
        "NEE_VUT_REF",
        "NEE_VUT_REF_QC",
    ]
    missing = sorted(set(required_fluxnet) - set(fluxnet.columns))
    if missing:
        raise RuntimeError(f"{site} Fluxnet product is missing columns: {missing}")

    ts_columns = [column for column in meteosens.columns if column.startswith("TS_")]
    if not ts_columns:
        raise RuntimeError(f"{site} Meteosens product has no soil temperature columns")

    meteo_columns = [
        column
        for column in ["TIMESTAMP", "SW_OUT", "LW_IN", "LW_OUT", "SWC_1"]
        if column in meteo.columns
    ]
    merged = fluxnet[required_fluxnet].merge(
        meteosens[["TIMESTAMP", ts_columns[0]]], on="TIMESTAMP", how="left"
    )
    merged = merged.merge(meteo[meteo_columns], on="TIMESTAMP", how="left")
    merged = merged.rename(columns={ts_columns[0]: "TS_F_MDS_1", "SWC_1": "SWC_F_MDS_1"})

    merged["NEE_VUT_REF_QC"] = pd.to_numeric(
        merged["NEE_VUT_REF_QC"], errors="coerce"
    )
    merged["TS_F_MDS_1_QC"] = merged["TS_F_MDS_1"].notna().astype(int)
    merged["SWC_F_MDS_1_QC"] = merged["SWC_F_MDS_1"].notna().astype(int)
    merged["NETRAD"] = (
        merged["SW_IN_F_MDS"]
        - merged.get("SW_OUT", 0)
        + merged.get("LW_IN", 0)
        - merged.get("LW_OUT", 0)
    )
    merged["NIGHT"] = (merged["SW_IN_F_MDS"] <= 20).fillna(False).astype(int)
    merged["TIMESTAMP_START"] = pd.to_datetime(merged.pop("TIMESTAMP")).dt.strftime(
        "%Y%m%d%H%M"
    )
    merged["TIMESTAMP_END"] = merged["TIMESTAMP_START"]
    return merged.sort_values("TIMESTAMP_START").reset_index(drop=True)


def main() -> None:
    args = parse_args()
    logging.basicConfig(level=logging.INFO, format="%(message)s")
    sites = station_ids(args.site_list, args.sites)
    if not sites:
        raise RuntimeError("No ecosystem stations found")

    args.output_dir.mkdir(parents=True, exist_ok=True)
    for site in sites:
        output = args.output_dir / f"{site}_ICOS_L2_FLUXNET_HH.csv"
        if output.exists() and not args.overwrite:
            LOGGER.info("Skipping %s (already exists)", output)
            continue
        LOGGER.info("Downloading %s", site)
        normalize(site).to_csv(output, index=False, na_rep="-9999")
        LOGGER.info("  wrote %s", output)


if __name__ == "__main__":
    main()
