#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.10"
# dependencies = ["icoscp_core", "requests"]
# ///

"""Download ICOS ecosystem flux archives for one or more stations.

Two products are available here, and most sites need both, because neither
covers the full record on its own:

  icos    The "Ecosystem final quality (L2) product in ETC-Archive format"
          (datatype etcL2Fluxnet). By design this covers only the period since
          a station was ICOS-labelled -- a station labelled in 2019 has an L2
          product starting in 2019 however long it has been running -- so on
          its own it truncates most records severely. It does, however, reach
          later than the other products.

  ww2020  Warm Winter 2020 (1989-2020), the FLUXNET2015-format product that
          carries the pre-labelling history for 73 stations, including several
          outside the ICOS network.

The full-record FLUXNET-Archive product is fetched separately, by
scripts/download-fluxnet.sh via fluxnet-shuttle.

Both products are saved as shipped and the half-hourly table extracted
verbatim; nothing here reformats or renames columns. An earlier version of this
script rebuilt a FLUXNET-shaped table out of three ICOS L2 products, which
meant synthesising `NIGHT` from a shortwave threshold and fabricating QC flags
from value presence -- both of which then fed the QC filters downstream. The
archives already contain the real columns.

See docs/data-provenance.md.
"""

from __future__ import annotations

import argparse
import logging
import re
import zipfile
from pathlib import Path

import requests
from icoscp_core.icos import data, meta

LOGGER = logging.getLogger(__name__)

STATION_URI = "http://meta.icos-cp.eu/resources/stations/ES_{site}"
ETC_L2_FLUXNET = "http://meta.icos-cp.eu/resources/cpmeta/etcL2Fluxnet"
WW2020_COLLECTION = "https://meta.icos-cp.eu/collections/gdINRHdRH6xknqoLsIU1FOZ4"

# product -> (directory under data-raw, regex for the half-hourly table in the zip)
PRODUCTS = {
    "icos": ("ICOS", re.compile(r"_FLUXMET_(HH|HR)_.*\.csv$")),
    "ww2020": ("WW2020", re.compile(r"_FLUXNET2015_FULLSET_(HH|HR)_.*\.csv$")),
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("-s", "--sites", nargs="+", help="Site IDs, e.g. DE-Tha CH-Dav.")
    parser.add_argument(
        "-p", "--product", choices=sorted(PRODUCTS), default="icos",
        help="Which archive to fetch (default: icos).",
    )
    parser.add_argument("-o", "--overwrite", action="store_true")
    parser.add_argument("-d", "--output-dir", type=Path, default=Path("data-raw"))
    return parser.parse_args()


def icos_l2_uri(site: str) -> str:
    """The station's single ETC L2 FLUXNET object."""
    objects = meta.list_data_objects(
        datatype=ETC_L2_FLUXNET, station=STATION_URI.format(site=site)
    )
    if not objects:
        raise RuntimeError(f"No ICOS ETC L2 FLUXNET product for {site}")
    if len(objects) > 1:
        # Has never happened; if it starts to, picking blindly would silently
        # take a slice of the record, which is the bug this script once had.
        names = ", ".join(o.filename for o in objects)
        raise RuntimeError(f"{site} has {len(objects)} L2 objects, expected 1: {names}")
    LOGGER.info("  %s", objects[0].filename)
    return objects[0].uri


def ww2020_uri(site: str) -> str:
    """The station's member of the Warm Winter 2020 collection."""
    members = requests.get(
        WW2020_COLLECTION, headers={"Accept": "application/json"}, timeout=120
    ).json()["members"]
    pattern = re.compile(rf"^FLX_{re.escape(site)}_FLUXNET2015_FULLSET_")
    hits = [m for m in members if pattern.match(m["name"])]
    if not hits:
        raise RuntimeError(
            f"{site} is not in Warm Winter 2020. Its pre-labelling history is "
            f"not available from this product."
        )
    LOGGER.info("  %s", hits[0]["name"])
    return hits[0]["res"]


def fetch(uri: str, site_dir: Path) -> Path:
    """Save the object to site_dir as shipped, returning the local path.

    `get_file_stream` takes the object URI (not its metadata) and hands back the
    server-side filename, so the archive keeps the name that encodes its product
    and year span -- which is what makes the update check in
    scripts/check-data-updates.R able to compare releases.
    """
    site_dir.mkdir(parents=True, exist_ok=True)
    filename, stream = data.get_file_stream(uri)
    target = site_dir / filename
    with stream, open(target, "wb") as dst:
        while chunk := stream.read(1 << 20):
            dst.write(chunk)
    return target


def extract_hh(archive: Path, site_dir: Path, table_re: re.Pattern) -> list[str]:
    """Pull the half-hourly table out of the archive, leaving the archive in place."""
    if not zipfile.is_zipfile(archive):
        # Some objects are shipped as a bare CSV rather than a zip.
        return [archive.name] if table_re.search(archive.name) else []
    extracted = []
    with zipfile.ZipFile(archive) as zf:
        for member in zf.namelist():
            if table_re.search(member):
                target = site_dir / Path(member).name
                with zf.open(member) as src, open(target, "wb") as dst:
                    dst.write(src.read())
                extracted.append(target.name)
    return extracted


def main() -> None:
    args = parse_args()
    logging.basicConfig(level=logging.INFO, format="%(message)s")
    if not args.sites:
        raise SystemExit("Nothing to do: pass --sites.")

    subdir, table_re = PRODUCTS[args.product]
    for site in dict.fromkeys(args.sites):
        site_dir = args.output_dir / subdir / site
        existing = [p.name for p in site_dir.glob("*.csv") if table_re.search(p.name)]
        if existing and not args.overwrite:
            LOGGER.info("%s (%s): already have %s", site, args.product, existing[0])
            continue

        LOGGER.info("%s (%s):", site, args.product)
        uri = icos_l2_uri(site) if args.product == "icos" else ww2020_uri(site)
        archive = fetch(uri, site_dir)
        extracted = extract_hh(archive, site_dir, table_re)
        if not extracted:
            raise RuntimeError(
                f"{site}: no half-hourly table matching {table_re.pattern} in "
                f"{archive.name}"
            )
        LOGGER.info("  extracted %s", ", ".join(extracted))


if __name__ == "__main__":
    main()
