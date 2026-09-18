#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.10"
# dependencies = ["icoscp_core", "requests", "pandas"]
# ///

"""Is there newer flux data than what we have on disk?

Run this monthly. For every site and every product in that site's provenance
list (`site_info$source`), it compares the archive we hold against what the
provider currently publishes, and reports one line per product:

    ok        local archive matches the published one
    UPDATE    provider has a different archive -- usually a new release or an
              extended year span
    MISSING   declared in site_info but nothing downloaded yet
    ?         could not be checked (provider unreachable, or product not
              queryable -- see the notes per product below)

Exit status is 0 when everything is `ok`, 1 when any site needs attention, so
this can be wired into cron or CI.

How each product is checked:

  ICOS    Queried live from the ICOS metadata service (datatype etcL2Fluxnet).
          Filenames encode span, version and release
          (ICOSETC_DE-Tha_FLUXNET_FLUXMET_HH_2020-2025_v1.3_r1.zip), so a
          string comparison is a reliable change detector. New ICOS releases
          appear roughly annually and also add newly-labelled stations.

  FLUXNET Compared against a fluxnet-shuttle snapshot. Pass --refresh-snapshot
          to fetch a current one first; otherwise the newest snapshot in
          data-raw/ is used and its age is reported, because a stale snapshot
          makes this check meaningless.

  WW2020  Warm Winter 2020 is a closed 2022 release covering 1989-2020. It is
          checked for membership and filename but is not expected to change;
          an UPDATE here would mean the collection was revised.

See docs/data-provenance.md.
"""

from __future__ import annotations

import argparse
import datetime as dt
import re
import subprocess
import sys
from pathlib import Path

import pandas as pd
import requests
from icoscp_core.icos import meta

STATION_URI = "http://meta.icos-cp.eu/resources/stations/ES_{site}"
ETC_L2_FLUXNET = "http://meta.icos-cp.eu/resources/cpmeta/etcL2Fluxnet"
WW2020_COLLECTION = "https://meta.icos-cp.eu/collections/gdINRHdRH6xknqoLsIU1FOZ4"

RAW = Path("data-raw")
# product -> (site sub-directory under data-raw, glob for the local archive)
LOCAL = {
    "ICOS": ("ICOS", "*.zip"),
    "WW2020": ("WW2020", "*.zip"),
    "FLUXNET": ("FLUXNET", "*.zip"),
    "FLUXNET2015": ("FLUXNET", "*.zip"),
    "TERN": ("TERN", "*.csv"),
    "AmeriFlux_BASE": ("Ameriflux", "*.zip"),
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("-s", "--sites", nargs="+", help="Limit to these site IDs.")
    parser.add_argument(
        "-p", "--products", nargs="+", default=["ICOS", "WW2020", "FLUXNET"],
        help="Which products to check (default: ICOS WW2020 FLUXNET).",
    )
    parser.add_argument(
        "--refresh-snapshot", action="store_true",
        help="Fetch a current fluxnet-shuttle snapshot before comparing.",
    )
    return parser.parse_args()


def local_archive(site: str, product: str) -> str | None:
    subdir, pattern = LOCAL[product]
    # FLUXNET archives land flat in data-raw/FLUXNET; everything else per site.
    candidates = sorted(
        list((RAW / subdir / site).glob(pattern)) + list(RAW.joinpath(subdir).glob(f"*_{site}_*.zip"))
    )
    return candidates[-1].name if candidates else None


def newest_snapshot() -> Path | None:
    snaps = sorted(RAW.glob("fluxnet_shuttle_snapshot_*.csv"))
    return snaps[-1] if snaps else None


def snapshot_age_days(path: Path) -> int | None:
    match = re.search(r"(\d{8})T", path.name)
    if not match:
        return None
    stamp = dt.datetime.strptime(match.group(1), "%Y%m%d").date()
    return (dt.date.today() - stamp).days


def remote_icos(site: str) -> str | None:
    objects = meta.list_data_objects(
        datatype=ETC_L2_FLUXNET, station=STATION_URI.format(site=site)
    )
    return objects[0].filename if objects else None


_ww_cache: dict[str, str] | None = None


def remote_ww2020(site: str) -> str | None:
    global _ww_cache
    if _ww_cache is None:
        members = requests.get(
            WW2020_COLLECTION, headers={"Accept": "application/json"}, timeout=120
        ).json()["members"]
        _ww_cache = {}
        for m in members:
            g = re.match(r"FLX_([A-Za-z0-9\-]+)_FLUXNET2015_FULLSET_", m["name"])
            if g:
                _ww_cache[g.group(1)] = m["name"]
    return _ww_cache.get(site)


def main() -> int:
    args = parse_args()
    site_info = pd.read_csv(Path("data-core") / "site_info.csv")

    if args.refresh_snapshot:
        print("Refreshing fluxnet-shuttle snapshot...")
        subprocess.run(["fluxnet-shuttle", "listall", "-o", str(RAW)], check=True)

    snapshot_path = newest_snapshot()
    shuttle = pd.read_csv(snapshot_path) if snapshot_path is not None else None
    if snapshot_path is None:
        print("! No fluxnet-shuttle snapshot in data-raw/; FLUXNET cannot be checked.")
        print("  Re-run with --refresh-snapshot.\n")
    else:
        age = snapshot_age_days(snapshot_path)
        note = f"{age} days old" if age is not None else "age unknown"
        flag = "  <-- stale, re-run with --refresh-snapshot" if (age or 0) > 45 else ""
        print(f"fluxnet-shuttle snapshot: {snapshot_path.name} ({note}){flag}\n")

    wanted = set(args.products)
    rows = []
    for _, site_row in site_info.iterrows():
        site = site_row["site_ID"]
        if args.sites and site not in args.sites:
            continue
        for product in [p.strip() for p in str(site_row["source"]).split("+")]:
            if product not in wanted:
                continue
            local = local_archive(site, product)
            try:
                if product == "ICOS":
                    remote = remote_icos(site)
                elif product == "WW2020":
                    remote = remote_ww2020(site)
                elif product in ("FLUXNET", "FLUXNET2015"):
                    if shuttle is None:
                        remote = None
                    else:
                        hit = shuttle.loc[shuttle.site_id == site, "fluxnet_product_name"]
                        remote = hit.iloc[0] if len(hit) else None
                else:
                    remote = None
            except Exception as exc:  # provider trouble, not our bug
                rows.append((site, product, "?", f"lookup failed: {exc}"))
                continue

            if remote is None:
                rows.append((site, product, "?", "not published / not queryable"))
            elif local is None:
                rows.append((site, product, "MISSING", f"-> {remote}"))
            elif local == remote:
                rows.append((site, product, "ok", local))
            else:
                rows.append((site, product, "UPDATE", f"{local} -> {remote}"))

    width = max((len(r[1]) for r in rows), default=8)
    for site, product, status, detail in rows:
        if status != "ok":
            print(f"  {status:8s} {site:8s} {product:<{width}s}  {detail}")
    counts = pd.Series([r[2] for r in rows]).value_counts().to_dict()
    print("\n  " + ", ".join(f"{v} {k}" for k, v in sorted(counts.items())))
    if any(r[2] != "ok" for r in rows):
        print("\n  Next: see docs/data-provenance.md for what to run.")
        return 1
    print("\n  Everything up to date.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
