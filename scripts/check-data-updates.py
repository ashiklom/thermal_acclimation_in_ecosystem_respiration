#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.10"
# dependencies = ["icoscp_core", "requests", "pandas", "terndata.flux>=1.0.7"]
# ///

"""Is there newer flux data than what we have on disk?

For every site and every product in that site's provenance list
(`site_info$source`), it compares what we hold against what the provider
currently publishes, and reports one line per product:

    ok        local archive matches the published one
    UPDATE    provider has a different archive -- usually a new release or an
              extended year span
    MISSING   declared in site_info but nothing downloaded yet
    ?         could not be checked (provider unreachable, or product not
              queryable -- see the notes per product below)

Exit status is 0 when everything is `ok`, 1 when any site needs attention, so
this can be wired into cron or CI.

With `--catalog FILE` it instead writes the remote side only -- one row per
site and product, `site_ID,product,remote_id` -- for the pipeline's
`remote_catalog` target, and always refreshes the fluxnet-shuttle snapshot
first. The file is rewritten only when something in it changed, so a quiet
day leaves the pipeline with nothing outdated. A provider that cannot be asked
keeps its previous ids (blank the first time), which the pipeline reads as
"keep what we have", never as "changed".

What "what we hold" means: the downloaders record the `remote_id` they fetched
in a `.remote_id` file in the site's product directory. Where there is none --
data fetched before that existed -- the local archive's filename stands in,
which is exact for the products whose remote id *is* the filename (ICOS,
WW2020, FLUXNET) and never matches for the others, so those are re-fetched once.

How each product is checked:

  ICOS    Queried live from the ICOS metadata service (datatype etcL2Fluxnet),
          one query for every station. Filenames encode span, version and release
          (ICOSETC_DE-Tha_FLUXNET_FLUXMET_HH_2020-2025_v1.3_r1.zip), so a
          string comparison is a reliable change detector. New ICOS releases
          appear roughly annually and also add newly-labelled stations.

  FLUXNET Compared against a fluxnet-shuttle snapshot. Pass --refresh-snapshot
          to fetch a current one first (about 30 s); otherwise the newest
          snapshot in data-raw/ is used and its age is reported, because a
          stale snapshot makes this check meaningless.

  AmeriFlux_BASE
          AmeriFlux publishes no BASE version string through any public
          endpoint -- only which years each site has published, from the same
          data-availability API `amerifluxr::amf_data_coverage()` reads. So the
          remote id is the published year span and count (`years:2010-2025:16`).
          That catches every added year, and misses a reprocessing that
          re-releases the same years under a new version number.

  TERN    `terndata.flux.get_versions()`, latest version, via
          data-core/tern_fluxnet_site_mapping.csv.

  WW2020  Warm Winter 2020 is a closed 2022 release covering 1989-2020. It is
          checked for membership and filename but is not expected to change;
          an UPDATE here would mean the collection was revised.

  FLUXNET2015
          A closed legacy release with no programmatic interface at all, so
          only presence is checked. It never updates. Acquiring it is a manual
          step -- `download_fluxnet2015()` in R/download.R prints the
          procedure.

See docs/data-provenance.md.
"""

from __future__ import annotations

import argparse
import datetime as dt
import re
import subprocess
import sys
import time
from pathlib import Path

import pandas as pd
import requests

ETC_L2_FLUXNET = "http://meta.icos-cp.eu/resources/cpmeta/etcL2Fluxnet"
WW2020_COLLECTION = "https://meta.icos-cp.eu/collections/gdINRHdRH6xknqoLsIU1FOZ4"
AMF_AVAILABILITY = "https://amfcdn.lbl.gov/api/v1/data_availability/AmeriFlux/BASE-BADM/CCBY4.0"

RAW = Path("data-raw")
# product -> (site sub-directory under data-raw, glob for the local archive)
LOCAL = {
    "ICOS": ("ICOS", "*.zip"),
    "WW2020": ("WW2020", "*.zip"),
    "FLUXNET": ("FLUXNET", "*.zip"),
    "FLUXNET2015": ("FLUXNET2015", "*.zip"),
    "TERN": ("TERN", "*.csv"),
    "AmeriFlux_BASE": ("Ameriflux", "*.zip"),
}
# Written by `download_site()` in R/download.R after each fetch.
REMOTE_ID_FILE = ".remote_id"
# fluxnet-shuttle snapshots to keep in data-raw/ once a fresh one is written.
KEEP_SNAPSHOTS = 3


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("-s", "--sites", nargs="+", help="Limit to these site IDs.")
    parser.add_argument(
        "-p", "--products", nargs="+", default=list(LOCAL),
        help="Which products to check (default: all).",
    )
    parser.add_argument(
        "--refresh-snapshot", action="store_true",
        help="Fetch a current fluxnet-shuttle snapshot before comparing.",
    )
    parser.add_argument(
        "--catalog", type=Path,
        help="Write the remote catalogue (site_ID,product,remote_id) here and exit.",
    )
    return parser.parse_args()


def local_archive(site: str, product: str) -> str | None:
    subdir, pattern = LOCAL[product]
    # FLUXNET archives land flat in data-raw/FLUXNET; everything else per site.
    candidates = sorted(
        list((RAW / subdir / site).glob(pattern)) + list(RAW.joinpath(subdir).glob(f"*_{site}_*.zip"))
    )
    return candidates[-1].name if candidates else None


def local_id(site: str, product: str) -> str | None:
    """The remote id we last fetched, else the archive name standing in for it."""
    sidecar = RAW / LOCAL[product][0] / site / REMOTE_ID_FILE
    if sidecar.exists():
        return sidecar.read_text().strip() or None
    return local_archive(site, product)


def newest_snapshot() -> Path | None:
    snaps = sorted(RAW.glob("fluxnet_shuttle_snapshot_*.csv"))
    return snaps[-1] if snaps else None


def refresh_snapshot() -> None:
    subprocess.run(
        ["fluxnet-shuttle", "listall", "-o", str(RAW)],
        check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
    )
    for old in sorted(RAW.glob("fluxnet_shuttle_snapshot_*.csv"))[:-KEEP_SNAPSHOTS]:
        old.unlink()


def snapshot_age_days(path: Path) -> int | None:
    match = re.search(r"(\d{8})T", path.name)
    if not match:
        return None
    stamp = dt.datetime.strptime(match.group(1), "%Y%m%d").date()
    return (dt.date.today() - stamp).days


def get_json(url: str, expect: type, **kwargs):
    """GET `url` as JSON of type `expect`, retrying transient failures.

    The AmeriFlux endpoint has been seen to answer 200 with a JSON *object* --
    an error envelope -- in place of its usual list, and then the list again a
    minute later. That is retried like a network error rather than parsed.
    """
    last = None
    for attempt in range(3):
        if attempt:
            time.sleep(5 * attempt)
        try:
            resp = requests.get(url, timeout=120, **kwargs)
            resp.raise_for_status()
            data = resp.json()
            if isinstance(data, expect):
                return data
            last = RuntimeError(f"expected a JSON {expect.__name__}, got: {str(data)[:200]}")
        except (requests.RequestException, ValueError) as exc:
            last = exc
    raise last


# ------------------------------------------------------------ remote lookups
#
# Each returns {site_ID: remote_id} for every site the provider knows about,
# in as few requests as the provider allows, so a scan of the whole grid costs
# a handful of round trips rather than one per site.

def remote_icos(sites: list[str]) -> dict[str, str]:
    from icoscp_core.icos import meta

    out = {}
    for obj in meta.list_data_objects(datatype=ETC_L2_FLUXNET):
        site = obj.station_uri.rsplit("/", 1)[-1].removeprefix("ES_")
        # One current object per station; should that ever change, the latest
        # submission is the one a fresh download would fetch.
        if site not in out or obj.submission_time > out[site][1]:
            out[site] = (obj.filename, obj.submission_time)
    return {k: v[0] for k, v in out.items()}


def remote_ww2020(sites: list[str]) -> dict[str, str]:
    members = get_json(WW2020_COLLECTION, dict, headers={"Accept": "application/json"})["members"]
    out = {}
    for m in members:
        g = re.match(r"FLX_([A-Za-z0-9\-]+)_FLUXNET2015_FULLSET_", m["name"])
        if g:
            out[g.group(1)] = m["name"]
    return out


def remote_fluxnet(sites: list[str]) -> dict[str, str]:
    snapshot = newest_snapshot()
    if snapshot is None:
        raise RuntimeError("no fluxnet-shuttle snapshot in data-raw/")
    shuttle = pd.read_csv(snapshot)
    return dict(zip(shuttle["site_id"], shuttle["fluxnet_product_name"]))


def remote_ameriflux(sites: list[str]) -> dict[str, str]:
    out = {}
    for row in get_json(AMF_AVAILABILITY, list):
        years = row.get("publish_years") or []
        if years:
            out[row["SITE_ID"]] = f"years:{min(years)}-{max(years)}:{len(years)}"
    return out


def remote_tern(sites: list[str]) -> dict[str, str]:
    import terndata.flux as flux

    mapping = pd.read_csv(Path("data-core") / "tern_fluxnet_site_mapping.csv")
    mapping = mapping.loc[mapping["fluxnet_site"].isin(sites)]
    return {
        row.fluxnet_site: max(flux.get_versions(row.tern_site))
        for row in mapping.itertuples()
    }


def remote_fluxnet2015(sites: list[str]) -> dict[str, str]:
    # A closed 2020 release with no service behind it, and the shuttle does not
    # carry it -- comparing against the shuttle snapshot would report a
    # spurious UPDATE forever. What we hold is, by definition, current.
    return {s: local_id(s, "FLUXNET2015") for s in sites if local_id(s, "FLUXNET2015")}


REMOTE = {
    "ICOS": remote_icos,
    "WW2020": remote_ww2020,
    "FLUXNET": remote_fluxnet,
    "AmeriFlux_BASE": remote_ameriflux,
    "TERN": remote_tern,
    "FLUXNET2015": remote_fluxnet2015,
}


def wanted_pairs(args: argparse.Namespace) -> list[tuple[str, str]]:
    site_info = pd.read_csv(Path("data-core") / "site_info.csv")
    pairs = []
    for _, site_row in site_info.iterrows():
        site = site_row["site_ID"]
        if args.sites and site not in args.sites:
            continue
        for product in [p.strip() for p in str(site_row["source"]).split("+")]:
            if product in args.products:
                pairs.append((site, product))
    return pairs


def lookup_all(pairs: list[tuple[str, str]]) -> tuple[dict[str, dict[str, str]], dict[str, str]]:
    """Remote ids per product, and the products whose lookup failed (and why)."""
    remote, failed = {}, {}
    for product in sorted({p for _, p in pairs}):
        sites = [s for s, p in pairs if p == product]
        try:
            remote[product] = REMOTE[product](sites)
        except Exception as exc:  # provider trouble, not our bug
            failed[product] = f"{type(exc).__name__}: {exc}"
    return remote, failed


def write_catalog(path: Path, pairs, remote, failed) -> int:
    """Write the catalogue to `path`, touching the file only if it changed.

    Leaving an unchanged file alone is the point: the pipeline tracks it, and
    a no-change scan must leave `tar_outdated()` empty. For the same reason a
    product whose lookup failed keeps the remote_id it had in the previous
    catalogue instead of going blank for a day and back -- which would look
    like two changes. Rows for sites not scanned this time (`--sites`) are
    carried over as they were.
    """
    previous = {}
    old = None
    if path.exists():
        old = pd.read_csv(path, dtype=str, keep_default_na=False)
        previous = {(r.site_ID, r.product): r.remote_id for r in old.itertuples()}

    rows = {}
    for site, product in pairs:
        if product in failed:
            rid = previous.get((site, product), "")
        else:
            rid = remote.get(product, {}).get(site, "")
        rows[(site, product)] = rid
    for key, rid in previous.items():
        rows.setdefault(key, rid)
    for product, why in failed.items():
        print(f"! {product} lookup failed, previous ids kept: {why}", file=sys.stderr)

    new = pd.DataFrame(
        [{"site_ID": s, "product": p, "remote_id": r} for (s, p), r in sorted(rows.items())],
        columns=["site_ID", "product", "remote_id"],
    )
    if old is not None and new.equals(old.reset_index(drop=True)):
        print(f"{path}: no change.", file=sys.stderr)
        return 0
    if old is not None:
        before = set(previous.items())
        changed = sorted({k for k, v in rows.items() if (k, v) not in before})
        for site, product in changed:
            print(f"  changed  {site:8s} {product:14s} {previous.get((site, product), '') or '-'}"
                  f" -> {rows[(site, product)] or '-'}", file=sys.stderr)
    tmp = path.with_suffix(path.suffix + ".part")
    new.to_csv(tmp, index=False)
    tmp.replace(path)
    print(f"{path}: written.", file=sys.stderr)
    return 0


def main() -> int:
    args = parse_args()
    pairs = wanted_pairs(args)
    needs_shuttle = any(p == "FLUXNET" for _, p in pairs)

    if needs_shuttle and (args.refresh_snapshot or args.catalog):
        print("Refreshing fluxnet-shuttle snapshot...", file=sys.stderr)
        try:
            refresh_snapshot()
        except Exception as exc:
            # The FLUXNET lookup then falls back to the newest snapshot on
            # disk, or fails and is left blank -- either is safe.
            print(f"! snapshot refresh failed: {exc}", file=sys.stderr)

    remote, failed = lookup_all(pairs)

    if args.catalog:
        return write_catalog(args.catalog, pairs, remote, failed)

    snapshot_path = newest_snapshot()
    if needs_shuttle:
        if snapshot_path is None:
            print("! No fluxnet-shuttle snapshot in data-raw/; FLUXNET cannot be checked.")
            print("  Re-run with --refresh-snapshot.\n")
        else:
            age = snapshot_age_days(snapshot_path)
            note = f"{age} days old" if age is not None else "age unknown"
            flag = "  <-- stale, re-run with --refresh-snapshot" if (age or 0) > 45 else ""
            print(f"fluxnet-shuttle snapshot: {snapshot_path.name} ({note}){flag}\n")

    rows = []
    for site, product in pairs:
        if product in failed:
            rows.append((site, product, "?", f"lookup failed: {failed[product]}"))
            continue
        want = remote.get(product, {}).get(site)
        have = local_id(site, product)
        if want is None:
            rows.append((site, product, "?", "not published / not queryable"))
        elif local_archive(site, product) is None:
            rows.append((site, product, "MISSING", f"-> {want}"))
        elif have == want:
            rows.append((site, product, "ok", have))
        else:
            rows.append((site, product, "UPDATE", f"{have} -> {want}"))

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
