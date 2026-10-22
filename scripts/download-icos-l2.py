"""Latest ICOS ETC L2 FLUXNET-format products of the FLUXNET-format sites
(refactor-v2 stage S18), a fill source for the data update.

The ICOS Carbon Portal's current (not deprecated) "ETC L2 Fluxnet" data object
of each station (one per station; ONEFlux processing, starting at the
station's ICOS labelling) is downloaded into data-raw/flux/ICOSL2/ (the zip)
and its half-hourly FLUXMET table extracted into data-raw/flux/ICOSL2/unzip/,
replacing the site's earlier table there (R/read-raw.R reads it as product
"ICOSL2"). Needs internet access and an ICOS account login, stored once with
icoscp_core's interactive `auth.init_config_file()` (in ~/.icoscp); stops at
once without one.

Usage (from the repository root):
  python scripts/download-icos-l2.py [--sites A,B] [--out data-raw/flux/ICOSL2]

Writes <out>/download_manifest.csv: site_ID, file, time_start, time_end,
bytes, md5, downloaded (UTC), or the error (rows of sites not downloaded this
time are kept). Sites without an ICOS L2 object are listed as "not available".
"""

import argparse
import datetime
import hashlib
import pathlib
import re
import sys
import zipfile

import pandas as pd
from icoscp_core.icos import auth, data, meta

L2_FLUXNET = "http://meta.icos-cp.eu/resources/cpmeta/etcL2Fluxnet"


def md5(path):
    h = hashlib.md5()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def extract_table(zip_path, unzip_dir, site):
    """Extract the zip's FLUXMET HH (or HR) table, replacing the site's earlier one."""
    with zipfile.ZipFile(zip_path) as z:
        tables = [n for n in z.namelist() if re.search(r"_FLUXMET_(HH|HR)_.*\.csv$", n)]
        if len(tables) != 1:
            raise RuntimeError(f"{zip_path.name}: expected one FLUXMET HH/HR table, found {len(tables)}")
        unzip_dir.mkdir(parents=True, exist_ok=True)
        for old in unzip_dir.glob(f"ICOSETC_{site}_*.csv"):
            old.unlink()
        dest = unzip_dir / pathlib.Path(tables[0]).name
        with z.open(tables[0]) as src, open(dest, "wb") as out:
            out.write(src.read())
    return dest


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--sites", help="comma-separated site IDs (default: every FLUXNET-format site)")
    parser.add_argument("--out", default="data-raw/flux/ICOSL2")
    args = parser.parse_args()

    # fail early and loudly without a login
    try:
        auth.get_token()
    except Exception as e:  # noqa: BLE001
        sys.exit(f"No usable ICOS account login ({e}). Log in once with: "
                 "python -c 'from icoscp_core.icos import auth; auth.init_config_file()'")

    site_info = pd.read_csv("data/site_info.csv", encoding="utf-8-sig")
    sites = site_info.site_ID[site_info.source != "AmeriFlux_BASE"].tolist()
    if args.sites:
        wanted = args.sites.split(",")
        unknown = set(wanted) - set(sites)
        if unknown:
            sys.exit(f"not FLUXNET-format sites: {', '.join(sorted(unknown))}")
        sites = wanted

    objects = meta.list_data_objects(datatype=L2_FLUXNET, limit=100000)
    current = {}
    for o in objects:
        site = o.station_uri.rsplit("/", 1)[-1].removeprefix("ES_")
        if site not in current or o.submission_time > current[site].submission_time:
            current[site] = o

    out = pathlib.Path(args.out)
    out.mkdir(parents=True, exist_ok=True)
    rows = []
    for site in sites:
        row = {"site_ID": site, "file": None, "time_start": None, "time_end": None, "bytes": None, "md5": None,
               "downloaded": None, "error": None}
        o = current.get(site)
        if o is None:
            row["error"] = "not available"
            print(f"{site}: no ICOS L2 object", flush=True)
            rows.append(row)
            continue
        try:
            for old in out.glob(f"ICOSETC_{site}_*.zip"):
                old.unlink()
            zip_path = out / data.save_to_folder(o, str(out))
            table = extract_table(zip_path, out / "unzip", site)
            row.update(file=table.name, time_start=str(o.time_start), time_end=str(o.time_end),
                       bytes=table.stat().st_size, md5=md5(table),
                       downloaded=datetime.datetime.now(datetime.UTC).strftime("%Y-%m-%d %H:%M:%S UTC"))
            print(f"{site}: {table.name}", flush=True)
        except Exception as e:  # noqa: BLE001
            row["error"] = str(e)
            print(f"{site}: FAILED: {e}", flush=True)
        rows.append(row)

    manifest = pd.DataFrame(rows)
    path = out / "download_manifest.csv"
    if path.exists():
        earlier = pd.read_csv(path)
        manifest = pd.concat([earlier[~earlier.site_ID.isin(manifest.site_ID)], manifest])
    order = {s: i for i, s in enumerate(site_info.site_ID)}
    manifest = manifest.sort_values("site_ID", key=lambda s: s.map(order))
    manifest.to_csv(path, index=False)
    print(f"{manifest.file.notna().sum()} of {len(manifest)} sites downloaded")


if __name__ == "__main__":
    main()
