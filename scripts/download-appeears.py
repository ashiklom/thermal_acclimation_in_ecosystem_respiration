#!/usr/bin/env -S uv run --script
#
# /// script
# requires-python = ">=3.13"
# dependencies = [
#   "pandas",
#   "requests",
# ]
# ///
"""Submit and download the AppEEARS point extraction that 03_01 needs.

`03_01_prepare_data_for_driver_analysis.R` reads NDVI/EVI/LAI/Fpar/GPP from
three CSVs that AppEEARS produces for a single "point" task requesting all
three products together (docs/data-provenance.md has the background). There
is no downloader for this because AppEEARS is an asynchronous submit/poll/
download API, not a file server, and its login endpoint
(`/api/login`, username/password) is currently broken. So this script skips
login entirely and authenticates with the `appeears_token` already sitting in
`_creds.toml` -- get a fresh one from a logged-in AppEEARS browser session via
Developer tools -> Application -> Session storage -> session -> token if the
one there has expired (AppEEARS tokens are short-lived).
"""

import argparse
import sys
import time
from pathlib import Path

import pandas as pd
import requests
import tomllib

API = "https://appeears.earthdatacloud.nasa.gov/api"

TASK_NAME = "towers"

# product -> (AppEEARS layer names, output CSV expected by 03_01)
PRODUCTS = {
    "MOD13A2.061": {
        "layers": ["_1_km_16_days_NDVI", "_1_km_16_days_EVI"],
        "result_csv": "towers-MOD13A2-061-results.csv",
    },
    "MOD15A2H.061": {
        "layers": ["Fpar_500m", "Lai_500m"],
        "result_csv": "towers-MOD15A2H-061-results.csv",
    },
    "MYD17A2HGF.061": {
        "layers": ["Gpp_500m"],
        "result_csv": "towers-MYD17A2HGF-061-results.csv",
    },
}

START_DATE = "01-01-2000"
END_DATE = "07-01-2026"


def load_token() -> str:
    with open("_creds.toml", "rb") as f:
        creds = tomllib.load(f)
    return creds["appeears_token"]


def auth_headers(token: str) -> dict:
    return {"Authorization": f"Bearer {token}"}


def build_task() -> dict:
    site_info = pd.read_csv("data-core/site_info.csv")
    coordinates = [
        {"id": row.site_ID, "latitude": row.LAT, "longitude": row.LONG}
        for row in site_info.itertuples()
    ]
    layers = [
        {"product": product, "layer": layer}
        for product, spec in PRODUCTS.items()
        for layer in spec["layers"]
    ]
    return {
        "task_type": "point",
        "task_name": TASK_NAME,
        "params": {
            "dates": [{"startDate": START_DATE, "endDate": END_DATE}],
            "layers": layers,
            "coordinates": coordinates,
        },
    }


def submit_task(token: str) -> str:
    resp = requests.post(
        f"{API}/task", json=build_task(), headers=auth_headers(token)
    )
    resp.raise_for_status()
    task_id = resp.json()["task_id"]
    print(f"submitted task {task_id}")
    return task_id


def wait_for_task(token: str, task_id: str, poll_seconds: int, timeout_seconds: int) -> None:
    deadline = time.monotonic() + timeout_seconds
    while True:
        # /api/status/{id} reports per-step progress but no overall status;
        # /api/task/{id} is the one that carries status in
        # {"processing", "done", "error", "expired", ...}.
        resp = requests.get(f"{API}/task/{task_id}", headers=auth_headers(token))
        resp.raise_for_status()
        status = resp.json()["status"]
        print(f"task {task_id}: {status}")
        if status == "done":
            return
        if status in ("error", "expired"):
            raise RuntimeError(f"AppEEARS task {task_id} ended with status '{status}'")
        if time.monotonic() > deadline:
            raise TimeoutError(
                f"task {task_id} did not finish within {timeout_seconds}s; "
                f"rerun with --task-id {task_id} to keep waiting"
            )
        time.sleep(poll_seconds)


def download_results(token: str, task_id: str, out_dir: Path) -> None:
    resp = requests.get(f"{API}/bundle/{task_id}", headers=auth_headers(token))
    resp.raise_for_status()
    files = {f["file_name"]: f["file_id"] for f in resp.json()["files"]}

    out_dir.mkdir(parents=True, exist_ok=True)
    for spec in PRODUCTS.values():
        name = spec["result_csv"]
        if name not in files:
            print(f"WARNING: {name} not found in task bundle, skipping", file=sys.stderr)
            continue
        file_resp = requests.get(
            f"{API}/bundle/{task_id}/{files[name]}",
            headers=auth_headers(token),
            stream=True,
        )
        file_resp.raise_for_status()
        dest = out_dir / name
        with open(dest, "wb") as f:
            for chunk in file_resp.iter_content(chunk_size=1 << 16):
                f.write(chunk)
        print(f"wrote {dest}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--task-id",
        help="Resume polling/downloading an already-submitted task instead of submitting a new one",
    )
    parser.add_argument("--poll-seconds", type=int, default=30)
    parser.add_argument("--timeout-seconds", type=int, default=3600)
    parser.add_argument("--out-dir", type=Path, default=Path("data-raw"))
    args = parser.parse_args()

    token = load_token()
    task_id = args.task_id or submit_task(token)
    wait_for_task(token, task_id, args.poll_seconds, args.timeout_seconds)
    download_results(token, task_id, args.out_dir)


if __name__ == "__main__":
    main()
