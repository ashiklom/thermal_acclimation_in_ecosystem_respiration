#!/usr/bin/env python

"""Submit, track, and download the AppEEARS point extraction that 03_01 needs.

AppEEARS tasks for this request take days to process, so this is split into
subcommands rather than blocking: `submit` returns immediately, and `status` /
`download` can be run whenever. Without --task-id, `status` and `download` act
on the most recent task named "towers".

Authenticates with `appeears_token` from `_creds.toml` because the AppEEARS
login endpoint is broken. The token is short-lived; refresh it from a logged-in
browser session (Developer tools -> Application -> Session storage -> session
-> token).
"""

import argparse
import json
import sys
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


class AppEEARS:
    def __init__(self, token: str):
        self.session = requests.Session()
        self.session.headers["Authorization"] = f"Bearer {token}"

    def get(self, path: str, **kwargs) -> requests.Response:
        resp = self.session.get(f"{API}/{path}", **kwargs)
        resp.raise_for_status()
        return resp

    def post(self, path: str, **kwargs) -> requests.Response:
        resp = self.session.post(f"{API}/{path}", **kwargs)
        resp.raise_for_status()
        return resp


def load_token() -> str:
    with open("_creds.toml", "rb") as f:
        creds = tomllib.load(f)
    return creds["appeears_token"]


def print_json(obj) -> None:
    print(json.dumps(obj, indent=2))


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


def summarize_task(task: dict) -> dict:
    # Drops the full coordinate list, which is 117 entries of noise.
    params = task.get("params", {})
    summary = {k: v for k, v in task.items() if k != "params"}
    summary["dates"] = params.get("dates")
    summary["layers"] = params.get("layers")
    summary["n_coordinates"] = len(params.get("coordinates", []))
    return summary


def list_tasks(api: AppEEARS, limit: int) -> list[dict]:
    tasks = api.get("task", params={"limit": limit}).json()
    return sorted(tasks, key=lambda t: t["created"], reverse=True)


def resolve_task_id(api: AppEEARS, task_id: str | None) -> str:
    if task_id:
        return task_id
    towers = [t for t in list_tasks(api, limit=100) if t["task_name"] == TASK_NAME]
    if not towers:
        sys.exit(f"no AppEEARS tasks named '{TASK_NAME}' found; run `submit` first")
    return towers[0]["task_id"]


def cmd_submit(api: AppEEARS, args) -> None:
    task = build_task()
    resp = api.post("task", json=task).json()
    print_json({**resp, **summarize_task(task)})


def cmd_status(api: AppEEARS, args) -> None:
    task_id = resolve_task_id(api, args.task_id)
    summary = summarize_task(api.get(f"task/{task_id}").json())
    if summary["status"] != "done":
        # Per-step progress lives on a separate endpoint from the task status.
        summary["progress"] = api.get(f"status/{task_id}").json().get("progress")
    print_json(summary)


def cmd_list(api: AppEEARS, args) -> None:
    print_json([
        {k: t.get(k) for k in ("task_id", "task_name", "task_type", "status", "created", "updated")}
        for t in list_tasks(api, args.limit)
    ])


def cmd_download(api: AppEEARS, args) -> None:
    task_id = resolve_task_id(api, args.task_id)
    status = api.get(f"task/{task_id}").json()["status"]
    if status != "done":
        sys.exit(f"task {task_id} is '{status}', not 'done'; nothing to download yet")

    files = {f["file_name"]: f["file_id"] for f in api.get(f"bundle/{task_id}").json()["files"]}
    expected = [spec["result_csv"] for spec in PRODUCTS.values()]
    missing = [name for name in expected if name not in files]
    if missing:
        sys.exit(f"task {task_id} bundle is missing {', '.join(missing)}")

    args.out_dir.mkdir(parents=True, exist_ok=True)
    for name in expected:
        dest = args.out_dir / name
        resp = api.get(f"bundle/{task_id}/{files[name]}", stream=True)
        with open(dest, "wb") as f:
            for chunk in resp.iter_content(chunk_size=1 << 16):
                f.write(chunk)
        print(f"wrote {dest}")


def main() -> None:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    sub = parser.add_subparsers(dest="command", required=True)

    sub.add_parser("submit", help="Submit a new point task and print its ID as JSON")

    p = sub.add_parser("status", help="Print status of a task (default: most recent) as JSON")
    p.add_argument("--task-id")

    p = sub.add_parser("list", help="Print one page of recent tasks as JSON")
    p.add_argument("--limit", type=int, default=10)

    p = sub.add_parser("download", help="Download a finished task's CSVs (default: most recent)")
    p.add_argument("--task-id")
    p.add_argument("--out-dir", type=Path, default=Path("data-raw"))

    args = parser.parse_args()
    api = AppEEARS(load_token())
    {
        "submit": cmd_submit,
        "status": cmd_status,
        "list": cmd_list,
        "download": cmd_download,
    }[args.command](api, args)


if __name__ == "__main__":
    main()
