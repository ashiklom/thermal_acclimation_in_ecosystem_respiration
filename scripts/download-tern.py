"""TERN (OzFlux) L6 flux-tower data in FLUXNET format (refactor-v2 stage S18),
to extend AU-Tum past its ONEFlux records.

Downloads a site's L6 dataset (PyFluxPro processing, TERN's own; hourly at
AU-Tum) with terndata.flux into data-raw/flux/TERN/ (netCDF, as served) and
writes it as a FLUXNET-format table into data-raw/flux/TERN/unzip/, which
R/read-raw.R reads as product "TERN". Needs internet access.

How TERN's variables become FLUXNET columns. These are not ONEFlux products:
the names only say which step-01 input each one stands in for.
  TIMESTAMP_START/END  TERN's time marks the end of the period, in local
                       standard time like FLUXNET's TIMESTAMP_END (checked
                       against FLUXNET2015 at AU-Tum: air temperature and
                       radiation align at lag 0)
  NEE_VUT_REF          Fco2, the CO2 flux (u*-filtered and gap-filled by
                       PyFluxPro), umol m-2 s-1
  NEE_VUT_REF_QC       0 where Fco2 is an observation that passed TERN's
                       checks (QC flag 0), else 3 (gap-filled: never used
                       as an observation)
  TA_F_MDS, TS_F_MDS_1, SWC_F_MDS_1, SW_IN_F_MDS, VPD_F_MDS, H_F_MDS, LE_F_MDS
                       Ta, Ts (the 2 cm sensor at AU-Tum), Sws x 100 (%),
                       Fsd, VPD x 10 (hPa), Fh, Fe; their _QC is 0 for
                       observations (TERN flag 0), 2 for TERN's gap fill
  NETRAD, USTAR, RH, P Fn, ustar, RH, Precip
  SW_IN_POT, NIGHT     top-of-atmosphere shortwave radiation averaged over
                       the period (NOAA solar geometry, minute steps), and
                       NIGHT = SW_IN_POT == 0, ONEFlux's definition (agrees
                       with FLUXNET2015's NIGHT at AU-Tum in all but 82 of
                       122,712 dawn/dusk hours)
  TERN_Fco2_QCFlag, TERN_Sco2, TERN_ER
                       TERN's own flux flag, storage term and modelled
                       ecosystem respiration, kept for comparisons
Missing values are -9999, as in FLUXNET files.

Usage (from the repository root):
  python scripts/download-tern.py --site AU-Tum [--version 2026_v2] [--out data-raw/flux/TERN]
"""

import argparse
import datetime
import pathlib
import sys
import zoneinfo

import numpy as np
import pandas as pd
import terndata.flux as tern

# FLUXNET site ID -> TERN site name
TERN_SITES = {"AU-Tum": "Tumbarumba"}
# TERN variable -> (FLUXNET column, scale, has a gap-fill QC column)
MET = {
    "Ta": ("TA_F_MDS", 1, True),
    "Ts": ("TS_F_MDS_1", 1, True),
    "Sws": ("SWC_F_MDS_1", 100, True),
    "Fsd": ("SW_IN_F_MDS", 1, True),
    "Fh": ("H_F_MDS", 1, True),
    "Fe": ("LE_F_MDS", 1, True),
    "VPD": ("VPD_F_MDS", 10, True),
    "Fn": ("NETRAD", 1, False),
    "ustar": ("USTAR", 1, False),
    "RH": ("RH", 1, False),
    "Precip": ("P", 1, False),
}


def sw_in_pot(start, lat, lon, utc_offset, step_min):
    """Top-of-atmosphere shortwave radiation (W m-2) averaged over each period.

    `start` are the periods' starts in local standard time (UTC + utc_offset
    hours); sampled at the middle of each minute.
    """
    out = np.zeros(len(start))
    for k in range(step_min):
        t = start + pd.to_timedelta(k + 0.5, unit="min")
        doy = t.dayofyear.values
        hour = t.hour.values + t.minute.values / 60 + t.second.values / 3600
        g = 2 * np.pi / 365 * (doy - 1 + (hour - 12) / 24)
        decl = (0.006918 - 0.399912 * np.cos(g) + 0.070257 * np.sin(g) - 0.006758 * np.cos(2 * g)
                + 0.000907 * np.sin(2 * g) - 0.002697 * np.cos(3 * g) + 0.00148 * np.sin(3 * g))
        eot = 229.18 * (0.000075 + 0.001868 * np.cos(g) - 0.032077 * np.sin(g) - 0.014615 * np.cos(2 * g)
                        - 0.040849 * np.sin(2 * g))
        solar_time = hour * 60 + eot + 4 * lon - 60 * utc_offset
        hour_angle = np.radians(solar_time / 4 - 180)
        phi = np.radians(lat)
        cos_zenith = np.sin(phi) * np.sin(decl) + np.cos(phi) * np.cos(decl) * np.cos(hour_angle)
        out += np.clip(1367 * (1 + 0.033 * np.cos(2 * np.pi * doy / 365)) * cos_zenith, 0, None)
    return out / step_min


def fluxnet_table(ds):
    """TERN's L6 dataset as a FLUXNET-format data frame (see the module docstring)."""
    step = int(ds.attrs["time_step"])
    values = {v: ds[v].values.ravel() for v in ds.data_vars if ds[v].dims[:1] == ("time",)}
    t = pd.DataFrame(values, index=pd.DatetimeIndex(ds.time.values))
    t = t[~t.index.duplicated()].sort_index()
    end = pd.date_range(t.index[0], t.index[-1], freq=f"{step}min")
    t = t.reindex(end)
    start = end - pd.Timedelta(minutes=step)
    # standard (not daylight-saving) time: the zone's offset in its winter
    zone = zoneinfo.ZoneInfo(ds.attrs["time_zone"])
    tz = min(datetime.datetime(2001, m, 1, tzinfo=zone).utcoffset().total_seconds() for m in (1, 7)) / 3600

    out = pd.DataFrame({"TIMESTAMP_START": start.strftime("%Y%m%d%H%M").astype(np.int64),
                        "TIMESTAMP_END": end.strftime("%Y%m%d%H%M").astype(np.int64)})
    observed = lambda v: (t[f"{v}_QCFlag"] == 0).values  # noqa: E731
    out["NEE_VUT_REF"] = t["Fco2"].values
    out["NEE_VUT_REF_QC"] = np.where(observed("Fco2"), 0, 3)
    for v, (col, scale, qc) in MET.items():
        out[col] = t[v].values * scale
        if qc:
            out[f"{col}_QC"] = np.where(observed(v), 0, 2)
    out["SW_IN_POT"] = sw_in_pot(start, float(ds.attrs["latitude"]), float(ds.attrs["longitude"]), tz, step)
    out["NIGHT"] = (out["SW_IN_POT"] == 0).astype(int)
    out["TERN_Fco2_QCFlag"] = t["Fco2_QCFlag"].values
    out["TERN_Sco2"] = t["Sco2_single"].values
    out["TERN_ER"] = t["ER"].values
    for col in out.columns:
        if col.endswith("_QC"):
            out.loc[out[col.removesuffix("_QC")].isna(), col] = np.nan
    return out.fillna(-9999), step


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--site", required=True, choices=sorted(TERN_SITES))
    parser.add_argument("--version", help="TERN dataset version (default: the latest)")
    parser.add_argument("--out", default="data-raw/flux/TERN")
    args = parser.parse_args()

    name = TERN_SITES[args.site]
    version = args.version or tern.get_versions(name)[-1]
    out = pathlib.Path(args.out)
    (out / "unzip").mkdir(parents=True, exist_ok=True)
    ds = tern.get_l6_dataset(name, version, "30min", missing_as_nan=True)  # "30min": the native time step
    if ds.attrs.get("fluxnet_id") != args.site:
        sys.exit(f"TERN dataset {name} is {ds.attrs.get('fluxnet_id')}, not {args.site}")
    nc = out / f"TERN_{args.site}_L6_{version}.nc"
    ds.to_netcdf(nc)
    table, step = fluxnet_table(ds)
    years = table.TIMESTAMP_START // 10**8
    res = {30: "HH", 60: "HR"}[step]
    for old in (out / "unzip").glob(f"TERN_{args.site}_L6_*.csv"):
        old.unlink()
    csv = out / "unzip" / f"TERN_{args.site}_L6_{res}_{years.min()}-{years.max()}_{version}.csv"
    table.to_csv(csv, index=False, float_format="%.6g")
    print(f"{args.site}: TERN {name} {version}: {nc.name}, {csv.name} ({len(table)} records)")


if __name__ == "__main__":
    main()
