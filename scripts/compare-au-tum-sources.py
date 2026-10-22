"""Compare AU-Tum's flux records from three processing streams (refactor-v2
stage S18), for the analysis website's AU-Tum page.

Streams (the NEE column each one gives step 01):
  F15_VUT   FLUXNET2015 (ONEFlux, 2001-2014), NEE_VUT_REF: the published
            record the pipeline uses
  F15_CUT   the same file's NEE_CUT_REF (constant u* threshold): the u*
            method alone
  SHUT_CUT  the FLUXNET-archive shuttle's TERN product (ONEFlux v1.3,
            2002-2022), NEE_CUT_REF (it has no NEE_VUT_REF)
  TERN      TERN's L6 product (PyFluxPro, 2002-2026), Fco2, as
            scripts/download-tern.py writes it (NEE_VUT_REF there)

Each stream's night-time respiration records are selected as step 01 does
(R/step01-prep.R, night_ok): air temperature present, NEE an observation
(QC 0), NIGHT, and -5 < NEE < 30 umol m-2 s-1.

Usage (from the repository root):
  python scripts/compare-au-tum-sources.py [--out data-proc/au-tum-sources.json]
Reads original-data/SiteData/FLUXNET2015/unzip/FLX_AU-Tum_FLUXNET2015_FULLSET_HR_*.csv,
data-raw/flux/SHUTTLE/unzip/TERN_AU-Tum_FLUXNET_FLUXMET_HR_*.csv and
data-raw/flux/TERN/unzip/TERN_AU-Tum_L6_HR_*.csv; writes one JSON file:
  streams   per stream: label, file, period, hours, night records
  annual    per stream and year: night records, their mean NEE and TS, the
            5% quantile of their u* (the effective u* filter), annual NEE
            (g C m-2, years with >= 95% of hours), mean TS and SWC, and an
            exponential fit of night NEE on TS (R10, Q10; >= 100 records)
  daily     per stream: date, mean NEE over all hours (gap-filled; complete
            days), mean night-record NEE and their number
  pairs     per pair of streams over their common years: agreement of the
            night records both select (n, bias, RMSE, r, slope) and of the
            drivers (TA, TS, SWC, USTAR; all hours), the records only one
            selects, per year, and a sample of the matched night records
  curves    per stream and period: night NEE by 1 degC TS bin
"""

import argparse
import glob
import json
import math
import pathlib

import numpy as np
import pandas as pd

ROOT = pathlib.Path(__file__).resolve().parent.parent
SEED = 20261022
G_C_PER_UMOL_HOUR = 12.011e-6 * 3600  # umol m-2 s-1 over one hour -> g C m-2

STREAMS = {
    "F15_VUT": ("FLUXNET2015 (ONEFlux), NEE_VUT_REF", "FLUXNET2015", "NEE_VUT_REF"),
    "F15_CUT": ("FLUXNET2015 (ONEFlux), NEE_CUT_REF", "FLUXNET2015", "NEE_CUT_REF"),
    "SHUT_CUT": ("Shuttle TERN product (ONEFlux v1.3), NEE_CUT_REF", "SHUTTLE", "NEE_CUT_REF"),
    "TERN": ("TERN L6 (PyFluxPro), Fco2", "TERN", "NEE_VUT_REF"),
}
FILES = {
    "FLUXNET2015": "../original-data/SiteData/FLUXNET2015/unzip/FLX_AU-Tum_FLUXNET2015_FULLSET_HR_*.csv",
    "SHUTTLE": "data-raw/flux/SHUTTLE/unzip/TERN_AU-Tum_FLUXNET_FLUXMET_HR_*.csv",
    "TERN": "data-raw/flux/TERN/unzip/TERN_AU-Tum_L6_HR_*.csv",
}
PAIRS = [("F15_VUT", "F15_CUT"), ("F15_CUT", "SHUT_CUT"), ("F15_VUT", "SHUT_CUT"), ("F15_VUT", "TERN"),
         ("SHUT_CUT", "TERN")]
DRIVERS = {"TA": "TA_F_MDS", "TS": "TS_F_MDS_1", "SWC": "SWC_F_MDS_1", "USTAR": "USTAR"}
PERIODS = {"2002-2014": (2002, 2014), "2015-2022": (2015, 2022), "2023-2026": (2023, 2026)}


def read_product(product):
    files = glob.glob(str(ROOT / FILES[product]))
    if len(files) != 1:
        raise SystemExit(f"{product}: expected one file matching {FILES[product]}, found {len(files)}")
    cols = ["TIMESTAMP_START", "NEE_VUT_REF", "NEE_VUT_REF_QC", "NEE_CUT_REF", "NEE_CUT_REF_QC", "NIGHT",
            *DRIVERS.values(), "TS_F_MDS_1_QC", "SWC_F_MDS_1_QC"]
    a = pd.read_csv(files[0], usecols=lambda c: c in cols).replace(-9999, np.nan)
    a.index = pd.to_datetime(a.pop("TIMESTAMP_START").astype(np.int64).astype(str), format="%Y%m%d%H%M")
    return a, pathlib.Path(files[0]).name


def stream_table(product_table, nee_col):
    a = product_table
    t = pd.DataFrame({"NEE": a[nee_col], "QC": a[f"{nee_col}_QC"], "NIGHT": a["NIGHT"]}, index=a.index)
    for k, c in DRIVERS.items():
        t[k] = a[c]
    t["night_ok"] = t.TA.notna() & (t.QC == 0) & (t.NIGHT == 1) & (t.NEE > -5) & (t.NEE < 30)
    return t


def r10_q10(night):
    """Exponential fit ln(NEE) = a + b TS on positive night NEE: R10 and Q10."""
    d = night[(night.NEE > 0) & night.TS.notna()]
    if len(d) < 100:
        return None, None
    b, a = np.polyfit(d.TS, np.log(d.NEE), 1)
    return math.exp(a + 10 * b), math.exp(10 * b)


def annual(t):
    rows = []
    for year, g in t.groupby(t.index.year):
        night = g[g.night_ok]
        complete = g.NEE.notna().mean() >= 0.95 and len(g) >= 0.95 * (8784 if year % 4 == 0 else 8760)
        r10, q10 = r10_q10(night)
        rows.append({"year": int(year), "n_night": int(len(night)),
                     "night_nee": night.NEE.mean() if len(night) else None,
                     "night_ts": night.TS.mean() if len(night) else None,
                     "ustar_p05": night.USTAR.quantile(0.05) if len(night) else None,
                     "nee_sum": g.NEE.sum() * G_C_PER_UMOL_HOUR if complete else None,
                     "ts_mean": g.TS.mean(), "swc_mean": g.SWC.mean(), "r10": r10, "q10": q10})
    return rows


def daily(t):
    by_day = t.groupby(t.index.normalize())
    nee = by_day.NEE.mean().where(by_day.NEE.count() == by_day.size())
    night = t[t.night_ok].groupby(t[t.night_ok].index.normalize()).NEE.agg(["mean", "count"])
    d = pd.DataFrame({"nee": nee}).join(night, how="outer")
    return {"date": d.index.strftime("%Y-%m-%d").tolist(), "nee": rnd(d.nee), "night": rnd(d["mean"]),
            "n_night": d["count"].fillna(0).astype(int).tolist()}


def agreement(x, y):
    ok = x.notna() & y.notna()
    x, y = x[ok], y[ok]
    if len(x) < 10:
        return {"n": int(len(x))}
    slope = np.polyfit(x, y, 1)[0] if x.std() > 0 else None
    return {"n": int(len(x)), "bias": float((y - x).mean()), "rmse": float(np.sqrt(((y - x) ** 2).mean())),
            "r": float(np.corrcoef(x, y)[0, 1]), "slope": float(slope) if slope is not None else None,
            "mean_x": float(x.mean()), "mean_y": float(y.mean())}


def pair(a, b, ta, tb, rng):
    years = sorted(set(ta.index.year) & set(tb.index.year))
    ta = ta[ta.index.year.isin(years)]
    tb = tb[tb.index.year.isin(years)]
    j = ta.join(tb, how="inner", lsuffix="_a", rsuffix="_b")
    both = j[j.night_ok_a & j.night_ok_b]
    sample = both.sample(min(3000, len(both)), random_state=rng) if len(both) else both
    per_year = (j.assign(only_a=j.night_ok_a & ~j.night_ok_b, only_b=~j.night_ok_a & j.night_ok_b,
                         both=j.night_ok_a & j.night_ok_b)
                .groupby(j.index.year)[["only_a", "only_b", "both"]].sum().reset_index(names="year"))
    return {"a": a, "b": b, "years": [int(min(years)), int(max(years))] if years else None,
            "night": agreement(both.NEE_a, both.NEE_b),
            "drivers": {k: agreement(j[f"{k}_a"], j[f"{k}_b"]) for k in DRIVERS},
            "selection": per_year.astype(int).to_dict(orient="list"),
            "sample": {"x": rnd(sample.NEE_a), "y": rnd(sample.NEE_b), "ts": rnd(sample.TS_a)}}


def curves(t):
    out = {}
    for name, (y0, y1) in PERIODS.items():
        night = t[t.night_ok & (t.index.year >= y0) & (t.index.year <= y1) & t.TS.notna()]
        if len(night) < 100:
            continue
        g = night.groupby(np.floor(night.TS)).NEE.agg(["mean", "count"])
        g = g[g["count"] >= 20]
        out[name] = {"ts": (g.index + 0.5).tolist(), "nee": rnd(g["mean"]), "n": g["count"].astype(int).tolist()}
    return out


def rnd(x, digits=3):
    return [None if pd.isna(v) else round(float(v), digits) for v in x]


def clean(obj):
    if isinstance(obj, dict):
        return {k: clean(v) for k, v in obj.items()}
    if isinstance(obj, list):
        return [clean(v) for v in obj]
    if isinstance(obj, (float, np.floating)):
        return None if math.isnan(obj) or math.isinf(obj) else round(float(obj), 5)
    if isinstance(obj, np.integer):
        return int(obj)
    return obj


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--out", default="data-proc/au-tum-sources.json")
    args = parser.parse_args()
    rng = np.random.RandomState(SEED)
    products = {p: read_product(p) for p in FILES}
    tables, streams = {}, {}
    for sid, (label, product, nee_col) in STREAMS.items():
        a, file = products[product]
        t = stream_table(a, nee_col)
        tables[sid] = t
        streams[sid] = {"label": label, "product": product, "file": file, "nee_column": nee_col,
                        "start": t.index.min().strftime("%Y-%m-%d"), "end": t.index.max().strftime("%Y-%m-%d"),
                        "hours": int(len(t)), "n_night": int(t.night_ok.sum())}
    out = {"streams": streams,
           "annual": {s: annual(t) for s, t in tables.items()},
           "daily": {s: daily(t) for s, t in tables.items()},
           "pairs": [pair(a, b, tables[a], tables[b], rng) for a, b in PAIRS],
           "curves": {s: curves(t) for s, t in tables.items()}}
    path = ROOT / args.out
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(clean(out), separators=(",", ":")))
    for p in out["pairs"]:
        n = p["night"]
        print(f"{p['a']:>8} vs {p['b']:<8} {p['years']}: night n={n.get('n')} bias={n.get('bias', float('nan')):.3f} "
              f"rmse={n.get('rmse', float('nan')):.3f} r={n.get('r', float('nan')):.3f}")
    print(f"wrote {path}")


if __name__ == "__main__":
    main()
