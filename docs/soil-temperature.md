# Soil temperature: how the column the model sees comes to exist

Soil temperature reaches the respiration model through **two stages**, and the
manuscript's logic requires that they stay two. Both live in
`R/soil-temperature.R`; the estimators they share are in `R/ts-estimators.R`.

```
reader ─► stage A ─► QC · growing season · year gap scan ─► stage B ─► model
          TS_measured                                       TS_final
          (qualification-facing)                            (fit-facing)
```

## Why two stages

The manuscript qualified years — the QC filter, the growing-season detector,
the gap scan — on one soil-temperature column, and then, at 35 sites, fitted a
`TS ~ TA` regression **on the years that scan had qualified** and fitted the
model on the regression instead. So the column that decides which years are
trustworthy is not the column the model is fitted on, and it cannot be:
stage B's regression needs stage A's qualification to exist first. A single
"decide soil temperature once, before everything" step would change the
regression's training set and the year selection at those 35 sites, and would
not reproduce the manuscript.

Making the two stages explicit — two named columns, two functions, one
provenance trail — is the tidying. Downstream of stage B there is exactly one
soil-temperature column and no branching on site, origin or strategy.

## Stage A — `qualification_soil_temperature()`

**In:** the record's columns under shared names (`fluxnet_ts_input()`,
`ameriflux_ts_input()` — the readers' only job). **Out:** `TS`, `TS_QC`, and a
one-row provenance table.

Which arm runs is **`ts_source` in `site_info.csv`** — one level per mechanism
the manuscript applied, derived in `scripts/revise-site-info.R`:

| `ts_source` | sites | what stage A does | write-back | truth? |
|---|---|---|---|---|
| `sensor` | 86 | the declared sensor, untouched | — | sensor |
| `sensor_depth2` | CZ-Stn | the second depth, wholesale | replace | sensor |
| `gapfill_pi` | US-NR1, US-ICh, US-ICs | the PI's gap-filled product, in the sensor's gaps only | fill_gaps | sensor |
| `recalibrated` | FI-Sod | pre-2006 rebuilt by chaining two depth-to-depth regressions (`recalibrate_fi_sod_soil_temp()`) | overlay | none |
| `gapfill_ta` | US-MBP | `TA × 0.369 + 5.87`, in the gaps only | fill_gaps | none |
| `ta_substitute` | GF-Guy | air temperature | replace | none |
| `borrowed_site` | US-Cwt | `TA × 0.647 + 5.14`, a neighbour's coefficients | replace | none |
| `lm_ta_recent` | US-BZo | `lm_ta_recent`: `TS ~ TA` on years > 2021 above freezing | replace | none |
| `lm_ta_cold` | 6 AmeriFlux | `lm_ta_pos`: `TS ~ TA` above freezing | replace | none |
| `reconstructed` | 16 (`estimate_Ts`) | `fix_soil_temp()`: the estimator `estimate_ts_method` names, predictors gap-filled by DOY × time climatology | replace | none |

`TS_SOURCES` in `R/constants.R` is that last column: whether *every* row of the
column is a sensor reading. It is deliberately strict — FI-Sod and US-MBP are
"none" because part of their column is derived — and it is what the refuse
rule (below) and `ts_measured_is_synthetic()` read.

The only site names left inside stage A are the manuscript's three
training-target rules in the `reconstructed` arm: DE-Hte trains on the second
depth, FR-Bil discards soil temperature before 2021, FR-Pue before 2016.

Stage A's provenance row (`site_data$ts_provenance`; `ts_provenance.csv` in
the run) records `ts_source`, `ts_truth`, `stage_a_estimator`,
`stage_a_family`, `stage_a_mode`, `stage_a_n_train`, and a note.

## Stage B — `get_soil_temperature()`

**In:** `site_data`, the recipe, the per-site fill. **Out:** `ac` and `nightNEE`
with **`TS_final` as their only soil-temperature column**, and a metadata row.

It selects a candidate under the recipe's `ts` strategy (`choose_ts_col()`),
attaches `TS_memfill` if that is what was chosen, looks up the bounds
belonging to the selected column under the recipe's `bounds` strategy, and
drops every other candidate — `TS`, `TS_measured`, `TS_linear`, `TS_memfill` —
so that "no branching downstream" is a checkable property rather than a
convention. The metadata row carries everything the settings table reports
about soil temperature, and copies stage A's provenance fields in.

**No second method where there is no measured truth.** Where `ts_source`
leaves rows that are not a sensor reading, every variant alternative to
`TS_measured` is a model fitted *to* it — `TS_linear` regresses it on air
temperature, `TS_memfill` is cross-validated against it — and either returns
a function of the same predictors wearing a skill score. So under `screen_best`
and `memory_fill`, stage B keeps `TS_measured` at those 27 sites, records
`ts_refused = TRUE`, and `fill_soil_temp()` declines before doing any work.
The manuscript's `site_info` strategy is exempt: `ts_col` there is a
declaration, and its two double-applications (FI-Sod, US-MBP) are the
manuscript's own.

## The shared parts

- **`ts_estimators()`** (`R/ts-estimators.R`): every model that produces a
  soil-temperature estimate, one shape — `family`, `predictors`, `fit`,
  `predict`. The manuscript's are `lm_ta`, `lm_ta_pos`, `lm_ta_recent`,
  `lm_ta_netrad` and `rf_ta_netrad_manuscript` (workflow 01_01's
  `randomForest`, 60 k rows, 70/30); the fill's are the `ranger` forests and
  the memory-feature models. Stage A and the fill draw from the same list.
- **`write_back_ts(ts, estimate, mode)`**: `replace`, `overlay` or
  `fill_gaps`. No default; the mode is the point.
- **`ts_bounds_rows()`**: both bounds definitions for any candidate column.

## What is the manuscript and what is a variant

Under the `original` recipe nothing in this design changes a number:
`tests/ts-swc-baseline.R` freezes step-01 digests and the transcribed step-02
substitution at 20 sites — one per stage-A arm — and the pipeline must
reproduce them. Everything a variant does differently is a `switch()` branch
in `R/strategies.R` or the refuse rule above.

## Adding a site, or a mechanism

A new site with a working sensor needs nothing: `ts_source = sensor` is the
default `revise-site-info.R` assigns. A site that needs one of the existing
mechanisms is one row in that script's `ts_source_sites` tribble. A new
mechanism is one level in `TS_SOURCES`, one arm in
`qualification_soil_temperature()`, and — if it fits a model — one entry in
`ts_estimators()`.

## Future work

- **`ts_qc = sensor` (prep-stage axis).** Qualify on the raw sensor,
  skipping stage A's reconstructions and wholesale regressions, so that a
  variant has a real truth at the 27 "none" sites. The first recipe with a
  different prep key; `site_data` maps over `crossing(site, prep_key)` and the
  `stop()` in `_targets.R` names the spot. A site whose sensor is too sparse
  to qualify a year drops from that variant.
- **Declare the three training-target rules** (DE-Hte, FR-Bil, FR-Pue) as
  site_info columns, and stage A stops testing site names altogether.
