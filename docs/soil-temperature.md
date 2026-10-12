# Soil temperature and soil water: how the columns the model sees come to exist

Soil temperature reaches the respiration model through **two stages**, which is required to preserve the original manuscript's logic.
Both live in `R/soil-temperature.R`; the estimators they share are in `R/ts-estimators.R`.

```
reader ─► stage A ─► QC · growing season · year gap scan ─► stage B ─► model
          TS_measured                                       TS_final
          (qualification-facing)                            (fit-facing)
```

## Outputs from site data prep

`prep_nee_ac()` returns `ac` (the full half-hourly record) and `nightNEE` (the high-quality nighttime subset).
Both data frames have _all_ of the following soil temperature columns, even if they are unused.
The fit gets to choose which column it uses:

- `TS_measured`: stage A's column: the sensor, repaired or reconstructed per site
- `TS_linear`: a `TS ~ TA` linear regression, built at **every** site (even if unused)
- `TS_memfill`: the blocked-CV-selected reconstruction from the per-site `site_fill` target; attached by `get_soil_temperature()` under a `measured_or_best_fill` recipe

In addition, the prep includes the following options for soil water content (SWC, as a %), which are used by direct fits: 

- `SWC_measured`: tower soil water measurements
- `SWC_era5`: ERA5-Land layer-1 soil water

Beside the tables, `site_data` carries:

- `ts_bounds`: the 2.5/97.5 percentiles of growing-season soil temperature, one row per TS column and definition:
    - `climatology` --- calculated from the growing season climatology (i.e., calculate the average for each DOY over all years; then, take the quantiles of that)
    - `halfhourly` --- the quantiles of the raw half-hourly values within the growing season bounds
    - `climatology_uptake` --- the manuscript's for `TS_measured`: like `climatology`, but over the NEE-uptake days and with some quirks (see [below](#climatology_uptake-versus-climatology)); an estimate's version sees only the years step 01 kept
    - `TS_memfill`'s rows come with the column, from the `site_fill` target, and are added by `get_soil_temperature()`.
- `uptake_doy`: the NEE-uptake days the season was detected from, for the estimates' `climatology_uptake` rows
- `ts_qc`: the quality verdict on `TS_measured`
- `ts_provenance`: stage A's provenance row
- `feature_gs`: the growing season, year count, and `TS_measured`'s `climatology_uptake` bounds.

## Soil-temperature bounds

**Bounds travel with the column.**
`tStart`/`tEnd` gate the window-skip test in `fit_tas_window()`, and they are percentiles of a particular column, so selecting a column and selecting its bounds are one act.
Every candidate column carries a row per definition in `ts_bounds` (`ts_bounds_rows()`), so selecting bounds is a lookup, not a computation.

The manuscript used different definitions for different columns, and they differ a lot:
- the measured column's bounds are percentiles of the *day-of-year climatology* over the NEE-uptake days, from `detect_growing_season()` (`climatology_uptake`);
- the regressed column's are percentiles of the raw *half-hourly* values (`halfhourly`).

Across 44 sites the half-hourly band is 16.7 °C wide against the climatology's 10.0 °C, before the column changes at all ([`ts-rework.html`](ts-rework.html), F4).
A recipe names the definition for each kind of column, `ts_bounds_measured` and `ts_bounds_estimated` (see [`recipes.md`](recipes.md#ts_bounds_measured--ts_bounds_estimated)); setting them equal applies one definition throughout.

### `climatology_uptake` versus `climatology`

The `climatology_uptake` row is *not* the `climatology` row, though both are 2.5/97.5 percentiles of day-of-year mean soil temperature.
For `TS_measured` it is the `tStart`/`tEnd` of `detect_growing_season()`, and differs in:

- **Days.** `climatology_uptake` averages over the NEE-uptake days (DOYs whose multi-year mean NEE is below the cut-off), not over `[gStart, gEnd]`.
  The two sets are close but not equal:
  the season `[gStart, gEnd]` is the 7th uptake day from each end widened by 4 days, so isolated uptake days outside it count towards `climatology_uptake` and weak-uptake days inside it do not;
  and the uptake days ignore both the TS ≥ 0 tightening of `gStart` and the `gStart`/`gEnd` overrides in `site_info.csv` (24 sites).
  (The TS ≥ 0 tightening and the `site_info.csv` overrides _do_ still apply to `gStart`/`gEnd` themselves.)
- **Years.** `climatology_uptake` is computed on the whole record, before disqualified years are dropped;
  `climatology` is computed on the years from the first to the last year step 01 kept (disqualified years inside that span are still included).
- **Floors.** In `climatology_uptake`, `tStart` is floored at 0 °C, and at 2 °C at `SITES_TS_MIN_2C` (CH-Dav, US-Ha1, US-GLE).
  `climatology` is not floored.

Under `climatology_uptake`, `tStart` / `tEnd` values estimated from `TS_measured` come directly from `detect_growing_season()`, which runs before any years are dropped.
That's why `prep_nee_ac()` writes it by hand instead of building it with `ts_bounds_rows()` like the other rows.

Estimated columns (e.g., `TS_linear`, `TS_memfill`) also get a `climatology_uptake` row, computed by `ts_bounds_climatology_uptake()`.
It reuses the measured column's uptake days (`uptake_doy`) and the same floors, so the method is the same.
The only difference is the years: an estimated column only exists from the first to the last year step 01 kept, so its climatology covers that span and not the whole record.
Given the same data, the two calculations give identical results (tested in `test-recipes.R`).

## Why two stages

The manuscript qualified years —
the QC filter, the growing-season detector, the gap scan —
on one soil-temperature column,
and then, at 35 sites,
fitted a `TS ~ TA` regression **on the years that scan had qualified**
and fitted the model on the regression instead.
So the column that decides which years are trustworthy
is not the column the model is fitted on,
and it cannot be:
stage B's regression needs stage A's qualification to exist first.
A single "decide soil temperature once, before everything" step
would change the regression's training set and the year selection at those 35 sites,
and would not reproduce the manuscript.

Making the two stages explicit —
two named columns, two functions, one provenance trail —
is the tidying.
Downstream of stage B there is exactly one soil-temperature column
and no branching on site, origin or strategy.

## Stage A — `qualification_soil_temperature()`

**In:** the record's columns under shared names
(`fluxnet_ts_input()`, `ameriflux_ts_input()` — the readers' only job).
**Out:** `TS`, `TS_QC`, and a one-row provenance table.

Which arm runs is **`ts_source` in `site_info.csv`** —
one level per mechanism the manuscript applied,
derived in `scripts/revise-site-info.R`:

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
| `reconstructed` | 16 (`estimate_Ts` in `site_info_orig.csv`) | `fix_soil_temp()`: the estimator `estimate_ts_method` names, predictors gap-filled by DOY × time climatology | replace | none |

`TS_SOURCES` in `R/constants.R` is that last column:
whether *every* row of the column is a sensor reading.
It is deliberately strict —
FI-Sod and US-MBP are "none" because part of their column is derived —
and it is what the refuse rule (below) reads,
through `ts_measured_truth()`.

The only site names left inside stage A
are the manuscript's three training-target rules in the `reconstructed` arm:
DE-Hte trains on the second depth,
FR-Bil discards soil temperature before 2021,
FR-Pue before 2016.

Stage A's provenance row
(`site_data$ts_provenance`; `ts_provenance.csv` in the run)
records `ts_source`, `ts_truth`, `stage_a_estimator`,
`stage_a_family`, `stage_a_mode`, `stage_a_n_train`, and a note.

## Stage B — `get_soil_temperature()`

**In:** `site_data`, the recipe, the per-site fill.
**Out:** `ac` and `nightNEE`
with **`TS_final` as their only soil-temperature column**,
and a metadata row.

It selects a candidate under the recipe's `ts` strategy (`choose_ts_col()`),
attaches `TS_memfill` if that is what was chosen,
looks up the bounds belonging to the selected column under the recipe's `ts_bounds_measured` or `ts_bounds_estimated` strategy,
and drops every other candidate —
`TS_measured`, `TS_linear`, `TS_memfill` —
so that "no branching downstream" is a checkable property rather than a convention.
The metadata row carries everything the settings table reports about soil temperature,
and copies stage A's provenance fields in.

### The refuse rule: no estimate on top of an estimate

Every variant alternative to `TS_measured` is a model fitted *to* it:
`TS_linear` regresses it on air temperature,
and `TS_memfill` is cross-validated against it.
That only makes sense if `TS_measured` is real sensor data.
Where `ts_source` leaves rows that are not a sensor reading,
the second model just learns to copy the first,
and returns a function of the same predictors wearing a skill score.
(In the extreme, DE-Hte's `lm_ta_netrad` fill reproduces its target almost exactly: CV error 1.5e-14.)

So under `measured_or_lm` and `measured_or_best_fill`,
stage B keeps `TS_measured` at those sites (and that is what later fits use),
records `ts_refused = TRUE` and the reason,
and `fill_soil_temp()` declines before doing any work:
the `site_fill` target records `status = "no measured truth"`,
so these sites have no rows in `fill_cv.csv`.

The rule applies at 27 sites, every `ts_source` mapped to `"none"` in `TS_SOURCES`:

- 16 sites with `ts_source = reconstructed` (soil temperature estimated wholesale)
- 7 sites with `ts_source = lm_ta_cold` or `lm_ta_recent` (wholesale `TS ~ TA` regressions)
- GF-Guy, which uses air temperature in place of soil temperature
- US-Cwt, which uses a regression borrowed from a neighbouring site
- FI-Sod and US-MBP, where only part of the record is estimated

The manuscript's `site_info` strategy is exempt:
`ts_col` there is a declaration,
and its two double-applications are the manuscript's own.
At FI-Sod and US-MBP, stage A rebuilds part of `TS_measured`
(chained depth-to-depth regressions before 2006 at FI-Sod; `TA × 0.369 + 5.87` in the gaps at US-MBP),
and then `ts_col = TS_linear` fits `TS ~ TA` (on `TA > 0` rows) to that partly-estimated column.
At US-MBP the filled rows are already an exact linear function of TA, so the second fit is pulled toward the first
(see also [Ordering](#ordering-and-why-it-is-load-bearing)).

The rule reads what stage A actually ran (`ts_truth` in the provenance row), not what `ts_source` declares.
Under `ts_qc = sensor` ([below](#the-ts_qc-prep-axis)) the rule still applies,
but stage A skips its reconstructions and keeps the raw sensor,
so `TS_measured` is real data again, nothing is refused, and the second method has something to train on.

## The shared parts

- **`ts_estimators()`** (`R/ts-estimators.R`):
  every model that produces a soil-temperature estimate,
  one shape — `family`, `predictors`, `fit`, `predict`.
  The manuscript's are `lm_ta`, `lm_ta_pos`, `lm_ta_recent`, `lm_ta_netrad`
  and `rf_ta_netrad_manuscript`
  (workflow 01_01's `randomForest`, 60 k rows, 70/30);
  the fill's are the `ranger` forests and the memory-feature models.
  Stage A and the fill draw from the same list.
- **`write_back_ts(ts, estimate, mode)`**: `replace`, `overlay` or `fill_gaps`.
  No default; the mode is the point.
- **`ts_bounds_rows()`**: both bounds definitions for any candidate column.

## What is the manuscript and what is a variant

Under the `original` recipe nothing in this design changes a number,
and `pixi run ts-baseline` (below) holds it to that.
Everything a variant does differently
is a `switch()` branch in `R/strategies.R`
or the refuse rule above.

## Adding a site, or a mechanism

A new site with a working sensor needs nothing:
`ts_source = sensor` is the default `revise-site-info.R` assigns.
A site that needs one of the existing mechanisms
is one row in that script's `ts_source_sites` tribble.
A new mechanism is one level in `TS_SOURCES`,
one arm in `qualification_soil_temperature()`,
and — if it fits a model — one entry in `ts_estimators()`.

## The `ts_qc` prep axis

`qualification_soil_temperature(input, site_info, ts_qc)`:
under `manuscript` the arm is the declaration;
under `sensor` every arm whose result is not measured at every row
is replaced by the raw declared sensor,
and the arms that are (`sensor_depth2` at CZ-Stn, `gapfill_pi` at US-NR1/ICh/ICs) still run.
Qualification, the quality screen and the fill's truth are then all measurements.
The provenance row records both the declaration (`ts_source`)
and what ran (`stage_a_arm`, `ts_truth`),
and **stage B and the fill read the latter** —
so under `ts_qc = sensor` nothing is refused
and the fill has a real truth at the 27 sites.
A site whose raw sensor is too sparse to qualify a year
fails its own step-01 target and drops from that recipe —
the honest result, not a fallback.

Because `ts_qc` is a prep axis,
a recipe with `ts_qc = sensor` has a different prep key from the manuscript's
and gets its own step 01 and fill per site
(`site_data_site_info_sensor_*`, `site_fill_site_info_sensor_*` in `_targets.R`),
which only its fits read.
The manuscript's (`site_data_site_info_manuscript_*`) —
what every collector and the `workflows/` scripts consume —
never moves.
See [`recipes.md`](recipes.md#ts_qc).

## Soil water

Soil water is not a per-site column,
because the choice depends on the model as well as the site
(`default_swc_col()` in R/soil-water-columns.R):

| | total model | direct model |
|---|---|---|
| `SWC_use = YES` | `SWC_measured` | `SWC_measured` |
| `SWC_use = NO` | none (soil water is not in the formula) | `SWC_era5` |

Measured soil water is a requirement where it is used:
nighttime rows without it are dropped.
At DE-Tha that takes 66,874 rows down to 21,821.
The ERA5 fallback is deliberately *not* filtered on,
since it is a daily reanalysis with its own gaps.
The asymmetry is inherited from the original.
Many `SWC_use = NO` sites still report a soil-water column the analysis discards;
CH-Dav has 326,351 non-missing values of it.
So measured and reanalysis soil water can be compared
through the `swc_col=` override of `fit_tas_site()`.

ERA5 is joined on in its own target (`site_prep` → `site_data`),
clipped to the site's own flux days,
so that extending the ERA5 file re-runs only the join.
See docs/data-provenance.md.

## Ordering, and why it is load-bearing

Every step-01 filter runs on **stage A's** column:
- the QC flag test;
- the data-gap scan;
- growing-season detection;
- the TS ≥ 2 °C truncation at CH-Dav, US-Ha1 and US-GLE.

Estimated columns are added afterwards and selected in step 02.
Filtering on a regressed column would keep a different set of rows.
The separation also makes three inherited collisions visible.
None is obviously wrong,
and all are worth putting to the authors:

- **US-GLE:** the 2 °C truncation raises `tStart` to 2 °C on the measured column,
  while the substituted column's own lower bound is −2.26 °C.
- **FI-Sod** rebuilds pre-2006 soil temperature in stage A,
  then, as a `TS_linear` site,
  fits `TS ~ TA` on top of it.
- **US-MBP** fills stage-A gaps from air temperature,
  then has the whole column replaced by a differently fitted regression.

`ts_source ∈ {lm_ta_recent, lm_ta_cold}` and `ts_col = "TS_linear"`
are disjoint by construction,
and a test checks it.
A site in both would be regressed twice,
by two different fits.

## Verifying a change here

```bash
pixi run ts-baseline
```

`tests/ts-swc-baseline.R` freezes step-01 digests
and a verbatim transcription of the step-02 substitution,
as it stood at `b077bb9`,
at 20 sites:
one per stage-A arm, the random-forest reconstruction included.
It checks that the current selection path still reproduces them.
It deliberately calls nothing in `R/`.
Step 01 is cached per site, keyed by a digest of `R/`.
Sites whose data has changed since the baseline was written are listed,
and their digests are skipped.

## Future work

- **Declare the three training-target rules** (DE-Hte, FR-Bil, FR-Pue)
  as site_info columns,
  and stage A stops testing site names altogether.
