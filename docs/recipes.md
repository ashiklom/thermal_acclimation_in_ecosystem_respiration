# Methodology recipes

A **recipe** is a named set of choices, one *strategy* per *axis*, that a run of the pipeline is made under.
The `_targets.R` pipeline runs (or at least attempts) the complete factorial combination of `sites x recipes x models`.
This allows the manuscript's specific logic --- and any number of alternatives --- to be produced by the same code and cleanly compared.

- Registry: `data-core/recipes.csv` — one row per recipe.
- Machinery: `R/recipes.R` (validation, scoping), `R/strategies.R` (what each choice resolves to).
- Report: `reports/variant-comparison.qmd`, a `tar_quarto` target.
- Background: [`ts-rework.html`](ts-rework.html) (findings F1–F13) is the analysis these options come from;
  [`ts-variants.html`](ts-variants.html) is the log of building them.
- Soil temperature specifically --- the two stages, `ts_source`, the estimator registry and the refuse rule --- is documented in [docs/soil-temperature.md](soil-temperature.md).

## The axes

| axis | strategies | stage | what it decides |
|---|---|---|---|
| `ts` | `site_info` · `measured_or_lm` · `measured_or_best_fill` | fit | which soil-temperature column the model is fitted on |
| `season` | `detect_or_override` · `force_detect` · `whole_year` | fit | the day-of-year span the moving windows are laid over |
| `ts_bounds_measured` | `climatology_uptake` · `climatology` · `halfhourly` | fit | which population `tStart`/`tEnd` (the window-skip gate) are percentiles of, when `TS_measured` is selected |
| `ts_bounds_estimated` | `climatology_uptake` · `climatology` · `halfhourly` | fit | the same, when an estimate (`TS_linear`, `TS_memfill`) is selected |
| `swc` | `site_info` · `era5` | fit | which soil-water column the direct model uses |
| `year_qc` | `site_info` | prep | how years are qualified |
| `ts_qc` | `manuscript` · `sensor` | prep | which soil-temperature column step 01 qualifies years on and screens |

### `ts` -- soil temperature column

Picks which soil-temperature column the acclimation model is fitted on.
Step 01's `TS_measured` is not always a sensor reading: at some sites it has already been filled or replaced with an estimate (set by `ts_source` in `site_info.csv`).
Step 01 also records a quality verdict on `TS_measured` --- `GOOD` or `BAD` --- in `site_data$ts_qc`.
Possible values of `ts` are:

- `site_info` — whatever `ts_col` says in `site_info.csv`. This is what the manuscript does.
- `measured_or_lm` — `TS_measured` if `GOOD`; otherwise `TS_linear` (`lm(TS ~ TA)`).
  Good sensors are no longer thrown away (F10: 27 of 50 hand-flagged sites pass every physical test).
- `measured_or_best_fill` — the same, but a `BAD` sensor is replaced by `TS_memfill`, the best cross-validated fill at that site (F12: out-of-fold error cut by 58%).
  Falls back to `TS_linear` if no fill is available, saying why in `settings$ts_reason`.

The two non-manuscript strategies follow the **refuse rule**:
where `TS_measured` is already partly or wholly an estimate (27 sites under `ts_qc = manuscript`), they keep `TS_measured` and set `ts_refused = TRUE`.
Under `ts_qc = sensor`, step 01 skips those reconstructions and keeps the site's declared sensor column, so `TS_measured` is real data everywhere and the rule refuses nothing:
a `BAD` sensor is replaced by `TS_linear` or `TS_memfill` trained on the actual measurements.
See [soil-temperature.md](soil-temperature.md#the-refuse-rule-no-estimate-on-top-of-an-estimate).


### `season`

- `detect_or_override` — the manuscript's season,
  and the name says what the old name `detect` understated.
  `detect_growing_season()` derives `gStart`/`gEnd` from the day-of-year NEE climatology,
  raises `gStart` to the first day whose mean soil temperature is non-negative,
  and then **overwrites either bound with the literal in `site_info.csv` where one is declared**.
  Twenty-four of the 117 rows declare at least one —
  including two of the development sites,
  FI-Sod (`gEnd = 270`) and NL-Loo (`gStart = 120`) —
  so at those sites the "detected" season is partly hand-set.
- `force_detect` — the detector alone, overrides ignored.
  Step 01 carries the unoverridden pair out of `detect_growing_season()`
  as `gStart_detected`/`gEnd_detected`,
  and this strategy lays the windows out on them.
  Identical to `detect_or_override` at the 93 sites with no override;
  the difference at the other 24 is the hand-set part of the season.
- `whole_year` — a full year of DOY,
  starting where the site's growing year starts:
  1–366 at an ordinary site,
  and 183–548 at one whose growing year is wrapped
  (`growing_year_start` in site_info.csv),
  so that the span is in the same coordinates as the data.

Under every strategy **only the window layout changes**:
the `detect_or_override` season still drives the year gap scan (in step 01)
and the control-year choice,
because both need a span to be defined over.
See *Future work*.

### `ts_qc`

- `manuscript` — stage A as the manuscript had it:
  the sensor after its per-site repairs,
  or a reconstruction where the site has none (`ts_source` in `site_info.csv`).
- `sensor` — the raw declared sensor
  wherever the manuscript's arm would leave rows that are not a sensor reading,
  so the refuse rule has nothing to refuse.
  Sites whose raw sensor is too sparse to qualify a year drop from the recipe.

This is a **prep** axis, so a `sensor` recipe gets its own step 01 and fill per site.
`memfill_sensor` is the registry's example;
it is not in `DEV_RECIPES` because it doubles a run's step-01 cost.
See [soil-temperature.md](soil-temperature.md#the-ts_qc-prep-axis).

### `ts_bounds_measured` / `ts_bounds_estimated`

The bounds are percentiles of the selected column, so a recipe names one definition for each kind of column:
`ts_bounds_measured` when the selected column is `TS_measured`, and `ts_bounds_estimated` when it is `TS_linear` or `TS_memfill`.

- `climatology_uptake` — the manuscript's definition for measured soil temperature: percentiles of the day-of-year climatology over the NEE-uptake days, floored at 0 °C (2 °C at three sites).
- `climatology` — 2.5/97.5 percentiles of the day-of-year climatology over `[gStart, gEnd]`.
- `halfhourly` — 2.5/97.5 percentiles of the raw half-hourly values over `[gStart, gEnd]`.

The manuscript is `climatology_uptake` / `halfhourly`, so its gate changes definition along with the column
(F4: a 10.0 °C band under one definition against 16.7 °C under the other, on the same data).
Setting both axes to the same definition removes the inconsistency.
`uptake_all` does so with the manuscript's measured-column definition, so the measured-column sites are exactly as under `original` and only the `TS_linear` sites move;
`memfill_hh` does so with `halfhourly`, which moves every site.

See [soil-temperature.md](soil-temperature.md#soil-temperature-bounds) for how the definitions differ in detail.

### `swc`

- `site_info` — measured where `SWC_use = YES`, ERA5-Land otherwise.
- `era5` — ERA5-Land for every site, so the soil-water driver is one product.
  The total model has no soil water in its formula,
  so the choice is NA there.

See [soil-temperature.md](soil-temperature.md#soil-water).

### `year_qc`

- `site_info` — the gap scan in `get_good_years()` plus the manual `year_removed` list.
  The only strategy so far.

## Prep versus fit

Step 01 (`prep_nee_ac()`) costs 30–140 s a site
and step 02 costs hundreds of Stan fits.
Every axis whose choice can be deferred to step 02 is,
so recipes that differ only in a *fit* choice share one step-01 result.
Step 01 therefore produces every candidate soil-temperature column
(`TS_measured`, `TS_linear`; `TS_memfill` comes from `site_fill`)
and both bounds definitions for each,
and a recipe *selects*.
`RECIPE_PREP_AXES` names the axes that break this sharing:
`_targets.R` maps step 01 and the fill over each site × distinct prep key
among the recipes run (always including the manuscript's),
and each recipe's fits read its own key's `site_data_<prep>_<site>` and `site_fill_<prep>_<site>`.

`swc` is a prep axis because at AmeriFlux sites
step 01 drops nighttime rows without measured soil water *before* the year gap scan,
so a soil-water outage can disqualify a whole year.
It does so only where the recipe fits on `SWC_measured` (`uses_measured_swc()`),
which under `swc = site_info` is exactly the `SWC_use` sites, as in the manuscript.
The `swc_col =` override of `fit_tas_site()` acts after step 01
and cannot change which years qualified; it warns where they would differ.

## Scoping a run

`THERMAL_SITES`, `THERMAL_RECIPES`, `THERMAL_MODELS` and `THERMAL_FIT`;
see the README.
`THERMAL_FIT=fast` (`fit_settings("fast")`) shrinks the sampler
to two chains and a few hundred iterations with no retry;
its TAS values are smoke-test artefacts,
which `settings$fit_profile` records and the reports flag.

## Adding things

- **A recipe**: one row in `data-core/recipes.csv`.
  `read_recipes()` validates every row against `RECIPE_AXES` at pipeline definition.
- **A strategy**: one branch in the matching `choose_*()` in `R/strategies.R`
  and one entry in `RECIPE_AXES`.
  If it needs a column step 01 does not produce
  (ERA5-Land soil temperature, say),
  produce it in step 01 or as a per-site target like `site_fill`,
  and attach it in `fit_tas_site()` the way `TS_memfill` is attached.
- **An axis**: a column in the CSV,
  an entry in `RECIPE_AXES`,
  a `choose_*()` function,
  and a call to it in `fit_tas_site()`
  (or `prep_nee_ac()`, with the axis added to `RECIPE_PREP_AXES`).

## Future work, documented so it is not rediscovered

### A fully season-free variant: what the year-qualification rule needs

`whole_year` deliberately changes only the window layout.
Making the pipeline independent of season detection altogether
requires replacing two more uses of `gStart`/`gEnd`:

1. **Year qualification** (`get_good_years()`).
   Gaps are counted within the season
   and the thresholds scale with its length
   (`max(31, 0.225 × length)` days for the largest gap,
   `max(1/3, …)` for the total).
   Applied to a whole year this would reject almost every boreal year,
   where a nighttime-NEE gap over winter is normal.
   A season-free rule has to define
   "the part of the year this site is expected to have data in"
   without detecting a season —
   options are a per-site data-density profile
   (qualify a year on the fraction of the *site's typical* covered DOYs it covers),
   or qualifying per window rather than per year
   and letting the fit-stage `nobs` guard do the rest.
2. **Control year** (`fit_tas_site()`):
   the year whose growing-season mean TS is closest to the long-term mean.
   Without a season,
   the natural replacement is the year whose mean TS *over the fitted windows*
   is closest to the mean.

(1) is a `prep`-stage change —
a new `year_qc` strategy,
and so a new prep key with its own `site_data_<prep>_*` —
and (2) a `fit`-stage one.

### `year_qc = computed`

Replace the manual `year_removed` list with a scored rule
(gap statistics, u* coverage, residual anomalies),
validated against the 20 sites that carry a hand list today.
Also a prep-stage change.

### `ts = era5`

ERA5-Land `stl1`/`stl2` bias-corrected to the tower on the periods the screen passes.
Needs a CDS download target alongside the existing ERA5 soil-water one,
a `TS_era5` column attached like `TS_memfill`,
and a row in the CV table.
Memory features alone already reach 1.26 °C out of fold (F12),
which is the number this has to beat.

### Data-driven column choice in step 01

CZ-Stn's depth swap, DE-Hte's `TS_F_MDS_2`, FI-Sod's recalibration
and GF-Guy's air-temperature substitution
are still hand-coded in step 01.
The raw-record screen recovers CZ-Stn's and GF-Guy's from the data (F9);
turning it into a `ts_column` prep-stage axis would retire those branches.

### Stochastic imputation

`TS_memfill` is a conditional mean and is slightly too smooth
(within-cell spread ratio 0.90, across-year 0.87; F12).
Drawing from the predictive distribution and fitting several imputations,
or a measurement-error term in the brms formula,
is the principled fix and is step 5 of [`ts-rework.html`](ts-rework.html).
