# Methodology recipes

A **recipe** is a named set of choices, one *strategy* per *axis*, that a run
of the pipeline is made under. `sites × recipes × models` is the grid
`_targets.R` builds, so that the manuscript's logic and any number of
alternatives to it are produced by the same code in the same run and can be
compared row for row.

- Registry: `data-core/recipes.csv` — one row per recipe.
- Machinery: `R/recipes.R` (validation, scoping), `R/strategies.R` (what each
  choice resolves to).
- Report: `reports/variant-comparison.qmd`, a `tar_quarto` target.
- Background: `ts-rework.html` (findings F1–F13) is the analysis these options
  come from; `ts-variants.html` is the log of building them.

## The axes

| axis | strategies | stage | what it decides |
|---|---|---|---|
| `ts` | `site_info` · `screen_best` · `memory_fill` | fit | which soil-temperature column the model is fitted on |
| `season` | `detect` · `whole_year` | fit | the day-of-year span the 14-day windows tile |
| `bounds` | `native` · `climatology` · `halfhourly` | fit | which population `tStart`/`tEnd` (the window-skip gate) are percentiles of |
| `swc` | `site_info` · `era5` | fit | which soil-water column the direct model uses |
| `year_qc` | `site_info` | prep | how years are qualified |

### `ts`

- `site_info` — the `ts_col` declaration in `site_info.csv`. The manuscript.
- `screen_best` — `TS_measured` unless the quality verdict (`site_data$ts_qc`)
  is `BAD`, in which case `TS_linear`, the regression the manuscript would
  have used anyway. Isolates the effect of no longer discarding good sensors
  (F10: 27 of 50 hand-flagged sites pass every physical test).
- `memory_fill` — as `screen_best`, but a `BAD` sensor is replaced by
  `TS_memfill`, the blocked-CV-selected reconstruction from the per-site
  `site_fill` target (F12: memory methods cut out-of-fold error 58%). If the
  fill is unavailable the strategy falls back to `TS_linear` and records why
  in `settings$ts_reason`.

The verdict is computed by `ts_quality()` on the column step 01 leaves as
`TS_measured` — *after* the site-specific column choices step 01 makes — because
that is the column a run would otherwise fit on. The raw-record screen, which is
the one that generalises to an unseen site, is `scripts/ts-qc-screen.R`.

### `season`

- `detect` — the detected growing season.
- `whole_year` — a full year of DOY, starting where the site's growing year
  starts: 1–366 at an ordinary site, and 183–548 at one whose growing year is
  wrapped (`growing_year_start` in site_info.csv), so that the span is in the
  same coordinates as the data. **Only the window layout changes.** The
  detected season still drives the year gap scan (in step 01) and the
  control-year choice, because both need a span to be defined over. See
  *Future work*.

### `bounds`

- `native` — each column's own definition as the manuscript had it: the
  day-of-year climatology for the measured column, the raw half-hourly values
  for the regressed one. F4 measured the resulting inconsistency: the
  admissible band is 10.0 °C wide on measured data under one definition and
  16.7 °C under the other, *before the column changes at all*.
- `climatology` / `halfhourly` — one definition applied to whichever column is
  selected. Step 01 produces both for every column (`ts_bounds_rows()`), so this
  is a lookup, not a computation.

Note the `climatology` row for `TS_measured` is *not* the `native` row. The
native one comes from `detect_growing_season()`, computed on the record before
disqualified years are dropped and floored at 0 °C (2 °C at three sites); the
consistent one is computed on the step-01 output. Both are in `ts_bounds`,
flagged by `native`.

### `swc`

- `site_info` — measured where `SWC_use = YES`, ERA5-Land otherwise.
- `era5` — ERA5-Land for every site, so the soil-water driver is one product.
  The total model has no soil water in its formula, so the choice is NA there.

### `year_qc`

- `site_info` — the gap scan in `get_good_years()` plus the manual
  `year_removed` list. The only strategy so far.

## Prep versus fit

Step 01 (`prep_nee_ac()`) costs 30–140 s a site and step 02 costs hundreds of
Stan fits. Every axis whose choice can be deferred to step 02 is, so recipes
that differ only in a *fit* choice share one step-01 result. Step 01 therefore
produces every candidate soil-temperature column (`TS_measured`, `TS_linear`;
`TS_memfill` comes from `site_fill`) and both bounds definitions for each, and a
recipe *selects*. `RECIPE_PREP_AXES` names the axes that break this sharing;
`_targets.R` asserts that every recipe in a run shares one prep key and says
what to change when one does not.

## Scoping a run

```
THERMAL_SITES    dev (default) | all | DE-Tha,SE-Nor,...
THERMAL_RECIPES  dev (default) | all | original,memfill,...
THERMAL_MODELS   total,direct (default) | total | direct
THERMAL_FIT      full (default) | fast
```

`fast` (`fit_settings("fast")`) shrinks the sampler to two chains and a few
hundred iterations with no retry, so the whole pipeline — every recipe, every
collector, both reports — runs end to end on a laptop in minutes. Its TAS
values are smoke-test artefacts; `settings$fit_profile` records it and the
report says so in a banner.

## Adding things

- **A recipe**: one row in `data-core/recipes.csv`. `read_recipes()` validates
  every row against `RECIPE_AXES` at pipeline definition.
- **A strategy**: one branch in the matching `choose_*()` in `R/strategies.R`
  and one entry in `RECIPE_AXES`. If it needs a column step 01 does not produce
  (ERA5-Land soil temperature, say), produce it in step 01 or as a per-site
  target like `site_fill`, and attach it in `total_tas_site()` the way
  `TS_memfill` is attached.
- **An axis**: a column in the CSV, an entry in `RECIPE_AXES`, a `choose_*()`
  function, and a call to it in `total_tas_site()` (or `prep_nee_ac()`, with
  the axis added to `RECIPE_PREP_AXES`).

## Future work, documented so it is not rediscovered

### A fully season-free variant: what the year-qualification rule needs

`whole_year` deliberately changes only the window layout. Making the pipeline
independent of season detection altogether requires replacing two more uses of
`gStart`/`gEnd`:

1. **Year qualification** (`get_good_years()`). Gaps are counted within the
   season and the thresholds scale with its length (`max(31, 0.225 × length)`
   days for the largest gap, `max(1/3, …)` for the total). Applied to a whole
   year this would reject almost every boreal year, where a nighttime-NEE gap
   over winter is normal. A season-free rule has to define "the part of the year
   this site is expected to have data in" without detecting a season — options
   are a per-site data-density profile (qualify a year on the fraction of the
   *site's typical* covered DOYs it covers), or qualifying per window rather
   than per year and letting the fit-stage `nobs` guard do the rest.
2. **Control year** (`total_tas_site()`): the year whose growing-season mean TS
   is closest to the long-term mean. Without a season, the natural replacement
   is the year whose mean TS *over the fitted windows* is closest to the mean.

Both are `prep`-stage changes for (1) and a `fit`-stage change for (2); (1)
needs a new `year_qc` strategy, which is the first recipe with a different prep
key and therefore the first that needs `site_data` mapped over
`crossing(site, prep_key)` — `_targets.R` names the spot.

### `year_qc = computed`

Replace the manual `year_removed` list with a scored rule (gap statistics,
u* coverage, residual anomalies), validated against the 20 sites that carry a
hand list today. Same prep-key consequence as above.

### `ts = era5`

ERA5-Land `stl1`/`stl2` bias-corrected to the tower on the periods the screen
passes. Needs a CDS download target alongside the existing ERA5 soil-water one,
a `TS_era5` column attached like `TS_memfill`, and a row in the CV table.
Memory features alone already reach 1.26 °C out of fold (F12), which is the
number this has to beat.

### Data-driven column choice in step 01

CZ-Stn's depth swap, DE-Hte's `TS_F_MDS_2`, FI-Sod's recalibration and GF-Guy's
air-temperature substitution are still hand-coded in step 01. The raw-record
screen recovers CZ-Stn's and GF-Guy's from the data (F9); turning it into a
`ts_column` prep-stage axis would retire those branches.

### Stochastic imputation

`TS_memfill` is a conditional mean and is slightly too smooth (within-cell
spread ratio 0.90, across-year 0.87; F12). Drawing from the predictive
distribution and fitting several imputations, or a measurement-error term in
the brms formula, is the principled fix and is step 5 of `ts-rework.html`.
