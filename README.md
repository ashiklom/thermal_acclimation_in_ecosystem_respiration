# Strength and drivers of thermal responses in ecosystem respiration

## Project objectives

1. Develop algorithms to estimate site-specific total and direct thermal response strength in ecosystem respiration (ER) using global eddy-covariance data.
2. Estimate total and direct thermal response strength at 117 terrestrial ecosystems.
3. Identify drivers of total and direct thermal response strength.
4. Predict how thermal responses will influence future warming-induced change in ER.

## Setup

Everything runs inside a [pixi](https://pixi.sh) environment:

```bash
pixi install     # R, Python, cmdstan and the packages available on conda-forge
pixi run deps    # the rest, from CRAN/GitHub: amerifluxr, REddyProc, gslnls, base64url
pixi run test    # unit tests, about a minute
```

`pixi run deps` is not optional. Those four packages are not on conda-forge,
and the conda-forge build of base64url is broken on osx-arm64; see
`dependencies.R`.

Downloading the data needs (free) accounts with the data providers,
configured in `_creds.toml` at the project root:

```toml
user_id = "myusername"               # AmeriFlux data portal
user_email = "my.name@email.com"     # AmeriFlux data portal
cds_api_key = "12345-67-890ab"       # Copernicus Data Store (ERA5-Land)
appeears_token = "..."               # NASA AppEEARS bearer token; see docs/data-provenance.md
```

## Running the pipeline

`_targets.R` downloads the flux data, prepares it (step 01), and estimates
thermal response strength (step 02). It does this under several
**methodology recipes** side by side: the manuscript's logic (`original`)
and the variants derived from the soil-temperature analysis. It then
compares them in `reports/variant-comparison.qmd`. See `docs/recipes.md`.

Every dimension of the run is an environment variable:

```
THERMAL_SITES    dev (default) | all | DE-Tha,SE-Nor,...
THERMAL_RECIPES  dev (default) | all | original,memfill,...
THERMAL_MODELS   total,direct (default) | total | direct
THERMAL_FIT      full (default) | fast
```

```bash
# smoke test, minutes: two sites, every recipe, total model, shrunken sampler
THERMAL_SITES=DE-RuC,DE-Hte THERMAL_RECIPES=all THERMAL_MODELS=total THERMAL_FIT=fast pixi run targets

# the full grid, on the cluster
sbatch submit.sh
```

`THERMAL_FIT=fast` only proves the pipeline runs end to end. Its TAS values
are not results, and the reports say so. The rendered reports land in
`reports/`. Running on YCRC is covered in `docs/running-on-ycrc.md`.

## Keeping the data current

```bash
scripts/scan-and-run.sh
```

This asks every provider what it publishes and exits within a minute if
nothing changed. If something did, it runs the pipeline, which re-downloads
and refits only the affected sites. See docs/data-provenance.md,
"Keeping it current automatically". Known limitations are listed in
`docs/issues.md`.

## Downstream analysis (`workflows/`)

These scripts run by hand, in order, after the pipeline. They read what it
writes to `data-proc/`.

| script | reads | writes |
|---|---|---|
| `02_02_compare_different_TAS.R` | `outcome_temp*.csv` | nothing (prints tests and plots) |
| `02_03_simulation_test_identifiability_indirect_effects.R` | nothing (a simulation) | `simulation_identifiability_*.csv`, `figures/tas_identifiability_simulation.png` |
| `03_01_prepare_data_for_driver_analysis.R` | the outcome tables, `respiration/`, BIF, GSOC, MODIS | `acclimation_data.csv` |
| `03_02_identify_thermal_response_driver.R` | `acclimation_data.csv` | `varImp_plot.csv`, `partial_plot.csv` |
| `04_01_get_future_night_soil_temperature_change.R` | WorldClim tmin | `Tmin_month_ssp245_wc.csv` |
| `04_02_thermal_response_effect_on_future_ecosystem_respiration.R` | `acclimation_data.csv`, `Tmin_month_ssp245_wc.csv`, `respiration/`, growing-season features | `acclimation_data_future_ssp245.csv` |

The external inputs (BIF, GSOC, WorldClim) are pipeline targets. MODIS comes
from `pixi run download-appeears`; see docs/data-provenance.md.

## Documentation

- `docs/recipes.md`: recipes, their axes, and how to add one.
- `docs/soil-temperature.md`: how the soil-temperature and soil-water columns the model sees come to exist.
- `docs/growing-year.md`: sites whose growing season crosses New Year.
- `docs/data-provenance.md`: where each site's flux data comes from, and how it is kept current.
- `docs/running-on-ycrc.md`: cluster settings and sizing.
- `docs/issues.md`: known issues left as they are.
- `docs/ts-rework.html`, `docs/ts-variants.html`: the soil-temperature analysis (findings F1–F13) and the log of building the recipe grid.

## Demonstration data and scripts for one site: US-Kon

See the README in `Demo_code_data_for_1site/`. It is the original authors'
self-contained demonstration, using their scripts rather than this pipeline.
