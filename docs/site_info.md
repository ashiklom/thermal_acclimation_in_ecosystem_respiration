# `site_info.csv`: the site declaration table

`data-core/site_info.csv` has one row per site (117 sites) and 30 columns.
`get_site_info()` (`R/utils.R`) reads it with a fixed column specification, recodes the `YES`/`NO` columns to logical, and stops if any site's `ts_source` is missing or unknown, or if a site that is not `reconstructed` declares a training target.
In the pipeline it is a file target (`site_info_file`), read once per site into a `site_info` target, so editing a row only invalidates the sites it touches ([`data-provenance.md`](data-provenance.md#how-this-reaches-the-pipeline)).

Several columns are not entered by hand.
`scripts/revise-site-info.R` rebuilds the file from `data-core/site_info_orig.csv`, and derives `source`, `estimate_ts_method`, `ts_col`, `ts_linear_domain`, `growing_year_start`, `ts_source`, `ts_train_col` and `ts_train_start_year`, and redeclares `RH`/`VPD` at some sites.
Edit those columns there, not in the CSV.

**Never used** by the pipeline: `ELEV`, `MAP` and `comment`.
`IGBP` and `Climate_class` are read only by the diagnostic script `scripts/ts-qc-validate.R`.

`site_info_orig.csv` also has an `estimate_Ts` column (`YES` where the manuscript reconstructed soil temperature).
`revise-site-info.R` turns it into `ts_source = reconstructed` and drops it, so it is not in `site_info.csv`.

## Site description

| column | meaning | used by |
|---|---|---|
| `site_ID` | FLUXNET-style site ID, e.g. `US-Ha1` | everywhere: the per-site target key, and in error messages |
| `LAT`, `LONG` | site coordinates, decimal degrees | AmeriFlux reader: sunrise/sunset for day/night (`site_sunlight_times()`), the time zone (`site_utc_offset()`), and REddyProc's location; ERA5-Land and MODIS downloads (`scripts/download-era5-swc.py`, `scripts/download-appeears.py`) |
| `ELEV` | elevation, m | **unused** |
| `IGBP` | IGBP vegetation class | `scripts/ts-qc-validate.R` only |
| `Climate_class` | Köppen climate class | `scripts/ts-qc-validate.R` only |
| `MAP` | mean annual precipitation, mm | **unused** |
| `comment` | free-text notes from the manuscript | **unused**; read it before changing `year_removed`, `gStart`/`gEnd` or `SWC_use` at a site |

## Data source and reader

| column | meaning | used by |
|---|---|---|
| `source` | the flux products to splice, oldest first, joined by `+` (e.g. `FLUXNET2015+FLUXNET+ICOS`); derived | `site_sources()`; `download_site()`; `read_spliced_products()` in step 01. `AmeriFlux_BASE` selects the AmeriFlux reader (`site_reader()`), anything else the FLUXNET-family reader. See [`data-provenance.md`](data-provenance.md) |

## AmeriFlux column names

These name the columns of an AmeriFlux BASE file to read.
They are blank at every non-AmeriFlux site; the FLUXNET-family reader uses fixed names (`NEE_VUT_REF`, `TA_F_MDS`, `TS_F_MDS_1`, …) and ignores them.
All are read in `R/ameriflux.R`, apart from `TS` and `TA`, which stage A reads through `ameriflux_ts_input()` (`R/soil-temperature.R`).

| column | meaning | used by |
|---|---|---|
| `NEE` | NEE as a sum of columns, joined by `+` (e.g. `FC_1_1_1 + SC`) | AmeriFlux reader |
| `FC` | the CO₂ flux column; where set, NEE is masked wherever `FC` is missing | AmeriFlux reader |
| `TA` | air temperature | AmeriFlux reader; stage A |
| `TS` | the soil-temperature sensor (at a few sites an air-temperature column) | stage A, as `TS_sensor` |
| `SWC` | soil water content | AmeriFlux reader, only where `SWC_use` is `YES` |
| `SW_IN` | incoming shortwave; a `PPFD_*` column is converted by dividing by 2.3 | AmeriFlux reader |
| `USTAR` | friction velocity | AmeriFlux reader |
| `RH` | relative humidity, converted to VPD for REddyProc | AmeriFlux reader |
| `VPD` | VPD, used only where `RH` is blank | AmeriFlux reader |
| `netrad_column` | the net-radiation column; blank falls back to a bare `NETRAD` if the file has one | AmeriFlux reader and `ameriflux_ts_input()`, for the radiation-driven soil-temperature estimators. At non-AmeriFlux sites it is read only by `revise-site-info.R` to set `estimate_ts_method = NETRAD` |

## Growing season and years

| column | meaning | used by |
|---|---|---|
| `gStart`, `gEnd` | day-of-year overrides for the detected growing season; blank keeps the detector's value | `detect_growing_season()` (`R/growing-season.R`). The `season` recipe axis decides whether they apply: `detect_or_override` uses them, `force_detect` ignores them. See [`recipes.md`](recipes.md#season) and [`soil-temperature.md`](soil-temperature.md#climatology_uptake-versus-climatology) |
| `growing_year_start` | day of year on which the site's growing year starts (183 at the two wrapped Southern-Hemisphere sites); blank means 1; derived | `growing_year_start()` (`R/utils.R`), step 01. See [`growing-year.md`](growing-year.md) |
| `year_removed` | years dropped by hand, e.g. `2001:2003, 2005, 2013` | `get_good_years()` (`R/growing-season.R`), after the gap scan; the `year_qc = site_info` strategy ([`recipes.md`](recipes.md#year_qc)) |

## Soil temperature

See [`soil-temperature.md`](soil-temperature.md) for the two stages these feed.

| column | meaning | used by |
|---|---|---|
| `ts_source` | how stage A builds `TS_measured`: one of the levels in `TS_SOURCES` (`R/constants.R`); required; derived | `ts_source()`; `qualification_soil_temperature()` picks its arm from it; the truth check behind the refuse rule. See [the stage A table](soil-temperature.md#stage-a--qualification_soil_temperature) |
| `estimate_ts_method` | the estimator for `reconstructed` sites: `NETRAD`, `linear regression`, or blank for "TA only"; derived | `ts_estimate_method()`, in `fix_soil_temp()` (stage A) |
| `ts_train_col` | the stage-A input column a `reconstructed` site's estimator is trained on: one of `TS_TRAIN_COLS` (`R/constants.R`), or blank for `TS_sensor`; `TS_depth2` at DE-Hte only; derived | `fix_soil_temp()` (stage A) |
| `ts_train_start_year` | first year of the training target at a `reconstructed` site; earlier years are not trained on, though they are still reconstructed; 2021 at FR-Bil, 2016 at FR-Pue; blank means every year; derived | `fix_soil_temp()` (stage A) |
| `ts_col` | the manuscript's fit column, `TS_measured` or `TS_linear` (35 sites); derived | the `ts = site_info` strategy in `choose_ts_col()` (stage B, [`recipes.md`](recipes.md#ts----soil-temperature-column)). In step 01, a `TS_linear` site stops if its `TS ~ TA` fit fails, where elsewhere the failure is only reported |
| `ts_linear_domain` | rows the `TS ~ TA` regression behind `TS_linear` is fitted on: `night` (the nighttime table; US-Tw1 only), or blank for `ac` (half-hourly, above freezing); independent of `ts_col`, since `TS_linear` is built at every site; derived | `ts_linear_domain_for()` (`R/soil-temp-columns.R`), in step 01 |

## Soil water

| column | meaning | used by |
|---|---|---|
| `SWC_use` | `YES` where the tower's soil water is used | step 01 (nighttime rows must have SWC); the AmeriFlux reader; `default_swc_col()` and the `swc = site_info` strategy ([`recipes.md`](recipes.md#swc)): measured where `YES`, ERA5-Land otherwise |
