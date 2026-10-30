# targets pipeline for workflow steps 01 (site data preparation) and 02 (TAS
# fits), mapped over sites. Run with `pixi run targets` (locally: crew
# workers on this machine; on YCRC Bouchet: Slurm jobs, see submit.sh).
#
# Environment variables:
#   THERMAL_SITE_INFO  the site table to run on (scripts/derive-site-info-new.R):
#                    data/site_info_new.csv (default) or, with
#                    TAR_PROJECT=original, data/site_info_original.csv
#   THERMAL_SITE_INFO_TA_ONLY  the sites without soil temperature, run with the
#                    air-temperature columns only (R/ta-columns.R):
#                    data/site_info_ta_only.csv (default project), none
#                    ("") with TAR_PROJECT=original
#   TAS_SITES        "all" (default), "original" (the manuscript's sites: an
#                    empty `added`), "dev" (DEV_SITES in R/step02-tas.R), or
#                    a comma-separated list of site IDs
#   TAS_FIT_PROFILE  "full" (default; the published sampler settings) or
#                    "fast" (only checks that everything runs)
#   TAS_WORKERS, TAS_SLURM_*  worker settings, see R/controllers.R
#   TAR_PROJECT      unset (default; the latest data: newest flux releases
#                    and new sites; outputs in data-proc/), "original" (the
#                    manuscript's input data; its own store, _targets.yaml;
#                    set THERMAL_DATA_PROC=data-proc-original to keep its
#                    outputs apart) or "era5_nearest" (the latest data with
#                    the ERA5 soil-water extension as extracted rather than
#                    shifted to the published series' level; a sensitivity
#                    test)

library(targets)
library(tarchetypes)

tar_source(c(
  "R/paths.R", "R/io.R", "R/read-raw.R", "R/sun-times.R", "R/step01-prep.R", "R/ts-qualify.R",
  "R/step02-prep.R", "R/step02-tas.R", "R/ts-columns.R", "R/ta-columns.R", "R/ts-ranking.R", "R/ts-fits.R", "R/ts-screen.R", "R/step03-drivers.R", "R/step04-scenario.R",
  "R/download.R", "R/pipeline.R", "R/controllers.R"
))

tar_option_set(
  controller = pipeline_controller(),
  memory = "transient",
  garbage_collection = TRUE,
  # workers read their dependencies from and write their results to the
  # store themselves (shared file system on Bouchet): the controller holding
  # them all ran out of memory with the fit matrix's many branches
  retrieval = "worker",
  storage = "worker",
  # a failing site (or a site's column of the fit matrix) must not stop the
  # others: its value is NULL, which the combining steps leave out
  error = "null"
)

# input data version (refactor-v2 stage S18)
data_version <- if (Sys.getenv("TAR_PROJECT") == "original") "published" else "update"
era5_offset_correction <- Sys.getenv("TAR_PROJECT") != "era5_nearest"

site_info_path <- Sys.getenv("THERMAL_SITE_INFO",
                             file.path("data", if (data_version == "update") "site_info_new.csv" else "site_info_original.csv"))
all_site_info <- read_site_info(site_info_path)
all_sites <- all_site_info$site_ID
selected <- Sys.getenv("TAS_SITES", "all")
pipeline_sites <- switch(selected,
  all = all_sites,
  original = all_sites[all_site_info$added == ""],
  dev = intersect(DEV_SITES, all_sites),
  strsplit(selected, ",")[[1]]
)
site_values <- data.frame(
  site_id = all_sites[all_sites %in% pipeline_sites],
  name = gsub("-", "_", all_sites[all_sites %in% pipeline_sites])
)
# the sites without soil temperature: the air-temperature columns only, with
# a site table (and settings) of their own, so that adding them leaves the
# tables every existing fit depends on unchanged (merged with the deferred
# refit)
ta_only_path <- Sys.getenv("THERMAL_SITE_INFO_TA_ONLY",
                           if (data_version == "update") file.path("data", "site_info_ta_only.csv") else "")
ta_only_sites <- if (nzchar(ta_only_path)) read_site_info(ta_only_path)$site_ID else character()
stopifnot(all(pipeline_sites %in% c(all_sites, ta_only_sites)))
ta_only_sites <- switch(selected,
  all = ta_only_sites,
  original = character(),
  dev = intersect(DEV_SITES, ta_only_sites),
  intersect(ta_only_sites, pipeline_sites)
)
ta_only_values <- data.frame(site_id = ta_only_sites, name = gsub("-", "_", ta_only_sites))

inputs <- list(
  tar_target(site_info_file, site_info_path, format = "file"),
  tar_target(site_info, read_site_info(site_info_file)),
  tar_target(site_settings_file, file.path("data", "site_settings.csv"), format = "file"),
  tar_target(site_settings, read_site_settings(site_settings_file)),
  tar_target(coordinates_file, file.path("data", "ameriflux_coordinates.csv"), format = "file"),
  tar_target(coordinates, utils::read.csv(coordinates_file)),
  tar_target(era5_file, path_original("ERA5_daily_swc_1990_2024_allsites.csv"), format = "file"),
  if (data_version == "update") list(
    # ERA5-Land soil water after 2024 (scripts/download-era5-swc.py)
    tar_target(era5_update_file, path_raw("era5", "ERA5_daily_swc_update.csv"), format = "file"),
    # the whole series of the sites the published file lacks (added sites)
    tar_target(era5_added_file, path_raw("era5", "ERA5_daily_swc_new.csv"), format = "file"),
    tar_target(era5_swc, add_era5_sites(extend_era5_swc(read_era5_swc(era5_file), read_era5_swc(era5_update_file), era5_offset_correction),
                                        read_era5_swc(era5_added_file))),
    tar_target(era5_offsets_file, {
      path <- path_proc("era5_extension_offsets.csv")
      utils::write.csv(attr(era5_swc, "offsets"), path, row.names = FALSE)
      path
    }, format = "file")
  ) else tar_target(era5_swc, read_era5_swc(era5_file)),
  # The published random-forest soil temperatures (01_01) are an input:
  # regenerating them does not reproduce them exactly (see refactor-v2-log.md).
  tar_target(ts_rf_files, list.files(path_original("TS_RandomForest"), pattern = "\\.csv$", full.names = TRUE), format = "file"),
  # The sampler setting is a target, so changing the profile refits.
  tar_target(sampler, tas_fit_settings(), deployment = "main", cue = tar_cue(mode = "always")),
  # step 03-04 inputs
  tar_target(bif_file, path_original("AMF_AA-Net_BIF_CCBY4_20251017.csv"), format = "file"),
  tar_target(gsoc_file, path_original("GSOCmap1.5.0.tif"), format = "file"),
  tar_target(modis_files, path_original(c("towers-MOD13A2-061-results.csv", "towers-MOD15A2H-061-results.csv",
                                          "towers-MYD17A2HGF-061-results.csv")), format = "file"),
  # the added sites' MODIS (scripts/download-modis-ornl.py; none: no files),
  # listed on every run so that files downloaded later are picked up
  tar_target(modis_added_files, sort(list.files(path_raw("modis"), "^towers-new-.*\\.csv$", full.names = TRUE)), format = "file",
             cue = tar_cue(mode = "always")),
  tar_target(worldclim, download_worldclim()),
  tar_target(worldclim_files, unlist(worldclim), format = "file"),
  tar_target(published_tas_files, file.path("data", c("outcome_temp.csv", "outcome_temp_water_gpp.csv")), format = "file"),
  tar_target(pipeline_site_info, site_info[site_info$site_ID %in% site_values$site_id, ]),
  # the air-temperature columns (R/ta-columns.R)
  tar_target(ta_methods, TA_COLUMNS),
  tar_target(scenario_model, {
    use_stan_cache()
    compile_scenario_model()
    TRUE
  }, deployment = "main"),
  tar_target(stan_models, {
    use_stan_cache()
    compile_tas_models()
    sort(list.files(path_proc("stan-cache"), pattern = "\\.stan$", full.names = TRUE))
  }, deployment = "main", format = "file")
)

# Every site's air-temperature columns (R/ta-columns.R), in the layout of the
# soil-temperature columns' fit matrix; per site, `ta_settings` and `ta_tmin`
# are its rows of the settings and of the future minimum temperatures
ta_site_targets <- list(
  tar_target(ta_qc, {
    raw_files
    read_ta_qc(site, site_data_dir(site, data_version))
  }),
  tar_target(ta_candidate_values, ta_candidates(step01, ta_qc)),
  tar_target(ta_summary, ta_columns_summary(site_id, ta_candidate_values, stats::setNames(
    lapply(TA_COLUMNS, ts_column_inputs, site = site, settings = ta_settings, step01 = step01, candidates = ta_candidate_values,
           swc_ERA5 = era5_swc, manuscript_inputs = NULL), TA_COLUMNS))),
  tar_target(ta_fit_key, data.frame(column = ta_methods,
                                    key = ts_inputs_key(ts_column_inputs(ta_methods, site, ta_settings, step01, ta_candidate_values, era5_swc, NULL))),
             pattern = map(ta_methods)),
  tar_target(ta_tas_total, {
    stan_models
    ts_fit_column(ta_methods, "total", ta_fit_key, NULL, function() {
      ts_column_inputs(ta_methods, site, ta_settings, step01, ta_candidate_values, era5_swc, NULL)
    }, sampler)
  }, pattern = map(ta_methods), iteration = "list"),
  tar_target(ta_tas_direct, {
    stan_models
    ts_fit_column(ta_methods, "direct", ta_fit_key, NULL, function() {
      ts_column_inputs(ta_methods, site, ta_settings, step01, ta_candidate_values, era5_swc, NULL)
    }, sampler)
  }, pattern = map(ta_methods), iteration = "list"),
  tar_target(ta_table_key, data.frame(column = ta_methods,
                                      key = ts_tables_key(ts_step01_tables(ta_methods, step01, ta_candidate_values))),
             pattern = map(ta_methods)),
  tar_target(ta_climate_stats, ts_tables_column(ta_methods, ta_table_key, NULL,
                                                function() ts_step01_tables(ta_methods, step01, ta_candidate_values),
                                                function(t) site_climate_stats(site_id, t$ac, t$night)),
             pattern = map(ta_methods), iteration = "list"),
  tar_target(ta_scenario, {
    scenario_model
    use_stan_cache()
    ts_tables_column(ta_methods, ta_table_key, NULL,
                     function() ts_step01_tables(ta_methods, step01, ta_candidate_values),
                     function(t) tryCatch(scenario_site(site_id, t$ac, t$night, t$feature[["gStart"]], t$feature[["gEnd"]],
                                                        ta_tmin, sampler),
                                          error = function(e) {
                                            warning(site_id, ", ", ta_methods, ": no scenario: ", conditionMessage(e), call. = FALSE)
                                            NULL
                                          }))
  }, pattern = map(ta_methods), iteration = "list")
)

per_site <- tar_map(
  values = site_values,
  names = "name",
  tar_target(site, site_info[site_info$site_ID == site_id, ]),
  if (data_version == "update") list(
    # the latest releases: AmeriFlux records from data-raw/flux, and the
    # 01_01 soil temperatures estimated anew on them
    tar_target(raw_files, site_raw_files(site, site_data_dir(site, data_version)), format = "file"),
    tar_target(ts_rf, {
      raw_files
      if (site$estimate_Ts == "YES") estimate_ts_site(site, site_data_dir(site, data_version)) else NULL
    }),
    tar_target(step01, {
      raw_files
      prepare_site(site, ts_rf, coordinates, site_data_dir(site, data_version))
    })
  ) else list(
    tar_target(raw_files, site_raw_files(site), format = "file"),
    tar_target(ts_rf, read_published_ts_rf(site, ts_rf_files)),
    tar_target(step01, {
      raw_files
      prepare_site(site, ts_rf, coordinates)
    })
  ),
  tar_target(step01_files, write_step01_outputs(step01, site_id), format = "file"),
  # step 01's tables as targets of their own, so that what uses one table
  # reruns only when that table changes (string indexing: tar_map() renames
  # symbols that match target names)
  tar_target(gs_feature, step01[["feature"]]),
  tar_target(step01_ac, step01[["ac"]]),
  tar_target(step01_night, step01[["night"]]),
  tar_target(step01_climatology, step01[["climatology"]]),
  tar_target(tas_inputs, prepare_tas_inputs(site, site_settings[site_settings$site_ID == site_id, ], gs_feature,
                                            step01_night, step01_ac, era5_swc, step01_climatology)),
  tar_target(tas_total, {
    stan_models
    fit_tas_target(tas_inputs, "total", sampler)
  }),
  tar_target(tas_direct, {
    stan_models
    fit_tas_target(tas_inputs, "direct", sampler)
  }),
  tar_target(tas_inputs_summary, summarise_tas_inputs(tas_inputs)),
  # candidate soil-temperature columns (R/ts-columns.R)
  tar_target(ts_sensor, {
    raw_files
    read_ts_sensor(site, site_settings[site_settings$site_ID == site_id, ], site_data_dir(site, data_version))
  }),
  # soil-temperature sensor screen (R/ts-screen.R): input data only, no fits
  tar_target(ts_screen, ts_screen_site(step01, ts_sensor, site_settings[site_settings$site_ID == site_id, ], tas_inputs)),
  tar_target(ts_candidate_values, ts_candidates(step01, ts_sensor, site_settings[site_settings$site_ID == site_id, ], tas_inputs)),
  tar_target(ts_columns, {
    settings <- site_settings[site_settings$site_ID == site_id, ]
    columns <- names(ts_candidate_values$values)
    inputs <- stats::setNames(lapply(columns, ts_column_inputs, site = site, settings = settings, step01 = step01,
                                     candidates = ts_candidate_values, swc_ERA5 = era5_swc, manuscript_inputs = tas_inputs), columns)
    list(table = ts_columns_table(step01, ts_candidate_values, inputs), summary = ts_columns_summary(ts_candidate_values, inputs))
  }),
  tar_target(ts_columns_file, write_table(ts_columns[["table"]], path_proc("ts-columns", paste0(site_id, ".parquet"))), format = "file"),
  tar_target(ts_summary, ts_columns[["summary"]]),
  # their quality (R/ts-ranking.R): estimators scored out of fold
  tar_target(ts_cv, ts_cv_metrics(step01, ts_sensor, site_settings[site_settings$site_ID == site_id, ])),
  tar_target(ts_site_quality, ts_quality(site_id, ts_columns[["table"]], ts_columns[["summary"]], ts_cv, gs_feature)),
  # the fit matrix over the selected columns (R/ts-fits.R): one branch per
  # column; columns with the manuscript's input reuse its results
  tar_target(ts_fit_key, {
    settings <- site_settings[site_settings$site_ID == site_id, ]
    data.frame(column = ts_methods_selected,
               key = ts_inputs_key(ts_column_inputs(ts_methods_selected, site, settings, step01, ts_candidate_values, era5_swc, tas_inputs)))
  }, pattern = map(ts_methods_selected)),
  tar_target(ts_tas_total, {
    stan_models
    settings <- site_settings[site_settings$site_ID == site_id, ]
    ts_fit_column(ts_methods_selected, "total", ts_fit_key, tas_total, function() {
      ts_column_inputs(ts_methods_selected, site, settings, step01, ts_candidate_values, era5_swc, tas_inputs)
    }, sampler)
  }, pattern = map(ts_methods_selected), iteration = "list"),
  tar_target(ts_tas_direct, {
    stan_models
    settings <- site_settings[site_settings$site_ID == site_id, ]
    ts_fit_column(ts_methods_selected, "direct", ts_fit_key, tas_direct, function() {
      ts_column_inputs(ts_methods_selected, site, settings, step01, ts_candidate_values, era5_swc, tas_inputs)
    }, sampler)
  }, pattern = map(ts_methods_selected), iteration = "list"),
  tar_target(ts_table_key, data.frame(column = ts_methods_selected,
                                      key = ts_tables_key(ts_step01_tables(ts_methods_selected, step01, ts_candidate_values))),
             pattern = map(ts_methods_selected)),
  tar_target(ts_climate_stats, ts_tables_column(ts_methods_selected, ts_table_key, climate_stats,
                                                function() ts_step01_tables(ts_methods_selected, step01, ts_candidate_values),
                                                function(t) site_climate_stats(site_id, t$ac, t$night)),
             pattern = map(ts_methods_selected), iteration = "list"),
  tar_target(ts_scenario, {
    scenario_model
    use_stan_cache()
    ts_tables_column(ts_methods_selected, ts_table_key, scenario,
                     function() ts_step01_tables(ts_methods_selected, step01, ts_candidate_values),
                     # a column whose control year has no records for a window drops the
                     # site from that column's scenario (with a warning), as failed TAS fits do
                     function(t) tryCatch(scenario_site(site_id, t$ac, t$night, t$feature[["gStart"]], t$feature[["gEnd"]],
                                                        tmin_month[tmin_month$site_ID == site_id, ], sampler),
                                          error = function(e) {
                                            warning(site_id, ", ", ts_methods_selected, ": no scenario: ", conditionMessage(e), call. = FALSE)
                                            NULL
                                          }))
  }, pattern = map(ts_methods_selected), iteration = "list"),
  # steps 03-04: what does not depend on TAS
  tar_target(climate_stats, site_climate_stats(site_id, step01_ac, step01_night)),
  tar_target(scenario, {
    scenario_model
    use_stan_cache()
    scenario_site(site_id, step01_ac, step01_night, gs_feature[["gStart"]], gs_feature[["gEnd"]],
                  tmin_month[tmin_month$site_ID == site_id, ], sampler)
  }),
  # the air-temperature columns
  tar_target(ta_settings, site_settings[site_settings$site_ID == site_id, ]),
  tar_target(ta_tmin, tmin_month[tmin_month$site_ID == site_id, ]),
  ta_site_targets
)

# The sites without soil temperature: step 01's record and the air-temperature
# columns only
ta_only <- if (nrow(ta_only_values)) tar_map(
  values = ta_only_values,
  names = "name",
  tar_target(site, site_info_ta_only[site_info_ta_only$site_ID == site_id, ]),
  tar_target(raw_files, site_raw_files(site, site_data_dir(site, data_version)), format = "file"),
  tar_target(step01, {
    raw_files
    if (site$source == "AmeriFlux_BASE") stop(site_id, ": AmeriFlux BASE sites without soil temperature are not supported", call. = FALSE)
    prep_site_euroflux(site, base_dir = site_data_dir(site, data_version), soil_temperature = FALSE)
  }),
  tar_target(ta_settings, site_settings_ta_only[site_settings_ta_only$site_ID == site_id, ]),
  tar_target(ta_tmin, tmin_month_ta_only[tmin_month_ta_only$site_ID == site_id, ]),
  ta_site_targets
) else list()

combined <- list(
  tar_combine(features, per_site[["gs_feature"]], command = combine_features(list(!!!.x))),
  tar_combine(tas_total_all, per_site[["tas_total"]], command = combine_tas_sites(list(!!!.x))),
  tar_combine(tas_direct_all, per_site[["tas_direct"]], command = combine_tas_sites(list(!!!.x))),
  tar_combine(tas_inputs_summary_all, per_site[["tas_inputs_summary"]], command = dplyr::bind_rows(!!!.x)),
  tar_combine(ts_columns_summary_all, per_site[["ts_summary"]], command = dplyr::bind_rows(!!!.x)),
  tar_target(ts_columns_summary_file, {
    path <- file.path(Sys.getenv("THERMAL_DATA_PROC", "data-proc"), "ts_columns_summary.csv")
    utils::write.csv(ts_columns_summary_all, path, row.names = FALSE)
    path
  }, format = "file"),
  tar_combine(ts_screen_all, per_site[["ts_screen"]], command = combine_ts_screen(list(!!!.x))),
  # soil drivers the latest data's spliced records took from older products
  if (data_version == "update") list(
    tar_combine(source_fills, per_site[["step01"]], command = dplyr::bind_rows(lapply(list(!!!.x), function(p) {
      if (is.null(p$ts_params$filled)) NULL else cbind(site_ID = p$ts_params$site_ID, p$ts_params$filled)
    }))),
    tar_target(source_fills_file, {
      path <- path_proc("source_fills.csv")
      utils::write.csv(source_fills, path, row.names = FALSE)
      path
    }, format = "file")
  ),
  tar_target(ts_screen_validation_table, ts_screen_validation(ts_screen_all, site_info, site_settings)),
  tar_target(ts_screen_files, write_ts_screen(ts_screen_all, validation = ts_screen_validation_table), format = "file"),
  tar_combine(ts_quality_all, per_site[["ts_site_quality"]], command = dplyr::bind_rows(!!!.x)),
  tar_target(ts_ranking, rank_ts_columns(ts_quality_all)),
  # the soil-temperature columns the fits are run with (with the latest
  # data, the original data's selection, data/ts_methods_selected.csv, so
  # that both are run with the same columns)
  if (data_version == "update") list(
    tar_target(ts_methods_selected_file, file.path("data", "ts_methods_selected.csv"), format = "file"),
    tar_target(ts_methods_selected, utils::read.csv(ts_methods_selected_file)$column)
  ) else tar_target(ts_methods_selected, select_ts_methods(ts_ranking)),
  # every selected column's TAS estimates, climate statistics and scenario
  tar_combine(ts_fit_sets_total, per_site[["ts_tas_total"]], command = combine_ts_fits(list(!!!.x), ts_methods_selected)),
  tar_combine(ts_fit_sets_direct, per_site[["ts_tas_direct"]], command = combine_ts_fits(list(!!!.x), ts_methods_selected)),
  tar_combine(ts_climate_all, per_site[["ts_climate_stats"]], command = combine_ts_rows(list(!!!.x), ts_methods_selected)),
  tar_combine(ts_scenario_all, per_site[["ts_scenario"]], command = combine_ts_rows(list(!!!.x), ts_methods_selected)),
  tar_target(ts_outcome_files, {
    dir <- Sys.getenv("THERMAL_DATA_PROC", "data-proc")
    paths <- file.path(dir, c("ts_outcome_total.csv", "ts_outcome_direct.csv", "ts_siteyear_total.csv", "ts_siteyear_direct.csv"))
    utils::write.csv(ts_outcome_table(ts_fit_sets_total), paths[1], row.names = FALSE)
    utils::write.csv(ts_outcome_table(ts_fit_sets_direct), paths[2], row.names = FALSE)
    utils::write.csv(ts_outcome_table(ts_fit_sets_total, "siteyear"), paths[3], row.names = FALSE)
    utils::write.csv(ts_outcome_table(ts_fit_sets_direct, "siteyear"), paths[4], row.names = FALSE)
    paths
  }, format = "file"),
  # steps 03-04 for every column's TAS estimates (the driver analysis over
  # the sites where the column has both)
  tar_target(ts_drivers, driver_data(pipeline_site_info, ts_climate_all[[ts_methods_selected]], spectral, soil,
                                     ts_fit_sets_total[[ts_methods_selected]]$outcome, ts_fit_sets_direct[[ts_methods_selected]]$outcome),
             pattern = map(ts_methods_selected), iteration = "list"),
  tar_target(ts_driver_results, driver_analysis(driver_sites(ts_drivers)),
             pattern = map(ts_drivers), iteration = "list"),
  tar_target(ts_future, scenario_data(ts_drivers, tmin_month, ts_scenario_all[[ts_methods_selected]]),
             pattern = map(ts_drivers, ts_methods_selected), iteration = "list"),
  tar_target(ts_step0304_files, write_step0304_outputs(ts_methods_selected, ts_drivers, ts_driver_results, ts_future, tmin_month,
                                                       dir = file.path(Sys.getenv("THERMAL_DATA_PROC", "data-proc"), "step03-04-ts", ts_methods_selected)),
             pattern = map(ts_methods_selected, ts_drivers, ts_driver_results, ts_future), format = "file"),
  tar_target(ts_quality_files, {
    dir <- Sys.getenv("THERMAL_DATA_PROC", "data-proc")
    paths <- file.path(dir, c("ts_quality.csv", "ts_ranking.csv", "ts_methods_selected.csv"))
    utils::write.csv(ts_quality_all, paths[1], row.names = FALSE)
    utils::write.csv(ts_ranking, paths[2], row.names = FALSE)
    utils::write.csv(data.frame(column = ts_methods_selected), paths[3], row.names = FALSE)
    paths
  }, format = "file"),
  tar_target(output_files, write_pipeline_outputs(features, tas_total_all, tas_direct_all, tas_inputs_summary_all), format = "file"),
  tar_combine(climate_all, per_site[["climate_stats"]], command = dplyr::bind_rows(!!!.x)),
  tar_combine(scenario_all, per_site[["scenario"]], command = dplyr::bind_rows(!!!.x)),
  tar_target(soil, soil_carbon(site_info, bif_file, gsoc_file)),
  tar_target(spectral, spectral_indices(c(modis_files[1], grep("MOD13", modis_added_files, value = TRUE)),
                                       c(modis_files[2], grep("MOD15", modis_added_files, value = TRUE)),
                                       c(modis_files[3], grep("MYD17", modis_added_files, value = TRUE)))),
  tar_target(tmin_month, future_tmin_change(site_info, worldclim$baseline, worldclim$future)),
  # sets of TAS estimates the step 03-04 analyses are run for
  tar_target(fits_published, list(total = utils::read.csv(published_tas_files[1]), direct = utils::read.csv(published_tas_files[2]))),
  tar_target(fits_v2, list(total = tas_total_all$outcome, direct = tas_direct_all$outcome))
)

# Steps 03-04 for each set of TAS estimates.
by_fit_set <- tar_map(
  values = list(fit_set = c("published", "v2"), fits = rlang::syms(c("fits_published", "fits_v2"))),
  names = "fit_set",
  tar_target(drivers, driver_data(pipeline_site_info, climate_all, spectral, soil, fits$total, fits$direct)),
  # (over the sites with both TAS and every predictor: a site's fit can be
  # missing, e.g. US-BZS with the latest data)
  tar_target(driver_results, driver_analysis(driver_sites(drivers))),
  tar_target(future, scenario_data(drivers, tmin_month, scenario_all)),
  tar_target(step0304_files, write_step0304_outputs(fit_set, drivers, driver_results, future, tmin_month), format = "file")
)

# The air-temperature columns: their site tables, TAS estimates, climate
# statistics and scenarios, and steps 03-04 for each column (over the sites
# with soil temperature and those without)
ta_combined <- list(
  if (nzchar(ta_only_path)) list(
    tar_target(site_info_ta_only_file, ta_only_path, format = "file"),
    tar_target(site_info_ta_only, read_site_info_ta_only(site_info_ta_only_file, site_info)),
    tar_target(site_settings_ta_only_file, file.path("data", "site_settings_ta_only.csv"), format = "file"),
    tar_target(site_settings_ta_only, read_site_settings(site_settings_ta_only_file)),
    tar_target(tmin_month_ta_only, future_tmin_change(site_info_ta_only, worldclim$baseline, worldclim$future))
  ) else list(
    tar_target(site_info_ta_only, site_info[0, ]),
    tar_target(tmin_month_ta_only, tmin_month[0, ])
  ),
  tar_target(ta_site_info, dplyr::bind_rows(pipeline_site_info, site_info_ta_only[site_info_ta_only$site_ID %in% ta_only_values$site_id, ])),
  # (over all sites: the regression on the observed soil carbon as it will be
  # once the tables are merged)
  tar_target(ta_soil, soil_carbon(dplyr::bind_rows(site_info, site_info_ta_only), bif_file, gsoc_file)),
  tar_target(ta_tmin_month, dplyr::bind_rows(tmin_month, tmin_month_ta_only)),
  tar_combine(ta_fit_sets_total, per_site[["ta_tas_total"]], ta_only[["ta_tas_total"]], command = combine_ts_fits(list(!!!.x), ta_methods)),
  tar_combine(ta_fit_sets_direct, per_site[["ta_tas_direct"]], ta_only[["ta_tas_direct"]], command = combine_ts_fits(list(!!!.x), ta_methods)),
  tar_combine(ta_climate_all, per_site[["ta_climate_stats"]], ta_only[["ta_climate_stats"]], command = combine_ts_rows(list(!!!.x), ta_methods)),
  tar_combine(ta_scenario_all, per_site[["ta_scenario"]], ta_only[["ta_scenario"]], command = combine_ts_rows(list(!!!.x), ta_methods)),
  tar_combine(ta_summary_all, per_site[["ta_summary"]], ta_only[["ta_summary"]], command = dplyr::bind_rows(!!!.x)),
  tar_target(ta_outcome_files, {
    paths <- path_proc(c("ta_outcome_total.csv", "ta_outcome_direct.csv", "ta_siteyear_total.csv", "ta_siteyear_direct.csv",
                         "ta_columns_summary.csv"))
    utils::write.csv(ts_outcome_table(ta_fit_sets_total), paths[1], row.names = FALSE)
    utils::write.csv(ts_outcome_table(ta_fit_sets_direct), paths[2], row.names = FALSE)
    utils::write.csv(ts_outcome_table(ta_fit_sets_total, "siteyear"), paths[3], row.names = FALSE)
    utils::write.csv(ts_outcome_table(ta_fit_sets_direct, "siteyear"), paths[4], row.names = FALSE)
    utils::write.csv(ta_summary_all, paths[5], row.names = FALSE)
    paths
  }, format = "file"),
  tar_target(ta_drivers, driver_data(ta_site_info, ta_climate_all[[ta_methods]], spectral, ta_soil,
                                     ta_fit_sets_total[[ta_methods]]$outcome, ta_fit_sets_direct[[ta_methods]]$outcome),
             pattern = map(ta_methods), iteration = "list"),
  tar_target(ta_driver_results, driver_analysis(driver_sites(ta_drivers)),
             pattern = map(ta_drivers), iteration = "list"),
  tar_target(ta_future, scenario_data(ta_drivers, ta_tmin_month, ta_scenario_all[[ta_methods]]),
             pattern = map(ta_drivers, ta_methods), iteration = "list"),
  tar_target(ta_step0304_files, write_step0304_outputs(ta_methods, ta_drivers, ta_driver_results, ta_future, ta_tmin_month,
                                                       dir = path_proc("step03-04-ta", ta_methods)),
             pattern = map(ta_methods, ta_drivers, ta_driver_results, ta_future), format = "file")
)

list(inputs, per_site, ta_only, combined, by_fit_set, ta_combined)
