# targets pipeline for workflow steps 01 (site data preparation) and 02 (TAS
# fits), mapped over sites. Run with `pixi run targets` (locally: crew
# workers on this machine; on YCRC Bouchet: Slurm jobs, see submit.sh).
#
# Environment variables:
#   TAS_SITES        "all" (default), "dev" (DEV_SITES in R/step02-tas.R), or
#                    a comma-separated list of site IDs
#   TAS_FIT_PROFILE  "full" (default; the published sampler settings) or
#                    "fast" (only checks that everything runs)
#   TAS_WORKERS, TAS_SLURM_*  worker settings, see R/controllers.R

library(targets)
library(tarchetypes)

tar_source(c(
  "R/paths.R", "R/read-raw.R", "R/sun-times.R", "R/step01-prep.R",
  "R/step02-prep.R", "R/step02-tas.R", "R/pipeline.R", "R/controllers.R"
))

tar_option_set(
  controller = pipeline_controller(),
  memory = "transient",
  garbage_collection = TRUE,
  # a failing site must not stop the others
  error = "continue"
)

all_sites <- utils::read.csv(file.path("data", "site_info.csv"))$site_ID
selected <- Sys.getenv("TAS_SITES", "all")
pipeline_sites <- switch(selected,
  all = all_sites,
  dev = DEV_SITES,
  strsplit(selected, ",")[[1]]
)
stopifnot(all(pipeline_sites %in% all_sites))
site_values <- data.frame(
  site_id = all_sites[all_sites %in% pipeline_sites],
  name = gsub("-", "_", all_sites[all_sites %in% pipeline_sites])
)

inputs <- list(
  tar_target(site_info_file, file.path("data", "site_info.csv"), format = "file"),
  tar_target(site_info, utils::read.csv(site_info_file)),
  tar_target(site_settings_file, file.path("data", "site_settings.csv"), format = "file"),
  tar_target(site_settings, read_site_settings(site_settings_file)),
  tar_target(coordinates_file, file.path("data", "ameriflux_coordinates.csv"), format = "file"),
  tar_target(coordinates, utils::read.csv(coordinates_file)),
  tar_target(era5_file, path_original("ERA5_daily_swc_1990_2024_allsites.csv"), format = "file"),
  tar_target(era5_swc, read_era5_swc(era5_file)),
  # The published random-forest soil temperatures (01_01) are an input:
  # regenerating them does not reproduce them exactly (see refactor-v2-log.md).
  tar_target(ts_rf_files, list.files(path_original("TS_RandomForest"), pattern = "\\.csv$", full.names = TRUE), format = "file"),
  # The sampler setting is a target, so changing the profile refits.
  tar_target(sampler, tas_fit_settings(), deployment = "main", cue = tar_cue(mode = "always")),
  tar_target(stan_models, {
    use_stan_cache()
    compile_tas_models()
    sort(list.files(path_proc("stan-cache"), pattern = "\\.stan$", full.names = TRUE))
  }, deployment = "main", format = "file")
)

per_site <- tar_map(
  values = site_values,
  names = "name",
  tar_target(site, site_info[site_info$site_ID == site_id, ]),
  tar_target(raw_files, site_raw_files(site), format = "file"),
  tar_target(ts_rf, read_published_ts_rf(site, ts_rf_files)),
  tar_target(step01, {
    raw_files
    prepare_site(site, ts_rf, coordinates)
  }),
  tar_target(step01_files, write_step01_outputs(step01, site_id), format = "file"),
  # (string indexing: tar_map() renames symbols that match target names)
  tar_target(gs_feature, step01[["feature"]]),
  tar_target(tas_inputs, prepare_tas_inputs(site, site_settings[site_settings$site_ID == site_id, ], gs_feature,
                                            step01[["night"]], step01[["ac"]], era5_swc)),
  tar_target(tas_total, {
    stan_models
    fit_tas_target(tas_inputs, "total", sampler)
  }),
  tar_target(tas_direct, {
    stan_models
    fit_tas_target(tas_inputs, "direct", sampler)
  })
)

combined <- list(
  tar_combine(features, per_site[["gs_feature"]], command = combine_features(list(!!!.x))),
  tar_combine(tas_total_all, per_site[["tas_total"]], command = combine_tas_sites(list(!!!.x))),
  tar_combine(tas_direct_all, per_site[["tas_direct"]], command = combine_tas_sites(list(!!!.x))),
  tar_target(output_files, write_pipeline_outputs(features, tas_total_all, tas_direct_all), format = "file")
)

list(inputs, per_site, combined)
