library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

tar_source()

# ------------------------------------------------------------ controllers
#
# Local: 8 workers, each fit running `N_CORES` chains. Slurm (YCRC): one job
# per worker, sized by `THERMAL_SLURM_WORKERS`/`THERMAL_SLURM_MINUTES`, which
# `submit.sh` sets. Every setting below was learned from a failed run; see
# docs/running-on-ycrc.md before changing one.
local <- crew_controller_local(workers = 8)

# Absolute, because a worker inherits its working directory from `sbatch`.
crew_log_dir <- file.path(getwd(), "_logs")
dir.create(crew_log_dir, showWarnings = FALSE, recursive = TRUE)
slurm <- crew_controller_slurm(
  workers = as.integer(Sys.getenv("THERMAL_SLURM_WORKERS", "20")),
  seconds_idle = 600,
  # TLS was implicated in workers dying early on this cluster.
  tls = crew::crew_tls(mode = "none"),
  options_cluster = crew_options_slurm(
    # Keep at or above the controller's own --time in submit.sh.
    time_minutes = as.integer(Sys.getenv("THERMAL_SLURM_MINUTES", "1425")),
    # One task with N_CORES CPUs, so brms's chains share one node.
    n_tasks = 1,
    cpus_per_task = N_CORES,
    # `%A_%a`: crew.cluster submits each batch of workers as a job array.
    log_output = file.path(crew_log_dir, "crew-%A_%a.out"),
    log_error = file.path(crew_log_dir, "crew-%A_%a.err"),
    memory_gigabytes_required = 32,
    # pthreads OpenBLAS sizes its pool from the node's cores, not the job's;
    # uncapped, crowded workers abort on start. Stan does the arithmetic.
    script_lines = c(
      "export OPENBLAS_NUM_THREADS=1",
      "export OMP_NUM_THREADS=1"
    ),
    verbose = TRUE
  )
)

fqdn <- system2("hostname", stdout = TRUE)
# `error = "null"`: an errored target's value is NULL, the collectors drop it
# (`built()` in R/write-outputs.R), and one site's failure is a gap in the
# tables plus a row in `tar_meta(fields = error)`. Under "continue" it would
# stop the collectors and both reports. No global `cue = "never"`: a code fix
# has to invalidate what it touches; run fewer sites to make a run cheap.
tar_option_set(
  error = "null",
  controller = if (grepl("ycrc.yale.edu", fqdn, fixed = TRUE)) slurm else local
)

# ----------------------------------------------------------------- the grid
#
# Sites x recipes x models, each scoped by an environment variable (see
# docs/recipes.md). THERMAL_FIT=fast shrinks the sampler for smoke tests;
# its TAS values are not results.
sites <- pipeline_sites()
recipes <- pipeline_recipes()
models <- pipeline_models()
FIT_PROFILE <- Sys.getenv("THERMAL_FIT", "full")
fit_settings(FIT_PROFILE) # fail here, by name, rather than inside every fit target

message(
  "Pipeline grid: ", length(sites), " site(s) x ", length(recipes), " recipe(s) x ",
  length(models), " model(s) = ", length(sites) * length(recipes) * length(models),
  " fits; fit profile ", FIT_PROFILE
)
message("  sites:   ", paste(sites, collapse = ", "))
message("  recipes: ", paste(recipes, collapse = ", "))

# Step 01 is shared across recipes with the same prep key (`RECIPE_PREP_AXES`).
# The manuscript's prep is the spine: one `site_data` per site, which every
# collector, the manuscript-layout outputs and `workflows/` read. A recipe
# with a different prep key gets its own `site_data_v_*`/`site_fill_v_*` that
# only its fits read.
sanitize <- function(x) gsub("-", ".", x, fixed = TRUE)
prep_of <- vapply(recipes, function(r) recipe_prep_key(get_recipe(r)), "")
variant_preps <- tibble::tibble(prep_key = setdiff(unique(prep_of), MANUSCRIPT_PREP_KEY())) |>
  dplyr::mutate(
    prep_label = prep_key_label(.data$prep_key),
    # any recipe with this key defines the same step 01; take the first
    prep_recipe = vapply(.data$prep_key, function(k) names(prep_of)[prep_of == k][[1]], "")
  )
if (nrow(variant_preps)) {
  message("  variant step-01 preps: ", paste(variant_preps$prep_key, collapse = ", "))
}

# ---------------------------------------------------------------- inputs

# What every provider publishes, as of the last scan. The scan runs outside
# the pipeline (`scripts/scan-and-run.sh`) and rewrites the file only when
# something changed; see R/remote-catalog.R.
remote_targets <- list(
  tar_file(remote_catalog_file, ensure_remote_catalog(site_info_file)),
  tar_target(remote_catalog, read_remote_catalog(remote_catalog_file), deployment = "main")
)

# Non-flux inputs for `workflows/`. Each downloader returns at once when the
# data is on disk. MODIS (AppEEARS) is not here: it is an asynchronous
# submit/poll/download cycle; see docs/data-provenance.md.
external <- list(
  tar_file(ameriflux_bif_file, download_ameriflux_bif()),
  tar_file(gsoc_file, download_gsoc()),
  tar_file(worldclim_files, download_worldclim())
)

# ------------------------------------------------------------- per site

# Recipe-independent: the declaration, the download, step 01, and the
# soil-temperature reconstruction the `memory_fill` recipes read.
site_targets <- tar_map(
  values = tibble::tibble(site_name = sites),
  # Depends on `site_info_file`, so an edit invalidates only the sites it touches.
  tar_target(site_info, get_site_info(site_name, path = site_info_file)),
  # Compared by value: a site whose providers published nothing new stays
  # up to date from here down.
  tar_target(site_remote, remote_for_site(remote_catalog, site_name), deployment = "main"),
  # Its value is the files' hashes, so a re-fetch of identical bytes (or a
  # failed update that restores the old copy) invalidates nothing.
  tar_target(site_dl, download_site(site_info, remote = site_remote), format = "file"),
  # Step 01 in two parts, so that extending the ERA5 file re-runs only the
  # cheap join, and only where the site's own days gained values.
  tar_target(site_prep, {site_dl; prep_nee_ac(site_info, era5 = NULL)}, format = "qs"),
  tar_target(site_end, site_record_end(site_prep)),
  tar_target(site_era5, site_era5_swc(site_prep, site_name, path = era5_swc_file, table = era5_swc_tbl)),
  tar_target(site_data, attach_era5_swc(site_prep, site_era5), format = "qs"),
  tar_target(site_fill, fill_soil_temp(site_data, site_info), format = "qs")
)

# ERA5-Land soil water: one file for every site, extended only when some
# site's flux record runs past its end (or a site is missing from it).
era5_targets <- list(
  tar_combine(era5_through, site_targets$site_end, command = latest_record_end(!!!.x)),
  tar_file(era5_swc_file, ensure_era5_coverage(era5_through, site_info_path = site_info_file)),
  tar_target(era5_swc_tbl, load_era5_table(era5_swc_file), format = "qs")
)

# One target per recipe, so editing a row invalidates only its fits.
recipe_targets <- tar_map(
  values = tibble::tibble(recipe_id = recipes),
  tar_target(recipe, get_recipe(recipe_id, path = recipes_file))
)

# Step 01 and the fill under each variant prep key, per site.
variant_prep_grid <- tidyr::crossing(site_name = sites, variant_preps) |>
  dplyr::mutate(
    site_info_sym = rlang::syms(paste0("site_info_", sanitize(.data$site_name))),
    site_dl_sym = rlang::syms(paste0("site_dl_", sanitize(.data$site_name))),
    recipe_sym = rlang::syms(paste0("recipe_", .data$prep_recipe))
  )
variant_prep_targets <- if (nrow(variant_prep_grid)) {
  tar_map(
    values = variant_prep_grid,
    names = c("site_name", "prep_label"),
    tar_target(site_prep_v, {site_dl_sym; prep_nee_ac(site_info_sym, recipe = recipe_sym, era5 = NULL)}, format = "qs"),
    tar_target(site_era5_v, site_era5_swc(site_prep_v, site_name, path = era5_swc_file, table = era5_swc_tbl)),
    tar_target(site_data_v, attach_era5_swc(site_prep_v, site_era5_v), format = "qs"),
    tar_target(site_fill_v, fill_soil_temp(site_data_v, site_info_sym), format = "qs")
  )
} else {
  list()
}

# ----------------------------------------------------------------- fits

# Sites x recipes x models. Per-site inputs are referenced as symbols built
# from the site name, so a fit depends on exactly its own site's data.
grid <- tidyr::crossing(site_name = sites, recipe_id = recipes, model = models) |>
  dplyr::mutate(
    prep_key = unname(prep_of[.data$recipe_id]),
    # the manuscript's step 01, or this recipe's own
    prep_suffix = ifelse(
      .data$prep_key == MANUSCRIPT_PREP_KEY(),
      paste0("_", sanitize(.data$site_name)),
      paste0("_v_", sanitize(.data$site_name), "_", prep_key_label(.data$prep_key))
    ),
    site_data_sym = rlang::syms(paste0("site_data", .data$prep_suffix)),
    site_info_sym = rlang::syms(paste0("site_info_", sanitize(.data$site_name))),
    site_fill_sym = rlang::syms(paste0("site_fill", .data$prep_suffix)),
    recipe_sym = rlang::syms(paste0("recipe_", .data$recipe_id)),
    direct = .data$model == "direct",
    fit_profile = FIT_PROFILE
  )

fit_targets <- tar_map(
  values = grid,
  names = c("site_name", "recipe_id", "model"),
  tar_target(
    site_tas,
    total_tas_site(
      site_data_sym, site_info_sym,
      direct = direct, recipe = recipe_sym, fill = site_fill_sym,
      fit_profile = fit_profile
    ),
    format = "qs"
  )
)

# ------------------------------------------------------------- collectors
#
# Combined once over the whole grid, with `recipe_id` and `model` as columns.
# The manuscript-layout files `workflows/` reads are the `original` slice.
tas_all <- fit_targets$site_tas

combined <- list(
  tar_combine(variant_outcome_tbl, tas_all, command = collect_outcome(!!!.x)),
  tar_combine(variant_siteyear_tbl, tas_all, command = collect_outcome_siteyear(!!!.x)),
  tar_combine(variant_settings_tbl, tas_all, command = collect_settings(!!!.x)),
  tar_combine(variant_window_skips_tbl, tas_all, command = collect_window_skips(!!!.x)),
  tar_combine(feature_gs_tbl, site_targets$site_data, command = collect_feature_gs(!!!.x)),
  tar_combine(ts_qc_tbl, site_targets$site_data, command = collect_ts_qc(!!!.x)),
  tar_combine(ts_provenance_tbl, site_targets$site_data, command = collect_ts_provenance(!!!.x)),
  tar_combine(fill_cv_tbl, site_targets$site_fill, command = collect_fill_cv(!!!.x)),
  tar_combine(fill_summary_tbl, site_targets$site_fill, command = collect_fill_summary(!!!.x)),
  tar_file(manuscript_siteyear_files, MANUSCRIPT_SITEYEAR_CSVS),
  tar_target(new_siteyears_tbl, collect_new_siteyears(variant_siteyear_tbl, manuscript_siteyear_files))
)

outputs <- list(
  # -- the variant grid, in full --
  tar_file(variant_outcome_csv,
           write_result_csv(variant_outcome_tbl, file.path(DIR_ANALYSIS, "variant_outcome.csv"))),
  tar_file(variant_siteyear_csv,
           write_result_csv(variant_siteyear_tbl, file.path(DIR_ANALYSIS, "variant_siteyear.csv"))),
  tar_file(variant_settings_csv,
           write_result_csv(variant_settings_tbl, file.path(DIR_ANALYSIS, "variant_settings.csv"))),
  tar_file(variant_window_skips_csv,
           write_result_csv(variant_window_skips_tbl, file.path(DIR_ANALYSIS, "variant_window_skips.csv"))),
  tar_file(ts_qc_csv,
           write_result_csv(ts_qc_tbl, file.path(DIR_ANALYSIS, "ts_qc.csv"))),
  tar_file(ts_provenance_csv,
           write_result_csv(ts_provenance_tbl, file.path(DIR_ANALYSIS, "ts_provenance.csv"))),
  tar_file(fill_cv_csv,
           write_result_csv(fill_cv_tbl, file.path(DIR_ANALYSIS, "fill_cv.csv"))),
  tar_file(fill_summary_csv,
           write_result_csv(fill_summary_tbl, file.path(DIR_ANALYSIS, "fill_summary.csv"))),
  tar_file(new_siteyears_csv,
           write_result_csv(new_siteyears_tbl, file.path(DIR_ANALYSIS, "new_siteyears.csv"))),

  # -- the manuscript layout: the `original` recipe only --
  tar_file(outcome_temp_csv,
           write_result_csv(original_only(variant_outcome_tbl, "total"),
                            file.path(DIR_ANALYSIS, "outcome_temp.csv"))),
  tar_file(outcome_temp_water_gpp_csv,
           write_result_csv(original_only(variant_outcome_tbl, "direct"),
                            file.path(DIR_ANALYSIS, "outcome_temp_water_gpp.csv"))),
  tar_file(outcome_siteyear_temp_csv,
           write_result_csv(original_only(variant_siteyear_tbl, "total"),
                            file.path(DIR_ANALYSIS, "outcome_siteyear_temp.csv"))),
  tar_file(outcome_siteyear_temp_water_gpp_csv,
           write_result_csv(original_only(variant_siteyear_tbl, "direct"),
                            file.path(DIR_ANALYSIS, "outcome_siteyear_temp_water_gpp.csv"))),
  # Not manuscript outputs; the run report reads them.
  tar_file(run_settings_csv,
           write_result_csv(original_only(variant_settings_tbl),
                            file.path(DIR_ANALYSIS, "run_settings.csv"))),
  tar_file(window_skips_csv,
           write_result_csv(original_only(variant_window_skips_tbl),
                            file.path(DIR_ANALYSIS, "window_skips.csv"))),

  tar_file(growing_season_features_csv,
           write_result_csv(feature_gs_tbl, file.path(DIR_FEATURES, "growing_season_features.csv"))),
  tar_file(respiration_csv, write_respiration_all(!!!rlang::syms(
    paste0("site_data_", sanitize(sites))
  )))
)

# `tar_quarto()` makes every literal `tar_read()` in a report a dependency.
#   run_report      the `original` recipe's run, against the manuscript.
#   variant_report  every recipe against `original` and the manuscript.
reports <- list(
  tar_quarto(run_report, path = "reports/pipeline-report.qmd", quiet = FALSE),
  tar_quarto(variant_report, path = "reports/variant-comparison.qmd", quiet = FALSE)
)

list(
  tar_file(site_info_file, SITE_INFO_CSV),
  remote_targets,
  tar_file(recipes_file, RECIPES_CSV),
  external,
  site_targets,
  era5_targets,
  recipe_targets,
  variant_prep_targets,
  fit_targets,
  combined,
  outputs,
  reports
)
