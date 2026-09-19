library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

tar_source()

# Each brms fit runs `N_CORES` chains in parallel, so `workers * N_CORES` is the
# peak thread demand. But a fit is not 4-cores-busy for its whole life -- much
# of a target is serial R work -- so sizing workers at cores/N_CORES leaves the
# machine idle and costs wall time. Measured on the six-site sample, 18 cores:
#
#   workers=10  load avg 60  sum of target times 223.0 min  wall 49.9 min
#   workers=4   load avg 13  sum of target times 154.0 min  wall 59.8 min
#
# So 4 made each fit 31% faster and the whole run 20% slower: 12 targets over 4
# workers is three scheduling waves, where 10 ran nearly all at once. 8 is the
# midpoint and is *not yet benchmarked* -- worth timing the next time a full
# run happens anyway.
local <- crew_controller_local(workers = 8)
# Worker count is a knob because the full grid's cost is near one 12 h window
# at 20 workers -- see ts-variants.html "Running it" for the arithmetic. It is
# read here, at pipeline definition, and does not enter any target's command.
slurm <- crew_controller_slurm(
  workers = as.integer(Sys.getenv("THERMAL_SLURM_WORKERS", "20")),
  options_cluster = crew_options_slurm(
    time_minutes = 12*60,
    n_tasks = 4,
    verbose = TRUE
  )
)

fqdn <- system2("hostname", stdout = TRUE)
# No global `cue = tar_cue("never")`. It was a development escape hatch from
# when every run meant hours of Stan sampling, but as a default it means a code
# fix to `prep_nee_ac()` invalidates nothing downstream, and the results table
# ends up assembled from two different versions of the code. targets already
# skips unchanged work; the way to make a run affordable is to run fewer sites,
# which is what `pipeline_sites()` does.
tar_option_set(
  error = "continue",
  controller = if (grepl("ycrc.yale.edu", fqdn, fixed = TRUE)) slurm else local
)

# ----------------------------------------------------------------- the grid
#
# Three dimensions, each scoped by an environment variable so that a run can
# be as small as one site x one recipe x one model on a laptop or the whole
# grid on the cluster, from the same file:
#
#   THERMAL_SITES    dev (default) | all | DE-Tha,SE-Nor,...
#   THERMAL_RECIPES  dev (default) | all | original,memfill,...
#   THERMAL_MODELS   total,direct (default) | total | direct
#   THERMAL_FIT      full (default) | fast   -- see fit_settings()
#
# `fast` shrinks the sampler so the entire pipeline can be exercised end to
# end in minutes. Its TAS values are smoke-test artefacts; the settings table
# and the report both say so.
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

# Step 01 is shared across recipes. Every recipe in the registry today has the
# same prep key, so one `site_data` per site serves all of them; this is the
# assertion that keeps that true. When a recipe with a different prep key is
# added (a `computed` year qualification, say), `site_data` has to be mapped
# over `crossing(site, prep_key)` and the fit grid pointed at the matching one
# -- the shape is the same as `site_fill` below, and this stop() names the spot.
prep_keys <- unique(vapply(recipes, function(r) recipe_prep_key(get_recipe(r)), ""))
if (length(prep_keys) != 1) {
  stop(
    "The recipes in this run have ", length(prep_keys), " distinct prep keys (",
    paste(prep_keys, collapse = "; "), "). site_data is built once per site; ",
    "see the comment above this check in _targets.R for what to change."
  )
}

sanitize <- function(x) gsub("-", ".", x, fixed = TRUE)

# Per-site, recipe-independent: the declaration, the download, step 01, and
# the soil-temperature reconstruction the `memory_fill` recipes read.
site_targets <- tar_map(
  values = tibble::tibble(site_name = sites),
  # One read of the site declaration per site, threaded into every stage that
  # needs it. It depends on `site_info_file`, so editing site_info.csv
  # invalidates exactly the sites whose row could have changed.
  tar_target(site_info, get_site_info(site_name, path = site_info_file)),
  tar_target(site_dl, download_site(site_info), format = "file"),
  tar_target(site_data, {site_dl; prep_nee_ac(site_info)}, format = "qs"),
  tar_target(site_fill, fill_soil_temp(site_data, site_info), format = "qs")
)

# One target per recipe, read from the registry file so that editing a row
# invalidates exactly the fits made under it.
recipe_targets <- tar_map(
  values = tibble::tibble(recipe_id = recipes),
  tar_target(recipe, get_recipe(recipe_id, path = recipes_file))
)

# The fits: sites x recipes x models. The per-site inputs are referenced as
# symbols built from the site name, the same device `write_respiration_all()`
# already relies on, so a fit target depends on exactly its own site's data.
grid <- tidyr::crossing(site_name = sites, recipe_id = recipes, model = models) |>
  dplyr::mutate(
    site_data_sym = rlang::syms(paste0("site_data_", sanitize(.data$site_name))),
    site_info_sym = rlang::syms(paste0("site_info_", sanitize(.data$site_name))),
    site_fill_sym = rlang::syms(paste0("site_fill_", sanitize(.data$site_name))),
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
# Everything is combined once, over the whole grid, with `recipe_id` and
# `model` as columns. The manuscript-layout files the `workflows/` scripts
# read are the `original` recipe's slice of those tables, so they keep their
# names and their column contract.
tas_all <- fit_targets$site_tas

combined <- list(
  tar_combine(variant_outcome_tbl, tas_all, command = collect_outcome(!!!.x)),
  tar_combine(variant_siteyear_tbl, tas_all, command = collect_outcome_siteyear(!!!.x)),
  tar_combine(variant_settings_tbl, tas_all, command = collect_settings(!!!.x)),
  tar_combine(variant_window_skips_tbl, tas_all, command = collect_window_skips(!!!.x)),
  tar_combine(feature_gs_tbl, site_targets$site_data, command = collect_feature_gs(!!!.x)),
  tar_combine(ts_qc_tbl, site_targets$site_data, command = collect_ts_qc(!!!.x)),
  tar_combine(fill_cv_tbl, site_targets$site_fill, command = collect_fill_cv(!!!.x)),
  tar_combine(fill_summary_tbl, site_targets$site_fill, command = collect_fill_summary(!!!.x))
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
  tar_file(fill_cv_csv,
           write_result_csv(fill_cv_tbl, file.path(DIR_ANALYSIS, "fill_cv.csv"))),
  tar_file(fill_summary_csv,
           write_result_csv(fill_summary_tbl, file.path(DIR_ANALYSIS, "fill_summary.csv"))),

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

# External inputs the downstream `workflows/` scripts need. Each downloader
# returns quietly when the data is already on disk, so these are cheap to keep
# in the graph -- the cost is re-hashing ~4.8 GB of raster on each run, which is
# a few seconds and buys proper invalidation if a file is replaced.
#
# MODIS/AppEEARS is deliberately absent: it needs an Earthdata login and an
# asynchronous task, and `03_01` degrades to NA spectral predictors without it.
# See docs/data-provenance.md.
external <- list(
  tar_file(ameriflux_bif_file, download_ameriflux_bif()),
  tar_file(gsoc_file, download_gsoc()),
  tar_file(worldclim_files, download_worldclim())
)

# Two reports. `tar_quarto()` scans each document for `tar_read`/`tar_load`
# calls and makes each one a dependency, so a report re-renders whenever the
# results it describes change rather than going quietly stale.
#
#   run_report      the `original` recipe's run, against the manuscript.
#   variant_report  every recipe against `original` and against the
#                   manuscript oracle, with per-site provenance.
reports <- list(
  tar_quarto(run_report, path = "reports/pipeline-report.qmd", quiet = FALSE),
  tar_quarto(variant_report, path = "reports/variant-comparison.qmd", quiet = FALSE)
)

list(
  tar_file(site_info_file, SITE_INFO_CSV),
  tar_file(recipes_file, RECIPES_CSV),
  external,
  site_targets,
  recipe_targets,
  fit_targets,
  combined,
  outputs,
  reports
)
