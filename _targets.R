library(targets)
library(tarchetypes)

tar_source()

# `error = "null"`: an errored target's value is NULL, the collectors drop it
# (`built()` in R/write-outputs.R), and one site's failure is a gap in the
# tables plus a row in `tar_meta(fields = error)`. Under "continue" it would
# stop the collectors and both reports. 
tar_option_set(error = "null", controller = pipeline_controller())

# ----------------------------------------------------------------- the grid
#
# Sites x recipes x models, each scoped by an environment variable (see
# docs/recipes.md).
sites <- pipeline_sites()
recipes <- lapply(rlang::set_names(pipeline_recipes()), get_recipe)
models <- pipeline_models()
# THERMAL_FIT=fast shrinks the sampler for quick tests, but produces fake results.
FIT_PROFILE <- Sys.getenv("THERMAL_FIT", "full")
fit_settings(FIT_PROFILE) # check and fail early

# Data preparation:
# Only a subset of recipe parts (e.g., `year_qc`, `ts_qc`) affect data prep, 
# and the same data prep can be shared by fits of different models.
# Always build the original manuscript prep because the site-level outputs and 
# `workflows/` scripts need it.
preps <- tibble::tibble(
  prep_axes = unique(lapply(c(list(original_recipe()), recipes), `[`, RECIPE_PREP_AXES)),
  prep = vapply(.data$prep_axes, recipe_prep_key, "")
)

# Each recipe is written into its fits' commands as a literal, so editing one
# row of recipes.csv invalidates only that recipe's fits.
fits <- tidyr::crossing(recipe_id = names(recipes), model = models) |>
  dplyr::mutate(
    recipe = unname(recipes[.data$recipe_id]),
    prep = vapply(.data$recipe, recipe_prep_key, "")
  )

message(
  "Pipeline grid: ", length(sites), " site(s) x ", length(recipes), " recipe(s) x ",
  length(models), " model(s) = ", nrow(fits) * length(sites), " fits; fit profile ",
  FIT_PROFILE, "; step-01 preps: ", paste(preps$prep, collapse = ", ")
)

# ------------------------------------------------------------- per site
#
# Nested `tar_map()`s: sites, then step-01 preps, then recipe x model fits.
# Each level renames the targets inside it, so a fit's `site_data` is its own
# site's under its own recipe's prep, e.g. `site_tas_original_total` under the
# `site_info_manuscript` prep at US-Kon is
# `site_tas_original_total_site_info_manuscript_US.Kon`.

# For each kind of data preparation ("prep"):
per_prep <- function(p) {
  # Get the fits matching that prep 
  prep_fits <- fits |> 
    dplyr::filter(.data$prep == p$prep) |>
    dplyr::select("recipe_id", "model", "recipe")
  tar_map(
    values = p,
    names = "prep",
    # Prepare the site nighttime respiration according to the corresponding 
    # recipe in `prep_fits$prep_axes`.
    #
    # `era5 = NULL` here because we attach it later in a separate step. ERA5 
    # SWC is stored as one big CSV file with all sites, and we don't want to 
    # reprocess every site every time that changes.
    tar_target(site_prep, {site_dl; prep_nee_ac(site_info, recipe = prep_axes, era5 = NULL)}, format = "qs"),
    # Get the end date for ERA5 (used in `tar_combine` later)
    tar_target(site_end, site_record_end(site_prep)),
    # Attach ERA5 SWC to the prepared data
    tar_target(
      site_data,
      attach_era5_swc(site_prep, site_era5_swc(site_prep, site_name, path = era5_swc_file, table = era5_swc_tbl)),
      format = "qs"
    ),
    # The soil-temperature reconstruction the `memory_fill` recipes read.
    tar_target(site_fill, fill_soil_temp(site_data, site_info), format = "qs"),
    if (nrow(prep_fits)) {
      tar_map(
        values = prep_fits,
        names = c("recipe_id", "model"),
        tar_target(
          site_tas,
          fit_tas_site(site_data, site_info, model = model, recipe = recipe,
                       fill = site_fill, fit_profile = FIT_PROFILE),
          format = "qs"
        )
      )
    }
  )
}

per_site <- tar_map(
  values = tibble::tibble(site_name = sites),
  # Depends on `site_info_file`, so an edit invalidates only the sites it touches.
  tar_target(site_info, get_site_info(site_name, path = site_info_file)),
  # Compared by value: a site whose providers published nothing new stays
  # up to date from here down.
  tar_target(site_remote, remote_for_site(remote_catalog, site_name), deployment = "main"),
  # Its value is the files' hashes, so a re-fetch of identical bytes (or a
  # failed update that restores the old copy) invalidates nothing.
  tar_target(site_dl, download_site(site_info, remote = site_remote), format = "file"),
  lapply(split(preps, preps$prep), per_prep)
)

# The per-site targets whose names start with `prefix`.
per_site_named <- function(prefix) tar_select_targets(per_site, dplyr::starts_with(prefix))
manuscript_data <- per_site_named(paste0("site_data_", MANUSCRIPT_PREP_KEY(), "_"))

# ---------------------------------------------------------------- inputs

inputs <- list(
  tar_file(site_info_file, SITE_INFO_CSV),
  # Fits carry their recipe literally; this is for the variant report.
  tar_file(recipes_file, RECIPES_CSV),
  # What every provider publishes, as of the last scan. The scan runs outside
  # the pipeline (`scripts/scan-and-run.sh`) and rewrites the file only when
  # something changed; see R/remote-catalog.R.
  tar_file(remote_catalog_file, ensure_remote_catalog(site_info_file)),
  tar_target(remote_catalog, read_remote_catalog(remote_catalog_file), deployment = "main"),
  # Non-flux inputs for `workflows/`. Each downloader returns at once when the
  # data is on disk. MODIS (AppEEARS) is not here: it is an asynchronous
  # submit/poll/download cycle; see docs/data-provenance.md.
  tar_file(ameriflux_bif_file, download_ameriflux_bif()),
  tar_file(gsoc_file, download_gsoc()),
  tar_file(worldclim_files, download_worldclim()),
  # ERA5-Land soil water: one file for every site, extended only when some
  # site's flux record runs past its end (or a site is missing from it).
  tar_combine(era5_through, per_site_named("site_end_"), command = latest_record_end(!!!.x)),
  tar_file(era5_swc_file, ensure_era5_coverage(era5_through, site_info_path = site_info_file)),
  tar_target(era5_swc_tbl, load_era5_table(era5_swc_file), format = "qs")
)

# ---------------------------------------------------------------- outputs
#
# Fits are combined once over the whole grid, with `recipe_id` and `model` as
# columns; the site-level tables come from the manuscript's prep. The CSVs are
# written in two groups, one per report, so a report re-renders only when one
# of its own files changes.
outputs <- list(
  tar_combine(fit_tables, per_site_named("site_tas_"), command = collect_fit_tables(!!!.x)),
  tar_combine(site_tables, manuscript_data, command = collect_site_tables(!!!.x)),
  tar_combine(fill_tables, per_site_named(paste0("site_fill_", MANUSCRIPT_PREP_KEY(), "_")),
              command = collect_fill_tables(!!!.x)),
  tar_file(manuscript_siteyear_files, MANUSCRIPT_SITEYEAR_CSVS),
  # The manuscript layout `workflows/` reads, and the run report's inputs.
  tar_file(run_csvs, write_run_csvs(fit_tables, site_tables, manuscript_siteyear_files)),
  tar_file(variant_csvs, write_variant_csvs(fit_tables, site_tables, fill_tables)),
  tar_combine(respiration_csv, manuscript_data, command = write_respiration_all(!!!.x), format = "file"),
  # `tar_quarto()` makes every literal `tar_read()` in a report a dependency.
  #   run_report      the `original` recipe's run, against the manuscript.
  #   variant_report  every recipe against `original` and the manuscript.
  tar_quarto(run_report, path = "reports/pipeline-report.qmd", quiet = FALSE),
  tar_quarto(variant_report, path = "reports/variant-comparison.qmd", quiet = FALSE)
)

list(inputs, per_site, outputs)
