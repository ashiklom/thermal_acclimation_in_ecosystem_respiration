# Helpers for the targets pipeline (_targets.R).

#' Read the site table the pipeline runs on
#'
#' The site table (THERMAL_SITE_INFO; scripts/derive-site-info-new.R writes them): one row per
#' site, with every column step 01 reads, `season_wrap` (TRUE/FALSE: whether the growing season
#' spans New Year) and `added` (the date a site was added; empty for the manuscript's sites).
#' Stops if a site lacks either or a site is listed twice.
#'
#' @param path Path of the site table.
#' @return The table, as utils::read.csv() reads it.
#' @noRd
read_site_info <- function(path) {
  site_info <- utils::read.csv(path)
  for (col in c("season_wrap", "added")) {
    if (!col %in% names(site_info)) stop(path, ": no column `", col, "` (scripts/derive-site-info-new.R)", call. = FALSE)
  }
  bad <- site_info$site_ID[!is.logical(site_info$season_wrap) | is.na(site_info$season_wrap)]
  if (!is.logical(site_info$season_wrap)) bad <- site_info$site_ID
  if (length(bad)) stop(path, ": season_wrap must be TRUE or FALSE; not at ", paste(bad, collapse = ", "), call. = FALSE)
  dup <- unique(site_info$site_ID[duplicated(site_info$site_ID)])
  if (length(dup)) stop(path, ": sites listed twice: ", paste(dup, collapse = ", "), call. = FALSE)
  site_info$added[is.na(site_info$added)] <- ""
  site_info
}

#' A site's rows of the pipeline's tables
#'
#' The rows of one site, for its per-site targets: what a site's targets read from a table of
#' all sites is its own rows, so that adding or editing other sites' rows reruns nothing of it.
#' Of the site table, the fields step 01 and later read: not `added`, which is bookkeeping.
#'
#' @param x A table with a site column.
#' @param site_id Site ID.
#' @param by Name of the site column (`name` in the ERA5 table).
#' @return The site's rows.
#' @noRd
site_rows <- function(x, site_id, by = "site_ID") {
  x[x[[by]] == site_id, setdiff(names(x), "added"), drop = FALSE]
}

#' Raw files of a site's step-01 preparation
#'
#' The raw files a site's step-01 preparation reads, for file targets: its products' files, plus
#' US-BZF's record at US-BZS (borrowed air temperature) and the PI soil-water files at FR-Pue.
#'
#' @param site The site's row of data/site_info.csv.
#' @param base_dir Root of the raw site data.
#' @return A character vector of file paths.
#' @noRd
site_raw_files <- function(site, base_dir = path_original("SiteData")) {
  products <- source_products(site$source)
  files <- vapply(products, function(p) raw_product_file(site$site_ID, p, base_dir), character(1))
  if (site$site_ID == "US-BZS") files <- c(files, raw_product_file("US-BZF", "AmeriFlux_BASE", base_dir))
  if (site$site_ID == "FR-Pue") {
    files <- c(files, list.files(file.path(base_dir, "soil_water_content_FR-Pue"), pattern = "\\.csv$", full.names = TRUE))
  }
  unname(files)
}

#' Directory of a site's raw data
#'
#' Where a site's raw data are, by input data version: the latest AmeriFlux releases are in
#' data-raw/flux (R/download-flux.R); FLUXNET-format sites read their new product from there
#' whatever this is (R/read-raw.R).
#'
#' @param site The site's row of data/site_info.csv.
#' @param data_version "published" or "update".
#' @return path_raw("flux") for AmeriFlux BASE sites in the update, else
#'   path_original("SiteData").
#' @noRd
site_data_dir <- function(site, data_version = "published") {
  if (data_version == "update" && site$source == "AmeriFlux_BASE") path_raw("flux") else path_original("SiteData")
}

#' Step-01 preparation of one site
#'
#' Step-01 preparation of one site from its raw data, dispatching on the data product:
#' prep_site_ameriflux() for AmeriFlux BASE sites, prep_site_euroflux() for the others.
#'
#' @param site The site's row of data/site_info.csv.
#' @param ts_rf The site's 01_01 soil-temperature table, or NULL where it is not estimated.
#' @param coordinates The AmeriFlux sites' coordinates (data/ameriflux_coordinates.csv); used
#'   only at AmeriFlux BASE sites.
#' @param base_dir Root of the raw site data.
#' @return The site's step-01 list: ac, night, feature, climatology, ts_base, ts_params and
#'   ts_ok (see R/step01-prep.R).
#' @noRd
prepare_site <- function(site, ts_rf, coordinates, base_dir = path_original("SiteData")) {
  if (site$source == "AmeriFlux_BASE") {
    prep_site_ameriflux(site, ts_rf = ts_rf, coordinates = coordinates, base_dir = base_dir)
  } else {
    prep_site_euroflux(site, ts_rf = ts_rf, base_dir = base_dir)
  }
}

#' Write a site's step-01 tables
#'
#' Write a site's step-01 tables as parquet files: <site>_ac, <site>_nightNEE and
#' <site>_climatology.
#'
#' @param prepared The site's step-01 list (prepare_site()); its ac, night and climatology are
#'   written.
#' @param site_id Site ID.
#' @param dir Destination directory.
#' @return The three paths.
#' @noRd
write_step01_outputs <- function(prepared, site_id, dir = path_proc("RespirationData")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(dir, paste0(site_id, c("_ac.parquet", "_nightNEE.parquet", "_climatology.parquet")))
  arrow::write_parquet(prepared$ac, paths[1])
  arrow::write_parquet(prepared$night, paths[2])
  arrow::write_parquet(prepared$climatology, paths[3])
  paths
}

#' Fit one TAS model to a site's prepared inputs
#'
#' Uses the compiled Stan programs in the cache (use_stan_cache()).
#'
#' @param inputs The site's TAS inputs, as prepare_tas_inputs() returns them.
#' @param model_type "direct" or "total".
#' @param sampler Sampler settings (tas_fit_settings()).
#' @return estimate_tas()'s list(outcome, siteyear, fit_profile).
#' @noRd
fit_tas_target <- function(inputs, model_type, sampler = tas_fit_settings()) {
  use_stan_cache()
  s <- inputs$settings
  estimate_tas(s$site_ID, inputs$night, inputs$ac, inputs$ac_day, s$gStart, s$gEnd, s$tStart, s$tEnd,
               s$nobs_threshold, s$control_year, model_type = model_type, sampler = sampler)
}

#' Summary of a site's TAS inputs
#'
#' One row describing a site's prepared TAS inputs: its fit settings, the size of the nighttime
#' table, the number of 14-day windows (as in estimate_tas()), and hashes of the tables, so that
#' the inputs of two runs can be compared without keeping them.
#'
#' @param inputs The site's TAS inputs, as prepare_tas_inputs() returns them.
#' @param window_size Window length (days), as in estimate_tas().
#' @return A one-row data frame: the settings' columns, n_night, n_years, first_year, last_year,
#'   nwindow, NEE_daytime_mean, night_hash, ac_hash and ac_day_hash.
#' @noRd
summarise_tas_inputs <- function(inputs, window_size = 14) {
  s <- inputs$settings
  data.frame(
    s,
    n_night = nrow(inputs$night),
    n_years = length(unique(inputs$night$growing_year)),
    first_year = min(inputs$night$growing_year),
    last_year = max(inputs$night$growing_year),
    nwindow = max(round((s$gEnd - s$gStart + 1) / window_size), 1),
    NEE_daytime_mean = mean(inputs$night$NEE_daytime),
    night_hash = digest::digest(inputs$night),
    ac_hash = digest::digest(inputs$ac),
    ac_day_hash = digest::digest(inputs$ac_day)
  )
}

#' Read a site's published random-forest soil temperatures
#'
#' A site's published random-forest soil temperatures (01_01), or NULL if the site does not
#' estimate soil temperature. Stops if the site's file is not among `files`.
#'
#' @param site The site's row of data/site_info.csv.
#' @param files Paths of the published TS_RandomForest files (<site>_TS_rfp.csv).
#' @return A data frame, or NULL.
#' @noRd
read_published_ts_rf <- function(site, files) {
  if (site$estimate_Ts != "YES") return(NULL)
  file <- files[basename(files) == paste0(site$site_ID, "_TS_rfp.csv")]
  if (length(file) != 1) stop("no published TS_RandomForest file for ", site$site_ID)
  utils::read.csv(file)
}

#' Write the combined step 01-02 tables
#'
#' Write the combined tables under the published file names in data-proc/: growing-season
#' features (one table for all sites) and the total and direct outcome tables; and the summary
#' of every site's TAS inputs.
#'
#' @param features The growing-season features of all sites.
#' @param total The total model's combined results, list(outcome, siteyear)
#'   (combine_tas_sites()).
#' @param direct The direct model's combined results.
#' @param inputs_summary The summarise_tas_inputs() rows of all sites.
#' @param dir Destination directory.
#' @return The paths of the six CSV files.
#' @noRd
write_pipeline_outputs <- function(features, total, direct, inputs_summary, dir = Sys.getenv("THERMAL_DATA_PROC", "data-proc")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(dir, c(
    "growing_season_feature.csv",
    "outcome_temp.csv", "outcome_siteyear_temp.csv",
    "outcome_temp_water_gpp.csv", "outcome_siteyear_temp_water_gpp.csv",
    "tas_inputs_summary.csv"
  ))
  utils::write.csv(features, paths[1], row.names = FALSE)
  utils::write.csv(total$outcome, paths[2], row.names = FALSE)
  utils::write.csv(total$siteyear, paths[3], row.names = FALSE)
  utils::write.csv(direct$outcome, paths[4], row.names = FALSE)
  utils::write.csv(direct$siteyear, paths[5], row.names = FALSE)
  utils::write.csv(inputs_summary, paths[6], row.names = FALSE)
  paths
}

#' Write one fit set's step 03-04 tables
#'
#' Write one fit set's step 03-04 tables under the published file names, in
#' data-proc/step03-04/<fit_set>/.
#'
#' @param fit_set Name of the fit set.
#' @param drivers The driver table (driver_data()).
#' @param driver_results The driver analysis, list(varImp, partial) (driver_analysis()).
#' @param future The scenario table (scenario_data()).
#' @param tmin_month Monthly minimum-temperature change by site (future_tmin_change()).
#' @param dir Destination directory.
#' @return The paths of the five CSV files.
#' @noRd
write_step0304_outputs <- function(fit_set, drivers, driver_results, future, tmin_month,
                                   dir = file.path(Sys.getenv("THERMAL_DATA_PROC", "data-proc"), "step03-04", fit_set)) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(dir, c("acclimation_data.csv", "varImp_plot.csv", "partial_plot.csv",
                            "acclimation_data_future_ssp245.csv", "Tmin_month_ssp245_wc.csv"))
  utils::write.csv(drivers, paths[1], row.names = FALSE)
  utils::write.csv(driver_results$varImp, paths[2], row.names = FALSE)
  utils::write.csv(driver_results$partial, paths[3], row.names = FALSE)
  utils::write.csv(future, paths[4], row.names = FALSE)
  utils::write.csv(tmin_month, paths[5], row.names = FALSE)
  paths
}
