# Helpers for the targets pipeline (_targets.R).

# The raw files a site's step-01 preparation reads, for file targets: its
# products' files, plus US-BZF's record at US-BZS (borrowed air temperature)
# and the PI soil-water files at FR-Pue.
site_raw_files <- function(site, base_dir = path_original("SiteData")) {
  products <- source_products(site$source)
  files <- vapply(products, function(p) raw_product_file(site$site_ID, p, base_dir), character(1))
  if (site$site_ID == "US-BZS") files <- c(files, raw_product_file("US-BZF", "AmeriFlux_BASE", base_dir))
  if (site$site_ID == "FR-Pue") {
    files <- c(files, list.files(file.path(base_dir, "soil_water_content_FR-Pue"), pattern = "\\.csv$", full.names = TRUE))
  }
  unname(files)
}

# Where a site's raw data are, by input data version: the latest AmeriFlux
# releases are in data-raw/flux (R/download-flux.R); FLUXNET-format sites
# read their new product from there whatever this is (R/read-raw.R).
site_data_dir <- function(site, data_version = "published") {
  if (data_version == "update" && site$source == "AmeriFlux_BASE") path_raw("flux") else path_original("SiteData")
}

# Step-01 preparation of one site from its raw data (dispatching on the data
# product), returning list(ac, night, feature).
prepare_site <- function(site, ts_rf, coordinates, base_dir = path_original("SiteData")) {
  if (site$source == "AmeriFlux_BASE") {
    prep_site_ameriflux(site, ts_rf = ts_rf, coordinates = coordinates, base_dir = base_dir)
  } else {
    prep_site_euroflux(site, ts_rf = ts_rf, base_dir = base_dir)
  }
}

# Write a site's step-01 tables as parquet files; returns their paths.
write_step01_outputs <- function(prepared, site_id, dir = path_proc("RespirationData")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(dir, paste0(site_id, c("_ac.parquet", "_nightNEE.parquet", "_climatology.parquet")))
  arrow::write_parquet(prepared$ac, paths[1])
  arrow::write_parquet(prepared$night, paths[2])
  arrow::write_parquet(prepared$climatology, paths[3])
  paths
}

# Fit one TAS model to a site's prepared inputs.
fit_tas_target <- function(inputs, model_type, sampler = tas_fit_settings()) {
  use_stan_cache()
  s <- inputs$settings
  estimate_tas(s$site_ID, inputs$night, inputs$ac, inputs$ac_day, s$gStart, s$gEnd, s$tStart, s$tEnd,
               s$nobs_threshold, s$control_year, model_type = model_type, sampler = sampler)
}

# One row describing a site's prepared TAS inputs: its fit settings, the
# size of the nighttime table, the number of 14-day windows (as in
# estimate_tas()), and hashes of the tables, so that the inputs of two runs
# can be compared without keeping them.
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

# A site's published random-forest soil temperatures (01_01), or NULL if the
# site does not estimate soil temperature.
read_published_ts_rf <- function(site, files) {
  if (site$estimate_Ts != "YES") return(NULL)
  file <- files[basename(files) == paste0(site$site_ID, "_TS_rfp.csv")]
  if (length(file) != 1) stop("no published TS_RandomForest file for ", site$site_ID)
  utils::read.csv(file)
}

# Write the combined tables under the published file names in data-proc/:
# growing-season features (one table for all sites) and the total and direct
# outcome tables; and the summary of every site's TAS inputs. Returns the
# paths.
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

# Write one fit set's step 03-04 tables under the published file names, in
# data-proc/step03-04/<fit_set>/. Returns the paths.
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
