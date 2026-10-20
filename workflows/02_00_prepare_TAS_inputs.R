# This script prepares each site's inputs to the TAS fits (02_01a, 02_01b)
# from its step-01 outputs: all site-specific choices (soil-temperature
# regression, soil water source, trimmed growing years, temperature bounds,
# control year) are made here; see R/step02-prep.R.

stopifnot(requireNamespace("dplyr"), requireNamespace("lubridate"), requireNamespace("zoo"))
source(file.path("R", "paths.R"))
source(file.path("R", "step02-prep.R"))

# Usage: Rscript workflows/02_00_prepare_TAS_inputs.R [--sites=SITE1,SITE2]
#
# Writes data-proc/TAS_inputs/<site>.rds for each requested site (default:
# every site in data/site_info.csv). Sites are independent.

args <- commandArgs(trailingOnly = TRUE)
sites_arg <- grep("^--sites=", args, value = TRUE)

site_info <- read.csv(file.path('data', 'site_info.csv'))
site_settings <- read_site_settings()
feature_gs <- read.csv(path_proc('growing_season_feature_EuropFlux.csv'))
feature_gs_AmeriFlux <- read.csv(path_proc('growing_season_feature_AmeriFlux.csv'))
feature_gs <- rbind(feature_gs, feature_gs_AmeriFlux)
swc_ERA5 <- read_era5_swc()

sites <- if (length(sites_arg) > 0) strsplit(sub("^--sites=", "", sites_arg), ",")[[1]] else site_info$site_ID
stopifnot(all(sites %in% site_info$site_ID), setequal(site_settings$site_ID, site_info$site_ID))

for (name_site in sites) {
  print(name_site)
  a_measure_night_complete <- read.csv(path_proc("RespirationData", paste0(name_site, '_nightNEE.csv')))
  ac <- read.csv(path_proc("RespirationData", paste0(name_site, '_ac.csv')))
  inputs <- prepare_tas_inputs(site_info[site_info$site_ID == name_site, ], site_settings[site_settings$site_ID == name_site, ],
                               feature_gs, a_measure_night_complete, ac, swc_ERA5)
  saveRDS(inputs, path_proc("TAS_inputs", paste0(name_site, ".rds")))
}
