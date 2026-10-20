# This script estimates thermal acclimation strength in ecosystem respiration using moving window methods.
# Authors: Junna Wang, August and September, 2025
# Methods we tested:
# Step 1: determine the window size; 2 weeks ~ 4 weeks
# Step 2: for a specific year, do we need to  due to data limitation (such as no observations during a period). 
# Step 3: use all the data within that moving window to build an ER model. Use these parameter values as initial values of each year. 
# Step 4: find a method to estimate missed values. It is likely linear interpolation. 

# It takes about 2 days to finish the estimate of TAS; but on other computers it could take one week, depending on number of cores used.  
# please install 'cmdstanr' before you run this script. 

stopifnot(requireNamespace("brms"), requireNamespace("caret"), requireNamespace("dplyr"), requireNamespace("gslnls"), requireNamespace("lubridate"), requireNamespace("nlme"), requireNamespace("zoo"))
source(file.path("R", "paths.R"))
source(file.path("R", "step02-tas.R"))

# Usage: Rscript workflows/02_01b_estimate_direct_TAS_use_moving_window.R [--sites=SITE1,SITE2 | --combine-only | --compile-only]
#
# Estimates direct TAS for each requested site (default: every site in
# data/site_info.csv) and saves the result to data-proc/TAS_sites/direct/<site>.rds.
# Sites are independent, so they can run in separate processes; after
# per-site runs, --combine-only (or a run over every site) writes data-proc/outcome_temp_water_gpp.csv and outcome_siteyear_temp_water_gpp.csv.

args <- commandArgs(trailingOnly = TRUE)
sites_arg <- grep("^--sites=", args, value = TRUE)
combine_only <- "--combine-only" %in% args
compile_only <- "--compile-only" %in% args

set.seed(123)

# Compile both models into data-proc/stan-cache/ unless already there
# (--compile-only: do just that, e.g. before starting parallel per-site runs).
use_stan_cache()
if (!combine_only) compile_tas_models()
if (compile_only) quit(save = "no")

site_info <- read.csv(file.path('data', 'site_info.csv'))
feature_gs <- read.csv(path_proc('growing_season_feature_EuropFlux.csv'))
feature_gs_AmeriFlux <- read.csv(path_proc('growing_season_feature_AmeriFlux.csv'))
feature_gs <- rbind(feature_gs, feature_gs_AmeriFlux)
swc_ERA5 <- read.csv(path_original("ERA5_daily_swc_1990_2024_allsites.csv"))
swc_ERA5$date <- as.Date(swc_ERA5$date)
swc_ERA5$YEAR <- lubridate::year(swc_ERA5$date)
swc_ERA5$MONTH <- lubridate::month(swc_ERA5$date)
swc_ERA5$DAY <- lubridate::day(swc_ERA5$date)

sites <- if (combine_only) character() else if (length(sites_arg) > 0) strsplit(sub("^--sites=", "", sites_arg), ",")[[1]] else site_info$site_ID
stopifnot(all(sites %in% site_info$site_ID))

for (name_site in sites) {
  print(name_site)
  a_measure_night_complete <- read.csv(path_proc("RespirationData", paste0(name_site, '_nightNEE.csv')))
  ac <- read.csv(path_proc("RespirationData", paste0(name_site, '_ac.csv')))
  result <- estimate_direct_tas_site(site_info[site_info$site_ID == name_site, ], feature_gs, a_measure_night_complete, ac, swc_ERA5)
  saveRDS(result, path_proc("TAS_sites", "direct", paste0(name_site, ".rds")))
}

# Combine only in a run over every site, or when asked to (--combine-only)
# after per-site runs in separate processes.
site_files <- path_proc("TAS_sites", "direct", paste0(site_info$site_ID, ".rds"))
if (length(sites_arg) > 0 && !combine_only) {
  # per-site run: nothing to combine
} else if (all(file.exists(site_files))) {
  combined <- combine_tas_sites(lapply(site_files, readRDS))
  write.csv(combined$outcome, file = path_proc('outcome_temp_water_gpp.csv'), row.names = F)
  write.csv(combined$siteyear, file = path_proc('outcome_siteyear_temp_water_gpp.csv'), row.names = F)
} else {
  message("Not every site has a result yet; ", basename(path_proc('outcome_temp_water_gpp.csv')), " not written.")
}
