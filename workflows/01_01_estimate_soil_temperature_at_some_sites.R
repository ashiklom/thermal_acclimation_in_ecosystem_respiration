# title: Estimate Soil Temperature Based on Air Temp and net Radiation using Random Forests or linear regression for 16 sites

#  Description
#  this script intends to estimate surface soil temperature at sites with lots of missing soil temperature data
#  the method is random forests for 8 sites ("DE-Akm", "FR-Pue", "US-Los", "US-SRG", "CA-Man", "US-Ced", "US-Ho1", "CZ-RAJ"), 
#  but for the remaining sites, we used linear regression due to limited (1-2 years) TS observations or not enough observation of net radiation.
#   
#  Author: Junna Wang, July 2025
#  It will take ~40 min to finish running this script. 

stopifnot(requireNamespace("amerifluxr"), requireNamespace("caret"), requireNamespace("dplyr"), requireNamespace("lubridate"), requireNamespace("randomForest"))
source(file.path("R", "paths.R"))
source(file.path("R", "read-raw.R"))
source(file.path("R", "step01-prep.R"))

# Usage: Rscript workflows/01_01_estimate_soil_temperature_at_some_sites.R [--sites=US-Ha1,FR-Pue]
#
# Estimates soil temperature for each requested site (default: every site
# with estimate_Ts == "YES" in data/site_info.csv) and writes
# data-proc/TS_RandomForest/<site>_TS_rfp.csv. Sites are independent.

args <- commandArgs(trailingOnly = TRUE)
sites_arg <- grep("^--sites=", args, value = TRUE)

site_info <- read.csv(file.path('data', 'site_info.csv'))

sites <- if (length(sites_arg) > 0) strsplit(sub("^--sites=", "", sites_arg), ",")[[1]] else site_info$site_ID[site_info$estimate_Ts == 'YES']
stopifnot(all(sites %in% site_info$site_ID[site_info$estimate_Ts == 'YES']))

for (name_site in sites) {
  print(name_site)
  data <- estimate_ts_site(site_info[site_info$site_ID == name_site, ])
  write.csv(data, file=path_proc("TS_RandomForest", paste0(name_site, '_TS_rfp.csv')), row.names = F)
}
