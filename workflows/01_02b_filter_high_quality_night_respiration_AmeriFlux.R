# Purpose of this script: prepare for data for fitting temperature-respiration curves for AmeriFlux sites. 
# Output of this script: two .csv files. One for filtered night NEE, and the other for gap-filled whole time-series.
# Author: Junna Wang
#  It will take ~2 hours to finish running this script. 

# Special attention: 
# SWC measurements of some old sites is every 4 hours, so these SWC data needs to interpolate. 
# we added 6 more Ameriflux sites into the data in Aug. 2025
# we added 2 more tropical Ameriflux sites into the data in Dec. 2025

stopifnot(requireNamespace("amerifluxr"), requireNamespace("dplyr"), requireNamespace("lubridate"), requireNamespace("lutz"), requireNamespace("REddyProc"), requireNamespace("suncalc"), requireNamespace("zoo"))
source(file.path("R", "paths.R"))
source(file.path("R", "io.R"))
source(file.path("R", "sun-times.R"))
source(file.path("R", "read-raw.R"))
source(file.path("R", "step01-prep.R"))
source(file.path("R", "ts-qualify.R"))

# Usage: Rscript workflows/01_02b_filter_high_quality_night_respiration_AmeriFlux.R [--sites=SITE1,SITE2 | --combine-only] [--ts-rf=published|regenerated]
#
# Prepares each requested site (default: every site with site_info$source == 'AmeriFlux_BASE' in
# data/site_info.csv), writing data-proc/RespirationData/<site>_ac.parquet,
# <site>_nightNEE.parquet and <site>_climatology.parquet and the site's growing-season features to
# data-proc/growing_season_feature_sites/<site>.rds. Sites are independent;
# after per-site runs, --combine-only (or a run over every site) writes data-proc/growing_season_feature_AmeriFlux.parquet.

args <- commandArgs(trailingOnly = TRUE)
sites_arg <- grep("^--sites=", args, value = TRUE)
combine_only <- "--combine-only" %in% args
# where estimated soil temperature (01_01) comes from; see read_ts_rf()
ts_rf_arg <- grep("^--ts-rf=", args, value = TRUE)
ts_rf_source <- if (length(ts_rf_arg) > 0) sub("^--ts-rf=", "", ts_rf_arg) else "published"

site_info <- read.csv(file.path('data', 'site_info.csv'))
coordinates <- read.csv(file.path('data', 'ameriflux_coordinates.csv'))
all_sites <- site_info$site_ID[site_info$source == 'AmeriFlux_BASE']

sites <- if (combine_only) character() else if (length(sites_arg) > 0) strsplit(sub("^--sites=", "", sites_arg), ",")[[1]] else all_sites
stopifnot(all(sites %in% all_sites))

for (name_site in sites) {
  print(name_site)
  site <- site_info[site_info$site_ID == name_site, ]
  result <- prep_site_ameriflux(site, ts_rf = read_ts_rf(site, ts_rf_source), coordinates = coordinates)
  write_table(result$ac, path_proc("RespirationData", paste0(name_site, '_ac.parquet')))
  write_table(result$night, path_proc("RespirationData", paste0(name_site, '_nightNEE.parquet')))
  write_table(result$climatology, path_proc("RespirationData", paste0(name_site, '_climatology.parquet')))
  saveRDS(result$feature, path_proc("growing_season_feature_sites", paste0(name_site, ".rds")))
}

# Combine only in a run over every site, or when asked to (--combine-only)
# after per-site runs in separate processes.
feature_files <- path_proc("growing_season_feature_sites", paste0(all_sites, ".rds"))
if (length(sites_arg) > 0 && !combine_only) {
  # per-site run: nothing to combine
} else if (all(file.exists(feature_files))) {
  write_table(combine_features(lapply(feature_files, readRDS)), path_proc('growing_season_feature_AmeriFlux.parquet'))
} else {
  message("Not every site has been prepared yet; growing_season_feature_AmeriFlux.parquet not written.")
}
