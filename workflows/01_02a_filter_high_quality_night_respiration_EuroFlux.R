# Purpose of this script: prepare data for fitting temperature-respiration curves for EuroFlux sites. 
# Output of this script: two .csv files. One for filtered night NEE, and the other for gap-filled whole time-series.
# Author: Junna Wang
#  It will take ~ 30 mins to finish running this script. 

# Special attention: 
# site FR-Pue - soil water provided by PI
# sites in southern hemisphere ('AU-Tum', 'ZA-Kru') have growing seasons spanning two years. 
# we added 11 European sites in Aug. 2025

stopifnot(requireNamespace("dplyr"), requireNamespace("lubridate"))
source(file.path("R", "paths.R"))
source(file.path("R", "step01-prep.R"))

# Usage: Rscript workflows/01_02a_filter_high_quality_night_respiration_EuroFlux.R [--sites=SITE1,SITE2 | --combine-only]
#
# Prepares each requested site (default: every site with site_info$source != 'AmeriFlux_BASE' in
# data/site_info.csv), writing data-proc/RespirationData/<site>_ac.csv and
# <site>_nightNEE.csv and the site's growing-season features to
# data-proc/growing_season_feature_sites/<site>.rds. Sites are independent;
# after per-site runs, --combine-only (or a run over every site) writes data-proc/growing_season_feature_EuropFlux.csv.

args <- commandArgs(trailingOnly = TRUE)
sites_arg <- grep("^--sites=", args, value = TRUE)
combine_only <- "--combine-only" %in% args

site_info <- read.csv(file.path('data', 'site_info.csv'))
raw_files <- raw_site_files()
all_sites <- site_info$site_ID[site_info$source != 'AmeriFlux_BASE']

sites <- if (combine_only) character() else if (length(sites_arg) > 0) strsplit(sub("^--sites=", "", sites_arg), ",")[[1]] else all_sites
stopifnot(all(sites %in% all_sites))

for (name_site in sites) {
  print(name_site)
  result <- prep_site_euroflux(site_info[site_info$site_ID == name_site, ], raw_files)
  write.csv(result$ac, file=path_proc("RespirationData", paste0(name_site, '_ac.csv')), row.names = F)
  write.csv(result$night, file=path_proc("RespirationData", paste0(name_site, '_nightNEE.csv')), row.names = F)
  saveRDS(result$feature, path_proc("growing_season_feature_sites", paste0(name_site, ".rds")))
}

# Combine only in a run over every site, or when asked to (--combine-only)
# after per-site runs in separate processes.
feature_files <- path_proc("growing_season_feature_sites", paste0(all_sites, ".rds"))
if (length(sites_arg) > 0 && !combine_only) {
  # per-site run: nothing to combine
} else if (all(file.exists(feature_files))) {
  write.csv(combine_features(lapply(feature_files, readRDS)), file=path_proc('growing_season_feature_EuropFlux.csv'), row.names = F)
} else {
  message("Not every site has been prepared yet; growing_season_feature_EuropFlux.csv not written.")
}
