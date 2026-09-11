library(targets)
library(tarchetypes)

# tar_source()
source("R/utils.R")
source("R/prepare-site-data.R")

list(
  tar_target(all_site_info, get_site_info()),
  tar_target(site_names, all_site_info[["site_ID"]]),
  tar_target(site_data, prepare_nee_ac(site_names), pattern = map(site_names), format = "qs"),
  NULL
)
