library(targets)
library(tarchetypes)
library(crew)

tar_option_set(
  error = "continue",
  controller = crew_controller_local(workers = 10)
)

tar_source()

all_site_info <- get_site_info()

# Use static branching to get informative target names, and to have finer
# control over which sites I run while developing.
# https://books.ropensci.org/targets/static.html
values <- tibble::tibble(
  site_name = all_site_info |>
    dplyr::filter(
      .data$source %in% c("ICOS", "TERN", "FLUXNET"),
      .data$LAT > 0
    ) |>
    dplyr::pull("site_ID")
)

site_targets <- tar_map(
  values = values,
  tar_target(site_data, prep_nee_ac(site_name), format = "qs"),
  tar_target(site_tas_total, total_tas_site(site_data), format = "qs"),
  tar_target(site_tas_direct, total_tas_site(site_data, direct = TRUE), format = "qs")
)

list(
  site_targets,
  NULL
)
