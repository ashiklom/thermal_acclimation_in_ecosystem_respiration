library(targets)
library(tarchetypes)

tar_source()

tar_option_set(error = "continue")

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
  tar_target(site_data, prep_nee_ac(site_name), format = "qs", cue = tar_cue("never"))
)

list(
  site_targets,
  NULL
)

# list(
#   tar_target(all_site_info, get_site_info()),
#   # Some test sites
#   tar_target(site_names, all_site_info[["site_ID"]]),
#   tar_target(site_data, prep_nee_ac(site_names), pattern = map(site_names), format = "qs"),
#   NULL
# )
