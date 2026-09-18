library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

tar_source()

local <- crew_controller_local(workers = 10)
slurm <- crew_controller_slurm(
  workers = 20,
  options_cluster = crew_options_slurm(
    time_minutes = 12*60,
    n_tasks = 4,
    verbose = TRUE
  )
)

fqdn <- system2("hostname", stdout = TRUE)
tar_option_set(
  error = "continue",
  controller = if (grepl("ycrc.yale.edu", fqdn, fixed = TRUE)) slurm else local,
  cue = tar_cue("never")
)

all_site_info <- get_site_info()

# Use static branching to get informative target names, and to have finer
# control over which sites I run while developing.
# https://books.ropensci.org/targets/static.html
values <- tibble::tibble(
  site_name = all_site_info |>
    dplyr::filter(
      # Every site the FLUXNET-family reader handles, i.e. not AmeriFlux BASE.
      # Matched by substring because `source` is a `+`-separated provenance
      # list (e.g. "WW2020+FLUXNET+ICOS"); see FLUX_PRODUCTS in R/constants.R.
      !grepl("AmeriFlux_BASE", .data$source, fixed = TRUE),
      .data$LAT > 0
    ) |>
    dplyr::pull("site_ID")
)

site_targets <- tar_map(
  values = values,
  tar_target(site_dl, download_site(site_name) ,format = "file"),
  tar_target(site_data, {site_dl; prep_nee_ac(site_name)}, format = "qs"),
  tar_target(site_tas_total, total_tas_site(site_data), format = "qs"),
  tar_target(site_tas_direct, total_tas_site(site_data, direct = TRUE), format = "qs")
)

list(
  site_targets,
  NULL
)
