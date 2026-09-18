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
# No global `cue = tar_cue("never")`. It was a development escape hatch from
# when every run meant hours of Stan sampling, but as a default it means a code
# fix to `prep_nee_ac()` invalidates nothing downstream, and the results table
# ends up assembled from two different versions of the code. targets already
# skips unchanged work; the way to make a run affordable is to run fewer sites,
# which is what `pipeline_sites()` does.
tar_option_set(
  error = "continue",
  controller = if (grepl("ycrc.yale.edu", fqdn, fixed = TRUE)) slurm else local
)

# Use static branching to get informative target names, and to have finer
# control over which sites I run while developing.
# https://books.ropensci.org/targets/static.html
#
# `pipeline_sites()` returns the six-site development sample by default and the
# full list under THERMAL_SITES=all. See `DEV_SITES` in R/constants.R for what
# each of the six is there to cover.
values <- tibble::tibble(site_name = pipeline_sites())
message("Pipeline sites (", nrow(values), "): ", paste(values$site_name, collapse = ", "))

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
