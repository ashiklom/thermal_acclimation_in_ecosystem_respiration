library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

tar_source()

# Each brms fit runs `N_CORES` chains in parallel, so `workers * N_CORES` is the
# peak thread demand. But a fit is not 4-cores-busy for its whole life -- much
# of a target is serial R work -- so sizing workers at cores/N_CORES leaves the
# machine idle and costs wall time. Measured on the six-site sample, 18 cores:
#
#   workers=10  load avg 60  sum of target times 223.0 min  wall 49.9 min
#   workers=4   load avg 13  sum of target times 154.0 min  wall 59.8 min
#
# So 4 made each fit 31% faster and the whole run 20% slower: 12 targets over 4
# workers is three scheduling waves, where 10 ran nearly all at once. 8 is the
# midpoint and is *not yet benchmarked* -- worth timing the next time a full
# run happens anyway.
local <- crew_controller_local(workers = 8)
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
  # One read of the site declaration per site, threaded into every stage that
  # needs it. It depends on `site_info_file`, so editing site_info.csv
  # invalidates exactly the sites whose row could have changed.
  tar_target(site_info, get_site_info(site_name, path = site_info_file)),
  tar_target(site_dl, download_site(site_info), format = "file"),
  tar_target(site_data, {site_dl; prep_nee_ac(site_info)}, format = "qs"),
  tar_target(site_tas_total, total_tas_site(site_data, site_info), format = "qs"),
  tar_target(
    site_tas_direct,
    total_tas_site(site_data, site_info, direct = TRUE),
    format = "qs"
  )
)

list(
  tar_file(site_info_file, SITE_INFO_CSV),
  site_targets
)
