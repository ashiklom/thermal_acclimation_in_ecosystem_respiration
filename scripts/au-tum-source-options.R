# AU-Tum's TAS under each candidate source of its latest record (refactor-v2
# stage S18; the analysis website's AU-Tum page): step 01 and the main TAS
# fits, as the pipeline runs them for the update (R/pipeline.R), with only
# the site's `source` changed.
#
#   published      FLUXNET2015 (ONEFlux, NEE_VUT_REF), 2001-2014
#   F15_TERN       FLUXNET2015, then TERN L6 (PyFluxPro) after its end
#   TERN           TERN L6 alone, 2002-2026
#
# Usage (from the repository root):
#   Rscript scripts/au-tum-source-options.R [--options=published,F15_TERN,TERN] [--out=data-proc/au-tum-options]
# Writes <out>/<option>/: outcome_{total,direct}.csv, siteyear_{total,direct}.csv
# and tas_inputs_summary.csv. TAS_FIT_PROFILE as for the pipeline (default full).

for (f in c("paths", "io", "read-raw", "sun-times", "step01-prep", "ts-qualify", "step02-prep", "step02-tas", "pipeline")) {
  source(file.path("R", paste0(f, ".R")))
}

SOURCES <- c(published = "FLUXNET2015", F15_TERN = "FLUXNET2015+TERN", TERN = "TERN")

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit) else default
}
options <- strsplit(opt("options", paste(names(SOURCES), collapse = ",")), ",")[[1]]
stopifnot(all(options %in% names(SOURCES)))
out <- opt("out", path_proc("au-tum-options"))

site_info <- utils::read.csv(file.path("data", "site_info_update.csv"))
settings <- read_site_settings()
settings <- settings[settings$site_ID == "AU-Tum", ]
coordinates <- utils::read.csv(file.path("data", "ameriflux_coordinates.csv"))
era5 <- extend_era5_swc(read_era5_swc(), read_era5_swc(path_raw("era5", "ERA5_daily_swc_update.csv")))
sampler <- tas_fit_settings()
use_stan_cache()
compile_tas_models()

for (option in options) {
  site <- site_info[site_info$site_ID == "AU-Tum", ]
  site$source <- SOURCES[[option]]
  message(option, ": source ", site$source)
  step01 <- prepare_site(site, NULL, coordinates, site_data_dir(site, "update"))
  inputs <- prepare_tas_inputs(site, settings, step01$feature, step01$night, step01$ac, era5, step01$climatology)
  dir <- file.path(out, option)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(cbind(option = option, source = site$source, summarise_tas_inputs(inputs)),
                   file.path(dir, "tas_inputs_summary.csv"), row.names = FALSE)
  fits <- parallel::mclapply(c("total", "direct"), function(m) fit_tas_target(inputs, m, sampler), mc.cores = 2)
  for (i in 1:2) {
    m <- c("total", "direct")[i]
    utils::write.csv(fits[[i]]$outcome, file.path(dir, paste0("outcome_", m, ".csv")), row.names = FALSE)
    utils::write.csv(fits[[i]]$siteyear, file.path(dir, paste0("siteyear_", m, ".csv")), row.names = FALSE)
  }
  message(option, ": done")
}
