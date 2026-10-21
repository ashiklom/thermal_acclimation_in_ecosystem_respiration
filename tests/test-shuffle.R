# The data preparation must not depend on the order of rows or columns in its
# input tables. Runs step-01 site preparation (on the raw tables) and step-02
# input preparation (on the step-01 tables) twice for a few sites: once on
# the tables as read, once with their rows and columns randomly permuted.
# Requires identical results.
#
# Usage (from the repository root):
#   Rscript tests/test-shuffle.R [--steps=01,02] [--sites01=FI-Sod,US-Jo2] [--sites02=AU-Tum,US-Tw1]

source(file.path("R", "paths.R"))
source(file.path("R", "io.R"))
source(file.path("R", "read-raw.R"))
source(file.path("R", "sun-times.R"))
source(file.path("R", "step01-prep.R"))
source(file.path("R", "step02-prep.R"))

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  strsplit(if (length(hit) > 0) sub(paste0("^--", name, "="), "", hit) else default, ",")[[1]]
}
steps <- opt("steps", "01,02")
sites01 <- opt("sites01", "FI-Sod,GF-Guy,US-Jo2,US-Myb,AU-Tum")
sites02 <- opt("sites02", "AU-Tum,US-Tw1,US-GLE,ZA-Kru,US-Uaf")

shuffle <- function(x) {
  x <- x[sample(nrow(x)), sample(ncol(x)), drop = FALSE]
  rownames(x) <- NULL
  x
}

site_info <- utils::read.csv(file.path("data", "site_info.csv"))
n_bad <- 0
# Row names are not data (the CSV outputs never include them).
drop_rownames <- function(x) {
  if (is.data.frame(x)) {
    rownames(x) <- NULL
    x
  } else if (is.list(x)) {
    lapply(x, drop_rownames)
  } else {
    x
  }
}
check <- function(label, a, b) {
  a <- drop_rownames(a)
  b <- drop_rownames(b)
  if (identical(a, b)) {
    message("ok        ", label)
  } else {
    n_bad <<- n_bad + 1
    message("DIFFERENT ", label, ": ", paste(utils::head(all.equal(a, b), 3), collapse = "; "))
  }
}

set.seed(20261020)

if ("01" %in% steps) {
  plain_fluxnet <- read_fluxnet
  plain_base <- read_ameriflux_base
  for (s in sites01) {
    site <- site_info[site_info$site_ID == s, ]
    prep <- if (site$source == "AmeriFlux_BASE") prep_site_ameriflux else prep_site_euroflux
    read_fluxnet <- plain_fluxnet
    read_ameriflux_base <- plain_base
    a <- prep(site)
    read_fluxnet <- function(...) shuffle(plain_fluxnet(...))
    read_ameriflux_base <- function(...) shuffle(plain_base(...))
    b <- prep(site)
    check(paste("step 01", s), a, b)
  }
  read_fluxnet <- plain_fluxnet
  read_ameriflux_base <- plain_base
}

if ("02" %in% steps) {
  settings <- read_site_settings()
  feature_gs <- read_growing_season_features()
  swc_ERA5 <- read_era5_swc()
  for (s in sites02) {
    night <- read_step01_table(s, "nightNEE")
    ac <- read_step01_table(s, "ac")
    args_plain <- list(site_info[site_info$site_ID == s, ], settings[settings$site_ID == s, ], feature_gs, night, ac, swc_ERA5)
    args_shuffled <- list(args_plain[[1]], args_plain[[2]], shuffle(feature_gs), shuffle(night), shuffle(ac), shuffle(swc_ERA5))
    check(paste("step 02 inputs", s), do.call(prepare_tas_inputs, args_plain), do.call(prepare_tas_inputs, args_shuffled))
  }
}

if (n_bad > 0) stop(n_bad, " preparation(s) depend on row or column order")
message("OK: preparation independent of row and column order")
