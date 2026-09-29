# Monthly nighttime (minimum) air temperature change from the current period
# (2000-2020) to the future (2041-2060) under SSP2-4.5, averaged over 13
# CMIP6 GCMs, at every site. Reads the WorldClim 2.1 tmin rasters the
# pipeline's `worldclim_files` target downloads; writes
# data-proc/analysis/Tmin_month_ssp245_wc.csv.
# Authors: Junna Wang, October, 2025

stopifnot(requireNamespace("terra"))

dir_rawdata <- "data-raw"

site_info <- read.csv(file.path("data-core", "site_info.csv"))
xy <- data.frame(x = site_info$LONG, y = site_info$LAT)

#----------------Step 1: monthly temperature of the current period
dir_base <- file.path(dir_rawdata, "Climate", "wc2.1_2.5m_tmin")
files <- list.files(dir_base, pattern = "[.]tif$")
# One layer per calendar month: the mean of that month's rasters over the years.
base_line <- terra::rast(lapply(1:12, function(i) {
  month_files <- files[grepl(sprintf("-%02d[.]tif$", i), files)]
  terra::app(terra::rast(file.path(dir_base, month_files)), fun = mean)
}))
names(base_line) <- 1:12

#----------------Step 2: monthly temperature of the future period under emissions scenario
files <- list.files(
  file.path(dir_rawdata, "Climate", "wc2.1_2.5m_tmin_2041-2060"),
  pattern = "[.]tif$", full.names = TRUE
)
for (ssp in c("ssp245")) {
  gcms <- lapply(files[grepl(ssp, files)], terra::rast)
  # Layer i is month i; average it across the GCMs.
  future <- terra::rast(lapply(seq_len(terra::nlyr(gcms[[1]])), function(i) {
    terra::app(terra::rast(lapply(gcms, function(x) x[[i]])), fun = mean)
  }))
  Tmin_change2010_2050 <- future - base_line

  #----------------extract temperature change at our study sites----------------
  # Each site's cell and its rook neighbours, averaged.
  icell <- terra::adjacent(
    Tmin_change2010_2050, terra::cellFromXY(Tmin_change2010_2050, xy), include = TRUE
  )
  tmp <- terra::extract(Tmin_change2010_2050, c(icell))
  # One column per month; an extra ID column would shift every month.
  stopifnot(ncol(tmp) == 12)
  # `c(icell)` runs column by column, so row k of `tmp` belongs to site
  # ((k - 1) %% nrow(xy)) + 1; rows follow site_info.
  site_of_row <- rep(seq_len(nrow(xy)), times = ncol(icell))
  Tmin_month <- data.frame(site_ID = site_info$site_ID)
  for (i in 1:12) {
    Tmin_month[[paste0("Tmin", i)]] <- as.vector(tapply(tmp[, i], site_of_row, mean, na.rm = TRUE))
  }
  dir.create("data-proc/analysis", recursive = TRUE, showWarnings = FALSE)
  write.csv(Tmin_month, file = file.path("data-proc", "analysis", paste0("Tmin_month_", ssp, "_wc.csv")),
            row.names = FALSE)
}
