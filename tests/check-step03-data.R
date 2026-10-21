# Rebuild the driver-analysis table (03_01) with R/step03-drivers.R from the
# step-01 tables in data-proc/ and the published TAS estimates, and compare it
# with data/acclimation_data.csv.
#
# Usage: Rscript tests/check-step03-data.R

source(file.path("R", "paths.R"))
source(file.path("R", "io.R"))
source(file.path("R", "step03-drivers.R"))

site_info <- utils::read.csv(file.path("data", "site_info.csv"))
climate <- do.call(rbind, parallel::mclapply(site_info$site_ID, function(s) {
  site_climate_stats(s, read_step01_table(s, "ac"), read_step01_table(s, "nightNEE"))
}, mc.cores = 8))
soil <- soil_carbon(site_info, path_original("AMF_AA-Net_BIF_CCBY4_20251017.csv"), path_original("GSOCmap1.5.0.tif"))
spectral <- spectral_indices(path_original("towers-MOD13A2-061-results.csv"), path_original("towers-MOD15A2H-061-results.csv"),
                             path_original("towers-MYD17A2HGF-061-results.csv"))
new <- driver_data(site_info, climate, spectral, soil,
                   utils::read.csv(file.path("data", "outcome_temp.csv")), utils::read.csv(file.path("data", "outcome_temp_water_gpp.csv")))
published <- utils::read.csv(file.path("data", "acclimation_data.csv"))
stopifnot(identical(names(new), names(published)), identical(new$site_ID, published$site_ID))
worst <- vapply(names(new), function(n) {
  if (!is.numeric(new[[n]])) return(if (identical(as.character(new[[n]]), as.character(published[[n]]))) 0 else Inf)
  if (!identical(unname(is.na(new[[n]])), unname(is.na(published[[n]])))) return(Inf)
  max(abs(new[[n]] - published[[n]]) / pmax(1, abs(published[[n]])), 0, na.rm = TRUE)
}, numeric(1))
print(signif(worst, 3))
if (any(worst > 1e-9)) stop("driver table differs from the published one: ", paste(names(worst)[worst > 1e-9], collapse = ", "))
message("OK: driver table equals data/acclimation_data.csv (max relative difference ", signif(max(worst), 3), ")")
