# Soil-water column selection between `SWC_measured` (tower) and `SWC_era5`
# (ERA5-Land), both in percent. See docs/soil-temperature.md, "Soil water".

# Which column a run uses: measured where the site uses it; otherwise ERA5 for
# the direct model, and none for the total model, which has no soil water.
default_swc_col <- function(site_info, direct) {
  if (isTRUE(site_info[["SWC_use"]])) return("SWC_measured")
  if (isTRUE(direct)) return("SWC_era5")
  NA_character_
}

# Materialise the chosen column as `SWC`, the name the model formula uses.
resolve_swc_column <- function(dat, swc_col, name_site) {
  if (!swc_col %in% names(dat)) {
    stop(
      name_site, ": requested soil-water column ", shQuote(swc_col),
      " is not present. Available: ",
      paste(grep("^SWC", names(dat), value = TRUE), collapse = ", "),
      ". It has to be produced by `prep_nee_ac()`."
    )
  }
  # An all-NA ERA5 fallback means it is missing; say so here.
  if (identical(swc_col, "SWC_era5") && all(is.na(dat[[swc_col]]))) {
    stop(
      name_site, " has no measured soil water, so the direct model needs the ",
      "ERA5-Land fallback, but its ERA5 soil water is entirely NA. ",
      "`prep_nee_ac()` says which case this is when it builds the site: either ",
      "no rows for the site in ", file.path("data-raw", "ERA5_daily_swc.csv"),
      ", which `pixi run download_era5` fixes, or rows whose values are all NA, ",
      "which it does not -- see `read_era5_swc()`."
    )
  }
  dat[["SWC"]] <- dat[[swc_col]]
  dat
}
