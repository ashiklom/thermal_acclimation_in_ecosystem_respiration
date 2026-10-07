# Soil-water column selection between `SWC_measured` (tower) and `SWC_era5`
# (ERA5-Land), both in percent. See docs/soil-temperature.md, "Soil water".

#' Which soil-water column a run uses
#'
#' Measured where the site uses it; otherwise ERA5 for the direct model, and
#' none for the total model, which has no soil water.
#'
#' @param site_info One row of the site declaration table.
#' @param direct Whether the model is the direct model (`TRUE`) rather than the
#'   total model.
#' @return `"SWC_measured"`, `"SWC_era5"`, or `NA_character_` for no column.
default_swc_col <- function(site_info, direct) {
  if (isTRUE(site_info[["SWC_use"]])) return("SWC_measured")
  if (isTRUE(direct)) return("SWC_era5")
  NA_character_
}

#' Materialise the chosen column as `SWC`, the name the model formula uses
#'
#' @param dat A site's flux table (`ac` or the nighttime NEE table), with the
#'   `SWC_*` columns `prep_nee_ac()` produces.
#' @param swc_col Name of the soil-water column to use, e.g. `"SWC_measured"`
#'   or `"SWC_era5"`.
#' @param name_site Site ID.
#' @return `dat` with an `SWC` column copied from `swc_col` (percent). Errors if
#'   the column is absent, or is the ERA5 fallback and entirely NA.
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
      ", which `pixi run download-era5` fixes, or rows whose values are all NA, ",
      "which it does not -- see `read_era5_swc()`."
    )
  }
  dat[["SWC"]] <- dat[[swc_col]]
  dat
}
