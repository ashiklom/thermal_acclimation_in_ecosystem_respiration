# Soil water column selection.
#
# Two sources are carried side by side on every table `prep_nee_ac()` returns:
# `SWC_measured` from the flux tower, and `SWC_era5` from ERA5-Land reanalysis.
# Both are in PERCENT (0-100); see `ERA5_SWC_TO_PERCENT`.
#
# Keeping both means the choice is a selection at model-fitting time rather
# than a destructive join, and it makes the two directly comparable at sites
# that have both -- which is most of them, including sites flagged
# `SWC_use == "NO"` that nevertheless report a soil water column.

# Which column a run should use. Depends on the model as well as the site: the
# direct model has soil water in its formula and needs a value everywhere, so a
# site with no measured soil water falls back to reanalysis; the total model
# does not use soil water and needs no fallback.
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
  # Scoped to the reanalysis column on purpose. An empty `SWC_era5` means the
  # fallback the direct model depends on is simply missing, which used to
  # surface as an error from `read_era5_swc()` at this same point. An empty
  # `SWC_measured` is a different situation and is left to behave as before.
  if (identical(swc_col, "SWC_era5") && all(is.na(dat[[swc_col]]))) {
    stop(
      name_site, " has no measured soil water, so the direct model needs the ",
      "ERA5-Land fallback, but no ERA5 soil water was found for it in ",
      file.path("data-raw", "ERA5_daily_swc.csv"),
      ". Run `pixi run download_era5` for this site."
    )
  }
  dat[["SWC"]] <- dat[[swc_col]]
  dat
}
