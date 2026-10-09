# The `TS_linear` substitution (its fit domain and write-back) and the two
# bounds definitions every candidate column carries. The `TS ~ TA` line is
# `lm_ta` in R/ts-estimators.R.

#' The rows a site's `TS ~ TA` regression is fitted on
#'
#' The rows a site's regression is fitted on (`site_info$ts_linear_domain`):
#' `ac` above freezing everywhere but US-Tw1, which fits on the nighttime table.
#' `ac[ac$TA > 0, ]` is verbatim from the original; its NA-TA rows come back
#' all-NA and `na.omit` drops them, so the coefficients are unaffected.
#'
#' @param ac Data frame of the site's half-hourly data, with `TA`.
#' @param nightNEE Data frame of the site's nighttime NEE observations.
#' @param domain `"ac"` or `"night"`, from `ts_linear_domain_for()`.
#' @return The rows of `ac` with `TA > 0`, or `nightNEE`. Errors if `domain` is
#'   missing or unknown.
ts_fit_data <- function(ac, nightNEE, domain) {
  if (length(domain) != 1 || is.na(domain)) {
    stop(
      "A site selected for TS_linear must declare ts_linear_domain; got ",
      if (length(domain) == 1) "NA" else paste0("length ", length(domain)), "."
    )
  }
  switch(
    domain,
    # using data with TA > 0, because we focus on grouping season
    ac = ac[ac$TA > 0, ],
    # slope will be too low if using ac data for the subtropical wetland sites.
    night = nightNEE,
    stop("Unknown ts_linear_domain: ", shQuote(domain), ". Expected \"ac\" or \"night\".")
  )
}

#' Write an estimate back over a soil-temperature column
#'
#' Write an estimate back over a soil-temperature column, in a named mode:
#'
#'   replace    the estimate wholesale, NAs included (whole-column
#'              reconstructions, the depth swap, the air-temperature substitute)
#'   overlay    the estimate where it exists, the original elsewhere: a hybrid.
#'              The manuscript's `TS_linear`; wholesale would introduce NAs
#'              the step-01 filters have certified absent.
#'   fill_gaps  the estimate only where the original is missing (the PI and
#'              US-MBP gap-fills, the predictor climatologies)
#'
#' The mode travels in the provenance row.
#'
#' @param ts The column to write over.
#' @param estimate The estimate, aligned one to one with `ts`.
#' @param mode `"replace"`, `"overlay"` or `"fill_gaps"`; no default.
#' @return The written-back column, the same length as `ts`.
write_back_ts <- function(ts, estimate, mode) {
  # No default: a call that omits the mode fails.
  mode <- match.arg(mode, c("replace", "overlay", "fill_gaps"))
  if (length(estimate) != length(ts)) {
    stop(
      "write_back_ts: the estimate has ", length(estimate), " values for ",
      length(ts), " rows; the two have to align one to one."
    )
  }
  switch(
    mode,
    replace = estimate,
    overlay = {
      ts[!is.na(estimate)] <- estimate[!is.na(estimate)]
      ts
    },
    fill_gaps = {
      ts[is.na(ts)] <- estimate[is.na(ts)]
      ts
    }
  )
}

#' The half-hourly definition of the bounds
#'
#' 2.5/97.5 percentiles of growing-season soil temperature, which gate the
#' window-skip test. They describe one column only, so each candidate gets its
#' own.
#'
#' @param ts Soil temperature, one value per half-hour.
#' @param doy Day of year of each value of `ts`, possibly wrapped.
#' @param gStart,gEnd Growing-season bounds, as (possibly wrapped) DOY, inclusive.
#' @return A list of `tStart` and `tEnd`, the 2.5% and 97.5% quantiles.
ts_bounds <- function(ts, doy, gStart, gEnd) {
  gs <- ts[dplyr::between(doy, gStart, gEnd)]
  list(
    tStart = unname(quantile(gs, 0.025, na.rm = TRUE)),
    tEnd = unname(quantile(gs, 0.975, na.rm = TRUE))
  )
}

#' The fitting domain for a site's `TS_linear`
#'
#' A site that selects it must declare one (returned as is, NA included, so
#' `ts_fit_data()` rejects a missing one). Elsewhere the column is only a
#' diagnostic and defaults to `"ac"`, as 34 of the 35 declared sites have it.
#'
#' @param site_info One row of the site declaration table.
#' @return `"ac"` or `"night"`, or NA for a site that selects `TS_linear`
#'   without declaring one.
ts_linear_domain_for <- function(site_info) {
  domain <- site_info[["ts_linear_domain"]]
  if (identical(site_info[["ts_col"]], "TS_linear")) {
    return(domain)
  }
  if (length(domain) == 1 && !is.na(domain)) domain else "ac"
}

#' Apply the `TS_linear` substitution
#'
#' Substitute the TS ~ TA regression for measured soil temperature in both
#' tables, and recompute the bounds on the new scale.
#'
#' @param ac Data frame of the site's half-hourly data, with `TA`, `TS` and
#'   `DOY`.
#' @param nightNEE Data frame of the site's nighttime NEE observations, with
#'   `TA` and `TS`.
#' @param site_info One row of the site declaration table.
#' @param gStart,gEnd Growing-season bounds, as (possibly wrapped) DOY.
#' @return A list: `ac` and `nightNEE` with `TS` overlaid by the regression, and
#'   `tStart` and `tEnd`, the half-hourly bounds of the new `ac$TS`.
apply_ts_linear <- function(ac, nightNEE, site_info, gStart, gEnd) {
  mod <- ts_ta_model(ts_fit_data(ac, nightNEE, ts_linear_domain_for(site_info)))
  nightNEE$TS <- write_back_ts(nightNEE$TS, predict_ts_from_ta(mod, nightNEE$TA), "overlay")
  ac$TS <- write_back_ts(ac$TS, predict_ts_from_ta(mod, ac$TA), "overlay")
  bounds <- ts_bounds(ac$TS, ac$DOY, gStart, gEnd)
  list(ac = ac, nightNEE = nightNEE, tStart = bounds$tStart, tEnd = bounds$tEnd)
}

#' The day-of-year-climatology definition of the bounds
#'
#' Average each DOY over the years present, keep the DOYs inside the growing
#' season, take the 2.5/97.5 percentiles of *those* means. Averaging removes
#' the diurnal and interannual variance before the quantile is taken, so this
#' band is much narrower than `ts_bounds()`' half-hourly one on the same data --
#' finding F4.
#'
#' @param ts Soil temperature, one value per half-hour.
#' @param doy Day of year of each value of `ts`, possibly wrapped.
#' @param gStart,gEnd Growing-season bounds, as (possibly wrapped) DOY, inclusive.
#' @return A list of `tStart` and `tEnd`, the 2.5% and 97.5% quantiles of the
#'   DOY means.
ts_bounds_climatology <- function(ts, doy, gStart, gEnd) {
  in_gs <- dplyr::between(doy, gStart, gEnd) & !is.na(ts)
  clim <- tapply(ts[in_gs], doy[in_gs], mean)
  list(
    tStart = unname(quantile(clim, 0.025, na.rm = TRUE)),
    tEnd = unname(quantile(clim, 0.975, na.rm = TRUE))
  )
}

#' Both definitions for one column, as `ts_bounds` rows
#'
#' The measured column's third row, `climatology_uptake`, is not one of these:
#' it is written by `prep_nee_ac()` from season detection.
#'
#' @param ts Soil temperature, one value per half-hour.
#' @param doy Day of year of each value of `ts`, possibly wrapped.
#' @param gStart,gEnd Growing-season bounds, as (possibly wrapped) DOY, inclusive.
#' @param ts_col Name of the soil-temperature column `ts` came from.
#' @return A two-row tibble with `ts_col`, `definition` (`"halfhourly"`,
#'   `"climatology"`), `tStart` and `tEnd`.
ts_bounds_rows <- function(ts, doy, gStart, gEnd, ts_col) {
  hh <- ts_bounds(ts, doy, gStart, gEnd)
  cl <- ts_bounds_climatology(ts, doy, gStart, gEnd)
  tibble::tibble(
    ts_col = ts_col,
    definition = c("halfhourly", "climatology"),
    tStart = c(hh$tStart, cl$tStart),
    tEnd = c(hh$tEnd, cl$tEnd)
  )
}

#' The bounds for a TS column under one definition
#'
#' @param ts_bounds A site's `ts_bounds` table: one row per soil-temperature
#'   column and bounds definition.
#' @param ts_col Name of the soil-temperature column.
#' @param definition `"halfhourly"`, `"climatology"` or `"climatology_uptake"`
#'   (the measured column only).
#' @return A list of `tStart` and `tEnd`. Errors unless exactly one row
#'   matches.
ts_bounds_for <- function(ts_bounds, ts_col, definition) {
  keep <- ts_bounds[["definition"]] == definition
  row <- ts_bounds[ts_bounds[["ts_col"]] == ts_col & keep, ]
  if (nrow(row) != 1) {
    stop(
      "Expected exactly one bounds row for ", shQuote(ts_col),
      " under the ", shQuote(definition), " definition",
      ", found ", nrow(row), ". Available: ",
      paste(unique(ts_bounds[["ts_col"]]), collapse = ", "), "."
    )
  }
  list(tStart = row[["tStart"]][[1]], tEnd = row[["tEnd"]][[1]])
}
