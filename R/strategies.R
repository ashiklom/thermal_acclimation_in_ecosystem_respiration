# The strategies behind each recipe axis.
#
# One `choose_*()` per axis, each a `switch()` on the recipe's choice that
# names which column, span or bounds row a run uses -- nothing here computes
# a column -- and returns the reason with it, for provenance.

# ----------------------------------------------------------------- ts axis

#' Which soil-temperature column the model is fitted on
#'
#'   site_info    the declaration in site_info.csv (`ts_col`): the manuscript.
#'   screen_best  measured soil temperature unless the quality verdict is BAD,
#'                in which case the TS ~ TA regression the manuscript would
#'                have used anyway. Isolates "stop discarding good sensors".
#'   memory_fill  as screen_best, but a BAD sensor is replaced by the
#'                blocked-CV-selected reconstruction instead of TS ~ TA.
#'                Needs the per-site `fill` target; falls back to TS_linear,
#'                and says so, when it is missing.
#'
#' @param recipe A recipe: one strategy per `RECIPE_AXES` axis.
#' @param site_data The site's step-01 result, from `prep_nee_ac()`.
#' @param site_info One row of the site declaration table.
#' @param fill The site's `fill_soil_temp()` result, or `NULL`.
#' @return A list of `ts_col`, the column name, and `reason`, a string for
#'   provenance.
choose_ts_col <- function(recipe, site_data, site_info, fill = NULL) {
  verdict <- ts_verdict(site_data)
  switch(
    recipe$ts,
    site_info = list(
      ts_col = site_info[["ts_col"]],
      reason = "declared in site_info.csv"
    ),
    screen_best = if (identical(verdict, "BAD")) {
      list(ts_col = "TS_linear", reason = paste("screen verdict BAD:", ts_flags(site_data)))
    } else {
      list(ts_col = "TS_measured", reason = paste("screen verdict", verdict))
    },
    memory_fill = if (identical(verdict, "BAD")) {
      if (fill_available(fill)) {
        list(ts_col = "TS_memfill",
             reason = paste0("screen verdict BAD: ", ts_flags(site_data),
                             "; reconstructed by ", fill$method,
                             if (isTRUE(fill$degenerate)) {
                               " [degenerate: the measured column is itself a regression on the predictors]"
                             } else ""))
      } else {
        list(ts_col = "TS_linear",
             reason = paste0("screen verdict BAD: ", ts_flags(site_data),
                             "; fill unavailable (", fill_status(fill), "), fell back to TS_linear"))
      }
    } else {
      list(ts_col = "TS_measured", reason = paste("screen verdict", verdict))
    },
    stop("Unknown ts strategy ", shQuote(recipe$ts))
  )
}

#' The site's soil-temperature quality verdict
#'
#' @param site_data The site's step-01 result, from `prep_nee_ac()`.
#' @return `"BAD"` or `"GOOD"`, from `site_data$ts_qc`. Errors unless there is
#'   exactly one.
ts_verdict <- function(site_data) {
  verdict <- site_data[["ts_qc"]][["verdict"]]
  stopifnot("site_data has a ts_qc verdict" = length(verdict) == 1)
  verdict
}

#' The quality rules that fired for the site
#'
#' @param site_data The site's step-01 result, from `prep_nee_ac()`.
#' @return A string: the comma-separated names of the rules that fired, or
#'   `"no flags"`.
ts_flags <- function(site_data) {
  q <- site_data[["ts_qc"]]
  f <- q[["flags"]][[1]]
  if (is.null(f) || is.na(f) || !nzchar(f)) "no flags" else f
}

#' Whether a usable fill was supplied
#'
#' @param fill The site's `fill_soil_temp()` result, or `NULL`.
#' @return `TRUE` if `fill` has status `"ok"` and an `ac_ts` column, else
#'   `FALSE`.
fill_available <- function(fill) {
  !is.null(fill) && identical(fill[["status"]], "ok") && !is.null(fill[["ac_ts"]])
}
#' Why a fill is or is not usable
#'
#' @param fill The site's `fill_soil_temp()` result, or `NULL`.
#' @return The fill's `status` string (`"ok"` or the reason it failed),
#'   `"no fill target"` if `fill` is `NULL`, or `"unknown"` if it has none.
fill_status <- function(fill) {
  if (is.null(fill)) "no fill target" else fill[["status"]] %||% "unknown"
}

# ------------------------------------------------------------- season axis

#' The day-of-year span the moving windows are laid over
#'
#'   detect_or_override  the manuscript's season: `detect_growing_season()`'s
#'                       bounds, each replaced by the site_info.csv literal
#'                       where one is declared. Twenty-four sites declare one,
#'                       so this is not "detected" -- hence the name.
#'   force_detect        the detector's bounds alone, overrides ignored. What
#'                       separates the hand-set part of the season from the
#'                       data-driven part; step 01 carries the unoverridden
#'                       pair out as `gStart_detected`/`gEnd_detected`.
#'   whole_year          a full year of DOY, starting where the site's growing
#'                       year starts. Tests the hypothesis that season detection
#'                       is redundant with the fit-stage guards.
#'
#' Only the window layout changes; the gap scan and control year still use the
#' detect-or-override season (docs/recipes.md). `whole_year` starts at the
#' site's DOY origin (`feature_gs`), so it is in the data's wrapped frame.
#'
#' @param recipe A recipe: one strategy per `RECIPE_AXES` axis.
#' @param feature_gs The site's one-row `feature_gs` table from step 01.
#' @return A list of `gStart` and `gEnd`, days of year in the data's wrapped
#'   frame, and `reason`, a string for provenance.
choose_window_season <- function(recipe, feature_gs) {
  origin <- feature_gs[["growing_year_start"]]
  if (is.null(origin) || is.na(origin)) origin <- 1L
  switch(
    recipe$season,
    detect_or_override = list(gStart = feature_gs[["gStart"]], gEnd = feature_gs[["gEnd"]],
                              reason = "detected growing season, site_info overrides applied"),
    force_detect = list(gStart = feature_gs[["gStart_detected"]], gEnd = feature_gs[["gEnd_detected"]],
                        reason = "detected growing season, site_info overrides ignored"),
    whole_year = list(gStart = origin, gEnd = origin + 365,
                      reason = sprintf("whole year, DOY %d-%d", origin, origin + 365)),
    stop("Unknown season strategy ", shQuote(recipe$season))
  )
}

# ------------------------------------------------------------- bounds axis

#' Choose the soil-temperature bounds under a recipe
#'
#' Which population tStart/tEnd -- the window-skip gate -- are percentiles of.
#'
#'   manuscript   each column's own definition, as the manuscript had it: the
#'                `manuscript` row (day-of-year climatology over the NEE uptake
#'                days, floored; docs/recipes.md) for the measured column and
#'                the raw half-hourly values for the regressed one. Finding F4.
#'   climatology  the day-of-year-climatology definition for whichever column
#'                is selected.
#'   halfhourly   the half-hourly definition for whichever column is selected.
#'
#' @param recipe A recipe: one strategy per `RECIPE_AXES` axis.
#' @param ts_bounds The site's bounds table: one row per soil-temperature
#'   column and definition, with `tStart`, `tEnd` and `is_manuscript`.
#' @param ts_col Name of the selected soil-temperature column.
#' @return A list of `tStart` and `tEnd` (degrees C) and `reason`, a string for
#'   provenance.
choose_bounds <- function(recipe, ts_bounds, ts_col) {
  b <- switch(
    recipe$bounds,
    manuscript = ts_bounds_for(ts_bounds, ts_col),
    climatology = ts_bounds_for(ts_bounds, ts_col, definition = "climatology"),
    halfhourly = ts_bounds_for(ts_bounds, ts_col, definition = "halfhourly"),
    stop("Unknown bounds strategy ", shQuote(recipe$bounds))
  )
  b$reason <- paste0(recipe$bounds, " definition for ", ts_col)
  b
}

# ---------------------------------------------------------------- swc axis

#' Which soil-water column the direct model uses
#'
#' The total model has no soil water in its formula, so the choice is NA there
#' whatever the recipe says.
#'
#'   site_info  measured where `SWC_use = YES`, ERA5-Land otherwise: today.
#'   era5       ERA5-Land everywhere, so that the soil-water driver is one
#'              product across sites.
#'
#' @param recipe A recipe: one strategy per `RECIPE_AXES` axis.
#' @param site_info One row of the site declaration table.
#' @param direct `TRUE` for the direct model, `FALSE` for the total model.
#' @return A list of `swc_col`, the column name or `NA_character_`, and
#'   `reason`, a string for provenance.
choose_swc_col <- function(recipe, site_info, direct) {
  switch(
    recipe$swc,
    site_info = list(swc_col = default_swc_col(site_info, direct),
                     reason = "site_info SWC_use declaration"),
    era5 = list(swc_col = if (direct) "SWC_era5" else NA_character_,
                reason = "ERA5-Land for every site"),
    stop("Unknown swc strategy ", shQuote(recipe$swc))
  )
}
