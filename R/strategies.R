# The strategies behind each recipe axis.
#
# One `choose_*()` per axis; each is a `switch()` on the recipe's choice, and
# every branch is a few lines that name *which* column, span or table row a
# run uses. Nothing here computes a column -- step 01 produces every candidate
# and these select -- so a strategy is cheap to add and impossible to get
# silently wrong: an unknown choice fails by name in `validate_recipe()`
# before any data is touched.
#
# Every function returns, alongside its choice, the *reason* for it where one
# exists, so that provenance can be written next to every result.

# ----------------------------------------------------------------- ts axis
#
# Which soil-temperature column the model is fitted on.
#
#   site_info    the declaration in site_info.csv (`ts_col`): the manuscript.
#   screen_best  measured soil temperature unless the quality verdict is BAD,
#                in which case the TS ~ TA regression the manuscript would
#                have used anyway. Isolates "stop discarding good sensors".
#   memory_fill  as screen_best, but a BAD sensor is replaced by the
#                blocked-CV-selected reconstruction instead of TS ~ TA.
#                Needs the per-site `fill` target; falls back to TS_linear,
#                and says so, when it is missing.
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
                             "; reconstructed by ", fill$method))
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

ts_verdict <- function(site_data) {
  q <- site_data[["ts_qc"]]
  if (is.null(q) || !"verdict" %in% names(q)) {
    stop("site_data carries no `ts_qc` verdict. It is produced by `prep_nee_ac()`; ",
         "rebuild the site_data target or delete the stale `_targets/` store.")
  }
  q[["verdict"]][[1]]
}

ts_flags <- function(site_data) {
  q <- site_data[["ts_qc"]]
  f <- q[["flags"]][[1]]
  if (is.null(f) || is.na(f) || !nzchar(f)) "no flags" else f
}

fill_available <- function(fill) {
  !is.null(fill) && identical(fill[["status"]], "ok") && !is.null(fill[["ac_ts"]])
}
fill_status <- function(fill) {
  if (is.null(fill)) "no fill target" else fill[["status"]] %||% "unknown"
}

# ------------------------------------------------------------- season axis
#
# The day-of-year span the 14-day windows tile.
#
#   detect      the detected growing season: today's behaviour.
#   whole_year  DOY 1-366. Tests the hypothesis that season detection is
#               redundant with the fit-stage guards. Only the *window layout*
#               changes: the detected season still drives the year gap scan
#               (in step 01) and the control-year choice, because both need a
#               span to be defined over and a season-free rule for them is a
#               separate piece of work. See docs/recipes.md.
choose_window_season <- function(recipe, feature_gs) {
  switch(
    recipe$season,
    detect = list(gStart = feature_gs[["gStart"]], gEnd = feature_gs[["gEnd"]],
                  reason = "detected growing season"),
    whole_year = list(gStart = 1, gEnd = 366, reason = "whole year, DOY 1-366"),
    stop("Unknown season strategy ", shQuote(recipe$season))
  )
}

# ------------------------------------------------------------- bounds axis
#
# Which population tStart/tEnd -- the window-skip gate -- are percentiles of.
#
#   native       each column's own definition, as the manuscript had it: the
#                day-of-year climatology for the measured column and the raw
#                half-hourly values for the regressed one. Finding F4.
#   climatology  the day-of-year-climatology definition for whichever column
#                is selected.
#   halfhourly   the half-hourly definition for whichever column is selected.
choose_bounds <- function(recipe, ts_bounds, ts_col) {
  b <- switch(
    recipe$bounds,
    native = ts_bounds_for(ts_bounds, ts_col),
    climatology = ts_bounds_for(ts_bounds, ts_col, definition = "climatology"),
    halfhourly = ts_bounds_for(ts_bounds, ts_col, definition = "halfhourly"),
    stop("Unknown bounds strategy ", shQuote(recipe$bounds))
  )
  b$reason <- paste0(recipe$bounds, " definition for ", ts_col)
  b
}

# ---------------------------------------------------------------- swc axis
#
# Which soil-water column the direct model uses. The total model has no soil
# water in its formula, so the choice is NA there whatever the recipe says.
#
#   site_info  measured where `SWC_use = YES`, ERA5-Land otherwise: today.
#   era5       ERA5-Land everywhere, so that the soil-water driver is one
#              product across sites.
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
