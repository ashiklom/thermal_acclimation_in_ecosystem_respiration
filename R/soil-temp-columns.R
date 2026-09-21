# Soil temperature estimated from air temperature.
#
# Three places in this pipeline fit the same TS ~ TA regression and differ only
# in which rows they fit on and how they write the result back:
#
#   * `prep_ustar_df()` builds `TS` for AmeriFlux sites that have no usable
#     measured soil temperature, replacing the column wholesale.
#   * `fix_soil_temp()` falls back to it when a site has no net radiation.
#   * `total_tas_site()` replaces measured `TS` for the sites declared
#     `ts_col == "TS_linear"`, as an *overlay* rather than a replacement.
#
# The fit is shared here; the two write-back semantics are kept separate on
# purpose, because they are not interchangeable -- see `overlay_ts()`.

# Fit soil temperature on air temperature. `na.action = na.omit` drops
# incomplete rows, which is why callers can pass a subset containing NA rows.
ts_ta_model <- function(fit_data) {
  lm(data = fit_data, TS ~ TA, na.action = na.omit)
}

# The rows a site's regression is fitted on. A separate concept from the model
# because it is a per-site declaration (`site_info$ts_linear_domain`): all but
# one site fit on the growing-season part of `ac`, while US-Tw1 fits on the
# nighttime table.
#
# NB `ac[ac$TA > 0, ]` is kept verbatim from the original. The predicate yields
# NA for rows with missing TA, so those rows are selected as all-NA rows and
# then dropped again by `na.omit` -- the coefficients are the same as filtering
# them out first, and preserving the expression keeps that provable.
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

# Predictions aligned one-to-one with `ta`, NA wherever TA is missing.
predict_ts_from_ta <- function(mod, ta) {
  predict(mod, newdata = data.frame(TA = ta), na.action = na.pass)
}

# Write predictions over `ts` *only where a prediction exists*, so measured
# soil temperature survives wherever air temperature is missing. The resulting
# column is therefore a hybrid, not a pure regression. This is the original's
# behaviour at the `ts_col == "TS_linear"` sites and it is load-bearing: a
# wholesale replacement would introduce NAs that the step-01 quality filters
# have already certified absent.
overlay_ts <- function(ts, ts_pred) {
  ts[!is.na(ts_pred)] <- ts_pred[!is.na(ts_pred)]
  ts
}

# Replace `ts` entirely with the regression, including the NAs. Used where
# there is no measured soil temperature to preserve.
replace_ts <- function(ts_pred) {
  ts_pred
}

# The 2.5/97.5 percentiles of growing-season soil temperature, which gate the
# window-skip test in `total_tas_window()`.
#
# These have to be derived from the *same* TS column the model will see. That is
# the whole reason they are computed here rather than carried as scalars: when
# TS is substituted, bounds derived from the column it replaced no longer
# describe the data, and the window-skip test silently uses the wrong range.
ts_bounds <- function(ts, doy, gStart, gEnd) {
  gs <- ts[dplyr::between(doy, gStart, gEnd)]
  list(
    tStart = unname(quantile(gs, 0.025, na.rm = TRUE)),
    tEnd = unname(quantile(gs, 0.975, na.rm = TRUE))
  )
}

# The fitting domain to use for a site's `TS_linear` column.
#
# `TS_linear` is now built at *every* site, not just the 35 that select it, so
# that measured and regressed soil temperature can be compared anywhere. That
# splits the domain question in two:
#
#   * A site that *declares* `ts_col = "TS_linear"` must also declare its
#     domain. The declaration is returned unchanged -- NA included -- so that
#     `ts_fit_data()` still rejects it. Silently defaulting here would let a
#     half-filled site_info row through and change that site's coefficients
#     without a word.
#   * Anywhere else the column is a diagnostic: nothing selects it unless a
#     sensitivity run names it. A missing declaration is expected rather than
#     an error, and the default is `"ac"`, which is what 34 of the 35 declared
#     sites use (US-Tw1 is the only `"night"`).
ts_linear_domain_for <- function(site_info) {
  domain <- site_info[["ts_linear_domain"]]
  if (identical(site_info[["ts_col"]], "TS_linear")) {
    return(domain)
  }
  if (length(domain) == 1 && !is.na(domain)) domain else "ac"
}

# Substitute the TS ~ TA regression for measured soil temperature in both
# tables, and recompute the bounds on the new scale.
apply_ts_linear <- function(ac, nightNEE, site_info, gStart, gEnd) {
  mod <- ts_ta_model(ts_fit_data(ac, nightNEE, ts_linear_domain_for(site_info)))
  nightNEE$TS <- overlay_ts(nightNEE$TS, predict_ts_from_ta(mod, nightNEE$TA))
  ac$TS <- overlay_ts(ac$TS, predict_ts_from_ta(mod, ac$TA))
  bounds <- ts_bounds(ac$TS, ac$DOY, gStart, gEnd)
  list(ac = ac, nightNEE = nightNEE, tStart = bounds$tStart, tEnd = bounds$tEnd)
}

# The day-of-year-climatology definition of the bounds: average each DOY over
# the years present, keep the DOYs inside the growing season, take the
# 2.5/97.5 percentiles of *those* means. Averaging removes the diurnal and
# interannual variance before the quantile is taken, so this band is much
# narrower than `ts_bounds()`' half-hourly one on the same data -- finding F4.
ts_bounds_climatology <- function(ts, doy, gStart, gEnd) {
  in_gs <- dplyr::between(doy, gStart, gEnd) & !is.na(ts)
  clim <- tapply(ts[in_gs], doy[in_gs], mean)
  list(
    tStart = unname(quantile(clim, 0.025, na.rm = TRUE)),
    tEnd = unname(quantile(clim, 0.975, na.rm = TRUE))
  )
}

# Both definitions for one column, as rows of the `ts_bounds` table. Neither
# row is `native`: the native rows are the manuscript's own numbers and are
# written by `prep_nee_ac()` directly, because one of them (the measured
# column's) is not a pure function of the column -- it carries the 0 C floor
# and, at three sites, the 2 C truncation.
ts_bounds_rows <- function(ts, doy, gStart, gEnd, ts_col) {
  hh <- ts_bounds(ts, doy, gStart, gEnd)
  cl <- ts_bounds_climatology(ts, doy, gStart, gEnd)
  tibble::tibble(
    ts_col = ts_col,
    definition = c("halfhourly", "climatology"),
    native = FALSE,
    tStart = c(hh$tStart, cl$tStart),
    tEnd = c(hh$tEnd, cl$tEnd)
  )
}

# The bounds recorded for a TS column by `prep_nee_ac()`.
#
# With no `definition`, the column's *native* row: the one the manuscript used
# for it. That is what every pre-existing caller means, and it is what keeps
# tests/ts-swc-baseline.R's oracle comparison meaningful. With a `definition`,
# the row computed under that definition, so a recipe can apply one definition
# to every column.
ts_bounds_for <- function(ts_bounds, ts_col, definition = NULL) {
  has_def <- "definition" %in% names(ts_bounds)
  row <- if (is.null(definition)) {
    if (has_def) {
      ts_bounds[ts_bounds[["ts_col"]] == ts_col & ts_bounds[["native"]], ]
    } else {
      ts_bounds[ts_bounds[["ts_col"]] == ts_col, ]
    }
  } else {
    if (!has_def) {
      stop("This ts_bounds table predates bounds definitions; rebuild the site_data target.")
    }
    ts_bounds[ts_bounds[["ts_col"]] == ts_col & ts_bounds[["definition"]] == definition, ]
  }
  if (nrow(row) != 1) {
    stop(
      "Expected exactly one bounds row for ", shQuote(ts_col),
      if (!is.null(definition)) paste0(" under the ", shQuote(definition), " definition"),
      ", found ", nrow(row), ". Available: ",
      paste(unique(ts_bounds[["ts_col"]]), collapse = ", "), "."
    )
  }
  list(tStart = row[["tStart"]][[1]], tEnd = row[["tEnd"]][[1]])
}
