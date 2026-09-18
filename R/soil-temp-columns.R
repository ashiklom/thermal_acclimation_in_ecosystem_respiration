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

# Substitute the TS ~ TA regression for measured soil temperature in both
# tables, and recompute the bounds on the new scale.
apply_ts_linear <- function(ac, nightNEE, site_info, gStart, gEnd) {
  mod <- ts_ta_model(ts_fit_data(ac, nightNEE, site_info[["ts_linear_domain"]]))
  nightNEE$TS <- overlay_ts(nightNEE$TS, predict_ts_from_ta(mod, nightNEE$TA))
  ac$TS <- overlay_ts(ac$TS, predict_ts_from_ta(mod, ac$TA))
  bounds <- ts_bounds(ac$TS, ac$DOY, gStart, gEnd)
  list(ac = ac, nightNEE = nightNEE, tStart = bounds$tStart, tEnd = bounds$tEnd)
}
