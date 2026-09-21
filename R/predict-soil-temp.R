.data <- rlang::.data

# Which estimator reconstructs a site's soil temperature, from the declaration
# rather than a site list. `estimate_ts_method` is derived in
# `scripts/revise-site-info.R`: "NETRAD" wherever a `netrad_column` is named,
# "linear regression" at the two sites with too little soil temperature to
# train a forest on, and empty otherwise. Empty is spelled out here so that
# every branch of `fix_soil_temp()` has a name to report.
ts_estimate_method <- function(site_info) {
  declared <- site_info[["estimate_ts_method"]]
  if (is.null(declared) || length(declared) != 1 || is.na(declared)) return("TA only")
  declared
}

fix_soil_temp <- function(a, site_info) {
  name_site <- site_info[["site_ID"]]

  if (site_reader(site_info) == "ameriflux") {
    # Renamed, not just selected. The AmeriFlux columns are named per site
    # (`TS_PI_1_1_A`, `T_SONIC_1_1_1`, ...), and everything below this point --
    # the gap fills, the formulae, the estimators -- is written against
    # `TS` and `TA`. Selecting them under their original names left `data$TA`
    # NULL, which `sum(is.na(NULL))` reports as zero missing values, so the gap
    # fill quietly did nothing and the fit failed with "object 'TA' not found".
    # Nothing reached this branch to notice: the only sites that need it are
    # the `estimate_Ts` ones, and the reader stopped before calling it.
    data <- a |>
      dplyr::select(
        "TIMESTAMP", "YEAR", "DOY", "HOUR", "MINUTE",
        TS = dplyr::all_of(site_info$TS),
        TA = dplyr::all_of(site_info$TA)
      )
    # Which column holds net radiation is declared per site in
    # site_info.csv. It used to be an if/else here as well, duplicating the
    # mapping that `scripts/revise-site-info.R` already builds -- the two
    # agreed, but only by hand.
    netrad_column <- site_info[["netrad_column"]]
    if (!is.na(netrad_column)) {
      if (!netrad_column %in% names(a)) {
        stop(
          name_site, " declares netrad_column = ", shQuote(netrad_column),
          ", which is not in its AmeriFlux record."
        )
      }
      data$NETRAD <- a[[netrad_column]]
    }
    
  } else {
    # TS_F_MDS_1 is the training target here, not the output: the model learns
    # TS from TA (and NETRAD) on the hours where TS was measured, then predicts
    # the rest. So some measured TS has to be present somewhere in the record.
    # A site whose only downloaded product omits soil temperature entirely --
    # FR-Fon's Warm Winter 2020 archive, for instance -- needs its later
    # products fetched before this can run.
    needed <- c("TS_F_MDS_1", "TA_F_MDS", "NETRAD")
    absent <- setdiff(needed, names(a))
    if (length(absent)) {
      stop(
        name_site, " needs predicted soil temperature, but the spliced record ",
        "has no ", paste(absent, collapse = ", "), " column. Declared products: ",
        paste(site_sources(site_info), collapse = " + "),
        ". Check that all of them are downloaded."
      )
    }
    data <- a |>
      dplyr::select(
        "TIMESTAMP", "YEAR", "DOY", "HOUR", "MINUTE",
        TS = "TS_F_MDS_1",
        TA = "TA_F_MDS",
        NETRAD = "NETRAD"
      )

    if (name_site == 'DE-Hte') {
      # too few data in TS_F_MDS_1, so use TS_F_MDS_2
      data$TS <- a$TS_F_MDS_2
    } else if (name_site == 'FR-Bil') {
      # abnormal Soil temperature data before 2021
      data$TS[data$YEAR < 2021] <- NA
    } else if (name_site == "FR-Pue") {
      data$TS[data$YEAR < 2016] <- NA
    }
  }

  # The predictors are gap-filled from their own day-of-year x time-of-day
  # climatology before anything is fitted, as the original did.
  data$TA <- write_back_ts(data$TA, doy_hour_climatology(data, "TA"), "fill_gaps")
  if ("NETRAD" %in% colnames(data)) {
    data$NETRAD <- write_back_ts(data$NETRAD, doy_hour_climatology(data, "NETRAD"), "fill_gaps")
  }

  # The two site lists this replaced -- the random-forest sites and the two
  # linear-regression ones -- were exactly `estimate_ts_method == "NETRAD"`
  # and `== "linear regression"`. Keeping both was how a site could be given a
  # net radiation column and still be silently fitted without it; it is also
  # what would have had to be edited, by hand, to route the AmeriFlux sites
  # here. tests/testthat/test-soil-temp-columns.R pins the two against each
  # other, so the equivalence is checked rather than asserted in prose.
  method <- ts_estimate_method(site_info)
  estimator_name <- switch(
    method,
    # Net radiation is available, so the random forest can use it.
    "NETRAD" = "rf_ta_netrad_manuscript",
    # One or two years of TS and incomplete NETRAD: too little to train on.
    "linear regression" = "lm_ta_netrad",
    # No net radiation at this site at all.
    "TA only" = "lm_ta_pos",
    stop(
      name_site, " declares estimate_ts_method = ", shQuote(method),
      ", which is not one of \"NETRAD\", \"linear regression\", or empty."
    )
  )
  est <- ts_estimators()[[estimator_name]]
  mod <- est$fit(data)
  data$TS_pred <- est$predict(mod, data)

  # Only the prediction and its key, plus what made it. Every caller joins the
  # prediction back onto the table it came from; the provenance travels in
  # attributes because the caller's table has no room for a second row.
  out <- data[, c("TIMESTAMP", "TS_pred")]
  attr(out, "estimator") <- estimator_name
  attr(out, "family") <- est$family
  attr(out, "n_train") <- sum(stats::complete.cases(data[, c("TS", est$predictors), drop = FALSE]))
  out
}


# The mean of `col` at each day-of-year x hour x minute, aligned to `dat`'s
# rows: the value the original gap-filled a predictor with. NaN where a slot
# has no observations at all, which `write_back_ts()` writes through as NA.
doy_hour_climatology <- function(dat, col) {
  key <- c("DOY", "HOUR", "MINUTE")
  clim <- dat |>
    dplyr::summarise(.clim = mean(.data[[col]], na.rm = TRUE), .by = dplyr::all_of(key))
  out <- dplyr::left_join(dat[, key], clim, by = key)[[".clim"]]
  out[is.nan(out)] <- NA_real_
  out
}
