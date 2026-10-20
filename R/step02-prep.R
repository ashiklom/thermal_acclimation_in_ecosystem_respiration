# Workflow step 02, part 1: turn a site's step-01 tables into the inputs of
# the TAS fits (Junna Wang, 2025). All site-specific choices are made here,
# from data/site_info.csv (SWC_use) and data/site_settings.csv:
#
#   ts_regression          "none"; "halfhourly_ta_positive": replace TS by
#                          lm(TS ~ TA) fitted to the half-hourly table where
#                          TA > 0 (the 35 sites whose inter-annual TS did not
#                          follow TA); "night_all": the same, fitted to the
#                          nighttime table (US-Tw1, a (sub)tropical wetland,
#                          where the half-hourly fit gives too low a slope)
#   ts_bounds_method       "step01": tStart/tEnd from step 01's growing-season
#                          features; "halfhourly_growing_season": 2.5/97.5%
#                          quantiles of the half-hourly TS within gStart..gEnd
#                          (the regression sites, after the regression)
#   trim_last_growing_year southern-hemisphere sites whose growing season
#                          spans New Year lose the last, incomplete one
#
# prepare_tas_inputs() returns list(night, ac, ac_day, settings): the nighttime
# table and the half-hourly table, both with the soil temperature and soil
# water the fits use and a growing_year column; daily daytime NEE (ac_day);
# and a one-row data frame of the site's fit settings (gStart, gEnd, tStart,
# tEnd, ts_bounds_method, dt, nobs_threshold, control_year).

read_site_settings <- function(path = file.path("data", "site_settings.csv")) {
  utils::read.csv(path)
}

# ERA5-Land daily soil water (%) for every site, keyed by calendar date.
read_era5_swc <- function(path = path_original("ERA5_daily_swc_1990_2024_allsites.csv")) {
  swc_ERA5 <- utils::read.csv(path)
  swc_ERA5$date <- as.Date(swc_ERA5$date)
  swc_ERA5$YEAR <- lubridate::year(swc_ERA5$date)
  swc_ERA5$MONTH <- lubridate::month(swc_ERA5$date)
  swc_ERA5$DAY <- lubridate::day(swc_ERA5$date)
  swc_ERA5
}

prepare_tas_inputs <- function(site, settings, feature_gs, a_measure_night_complete, ac, swc_ERA5) {
  name_site <- site$site_ID
  stopifnot(identical(settings$site_ID, name_site))

  if (site$SWC_use == 'YES') {
    a_measure_night_complete <- a_measure_night_complete |> dplyr::filter(!is.na(SWC))
  }

  # if no measured SWC data use daily SWC from ERA5 land (only the direct
  # model uses soil water)
  if (site$SWC_use == 'NO') {
    a_measure_night_complete$SWC <- NULL
    ac$SWC <- NULL
    swc_ERA5_site <- swc_ERA5[swc_ERA5$name == name_site, ]
    a_measure_night_complete <- a_measure_night_complete |> dplyr::left_join(swc_ERA5_site[, c('YEAR', 'MONTH', 'DAY', 'SWC')], by = c('YEAR', 'MONTH', 'DAY'))
    ac <- ac |> dplyr::left_join(swc_ERA5_site[, c('YEAR', 'MONTH', 'DAY', 'SWC')], by = c('YEAR', 'MONTH', 'DAY'))
  }

  # For the 36 sites, obtain TS from simple linear regression of TA, in order to follow reviewer's suggestion
  if (settings$ts_regression != "none") {
    if (settings$ts_regression == "night_all") {
      # slope will be too low if using ac data for the tropical and subtropical sites.
      mod_lm <- lm(data = a_measure_night_complete, TS ~ TA, na.action = na.omit)
    } else {
      stopifnot(settings$ts_regression == "halfhourly_ta_positive")
      # using data with TA > 0, because we focus on grouping season
      mod_lm <- lm(data = ac[ac$TA > 0, ], TS ~ TA, na.action = na.omit)
    }
    TS_pred <- predict(mod_lm, newdata = data.frame(TA = a_measure_night_complete$TA), na.action = na.pass)
    a_measure_night_complete$TS[!is.na(TS_pred)] <- TS_pred[!is.na(TS_pred)]
    TS_pred <- predict(mod_lm, newdata = data.frame(TA = ac$TA), na.action = na.pass)
    ac$TS[!is.na(TS_pred)] <- TS_pred[!is.na(TS_pred)]
  }

  # calculate daily daytime NEE and rolling average
  dt = 30  # minute
  if (ac$MINUTE[2] - ac$MINUTE[1] != 30) { dt = 60 }   # minutes
  # unit is umol / m2 / s
  ac_day <- ac |> dplyr::filter(daytime) |> dplyr::group_by(YEAR, DOY) |>
    dplyr::summarise(NEE_daytime1 = max(-sum(NEE_uStar_f, na.rm=T) * dt * 60 / 86400, 0.0), .groups = "drop")
  # get rolling average of the prior three days
  ac_day$NEE_daytime <- zoo::rollapply(ac_day$NEE_daytime1, width = 3, FUN = mean,
                                  align = "right", fill = ac_day$NEE_daytime1[1])

  # attach it to nighttime data
  if (!"NEE_daytime1" %in% names(a_measure_night_complete)) {
    a_measure_night_complete <- a_measure_night_complete |> dplyr::mutate(DOY_gpp = dplyr::case_when(HOUR >= 12 ~ DOY,
                                                                                        HOUR < 12 & DOY == min(ac$DOY) ~ DOY,
                                                                                        HOUR < 12 & DOY != min(ac$DOY) ~ DOY - 1)) |>
      dplyr::left_join(ac_day, by = c("YEAR", "DOY_gpp"="DOY")) |> dplyr::select(-"DOY_gpp") |> dplyr::filter(!is.na(NEE_daytime1))
  }

  # Deal with sites in southern hemisphere
  # add growing_year
  a_measure_night_complete <- a_measure_night_complete  |>
    dplyr::mutate(growing_year = dplyr::case_when(DOY <= 366 ~ YEAR,
                                    TRUE ~ YEAR - 1))
  ac <- ac |>
    dplyr::mutate(growing_year = dplyr::case_when(DOY <= 366 ~ YEAR,
                                    TRUE ~ YEAR - 1))

  # remove the growing_year with incomplete data
  # sites in southern hemisphere lose one year, because growing season crosses two years
  if (settings$trim_last_growing_year) {
    a_measure_night_complete <- a_measure_night_complete |> dplyr::filter(dplyr::between(growing_year, ac$YEAR[1], ac$YEAR[nrow(ac)] - 1))
    ac <- ac |> dplyr::filter(dplyr::between(growing_year, ac$YEAR[1], ac$YEAR[nrow(ac)] - 1))
  }
  years <- sort(unique(a_measure_night_complete$growing_year))

  # get start and end dates of growing season
  gStart <- feature_gs$gStart[feature_gs$site_ID == name_site]
  gEnd <- feature_gs$gEnd[feature_gs$site_ID == name_site]
  tStart <- feature_gs$tStart[feature_gs$site_ID == name_site]
  tEnd <- feature_gs$tEnd[feature_gs$site_ID == name_site]

  # Correct tStart and tEnd for sites with newly regressed TS
  if (settings$ts_bounds_method == "halfhourly_growing_season") {
    tStart <- quantile(ac$TS[dplyr::between(ac$DOY, gStart, gEnd)], c(0.025), na.rm=T)
    tEnd <- quantile(ac$TS[dplyr::between(ac$DOY, gStart, gEnd)], c(0.975), na.rm=T)
  } else {
    stopifnot(settings$ts_bounds_method == "step01")
  }

  # decide control year: the year with growing-season TS closest to long-term mean.
  ac_yearly_gs <- ac |> dplyr::filter(dplyr::between(DOY, gStart, gEnd)) |> dplyr::filter(growing_year %in% years) |> dplyr::group_by(growing_year) |> dplyr::summarise(TS=mean(TS, na.rm=T))
  control_year <- ac_yearly_gs$growing_year[which.min(abs(ac_yearly_gs$TS - mean(ac_yearly_gs$TS)))]

  # minimum number of observations of a window-year (before widening it)
  if (dt == 30) {
    nobs_threshold <- 100
  } else {
    nobs_threshold <- 60
  }

  list(
    night = a_measure_night_complete,
    ac = ac,
    ac_day = ac_day,
    settings = data.frame(
      site_ID = name_site, gStart = gStart, gEnd = gEnd, tStart = unname(tStart), tEnd = unname(tEnd),
      ts_bounds_method = settings$ts_bounds_method, dt = dt, nobs_threshold = nobs_threshold,
      control_year = control_year
    )
  )
}
