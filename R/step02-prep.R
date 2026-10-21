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
#   tstart_floor           lower bound on tStart (deg C): 2 where nighttime
#                          NEE below 2 deg C soil temperature is unreliable
#                          (step 01 drops those records), 0 elsewhere
#   trim_last_growing_year southern-hemisphere sites whose growing season
#                          spans New Year lose the last, incomplete one
#
# prepare_tas_inputs() returns list(night, ac, ac_day, settings): the nighttime
# table and the half-hourly table, both with the soil temperature and soil
# water the fits use and a growing_year column; daily daytime NEE (ac_day);
# and a one-row data frame of the site's fit settings (gStart, gEnd, tStart,
# tEnd, ts_bounds_method, dt, nobs_threshold, control_year).
#
# tStart and tEnd, the range of soil temperature over which TAS is estimated,
# follow step 01's rule at every site: the 2.5% and 97.5% quantiles of the
# day-of-year mean soil temperature of step 01's climatology record over its
# growing-season days, at least tstart_floor. The soil temperature is the one
# the fits use, so at the regression sites the regressed TS (the regression
# applied to the record's TA). ts_bounds_method records which it is:
# "step01" (step 01's own) or "regressed".

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

prepare_tas_inputs <- function(site, settings, feature_gs, a_measure_night_complete, ac, swc_ERA5, climatology) {
  name_site <- site$site_ID
  stopifnot(identical(settings$site_ID, name_site))

  # Only the columns used below, in a fixed order, sorted by time: nothing
  # below depends on the order of the input tables' rows or columns.
  by_time <- c("YEAR", "MONTH", "DAY", "HOUR", "MINUTE")
  a_measure_night_complete <- a_measure_night_complete |>
    dplyr::select(dplyr::all_of(c("YEAR", "MONTH", "DAY", "DOY", "HOUR", "MINUTE", "NEE", "TA", "TS", "SWC"))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(by_time)))
  ac <- ac |>
    dplyr::select(dplyr::all_of(c("YEAR", "MONTH", "DAY", "DOY", "HOUR", "MINUTE", "NEE_uStar_f", "TA", "TS", "SWC", "daytime"))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(by_time)))

  if (site$SWC_use == 'YES') {
    a_measure_night_complete <- a_measure_night_complete |> dplyr::filter(!is.na(.data$SWC))
  }

  # if no measured SWC data use daily SWC from ERA5 land (only the direct
  # model uses soil water)
  if (site$SWC_use == 'NO') {
    a_measure_night_complete$SWC <- NULL
    ac$SWC <- NULL
    swc_ERA5_site <- swc_ERA5[swc_ERA5$name == name_site, c('YEAR', 'MONTH', 'DAY', 'SWC')]
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
    TS_pred <- predict(mod_lm, newdata = data.frame(TA = climatology$TA), na.action = na.pass)
    climatology$TS[!is.na(TS_pred)] <- TS_pred[!is.na(TS_pred)]
  }

  # temperature range of the growing season, from the site's climatology
  TS_yearly <- climatology |> dplyr::group_by(.data$DOY) |> dplyr::summarise(TS = mean(.data$TS, na.rm=T), season = all(.data$season)) |>
    dplyr::filter(.data$season)
  tStart <- max(quantile(TS_yearly$TS, c(0.025), na.rm=T), 0.0, settings$tstart_floor)
  tEnd   <- quantile(TS_yearly$TS, c(0.975), na.rm=T)

  # time step (minutes): half-hourly records have two distinct minutes past
  # the hour (e.g. 15 and 45), hourly records one
  dt <- 60 / length(unique(ac$MINUTE))
  stopifnot(dt %in% c(30, 60))
  # calculate daily daytime NEE and rolling average
  # unit is umol / m2 / s
  ac_day <- ac |> dplyr::filter(.data$daytime) |> dplyr::group_by(.data$YEAR, .data$DOY) |>
    dplyr::summarise(NEE_daytime1 = max(-sum(.data$NEE_uStar_f, na.rm=T) * .env$dt * 60 / 86400, 0.0), .groups = "drop") |>
    dplyr::arrange(.data$YEAR, .data$DOY)
  # get rolling average of the prior three days
  ac_day$NEE_daytime <- zoo::rollapply(ac_day$NEE_daytime1, width = 3, FUN = mean,
                                  align = "right", fill = ac_day$NEE_daytime1[1])

  # attach it to nighttime data
  if (!"NEE_daytime1" %in% names(a_measure_night_complete)) {
    a_measure_night_complete <- a_measure_night_complete |> dplyr::mutate(DOY_gpp = dplyr::case_when(.data$HOUR >= 12 ~ .data$DOY,
                                                                                        .data$HOUR < 12 & .data$DOY == min(ac$DOY) ~ .data$DOY,
                                                                                        .data$HOUR < 12 & .data$DOY != min(ac$DOY) ~ .data$DOY - 1)) |>
      dplyr::left_join(ac_day, by = c("YEAR", "DOY_gpp"="DOY")) |> dplyr::select(-"DOY_gpp") |> dplyr::filter(!is.na(.data$NEE_daytime1))
  }

  # Deal with sites in southern hemisphere
  # add growing_year
  a_measure_night_complete <- a_measure_night_complete  |>
    dplyr::mutate(growing_year = dplyr::case_when(.data$DOY <= 366 ~ .data$YEAR,
                                    TRUE ~ .data$YEAR - 1))
  ac <- ac |>
    dplyr::mutate(growing_year = dplyr::case_when(.data$DOY <= 366 ~ .data$YEAR,
                                    TRUE ~ .data$YEAR - 1))

  # remove the growing_year with incomplete data
  # sites in southern hemisphere lose one year, because growing season crosses two years
  if (settings$trim_last_growing_year) {
    first_year <- min(ac$YEAR)
    last_year <- max(ac$YEAR)
    a_measure_night_complete <- a_measure_night_complete |> dplyr::filter(dplyr::between(.data$growing_year, .env$first_year, .env$last_year - 1))
    ac <- ac |> dplyr::filter(dplyr::between(.data$growing_year, .env$first_year, .env$last_year - 1))
  }
  years <- sort(unique(a_measure_night_complete$growing_year))

  # get start and end dates of growing season
  gStart <- feature_gs$gStart[feature_gs$site_ID == name_site]
  gEnd <- feature_gs$gEnd[feature_gs$site_ID == name_site]

  # decide control year: the year with growing-season TS closest to long-term mean.
  ac_yearly_gs <- ac |> dplyr::filter(dplyr::between(.data$DOY, .env$gStart, .env$gEnd)) |> dplyr::filter(.data$growing_year %in% .env$years) |>
    dplyr::group_by(.data$growing_year) |> dplyr::summarise(TS=mean(.data$TS, na.rm=T)) |> dplyr::arrange(.data$growing_year)
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
      ts_bounds_method = if (settings$ts_regression == "none") "step01" else "regressed",
      dt = dt, nobs_threshold = nobs_threshold,
      control_year = control_year
    )
  )
}
