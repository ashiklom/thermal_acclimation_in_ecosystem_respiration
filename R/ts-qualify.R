# The part of step 01 that depends on the soil-temperature column (Junna
# Wang, 2025), as one function of that column, so that any candidate column
# can be qualified the way the manuscript qualified its own.
#
# prep_site_euroflux() and prep_site_ameriflux() (R/step01-prep.R) build a
# site's record without it: `base`, one row per record (sorted by time) with
#   TIMESTAMP, YEAR, MONTH, DAY, DOY, HOUR, MINUTE   (DOY counted on from 366
#                                                    over New Year at wrapped
#                                                    southern-hemisphere sites)
#   NEE, TA, SWC, NEE_uStar_f, SW_IN, daytime (and, FLUXNET-format sources,
#   NEE_QC and GPP_DT): the columns of step 01's output tables
#   night_ok   the record passes every nighttime filter that does not involve
#              soil temperature (night, NEE quality and range, u*, ...)
# and `params`, the season and screening settings that do not depend on soil
# temperature (list):
#   site_ID, source ("fluxnet" or "ameriflux"), southern_hemisphere,
#   dt_secs (record time step), uptake_doy (days of year whose mean NEE marks
#   the growing season), gStart_nee (the season start from NEE alone),
#   gStart_site (site_info's override, or NA), gEnd, tstart_floor (2 at the
#   sites whose NEE below 2 deg C soil temperature is unreliable, else NA),
#   floor_before_gaps (whether the TS >= 2 filter precedes the year screening),
#   gap_fixed (site-specific c(gap_max, gap_total) thresholds, or NULL),
#   years_removed (years excluded for disturbances or data quality)
#
# ts_column_tables(base, params, ts, ts_ok) then does what step 01 does with
# its soil temperature `ts` (one value per row of `base`), given where it is
# usable (`ts_ok`): the growing-season start and temperature range, the
# nighttime table, the screening of years with large gaps, and the
# half-hourly table. It returns step 01's list(ac, night, feature,
# climatology), and `keep`: which rows of `base` the nighttime table holds.

#' Day-of-year means of soil temperature
#'
#' Day-of-year means of soil temperature over the whole record.
#'
#' @param base The site's record (uses DOY).
#' @param ts Soil temperature, one value per row of `base`.
#' @return A data frame of DOY and mean TS, sorted by DOY.
#' @noRd
ts_doy_means <- function(base, ts) {
  data.frame(DOY = base$DOY, TS = ts) |>
    dplyr::group_by(.data$DOY) |>
    dplyr::summarise(TS = mean(.data$TS, na.rm = T)) |>
    dplyr::arrange(.data$DOY)
}

#' Growing-season start and soil-temperature range
#'
#' Growing-season start and temperature range from the day-of-year means of soil temperature:
#' the start no earlier than the first day whose mean soil temperature is >= 0 C (unless
#' site_info sets it), the range the 2.5-97.5% quantiles of the mean soil temperature over the
#' NEE-uptake days, tStart at least params$tstart_floor where that is set.
#'
#' @param TS_doy Day-of-year means of soil temperature (ts_doy_means()).
#' @param params The site's settings (see the file header).
#' @return list(gStart, gEnd, tStart, tEnd); gEnd is params$gEnd.
#' @noRd
ts_season <- function(TS_doy, params) {
  gStart <- max(params$gStart_nee, min(TS_doy$DOY[which(TS_doy$TS >= 0)]))
  if (!is.na(params$gStart_site)) {
    gStart <- as.numeric(params$gStart_site)
  }
  uptake <- TS_doy$TS[TS_doy$DOY %in% params$uptake_doy]
  tStart <- quantile(uptake, c(0.025), na.rm = T)
  tEnd <- quantile(uptake, c(0.975), na.rm = T)
  if (!is.na(params$tstart_floor)) {
    tStart <- max(tStart, params$tstart_floor)
  }
  list(gStart = gStart, gEnd = params$gEnd, tStart = tStart, tEnd = tEnd)
}

#' Growing years without large gaps
#'
#' Growing years whose nighttime records have no large gaps within the growing season: the
#' longest gap (days) below gap_max_thresh and the gaps longer than 14 days together below
#' gap_total_thresh of the season (by default max(31, 22.5% of the season) and max(1/3, that
#' share of the season); params$gap_fixed where set). `night` holds the records (TIMESTAMP,
#' DOY); `years` the years whose season bounds count as records, so that gaps at either end of a
#' season count too. Years in params$years_removed are left out.
#'
#' @param night The nighttime records (TIMESTAMP, DOY).
#' @param gStart Growing-season start (day of year).
#' @param gEnd Growing-season end (day of year).
#' @param years Years whose season bounds count as records.
#' @param params The site's settings (see the file header).
#' @return The good growing years.
#' @noRd
ts_good_years <- function(night, gStart, gEnd, years, params) {
  if (!is.null(params$gap_fixed)) {
    gap_max_thresh <- params$gap_fixed[1]
    gap_total_thresh <- params$gap_fixed[2]
  } else {
    gap_max_thresh <- max(31, (gEnd-gStart+1) * 0.225)
    gap_total_thresh <- max(1/3, gap_max_thresh / (gEnd-gStart+1))
  }
  a_check_gaps <- night[, c("TIMESTAMP", "DOY")]

  # adding the start and end of growing season date
  if (params$southern_hemisphere) {
    gStart_original <- ifelse(gStart > 366, gStart-366, gStart)
    gEnd_original <- ifelse(gEnd > 366, gEnd-366, gEnd)
  } else {
    gStart_original <- gStart
    gEnd_original <- gEnd
  }
  a_gs_dates <- data.frame(TIMESTAMP = c(as.POSIXct((gStart_original - 1) * 86400 + params$dt_secs / 2, origin = paste0(years, "-01-01"), tz = "UTC"),
                                         as.POSIXct((gEnd_original - 1) * 86400 + params$dt_secs / 2, origin = paste0(years, "-01-01"), tz = "UTC")),
                           DOY = c(rep(gStart, length(years)), rep(gEnd, length(years))))
  a_check_gaps <- a_check_gaps |> rbind(a_gs_dates) |> unique() |> dplyr::arrange(.data$TIMESTAMP)

  good_years <- a_check_gaps |>
    dplyr::mutate(growing_year = dplyr::case_when(.data$DOY <= 366 ~ lubridate::year(.data$TIMESTAMP),
                                    TRUE ~ lubridate::year(.data$TIMESTAMP) - 1)) |>
    dplyr::arrange(.data$growing_year, .data$TIMESTAMP) |>  # <-- Sort before lag
    dplyr::group_by(.data$growing_year) |>
    dplyr::filter(.data$DOY >= .env$gStart & .data$DOY <= .env$gEnd) |>
    dplyr::mutate(lag_date = dplyr::lag(.data$TIMESTAMP)) |>
    dplyr::mutate(gap = as.numeric(difftime(.data$TIMESTAMP, .data$lag_date, units = "days"))) |>
    dplyr::mutate(gap_large = ifelse(.data$gap > 14, .data$gap, 0)) |>
    dplyr::filter(!is.na(.data$gap)) |>
    dplyr::summarise(gap_max = max(.data$gap, na.rm=T), gap_total = sum(.data$gap_large, na.rm=T) / (.env$gEnd - .env$gStart + 1)) |>
    dplyr::filter(.data$gap_max < .env$gap_max_thresh &  .data$gap_total < .env$gap_total_thresh) |>
    dplyr::distinct(.data$growing_year) |> dplyr::pull()

  setdiff(good_years, params$years_removed)
}

#' Step 01's tables from one soil-temperature column
#'
#' Does what step 01 does with its soil temperature `ts` (one value per row of `base`), given
#' where it is usable (`ts_ok`): the growing-season start and temperature range, the nighttime
#' table, the screening of years with large gaps, and the half-hourly table (at AmeriFlux sites
#' with TA, TS and NEE_uStar_f gap-filled by their day-of-year x time-of-day means).
#'
#' @param base The site's record without soil temperature (see the file header), sorted by
#'   time.
#' @param params The site's settings (see the file header).
#' @param ts Soil temperature, one value per row of `base`.
#' @param ts_ok Whether `ts` is usable, one value per row of `base`.
#' @return Step 01's list(ac, night, feature, climatology), and `keep`: which rows of `base` the
#'   nighttime table holds. feature is one row: site_ID, gStart, gEnd, tStart (at least 0),
#'   tEnd, nyear.
#' @noRd
ts_column_tables <- function(base, params, ts, ts_ok) {
  stopifnot(length(ts) == nrow(base), length(ts_ok) == nrow(base), !is.unsorted(base$TIMESTAMP))
  season <- ts_season(ts_doy_means(base, ts), params)
  gStart <- season$gStart
  gEnd <- season$gEnd
  base$TS <- ts
  climatology <- data.frame(DOY = base$DOY, TA = base$TA, TS = ts, season = base$DOY %in% params$uptake_doy)

  # nighttime records with usable soil temperature
  base$growing_year <- dplyr::if_else(base$DOY <= 366, base$YEAR, base$YEAR - 1)
  usable <- base$night_ok & ts_ok
  years <- min(base$YEAR[usable]):max(base$YEAR[usable])
  # some sites NEE measurements under TS < 2C was inaccurate
  floor_ok <- if (is.na(params$tstart_floor)) rep(TRUE, nrow(base)) else !is.na(ts) & ts >= params$tstart_floor
  screened <- if (params$floor_before_gaps) usable & floor_ok else usable
  good_years <- ts_good_years(base[screened, ], gStart, gEnd, years, params)
  keep <- usable & floor_ok & base$growing_year %in% good_years

  # (dplyr keeps the columns' attributes, e.g. REddyProc's on NEE_uStar_f)
  night <- base |> dplyr::filter(.env$keep) |>
    dplyr::select(dplyr::all_of(c("YEAR", "MONTH", "DAY", "DOY", "HOUR", "MINUTE", "NEE", "TA", "TS", "SWC")))

  # the half-hourly table over the years from the first to the last kept
  iStart <- min(night$YEAR)
  iEnd <- max(night$YEAR)
  ac <- base |> dplyr::filter(dplyr::between(.data$YEAR, .env$iStart, .env$iEnd))
  if (params$source == "fluxnet") {
    # (GPP_DT where the source has it)
    ac <- ac |> dplyr::select(dplyr::any_of(c("YEAR", "MONTH", "DAY", "DOY", "HOUR", "MINUTE", "NEE", "NEE_QC", "TA", "TS", "SWC", "NEE_uStar_f", "daytime", "SW_IN", "GPP_DT")))
  } else {
    ac <- ac |> dplyr::select(dplyr::all_of(c("YEAR", "MONTH", "DAY", "DOY", "HOUR", "MINUTE", "NEE_uStar_f", "TA", "TS", "SWC", "SW_IN", "daytime")))
    # I have to gap fill TA, TS, NEE;
    T_gf <- ac |> dplyr::group_by(.data$DOY, .data$HOUR, .data$MINUTE) |> dplyr::summarise(TA_gf=mean(.data$TA, na.rm=T), TS_gf=mean(.data$TS, na.rm=T), NEE_gf=mean(.data$NEE_uStar_f, na.rm=T)) |>  # for gap fill only.
      dplyr::arrange(.data$DOY, .data$HOUR, .data$MINUTE)
    # ensure no NA values: use the same time of the previous day
    steps_per_day <- 86400 / params$dt_secs
    for (i in c("TA_gf", "TS_gf", "NEE_gf")) {
      na.gf <- which(is.na(T_gf[[i]]))
      for (j in na.gf) {
        T_gf[j, i] <- T_gf[max(j - steps_per_day, 1), i]
      }
    }
    ac <- ac |> dplyr::left_join(T_gf, by=c("DOY", "HOUR", "MINUTE"))
    ac$TA[is.na(ac$TA)] <- ac$TA_gf[is.na(ac$TA)]
    ac$TS[is.na(ac$TS)] <- ac$TS_gf[is.na(ac$TS)]
    ac$NEE_uStar_f[is.na(ac$NEE_uStar_f)] <- ac$NEE_gf[is.na(ac$NEE_uStar_f)]
  }

  feature <- data.frame(site_ID=params$site_ID, gStart=gStart, gEnd=gEnd, tStart=max(season$tStart, 0.0), tEnd=season$tEnd, nyear=length(good_years))
  list(ac = ac, night = night, feature = feature, climatology = climatology, keep = keep)
}
