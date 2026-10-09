# Growing season, growing year and year qualification (step 01).

#' Parse a site's hand-removed years
#'
#' @param value The `year_removed` entry of site_info.csv: comma-separated
#'   years or `start:end` ranges, e.g. `"2007, 2008, 2010"` or `"2007:2009"`.
#' @return Numeric vector of years; empty if `value` is NA or blank.
parse_removed_years <- function(value) {
  if (is.na(value) || !nzchar(trimws(value))) return(numeric())
  unlist(lapply(strsplit(value, ",")[[1]], function(part) {
    part <- trimws(part)
    if (grepl(":", part)) {
      bounds <- as.numeric(strsplit(part, ":")[[1]])
      seq(bounds[1], bounds[2])
    } else {
      as.numeric(part)
    }
  }))
}

#' Calendar columns from the midpoint of each interval
#'
#' @param a Data frame of half-hourly flux data with a 12-digit `TIMESTAMP_START`.
#' @param dt The time step, a difftime.
#' @return `a` with `TIMESTAMP` (POSIXct, UTC, the interval midpoint) and
#'   `YEAR`, `MONTH`, `DAY`, `DOY`, `HOUR`, `MINUTE` taken from it.
add_timestamp_columns <- function(a, dt) {
  a$TIMESTAMP <- lubridate::ymd_hm(a$TIMESTAMP_START) + dt / 2
  a$YEAR <- lubridate::year(a$TIMESTAMP)
  a$MONTH <- lubridate::month(a$TIMESTAMP)
  a$DAY <- lubridate::day(a$TIMESTAMP)
  a$DOY <- lubridate::yday(a$TIMESTAMP)
  a$HOUR <- lubridate::hour(a$TIMESTAMP)
  a$MINUTE <- lubridate::minute(a$TIMESTAMP)
  a
}

#' Wrap day of year into the growing year
#'
#' Wrap day-of-year so that a growing season straddling New Year is one
#' contiguous interval. Days before `origin` belong to the growing year that
#' began the previous calendar year, and are pushed past the end of it: with
#' `origin = 183`, DOY runs 183..548 and 1 January is 367.
#'
#' The shift is 366 whatever the year's length, as in the original, so a
#' non-leap year skips DOY 366; see docs/growing-year.md. Invariant: `DOY > 366`
#' iff the row is in the calendar year after its growing year began.
#'
#' @param doy Calendar day of year.
#' @param origin The day of year the site's growing year begins, from
#'   `growing_year_start()`.
#' @return `doy`, with days before `origin` shifted up by 366; unchanged if
#'   `origin <= 1`.
wrap_growing_doy <- function(doy, origin) {
  if (origin <= 1) return(doy)
  ifelse(doy < origin, doy + 366, doy)
}

#' Unwrap a growing-year day of year
#'
#' The inverse, where a real day of year is needed. A no-op on unwrapped values.
#'
#' @param doy Day of year, possibly wrapped by `wrap_growing_doy()`.
#' @return `doy`, with values above 366 shifted down by 366.
unwrap_growing_doy <- function(doy) {
  ifelse(doy > 366, doy - 366, doy)
}

#' The growing year a row belongs to
#'
#' From its (possibly wrapped) DOY.
#'
#' @param doy Day of year, possibly wrapped by `wrap_growing_doy()`.
#' @param year Calendar year of the row.
#' @return Integer vector: `year`, or `year - 1` where `doy` is past 366.
growing_year_of <- function(doy, year) {
  year <- as.integer(year)
  dplyr::if_else(doy <= 366, year, year - 1L)
}


#' Detect a site's growing season from its NEE climatology
#'
#' @param ac Data frame of half-hourly flux data with `DOY` and the NEE and
#'   soil-temperature columns.
#' @param site_info One row of the site declaration table.
#' @param nee_col Name of the NEE column.
#' @param ts_col Name of the soil-temperature column.
#' @param nee_threshold Growing-season NEE cut-off: `"capped"`, `"uncapped"` or
#'   `"zero"`.
#' @return A list: `gStart` and `gEnd`, the season bounds as (possibly wrapped)
#'   DOY after the site_info.csv overrides; `tStart` and `tEnd`, the 2.5/97.5
#'   percentiles of DOY-mean soil temperature over the qualifying DOYs;
#'   `gStart_detected` and `gEnd_detected`, the bounds before the overrides;
#'   and `uptake_doy`, the qualifying DOYs themselves (the NEE-uptake days).
detect_growing_season <- function(ac, site_info, nee_col = "NEE", ts_col = "TS",
                                  nee_threshold) {
  nee_threshold <- match.arg(nee_threshold, c("capped", "uncapped", "zero"))
  name_site <- site_info[["site_ID"]]

  nee_yearly <- ac |>
    dplyr::summarise(
      NEE = mean(.data[[nee_col]], na.rm = TRUE),
      TS = mean(.data[[ts_col]], na.rm = TRUE),
      .by = "DOY",
    ) |>
    dplyr::arrange(.data$DOY)

  # The original workflows' three growing-season cut-offs, which differ by
  # weeks at a site with strong uptake.
  nee_min <- min(nee_yearly[[nee_col]], na.rm = TRUE)
  cutoff <- switch(nee_threshold,
    capped = max(nee_min * 0.2, -0.8), # 01_02a, every EuroFlux site but two
    uncapped = nee_min * 0.2,          # 01_02b, every AmeriFlux site
    zero = 0.0                         # 01_02a, FI-Sod and DE-RuC
  )
  tmp <- nee_yearly |> dplyr::filter(.data[[nee_col]] < cutoff)
  if (nrow(tmp) < 8) {
    stop(name_site, " has too few seasonal points to estimate growing season.")
  }

  # As the original: the 7th qualifying DOY from each end, widened by 4 days.
  gStart <- tmp$DOY[[7]] - 4
  gEnd <- tmp$DOY[[nrow(tmp) - 6]] + 4

  nonnegative_ts <- which(nee_yearly[[ts_col]] >= 0)
  if (length(nonnegative_ts) > 0) {
    gStart <- max(gStart, min(nee_yearly$DOY[nonnegative_ts]))
  }

  # The detector's answer, before the site_info.csv overrides (24 sites);
  # the `force_detect` strategy reads it.
  gStart_detected <- gStart
  gEnd_detected <- gEnd

  if (!is.na(site_info[["gStart"]])) {
    gStart <- as.numeric(site_info[["gStart"]])
  }
  if (!is.na(site_info[["gEnd"]])) {
    gEnd <- as.numeric(site_info[["gEnd"]])
  }

  tStart <- quantile(tmp[[ts_col]], 0.025, na.rm = TRUE)
  tEnd <- quantile(tmp[[ts_col]], 0.975, na.rm = TRUE)

  list(
    gStart = gStart, gEnd = gEnd, tStart = tStart, tEnd = tEnd,
    gStart_detected = gStart_detected, gEnd_detected = gEnd_detected,
    uptake_doy = tmp$DOY
  )
}

#' Season start and end as timestamps, one pair per year
#'
#' So the gap scan sees gaps running off either end. TIMESTAMP uses the
#' unwrapped bound (a real date); DOY keeps the wrapped one, which the season
#' filter compares.
#'
#' @param gStart,gEnd Growing-season bounds, as (possibly wrapped) DOY.
#' @param yStart,yEnd First and last calendar year of the record.
#' @param dt The time step, a difftime.
#' @return Data frame with `TIMESTAMP` (POSIXct, UTC) and `DOY`: the season
#'   start in every year, then the season end in every year.
build_gs_dates <- function(gStart, gEnd, yStart, yEnd, dt) {
  gStart_adj <- unwrap_growing_doy(gStart)
  gEnd_adj <- unwrap_growing_doy(gEnd)
  data.frame(
    TIMESTAMP = c(
      as.POSIXct(
        (gStart_adj - 1) * 86400 + as.numeric(dt / 2, units = "secs"),
        origin = paste0(yStart:yEnd, "-01-01"), tz = "UTC"
      ),
      as.POSIXct(
        (gEnd_adj - 1) * 86400 + as.numeric(dt / 2, units = "secs"),
        origin = paste0(yStart:yEnd, "-01-01"), tz = "UTC"
      )
    ),
    DOY = c(rep(gStart, yEnd - yStart + 1), rep(gEnd, yEnd - yStart + 1))
  )
}

#' Growing years with few enough gaps in the season
#'
#' @param measured Data frame of the quality-filtered nighttime observations,
#'   with `TIMESTAMP`, `YEAR` and `DOY`.
#' @param gStart,gEnd Growing-season bounds, as (possibly wrapped) DOY.
#' @param dt The time step, a difftime.
#' @param site_info One row of the site declaration table.
#' @return Integer vector of the growing years whose largest and total in-season
#'   gaps are under `compute_gap_thresholds()`'s thresholds.
get_good_years <- function(measured, gStart, gEnd, dt, site_info) {

  name_site <- site_info[["site_ID"]]

  gap_thresh <- compute_gap_thresholds(gStart, gEnd, name_site)
  gap_max_thresh <- gap_thresh$gap_max_thresh
  gap_total_thresh <- gap_thresh$gap_total_thresh

  yStart <- min(measured$YEAR)
  yEnd <- max(measured$YEAR)

  a_gs_dates <- build_gs_dates(gStart, gEnd, yStart, yEnd, dt)
  a_check_gaps <- measured |>
    dplyr::select("TIMESTAMP", "DOY") |>
    dplyr::bind_rows(a_gs_dates) |>
    unique() |>
    dplyr::arrange(.data$TIMESTAMP)

  good_years <- a_check_gaps |>
    dplyr::mutate(growing_year = growing_year_of(.data$DOY, lubridate::year(.data$TIMESTAMP))) |>
    dplyr::arrange(growing_year, TIMESTAMP) |>
    dplyr::group_by(growing_year) |>
    dplyr::filter(DOY >= gStart & DOY <= gEnd) |>
    dplyr::mutate(lag_date = dplyr::lag(TIMESTAMP)) |>
    dplyr::mutate(gap = as.numeric(difftime(TIMESTAMP, lag_date, units = "days"))) |>
    dplyr::mutate(gap_large = ifelse(gap > 14, gap, 0)) |>
    dplyr::filter(!is.na(gap)) |>
    dplyr::summarise(
      gap_max = max(gap, na.rm = TRUE),
      gap_total = sum(gap_large, na.rm = TRUE) / (gEnd - gStart + 1),
      .groups = "drop_last"
    ) |>
    dplyr::filter(gap_max < gap_max_thresh & gap_total < gap_total_thresh) |>
    dplyr::distinct(growing_year) |>
    dplyr::pull()

  # Minus the years removed by hand in site_info.csv.
  setdiff(good_years, parse_removed_years(site_info[["year_removed"]]))
}

#' Gap thresholds a growing year must pass
#'
#' @param gStart,gEnd Growing-season bounds, as (possibly wrapped) DOY.
#' @param site_name Site ID.
#' @return A list: `gap_max_thresh`, the largest allowed gap in days, and
#'   `gap_total_thresh`, the allowed fraction of the season in gaps over 14 days.
compute_gap_thresholds <- function(gStart, gEnd, site_name) {
  if (site_name %in% c("US-ICt", "US-ICh", "US-ICs", "BR-Ma2", "BR-Sa1")) {
    list(gap_max_thresh = 60, gap_total_thresh = 0.8)
  } else if (site_name %in% c("ZA-Kru", "FI-Sod", "GF-Guy")) {
    list(gap_max_thresh = 60, gap_total_thresh = 0.7)
  } else {
    gap_max_thresh <- max(31, (gEnd - gStart + 1) * 0.225)
    gap_total_thresh <- max(1 / 3, gap_max_thresh / (gEnd - gStart + 1))
    list(gap_max_thresh = gap_max_thresh, gap_total_thresh = gap_total_thresh)
  }
}

#' Write a site's respiration tables to CSV
#'
#' @param ac Data frame of the site's half-hourly data.
#' @param measured_night Data frame of the site's nighttime NEE observations.
#' @param output_dir Directory to write into; created if absent.
#' @param site_name Site ID.
#' @return Called for its side effect of writing `<site_name>_ac.csv` and
#'   `<site_name>_nightNEE.csv`; returns `NULL`, invisibly.
write_respiration_outputs <- function(ac, measured_night, output_dir, site_name) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_files <- file.path(output_dir, paste0(site_name, c("_ac.csv", "_nightNEE.csv")))
  write.csv(ac, file = output_files[1], row.names = FALSE)
  write.csv(measured_night, file = output_files[2], row.names = FALSE)
}
