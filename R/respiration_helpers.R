# Shared helper functions for workflow 01_02 (respiration filtering).

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

# Wrap day-of-year so that a growing season straddling New Year is one
# contiguous interval. Days before `origin` belong to the growing year that
# began the previous calendar year, and are pushed past the end of it: with
# `origin = 183`, DOY runs 183..548 and 1 January is 367.
#
# `366` is the shift whatever the year's length, as in the original workflows.
# The consequence is that in a non-leap growing year the wrapped series steps
# 365 -> 367 over New Year, leaving DOY 366 empty; the day is not lost, only
# labelled one higher than the elapsed-day count. Every downstream use is a
# `between(DOY, ...)` window or a by-DOY average, both of which tolerate the
# skip, and the alternative -- a year-length-dependent shift -- would give the
# same calendar date two different DOYs depending on the year.
#
# The invariant the rest of the pipeline rests on: `DOY > 366` if and only if
# the row falls in the calendar year *after* the one its growing year started
# in. See `growing_year_of()`.
wrap_growing_doy <- function(doy, origin) {
  if (origin <= 1) return(doy)
  ifelse(doy < origin, doy + 366, doy)
}

# The inverse, for the places that need a real day of year back: building a
# calendar timestamp from a growing-season bound, and handing season starts to
# REddyProc, which wants a yday. A no-op on unwrapped values.
unwrap_growing_doy <- function(doy) {
  ifelse(doy > 366, doy - 366, doy)
}

# The growing year a row belongs to, from its (possibly wrapped) DOY and its
# calendar year. Unwrapped sites never exceed DOY 366, so this is the identity
# for them; see `wrap_growing_doy()` for why the test is the right one.
growing_year_of <- function(doy, year) {
  year <- as.integer(year)
  dplyr::if_else(doy <= 366, year, year - 1L)
}


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

  # The original workflows used three different cut-offs for "this day-of-year
  # counts as growing season", and they are not interchangeable. At a site whose
  # minimum mean NEE is -10, "capped" admits every day down to -0.8 while
  # "uncapped" stops at -2.0, which moves gStart/gEnd by weeks -- and those feed
  # the u-star season factor, the gap thresholds, and the window layout.
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

  # Original comment: 
  # """
  # I will use the mean of the first three values, and the mean of the last three values
  # """
  # TODO: What is this logic?
  gStart <- tmp$DOY[[7]] - 4
  gEnd <- tmp$DOY[[nrow(tmp) - 6]] + 4

  nonnegative_ts <- which(nee_yearly[[ts_col]] >= 0)
  if (length(nonnegative_ts) > 0) {
    gStart <- max(gStart, min(nee_yearly$DOY[nonnegative_ts]))
  }

  # What the detector said, before site_info.csv has its say. Twenty-four
  # sites declare a `gStart` or `gEnd` literal that replaces the detected
  # bound, so the season this function returns is *detected or overridden*,
  # and the two are only separable if the detected pair travels too. The
  # `force_detect` season strategy reads them; nothing in the manuscript path
  # does.
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
    gStart_detected = gStart_detected, gEnd_detected = gEnd_detected
  )
}

# The growing-season start and end as calendar timestamps, one pair per year,
# so that `get_good_years()` can see a gap that runs off either end of the
# season. The TIMESTAMP is built from the *unwrapped* bound -- it has to be a
# real date -- while the DOY column keeps the wrapped value, because that is
# what the `DOY >= gStart & DOY <= gEnd` filter downstream compares against.
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

  year_str <- site_info[["year_removed"]]
  if (!is.na(year_str)) {
    year_parts <- strsplit(year_str, ",")[[1]]
    years2remove <- unlist(lapply(year_parts, function(part) {
      if (grepl(":", part)) {
        rng <- as.numeric(strsplit(part, ":")[[1]])
        seq(rng[1], rng[2])
      } else {
        as.numeric(part)
      }
    }))
    if (length(years2remove) >= 1) {
      good_years <- setdiff(good_years, years2remove)
    }
  }

  good_years
}

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

write_respiration_outputs <- function(ac, measured_night, output_dir, site_name) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_files <- file.path(output_dir, paste0(site_name, c("_ac.csv", "_nightNEE.csv")))
  write.csv(ac, file = output_files[1], row.names = FALSE)
  write.csv(measured_night, file = output_files[2], row.names = FALSE)
}
