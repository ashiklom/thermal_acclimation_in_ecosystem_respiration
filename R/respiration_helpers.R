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
  a$TIMESTAMP <- ymd_hm(a$TIMESTAMP_START) + dt / 2
  a$YEAR <- year(a$TIMESTAMP)
  a$MONTH <- month(a$TIMESTAMP)
  a$DAY <- day(a$TIMESTAMP)
  a$DOY <- yday(a$TIMESTAMP)
  a$HOUR <- hour(a$TIMESTAMP)
  a$MINUTE <- minute(a$TIMESTAMP)
  a
}

adjust_southern_hemisphere <- function(a, lat) {
  if (lat < 0) {
    a$DOY[a$DOY < 183] <- a$DOY[a$DOY < 183] + 366
  }
  a
}

adjust_south_hemisphere_measured <- function(measured, lat) {
  if (lat < 0) {
    measured$DOY[measured$DOY < 183] <- measured$DOY[measured$DOY < 183] + 366
  }
  measured
}

detect_growing_season <- function(nee_yearly, site_info, nee_col = "NEE", ts_col = "TS",
                                  filter_fn = NULL, site_name = NULL) {
  if (is.null(filter_fn)) {
    filter_fn <- function(x) {
      x[[nee_col]] < max(min(x[[nee_col]], na.rm = TRUE) * 0.2, -0.8)
    }
  }
  tmp <- nee_yearly |> filter(filter_fn(nee_yearly))
  if (nrow(tmp) < 8) stop(site_name, " has too few seasonal points to estimate growing season.")

  gStart <- as.integer(mean(tmp$DOY[7])) - 4
  gEnd <- as.integer(mean(tmp$DOY[nrow(tmp) - 6])) + 4

  nonnegative_ts <- which(nee_yearly[[ts_col]] >= 0)
  if (length(nonnegative_ts) > 0) {
    gStart <- max(gStart, min(nee_yearly$DOY[nonnegative_ts]))
  }
  if (!is.na(site_info[["gStart"]])) {
    gStart <- as.numeric(site_info[["gStart"]])
  }
  if (!is.na(site_info[["gEnd"]])) {
    gEnd <- as.numeric(site_info[["gEnd"]])
  }

  tStart <- quantile(tmp[[ts_col]], 0.025, na.rm = TRUE)
  tEnd <- quantile(tmp[[ts_col]], 0.975, na.rm = TRUE)

  list(gStart = gStart, gEnd = gEnd, tStart = tStart, tEnd = tEnd)
}

build_gs_dates <- function(gStart, gEnd, yStart, yEnd, dt, southern_hemisphere) {
  if (southern_hemisphere) {
    gStart_orig <- ifelse(gStart > 366, gStart - 366, gStart)
    gEnd_orig <- ifelse(gEnd > 366, gEnd - 366, gEnd)
  } else {
    gStart_orig <- gStart
    gEnd_orig <- gEnd
  }
  data.frame(
    TIMESTAMP = c(
      as.POSIXct(
        (gStart_orig - 1) * 86400 + as.numeric(dt / 2, units = "secs"),
        origin = paste0(yStart:yEnd, "-01-01"), tz = "UTC"
      ),
      as.POSIXct(
        (gEnd_orig - 1) * 86400 + as.numeric(dt / 2, units = "secs"),
        origin = paste0(yStart:yEnd, "-01-01"), tz = "UTC"
      )
    ),
    DOY = c(rep(gStart, yEnd - yStart + 1), rep(gEnd, yEnd - yStart + 1))
  )
}

filter_good_years <- function(a_measure_night_complete, gStart, gEnd,
                              gap_max_thresh, gap_total_thresh,
                              southern_hemisphere, dt, site_name = NULL) {
  yStart <- min(a_measure_night_complete$YEAR)
  yEnd <- max(a_measure_night_complete$YEAR)
  a_check_gaps <- a_measure_night_complete[, c("TIMESTAMP", "DOY")]

  a_gs_dates <- build_gs_dates(gStart, gEnd, yStart, yEnd, dt, southern_hemisphere)
  a_check_gaps <- a_check_gaps |>
    rbind(a_gs_dates) |>
    unique() |>
    arrange(TIMESTAMP)

  good_years <- a_check_gaps |>
    mutate(growing_year = case_when(
      DOY <= 366 ~ year(TIMESTAMP),
      TRUE ~ year(TIMESTAMP) - 1
    )) |>
    arrange(growing_year, TIMESTAMP) |>
    group_by(growing_year) |>
    filter(DOY >= gStart & DOY <= gEnd) |>
    mutate(lag_date = dplyr::lag(TIMESTAMP)) |>
    mutate(gap = as.numeric(difftime(TIMESTAMP, lag_date, units = "days"))) |>
    mutate(gap_large = ifelse(gap > 14, gap, 0)) |>
    filter(!is.na(gap)) |>
    summarise(
      gap_max = max(gap, na.rm = TRUE),
      gap_total = sum(gap_large, na.rm = TRUE) / (gEnd - gStart + 1)
    ) |>
    filter(gap_max < gap_max_thresh & gap_total < gap_total_thresh) |>
    distinct(growing_year) |>
    pull()

  if (length(good_years) == 0 && !is.null(site_name)) {
    stop(site_name, " has no complete growing years.")
  }
  good_years
}

compute_gap_thresholds <- function(gStart, gEnd, site_name, source) {
  if (
    source == "AmeriFlux_BASE" &&
      site_name %in% c("US-ICt", "US-ICh", "US-ICs", "BR-Ma2", "BR-Sa1")
  ) {
    list(gap_max_thresh = 60, gap_total_thresh = 0.8)
  } else if (
    source %in% c("FLUXNET", "FLUXNET2015", "ICOS") &&
      site_name %in% c("ZA-Kru", "FI-Sod", "GF-Guy")
  ) {
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
