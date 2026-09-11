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

adjust_southern_hemisphere <- function(dat, lat) {
  if (lat < 0) {
    dat$DOY[dat$DOY < 183] <- dat$DOY[a$DOY < 183] + 366
  }
  dat
}


detect_growing_season <- function(nee_yearly, site_info, nee_col = "NEE", ts_col = "TS",
                                  filter_fn = NULL) {
  if (is.null(filter_fn)) {
    filter_fn <- function(x) {
      x[[nee_col]] < max(min(x[[nee_col]], na.rm = TRUE) * 0.2, -0.8)
    }
  }
  tmp <- nee_yearly |> dplyr::filter(filter_fn(nee_yearly))
  if (nrow(tmp) < 8) stop(site_name, " has too few seasonal points to estimate growing season.")

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
    gStart_adj <- ifelse(gStart > 366, gStart - 366, gStart)
    gEnd_adj <- ifelse(gEnd > 366, gEnd - 366, gEnd)
  } else {
    gStart_adj <- gStart
    gEnd_adj <- gEnd
  }
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

get_good_years <- function(measured, gStart, gEnd, dt, name_site) {

  site_info <- get_site_info(name_site)
  southern_hemisphere <- site_info[["LAT"]] < 0

  gap_thresh <- compute_gap_thresholds(gStart, gEnd, name_site)
  gap_max_thresh <- gap_thresh$gap_max_thresh
  gap_total_thresh <- gap_thresh$gap_total_thresh

  yStart <- min(measured$YEAR)
  yEnd <- max(measured$YEAR)

  a_gs_dates <- build_gs_dates(gStart, gEnd, yStart, yEnd, dt, southern_hemisphere)
  a_check_gaps <- measured |>
    dplyr::select("TIMESTAMP", "DOY") |>
    dplyr::bind_rows(a_gs_dates) |>
    unique() |>
    dplyr::arrange(.data$TIMESTAMP)

  good_years <- a_check_gaps |>
    dplyr::mutate(growing_year = dplyr::case_when(
      DOY <= 366 ~ lubridate::year(TIMESTAMP),
      TRUE ~ lubridate::year(TIMESTAMP) - 1
    )) |>
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
