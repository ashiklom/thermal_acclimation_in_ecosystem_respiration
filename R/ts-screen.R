# Soil-temperature sensor screen: is a soil-temperature record physically
# plausible as buried soil temperature, and which parts of it are not?
#
# Heat conduction damps and delays the daily cycle of air temperature with
# depth z: the daily range shrinks by exp(-z/d) and its peak arrives
# (z/d) * 24 / (2 pi) hours later (d: damping depth). So a sensor that reports
# air temperature (or sits at the surface) has a range ratio near 1 and no
# lag; one that leads air, is flat, or repeats values is not measuring soil.
# The rules and thresholds of the whole-record verdict follow the `alexey`
# branch's screen (calibrated there against the project's hand-made calls);
# the screen here adds a heat-equation consistency check (the damping and the
# lag must imply the same depth), restricts the diurnal diagnostics to the
# growing season (the period TAS is estimated over, free of snow cover), and
# screens every year separately, so that a partly bad record yields the time
# windows that are bad.
#
# Everything depends only on the input data (step 01's record and the raw
# sensor), not on any fit. Outputs per site and column (ts_screen_site()):
#   summary   one row: diagnostics, `verdict` (GOOD, SUSPECT, BAD), `flags`,
#             `bad_fraction` (share of the record inside bad windows)
#   yearly    one row per growing year: the year's diagnostics and whether it
#             is an outlier among the site's years
#   windows   machine-readable bad windows (start, end timestamps, inclusive;
#             scale "changepoint", "year" or "period"; rule; value and
#             reference), for a later reconstruction step (ts_screen_mask()).
#             A changepoint (the yearly diagnostics shift for good, e.g. a
#             sensor moved or replaced) makes everything before it a window:
#             by convention the latest segment, the current sensor set-up, is
#             the reference, as physics alone cannot tell which side is wrong
#   diurnal   growing-season mean diurnal cycle of the column and of air
#             temperature (for display)

#' Range ratio above which a series is air-like (with a lag below TS_SCREEN_AIRLIKE_LAG)
#' @noRd
TS_SCREEN_AIRLIKE_AMP <- 0.85
#' Lag (hours) below which an air-like range ratio counts as air
#' @noRd
TS_SCREEN_AIRLIKE_LAG <- 0.75
#' Range ratio below which a series has no diurnal signal
#' @noRd
TS_SCREEN_FLAT_AMP <- 0.03
#' Share of the record in stuck runs above which a series is stuck
#' @noRd
TS_SCREEN_STUCK_FRAC <- 0.10
#' Lag (hours) below which soil leads air (unphysical)
#' @noRd
TS_SCREEN_LEADS_H <- -0.75
#' Minimum share of the record with a value
#' @noRd
TS_SCREEN_COVERAGE_MIN <- 0.40
#' Allowed ratio between the lag-implied and the damping-implied depth
#' (conduction: 1); outside [1/x, x] the series is not conductive
#' @noRd
TS_SCREEN_DEPTH_RATIO <- 3
#' Robust z-score above which a year is an outlier among a site's years, and
#' the minimum absolute deviations that also have to be exceeded (offset in
#' deg C, range ratio, lag in hours)
#' @noRd
TS_SCREEN_YEAR_Z <- 4
#' @noRd
TS_SCREEN_YEAR_MIN_DEV <- c(offset = 1.5, amp_ratio = 0.15, lag_h = 1.5)
#' Fewest years on each side of a changepoint, and fewest screenable years to
#' look for one
#' @noRd
TS_SCREEN_CHANGE_MIN_SIDE <- 2
#' @noRd
TS_SCREEN_CHANGE_MIN_YEARS <- 6

#' Shortest stuck run (hours) reported as a bad period, and the gap (hours)
#' within which runs merge into one window
#' @noRd
TS_SCREEN_STUCK_PERIOD_H <- 24
#' @noRd
TS_SCREEN_STUCK_MERGE_H <- 24

#' Peak hour of a mean diurnal cycle
#'
#' From the phase of its first harmonic, which, unlike the hour of the maximum,
#' varies continuously.
#'
#' @param value Mean value at each time of day.
#' @param hour Time of day (hours, 0-24) of each value.
#' @return The peak hour in [0, 24), or NA with values at fewer than 12 times.
#' @noRd
ts_peak_hour <- function(value, hour) {
  ok <- !is.na(value)
  if (sum(ok) < 12) return(NA_real_)
  th <- 2 * pi * hour[ok] / 24
  (atan2(sum(value[ok] * sin(th)), sum(value[ok] * cos(th))) / (2 * pi) * 24) %% 24
}

#' Wrap a lag (hours) into [-12, 12)
#'
#' @param x Lag in hours.
#' @return `x` wrapped into [-12, 12).
#' @noRd
ts_wrap_lag <- function(x) ((x + 12) %% 24) - 12

#' Runs of repeated values
#'
#' Runs of exactly repeated consecutive values (missing values break runs),
#' except near 0 deg C, where a constant reading is the zero curtain under
#' snow, not a stuck logger.
#'
#' @param x Soil temperature (deg C), in time order.
#' @param zero_band Half-width (deg C) of the band around 0 deg C whose runs are
#'   exempt.
#' @return Data frame of runs: first and last index, length (records).
#' @noRd
ts_runs <- function(x, zero_band = 0.5) {
  if (!length(x)) return(data.frame(first = integer(), last = integer(), length = integer()))
  same <- c(FALSE, !is.na(x[-1]) & !is.na(x[-length(x)]) & x[-1] == x[-length(x)])
  starts <- which(!same)
  ends <- c(starts[-1] - 1L, length(x))
  runs <- data.frame(first = starts, last = ends, length = ends - starts + 1L)
  runs[!is.na(x[runs$first]) & abs(x[runs$first]) >= zero_band, ]
}

#' Diurnal diagnostics of a series against air temperature
#'
#' Median daily range of the series and of air temperature over a common set
#' of (nearly) complete days, their ratio, the lag of the series' diurnal peak
#' behind air's (unwrapped with the lag the damping implies, as phase is known
#' only modulo 24 h), and the depths (in damping depths) implied by the range
#' ratio and by the lag.
#'
#' @param day Day key of each record (records of one day share it).
#' @param hour Time of day (hours) of each record.
#' @param ts Soil temperature (deg C).
#' @param ta Air temperature (deg C).
#' @param min_obs Fewest values a day needs for its range to count.
#' @return One-row data frame: amp_ts, amp_ta, amp_ratio, n_amp_days, lag_h,
#'   zd_amp, zd_lag.
#' @noRd
ts_diurnal_diagnostics <- function(day, hour, ts, ta, min_obs) {
  rng <- function(v) if (sum(!is.na(v)) >= min_obs) diff(range(v, na.rm = TRUE)) else NA_real_
  amp_ts <- tapply(ts, day, rng)
  amp_ta <- tapply(ta, day, rng)
  both <- is.finite(amp_ts) & is.finite(amp_ta)
  out <- data.frame(amp_ts = NA_real_, amp_ta = NA_real_, amp_ratio = NA_real_, n_amp_days = sum(both),
                    lag_h = NA_real_, zd_amp = NA_real_, zd_lag = NA_real_)
  if (sum(both) < 5) return(out)
  out$amp_ts <- stats::median(amp_ts[both])
  out$amp_ta <- stats::median(amp_ta[both])
  out$amp_ratio <- out$amp_ts / out$amp_ta
  hours <- sort(unique(hour))
  peak_ts <- ts_peak_hour(as.numeric(tapply(ts, factor(hour, hours), mean, na.rm = TRUE)), hours)
  peak_ta <- ts_peak_hour(as.numeric(tapply(ta, factor(hour, hours), mean, na.rm = TRUE)), hours)
  lag <- ts_wrap_lag(peak_ts - peak_ta)
  zd_amp <- if (is.finite(out$amp_ratio) && out$amp_ratio > 0) -log(out$amp_ratio) else NA_real_
  if (is.finite(zd_amp) && is.finite(lag)) {
    expected <- zd_amp * 24 / (2 * pi)
    lag <- lag + 24 * round((expected - lag) / 24)
  }
  out$lag_h <- lag
  out$zd_amp <- zd_amp
  out$zd_lag <- lag * 2 * pi / 24
  out
}

#' The strongest changepoint in a stretch of a site's yearly diagnostics
#'
#' For each metric (offset, range ratio, lag), every split of the years into an
#' earlier and a later segment (at least TS_SCREEN_CHANGE_MIN_SIDE years each)
#' is scored by the difference of the segments' medians over the pooled
#' within-segment robust spread; a split counts if that difference exceeds the
#' metric's TS_SCREEN_YEAR_MIN_DEV and its score TS_SCREEN_YEAR_Z.
#'
#' @param yearly Yearly diagnostics (growing_year, offset, amp_ratio, lag_h),
#'   screenable years only, in year order.
#' @return NULL, or a one-row data frame: first year of the later segment, the
#'   metric, the earlier and the later segment's medians, and the score.
#' @noRd
ts_changepoint <- function(yearly) {
  if (is.null(yearly) || !nrow(yearly)) return(NULL)
  n <- nrow(yearly)
  if (n < TS_SCREEN_CHANGE_MIN_YEARS) return(NULL)
  best <- NULL
  for (metric in names(TS_SCREEN_YEAR_MIN_DEV)) {
    v <- yearly[[metric]]
    for (k in seq(TS_SCREEN_CHANGE_MIN_SIDE, n - TS_SCREEN_CHANGE_MIN_SIDE)) {
      a <- v[1:k]
      b <- v[(k + 1):n]
      d <- stats::median(b) - stats::median(a)
      spread <- 1.4826 * stats::median(abs(c(a - stats::median(a), b - stats::median(b))))
      score <- abs(d) / max(spread, 1e-9)
      if (abs(d) > TS_SCREEN_YEAR_MIN_DEV[[metric]] && score > TS_SCREEN_YEAR_Z && (is.null(best) || score > best$score)) {
        best <- data.frame(year = yearly$growing_year[k + 1], metric = metric, before = stats::median(a),
                           after = stats::median(b), score = score)
      }
    }
  }
  best
}

#' All changepoints in a site's yearly diagnostics
#'
#' Binary segmentation: the strongest changepoint (ts_changepoint()), then the
#' strongest in each of the two segments it leaves, and so on.
#'
#' @param yearly Yearly diagnostics, screenable years only, in year order.
#' @return Data frame of changepoints (as ts_changepoint()), in year order.
#' @noRd
ts_changepoints <- function(yearly) {
  found <- ts_changepoint(yearly)
  if (is.null(found)) return(NULL)
  rbind(ts_changepoints(yearly[yearly$growing_year < found$year, ]), found,
        ts_changepoints(yearly[yearly$growing_year >= found$year, ]))
}

#' Screen one soil-temperature series
#'
#' @param base Step 01's record (step01$ts_base): TIMESTAMP, YEAR, DOY, HOUR,
#'   MINUTE, TA, in time order.
#' @param ts The series (deg C), one value per row of `base` (NA where missing or
#'   unusable).
#' @param gStart,gEnd The site's growing season (day of year, as step 01 counts
#'   it).
#' @param dt_secs Time step of the record (seconds).
#' @return list(summary, yearly, windows, diurnal) (see the file header), without
#'   site and column identifiers.
#' @noRd
ts_screen_series <- function(base, ts, gStart, gEnd, dt_secs) {
  stopifnot(length(ts) == nrow(base), !is.unsorted(base$TIMESTAMP))
  steps_per_day <- 86400 / dt_secs
  min_obs <- max(4, floor(0.8 * steps_per_day))
  hour <- base$HOUR + base$MINUTE / 60
  growing_year <- ifelse(base$DOY <= 366, base$YEAR, base$YEAR - 1)
  day <- paste(growing_year, base$DOY)
  season <- dplyr::between(base$DOY, gStart, gEnd)
  has <- !is.na(ts)

  # whole record: coverage, offset, stuck runs, spikes; growing season: diurnal
  runs <- ts_runs(ts)
  stuck_runs <- runs[runs$length >= 6, ]
  diag <- cbind(
    data.frame(n = sum(has), coverage = mean(has), coverage_season = if (any(season)) mean(has[season]) else NA_real_,
               mean_offset = if (any(has & season)) mean(ts[season] - base$TA[season], na.rm = TRUE) else NA_real_,
               stuck_frac = if (any(has)) sum(stuck_runs$length) / sum(has) else NA_real_,
               stuck_max_h = if (nrow(runs)) max(runs$length) * dt_secs / 3600 else 0,
               spike_rate = mean(abs(diff(ts)) > 2 * dt_secs / 3600, na.rm = TRUE)),
    ts_diurnal_diagnostics(day[season], hour[season], ts[season], base$TA[season], min_obs)
  )

  # each growing year
  years <- sort(unique(growing_year[has]))
  yearly <- do.call(rbind, lapply(years, function(y) {
    rows <- growing_year == y & season
    cbind(data.frame(growing_year = y, coverage_season = mean(has[rows]),
                     offset = mean(ts[rows] - base$TA[rows], na.rm = TRUE)),
          ts_diurnal_diagnostics(day[rows], hour[rows], ts[rows], base$TA[rows], min_obs)[, c("amp_ratio", "lag_h", "n_amp_days")])
  }))
  if (is.null(yearly)) yearly <- data.frame(growing_year = integer(), coverage_season = double(), offset = double(),
                                            amp_ratio = double(), lag_h = double(), n_amp_days = integer())
  yearly$outlier <- rep("", nrow(yearly))
  yearly$segment <- rep(1L, nrow(yearly))
  screenable <- is.finite(yearly$offset) & is.finite(yearly$amp_ratio) & is.finite(yearly$lag_h) & yearly$n_amp_days >= 20
  changes <- ts_changepoints(yearly[screenable, ])
  for (y in changes$year) yearly$segment[yearly$growing_year >= y] <- yearly$segment[yearly$growing_year >= y] + 1L
  # single odd years, within their segment
  reference <- list()
  for (seg in unique(yearly$segment)) {
    in_seg <- screenable & yearly$segment == seg
    if (sum(in_seg) < 3) next
    for (metric in names(TS_SCREEN_YEAR_MIN_DEV)) {
      v <- yearly[[metric]]
      med <- stats::median(v[in_seg])
      s <- 1.4826 * stats::mad(v[in_seg])
      dev <- abs(v - med)
      out <- in_seg & dev > TS_SCREEN_YEAR_MIN_DEV[[metric]] & (s == 0 | dev / max(s, 1e-9) > TS_SCREEN_YEAR_Z)
      yearly$outlier[out] <- paste0(yearly$outlier[out], ifelse(nzchar(yearly$outlier[out]), ",", ""), "shift_", metric)
      reference[[paste(seg, metric)]] <- med
    }
  }

  # bad windows: runs of outlier years, and long stuck periods
  windows <- data.frame(start = as.POSIXct(character()), end = as.POSIXct(character()), scale = character(),
                        rule = character(), value = double(), reference = double())
  if (!is.null(changes)) {
    # everything before the latest segment; value and reference: the last
    # change's earlier and later medians
    last <- changes[nrow(changes), ]
    rows <- growing_year < last$year
    windows <- rbind(windows, data.frame(
      start = min(base$TIMESTAMP[rows]), end = max(base$TIMESTAMP[rows]), scale = "changepoint",
      rule = paste(unique(paste0("changepoint_", changes$metric)), collapse = ","), value = last$before, reference = last$after))
  }
  flagged <- yearly[nzchar(yearly$outlier), ]
  if (nrow(flagged)) {
    group <- cumsum(c(1, diff(flagged$growing_year) != 1))
    for (g in unique(group)) {
      yy <- flagged[group == g, ]
      rows <- growing_year %in% yy$growing_year
      rule <- unique(unlist(strsplit(yy$outlier, ",")))
      metric <- sub("^shift_", "", rule[1])
      windows <- rbind(windows, data.frame(
        start = min(base$TIMESTAMP[rows]), end = max(base$TIMESTAMP[rows]), scale = "year",
        rule = paste(rule, collapse = ","), value = stats::median(yy[[metric]], na.rm = TRUE),
        reference = reference[[paste(yy$segment[1], metric)]]))
    }
  }
  long <- runs[runs$length * dt_secs / 3600 >= TS_SCREEN_STUCK_PERIOD_H, ]
  if (nrow(long)) {
    group <- cumsum(c(1, (long$first[-1] - long$last[-nrow(long)]) * dt_secs / 3600 > TS_SCREEN_STUCK_MERGE_H))
    for (g in unique(group)) {
      r <- long[group == g, ]
      windows <- rbind(windows, data.frame(
        start = base$TIMESTAMP[min(r$first)], end = base$TIMESTAMP[max(r$last)], scale = "period", rule = "stuck",
        value = sum(r$length) * dt_secs / 3600, reference = TS_SCREEN_STUCK_PERIOD_H))
    }
  }
  windows <- windows[order(windows$start), ]

  # verdict: BAD if a calibrated whole-record rule fires; SUSPECT if only the
  # heat-equation check or part of the record (windows) is bad
  bad <- c(
    airlike = isTRUE(diag$amp_ratio > TS_SCREEN_AIRLIKE_AMP & abs(diag$lag_h) < TS_SCREEN_AIRLIKE_LAG),
    flat = isTRUE(diag$amp_ratio < TS_SCREEN_FLAT_AMP),
    stuck = isTRUE(diag$stuck_frac > TS_SCREEN_STUCK_FRAC),
    leads = isTRUE(diag$lag_h < TS_SCREEN_LEADS_H),
    coverage = isTRUE(diag$coverage < TS_SCREEN_COVERAGE_MIN)
  )
  ratio <- diag$zd_lag / diag$zd_amp
  suspect <- c(
    # damped but not delayed (e.g. a regression on air temperature), or the
    # delay and the damping imply very different depths
    nonconductive = isTRUE(diag$zd_amp > 0.05 & (diag$zd_lag <= 0 | ratio > TS_SCREEN_DEPTH_RATIO | ratio < 1 / TS_SCREEN_DEPTH_RATIO)),
    changepoint = any(windows$scale == "changepoint"),
    bad_years = any(windows$scale == "year"),
    stuck_periods = any(windows$scale == "period")
  )
  if (!any(has)) bad["coverage"] <- TRUE
  diag$verdict <- if (any(bad)) "BAD" else if (any(suspect)) "SUSPECT" else "GOOD"
  diag$flags <- paste(c(names(bad)[bad], names(suspect)[suspect]), collapse = ",")
  diag$bad_fraction <- if (any(has)) mean(ts_screen_mask(base$TIMESTAMP, windows)[has] == FALSE) else NA_real_

  hours <- sort(unique(hour[season]))
  diurnal <- data.frame(hour = hours,
                        ts = as.numeric(tapply(ts[season], factor(hour[season], hours), mean, na.rm = TRUE)),
                        ta = as.numeric(tapply(base$TA[season], factor(hour[season], hours), mean, na.rm = TRUE)))
  list(summary = diag, yearly = yearly, windows = windows, diurnal = diurnal)
}

#' Records outside bad windows
#'
#' The reconstruction-facing form of the screen: which records to keep.
#'
#' @param timestamp Record time stamps (POSIXct).
#' @param windows Bad windows (start, end; inclusive), e.g. ts_screen_site()'s
#'   windows for one site and column.
#' @return Logical, one per `timestamp`: TRUE outside every window.
#' @noRd
ts_screen_mask <- function(timestamp, windows) {
  keep <- rep(TRUE, length(timestamp))
  for (i in seq_len(nrow(windows))) keep[timestamp >= windows$start[i] & timestamp <= windows$end[i]] <- FALSE
  keep
}

#' Screen a site's soil-temperature series
#'
#' The sensor as recorded (TS_original, where usable as step 01 would use
#' it), its real readings (TS_real: measured, not gap-filled), step 01's
#' column (TS_reconstructed) and the manuscript's (TS_manuscript).
#'
#' @param step01 The site's step-01 result (prepare_site()).
#' @param sensor read_ts_sensor()'s table.
#' @param settings The site's row of data/site_settings.csv.
#' @param manuscript_inputs The site's tas_inputs (for TS_manuscript).
#' @return list(summary, yearly, windows, diurnal), data frames with site_ID and
#'   column first.
#' @noRd
ts_screen_site <- function(step01, sensor, settings, manuscript_inputs) {
  base <- step01$ts_base
  params <- step01$ts_params
  s <- sensor[match(base$TIMESTAMP, sensor$TIMESTAMP), ]
  usable <- if (params$source == "fluxnet") s$TS_sensor_QC %in% c(0, 1, 2) else rep(TRUE, nrow(base))
  truth <- ts_truth(s, base$YEAR, NA)
  recon <- step01$climatology$TS
  series <- list(
    TS_original = ifelse(usable, s$TS_sensor, NA_real_),
    TS_real = ifelse(truth, s$TS_real, NA_real_),
    TS_reconstructed = ifelse(step01$ts_ok, recon, NA_real_),
    TS_manuscript = ifelse(step01$ts_ok, ts_manuscript_values(step01, settings, manuscript_inputs), NA_real_)
  )
  results <- lapply(series, ts_screen_series, base = base, gStart = step01$feature$gStart, gEnd = step01$feature$gEnd,
                    dt_secs = params$dt_secs)
  # how much of the sensor each column replaces (share of the sensor's usable
  # records where the column differs from it by more than 0.01 deg C)
  for (column in names(series)) {
    both <- !is.na(series$TS_original) & !is.na(series[[column]])
    results[[column]]$summary$replaced_share <- if (any(!is.na(series$TS_original))) {
      mean(!both[!is.na(series$TS_original)] | abs(series[[column]] - series$TS_original)[!is.na(series$TS_original)] > 0.01)
    } else NA_real_
  }
  stack <- function(part) {
    dplyr::bind_rows(lapply(names(results), function(column) {
      x <- results[[column]][[part]]
      if (!nrow(x)) return(NULL)
      cbind(data.frame(site_ID = params$site_ID, column = column), x)
    }))
  }
  list(summary = stack("summary"), yearly = stack("yearly"), windows = stack("windows"), diurnal = stack("diurnal"))
}

#' Combine sites' screens
#'
#' @param sites List of ts_screen_site() results.
#' @return list(summary, yearly, windows, diurnal), each bound across sites.
#' @noRd
combine_ts_screen <- function(sites) {
  parts <- c("summary", "yearly", "windows", "diurnal")
  stats::setNames(lapply(parts, function(p) dplyr::bind_rows(lapply(sites, `[[`, p))), parts)
}

#' Write the screen's tables
#'
#' @param screen combine_ts_screen()'s result.
#' @param dir Output directory.
#' @param validation ts_screen_validation()'s table, or NULL.
#' @return Paths of ts_screen.csv, ts_screen_yearly.csv, ts_screen_windows.csv
#'   and ts_screen_diurnal.csv (time stamps as "YYYY-MM-DD HH:MM:SS", the
#'   record's own clock), and ts_screen_validation.csv if given.
#' @noRd
write_ts_screen <- function(screen, dir = Sys.getenv("THERMAL_DATA_PROC", "data-proc"), validation = NULL) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(dir, paste0("ts_screen", c("", "_yearly", "_windows", "_diurnal"), ".csv"))
  if (!is.null(validation)) {
    paths <- c(paths, file.path(dir, "ts_screen_validation.csv"))
    utils::write.csv(validation, paths[5], row.names = FALSE)
  }
  windows <- screen$windows
  for (v in c("start", "end")) windows[[v]] <- format(windows[[v]], "%Y-%m-%d %H:%M:%S")
  utils::write.csv(screen$summary, paths[1], row.names = FALSE)
  utils::write.csv(screen$yearly, paths[2], row.names = FALSE)
  utils::write.csv(windows, paths[3], row.names = FALSE)
  utils::write.csv(screen$diurnal, paths[4], row.names = FALSE)
  paths
}

#' The screen beside the manuscript's own soil-temperature corrections
#'
#' One row per site: the screen's verdict, flags and windows for the sensor
#' (its real readings where there are any, else the sensor as recorded), and
#' what the manuscript did: the share of the sensor its step-01 column and its
#' fits' column replace, its regression on air temperature, its sensor
#' first-year cut-off and its removed years. The manuscript's choices are a
#' guideline, not a target: automated checks and hand-made calls can
#' legitimately disagree.
#'
#' @param screen combine_ts_screen()'s result.
#' @param site_info data/site_info.csv.
#' @param settings data/site_settings.csv.
#' @return Data frame, one row per site.
#' @noRd
ts_screen_validation <- function(screen, site_info, settings) {
  sm <- screen$summary
  sensor <- do.call(rbind, lapply(split(sm, sm$site_ID), function(d) {
    real <- d[d$column == "TS_real", ]
    if (nrow(real) && is.finite(real$amp_ratio)) real else d[d$column == "TS_original", ]
  }))
  w <- screen$windows
  windows <- vapply(sensor$site_ID, function(s) {
    x <- w[w$site_ID == s & w$column == sensor$column[sensor$site_ID == s], ]
    if (!nrow(x)) return("")
    paste0(format(x$start, "%Y"), "-", format(x$end, "%Y"), " ", x$rule, collapse = "; ")
  }, "")
  replaced <- function(column) sm$replaced_share[match(paste(sensor$site_ID, column), paste(sm$site_ID, sm$column))]
  out <- data.frame(
    site_ID = sensor$site_ID, screened = sensor$column, verdict = sensor$verdict, flags = sensor$flags,
    bad_fraction = sensor$bad_fraction, windows = unname(windows),
    step01_replaced = replaced("TS_reconstructed"), manuscript_replaced = replaced("TS_manuscript")
  )
  out$ts_regression <- settings$ts_regression[match(out$site_ID, settings$site_ID)]
  out$sensor_first_year <- settings$ts_sensor_first_year[match(out$site_ID, settings$site_ID)]
  out$year_removed <- site_info$year_removed[match(out$site_ID, site_info$site_ID)]
  out$manuscript_acted <- (out$manuscript_replaced > 0.05) %in% TRUE | !is.na(out$sensor_first_year) |
    (!is.na(out$year_removed) & nzchar(out$year_removed))
  out[order(match(out$site_ID, site_info$site_ID)), ]
}

