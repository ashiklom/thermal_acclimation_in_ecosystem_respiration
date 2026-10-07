# Soil-temperature quality diagnostics, and the verdict built from them.
#
# Buried soil temperature is damped (amplitude ~ exp(-z/d)) and lagged
# (z/d radians) relative to air, so a sensor on the surface or reporting air
# temperature has an amplitude ratio near 1 and a lag near 0, whatever its
# column is called. Derivation and calibration over 117 sites:
# docs/ts-rework.html (F5, F9, F11). The verdict uses only the rules with no
# false positives against the hand-made calls; the relative, network-level
# checks live in scripts/ts-qc-validate.R.

#' Amplitude ratio above which a series is air-like
#'
#' Thresholds (first pass; see docs/ts-rework.html).
#'
#' Buried soil damps; this much amplitude is not soil.
TS_QC_AIRLIKE_AMP <- 0.85

#' Lag below which an air-like amplitude ratio is flagged
#'
#' Buried soil should also lag air; in hours.
TS_QC_AIRLIKE_LAG <- 0.75

#' Amplitude ratio below which a series is flat
#'
#' No diurnal signal at all.
TS_QC_FLAT_AMP <- 0.03

#' Fraction of the record in stuck runs above which a series is flagged
#'
#' A tenth of the record in repeated-value runs.
TS_QC_STUCK_FRAC <- 0.10

#' Lag, in hours, below which soil leads air
#'
#' Soil leading air is unphysical.
TS_QC_LEADS_H <- -0.75

#' Minimum fraction of rows with a soil-temperature value
TS_QC_COVERAGE_MIN <- 0.40

#' Peak hour of a 24-point hour-of-day climatology
#'
#' From the first harmonic's phase (continuous, unlike a noisy `which.max`).
#'
#' @param hourly_mean Mean value for each hour of the day.
#' @param hours Hour of the day (0--23) of each value of `hourly_mean`.
#' @return The peak hour, in [0, 24); `NA_real_` if fewer than 12 hours have a
#'   value.
peak_hour <- function(hourly_mean, hours) {
  ok <- !is.na(hourly_mean)
  if (sum(ok) < 12) return(NA_real_)
  th <- 2 * pi * hours[ok] / 24
  (atan2(sum(hourly_mean[ok] * sin(th)), sum(hourly_mean[ok] * cos(th))) / (2 * pi) * 24) %% 24
}

#' Wrap a lag in hours into [-12, 12)
#'
#' @param x Lag, in hours.
#' @return `x` wrapped into [-12, 12).
wrap_lag <- function(x) ((x + 12) %% 24) - 12

#' Median daily amplitude for several columns
#'
#' On a *common* set of complete days, so a ratio compares like with like.
#'
#' @param dat Data frame with `YEAR`, `DOY` and the columns in `cols`.
#' @param cols Names of the columns to take amplitudes of.
#' @param min_obs Fewest non-missing values a day needs for its amplitude to
#'   count.
#' @return Named numeric vector: the median daily range of each of `cols`, plus
#'   `n_days`, the number of common days.
daily_amplitudes <- function(dat, cols, min_obs) {
  amp1 <- function(x) if (sum(!is.na(x)) >= min_obs) diff(range(x, na.rm = TRUE)) else NA_real_
  per_day <- dat |>
    dplyr::summarise(dplyr::across(dplyr::all_of(cols), amp1), .by = c("YEAR", "DOY")) |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(cols), is.finite))
  out <- vapply(cols, function(cl) stats::median(per_day[[cl]], na.rm = TRUE), 0.0)
  c(out, n_days = nrow(per_day))
}

#' Stuck-logger statistics for a series
#'
#' Longest run of exactly-repeated consecutive values, and the fraction of
#' observations sitting in a run of at least `min_run`.
#'
#' Runs near 0 C are exempt: that is the zero curtain under snow, not a stuck
#' logger.
#'
#' @param x Soil temperature, in degrees C.
#' @param min_run Shortest run, in observations, that counts as stuck.
#' @param zero_band Half-width, in degrees C, of the band around 0 C whose runs
#'   are exempt.
#' @return Named numeric vector of `max_run` and `frac`; both NA if `x` has
#'   fewer than 10 non-missing values.
stuck_stats <- function(x, min_run = 6, zero_band = 0.5) {
  ok <- !is.na(x)
  if (sum(ok) < 10) return(c(max_run = NA_real_, frac = NA_real_))
  v <- x[ok]
  r <- rle(v)
  lens <- r$lengths
  lens[abs(r$values) < zero_band] <- 1L
  c(max_run = max(lens), frac = sum(lens[lens >= min_run]) / length(v))
}

#' Soil-temperature quality diagnostics for one series
#'
#' The diagnostics for one soil-temperature series against one air-temperature
#' series. `dat` needs YEAR, DOY, HOUR; `ts` and `ta` are vectors aligned to
#' its rows.
#'
#' @param dat Data frame giving the time of each row.
#' @param ts Soil temperature, in degrees C.
#' @param ta Air temperature, in degrees C.
#' @param dt_hours The time step, in hours.
#' @return One row: a tibble of coverage, mean offset from air, amplitudes and
#'   their ratio, lag (hours), stuck-run and spike statistics.
ts_diagnostics <- function(dat, ts, ta, dt_hours = 0.5) {
  stopifnot(length(ts) == nrow(dat), length(ta) == nrow(dat))
  min_obs_day <- max(4, floor(0.8 * 24 / dt_hours))
  d <- tibble::tibble(YEAR = dat$YEAR, DOY = dat$DOY, HOUR = dat$HOUR, .ts = ts, .ta = ta)

  amps <- daily_amplitudes(d, c(".ts", ".ta"), min_obs_day)
  amp_ratio <- unname(amps[[".ts"]] / amps[[".ta"]])

  hourly <- d |>
    dplyr::summarise(t = mean(.data$.ts, na.rm = TRUE), a = mean(.data$.ta, na.rm = TRUE),
                     .by = "HOUR") |>
    dplyr::arrange(.data$HOUR)
  lag_h <- wrap_lag(peak_hour(hourly$t, hourly$HOUR) - peak_hour(hourly$a, hourly$HOUR))

  # Phase is known only modulo 24 h; the amplitude's estimate of the lag picks
  # the branch (a 16 h lag otherwise reads as -8 h).
  zd_amp <- if (is.finite(amp_ratio) && amp_ratio > 0) -log(amp_ratio) else NA_real_
  lag_expected <- zd_amp * 24 / (2 * pi)
  lag_unwrapped <- if (is.finite(lag_expected)) {
    lag_h + 24 * round((lag_expected - lag_h) / 24)
  } else {
    lag_h
  }

  st <- stuck_stats(ts)

  tibble::tibble(
    n = sum(!is.na(ts)),
    coverage = sum(!is.na(ts)) / nrow(dat),
    mean_ts = mean(ts, na.rm = TRUE),
    mean_offset = mean(ts, na.rm = TRUE) - mean(ta, na.rm = TRUE),
    amp_ts = unname(amps[[".ts"]]),
    amp_ta = unname(amps[[".ta"]]),
    amp_ratio = amp_ratio,
    n_amp_days = unname(amps[["n_days"]]),
    lag_h = lag_h,
    lag_unwrapped = lag_unwrapped,
    zd_amp = zd_amp,
    stuck_max_run = unname(st[["max_run"]]),
    stuck_frac = unname(st[["frac"]]),
    spike_rate = mean(abs(diff(ts)) > 2 * dt_hours, na.rm = TRUE)
  )
}

#' The verdict from one row of diagnostics
#'
#' BAD if any rule fires, GOOD otherwise; `flags` names the rules that fired.
#'
#' @param diag One row from `ts_diagnostics()`.
#' @return `diag` with `verdict` (`"BAD"` or `"GOOD"`) and `flags` (a
#'   comma-separated string, empty if none fired) added.
ts_verdict_from <- function(diag) {
  flags <- c(
    airlike = isTRUE(diag$amp_ratio > TS_QC_AIRLIKE_AMP & abs(diag$lag_unwrapped) < TS_QC_AIRLIKE_LAG),
    flat = isTRUE(diag$amp_ratio < TS_QC_FLAT_AMP),
    stuck = isTRUE(diag$stuck_frac > TS_QC_STUCK_FRAC),
    leads = isTRUE(diag$lag_unwrapped < TS_QC_LEADS_H),
    coverage = isTRUE(diag$coverage < TS_QC_COVERAGE_MIN)
  )
  fired <- names(flags)[flags]
  diag$verdict <- if (length(fired)) "BAD" else "GOOD"
  diag$flags <- if (length(fired)) paste(fired, collapse = ",") else ""
  diag
}

#' The soil-temperature verdict for a site
#'
#' The verdict on the column step 01 leaves as measured -- the one a run would
#' fit on. The raw-record screen for unseen sites is scripts/ts-qc-screen.R.
#'
#' @param ac Data frame of the site's half-hourly data, with `YEAR`, `DOY`,
#'   `HOUR` and the two temperature columns.
#' @param dt_hours The time step, in hours.
#' @param ts_col Name of the soil-temperature column.
#' @param ta_col Name of the air-temperature column.
#' @return One row: `ts_col`, then the `ts_diagnostics()` columns, `verdict` and
#'   `flags`.
ts_quality <- function(ac, dt_hours, ts_col = "TS_measured", ta_col = "TA") {
  ts_diagnostics(ac, ac[[ts_col]], ac[[ta_col]], dt_hours = dt_hours) |>
    ts_verdict_from() |>
    dplyr::mutate(ts_col = ts_col, .before = 1)
}
