# Soil-temperature quality diagnostics, and the verdict built from them.
#
# Shallow soil temperature is a damped, phase-lagged version of the surface
# forcing. For a sinusoidal forcing, amplitude decays as exp(-z/d) and phase
# lags by z/d radians, d being the damping depth. That structure is what these
# diagnostics test for, and it is why they need no metadata: a sensor lying on
# the surface, pulled out of the ground, or reporting air temperature under
# another name has an amplitude ratio near 1 and a lag near 0, whatever its
# column is called. The full derivation and the calibration over all 117 sites
# is in ts-rework.html (findings F5, F9, F11).
#
# Two of the three structural rules first proposed were falsified by the data
# and are *not* here: the amplitude-vs-lag consistency test (the two estimates
# of z/d disagree systematically because air temperature is not the soil
# surface) and the cross-depth coherence test (needs several columns). Both
# survive as *relative*, network-calibrated checks in scripts/ts-qc-validate.R.
# The verdict below uses only the rules that fired with zero false positives
# against the project's hand-made calls, plus the air-like rule whose three
# "false positives" were judged to be detections.

# Thresholds. First pass, calibrated only as far as ts-rework.html describes.
TS_QC_AIRLIKE_AMP <- 0.85    # buried soil damps; this much amplitude is not soil
TS_QC_AIRLIKE_LAG <- 0.75    # ... and it should lag, in hours
TS_QC_FLAT_AMP <- 0.03       # no diurnal signal at all
TS_QC_STUCK_FRAC <- 0.10     # a tenth of the record in repeated-value runs
TS_QC_LEADS_H <- -0.75       # soil leading air is unphysical
TS_QC_COVERAGE_MIN <- 0.40

# Peak hour of a 24-point hour-of-day climatology, from the first harmonic
# rather than `which.max`: the argmax of a noisy 24-point profile jumps by
# whole hours, while the harmonic phase is continuous and is the quantity the
# damping-depth relation is expressed in.
peak_hour <- function(hourly_mean, hours) {
  ok <- !is.na(hourly_mean)
  if (sum(ok) < 12) return(NA_real_)
  th <- 2 * pi * hours[ok] / 24
  (atan2(sum(hourly_mean[ok] * sin(th)), sum(hourly_mean[ok] * cos(th))) / (2 * pi) * 24) %% 24
}

wrap_lag <- function(x) ((x + 12) %% 24) - 12

# Median daily amplitude (max - min within a calendar day) for several columns
# at once, on a *common* set of sufficiently-complete days.
#
# The common day set is the point. Computed independently, each column gets
# its own set of complete days -- a reconstruction is typically non-missing on
# strictly more days than its own predictors are -- and the ratio then mixes
# the damping with which days each column happened to cover.
daily_amplitudes <- function(dat, cols, min_obs) {
  amp1 <- function(x) if (sum(!is.na(x)) >= min_obs) diff(range(x, na.rm = TRUE)) else NA_real_
  per_day <- dat |>
    dplyr::summarise(dplyr::across(dplyr::all_of(cols), amp1), .by = c("YEAR", "DOY")) |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(cols), is.finite))
  out <- vapply(cols, function(cl) stats::median(per_day[[cl]], na.rm = TRUE), 0.0)
  c(out, n_days = nrow(per_day))
}

# Longest run of exactly-repeated consecutive values, and the fraction of
# observations sitting in a run of at least `min_run`.
#
# The near-zero exemption is not optional. A shallow sensor pinned at 0 C for
# weeks under snow is the zero curtain -- latent heat of fusion buffering the
# soil at the freezing point -- and is the most physically real signal in the
# record, not a stuck logger. Flagging it would condemn every seasonally
# snow-covered site in the network.
stuck_stats <- function(x, min_run = 6, zero_band = 0.5) {
  ok <- !is.na(x)
  if (sum(ok) < 10) return(c(max_run = NA_real_, frac = NA_real_))
  v <- x[ok]
  r <- rle(v)
  lens <- r$lengths
  lens[abs(r$values) < zero_band] <- 1L
  c(max_run = max(lens), frac = sum(lens[lens >= min_run]) / length(v))
}

# The diagnostics for one soil-temperature series against one air-temperature
# series. `dat` needs YEAR, DOY, HOUR; `ts` and `ta` are vectors aligned to
# its rows. Returns one row.
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

  # Phase is only known modulo 24 h. The amplitude gives an independent
  # estimate of z/d and hence of the lag, which picks the branch: a strongly
  # damped sensor lagging 16 h reads as -8 h wrapped, and "soil leads air" is
  # the wrong conclusion to draw from that.
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

# The verdict: BAD if any hard rule fires, GOOD otherwise, with the rules that
# fired named in `flags` so the reason travels with the result. SUSPECT is
# reserved for the relative rules, which need the network and live in
# scripts/ts-qc-validate.R.
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

# Quality of the soil-temperature column a run will treat as measured, as
# step 01 leaves it. Computed on the step-01 table rather than the raw record
# because this verdict is about the column the model would actually fit on --
# after the site-specific column choices step 01 makes -- not about the
# provider's file. The raw-record screen, which is the one that generalises to
# a site the project has never seen, is scripts/ts-qc-screen.R.
ts_quality <- function(ac, ts_col = "TS_measured", ta_col = "TA") {
  dt_hours <- if (length(unique(ac$MINUTE)) > 1) 0.5 else 1
  ts_diagnostics(ac, ac[[ts_col]], ac[[ta_col]], dt_hours = dt_hours) |>
    ts_verdict_from() |>
    dplyr::mutate(ts_col = ts_col, .before = 1)
}
