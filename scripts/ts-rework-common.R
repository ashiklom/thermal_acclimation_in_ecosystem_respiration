# Shared helpers for the soil-temperature rework scripts.
#
# Sourced by the `scripts/ts-*.R` analyses rather than living in `R/`, because
# nothing in the targets pipeline depends on it and putting it in `R/` would
# make every edit here invalidate the step-01 caches keyed on that directory.

suppressMessages({
  library(dplyr)
  library(targets)
})

# ---------------------------------------------------------------- step-01 cache
#
# `prep_nee_ac()` costs 30-140 s a site (REddyProc dominates at the AmeriFlux
# sites), and these analyses run over 60+ sites repeatedly. The cache is keyed
# on a digest of everything under `R/`, so any pipeline edit invalidates it
# rather than silently reporting stale numbers.
#
# Deliberately the same directory, file name and record layout as
# `tests/ts-swc-baseline.R` uses, so the two share hits instead of each paying
# for the same site.
STEP01_CACHE_DIR <- file.path("data-proc", "ts-baseline-cache")

r_code_digest <- function() {
  files <- sort(list.files("R", pattern = "[.]R$", full.names = TRUE))
  paste(vapply(files, function(f) as.character(tools::md5sum(f)), ""), collapse = "")
}

step01_cached <- function(name_site, code_key = r_code_digest()) {
  dir.create(STEP01_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(STEP01_CACHE_DIR, paste0(name_site, ".rds"))
  if (file.exists(path)) {
    hit <- readRDS(path)
    if (identical(hit$code_key, code_key)) return(hit$value)
  }
  value <- tryCatch(
    suppressWarnings(suppressMessages(prep_nee_ac(get_site_info(name_site)))),
    error = function(e) structure(conditionMessage(e), class = "baseline_error")
  )
  saveRDS(list(code_key = code_key, value = value), path)
  value
}

# ---------------------------------------------------------------- site sets

# Sites whose soil temperature is derived from air temperature inside
# `prep_ameriflux()` rather than through any of the declared mechanisms.
#
# These are invisible to `site_info.csv`: both carry `estimate_Ts = "NO"` and
# `ts_col = "TS_measured"`, and neither appears in `SITES_TS_FROM_TA_*`. The
# manipulation is a bare `name_site ==` branch in `R/ameriflux.R:245-250`.
#
# US-Cwt's entire column is `TA * 0.64718 + 5.13873`, coefficients borrowed
# from a nearby site of the same IGBP class; US-MBP's *gaps* are filled from
# `TA * 0.3688005 + 5.8670273`. Neither can serve as a truth against which a
# reconstruction from air temperature is scored -- the answer is one by
# construction, and US-Cwt duly returns exactly 1.000 on every ratio in
# `ts-linear-vs-measured.csv`, which is how the omission was noticed.
SITES_TS_SYNTHETIC_AMERIFLUX <- c("US-Cwt", "US-MBP")

# Sites whose soil temperature is genuinely measured: it is neither
# reconstructed in step 01 (`estimate_Ts`, `SITES_TS_FROM_TA_*`,
# `SITES_TS_SYNTHETIC_AMERIFLUX`) nor substituted in step 02 (`ts_col`).
# These are the only sites where a reconstruction can be scored against a
# truth, so they are the evaluation set for everything here.
#
# That this set has to be assembled from one CSV column, two constants in
# `R/constants.R` and two hard-coded branches in `R/ameriflux.R` is the
# argument for step 3 in miniature: "is this site's soil temperature real?"
# is not currently answerable from the data, only from five scattered
# declarations that no test relates to each other.
measured_ts_sites <- function(path = SITE_INFO_CSV) {
  si <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  si |>
    dplyr::filter(
      .data$ts_col == "TS_measured",
      .data$estimate_Ts == "NO",
      !.data$site_ID %in% c(
        SITES_TS_FROM_TA_RECENT, SITES_TS_FROM_TA_COLD,
        SITES_TS_SYNTHETIC_AMERIFLUX
      )
    ) |>
    dplyr::pull("site_ID")
}

# ---------------------------------------------------------------- CLI

# `--sites A,B,C` or `--sites all`; anything else falls back to `default`.
parse_sites_arg <- function(args, default) {
  i <- match("--sites", args)
  if (is.na(i) || i == length(args)) return(default)
  val <- args[[i + 1]]
  if (identical(val, "all")) return(default)
  strsplit(val, ",", fixed = TRUE)[[1]]
}

parse_flag <- function(args, flag) flag %in% args

parse_opt <- function(args, opt, default) {
  i <- match(opt, args)
  if (is.na(i) || i == length(args)) return(default)
  args[[i + 1]]
}

# ---------------------------------------------------------------- window layout
#
# The (growing_year x window) grid that `total_tas_site()` fits on, rebuilt
# here so that a soil-temperature column can be scored on the cells the model
# actually uses rather than on raw half-hours. Transcribed from
# `total_tas_site()` / `total_tas_window()`: `nwindow` windows of
# `WINDOW_SIZE` days starting at `gStart`, the last one clipped to `gEnd`.
tas_windows <- function(gStart, gEnd) {
  nwindow <- max(round((gEnd - gStart + 1) / WINDOW_SIZE), 1)
  tibble::tibble(
    iwindow = seq_len(nwindow),
    window_start = gStart + WINDOW_SIZE * (seq_len(nwindow) - 1),
    window_end = pmin(gStart + WINDOW_SIZE * seq_len(nwindow), gEnd)
  )
}

# Long table of (window, growing_year) cells with per-cell statistics of `col`.
window_cells <- function(dat, gStart, gEnd, col) {
  wins <- tas_windows(gStart, gEnd)
  dat <- dat |>
    dplyr::mutate(growing_year = dplyr::if_else(.data$DOY <= 366, .data$YEAR, .data$YEAR - 1L))
  purrr::map_dfr(seq_len(nrow(wins)), function(i) {
    w <- wins[i, ]
    dat |>
      dplyr::filter(dplyr::between(.data$DOY, w$window_start, w$window_end)) |>
      dplyr::summarise(
        n = sum(!is.na(.data[[col]])),
        cell_mean = mean(.data[[col]], na.rm = TRUE),
        cell_sd = stats::sd(.data[[col]], na.rm = TRUE),
        .by = "growing_year"
      ) |>
      dplyr::mutate(iwindow = w$iwindow, .before = 1)
  })
}

# ------------------------------------------------- scoring a reconstruction
#
# One scorer, used both for the `TS_linear` substitution the pipeline already
# performs and for every candidate gap-filling method, so that the existing
# method is one row of the same table rather than a separate story.
#
# The metrics are chosen for what `total_tas_site()` does with the column
# rather than for generic goodness of fit. It fits
# `NEE ~ exp(alpha*TS + beta*TS^2) * C0` inside each (14-day window x growing
# year) cell and then regresses log-respiration-ratio on the *cell-mean* TS
# across years, so the column enters on two independent axes:
#
#   within_sd_ratio          spread inside a cell; identifies `alpha`.
#                            Inflate it and the fitted sensitivity deflates.
#   across_year_spread_ratio spread of cell means across years within a
#                            window; this is the regressor TAS is the slope
#                            against. Compress it and |TAS| inflates
#                            mechanically, whatever the respiration says.
#
# A method can be excellent on RMSE and wrong on both.
#
# `gs` must already be restricted to the growing season. `truth` and `pred`
# are numeric vectors aligned to its rows.
ts_reconstruction_metrics <- function(gs, truth, pred, gStart, gEnd, min_obs_day = 40) {
  stopifnot(length(truth) == nrow(gs), length(pred) == nrow(gs))
  both <- !is.na(truth) & !is.na(pred)
  rmse <- function(a, b) sqrt(mean((a - b)^2, na.rm = TRUE))

  d <- gs
  d$.truth <- truth
  d$.pred <- pred

  amps <- daily_amplitudes(d, c(".truth", ".pred", "TA"), min_obs_day)
  hourly <- d |>
    dplyr::summarise(
      t = mean(.data$.truth, na.rm = TRUE),
      p = mean(.data$.pred, na.rm = TRUE),
      a = mean(.data$TA, na.rm = TRUE),
      .by = "HOUR"
    ) |>
    dplyr::arrange(.data$HOUR)
  ph_a <- peak_hour(hourly$a, hourly$HOUR)

  ct <- window_cells(d, gStart, gEnd, ".truth")
  cp <- window_cells(d, gStart, gEnd, ".pred")
  cells <- dplyr::inner_join(
    dplyr::select(ct, "iwindow", "growing_year", mean_t = "cell_mean", sd_t = "cell_sd", n_t = "n"),
    dplyr::select(cp, "iwindow", "growing_year", mean_p = "cell_mean", sd_p = "cell_sd"),
    by = c("iwindow", "growing_year")
  ) |>
    dplyr::filter(.data$n_t > 0, is.finite(.data$mean_t), is.finite(.data$mean_p))

  spread <- cells |>
    dplyr::summarise(
      nyear = dplyr::n(),
      sp_t = stats::sd(.data$mean_t),
      sp_p = stats::sd(.data$mean_p),
      .by = "iwindow"
    ) |>
    dplyr::filter(.data$nyear >= 3, .data$sp_t > 0)

  qt <- stats::quantile(truth[both], c(0.025, 0.975))
  qp <- stats::quantile(pred[both], c(0.025, 0.975))

  tibble::tibble(
    n = sum(both),
    coverage = sum(both) / sum(!is.na(truth)),
    rmse = rmse(pred[both], truth[both]),
    bias = mean(pred[both] - truth[both]),
    mae = mean(abs(pred[both] - truth[both])),
    r2 = suppressWarnings(stats::cor(pred[both], truth[both])^2),
    sd_truth = stats::sd(truth[both]),
    sd_pred = stats::sd(pred[both]),
    sd_ratio = stats::sd(pred[both]) / stats::sd(truth[both]),
    amp_truth = unname(amps[[".truth"]]),
    amp_pred = unname(amps[[".pred"]]),
    amp_ta = unname(amps[["TA"]]),
    amp_ratio_truth = unname(amps[[".truth"]] / amps[["TA"]]),
    amp_ratio_pred = unname(amps[[".pred"]] / amps[["TA"]]),
    amp_inflation = unname(amps[[".pred"]] / amps[[".truth"]]),
    n_amp_days = unname(amps[["n_days"]]),
    lag_truth_h = wrap_lag(peak_hour(hourly$t, hourly$HOUR) - ph_a),
    lag_pred_h = wrap_lag(peak_hour(hourly$p, hourly$HOUR) - ph_a),
    within_sd_ratio = stats::median(cells$sd_p / cells$sd_t, na.rm = TRUE),
    across_year_spread_ratio = stats::median(spread$sp_p / spread$sp_t, na.rm = TRUE),
    across_year_spread_truth = if (nrow(spread)) stats::median(spread$sp_t) else NA_real_,
    across_year_spread_pred = if (nrow(spread)) stats::median(spread$sp_p) else NA_real_,
    n_spread_windows = nrow(spread),
    cell_mean_rmse = rmse(cells$mean_p, cells$mean_t),
    cell_mean_bias = mean(cells$mean_p - cells$mean_t),
    n_cells = nrow(cells),
    q025_truth = unname(qt[[1]]), q975_truth = unname(qt[[2]]),
    q025_pred = unname(qp[[1]]), q975_pred = unname(qp[[2]]),
    d_q025 = unname(qp[[1]] - qt[[1]]), d_q975 = unname(qp[[2]] - qt[[2]])
  )
}

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
