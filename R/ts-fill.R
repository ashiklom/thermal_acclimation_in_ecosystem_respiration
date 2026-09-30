# Soil-temperature reconstruction: candidate methods, the blocked
# cross-validation that scores them, and `fill_soil_temp()`, the per-site
# target that picks one and produces `TS_memfill`. The memory methods won at
# 42 of 43 sites (docs/ts-rework.html, F12-F13).

# ------------------------------------------------------------------ features
#
# Shallow soil temperature is a damped, lagged integral of the surface
# forcing, which an instantaneous `TS ~ TA` map cannot represent. Running
# means of air temperature over 3, 7 and 30 days supply the memory. They are
# computed on a daily series completed over the whole date range, because
# half-hourly row offsets would span however many days the (gappy) rows
# happened to cover.
ts_fill_add_features <- function(dat) {
  need <- c("YEAR", "MONTH", "DAY", "DOY", "HOUR", "TA")
  absent <- setdiff(need, names(dat))
  if (length(absent)) stop("ts_fill_add_features needs ", paste(absent, collapse = ", "))

  dat$date <- as.Date(sprintf("%04d-%02d-%02d", dat$YEAR, dat$MONTH, dat$DAY))

  denan <- function(x) replace(x, is.nan(x), NA_real_)
  amp1 <- function(x) if (sum(!is.na(x)) >= 4) diff(range(x, na.rm = TRUE)) else NA_real_

  daily <- dat |>
    dplyr::summarise(
      TA_day = denan(mean(.data$TA, na.rm = TRUE)),
      TA_amp = amp1(.data$TA),
      .by = "date"
    ) |>
    dplyr::arrange(.data$date)

  full <- tibble::tibble(date = seq(min(daily$date), max(daily$date), by = "day"))
  daily <- dplyr::left_join(full, daily, by = "date")

  roll <- function(x, k) {
    denan(zoo::rollapply(
      x, width = k, FUN = function(v) mean(v, na.rm = TRUE),
      align = "right", fill = NA_real_, partial = TRUE
    ))
  }
  daily$TA_m3 <- roll(daily$TA_day, 3)
  daily$TA_m7 <- roll(daily$TA_day, 7)
  daily$TA_m30 <- roll(daily$TA_day, 30)
  daily$TA_lag1 <- dplyr::lag(daily$TA_day)
  daily$TA_amp_lag1 <- dplyr::lag(daily$TA_amp)

  dat <- dplyr::left_join(
    dat,
    dplyr::select(daily, "date", "TA_day", "TA_lag1", "TA_amp_lag1", "TA_m3", "TA_m7", "TA_m30"),
    by = "date"
  )

  # From the date, not from `DOY`, which at a wrapped site runs past 366 --
  # the seasonal harmonic has to be periodic in the calendar year. Identical
  # to `dat$DOY` wherever the site is not wrapped.
  th_d <- 2 * pi * lubridate::yday(dat$date) / 365.25
  th_h <- 2 * pi * dat$HOUR / 24
  dat$doy_s1 <- sin(th_d);     dat$doy_c1 <- cos(th_d)
  dat$doy_s2 <- sin(2 * th_d); dat$doy_c2 <- cos(2 * th_d)
  dat$hr_s1 <- sin(th_h);      dat$hr_c1 <- cos(th_h)
  dat
}

TS_FILL_FEATURES_MEMORY <- c(
  "TA", "TA_day", "TA_lag1", "TA_amp_lag1", "TA_m3", "TA_m7", "TA_m30",
  "doy_s1", "doy_c1", "doy_s2", "doy_c2", "hr_s1", "hr_c1"
)

# ------------------------------------------------------------------- methods
#
# The fill's candidates, from the registry in R/ts-estimators.R. The
# manuscript's fit-once forest is not one, but its family is (`rf_ta_netrad`).
ts_fill_methods <- function(have_netrad = FALSE, num_trees = 200) {
  all <- ts_estimators(num_trees = num_trees)
  wanted <- c("lm_ta_pos", "lm_ta", "rf_ta", "lm_memory", "rf_memory")
  if (have_netrad) wanted <- c(wanted, "lm_ta_netrad", "rf_ta_netrad", "rf_memory_netrad")
  all[wanted]
}

# ------------------------------------------------------------------- blocking
#
# A random 70/30 split of autocorrelated half-hours flatters a memory model
# by a third (F13). These schemes hold out contiguous blocks, like the gaps
# being filled. The pipeline uses `year`; the others are for
# scripts/ts-fill-cv.R.

blocks_random <- function(dat, nfold = 5, seed = 222) {
  set.seed(seed)
  split(seq_len(nrow(dat)), sample(rep_len(seq_len(nfold), nrow(dat))))
}
blocks_year <- function(dat) split(seq_len(nrow(dat)), dat$YEAR)
blocks_season <- function(dat) {
  # Four folds a year: the most expensive scheme.
  split(seq_len(nrow(dat)), paste(dat$YEAR, (dat$MONTH - 1) %/% 3))
}
blocks_multiyear <- function(dat, k = 2) {
  yrs <- sort(unique(dat$YEAR))
  if (length(yrs) < 2 * k) return(blocks_year(dat))
  grp <- setNames(((seq_along(yrs) - 1) %/% k), yrs)
  split(seq_len(nrow(dat)), grp[as.character(dat$YEAR)])
}

# Blocks for a named scheme. A function rather than a list of closures:
# targets hashes a list by serializing it, and a closure's serialization
# changes once R byte-compiles it, so a list made every fill -- and every fit
# downstream -- look outdated after the functions had been called.
TS_FILL_BLOCKING_NAMES <- c("random", "year", "multiyear", "season")
ts_fill_blocks <- function(dat, blocking) {
  switch(
    blocking,
    random = blocks_random(dat),
    year = blocks_year(dat),
    multiyear = blocks_multiyear(dat, k = 2),
    season = blocks_season(dat),
    stop("Unknown blocking ", shQuote(blocking), "; expected one of ",
         paste(TS_FILL_BLOCKING_NAMES, collapse = ", "), ".")
  )
}

# Cap training rows (the manuscript's forest caps at 60,000); applied inside
# each fold after the held-out block is removed, so it cannot leak.
subsample_rows <- function(dat, n, seed) {
  if (nrow(dat) <= n) return(dat)
  set.seed(seed)
  dat[sort(sample(nrow(dat), n)), , drop = FALSE]
}

# One out-of-fold prediction per row.
ts_fill_oof <- function(dat, method, blocks, max_train = 20000) {
  out <- rep(NA_real_, nrow(dat))
  for (bi in seq_along(blocks)) {
    idx <- blocks[[bi]]
    if (length(idx) >= nrow(dat)) next
    train <- subsample_rows(dat[-idx, , drop = FALSE], max_train, seed = 222 + bi)
    if (sum(!is.na(train$TS)) < 50) next
    mod <- tryCatch(method$fit(train), error = function(e) NULL)
    if (is.null(mod)) next
    p <- tryCatch(method$predict(mod, dat[idx, , drop = FALSE]), error = function(e) NULL)
    if (!is.null(p) && length(p) == length(idx)) out[idx] <- p
  }
  out
}

# --------------------------------------------------------------- window grid
#
# The (growing_year x window) grid `fit_tas_site()` fits on, so a column is
# scored on the cells the model uses.
tas_windows <- function(gStart, gEnd) {
  nwindow <- max(round((gEnd - gStart + 1) / WINDOW_SIZE), 1)
  tibble::tibble(
    iwindow = seq_len(nwindow),
    window_start = gStart + WINDOW_SIZE * (seq_len(nwindow) - 1),
    window_end = pmin(gStart + WINDOW_SIZE * seq_len(nwindow), gEnd)
  )
}

window_cells <- function(dat, gStart, gEnd, col) {
  wins <- tas_windows(gStart, gEnd)
  dat <- dat |>
    dplyr::mutate(growing_year = growing_year_of(.data$DOY, .data$YEAR))
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
# Scored for what the model does with the column, not only on RMSE:
#
#   within_sd_ratio          spread inside a cell; identifies alpha.
#   across_year_spread_ratio spread of cell means across years within a
#                            window; the variation that identifies TAS.
#
# `gs` is already restricted to the growing season; `truth` and `pred` align
# to its rows.
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
      t = mean(.data$.truth, na.rm = TRUE), p = mean(.data$.pred, na.rm = TRUE),
      a = mean(.data$TA, na.rm = TRUE), .by = "HOUR"
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
    dplyr::summarise(nyear = dplyr::n(), sp_t = stats::sd(.data$mean_t),
                     sp_p = stats::sd(.data$mean_p), .by = "iwindow") |>
    dplyr::filter(.data$nyear >= 3, .data$sp_t > 0)

  qt <- stats::quantile(truth[both], c(0.025, 0.975))
  qp <- stats::quantile(pred[both], c(0.025, 0.975))

  tibble::tibble(
    n = sum(both),
    coverage = sum(both) / max(1, sum(!is.na(truth))),
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
    n_spread_windows = nrow(spread),
    cell_mean_rmse = rmse(cells$mean_p, cells$mean_t),
    cell_mean_bias = mean(cells$mean_p - cells$mean_t),
    n_cells = nrow(cells),
    q025_truth = unname(qt[[1]]), q975_truth = unname(qt[[2]]),
    q025_pred = unname(qp[[1]]), q975_pred = unname(qp[[2]])
  )
}

# --------------------------------------------------------- the fill target
#
# Per site, recipe-independent: score every candidate out of fold, pick the
# lowest RMSE, refit it on every measured row and predict everywhere. Returns
# `TS_memfill` aligned to `ac` and `nightNEE`, the CV table, and bounds rows.
# Never errors: where nothing can be fitted it returns `status != "ok"` and
# the strategy falls back, recording why.
fill_soil_temp <- function(site_data, site_info, blocking = "year",
                           max_train = 20000, num_trees = 200) {
  name_site <- site_info[["site_ID"]]
  fail <- function(why) {
    message("  fill_soil_temp(", name_site, "): ", why)
    list(site_ID = name_site, status = why, method = NA_character_, cv = NULL,
         ac_ts = NULL, night_ts = NULL, ts_bounds = NULL)
  }

  # No truth, no fill: against a reconstruction, the winner is whichever
  # candidate shares its form (DE-Hte: `lm_ta_netrad` at 1.5e-14).
  if (identical(stage_a_truth(site_data), "none")) {
    return(fail(paste0("no measured truth: stage A ran ", stage_a_arm(site_data))))
  }

  ac <- site_data[["ac"]]
  if (!"TS_measured" %in% names(ac)) return(fail("no TS_measured column"))
  ac$TS <- ac$TS_measured
  if (sum(!is.na(ac$TS)) < 1000) return(fail("fewer than 1000 measured soil-temperature rows"))
  if (sum(!is.na(ac$TA)) < 1000) return(fail("fewer than 1000 air-temperature rows"))

  feats <- tryCatch(ts_fill_add_features(ac), error = function(e) NULL)
  if (is.null(feats)) return(fail("feature construction failed"))
  have_netrad <- "NETRAD" %in% names(feats) && sum(!is.na(feats$NETRAD)) > 1000
  methods <- ts_fill_methods(have_netrad, num_trees = num_trees)

  fg <- site_data[["feature_gs"]]
  gStart <- fg$gStart; gEnd <- fg$gEnd
  in_gs <- dplyr::between(feats$DOY, gStart, gEnd)
  gs <- feats[in_gs, , drop = FALSE]
  min_obs_day <- if (fg$dt_minutes < 60) 40 else 20

  blocks <- ts_fill_blocks(feats, blocking)
  scores <- list()
  for (mn in names(methods)) {
    pred <- ts_fill_oof(feats, methods[[mn]], blocks, max_train = max_train)
    m <- tryCatch(
      ts_reconstruction_metrics(gs, gs$TS_measured, pred[in_gs], gStart, gEnd, min_obs_day),
      error = function(e) NULL
    )
    if (!is.null(m)) {
      scores[[mn]] <- dplyr::bind_cols(
        tibble::tibble(site_ID = name_site, method = mn, blocking = blocking,
                       have_netrad = have_netrad),
        m
      )
    }
  }
  scores <- dplyr::bind_rows(scores)
  if (!nrow(scores) || all(!is.finite(scores$rmse))) return(fail("no method could be scored"))

  best <- scores$method[which.min(scores$rmse)]
  mod <- tryCatch(
    methods[[best]]$fit(subsample_rows(feats[!is.na(feats$TS), , drop = FALSE], max_train, seed = 222)),
    error = function(e) NULL
  )
  if (is.null(mod)) return(fail(paste("final fit of", best, "failed")))
  # `overlay`, as for TS_linear: a pure prediction would add NAs.
  ac_ts <- write_back_ts(ac$TS_measured, methods[[best]]$predict(mod, feats), "overlay")

  # nightNEE is a row subset of ac; align by the timestamp columns both carry.
  key <- c("YEAR", "MONTH", "DAY", "HOUR", "MINUTE")
  lookup <- feats[, key]
  lookup$TS_memfill <- ac_ts
  night <- site_data[["nightNEE"]]
  night_ts <- dplyr::left_join(night[, key], lookup, by = key)$TS_memfill
  if (length(night_ts) != nrow(night)) return(fail("nightNEE alignment produced duplicate rows"))

  bounds <- ts_bounds_rows(ac_ts, ac$DOY, gStart, gEnd, "TS_memfill")

  # A zero out-of-fold RMSE means the "measured" column is a function of the
  # predictors; returned, but labelled `degenerate` in `settings`.
  best_rmse <- scores$rmse[scores$method == best]
  list(
    site_ID = name_site, status = "ok", method = best, blocking = blocking,
    cv = scores, ac_ts = ac_ts, night_ts = night_ts, ts_bounds = bounds,
    cv_rmse = best_rmse,
    degenerate = is.finite(best_rmse) && best_rmse < 1e-6,
    # The declared counterpart: DE-Akm's forest-made truth is reproduced to
    # 0.30 C, not zero, so `degenerate` alone would miss it.
    truth_synthetic = identical(stage_a_truth(site_data), "none"),
    n_train = sum(!is.na(feats$TS))
  )
}
