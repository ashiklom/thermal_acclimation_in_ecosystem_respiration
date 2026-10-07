#' Number of CPU cores per fit
#'
#' Used internally by brms
N_CORES <- 4

#' Uniform window size: 2 weeks
WINDOW_SIZE <- 14

#' The models `fit_tas_site()` fits, by name
#'
#' `BRM_FORMULA_TOTAL` and `BRM_FORMULA_DIRECT` below. The name is the `model`
#' column of every output table and what THERMAL_MODELS selects.
MODEL_TYPES <- c("total", "direct")

#' The total model's brms formula
#'
#' The soil-temperature column is `TS_final`: the one column
#' `get_soil_temperature()` leaves on the tables. See R/soil-temperature.R.
BRM_FORMULA_TOTAL <- brms::bf(
  NEE ~ exp(alpha * TS_final + beta * TS_final^2) * C0,
  alpha + beta + C0 ~ 1,
  nl = TRUE
)

#' The direct model's brms formula
BRM_FORMULA_DIRECT <- brms::bf(
  NEE ~ exp(alpha * TS_final + beta * TS_final^2) * SWC / (Hs + SWC) * (C0 + NEE_daytime * k2),
  alpha + beta + C0 + Hs + k2 ~ 1,
  nl = TRUE
)

#' Priors for the total model
#'
#' The priors, with their means and SDs passed as data (`prior_stanvars()`)
#' rather than written into the Stan code. The code is then the same for every
#' window and site, so cmdstanr compiles each model once per process and every
#' later fit reuses it. `sigma` is brms's own default prior, whose scale brms
#' would otherwise compute from the data and write into the code.
BRM_PRIORS_TOTAL <- brms::prior("normal(C0_mu, C0_sd)", nlpar = "C0", lb = 0, ub = 10) +
  brms::prior("normal(alpha_mu, alpha_sd)", nlpar = "alpha", lb = 0, ub = 0.2) +
  brms::prior("normal(beta_mu, beta_sd)", nlpar = "beta", lb = -0.01, ub = 0.0) +
  brms::prior("student_t(3, 0, sigma_scale)", class = "sigma")

#' Priors for the direct model: the total model's plus `Hs` and `k2`
BRM_PRIORS_DIRECT <- BRM_PRIORS_TOTAL +
  brms::prior("normal(Hs_mu, Hs_sd)", nlpar = "Hs", lb = 0, ub = 1000) +
  brms::prior("normal(k2_mu, k2_sd)", nlpar = "k2", lb = 0, ub = 10)

################################################################################

#' One growing year's result in one window, filled in as the loop goes
#'
#' `status` records why a year produced no fit, which a row of NAs cannot.
#'
#' @return A named list of NA scalars: `status`, `nobsv`, `extend_days`, the
#'   model parameters (`alpha`, `beta`, `C0`, `k2`, `Hs`), `TS` and `ERref`.
empty_year_result <- function() {
  list(
    status = NA_character_, nobsv = NA_integer_, extend_days = NA_integer_,
    alpha = NA_real_, beta = NA_real_, C0 = NA_real_, k2 = NA_real_, Hs = NA_real_,
    TS = NA_real_, ERref = NA_real_
  )
}

#' Whether a year's subset needs a wider window
#'
#' Whether a year's subset is too small, or too narrow in soil temperature to
#' contain TSref, to fit on as it is.
#'
#' @param data_subset One growing year's nighttime NEE rows in the window, with
#'   `TS_final`.
#' @param TSref Reference soil temperature: the window's mean `TS_final` (degC).
#' @param nobs_threshold Minimum observations per window-year.
#' @return `TRUE` if `data_subset` has fewer than `nobs_threshold` rows or
#'   `TSref` lies outside its 2.5--97.5 % `TS_final` quantiles.
window_needs_widening <- function(data_subset, TSref, nobs_threshold) {
  ts_quants <- quantile(data_subset$TS_final, c(0.025, 0.975), na.rm = TRUE)
  nrow(data_subset) < nobs_threshold || !dplyr::between(TSref, ts_quants[[1]], ts_quants[[2]])
}

#' The first year-level rule that rejects a subset
#'
#' The first of the four year-level rules, in the original's order, that
#' rejects a subset; NA if none does.
#'
#' @param data_subset One growing year's nighttime NEE rows in the window, with
#'   `NEE` and `TS_final`.
#' @param TSref Reference soil temperature: the window's mean `TS_final` (degC).
#' @return The rule's status string (e.g. `"year_too_few_obs"`), or
#'   `NA_character_`.
year_rejection <- function(data_subset, TSref) {
  ts_quants <- quantile(data_subset$TS_final, c(0.025, 0.975), na.rm = TRUE)
  if (nrow(data_subset) <= 25) {
    return("year_too_few_obs")             # ensure enough observations
  }
  if (!dplyr::between(TSref, ts_quants[[1]], ts_quants[[2]])) {
    return("year_tsref_outside_quantiles") # ensure enough temperature range
  }
  if (median(data_subset$NEE) < 0.2) {
    return("year_median_nee_too_low")      # ensure nighttime NEE is positive
  }
  if (mean(data_subset$NEE) < 0.2) {
    return("year_mean_nee_too_low")
  }
  NA_character_
}


################################################################################

#' Sampler settings
#'
#' @param profile `"full"` or `"fast"`. `full` is the manuscript's; `fast` is
#'   for end-to-end smoke tests and its TAS values mean nothing. An argument
#'   rather than an environment variable, so it is part of each fit's command.
#' @return A list of `profile`, `prior_iter`, `iter`, `chains`, `retry` and
#'   `retry_iter`. Errors on an unknown profile.
fit_settings <- function(profile = "full") {
  switch(
    profile,
    full = list(profile = "full", prior_iter = 2000, iter = 1000, chains = 4,
                retry = TRUE, retry_iter = 4000),
    fast = list(profile = "fast", prior_iter = 500, iter = 400, chains = 2,
                retry = FALSE, retry_iter = NA_real_),
    stop("Unknown fit profile ", shQuote(profile), "; expected \"full\" or \"fast\".")
  )
}

#' Default prior means and SDs
#'
#' The prior means and SDs before `get_priors()` re-centres the means; the SDs
#' stay as they are.
#'
#' @param direct Whether the model is the direct model, which adds `Hs` and
#'   `k2`.
#' @return Named numeric vector of `<par>_mu` and `<par>_sd` values.
default_priors <- function(direct = FALSE) {
  priors <- c(C0_mu = 2, C0_sd = 5, alpha_mu = 0.1, alpha_sd = 1, beta_mu = -0.001, beta_sd = 0.1)
  if (direct) priors <- c(priors, Hs_mu = 10, Hs_sd = 10, k2_mu = 0.5, k2_sd = 2)
  priors
}

#' Re-centre prior means
#'
#' @param priors Named numeric vector of prior means and SDs, as from
#'   `default_priors()`.
#' @param mu Named numeric vector of new means, named by parameter (`alpha`,
#'   `C0`, ...).
#' @return `priors`, with each `<par>_mu` replaced by `mu[["<par>"]]`.
recentre_priors <- function(priors, mu) {
  priors[paste0(names(mu), "_mu")] <- mu
  priors
}

#' brms's default scale for a Gaussian `sigma`
#'
#' The response's MAD, rounded, at least 2.5 -- over the rows brms fits, those
#' complete in the model's variables.
#'
#' @param data Nighttime NEE rows the model is fitted to.
#' @param direct Whether the model is the direct model, which adds `SWC` and
#'   `NEE_daytime` to its variables.
#' @return A single number: the Student-t scale of the `sigma` prior, in NEE
#'   units (umol/m2/s).
default_sigma_scale <- function(data, direct = FALSE) {
  vars <- c("NEE", "TS_final", if (direct) c("SWC", "NEE_daytime"))
  nee <- data[stats::complete.cases(data[vars]), "NEE", drop = TRUE]
  max(2.5, round(stats::mad(nee), 1))
}

#' Prior hyperparameters as brms stanvars
#'
#' @param priors Named numeric vector of prior means and SDs.
#' @param data Nighttime NEE rows the model is fitted to.
#' @param direct Whether the model is the direct model.
#' @return A `brms::stanvar()` object passing every prior mean and SD, plus
#'   `sigma_scale`, to Stan as data.
prior_stanvars <- function(priors, data, direct = FALSE) {
  values <- c(priors, sigma_scale = default_sigma_scale(data, direct))
  Reduce(`+`, Map(brms::stanvar, values, names(values)))
}

#' Data-centred prior means for one window
#'
#' @param model_data The window's nighttime NEE rows, across all growing years.
#' @param direct Whether the model is the direct model.
#' @param fs Sampler settings from `fit_settings()`.
#' @return Named numeric vector of prior means and SDs, as `default_priors()`,
#'   with the means re-centred on a brms fit to `model_data`.
get_priors <- function(model_data, direct = FALSE, fs = fit_settings("full")) {
  priors <- default_priors(direct)

  # The NLS warm start is fitted on log parameters, which keeps them positive.
  if (direct) {
    frmu_nls <- NEE ~ exp(exp(alpha_ln) * TS_final - exp(beta_ln) * TS_final^2) *
      SWC / (exp(Hs_ln) + SWC) * (exp(C0_ln) + NEE_daytime * exp(k2_ln))
    start_nls <- c(C0_ln = 0.7, alpha_ln = -2.99, beta_ln = -6.9, k2_ln = -1.6, Hs_ln = 2.3)
  } else {
    frmu_nls <- NEE ~ exp(exp(alpha_ln) * TS_final - exp(beta_ln) * TS_final^2) * (exp(C0_ln))
    start_nls <- c(C0_ln = 0.7, alpha_ln = -2.99, beta_ln = -6.9)
  }

  # Centre the priors on an NLS fit, capped at the prior bounds.
  tryCatch(
    {
      mod_nls <- gslnls::gsl_nls(fn = frmu_nls, data = model_data, start = start_nls)
      est <- exp(stats::coef(mod_nls))
      mu <- c(alpha = min(est[["alpha_ln"]], 0.2), beta = -min(est[["beta_ln"]], 0.01),
              C0 = min(est[["C0_ln"]], 10))
      if (direct) mu <- c(mu, k2 = min(est[["k2_ln"]], 10), Hs = min(est[["Hs_ln"]], 1000))
      priors <- recentre_priors(priors, mu)
    },
    error = function(e) {
      message("NLS fit failed (", conditionMessage(e), "); keeping the default priors.")
    }
  )

  # Then re-centre them on a brms fit across all the window's data.
  mod0 <- brms::brm(
    if (direct) BRM_FORMULA_DIRECT else BRM_FORMULA_TOTAL,
    prior = if (direct) BRM_PRIORS_DIRECT else BRM_PRIORS_TOTAL,
    stanvars = prior_stanvars(priors, model_data, direct),
    data = model_data,
    iter = fs$prior_iter,
    cores = min(N_CORES, fs$chains),
    chains = fs$chains,
    backend = "cmdstanr",
    control = list(adapt_delta = 0.95, max_treedepth = 15),
    refresh = 0
  )
  est <- brms::fixef(mod0)[, "Estimate"]
  pars <- c("alpha", "beta", "C0", if (direct) c("k2", "Hs"))
  recentre_priors(priors, stats::setNames(est[paste0(pars, "_Intercept")], pars))
}


#' Step 02 for one site and model: TAS from 14-day windows across years
#'
#' @param site_data The site's step 01 result (`prep_nee_ac()` with ERA5 soil
#'   water attached): a list with `ac`, `nightNEE`, `feature_gs` and
#'   `ts_bounds`.
#' @param site_info One row of the site declaration table.
#' @param model One of `MODEL_TYPES`.
#' @param ts_col,swc_col Override the recipe's columns, for sensitivity runs.
#' @param fit If `FALSE`, the same window/year loop with no model fitting:
#'   which windows and years survive, and why. Deterministic, seconds.
#' @param recipe The methodology, resolved through R/strategies.R; NULL is the
#'   manuscript's.
#' @param fill The site's `fill_soil_temp()` result (memory_fill recipes).
#' @param fit_profile Sampler profile; see `fit_settings()`.
#' @return A list of `outcome` (one-row tibble with `TAS`, `TASp`, `RMSE` and
#'   `R2`; `NULL` when `fit = FALSE`), `outcome_siteyear` (one row per window
#'   and growing year), `window_skips` (skipped windows and why) and `settings`
#'   (one-row tibble of every choice behind the run).
fit_tas_site <- function(site_data, site_info, model = "total",
                         ts_col = NULL, swc_col = NULL, fit = TRUE,
                         recipe = NULL, fill = NULL, fit_profile = "full") {
  model <- rlang::arg_match0(model, MODEL_TYPES)
  # What the helpers below branch on: the direct model's extra terms, and
  # whether it takes soil water.
  direct <- model == "direct"
  recipe <- recipe %||% original_recipe()
  validate_recipe(recipe)
  fs <- fit_settings(fit_profile)

  a_measure_night_complete <- site_data[["nightNEE"]]
  ac <- site_data[["ac"]]
  feature_gs <- site_data[["feature_gs"]]

  name_site <- feature_gs[["site_ID"]]

  if (!identical(site_info[["site_ID"]], name_site)) {
    stop(
      "site_info is for ", site_info[["site_ID"]],
      " but site_data is for ", name_site, "."
    )
  }

  gStart <- feature_gs[["gStart"]]
  gEnd <- feature_gs[["gEnd"]]

  # Soil temperature (`TS_final` from here on). Before the soil-water filter,
  # because the fill aligns with the tables as step 01 left them.
  soil <- get_soil_temperature(site_data, site_info, recipe = recipe, fill = fill, ts_col = ts_col)
  ac <- soil[["ac"]]
  a_measure_night_complete <- soil[["nightNEE"]]
  ts_meta <- soil[["meta"]]
  ts_col <- ts_meta[["ts_col"]]
  tStart <- ts_meta[["tStart"]]
  tEnd <- ts_meta[["tEnd"]]

  # Soil water: choose a column, which depends on the model as well as the
  # site.
  swc_choice <- if (is.null(swc_col)) {
    choose_swc_col(recipe, site_info, direct)
  } else {
    list(swc_col = swc_col, reason = "swc_col argument")
  }
  swc_col <- swc_choice$swc_col
  if (!is.na(swc_col)) {
    ac <- resolve_swc_column(ac, swc_col, name_site)
    a_measure_night_complete <- resolve_swc_column(a_measure_night_complete, swc_col, name_site)
  }
  SWC_use <- !is.na(swc_col)

  # As the original: rows without measured soil water are dropped where it is
  # used; the ERA5 fallback is not filtered on.
  if (isTRUE(site_info[["SWC_use"]])) {
    a_measure_night_complete <- a_measure_night_complete |>
      dplyr::filter(!is.na(.data$SWC))
  }

  # The span the windows tile. The detected season above still picks the
  # control year under every season strategy.
  win <- choose_window_season(recipe, feature_gs)
  wStart <- win[["gStart"]]
  wEnd <- win[["gEnd"]]

  # Daily daytime NEE (umol/m2/s), and its rolling mean.
  dt <- feature_gs[["dt_minutes"]]

  ac_day <- ac |>
    dplyr::filter(.data$daytime) |>
    dplyr::summarise(
      NEE_daytime1 = max(-sum(.data$NEE_uStar_f, na.rm = TRUE) * dt * 60 / 86400, 0.0),
      .by = c("YEAR", "DOY")
    )

  # get rolling average of the prior three days
  ac_day[["NEE_daytime"]] <- zoo::rollapply(
    ac_day[["NEE_daytime1"]],
    width = 3,
    FUN = mean,
    align = "right",
    fill = ac_day[["NEE_daytime1"]][1]
  )

  # attach it to nighttime data
  ac_min_doy <- min(ac[["DOY"]])    # nolint

  # TODO: Move this logic out of here
  a_measure_night_complete <- a_measure_night_complete |>
    dplyr::mutate(DOY_gpp = dplyr::case_when(
      .data$HOUR >= 12 ~ .data$DOY,
      .data$HOUR < 12 & .data$DOY == ac_min_doy ~ DOY,
      .data$HOUR < 12 & .data$DOY != ac_min_doy ~ DOY - 1
    )) |>
    dplyr::left_join(ac_day, by = c("YEAR", "DOY_gpp" = "DOY")) |>
    dplyr::select(-"DOY_gpp") |>
    dplyr::filter(!is.na(.data$NEE_daytime1))

  a_measure_night_complete <- a_measure_night_complete |>
    dplyr::mutate(growing_year = growing_year_of(.data$DOY, .data$YEAR))
  ac <- ac |>
    dplyr::mutate(growing_year = growing_year_of(.data$DOY, .data$YEAR))

  # A wrapped site loses its partial first and last growing years.
  if (growing_year_start(site_info) > 1) {
    a_measure_night_complete <- a_measure_night_complete |>
      dplyr::filter(dplyr::between(.data$growing_year, ac$YEAR[1], ac$YEAR[nrow(ac)] - 1))
    ac <- ac |>
      dplyr::filter(dplyr::between(.data$growing_year, ac$YEAR[1], ac$YEAR[nrow(ac)] - 1))
  }

  years <- sort(unique(a_measure_night_complete$growing_year))

  # decide control year: the year with growing-season TS closest to long-term mean.
  ac_yearly_gs <- ac |>
    dplyr::filter(dplyr::between(.data$DOY, gStart, gEnd)) |>
    dplyr::filter(.data$growing_year %in% years) |>
    dplyr::group_by(.data$growing_year) |>
    dplyr::summarise(TS = mean(.data$TS_final, na.rm = TRUE), .groups = "drop_last") |>
    dplyr::ungroup()

  control_year <- ac_yearly_gs$growing_year[which.min(abs(ac_yearly_gs$TS - mean(ac_yearly_gs$TS)))]

  # Minimum observations per window-year, by time step.
  nobs_threshold <- if (dt == 30) 100 else 60

  # use non-overlapping windows and determine number of windows for growing season; decide to use overlapping windows
  nwindow <- max(round((wEnd - wStart + 1) / WINDOW_SIZE), 1)

  # Every choice behind this run's layout, so differences between runs can be
  # attributed.
  settings <- tibble::tibble(
    site_ID = name_site,
    recipe_id = recipe$recipe_id,
    model = model,
    fit_profile = fs$profile,
    ts_col = ts_meta[["ts_col"]],
    swc_col = if (is.na(swc_col)) NA_character_ else swc_col,
    SWC_use = SWC_use,
    gStart = gStart,
    gEnd = gEnd,
    window_gStart = wStart,
    window_gEnd = wEnd,
    tStart = tStart,
    tEnd = tEnd,
    dt = dt,
    nobs_threshold = nobs_threshold,
    window_size = WINDOW_SIZE,
    nwindow = nwindow,
    control_year = control_year,
    nyear_available = length(years),
    # Which strategy made each choice, and why.
    ts_strategy = ts_meta[["ts_strategy"]],
    ts_reason = ts_meta[["ts_reason"]],
    ts_verdict = ts_meta[["ts_verdict"]],
    ts_flags = ts_meta[["ts_flags"]],
    fill_method = ts_meta[["fill_method"]],
    fill_cv_rmse = ts_meta[["fill_cv_rmse"]],
    fill_degenerate = ts_meta[["fill_degenerate"]],
    fill_truth_synthetic = ts_meta[["fill_truth_synthetic"]],
    ts_measured_synthetic = ts_meta[["ts_measured_synthetic"]],
    ts_source = ts_meta[["ts_source"]],
    ts_refused = ts_meta[["ts_refused"]],
    stage_a_estimator = ts_meta[["stage_a_estimator"]],
    stage_a_family = ts_meta[["stage_a_family"]],
    stage_a_mode = ts_meta[["stage_a_mode"]],
    season_strategy = recipe$season,
    bounds_strategy = ts_meta[["bounds_strategy"]],
    bounds_reason = ts_meta[["bounds_reason"]],
    swc_strategy = recipe$swc,
    swc_reason = swc_choice$reason
  )

  window_results <- list()
  for (iwindow in seq_len(nwindow)) {
    message("########################################")
    message(iwindow, " of ", nwindow)
    window_start <- wStart + WINDOW_SIZE * (iwindow - 1)
    window_end <- min(wStart + WINDOW_SIZE * iwindow, wEnd)
    window_name <- paste(window_start, window_end, sep = "_")
    window_results[[window_name]] <- fit_tas_window(
      ac, ac_day, a_measure_night_complete,
      window_start, window_end, nwindow, nobs_threshold, control_year,
      SWC_use, tStart, tEnd, wEnd,
      direct = direct, fit = fit, fs = fs
    )
  }

  fitted_windows <- Filter(
    function(w) !is.null(w[["outcome_siteyear"]]), window_results
  )
  if (length(fitted_windows) == 0) {
    stop("No results produced, possibly because all windows were skipped.")
  }

  window_results_df <- window_results |>
    lapply(`[[`, "outcome_siteyear") |>
    dplyr::bind_rows() |>
    dplyr::mutate(site_ID = name_site, recipe_id = recipe$recipe_id, model = .env$model,
                  .before = 1)

  window_skips <- window_results |>
    lapply(`[[`, "skipped") |>
    dplyr::bind_rows()
  if (nrow(window_skips)) {
    window_skips <- dplyr::mutate(window_skips, site_ID = name_site,
                                  recipe_id = recipe$recipe_id, model = .env$model,
                                  .before = 1)
  }

  ER_obs_pred <- window_results |>
    lapply(`[[`, "ER_obs_pred") |>
    dplyr::bind_rows()

  if (!fit) {
    # NULL `outcome`, so a structure-only result cannot pass for a fitted one.
    return(list(
      outcome = NULL,
      outcome_siteyear = window_results_df,
      window_skips = window_skips,
      settings = settings
    ))
  }

  fit_stats <- caret::postResample(pred = ER_obs_pred$NEE_pred, obs = ER_obs_pred$NEE)

  tas <- across_year_tas(window_results_df)
  settings$tas_status <- tas[["status"]]

  outcome <- tibble::tibble(
    site_ID = name_site,
    recipe_id = recipe$recipe_id,
    model = model,
    fit_profile = fs$profile,
    RMSE = fit_stats[["RMSE"]],
    R2 = fit_stats[["Rsquared"]],
    control_year = control_year,
    window_size = WINDOW_SIZE,
    nwindow = nwindow,
    TAS = tas[["TAS"]],
    TASp = tas[["TASp"]]
  )

  list(
    outcome = outcome,
    outcome_siteyear = window_results_df,
    window_skips = window_skips,
    settings = settings
  )
}


#' The across-year regression that defines TAS
#'
#' Log respiration ratio on window-mean soil temperature, with `window` as a
#' factor and an AR(1) error within window across years.
#'
#' Too few windows, or a singular fit, is a result -- the site did not earn an
#' estimate -- so it returns NA TAS with a `status` rather than an error.
#'
#' @param window_results_df Per-window, per-growing-year results from
#'   `fit_tas_window()`, bound across windows: needs `lnRatio`, `TS`, `window`
#'   and `growing_year`.
#' @return A list of `TAS` (the slope on `TS`), `TASp` (its p-value) and
#'   `status` (`"fitted"`, or why not).
across_year_tas <- function(window_results_df) {
  usable <- window_results_df[!is.na(window_results_df[["lnRatio"]]), , drop = FALSE]
  n_windows <- length(unique(usable[["window"]]))
  if (n_windows < 2) {
    return(list(
      TAS = NA_real_, TASp = NA_real_,
      status = sprintf("too_few_windows: %d window(s) with a fitted ratio, need 2", n_windows)
    ))
  }
  mod_ar1 <- tryCatch(
    nlme::gls(
      lnRatio ~ TS + window,
      data = window_results_df,
      correlation = nlme::corAR1(form = ~ growing_year | window),
      na.action = na.omit
    ),
    error = function(e) e
  )
  if (inherits(mod_ar1, "error")) {
    return(list(TAS = NA_real_, TASp = NA_real_,
                status = paste0("gls_failed: ", conditionMessage(mod_ar1))))
  }
  smry <- summary(mod_ar1)[["tTable"]]
  list(TAS = smry["TS", "Value"], TASp = smry["TS", "p-value"], status = "fitted")
}

#' Fit one window across every growing year
#'
#' @param ac The site's flux table, with `TS_final`, `growing_year` and, when
#'   used, `SWC`.
#' @param ac_day Daily daytime NEE by `YEAR` and `DOY`, with its 3-day rolling
#'   mean `NEE_daytime` (umol/m2/s).
#' @param a_measure_night_complete Nighttime NEE rows, with `NEE_daytime1`,
#'   `NEE_daytime` and `growing_year` attached.
#' @param window_start,window_end First and last DOY of the window (possibly
#'   wrapped past 366).
#' @param nwindow Number of windows in the season; a lone window is never
#'   skipped for its soil temperature.
#' @param nobs_threshold Minimum observations per window-year.
#' @param control_year The growing year whose reference respiration the other
#'   years are compared against.
#' @param SWC_use Whether the model uses soil water, so the reference
#'   conditions include `SWC`.
#' @param tStart,tEnd Soil-temperature bounds (degC). With more than one
#'   window, a window whose mean lies outside `[max(tStart, 2), tEnd]` is
#'   skipped.
#' @param gEnd Last DOY of the span the windows tile; a year's window stops
#'   widening at it once the year has enough rows and `TSref` is at or above
#'   their maximum `TS_final`.
#' @param direct Whether the model is the direct model.
#' @param fit If `FALSE`, run the window/year layout with no model fitting.
#' @param fs Sampler settings from `fit_settings()`.
#' @return A list of `outcome_siteyear` (one row per growing year: `status`,
#'   parameters, `TS`, `ERref` and, when fitted, `lnRatio`), `ER_obs_pred`
#'   (observed rows with fitted `NEE_pred`) and `skipped` (a one-row tibble
#'   saying why, if the window was skipped; otherwise `NULL`).
fit_tas_window <- function(
  ac, ac_day, a_measure_night_complete,
  window_start, window_end, nwindow, nobs_threshold, control_year,
  SWC_use, tStart, tEnd, gEnd,
  direct = FALSE, fit = TRUE, fs = fit_settings("full")
) {

  # A skipped window is a record, not NULL (which would vanish from the list).
  window_skip <- function(reason, detail = NA_character_) {
    list(
      outcome_siteyear = NULL,
      ER_obs_pred = NULL,
      skipped = tibble::tibble(
        window = paste(window_start, window_end, sep = "_"),
        window_start = window_start,
        window_end = window_end,
        reason = reason,
        detail = detail
      )
    )
  }

  ac_yearly_window <- ac |>
    dplyr::filter(dplyr::between(.data$DOY, window_start, window_end)) |>
    dplyr::group_by(.data$growing_year) |>
    dplyr::summarise(TS = mean(.data$TS_final, na.rm = TRUE), .groups = "drop_last") |>
    dplyr::ungroup()

  skip <- (!dplyr::between(
    mean(ac_yearly_window$TS, na.rm = TRUE),
    max(tStart, 2.0),
    tEnd
  ) && nwindow > 1)

  if (skip) {
    message("Skipping because TS is not in valid range.")
    return(window_skip(
      "window_ts_out_of_range",
      sprintf("mean TS %.3f outside [%.3f, %.3f]",
              mean(ac_yearly_window$TS, na.rm = TRUE), max(tStart, 2.0), tEnd)
    ))
  }

  model_data <- a_measure_night_complete |>
    dplyr::filter(dplyr::between(.data$DOY, window_start, window_end))

  if (nrow(model_data) < 100) {
    message("Skipping because too few values in window (", nrow(model_data), ").")
    return(window_skip(
      "window_too_few_obs",
      sprintf("%d rows, need 100", nrow(model_data))
    ))
  }

  # get reference temperature, SWC, and NEEday of each window.
  # This is used to fit the data later.
  keep <- function(dat) dplyr::between(dat$DOY, window_start, window_end)
  data_ref <- data.frame(
    TS_final = mean(ac$TS_final[keep(ac)], na.rm = TRUE),
    NEE_daytime = mean(ac_day$NEE_daytime[keep(ac_day)], na.rm = TRUE)
  )
  if (SWC_use) {
    data_ref[["SWC"]] <- mean(ac$SWC[keep(ac)], na.rm = TRUE)
  }

  priors <- if (fit) get_priors(model_data, direct = direct, fs = fs) else NULL

  ERref_control <- NA

  years <- sort(unique(a_measure_night_complete[["growing_year"]]))
  TSref <- data_ref[["TS_final"]]

  year_results <- list()

  for (iyear in years) {
    data_subset <- model_data |>
      dplyr::filter(.data$growing_year == iyear)

    # Widen the window 3 days at a time, up to 24, until the year has enough
    # observations and TSref inside their 2.5-97.5 % range.
    extend_days <- 0
    while (window_needs_widening(data_subset, TSref, nobs_threshold)) {
      extend_days <- extend_days + 3
      data_subset <- a_measure_night_complete |>
        dplyr::filter(.data$growing_year == iyear) |>
        dplyr::filter(dplyr::between(.data$DOY, window_start - extend_days, window_end + extend_days))
      # some conditions to break out to avoid dead loop
      if (
        (nrow(data_subset) >= nobs_threshold) &&
          ((window_end + extend_days) >= gEnd) &&
          (TSref >= max(data_subset$TS_final, na.rm = TRUE))
      ) {
        break
      }
      if (extend_days >= 24) { # max window size: two months
        break
      }
    }

    year_result <- empty_year_result()

    year_result$nobsv <- nrow(data_subset)
    year_result$extend_days <- as.integer(extend_days)

    rejection <- year_rejection(data_subset, TSref)

    if (!is.na(rejection)) {
      year_result$status <- rejection
      year_results[[as.character(iyear)]] <- list(year_result = year_result, ER_obs_pred = NULL)
      next
    }

    if (!fit) {
      # Everything above this line is layout: window extension, the year
      # rejection rules, the observation count. Everything below is the model.
      year_result$status <- "not_fitted"
      year_result$TS <- ac_yearly_window |>
        dplyr::filter(.data$growing_year == iyear) |>
        dplyr::pull("TS")
      year_results[[as.character(iyear)]] <- list(year_result = year_result, ER_obs_pred = NULL)
      next
    }

    mod <- fit_with_retry(data_subset, priors, direct, fs = fs)

    # Extract model results
    ER_obs_pred <- data_subset |>
      dplyr::mutate(NEE_pred = fitted(mod)[, "Estimate"]) |>
      dplyr::filter(dplyr::between(.data$DOY, window_start, window_end))

    model_params <- brms::fixef(mod)[, "Estimate"]
    year_result$alpha <- model_params[["alpha_Intercept"]]
    year_result$beta <- model_params[["beta_Intercept"]]
    year_result$C0 <- model_params[["C0_Intercept"]]

    if (direct) {
      year_result$k2 <- model_params[["k2_Intercept"]]
      year_result$Hs <- model_params[["Hs_Intercept"]]
    }

    year_result$TS <- ac_yearly_window |>
      dplyr::filter(.data$growing_year == iyear) |>
      dplyr::pull("TS")

    df_ERref <- fitted(mod, newdata = data_ref)
    if (df_ERref[, "Estimate"] >= df_ERref[, "Est.Error"]) {
      year_result$ERref <- df_ERref[, "Estimate"]
    }

    if (iyear == control_year) {
      ERref_control <- year_result$ERref
    }

    year_result$status <- "fitted"
    year_results[[as.character(iyear)]] <- list(year_result = year_result, ER_obs_pred = ER_obs_pred)
  } # end year loop

  df_site_year_window <- year_results |>
    lapply(function(r) tibble::tibble(!!!r$year_result)) |>
    dplyr::bind_rows(.id = "growing_year") |>
    dplyr::mutate(
      growing_year = as.integer(.data$growing_year),
      window = paste(window_start, window_end, sep = "_")
    )

  ER_obs_pred_all <- year_results |>
    lapply(`[[`, "ER_obs_pred") |>
    dplyr::bind_rows()

  if (!fit) {
    return(list(
      outcome_siteyear = df_site_year_window,
      ER_obs_pred = NULL,
      skipped = NULL
    ))
  }

  # if no data in a control year during this window, use average ER across years as the reference conditions
  if (is.na(ERref_control)) {
    ERref_control <- mean(df_site_year_window$ERref, na.rm = TRUE)
  }

  df_site_year_window <- df_site_year_window |>
    dplyr::mutate(lnRatio = log(.data$ERref / ERref_control))

  # remove unrealistic extreme values due to potentially large gaps; this only affects a few sites
  x <- df_site_year_window$lnRatio
  outlier <- boxplot.stats(x, coef = 3)$out
  id.remove <- match(outlier[abs(outlier) > 1.5], x)
  if (length(id.remove) > 0) {
    df_site_year_window <- df_site_year_window[-id.remove, ]
  }

  list(
    outcome_siteyear = df_site_year_window,
    ER_obs_pred = ER_obs_pred_all,
    skipped = NULL
  )
}

#' Fit one year's brms model, retrying on failure or divergences
#'
#' @param data_subset One growing year's nighttime NEE rows in the window.
#' @param priors Prior means and SDs from `get_priors()`.
#' @param direct Whether the model is the direct model.
#' @param fs Sampler settings from `fit_settings()`; `retry` and `retry_iter`
#'   govern the second attempt.
#' @return The `brmsfit`, or a `try-error` if the last attempt failed.
fit_with_retry <- function(data_subset, priors, direct = FALSE, fs = fit_settings("full")) {
  brm_args <- list(
    if (direct) BRM_FORMULA_DIRECT else BRM_FORMULA_TOTAL,
    prior = if (direct) BRM_PRIORS_DIRECT else BRM_PRIORS_TOTAL,
    stanvars = prior_stanvars(priors, data_subset, direct),
    data = data_subset,
    iter = fs$iter,
    cores = min(N_CORES, fs$chains),
    chains = fs$chains,
    backend = "cmdstanr",
    control = list(adapt_delta = 0.90, max_treedepth = 15),
    refresh = 0
  )
  # Through a wrapper, not `do.call(brms::brm, ...)`, which would inline the
  # data into the call that `try()` prints on error.
  mod <- do.call(function(...) try(brms::brm(...)), brm_args)

  if (!inherits(mod, "try-error")) {
    np <- brms::nuts_params(mod)
    n_divergent <- sum(subset(np, Parameter == "divergent__")$Value)
    failed_brm <- n_divergent > 0 
  } else {
    failed_brm <- TRUE
  }

  # if brm models fail or have divergent transitions, try another time
  if (failed_brm && isTRUE(fs$retry)) {
    brm_args2 <- modifyList(brm_args, list(
      iter = fs$retry_iter,
      control = list(adapt_delta = 0.98, max_treedepth = 15)
    ))
    mod <- do.call(function(...) try(brms::brm(...)), brm_args2)
  }

  mod
}
