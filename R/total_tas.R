N_CORES <- 4        # Used internally by brms
WINDOW_SIZE <- 14   # Uniform window size: 2 weeks.

# The soil-temperature column is `TS_final`: the one column
# `get_soil_temperature()` leaves on the tables. See R/soil-temperature.R.
BRM_FORMULA_TOTAL <- brms::bf(
  NEE ~ exp(alpha * TS_final + beta * TS_final^2) * C0,
  alpha + beta + C0 ~ 1,
  nl = TRUE
)

BRM_FORMULA_DIRECT <- brms::bf(
  NEE ~ exp(alpha * TS_final + beta * TS_final^2) * SWC / (Hs + SWC) * (C0 + NEE_daytime * k2),
  alpha + beta + C0 + Hs + k2 ~ 1,
  nl = TRUE
)


################################################################################

# Soil water content is expressed as a PERCENT (0-100) everywhere in this
# analysis, matching the convention of the flux-tower SWC columns and of the
# `Hs ~ normal(10, 10), ub = 1000` prior in `BRM_FORMULA_DIRECT`. ERA5-Land
# reports volumetric soil water (`swvl1`) as a fraction (m3/m3), so it has to be
# rescaled on read. `data-raw/` deliberately holds the provider's native units.
ERA5_SWC_TO_PERCENT <- 100

read_era5_swc <- function(name_site, path = file.path("data-raw", "ERA5_daily_swc.csv")) {
  swc <- readr::read_csv(
    path,
    col_types = readr::cols(
      time = readr::col_date(),
      site = readr::col_character(),
      SWC = readr::col_double()
    ),
    progress = FALSE
  ) |>
    dplyr::filter(.data$site == name_site)

  if (nrow(swc) == 0) {
    stop("No ERA5 soil water data for site ", name_site, " in ", path, ".")
  }

  # One row per site-day, or the daily join onto the half-hourly tables would
  # multiply rows instead of annotating them -- which would silently inflate
  # every count downstream.
  if (anyDuplicated(swc[c("time")])) {
    stop(
      "ERA5 soil water for ", name_site, " has duplicate dates in ", path,
      ". Expected one row per site-day."
    )
  }

  # Guard against silently ingesting data that has already been rescaled, or
  # that is in some other unit entirely. ERA5 volumetric soil water is
  # physically bounded well below 1.
  swc_max <- max(swc$SWC, na.rm = TRUE)
  if (!is.finite(swc_max) || swc_max > 1.5) {
    stop(
      "ERA5 soil water for ", name_site, " has a maximum of ", signif(swc_max, 4),
      ", which is not a volumetric fraction (m3/m3). Expected values within ",
      "[0, 1]; check the units in ", path, "."
    )
  }

  swc |>
    dplyr::mutate(
      date = as.Date(.data$time),
      YEAR = lubridate::year(.data$date),
      MONTH = lubridate::month(.data$date),
      DAY = lubridate::day(.data$date),
      SWC = .data$SWC * ERA5_SWC_TO_PERCENT
    ) |>
    dplyr::select("YEAR", "MONTH", "DAY", "SWC")
}

################################################################################

opt_int <- S7::new_property(S7::class_integer, default = NA_integer_)
opt_num <- S7::new_property(S7::class_numeric, default = NA_real_)
opt_chr <- S7::new_property(S7::class_character, default = NA_character_)

# `status` records why a year produced no fit. Without it a skipped year is
# indistinguishable from a failed one -- both arrive as a row of NAs -- and the
# most useful comparison against the manuscript is exactly which years were
# dropped and for which of the four reasons.
Year_Result <- S7::new_class("Year_Result", properties = list(
  status = opt_chr,
  nobsv = opt_int,
  extend_days = opt_int,
  alpha = opt_num,
  beta = opt_num,
  C0 = opt_num,
  k2 = opt_num,
  Hs = opt_num,
  TS = opt_num,
  ERref = opt_num
))

# Which of the four year-level rules rejects a subset, or NA if none does.
#
# The order is the order the original tested them in, so the name returned is
# the *first* failure -- a year can breach more than one. Extracted from the
# loop so that each branch can be exercised on its own: written inline as a
# `||` chain beside a parallel `if/else` naming the reason, a mutation that
# misattributed a rejection still produced a plausible reason and the suite
# could not tell. The mutation check found exactly that hole.
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


year_result_df <- S7::new_generic("year_result_df", "year_result")
S7::method(year_result_df, Year_Result) <- function(year_result) {
  tibble::tibble(!!!S7::props(year_result))
}

################################################################################

# How hard the sampler works. `full` is the manuscript's configuration and the
# only one whose numbers mean anything. `fast` exists so that the whole
# pipeline -- every recipe, every collector, the report -- can be exercised end
# to end on a laptop in minutes rather than hours; a TAS produced under it is a
# smoke-test artefact and the settings row says so.
#
# Threaded as an argument rather than read from an environment variable inside
# the fit, so that it is part of the target command: changing it invalidates
# exactly the fits, and a store cannot mix profiles without saying so.
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

adjust_prior <- function(priors, nlpar, p1, p2, family = "normal") {
  priors$prior[priors$nlpar == nlpar] <- sprintf("%s(%f, %f)", family, p1, p2)
  priors
}

get_priors <- function(model_data, direct = FALSE, fs = fit_settings("full")) {
  # Start with default prior
  priors <- brms::prior("normal(2, 5)", nlpar = "C0", lb = 0, ub = 10) +
    brms::prior("normal(0.1, 1)", nlpar = "alpha", lb = 0, ub = 0.2) +
    brms::prior("normal(-0.001, 0.1)", nlpar = "beta", lb = -0.01, ub = 0.0)

  if (direct) {
    priors_water <- brms::prior("normal(10, 10)", nlpar = "Hs", lb = 0, ub = 1000)
    priors_gpp <- brms::prior("normal(0.5, 2)", nlpar = "k2", lb = 0, ub = 10)
    priors <- priors + priors_water + priors_gpp
  }

  if (direct) {
    frmu_nls <- NEE ~ exp(exp(alpha_ln) * TS_final - exp(beta_ln) * TS_final^2) *
      SWC / (exp(Hs_ln) + SWC) * (exp(C0_ln) + NEE_daytime * exp(k2_ln))
    start_nls <- c(C0_ln = 0.7, alpha_ln = -2.99, beta_ln = -6.9, k2_ln = -1.6, Hs_ln = 2.3)
  } else {
    frmu_nls <- NEE ~ exp(exp(alpha_ln) * TS_final - exp(beta_ln) * TS_final^2) * (exp(C0_ln))
    start_nls <- c(C0_ln = 0.7, alpha_ln = -2.99, beta_ln = -6.9)
  }


  # Next, attempt to update the prior using NLS fit.
  tryCatch(
    {
      mod_nls <- gslnls::gsl_nls(fn = frmu_nls, data = model_data, start = start_nls)
      priors <- priors |>
        adjust_prior("alpha", min(exp(coefficients(mod_nls)["alpha_ln"]), 0.2), 1.0) |>
        adjust_prior("beta", -min(exp(coefficients(mod_nls)["beta_ln"]), 0.01), 0.1) |>
        adjust_prior("C0", min(exp(coefficients(mod_nls)["C0_ln"]), 10), 5)
      if (direct) {
        priors <- priors |>
          adjust_prior("k2", min(exp(coefficients(mod_nls)["k2_ln"]), 10), 2) |>
          adjust_prior("Hs", min(exp(coefficients(mod_nls)["Hs_ln"]), 1000), 10)
      }
    },
    error = function(e) {
      message("NLS fit failed with error: ", str(e), ".\n\nKeeping priors unchanged.")
    }
  )

  # Next, try to update the prior again using brms fit across all data.
  brm_frmu <- if (direct) BRM_FORMULA_DIRECT else BRM_FORMULA_TOTAL
  mod0 <- brms::brm(
    brm_frmu,
    prior = priors,
    data = model_data,
    iter = fs$prior_iter,
    cores = min(N_CORES, fs$chains),
    chains = fs$chains,
    backend = "cmdstanr",
    control = list(adapt_delta = 0.95, max_treedepth = 15),
    refresh = 0
  )
  priors <- priors |>
    adjust_prior("alpha", brms::fixef(mod0)["alpha_Intercept", "Estimate"], 1.0) |>
    adjust_prior("beta", brms::fixef(mod0)["beta_Intercept", "Estimate"], 0.1) |>
    adjust_prior("C0", brms::fixef(mod0)["C0_Intercept", "Estimate"], 5)

  if (direct) {
    priors <- priors |> 
      adjust_prior("k2", brms::fixef(mod0)["k2_Intercept", "Estimate"], 2) |>
      adjust_prior("Hs", brms::fixef(mod0)["Hs_Intercept", "Estimate"], 10)
  }

  priors
}


# `ts_col` and `swc_col` override the soil-temperature and soil-water columns
# the model is fitted on, for sensitivity runs that compare estimation methods
# or measured-versus-reanalysis soil water against each other. Left NULL they
# resolve to the site's declaration, which is the normal path.
# `fit = FALSE` walks the same window and year loop but performs no model
# fitting at all -- no `gsl_nls` warm start, no `brm`, no `gls`. What comes back
# is the structural half of the run: which windows survived, which years were
# kept or rejected and why, how far each window had to be extended, how many
# observations it ended up with, and the growing-season soil temperature each
# year contributed.
#
# The point is that it is the *same* loop rather than a transcription of it. A
# separate reimplementation of the layout rules would only ever prove that the
# two agreed with each other. This runs in seconds instead of minutes, and
# because none of those quantities depends on the sampler, two runs of it are
# bit-identical -- so a difference against the manuscript is a real difference
# and not noise.
# `recipe` names the methodology; every choice below is resolved through
# R/strategies.R from it. Left NULL it is the manuscript's, so every existing
# caller keeps its meaning. `ts_col`/`swc_col` still override the recipe for
# ad-hoc sensitivity runs. `fill` is the site's `fill_soil_temp()` result and
# is only read by a `memory_fill` recipe. `fit_profile` is documented at
# `fit_settings()`.
total_tas_site <- function(site_data, site_info, direct = FALSE,
                           ts_col = NULL, swc_col = NULL, fit = TRUE,
                           recipe = NULL, fill = NULL, fit_profile = "full") {
  recipe <- recipe %||% original_recipe()
  validate_recipe(recipe)
  fs <- fit_settings(fit_profile)

  a_measure_night_complete <- site_data[["nightNEE"]]
  ac <- site_data[["ac"]]
  feature_gs <- site_data[["feature_gs"]]

  name_site <- feature_gs[["site_ID"]]

  # Both arguments carry a site identity. A mismatched pair would fit one
  # site's data against another site's declared columns -- which runs, and is
  # wrong, and leaves no trace downstream.
  if (!identical(site_info[["site_ID"]], name_site)) {
    stop(
      "site_info is for ", site_info[["site_ID"]],
      " but site_data is for ", name_site, "."
    )
  }

  gStart <- feature_gs[["gStart"]]
  gEnd <- feature_gs[["gEnd"]]

  # Soil temperature: one call, one column out. Selection, the fill, the
  # bounds and the provenance all live in `get_soil_temperature()`; from here
  # on the tables carry `TS_final` and nothing else soil-temperature-shaped.
  # Made before the soil-water block because the fill's rows align with the
  # tables as step 01 left them, and the measured-soil-water filter below
  # drops nighttime rows.
  soil <- get_soil_temperature(site_data, site_info, recipe = recipe, fill = fill, ts_col = ts_col)
  ac <- soil[["ac"]]
  a_measure_night_complete <- soil[["nightNEE"]]
  ts_meta <- soil[["meta"]]
  ts_col <- ts_meta[["ts_col"]]
  tStart <- ts_meta[["tStart"]]
  tEnd <- ts_meta[["tEnd"]]

  # Soil water: choose a column, as with soil temperature. `prep_nee_ac()`
  # carries both `SWC_measured` and `SWC_era5` on every table, so no reading or
  # joining happens here.
  #
  # Unlike TS, the choice depends on the model as well as the site, which is
  # why it cannot be a single column in site_info.csv: the direct model needs
  # soil water, so a site with none measured falls back to ERA5-Land, while the
  # total model does not use soil water at all and therefore needs no fallback.
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

  # Measured soil water is a requirement where it exists: the original drops
  # nighttime observations that lack it. The ERA5 fallback is deliberately not
  # filtered on -- it is a daily reanalysis with its own gaps, and filtering on
  # it would discard observations the original kept.
  if (isTRUE(site_info[["SWC_use"]])) {
    a_measure_night_complete <- a_measure_night_complete |>
      dplyr::filter(!is.na(.data$SWC))
  }

  # The span the windows tile. Under `whole_year` only this changes: the
  # detected `gStart`/`gEnd` above still choose the control year, because that
  # choice needs a season to be defined over and a season-free rule for it is
  # documented, not implemented.
  win <- choose_window_season(recipe, feature_gs)
  wStart <- win[["gStart"]]
  wEnd <- win[["gEnd"]]

  # calculate daily daytime NEE and rolling average
  # Is the data 30 minute or hourly? TODO: Check this logic!
  dt <- if (ac$MINUTE[2] - ac$MINUTE[1] != 30) 60 else 30

  # unit is umol / m2 / s
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

  # A site whose growing year is wrapped loses one year at each end of the
  # record: the first growing year began before the data start and the last
  # runs past their end, so both are partial. Keyed off the declaration rather
  # than the two site names the original listed, so that a third such site
  # does not silently keep its partial years.
  # TODO: Move this logic out of here.
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

  # determine moving window size and number of windows
  if (dt == 30) {
    nobs_threshold <- 100
  } else {
    nobs_threshold <- 60
  }

  # use non-overlapping windows and determine number of windows for growing season; decide to use overlapping windows
  nwindow <- max(round((wEnd - wStart + 1) / WINDOW_SIZE), 1)

  # Every knob that decided the layout of this run, recorded alongside the
  # numbers it produced. `outcome` reports only TAS and two fit statistics, so
  # without this a difference between two runs cannot be attributed to the
  # column selected, the bounds, the control year or the window count.
  settings <- tibble::tibble(
    site_ID = name_site,
    recipe_id = recipe$recipe_id,
    model = if (direct) "direct" else "total",
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
    # Provenance: which strategy made each choice, and why. The
    # soil-temperature fields are `get_soil_temperature()`'s metadata row,
    # carried verbatim so the record and the column cannot disagree.
    ts_strategy = ts_meta[["ts_strategy"]],
    ts_reason = ts_meta[["ts_reason"]],
    ts_verdict = ts_meta[["ts_verdict"]],
    ts_flags = ts_meta[["ts_flags"]],
    fill_method = ts_meta[["fill_method"]],
    fill_cv_rmse = ts_meta[["fill_cv_rmse"]],
    fill_degenerate = ts_meta[["fill_degenerate"]],
    fill_truth_synthetic = ts_meta[["fill_truth_synthetic"]],
    ts_measured_synthetic = ts_meta[["ts_measured_synthetic"]],
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
    window_results[[window_name]] <- total_tas_window(
      ac, ac_day, a_measure_night_complete,
      window_start, window_end, nwindow, nobs_threshold, control_year,
      SWC_use, tStart, tEnd, wEnd,
      direct = direct, fit = fit, fs = fs
    )
  }

  # NB: a skipped window used to `return(NULL)`, and assigning NULL to a list
  # element *removes* it, so `length(window_results)` counted only the windows
  # that produced something. Skips are now records, so the emptiness test has
  # to ask the question directly or it would never fire.
  fitted_windows <- Filter(
    function(w) !is.null(w[["outcome_siteyear"]]), window_results
  )
  if (length(fitted_windows) == 0) {
    stop("No results produced, possibly because all windows were skipped.")
  }

  model_name <- if (direct) "direct" else "total"
  window_results_df <- window_results |>
    lapply(`[[`, "outcome_siteyear") |>
    dplyr::bind_rows() |>
    dplyr::mutate(site_ID = name_site, recipe_id = recipe$recipe_id, model = model_name,
                  .before = 1)

  window_skips <- window_results |>
    lapply(`[[`, "skipped") |>
    dplyr::bind_rows()
  if (nrow(window_skips)) {
    window_skips <- dplyr::mutate(window_skips, site_ID = name_site,
                                  recipe_id = recipe$recipe_id, model = model_name,
                                  .before = 1)
  }

  ER_obs_pred <- window_results |>
    lapply(`[[`, "ER_obs_pred") |>
    dplyr::bind_rows()

  if (!fit) {
    # No ERref, so no lnRatio, so nothing for `gls` to regress. `outcome` is
    # NULL rather than a row of NAs, so that a structure-only result cannot be
    # mistaken for a fitted one further downstream.
    return(list(
      outcome = NULL,
      outcome_siteyear = window_results_df,
      window_skips = window_skips,
      settings = settings
    ))
  }

  fit_stats <- caret::postResample(pred = ER_obs_pred$NEE_pred, obs = ER_obs_pred$NEE)

  mod_ar1 <- nlme::gls(
    lnRatio ~ TS + window,
    data = window_results_df,
    correlation = nlme::corAR1(form = ~ growing_year | window),
    na.action = na.omit
  )
  mod_ar1_smry <- summary(mod_ar1)[["tTable"]]

  outcome <- tibble::tibble(
    site_ID = name_site,
    recipe_id = recipe$recipe_id,
    model = if (direct) "direct" else "total",
    fit_profile = fs$profile,
    RMSE = fit_stats[["RMSE"]],
    R2 = fit_stats[["Rsquared"]],
    control_year = control_year,
    window_size = WINDOW_SIZE,
    nwindow = nwindow,
    TAS = mod_ar1_smry["TS", "Value"],
    TASp = mod_ar1_smry["TS", "p-value"]
  )

  list(
    outcome = outcome,
    outcome_siteyear = window_results_df,
    window_skips = window_skips,
    settings = settings
  )
}


# NOTE: Can refactor this further to remove window_start and window_end? Instead, just pass data directly?
total_tas_window <- function(
  ac, ac_day, a_measure_night_complete,
  window_start, window_end, nwindow, nobs_threshold, control_year,
  SWC_use, tStart, tEnd, gEnd,
  direct = FALSE, fit = TRUE, fs = fit_settings("full")
) {

  # A skipped window used to `return(NULL)`, which vanished without trace --
  # and, because assigning NULL to a list element *removes* it, without even
  # leaving a gap in `window_results`. Skips are now reported.
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

  year_results <- list()

  for (iyear in years) {
    data_subset <- model_data |>
      dplyr::filter(.data$growing_year == iyear)

    # two rules are needed:
    # rule 1: total number of points > 100.
    # rule 2: TSref is within the 0.025 and 0.975 quantiles.
    # if the two rules are violated, extend window size.
    TSref <- data_ref[["TS_final"]]
    extend_days <- 0

    check_subset <- function(data_subset, TSref, nobs_threshold) {
      ts_quants <- quantile(data_subset$TS_final, c(0.025, 0.975), na.rm = TRUE)
      (nrow(data_subset) < nobs_threshold || !dplyr::between(TSref, ts_quants[[1]], ts_quants[[2]]))
    }

    while (check_subset(data_subset, TSref, nobs_threshold)) {
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

    year_result <- Year_Result()

    year_result@nobsv <- nrow(data_subset)
    year_result@extend_days <- as.integer(extend_days)

    rejection <- year_rejection(data_subset, TSref)

    if (!is.na(rejection)) {
      year_result@status <- rejection
      year_results[[as.character(iyear)]] <- list(year_result = year_result, ER_obs_pred = NULL)
      next
    }

    if (!fit) {
      # Everything above this line is layout: window extension, the year
      # rejection rules, the observation count. Everything below is the model.
      year_result@status <- "not_fitted"
      year_result@TS <- ac_yearly_window |>
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
    year_result@alpha <- model_params[["alpha_Intercept"]]
    year_result@beta <- model_params[["beta_Intercept"]]
    year_result@C0 <- model_params[["C0_Intercept"]]

    if (direct) {
      year_result@k2 <- model_params[["k2_Intercept"]]
      year_result@Hs <- model_params[["Hs_Intercept"]]
    }

    year_result@TS <- ac_yearly_window |>
      dplyr::filter(.data$growing_year == iyear) |>
      dplyr::pull("TS")

    df_ERref <- fitted(mod, newdata = data_ref)
    if (df_ERref[, "Estimate"] >= df_ERref[, "Est.Error"]) {
      year_result@ERref <- df_ERref[, "Estimate"]
    }

    if (iyear == control_year) {
      ERref_control <- year_result@ERref
    }

    year_result@status <- "fitted"
    year_results[[as.character(iyear)]] <- list(year_result = year_result, ER_obs_pred = ER_obs_pred)
  } # end year loop

  df_site_year_window <- year_results |>
    lapply(`[[`, "year_result") |>
    lapply(year_result_df) |>
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

fit_with_retry <- function(data_subset, priors, direct = FALSE, fs = fit_settings("full")) {
  brm_args <- list(
    if (direct) BRM_FORMULA_DIRECT else BRM_FORMULA_TOTAL,
    prior = priors,
    data = data_subset,
    iter = fs$iter,
    cores = min(N_CORES, fs$chains),
    chains = fs$chains,
    backend = "cmdstanr",
    control = list(adapt_delta = 0.90, max_treedepth = 15),
    refresh = 0
  )
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
