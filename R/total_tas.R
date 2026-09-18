N_CORES <- 4        # Used internally by brms
WINDOW_SIZE <- 14   # Uniform window size: 2 weeks.

BRM_FORMULA_TOTAL <- brms::bf(
  NEE ~ exp(alpha * TS + beta * TS^2) * C0,
  alpha + beta + C0 ~ 1,
  nl = TRUE
)

BRM_FORMULA_DIRECT <- brms::bf(
  NEE ~ exp(alpha * TS + beta*TS^2) * SWC / (Hs + SWC) * (C0 + NEE_daytime * k2),
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

Year_Result <- S7::new_class("Year_Result", properties = list(
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

year_result_df <- S7::new_generic("year_result_df", "year_result")
S7::method(year_result_df, Year_Result) <- function(year_result) {
  tibble::tibble(!!!S7::props(year_result))
}

################################################################################

adjust_prior <- function(priors, nlpar, p1, p2, family = "normal") {
  priors$prior[priors$nlpar == nlpar] <- sprintf("%s(%f, %f)", family, p1, p2)
  priors
}

get_priors <- function(model_data, direct = FALSE) {
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
    frmu_nls <- NEE ~ exp(exp(alpha_ln) * TS - exp(beta_ln)*TS^2) * 
      SWC / (exp(Hs_ln) + SWC) * (exp(C0_ln) + NEE_daytime * exp(k2_ln))
    start_nls <- c(C0_ln = 0.7, alpha_ln = -2.99, beta_ln = -6.9, k2_ln = -1.6, Hs_ln = 2.3)
  } else {
    frmu_nls <- NEE ~ exp(exp(alpha_ln) * TS - exp(beta_ln) * TS^2) * (exp(C0_ln))
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
    iter = 2000,
    cores = N_CORES,
    chains = 4,
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
total_tas_site <- function(site_data, site_info, direct = FALSE,
                           ts_col = NULL, swc_col = NULL) {
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

  # Soil water: choose a column, as with soil temperature. `prep_nee_ac()`
  # carries both `SWC_measured` and `SWC_era5` on every table, so no reading or
  # joining happens here.
  #
  # Unlike TS, the choice depends on the model as well as the site, which is
  # why it cannot be a single column in site_info.csv: the direct model needs
  # soil water, so a site with none measured falls back to ERA5-Land, while the
  # total model does not use soil water at all and therefore needs no fallback.
  swc_col <- swc_col %||% default_swc_col(site_info, direct)
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

  # Soil temperature: choose a column, do not compute one. `prep_nee_ac()`
  # produced every variant this site offers along with the growing-season
  # bounds belonging to each, so the column and its bounds are selected
  # together and cannot disagree.
  if (is.null(site_data[["ts_bounds"]])) {
    stop(
      name_site, ": this site_data was built before soil-temperature columns ",
      "were carried explicitly, so it has no `ts_bounds`. Rebuild it with ",
      "`prep_nee_ac()`, or delete the stale `_targets/` store."
    )
  }

  ts_col <- ts_col %||% site_info[["ts_col"]]
  ac <- resolve_ts_column(ac, ts_col)
  a_measure_night_complete <- resolve_ts_column(a_measure_night_complete, ts_col)
  ts_range <- ts_bounds_for(site_data[["ts_bounds"]], ts_col)
  tStart <- ts_range[["tStart"]]
  tEnd <- ts_range[["tEnd"]]

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

  # Deal with sites in southern hemisphere
  # add growing_year
  # TODO: Will this be a problem in leap years?
  a_measure_night_complete <- a_measure_night_complete |>
    dplyr::mutate(growing_year = dplyr::case_when(
      .data$DOY <= 366 ~ .data$YEAR,
      TRUE ~ .data$YEAR - 1
    ))
  ac <- ac |>
    dplyr::mutate(growing_year = dplyr::case_when(
      .data$DOY <= 366 ~ .data$YEAR,
      TRUE ~ .data$YEAR - 1
    ))

  # remove the growing_year with incomplete data
  # sites in southern hemisphere lose one year, because growing season crosses two years
  # TODO: Move this logic out of here.
  if (name_site %in% c("AU-Tum", "ZA-Kru")) {
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
    dplyr::summarise(TS = mean(.data$TS, na.rm = TRUE), .groups = "drop_last") |>
    dplyr::ungroup()

  control_year <- ac_yearly_gs$growing_year[which.min(abs(ac_yearly_gs$TS - mean(ac_yearly_gs$TS)))]

  # determine moving window size and number of windows
  if (dt == 30) {
    nobs_threshold <- 100
  } else {
    nobs_threshold <- 60
  }

  # use non-overlapping windows and determine number of windows for growing season; decide to use overlapping windows
  nwindow <- max(round((gEnd - gStart + 1) / WINDOW_SIZE), 1)

  window_results <- list()
  for (iwindow in seq_len(nwindow)) {
    message("########################################")
    message(iwindow, " of ", nwindow)
    window_start <- gStart + WINDOW_SIZE * (iwindow - 1)
    window_end <- min(gStart + WINDOW_SIZE * iwindow, gEnd)
    window_name <- paste(window_start, window_end, sep = "_")
    window_results[[window_name]] <- total_tas_window(
      ac, ac_day, a_measure_night_complete,
      window_start, window_end, nwindow, nobs_threshold, control_year,
      SWC_use, tStart, tEnd, gEnd,
      direct = direct
    )
  }

  if (length(window_results) == 0) {
    stop("No results produced, possibly because all windows were skipped.")
  }

  window_results_df <- window_results |>
    lapply(`[[`, "outcome_siteyear") |>
    dplyr::bind_rows() |>
    dplyr::mutate(site_ID = name_site, .before = 1)

  ER_obs_pred <- window_results |>
    lapply(`[[`, "ER_obs_pred") |>
    dplyr::bind_rows()

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
    RMSE = fit_stats[["RMSE"]],
    R2 = fit_stats[["Rsquared"]],
    control_year = control_year,
    window_size = WINDOW_SIZE,
    nwindow = nwindow,
    TAS = mod_ar1_smry["TS", "Value"],
    TASp = mod_ar1_smry["TS", "p-value"]
  )

  outcome
}


# NOTE: Can refactor this further to remove window_start and window_end? Instead, just pass data directly?
total_tas_window <- function(
  ac, ac_day, a_measure_night_complete,
  window_start, window_end, nwindow, nobs_threshold, control_year,
  SWC_use, tStart, tEnd, gEnd,
  direct = FALSE
) {

  ac_yearly_window <- ac |>
    dplyr::filter(dplyr::between(.data$DOY, window_start, window_end)) |>
    dplyr::group_by(.data$growing_year) |>
    dplyr::summarise(TS = mean(.data$TS, na.rm = TRUE), .groups = "drop_last") |>
    dplyr::ungroup()

  skip <- (!dplyr::between(
    mean(ac_yearly_window$TS, na.rm = TRUE),
    max(tStart, 2.0),
    tEnd
  ) && nwindow > 1)

  if (skip) {
    message("Skipping because TS is not in valid range.")
    return(NULL)
  }

  model_data <- a_measure_night_complete |>
    dplyr::filter(dplyr::between(.data$DOY, window_start, window_end))

  if (nrow(model_data) < 100) {
    message("Skipping because too few values in window (", nrow(model_data), ").")
    return(NULL)
  }

  # get reference temperature, SWC, and NEEday of each window.
  # This is used to fit the data later.
  keep <- function(dat) dplyr::between(dat$DOY, window_start, window_end)
  data_ref <- data.frame(
    TS = mean(ac$TS[keep(ac)], na.rm = TRUE),
    NEE_daytime = mean(ac_day$NEE_daytime[keep(ac_day)], na.rm = TRUE)
  )
  if (SWC_use) {
    data_ref[["SWC"]] <- mean(ac$SWC[keep(ac)], na.rm = TRUE)
  }

  priors <- get_priors(model_data, direct = direct)

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
    TSref <- data_ref[["TS"]]
    extend_days <- 0

    check_subset <- function(data_subset, TSref, nobs_threshold) {
      ts_quants <- quantile(data_subset$TS, c(0.025, 0.975), na.rm = TRUE)
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
          (TSref >= max(data_subset$TS, na.rm = TRUE))
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

    # ensure enough observations
    ts_quants <- quantile(data_subset$TS, c(0.025, 0.975), na.rm = TRUE)
    next_condition <- (
      # ensure enough observations
      (nrow(data_subset) <= 25) ||
        # ensure enough temperature range
        (!dplyr::between(TSref, ts_quants[[1]], ts_quants[[2]])) ||
        # ensure nighttime NEE is positive
        (median(data_subset$NEE) < 0.2) ||
        (mean(data_subset$NEE) < 0.2)
    )

    if (next_condition) {
      year_results[[as.character(iyear)]] <- list(year_result = year_result, ER_obs_pred = NULL)
      next
    }

    mod <- fit_with_retry(data_subset, priors, direct)

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

  list(outcome_siteyear = df_site_year_window, ER_obs_pred = ER_obs_pred_all)
}

fit_with_retry <- function(data_subset, priors, direct = FALSE) {
  brm_args <- list(
    if (direct) BRM_FORMULA_DIRECT else BRM_FORMULA_TOTAL,
    prior = priors,
    data = data_subset,
    iter = 1000,
    cores = N_CORES,
    chains = 4,
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
  if (failed_brm) {
    brm_args2 <- modifyList(brm_args, list(
      iter = 4000,
      control = list(adapt_delta = 0.98, max_treedepth = 15)
    ))
    mod <- do.call(function(...) try(brms::brm(...)), brm_args2)
  }

  mod
}
