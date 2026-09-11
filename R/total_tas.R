## testing

name_site <- "AU-Tum"
SWC_use <- get_site_info(name_site)[["SWC_use"]]
iwindow <- 1
iyear <- 2002

################################################################################

source("R/utils.R")

.data <- NULL

DIR_PROC <- file.path("data-proc")
DIR_RESP <- file.path(DIR_PROC, "respiration")
N_CORES <- 4        # Used internally by brms
WINDOW_SIZE <- 14   # Uniform window size: 2 weeks.

BRM_FORMULA <- brms::bf(
  NEE ~ exp(alpha * TS + beta * TS^2) * C0,
  alpha + beta + C0 ~ 1,
  nl = TRUE
)

# TODO: Move this into site_info.csv
site_TS_issue <- c("BE-Bra", "CA-Cbo", "CA-Gro", "CA-Mer", "CA-Obs", "CA-TP3", "CH-Lae", "DE-RuC", "DE-SfS", "FI-Sod",
                   "IT-Ren", "NL-Loo", "US-Bar", "US-BZB", "US-BZF", "US-BZS", "US-CMW", "US-GLE", "US-Ha2",
                   "US-IB2", "US-Jo2", "US-KL2", "US-Kon", "US-LL1", "US-MBP", "US-Myb", "US-NC4", "US-Tw1", "US-ICt",
                   "BE-Dor", "CA-TP4", "UK-AMo", "RU-Fyo", "ZA-Kru", "IT-Tor")

################################################################################

opt_int <- S7::new_property(S7::class_integer, default = NA_integer_)
opt_num <- S7::new_property(S7::class_numeric, default = NA_real_)

Year_Result <- S7::new_class("Year_Result", properties = list(
  nobsv = opt_int,
  extend_days = opt_int,
  alpha = opt_num,
  beta = opt_num,
  C0 = opt_num,
  Hs = opt_num,
  k2 = opt_num,
  TS = opt_num,
  ERref = opt_num,
  lnRatio = opt_num
))

################################################################################

adjust_prior <- function(priors, nlpar, p1, p2, family = "normal") {
  priors$prior[priors$nlpar == nlpar] <- sprintf("%s(%f, %f)", family, p1, p2)
  priors
}

get_priors <- function(model_data) {
  # Start with default prior
  priors <- brms::prior("normal(2, 5)", nlpar = "C0", lb = 0, ub = 10) +
    brms::prior("normal(0.1, 1)", nlpar = "alpha", lb = 0, ub = 0.2) +
    brms::prior("normal(-0.001, 0.1)", nlpar = "beta", lb = -0.01, ub = 0.0)

  # Next, attempt to update the prior using NLS fit.
  tryCatch(
    {
      mod_nls <- gslnls::gsl_nls(
        fn = NEE ~ exp(exp(alpha_ln) * TS - exp(beta_ln) * TS^2) * (exp(C0_ln)),
        data = model_data,
        start = c(C0_ln = 0.7, alpha_ln = -2.99, beta_ln = -6.9)
      )
      priors <- priors |>
        adjust_prior("alpha", min(exp(coefficients(mod_nls)["alpha_ln"]), 0.2), 1.0) |>
        adjust_prior("beta", -min(exp(coefficients(mod_nls)["beta_ln"]), 0.01), 0.1) |>
        adjust_prior("C0", min(exp(coefficients(mod_nls)["C0_ln"]), 10), 5)
    },
    error = function(e) {
      message("NLS fit failed with error: ", str(e), ".\n\nKeeping priors unchanged.")
    }
  )

  # Next, try to update the prior again using brms fit across all data.
  mod0 <- brms::brm(
    BRM_FORMULA,
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
    adjust_prior('alpha', brms::fixef(mod0)["alpha_Intercept", "Estimate"], 1.0) |>
    adjust_prior('beta', brms::fixef(mod0)["beta_Intercept", "Estimate"], 0.1) |>
    adjust_prior('C0', brms::fixef(mod0)["C0_Intercept", "Estimate"], 5)

  priors
}


total_tas_site <- function(name_site) {
  site_info <- get_site_info(name_site)

  a_measure_night_complete <- read.csv(file.path(
    DIR_RESP, name_site,
    paste0(name_site, "_nightNEE.csv")
  ))

  ac <- read.csv(file.path(
    DIR_RESP, name_site,
    paste0(name_site, "_ac.csv")
  ))

  feature_gs <- read.csv(file.path(DIR_PROC, "features", "growing_season_features.csv")) |>
    dplyr::filter(.data$site_ID == name_site)

  stopifnot(nrow(feature_gs) == 1)

  gStart <- feature_gs[["gStart"]]
  gEnd <- feature_gs[["gEnd"]]
  tStart <- feature_gs[["tStart"]]
  tEnd <- feature_gs[["tEnd"]]

  # TODO: Move this logic out of here
  if (site_info[["SWC_use"]]) {
    a_measure_night_complete <- a_measure_night_complete |>
      dplyr::filter(!is.na(.data$SWC))
  }

  # TODO: Move this logic out of here
  if (name_site %in% site_TS_issue) {
    if (name_site %in% c("US-Tw1")) {
      # slope will be too low if using ac data for the subtropical wetland sites.
      mod_lm <- lm(data = a_measure_night_complete, TS ~ TA, na.action = na.omit)
    } else {
      # using data with TA > 0, because we focus on grouping season
      mod_lm <- lm(data = ac[ac$TA > 0, ], TS ~ TA, na.action = na.omit)
    }
    TS_pred <- predict(mod_lm, newdata = data.frame(TA = a_measure_night_complete$TA), na.action = na.pass)
    a_measure_night_complete$TS[!is.na(TS_pred)] <- TS_pred[!is.na(TS_pred)]
    TS_pred <- predict(mod_lm, newdata = data.frame(TA = ac$TA), na.action = na.pass)
    ac$TS[!is.na(TS_pred)] <- TS_pred[!is.na(TS_pred)]
  }

  # calculate daily daytime NEE and rolling average
  # Is the data 30 minute or hourly? TODO: Check this logic!
  dt <- if (ac$MINUTE[2] - ac$MINUTE[1] != 30) 60 else 60 

  # unit is umol / m2 / s
  ac_day <- ac |>
    dplyr::filter(.data$daytime) |>
    dplyr::group_by(.data$YEAR, .data$DOY) |> 
    dplyr::summarise(
      NEE_daytime1 = max(-sum(.data$NEE_uStar_f, na.rm = TRUE) * dt * 60 / 86400, 0.0),
      .groups = "drop"
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
  ac_min_doy <- min(ac[["DOY"]])

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
      DOY <= 366 ~ YEAR,
      TRUE ~ YEAR - 1
    ))
  ac <- ac |>
    dplyr::mutate(growing_year = dplyr::case_when(
      DOY <= 366 ~ YEAR,
      TRUE ~ YEAR - 1
    ))

  # remove the growing_year with incomplete data
  # sites in southern hemisphere lose one year, because growing season crosses two years
  # TODO: Move this logic out of here.
  if (name_site %in% c('AU-Tum', 'ZA-Kru')) {
    a_measure_night_complete <- a_measure_night_complete |>
      dplyr::filter(dplyr::between(growing_year, ac$YEAR[1], ac$YEAR[nrow(ac)] - 1))
    ac <- ac |>
      dplyr::filter(dplyr::between(growing_year, ac$YEAR[1], ac$YEAR[nrow(ac)] - 1))
  }

  years <- sort(unique(a_measure_night_complete$growing_year))

  # decide control year: the year with growing-season TS closest to long-term mean. 
  ac_yearly_gs <- ac |>
    dplyr::filter(dplyr::between(.data$DOY, gStart, gEnd)) |>
    dplyr::filter(.data$growing_year %in% years) |>
    dplyr::group_by(.data$growing_year) |>
    dplyr::summarise(TS = mean(TS, na.rm = TRUE), .groups = "drop_last") |>
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

  for (iwindow in seq_len(nwindow)) {
    window_start <- gStart + WINDOW_SIZE * (iwindow - 1)
    window_end <- min(gStart + WINDOW_SIZE * iwindow, gEnd)
    window_result <- total_tas_window(
      ac, ac_day, a_measure_night_complete,
      window_start, window_end, nwindow,
      site_info[["SWC_use"]], tStart, tEnd
    )
  }
}


# NOTE: Can refactor this further to remove window_start and window_end? Instead, just pass data directly?
total_tas_window <- function(
  ac, ac_day, a_measure_night_complete,
  window_start, window_end, nwindow,
  SWC_use, tStart, tEnd
) {

  ac_yearly_window <- ac |>
    dplyr::filter(dplyr::between(.data$DOY, window_start, window_end)) |>
    dplyr::group_by(.data$growing_year) |>
    dplyr::summarise(TS = mean(.data$TS, na.rm=TRUE), .groups = "drop_last") |>
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
    message("Skipping because too few values in window.")
    return(NULL)
  }

  # get reference temperature, SWC, and NEEday of each window.
  # This is used to fit the data later.
  TSref <- mean(ac$TS[dplyr::between(ac$DOY, window_start, window_end)], na.rm = TRUE)
  NEEdayref <- mean(
    ac_day$NEE_daytime[dplyr::between(ac_day$DOY, window_start, window_end)],
    na.rm = TRUE
  )
  if (SWC_use) {
    SWCref <- mean(
      ac$SWC[dplyr::between(ac$DOY, window_start, window_end)],
      na.rm = TRUE
    )
    data_ref <- data.frame(TS = TSref, NEE_daytime = NEEdayref, SWC = SWCref)
  } else {
    data_ref <- data.frame(TS = TSref, NEE_daytime = NEEdayref)
  }

  priors <- get_priors(model_data)

  ERref_control <- NA

  years <- sort(unique(a_measure_night_complete[["growing_year"]]))

  year_results <- list()

  for (iyear in years) {
    year_results[[as.character(iyear)]] <- fit_tas_year(iyear, model_data, a_measure_night_complete)
  }

}


fit_tas_year <- function(iyear, model_data, a_measure_night_complete) {

  data_subset <- model_data |>
    dplyr::filter(.data$growing_year == iyear)

  year_result <- Year_Result()

  # two rules are needed:
  # rule 1: total number of points > 100. 
  # rule 2: TSref is within the 0.025 and 0.975 quantiles. 
  # if the two rules are violated, extend window size.
  extend_days <- 0
  ts_quants <- quantile(data_subset$TS, c(0.025, 0.975), na.rm = TRUE)

  while (nrow(data_subset) < nobs_threshold || !dplyr::between(TSref, ts_quants[[1]], ts_quants[[2]])) {
    extend_days <- extend_days + 3
    data_subset <- a_measure_night_complete |>
      dplyr::filter(growing_year == iyear) |> 
      dplyr::filter(dplyr::between(.data$DOY, window_start - extend_days, window_end + extend_days))
    # some conditions to break out to avoid dead loop
    if (nrow(data_subset) >= nobs_threshold && (window_end + extend_days) >= gEnd && TSref >= max(data_subset$TS, na.rm=T)) {
      break
    }
    if (extend_days >= 24) { # max window size: two months
      break
    }
  }

  year_result@nobsv <- nrow(data_subset)
  year_result@extend_days <- as.integer(extend_days)

  # ensure enough observations
  if (nrow(data_subset) <= 25) {
    return(list(year_result = year_result, ER_obs_pred = NULL))
  }

  # ensure enough temperature range
  if (!dplyr::between(TSref, ts_quants[[1]], ts_quants[[2]])) {
    return(list(year_result = year_result, ER_obs_pred = NULL))
  }

  # ensure nighttime NEE is positive
  if (median(data_subset$NEE) < 0.2 || mean(data_subset$NEE) < 0.2) {
    return(list(year_result = year_result, ER_obs_pred = NULL))
  }

  mod <- fit_with_retry(data_subset, priors)

  # Extract model results
  ER_obs_pred <- data_subset |>
    dplyr::mutate(NEE_pred = fitted(mod)[, "Estimate"]) |>
    dplyr::filter(dplyr::between(.data$DOY, window_start, window_end))

  model_params <- brms::fixef(mod)[, "Estimate"]
  year_result@alpha <- model_params[["alpha_Intercept"]]
  year_result@beta <- model_params[["beta_Intercept"]]
  year_result@C0 <- model_params[["C0_Intercept"]]

  year_result@TS <- ac_yearly_window |>
    dplyr::filter(.data$growing_year == iyear) |>
    dplyr::pull("TS")

  df_ERref <- fitted(mod, newdata = data_ref)
  if (df_ERref[, "Estimate"] < df_ERref[, "Est.Error"]) {
    year_result@ERref <- df_ERref[, "Estimate"]
  }

  list(year_result = year_result, ER_obs_pred = ER_obs_pred)
}



fit_with_retry <- function(data_subset, priors) {
  brm_args <- list(
    BRM_FORMULA,
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
