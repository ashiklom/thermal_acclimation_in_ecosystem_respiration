# Workflow step 02: thermal acclimation strength (TAS) of nighttime ecosystem
# respiration at one site, estimated with moving windows (Junna Wang, 2025).
#
# estimate_tas() estimates either TAS type for one site: "total" (02_01a;
# respiration as a function of soil temperature only) or "direct" (02_01b;
# soil temperature plus soil water and the preceding days' daytime uptake),
# which differ only in the respiration model (tas_model()). It has no
# site-specific logic: it takes the inputs prepared by prepare_tas_inputs()
# (R/step02-prep.R) as arguments, and returns list(outcome = <one row of
# outcome_*.csv>, siteyear = <the site's rows of outcome_siteyear_*.csv>,
# fit_profile).
# combine_tas_sites() binds per-site results in site_info order into the
# published output tables.

# Priors of the respiration models. Their means are re-centred window by
# window, so they are passed to Stan as data (see tas_stanvars()) instead of
# being written into the model code as numbers: the Stan program is then the
# same for every window, year and site, and is compiled once.
priors_temp <- brms::prior("normal(C0_mu, C0_sd)", nlpar = "C0", lb = 0, ub = 10) +
  brms::prior("normal(alpha_mu, alpha_sd)", nlpar = "alpha", lb = 0, ub = 0.2) +
  brms::prior("normal(beta_mu, beta_sd)", nlpar = "beta", lb = -0.01, ub = 0.0)

priors_water <- brms::prior("normal(Hs_mu, Hs_sd)", nlpar = "Hs", lb = 0, ub = 1000)

priors_gpp <- brms::prior("normal(k2_mu, k2_sd)", nlpar = "k2", lb = 0, ub = 10)

# brms's default prior for the residual SD, student_t(3, 0, s), writes a
# data-dependent s into the model code; pass s as data as well.
prior_sigma <- brms::prior("student_t(3, 0, sigma_scale)", class = "sigma")

# Initial prior means and SDs. The SDs never change; the means are replaced by
# the gsl_nls and pooled-window estimates.
prior_defaults <- c(C0_mu = 2, C0_sd = 5, alpha_mu = 0.1, alpha_sd = 1, beta_mu = -0.001, beta_sd = 0.1,
                    Hs_mu = 10, Hs_sd = 10, k2_mu = 0.5, k2_sd = 2)

# The prior values of the parameters in `priors`, from `prior_values`, plus
# brms's default residual-SD scale for `formula` and `data`, as stanvars.
tas_stanvars <- function(prior_values, priors, formula, data) {
  names <- as.vector(outer(c("_mu", "_sd"), priors$nlpar[nzchar(priors$nlpar)], function(suffix, par) paste0(par, suffix)))
  values <- c(prior_values[names], sigma_scale = default_sigma_scale(formula, data))
  Reduce(`+`, Map(function(value, name) brms::stanvar(value, name = name), unname(values), names(values)))
}

# The scale brms would use for its default student_t(3, 0, scale) prior on
# the residual SD of `formula` fitted to `data`.
default_sigma_scale <- function(formula, data) {
  dp <- brms::default_prior(formula, data = data)
  scale <- sub("^student_t\\(3, 0, (.*)\\)$", "\\1", dp$prior[dp$class == "sigma" & !nzchar(dp$coef)])
  stopifnot(length(scale) == 1)
  as.numeric(scale)
}

# Sampler settings. "full" (the default) is the published setting; "fast"
# (TAS_FIT_PROFILE=fast) is only for checking that the code runs end to end:
# its TAS values are not results.
tas_fit_settings <- function(profile = Sys.getenv("TAS_FIT_PROFILE", "full")) {
  switch(profile,
    full = list(profile = "full", chains = 4, cores = 4, pooled_iter = 2000, year_iter = 1000, retry = TRUE, retry_iter = 4000),
    fast = list(profile = "fast", chains = 2, cores = 2, pooled_iter = 400, year_iter = 400, retry = FALSE, retry_iter = 400),
    stop("unknown TAS_FIT_PROFILE: ", profile)
  )
}

# Sites to use for small local runs: together they exercise every
# site-specific code path of steps 01-02 (southern-hemisphere wrap and hourly
# data: AU-Tum; AmeriFlux with measured soil water: US-Uaf; TS ~ TA
# regression, TS >= 2 floor and ERA5 soil water: US-GLE; estimated soil
# temperature and TS >= 2 floor: CH-Dav; TS ~ TA regression and the NEE < 0
# season rule: DE-RuC; the night-data regression domain: US-Tw1;
# random-forest soil temperature and PI soil water: FR-Pue).
DEV_SITES <- c("AU-Tum", "US-Uaf", "US-GLE", "CH-Dav", "DE-RuC", "US-Tw1", "FR-Pue")

# A reproducible seed for one brm() fit, derived from what identifies the fit
# (site, model, window, year or "pooled", retry), so a fit's result does not
# depend on which fits ran before it or in which process.
tas_seed <- function(...) {
  key <- paste(..., sep = "|")
  strtoi(substr(digest::digest(key, algo = "xxhash32", serialize = FALSE), 1, 7), 16L)
}

# Compile each model's Stan program once per machine. cmdstanr keys compiled
# programs by a hash of their code in this directory, so once a program is
# compiled every later brm() call, in this or any other R process, reuses the
# executable (the code no longer depends on the data or the priors).
use_stan_cache <- function(dir = path_proc("stan-cache")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  options(cmdstanr_write_stan_file_dir = normalizePath(dir))
}

# Compile the total and direct models (brm() without sampling) into the
# cache. Run this once before fitting sites in parallel processes, so they
# never compile the same program at the same time.
compile_tas_models <- function() {
  data <- data.frame(NEE = c(1, 2, 3, 2), TS = c(5, 10, 15, 20), SWC = c(20, 25, 30, 35), NEE_daytime = c(1, 2, 3, 4))
  models <- list(tas_model("total"), tas_model("direct", swc = TRUE))
  for (m in models) {
    brms::brm(brms::bf(m$frmu, m$param, nl = TRUE), prior = m$priors + prior_sigma, data = data,
              stanvars = tas_stanvars(prior_defaults, m$priors, brms::bf(m$frmu, m$param, nl = TRUE), data),
              chains = 0, backend = "cmdstanr")
  }
  invisible(NULL)
}

new_outcome <- function() {
  data.frame(site_ID = character(), RMSE = double(), R2 = double(), control_year = double(), window_size = integer(),
             nwindow = integer(), TAS = double(), TASp = double(), TAS_se = double())
}

# The respiration model of each TAS type, as brms and gsl_nls formulas with
# their priors and starting values. The total model relates nighttime NEE to
# soil temperature only; the direct model also to soil water (where it is
# available) and to the preceding days' daytime uptake. `pars` are the
# nonlinear parameters whose prior means are re-centred window by window.
tas_model <- function(model_type = c("direct", "total"), swc = TRUE) {
  model_type <- match.arg(model_type)
  # The priors below are combined with brms's `+` method, which only exists
  # once brms is loaded (not yet the case in a fresh pipeline worker).
  loadNamespace("brms")
  if (model_type == "total") {
    list(
      # for brm models
      frmu = NEE ~ exp(alpha * TS + beta*TS^2) * C0,
      param = alpha+beta+C0 ~ 1,
      priors = priors_temp,
      # for nls models
      # alpha and CO are both positive and beta is negative
      frmu_nls = NEE ~ exp(exp(alpha_ln) * TS - exp(beta_ln)*TS^2) * (exp(C0_ln)),
      stprm = c(C0_ln = 0.7, alpha_ln = -2.99, beta_ln = -6.9),
      pars = c("alpha", "beta", "C0")
    )
  } else if (swc) {
    list(
      frmu = NEE ~ exp(alpha * TS + beta*TS^2) * SWC / (Hs + SWC) * (C0 + NEE_daytime * k2),
      param = alpha+beta+C0+Hs+k2 ~ 1,
      priors = priors_temp + priors_water + priors_gpp,
      # alpha, CO, k2, and Hs are all positive and beta are negative
      frmu_nls = NEE ~ exp(exp(alpha_ln) * TS - exp(beta_ln)*TS^2) * SWC / (exp(Hs_ln) + SWC) * (exp(C0_ln) + NEE_daytime * exp(k2_ln)),
      stprm = c(C0_ln = 0.7, alpha_ln = -2.99, beta_ln = -6.9, k2_ln = -1.6, Hs_ln = 2.3),
      pars = c("alpha", "beta", "C0", "k2", "Hs")
    )
  } else {
    list(
      frmu = NEE ~ exp(alpha * TS + beta*TS^2) * (C0 + NEE_daytime * k2),
      param = alpha+beta+C0+k2 ~ 1,
      priors = priors_temp + priors_gpp,
      frmu_nls = NEE ~ exp(exp(alpha_ln) * TS - exp(beta_ln)*TS^2) * (exp(C0_ln) + NEE_daytime * exp(k2_ln)),
      stprm = c(C0_ln = 0.7, alpha_ln = -2.99, beta_ln = -6.9, k2_ln = -1.6),
      pars = c("alpha", "beta", "C0", "k2")
    )
  }
}

# Prior mean for `par` from the log-scale gsl_nls estimates, capped at the
# prior's bound (beta is negative).
nls_prior_mean <- function(coefs, par) {
  switch(par,
    alpha = min(exp(coefs["alpha_ln"]), 0.2),
    beta = -min(exp(coefs["beta_ln"]), 0.01),
    C0 = min(exp(coefs["C0_ln"]), 10),
    k2 = min(exp(coefs["k2_ln"]), 10),
    Hs = min(exp(coefs["Hs_ln"]), 1000)
  )
}

estimate_tas <- function(site_id, night, ac, ac_day, gStart, gEnd, tStart, tEnd, nobs_threshold, control_year,
                         model_type = c("direct", "total"), window_size = 14, sampler = tas_fit_settings()) {
  model_type <- match.arg(model_type)
  outcome <- new_outcome()
  name_site <- site_id
  a_measure_night_complete <- night

  # every variable of the model must be complete in the nighttime data (rows
  # without them are removed when the inputs are prepared)
  model_vars <- c("NEE", "TS", if (model_type == "direct") c("SWC", "NEE_daytime"))
  incomplete <- vapply(model_vars, function(v) anyNA(a_measure_night_complete[[v]]), logical(1))
  if (any(incomplete)) stop(site_id, ": missing values in ", paste(model_vars[incomplete], collapse = ", "))

  years <- sort(unique(a_measure_night_complete$growing_year))

  # use non-overlapping windows and determine number of windows for growing season:
  # window k covers days gStart + window_size*(k-1) to gStart + window_size*k - 1
  # (at most gEnd)
  nwindow <- max(round((gEnd - gStart + 1) / window_size), 1)

  #------------------------------------PREPARE FOR formula, stprm, priors of ER models---------------------------------
  model <- tas_model(model_type, swc = TRUE)
  frmu <- model$frmu
  param <- model$param
  priors <- model$priors
  prior_values <- prior_defaults
  frmu_nls <- model$frmu_nls
  stprm <- model$stprm

  df_site_year_window <- data.frame(site_ID = character(), growing_year = integer(), window = character(), nobsv = integer(), extend_days = integer(),
                                    alpha = double(), beta = double(), C0 = double(), Hs = double(), k2 = double(), TS = double(), ERref = double(), lnRatio = double())
  ER_obs_pred <- data.frame()

#------------------------------------FIT ER MODELS FOR EACH YEAR AND WINDOWS WITH BRM METHOD---------------------------------
  icount = 0
  for (iwindow in 1:nwindow) {
    window_start = gStart + window_size*(iwindow-1)
    window_end = min(gStart + window_size*iwindow - 1, gEnd)

    ac_yearly_window <- ac |> dplyr::filter(dplyr::between(.data$DOY, .env$window_start, .env$window_end)) |> dplyr::group_by(.data$growing_year) |>
      dplyr::summarise(TS=mean(.data$TS, na.rm=T)) |> dplyr::arrange(.data$growing_year)
    if (!dplyr::between(mean(ac_yearly_window$TS, na.rm=T), max(tStart, 2.0), tEnd) & nwindow > 1) {
      next
    }

    data <- a_measure_night_complete |> dplyr::filter(dplyr::between(.data$DOY, .env$window_start, .env$window_end))
    # skip a window if no enough data; this is for sites ('US-ICt', 'US-ICh', 'US-ICs', 'FI-Sod') with a period of the whole day is daytime.
    if (nrow(data) < 100) { next }

    # try gsl_nls first because it is fast, and then update priors of brm models based on mod_nls
    # this can give abnormal initial values.
    mod_nls <- try(gslnls::gsl_nls(fn=as.formula(frmu_nls), data=data, start=stprm))
    if (!inherits(mod_nls, "try-error")) {
      for (par in model$pars) {
        prior_values[paste0(par, "_mu")] <- nls_prior_mean(coefficients(mod_nls), par)
      }
    }

    # call the brm model to estimate parameters; this step takes much longer time.
    mod0 <- try(brms::brm(brms::bf(frmu, param, nl = TRUE),
                      prior = priors + prior_sigma, data = data,
                      stanvars = tas_stanvars(prior_values, priors, brms::bf(frmu, param, nl = TRUE), data), seed = tas_seed(name_site, model_type, window_start, window_end, "pooled"), iter = sampler$pooled_iter, cores = sampler$cores, chains = sampler$chains, backend = "cmdstanr",
                      control = list(adapt_delta = 0.95, max_treedepth = 15), refresh = 0))
    # if the pooled fit fails there are no priors for the years: skip the window
    if (inherits(mod0, "try-error")) { next }

    # use this result as prior of each year
    for (par in model$pars) {
      prior_values[paste0(par, "_mu")] <- brms::fixef(mod0)[paste0(par, "_Intercept"), "Estimate"]
    }

    # get reference temperature, SWC, and NEEday of each window
    TSref <- mean(ac$TS[dplyr::between(ac$DOY, window_start, window_end)], na.rm=T)
    NEEdayref <- mean(ac_day$NEE_daytime[dplyr::between(ac_day$DOY, window_start, window_end)], na.rm=T)
    if (model_type == "direct") {
      SWCref <- mean(ac$SWC[dplyr::between(ac$DOY, window_start, window_end)], na.rm=T)
      data_ref <- data.frame(TS=TSref, NEE_daytime=NEEdayref, SWC=SWCref)
    } else {
      data_ref <- data.frame(TS=TSref, NEE_daytime=NEEdayref)
    }

    #----loop through each year----
    ERref_control <- NA
    for (iyear in years) {
      print(paste(name_site, iwindow, iyear, sep='_'))
      icount = icount + 1
      df_site_year_window[icount, "site_ID"] <- name_site
      df_site_year_window$growing_year[icount] <- iyear
      df_site_year_window$window[icount] <- paste(window_start, window_end, sep='_')

      # determine if there are enough data for the regression of each year
      data_subset <- data[data$growing_year == iyear, ]

      # two rules are needed:
      # rule 1: total number of points > 100.
      # rule 2: TSref is within the 0.025 and 0.975 quantiles.
      # if the two rules are violated, extend window size.
      extend_days <- 0
      while(nrow(data_subset) < nobs_threshold | !dplyr::between(TSref, quantile(data_subset$TS, 0.025, na.rm=T), quantile(data_subset$TS, 0.975, na.rm=T))) {
        extend_days <- extend_days + 3
        data_subset <- a_measure_night_complete |> dplyr::filter(.data$growing_year == .env$iyear) |>
          dplyr::filter(dplyr::between(.data$DOY, .env$window_start - .env$extend_days, .env$window_end + .env$extend_days))
        # some conditions to break out to avoid dead loop
        if (nrow(data_subset) >= nobs_threshold & (window_end + extend_days) >= gEnd & TSref >= max(data_subset$TS, na.rm=T)) {break}
        if (extend_days >= 24) { break }   # max window size: two months
      }
      df_site_year_window[icount, "nobsv"] <- nrow(data_subset)
      df_site_year_window[icount, "extend_days"] <- extend_days

      # ensure enough observations
      if (nrow(data_subset) <= 25) { next }   # this is mainly for US-Uaf, which has few data

      # ensure enough temperature range
      if (!dplyr::between(TSref, quantile(data_subset$TS, 0.025, na.rm=T), quantile(data_subset$TS, 0.975, na.rm=T))) { next }

      # ensure nighttime NEE is positive
      if (median(data_subset$NEE) < 0.2 | mean(data_subset$NEE) < 0.2) { next }

      mod <- try(brms::brm(brms::bf(frmu, param, nl = TRUE),
                       prior = priors + prior_sigma, data = data_subset,
                       stanvars = tas_stanvars(prior_values, priors, brms::bf(frmu, param, nl = TRUE), data_subset), seed = tas_seed(name_site, model_type, window_start, window_end, iyear), iter = sampler$year_iter, cores = sampler$cores, chains = sampler$chains, backend = "cmdstanr",
                       control = list(adapt_delta = 0.90, max_treedepth = 15), refresh = 0)) # , silent = 2

      if (!inherits(mod, "try-error")) {
        np <- brms::nuts_params(mod)
        n_divergent <- sum(subset(np, Parameter == "divergent__")$Value)
        failed_brm <- n_divergent > 0
      } else {
        failed_brm <- TRUE
      }

      # if brm models fail or have divergent transitions, try another time
      if (failed_brm && sampler$retry) {
        mod <- try(brms::brm(brms::bf(frmu, param, nl = TRUE),
                             prior = priors + prior_sigma, data = data_subset,
                             stanvars = tas_stanvars(prior_values, priors, brms::bf(frmu, param, nl = TRUE), data_subset), seed = tas_seed(name_site, model_type, window_start, window_end, iyear, "retry"), iter = sampler$retry_iter, cores = sampler$cores, chains = sampler$chains, backend = "cmdstanr",
                             control = list(adapt_delta = 0.98, max_treedepth = 15), refresh = 0)) # , silent = 2
      }
      # both attempts failed: leave this year without estimates
      if (inherits(mod, "try-error")) { next }

      # extract model results
      data_subset$NEE_pred <- fitted(mod)[, "Estimate"]
      ER_obs_pred <- rbind(ER_obs_pred, data_subset[dplyr::between(data_subset$DOY, window_start, window_end), ])


      # model parameters
      df_site_year_window[icount, sub("_Intercept$", "", names(brms::fixef(mod)[, "Estimate"]))] <- brms::fixef(mod)[, "Estimate"]

      # average TS of moving window at each year
      df_site_year_window$TS[icount] <- ac_yearly_window$TS[ac_yearly_window$growing_year == iyear]

      # ER at reference temperature, water, and NEEday conditions
      df_ERref <- fitted(mod, newdata=data_ref)
      df_site_year_window$ERref[icount] <- ifelse(df_ERref[, "Estimate"] <  df_ERref[, "Est.Error"], NA, df_ERref[, "Estimate"])

      if (iyear == control_year) {
        ERref_control <- df_site_year_window$ERref[icount]
      }
    }
    # end of each year loop

    # if no data in a control year during this window, use average ER across years as the reference conditions
    if (is.na(ERref_control)) {
      irow_site_window <- which(df_site_year_window$site_ID == name_site & df_site_year_window$window == paste(window_start, window_end, sep='_'))
      ERref_control <- mean(df_site_year_window$ERref[irow_site_window], na.rm=T)
    }

    df_site_year_window$lnRatio[(icount-length(years) + 1):icount] <- log(df_site_year_window$ERref[(icount-length(years) + 1):icount] / ERref_control)

    # remove unrealistic extreme values due to potentially large gaps; this only affects a few sites
    x <- df_site_year_window$lnRatio[(icount-length(years) + 1):icount]
    outlier <- boxplot.stats(x, coef = 3)$out
    id.remove <- match(outlier[abs(outlier) > 1.5], x)
    if (length(id.remove) > 0) {
      df_site_year_window <- df_site_year_window[-(icount - length(years) + id.remove), ]
      icount <- icount - length(id.remove)
    }

  }
  # end of each window

  outcome[1, "site_ID"] <- name_site
  outcome[1, c("RMSE", "R2")] <- caret::postResample(pred = ER_obs_pred$NEE_pred, obs = ER_obs_pred$NEE)[1:2]
  outcome[1, c("control_year", "window_size", "nwindow")] <- c(control_year, window_size, nwindow)

  # take account of potential autocorrelation across years
  mod_ar1 <- nlme::gls(lnRatio ~ TS + window,
                 data = df_site_year_window,
                 correlation = nlme::corAR1(form = ~ growing_year | window),
                 na.action = na.omit)
  outcome[1, c("TAS", "TASp", "TAS_se")] <- summary(mod_ar1)$tTable["TS", c("Value", "p-value", "Std.Error")]

  list(outcome = outcome, siteyear = df_site_year_window, fit_profile = sampler$profile)
}

# Bind per-site results (list(outcome, siteyear)), given in site_info order,
# into the two output tables.
combine_tas_sites <- function(results) {
  list(
    outcome = do.call(rbind, lapply(results, `[[`, "outcome")),
    siteyear = do.call(rbind, c(list(data.frame()), lapply(results, `[[`, "siteyear")))
  )
}
