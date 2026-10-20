# Workflow step 02: thermal acclimation strength (TAS) of nighttime ecosystem
# respiration at one site, estimated with moving windows (Junna Wang, 2025).
#
# estimate_total_tas_site() is the per-site body of 02_01a (total TAS:
# respiration as a function of soil temperature only); estimate_direct_tas_site()
# is that of 02_01b (direct TAS: soil temperature plus soil water and the
# preceding days' daytime uptake). Each takes one row of site_info, the
# growing-season features, and the site's step-01 tables, and returns
# list(outcome = <one row of outcome_*.csv>, siteyear = <the site's rows of
# outcome_siteyear_*.csv>). combine_tas_sites() binds per-site results in
# site_info order into the published output tables.

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
  models <- list(
    total = list(frmu = NEE ~ exp(alpha * TS + beta*TS^2) * C0, param = alpha+beta+C0 ~ 1, priors = priors_temp),
    direct = list(frmu = NEE ~ exp(alpha * TS + beta*TS^2) * SWC / (Hs + SWC) * (C0 + NEE_daytime * k2),
                  param = alpha+beta+C0+Hs+k2 ~ 1, priors = priors_temp + priors_water + priors_gpp)
  )
  for (m in models) {
    brms::brm(brms::bf(m$frmu, m$param, nl = TRUE), prior = m$priors + prior_sigma, data = data,
              stanvars = tas_stanvars(prior_defaults, m$priors, brms::bf(m$frmu, m$param, nl = TRUE), data),
              chains = 0, backend = "cmdstanr")
  }
  invisible(NULL)
}

# Below are sites whose inter-annual TS was not strongly correlated to inter-annual TA, so use TA to obtain TS by linear regression. 
site_TS_issue <- c("BE-Bra", "CA-Cbo", "CA-Gro", "CA-Mer", "CA-Obs", "CA-TP3", "CH-Lae", "DE-RuC", "DE-SfS", "FI-Sod",
                   "IT-Ren", "NL-Loo", "US-Bar", "US-BZB", "US-BZF", "US-BZS", "US-CMW", "US-GLE", "US-Ha2",
                   "US-IB2", "US-Jo2", "US-KL2", "US-Kon", "US-LL1", "US-MBP", "US-Myb", "US-NC4", "US-Tw1", "US-ICt",
                   "BE-Dor", "CA-TP4", "UK-AMo", "RU-Fyo", "ZA-Kru", "IT-Tor")

new_outcome <- function() {
  data.frame(site_ID = character(), RMSE = double(), R2 = double(), control_year = double(), window_size = integer(), 
             nwindow = integer(), TAS = double(), TASp = double())
}

estimate_total_tas_site <- function(site, feature_gs, a_measure_night_complete, ac) {
  outcome <- new_outcome()
  sampler <- tas_fit_settings()

  name_site <- site$site_ID
  
  #-------------------------------------------DATA PREPARATION--------------------------
  if (site$SWC_use == 'YES') {
    a_measure_night_complete <- a_measure_night_complete |> dplyr::filter(!is.na(SWC))
  }  
  
  # For the 36 sites, obtain TS from simple linear regression of TA, in order to follow reviewer's suggestion
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
  dt = 30  # minute
  if (ac$MINUTE[2] - ac$MINUTE[1] != 30) { dt = 60 }   # minutes 
  # unit is umol / m2 / s
  ac_day <- ac |> dplyr::filter(daytime) |> dplyr::group_by(YEAR, DOY) |> 
    dplyr::summarise(NEE_daytime1 = max(-sum(NEE_uStar_f, na.rm=T) * dt * 60 / 86400, 0.0), .groups = "drop")
  # get rolling average of the prior three days
  ac_day$NEE_daytime <- zoo::rollapply(ac_day$NEE_daytime1, width = 3, FUN = mean,
                                  align = "right", fill = ac_day$NEE_daytime1[1])
  
  # attach it to nighttime data
  if (!"NEE_daytime1" %in% names(a_measure_night_complete)) {
    a_measure_night_complete <- a_measure_night_complete |> dplyr::mutate(DOY_gpp = dplyr::case_when(HOUR >= 12 ~ DOY,
                                                                                        HOUR < 12 & DOY == min(ac$DOY) ~ DOY, 
                                                                                        HOUR < 12 & DOY != min(ac$DOY) ~ DOY - 1)) |>
      dplyr::left_join(ac_day, by = c("YEAR", "DOY_gpp"="DOY")) |> dplyr::select(-"DOY_gpp") |> dplyr::filter(!is.na(NEE_daytime1))
  }
  
  # Deal with sites in southern hemisphere
  # add growing_year
  a_measure_night_complete <- a_measure_night_complete  |> 
    dplyr::mutate(growing_year = dplyr::case_when(DOY <= 366 ~ YEAR, 
                                    TRUE ~ YEAR - 1))
  ac <- ac |> 
    dplyr::mutate(growing_year = dplyr::case_when(DOY <= 366 ~ YEAR, 
                                    TRUE ~ YEAR - 1))
  
  # remove the growing_year with incomplete data
  # sites in southern hemisphere lose one year, because growing season crosses two years
  if (name_site %in% c('AU-Tum', 'ZA-Kru')) {
    a_measure_night_complete <- a_measure_night_complete |> dplyr::filter(dplyr::between(growing_year, ac$YEAR[1], ac$YEAR[nrow(ac)] - 1))
    ac <- ac |> dplyr::filter(dplyr::between(growing_year, ac$YEAR[1], ac$YEAR[nrow(ac)] - 1))
  }
  years <- sort(unique(a_measure_night_complete$growing_year))
  
  #---------------------------------DECIDE CONTROL YEAR, MOVING WINDOW SIZE, WINDOW NUMBERS------------------------------------
  # get start and end dates of growing season
  gStart <- feature_gs$gStart[feature_gs$site_ID == name_site]
  gEnd <- feature_gs$gEnd[feature_gs$site_ID == name_site]
  tStart <- feature_gs$tStart[feature_gs$site_ID == name_site]
  tEnd <- feature_gs$tEnd[feature_gs$site_ID == name_site]
  
  # Correct tStart and tEnd for sites with newly regressed TS
  if (name_site %in% site_TS_issue) {
    tStart <- quantile(ac$TS[dplyr::between(ac$DOY, gStart, gEnd)], c(0.025), na.rm=T)
    tEnd <- quantile(ac$TS[dplyr::between(ac$DOY, gStart, gEnd)], c(0.975), na.rm=T)
  }
  
  # decide control year: the year with growing-season TS closest to long-term mean. 
  ac_yearly_gs <- ac |> dplyr::filter(dplyr::between(DOY, gStart, gEnd)) |> dplyr::filter(growing_year %in% years) |> dplyr::group_by(growing_year) |> dplyr::summarise(TS=mean(TS, na.rm=T))
  control_year <- ac_yearly_gs$growing_year[which.min(abs(ac_yearly_gs$TS - mean(ac_yearly_gs$TS)))]
  
  # determine moving window size and number of windows
  if (dt == 30) {
    nobs_threshold <- 100
  } else {
    nobs_threshold <- 60
  }
  
  # use uniform window size: 2 weeks
  window_size <- 14
  
  # use non-overlapping windows and determine number of windows for growing season; decide to use overlapping windows
  nwindow <- max(round((gEnd - gStart + 1) / window_size), 1)
  
  #------------------------------------PREPARE FOR formula, stprm, priors of ER models---------------------------------
  # for brm models
  frmu <- NEE ~ exp(alpha * TS + beta*TS^2) * C0
  param <- alpha+beta+C0 ~ 1
  priors <- priors_temp
  prior_values <- prior_defaults
  # for nls models
  # alpha and CO are both positive and beta is negative
  frmu_nls <- NEE ~ exp(exp(alpha_ln) * TS - exp(beta_ln)*TS^2) * (exp(C0_ln))
  stprm <- c(C0_ln = 0.7, alpha_ln = -2.99, beta_ln = -6.9)
  
  df_site_year_window <- data.frame(site_ID = character(), growing_year = integer(), window = character(), nobsv = integer(), extend_days = integer(),  
                                    alpha = double(), beta = double(), C0 = double(), Hs = double(), k2 = double(), TS = double(), ERref = double(), lnRatio = double())
  ER_obs_pred <- data.frame()
  
  #------------------------------------FIT ER MODELS FOR EACH YEAR AND WINDOWS WITH BRM METHOD---------------------------------
  icount = 0
  for (iwindow in 1:nwindow) {
    window_start = gStart + window_size*(iwindow-1)
    window_end = min(gStart + window_size*iwindow, gEnd)
    
    ac_yearly_window <- ac |> dplyr::filter(dplyr::between(DOY, window_start, window_end)) |> dplyr::group_by(growing_year) |> dplyr::summarise(TS=mean(TS, na.rm=T))
    if (!dplyr::between(mean(ac_yearly_window$TS, na.rm=T), max(tStart, 2.0), tEnd) & nwindow > 1) {
      next
    }
    
    data <- a_measure_night_complete |> dplyr::filter(dplyr::between(DOY, window_start, window_end))
    # skip a window if no enough data; this is for sites ('US-ICt', 'US-ICh', 'US-ICs', 'FI-Sod') with a period of the whole day is daytime. 
    if (nrow(data) < 100) { next }
    
    # try gsl_nls first because it is fast, and then update priors of brm models based on mod_nls
    # this can give abnormal initial values. 
    mod_nls <- try(gslnls::gsl_nls(fn=as.formula(frmu_nls), data=data, start=stprm))
    if (!inherits(mod_nls, "try-error")) {
      prior_values["alpha_mu"] <- min(exp(coefficients(mod_nls)["alpha_ln"]), 0.2)
      prior_values["beta_mu"] <- -min(exp(coefficients(mod_nls)["beta_ln"]), 0.01)
      prior_values["C0_mu"] <- min(exp(coefficients(mod_nls)["C0_ln"]), 10)
    }
    
    # call the brm model to estimate parameters; this step takes much longer time.
    mod0 <- try(brms::brm(brms::bf(frmu, param, nl = TRUE),
                      prior = priors + prior_sigma, data = data,
                      stanvars = tas_stanvars(prior_values, priors, brms::bf(frmu, param, nl = TRUE), data), seed = tas_seed(name_site, "total", window_start, window_end, "pooled"), iter = sampler$pooled_iter, cores = sampler$cores, chains = sampler$chains, backend = "cmdstanr",
                      control = list(adapt_delta = 0.95, max_treedepth = 15), refresh = 0))
    # if the pooled fit fails there are no priors for the years: skip the window
    if (inherits(mod0, "try-error")) { next }
    
    # use this result as prior of each year
    prior_values["alpha_mu"] <- brms::fixef(mod0)["alpha_Intercept", "Estimate"]
    prior_values["beta_mu"] <- brms::fixef(mod0)["beta_Intercept", "Estimate"]
    prior_values["C0_mu"] <- brms::fixef(mod0)["C0_Intercept", "Estimate"]
    
    # get reference temperature, SWC, and NEEday of each window
    TSref <- mean(ac$TS[dplyr::between(ac$DOY, window_start, window_end)], na.rm=T)
    NEEdayref <- mean(ac_day$NEE_daytime[dplyr::between(ac_day$DOY, window_start, window_end)], na.rm=T)
    if (site$SWC_use == 'YES') {
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
      df_site_year_window[icount, 1] <- name_site
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
        data_subset <- a_measure_night_complete |> dplyr::filter(growing_year == iyear) |> 
          dplyr::filter(dplyr::between(DOY, window_start - extend_days, window_end + extend_days))
        # some conditions to break out to avoid dead loop
        if (nrow(data_subset) >= nobs_threshold & (window_end + extend_days) >= gEnd & TSref >= max(data_subset$TS, na.rm=T)) {break}
        if (extend_days >= 24) { break }   # max window size: two months
      }
      
      df_site_year_window[icount, 4] <- nrow(data_subset)
      df_site_year_window[icount, 5] <- extend_days
      
      # ensure enough observations
      if (nrow(data_subset) <= 25) { next }   # this is mainly for US-Uaf, which has few data
      
      # ensure enough temperature range
      if (!dplyr::between(TSref, quantile(data_subset$TS, 0.025, na.rm=T), quantile(data_subset$TS, 0.975, na.rm=T))) { next }
      
      # ensure nighttime NEE is positive
      if (median(data_subset$NEE) < 0.2 | mean(data_subset$NEE) < 0.2) { next }
      
      mod <- try(brms::brm(brms::bf(frmu, param, nl = TRUE),
                       prior = priors + prior_sigma, data = data_subset,
                       stanvars = tas_stanvars(prior_values, priors, brms::bf(frmu, param, nl = TRUE), data_subset), seed = tas_seed(name_site, "total", window_start, window_end, iyear), iter = sampler$year_iter, cores = sampler$cores, chains = sampler$chains, backend = "cmdstanr", 
                       control = list(adapt_delta = 0.90, max_treedepth = 15), refresh = 0))
      
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
                             stanvars = tas_stanvars(prior_values, priors, brms::bf(frmu, param, nl = TRUE), data_subset), seed = tas_seed(name_site, "total", window_start, window_end, iyear, "retry"), iter = sampler$retry_iter, cores = sampler$cores, chains = sampler$chains, backend = "cmdstanr", 
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
      
      # ER at reference temperature
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
                 na.action = na.omit
  )
  outcome[1, c("TAS", "TASp")] <- summary(mod_ar1)$tTable["TS", c("Value", "p-value")]

  list(outcome = outcome, siteyear = df_site_year_window, fit_profile = sampler$profile)
}

estimate_direct_tas_site <- function(site, feature_gs, a_measure_night_complete, ac, swc_ERA5) {
  outcome <- new_outcome()
  sampler <- tas_fit_settings()

  name_site <- site$site_ID
  
  #-------------------------------------------DATA PREPARATION--------------------------
  if (site$SWC_use == 'YES') {
    a_measure_night_complete <- a_measure_night_complete |> dplyr::filter(!is.na(SWC))
  }
  
  # if no measured SWC data use daily SWC from ERA5 land
  ####################################################
  if (site$SWC_use == 'NO') {
    a_measure_night_complete$SWC <- NULL
    ac$SWC <- NULL
    # use SWC data from ERA5 land climate reanalysis
    swc_ERA5_site <- swc_ERA5[swc_ERA5$name == name_site, ]
    # attach to a_measure_night_complete and ac
    a_measure_night_complete <- a_measure_night_complete |> dplyr::left_join(swc_ERA5_site[, c('YEAR', 'MONTH', 'DAY', 'SWC')], by = c('YEAR', 'MONTH', 'DAY'))
    ac <- ac |> dplyr::left_join(swc_ERA5_site[, c('YEAR', 'MONTH', 'DAY', 'SWC')], by = c('YEAR', 'MONTH', 'DAY'))
    site$SWC_use <- 'YES'
  }
  ###########################################

  # For the 36 sites, obtain TS from simple linear regression of TA, in order to follow reviewer's suggestion
  if (name_site %in% site_TS_issue) {
    if (name_site %in% c("US-Tw1")) {
      # slope will be too low if using ac data for the tropical and subtropical sites. 
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
  dt = 30  # minute
  if (ac$MINUTE[2] - ac$MINUTE[1] != 30) { dt = 60 }   # minutes 
  # unit is umol / m2 / s
  ac_day <- ac |> dplyr::filter(daytime) |> dplyr::group_by(YEAR, DOY) |> 
    dplyr::summarise(NEE_daytime1 = max(-sum(NEE_uStar_f, na.rm=T) * dt * 60 / 86400, 0.0), .groups = "drop")
  # get rolling average of the prior three days
  ac_day$NEE_daytime <- zoo::rollapply(ac_day$NEE_daytime1, width = 3, FUN = mean,
                                  align = "right", fill = ac_day$NEE_daytime1[1])
  
  # attach it to nighttime data
  if (!"NEE_daytime1" %in% names(a_measure_night_complete)) {
    a_measure_night_complete <- a_measure_night_complete |> dplyr::mutate(DOY_gpp = dplyr::case_when(HOUR >= 12 ~ DOY,
                                                                                        HOUR < 12 & DOY == min(ac$DOY) ~ DOY, 
                                                                                        HOUR < 12 & DOY != min(ac$DOY) ~ DOY - 1)) |>
      dplyr::left_join(ac_day, by = c("YEAR", "DOY_gpp"="DOY")) |> dplyr::select(-"DOY_gpp") |> dplyr::filter(!is.na(NEE_daytime1))
  }
  
  # Deal with sites in southern hemisphere
  # add growing_year
  a_measure_night_complete <- a_measure_night_complete  |> 
    dplyr::mutate(growing_year = dplyr::case_when(DOY <= 366 ~ YEAR, 
                                    TRUE ~ YEAR - 1))
  ac <- ac |> 
    dplyr::mutate(growing_year = dplyr::case_when(DOY <= 366 ~ YEAR, 
                                    TRUE ~ YEAR - 1))
  
  # remove the growing_year with incomplete data
  # sites in southern hemisphere lose one year, because growing season crosses two years
  if (name_site %in% c('AU-Tum', 'ZA-Kru')) {
    a_measure_night_complete <- a_measure_night_complete |> dplyr::filter(dplyr::between(growing_year, ac$YEAR[1], ac$YEAR[nrow(ac)] - 1))
    ac <- ac |> dplyr::filter(dplyr::between(growing_year, ac$YEAR[1], ac$YEAR[nrow(ac)] - 1))
  }
  years <- sort(unique(a_measure_night_complete$growing_year))
  
  #---------------------------------DECIDE CONTROL YEAR, MOVING WINDOW SIZE, WINDOW NUMBERS------------------------------------
  # get start and end dates of growing season
  gStart <- feature_gs$gStart[feature_gs$site_ID == name_site]
  gEnd <- feature_gs$gEnd[feature_gs$site_ID == name_site]
  tStart <- feature_gs$tStart[feature_gs$site_ID == name_site]
  tEnd <- feature_gs$tEnd[feature_gs$site_ID == name_site]
  
  # Correct tStart and tEnd for sites with newly regressed TS
  if (name_site %in% site_TS_issue) {
    tStart <- quantile(ac$TS[dplyr::between(ac$DOY, gStart, gEnd)], c(0.025), na.rm=T)
    tEnd <- quantile(ac$TS[dplyr::between(ac$DOY, gStart, gEnd)], c(0.975), na.rm=T)
  }
  
  # decide control year: the year with growing-season TS closest to long-term mean. 
  ac_yearly_gs <- ac |> dplyr::filter(dplyr::between(DOY, gStart, gEnd)) |> dplyr::filter(growing_year %in% years) |> dplyr::group_by(growing_year) |> dplyr::summarise(TS=mean(TS, na.rm=T))
  control_year <- ac_yearly_gs$growing_year[which.min(abs(ac_yearly_gs$TS - mean(ac_yearly_gs$TS)))]

  # determine moving window size and number of windows
  if (dt == 30) {
    nobs_threshold <- 100
  } else {
    nobs_threshold <- 60
  }
  
  # use uniform window size: 2 weeks
  window_size <- 14
  
  # use non-overlapping windows and determine number of windows for growing season
  nwindow <- max(round((gEnd - gStart + 1) / window_size), 1)

  #------------------------------------PREPARE FOR formula, stprm, priors of ER models---------------------------------
  if (site$SWC_use == 'YES') {
    # for brm models
    frmu <- NEE ~ exp(alpha * TS + beta*TS^2) * SWC / (Hs + SWC) * (C0 + NEE_daytime * k2)
    param <- alpha+beta+C0+Hs+k2 ~ 1
    priors <- priors_temp + priors_water + priors_gpp
    prior_values <- prior_defaults
    # for nls models
    # alpha, CO, k2, and Hs are all positive and beta are negative
    frmu_nls <- NEE ~ exp(exp(alpha_ln) * TS - exp(beta_ln)*TS^2) * SWC / (exp(Hs_ln) + SWC) * (exp(C0_ln) + NEE_daytime * exp(k2_ln))
    stprm <- c(C0_ln = 0.7, alpha_ln = -2.99, beta_ln = -6.9, k2_ln = -1.6, Hs_ln = 2.3)
  } else {
    frmu <- NEE ~ exp(alpha * TS + beta*TS^2) * (C0 + NEE_daytime * k2)
    param <- alpha+beta+C0+k2 ~ 1
    priors <- priors_temp + priors_gpp
    prior_values <- prior_defaults
    frmu_nls <- NEE ~ exp(exp(alpha_ln) * TS - exp(beta_ln)*TS^2) * (exp(C0_ln) + NEE_daytime * exp(k2_ln))
    stprm <- c(C0_ln = 0.7, alpha_ln = -2.99, beta_ln = -6.9, k2_ln = -1.6)
  }
  
  df_site_year_window <- data.frame(site_ID = character(), growing_year = integer(), window = character(), nobsv = integer(), extend_days = integer(),  
                                    alpha = double(), beta = double(), C0 = double(), Hs = double(), k2 = double(), TS = double(), ERref = double(), lnRatio = double())
  ER_obs_pred <- data.frame()

#------------------------------------FIT ER MODELS FOR EACH YEAR AND WINDOWS WITH BRM METHOD---------------------------------
  icount = 0
  for (iwindow in 1:nwindow) {
    window_start = gStart + window_size*(iwindow-1)
    window_end = min(gStart + window_size*iwindow, gEnd)
    
    ac_yearly_window <- ac |> dplyr::filter(dplyr::between(DOY, window_start, window_end)) |> dplyr::group_by(growing_year) |> dplyr::summarise(TS=mean(TS, na.rm=T))
    if (!dplyr::between(mean(ac_yearly_window$TS, na.rm=T), max(tStart, 2.0), tEnd) & nwindow > 1) {
      next
    }
    
    data <- a_measure_night_complete |> dplyr::filter(dplyr::between(DOY, window_start, window_end)) 
    # skip a window if no enough data; this is for sites ('US-ICt', 'US-ICh', 'US-ICs', 'FI-Sod') with a period of the whole day is daytime. 
    if (nrow(data) < 100) { next }
    
    # try gsl_nls first because it is fast, and then update priors of brm models based on mod_nls
    # this can give abnormal initial values. 
    mod_nls <- try(gslnls::gsl_nls(fn=as.formula(frmu_nls), data=data, start=stprm))
    if (!inherits(mod_nls, "try-error")) {
      prior_values["alpha_mu"] <- min(exp(coefficients(mod_nls)["alpha_ln"]), 0.2)
      prior_values["beta_mu"] <- -min(exp(coefficients(mod_nls)["beta_ln"]), 0.01)
      prior_values["C0_mu"] <- min(exp(coefficients(mod_nls)["C0_ln"]), 10)
      prior_values["k2_mu"] <- min(exp(coefficients(mod_nls)["k2_ln"]), 10)
      if (site$SWC_use == 'YES') {
        prior_values["Hs_mu"] <- min(exp(coefficients(mod_nls)["Hs_ln"]), 1000)
      }
    }
    
    # call the brm model to estimate parameters; this step takes much longer time.
    mod0 <- try(brms::brm(brms::bf(frmu, param, nl = TRUE),
                      prior = priors + prior_sigma, data = data,
                      stanvars = tas_stanvars(prior_values, priors, brms::bf(frmu, param, nl = TRUE), data), seed = tas_seed(name_site, "direct", window_start, window_end, "pooled"), iter = sampler$pooled_iter, cores = sampler$cores, chains = sampler$chains, backend = "cmdstanr",
                      control = list(adapt_delta = 0.95, max_treedepth = 15), refresh = 0))
    # if the pooled fit fails there are no priors for the years: skip the window
    if (inherits(mod0, "try-error")) { next }

    # use this result as prior of each year
    prior_values["alpha_mu"] <- brms::fixef(mod0)["alpha_Intercept", "Estimate"]
    prior_values["beta_mu"] <- brms::fixef(mod0)["beta_Intercept", "Estimate"]
    prior_values["C0_mu"] <- brms::fixef(mod0)["C0_Intercept", "Estimate"]
    prior_values["k2_mu"] <- brms::fixef(mod0)["k2_Intercept", "Estimate"]
    #
    if (site$SWC_use == 'YES') {
      prior_values["Hs_mu"] <- brms::fixef(mod0)["Hs_Intercept", "Estimate"]
    } 

    # get reference temperature, SWC, and NEEday of each window
    TSref <- mean(ac$TS[dplyr::between(ac$DOY, window_start, window_end)], na.rm=T)
    NEEdayref <- mean(ac_day$NEE_daytime[dplyr::between(ac_day$DOY, window_start, window_end)], na.rm=T)
    if (site$SWC_use == 'YES') {
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
      df_site_year_window[icount, 1] <- name_site
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
        data_subset <- a_measure_night_complete |> dplyr::filter(growing_year == iyear) |> 
          dplyr::filter(dplyr::between(DOY, window_start - extend_days, window_end + extend_days))
        # some conditions to break out to avoid dead loop
        if (nrow(data_subset) >= nobs_threshold & (window_end + extend_days) >= gEnd & TSref >= max(data_subset$TS, na.rm=T)) {break}
        if (extend_days >= 24) { break }   # max window size: two months
      }
      df_site_year_window[icount, 4] <- nrow(data_subset)
      df_site_year_window[icount, 5] <- extend_days
      
      # ensure enough observations
      if (nrow(data_subset) <= 25) { next }   # this is mainly for US-Uaf, which has few data
      
      # ensure enough temperature range
      if (!dplyr::between(TSref, quantile(data_subset$TS, 0.025, na.rm=T), quantile(data_subset$TS, 0.975, na.rm=T))) { next }
      
      # ensure nighttime NEE is positive
      if (median(data_subset$NEE) < 0.2 | mean(data_subset$NEE) < 0.2) { next }
      
      mod <- try(brms::brm(brms::bf(frmu, param, nl = TRUE),
                       prior = priors + prior_sigma, data = data_subset,
                       stanvars = tas_stanvars(prior_values, priors, brms::bf(frmu, param, nl = TRUE), data_subset), seed = tas_seed(name_site, "direct", window_start, window_end, iyear), iter = sampler$year_iter, cores = sampler$cores, chains = sampler$chains, backend = "cmdstanr", 
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
                             stanvars = tas_stanvars(prior_values, priors, brms::bf(frmu, param, nl = TRUE), data_subset), seed = tas_seed(name_site, "direct", window_start, window_end, iyear, "retry"), iter = sampler$retry_iter, cores = sampler$cores, chains = sampler$chains, backend = "cmdstanr", 
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
  outcome[1, c("TAS", "TASp")] <- summary(mod_ar1)$tTable["TS", c("Value", "p-value")]

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
