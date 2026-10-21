# Workflow step 04: the effect of thermal responses on future nighttime
# ecosystem respiration under SSP2-4.5, 2041-2060 (Junna Wang, 2025).
#
#   future_tmin_change()   04_01: monthly change in minimum air temperature
#                          per site (WorldClim 2.1, 2041-2060 GCM mean minus
#                          the 2000-2020 mean)
#   scenario_site()        04_02 for one site: present and future growing-season
#                          nighttime respiration, without and with the site's
#                          total thermal response
#   scenario_data()        04_02's acclimation_data_future_ssp245.csv table

# Monthly minimum-temperature change (Tmin1..Tmin12, deg C) at each site: the
# mean over GCMs of the 2041-2060 monthly means minus the mean of the
# 2000-2020 monthly series, averaged over the site's cell and its four rook
# neighbours.
future_tmin_change <- function(site_info, baseline_files, future_files, ssp = "ssp245") {
  xy <- data.frame(x=site_info$LONG, y=site_info$LAT)
  base_line <- terra::rast()
  for (i in 1:12) {
    month <- sprintf("%02d", i)
    id <- grepl(pattern=paste0(month, '.tif$'), basename(baseline_files))
    base_line <- c(base_line, terra::app(terra::rast(baseline_files[id]), fun=mean))
  }
  names(base_line) <- 1:12
  raster_list <- lapply(future_files[grepl(ssp, future_files)], terra::rast)
  GCM <- list()
  for (i in 1:terra::nlyr(raster_list[[1]])) {
    layers <- lapply(raster_list, function(x) x[[i]])
    GCM[[i]] <- terra::app(terra::rast(layers), fun=mean)
  }
  GCM <- terra::rast(GCM)
  Tmin_change <- GCM - base_line
  icell <- terra::adjacent(Tmin_change, terra::cellFromXY(Tmin_change, xy), include=TRUE)
  tmp <- terra::extract(Tmin_change, c(icell))
  Tmin_month <- data.frame(site_ID=site_info$site_ID)
  for (i in 1:ncol(tmp)) {
    Tmin_month[,i+1] <- as.vector(tapply(tmp[,i], rep(1:nrow(xy), times=5), mean, na.rm=T))
  }
  colnames(Tmin_month)[2:13] <- paste0('Tmin', 1:12)
  Tmin_month
}

# The respiration model of 04_02: the total model of step 02, but with the
# beta prior bounded at -0.02 and fixed (not re-centred) prior means; like
# step 02 its prior values and residual-SD scale reach Stan as data.
scenario_priors <- function() {
  loadNamespace("brms")
  brms::prior("normal(C0_mu, C0_sd)", nlpar = "C0", lb = 0, ub = 10) +
    brms::prior("normal(alpha_mu, alpha_sd)", nlpar = "alpha", lb = 0, ub = 0.2) +
    brms::prior("normal(beta_mu, beta_sd)", nlpar = "beta", lb = -0.02, ub = 0.0)
}

compile_scenario_model <- function() {
  data <- data.frame(NEE = c(1, 2, 3, 2), TS = c(5, 10, 15, 20))
  f <- brms::bf(NEE ~ exp(alpha * TS + beta*TS^2) * C0, alpha+beta+C0 ~ 1, nl = TRUE)
  brms::brm(f, prior = scenario_priors() + prior_sigma, data = data,
            stanvars = tas_stanvars(prior_defaults, scenario_priors(), f, data), chains = 0, backend = "cmdstanr")
  invisible(NULL)
}

# 04_02 for one site. tmin_change: the site's row of future_tmin_change().
# Returns one row: TS_TA (slope of soil on air temperature), TSmin_c and
# TSmin_c_gs (minimum soil temperature change, year and growing season), and
# growing-season mean nighttime respiration at present (NEE_night_mod_p) and
# in the future without thermal response (NEE_night_mod_f). None of it
# depends on TAS: scenario_data() applies a set of TAS estimates.
scenario_site <- function(site_id, ac, a_measure_night_complete, gStart, gEnd, tmin_change,
                          sampler = tas_fit_settings()) {
  name_site <- site_id
  ac <- ac[order(ac$YEAR, ac$DOY, ac$HOUR, ac$MINUTE), ]
  good_years <- unique(a_measure_night_complete$YEAR)
  frmu <- NEE ~ exp(alpha * TS + beta*TS^2) * C0
  param <- alpha+beta+C0 ~ 1
  priors <- scenario_priors()
  out <- data.frame(site_ID = site_id)

  ############I should use control year, and this control year should have enough data during every period#######
  yearly_TSgs <- a_measure_night_complete |> dplyr::filter(dplyr::between(.data$DOY, .env$gStart, .env$gEnd)) |>
    dplyr::group_by(.data$YEAR) |> dplyr::summarise(n = dplyr::n(), TSgs=mean(.data$TS))
  yearly_TSgs <- yearly_TSgs |> dplyr::mutate(close_mean = abs(.data$TSgs - stats::median(yearly_TSgs$TSgs))) |>
    dplyr::filter(.data$n > stats::median(yearly_TSgs$n)) |> dplyr::arrange(.data$close_mean)

  df_NEE_p_f <- data.frame()
  for (control_year in yearly_TSgs$YEAR[1:3]) {
    a_measure_night_complete_control <- a_measure_night_complete |> dplyr::filter(.data$YEAR == .env$control_year)

    # recalculate TS_TA relationship using TA threshold
    if (!name_site %in% c('US-Tw1')) {
      tmp <- ac |> dplyr::filter(.data$TA > 0)     # only use the data with TA > 0C; use another range to calculate the coefficient
    } else {
      # tropical and subtropical site: its values are obtained using observed temperature only.
      tmp <- a_measure_night_complete |> dplyr::filter(.data$TA > 0)
    }
    out$TS_TA <- summary(stats::lm(data=tmp, TS~TA))$coefficients[2,1]

    night_pattern <- ac |> dplyr::filter(.data$YEAR %in% .env$good_years & !.data$daytime) |>
      dplyr::group_by(.data$DOY, .data$HOUR, .data$MINUTE, .data$MONTH) |>
      dplyr::summarise(TAp=mean(.data$TA, na.rm=T), TSp=mean(.data$TS, na.rm=T), SWC=mean(.data$SWC, na.rm=T), .groups = 'drop')
    # remove duplicate rows
    night_pattern  <- night_pattern[!duplicated(night_pattern[,c('DOY', 'HOUR', 'MINUTE')]), ]
    # deal with occasional TA missing cases
    if (sum(is.na(night_pattern$TAp)) > 0) {
      night_pattern$TAp <- zoo::na.approx(night_pattern$TAp)
    }
    # deal with occasional TS missing cases
    if (sum(is.na(night_pattern$TSp)) > 0) {
      mod_lm <- stats::lm(data=night_pattern, TSp ~ TAp)
      TSp_pred <- stats::predict(mod_lm, night_pattern)
      night_pattern$TSp[is.na(night_pattern$TSp)] <- TSp_pred[is.na(night_pattern$TSp)]
    }
    temp_change   <- data.frame(MONTH=1:12, TAmnc=as.numeric(tmin_change[paste0('Tmin', 1:12)]))
    night_pattern <- night_pattern |> dplyr::left_join(temp_change, by="MONTH")
    night_pattern$TAf <- night_pattern$TAp + night_pattern$TAmnc
    night_pattern$TSf <- night_pattern$TSp + night_pattern$TAmnc * out$TS_TA
    night_pattern$NEEp <- NA
    night_pattern$NEEf <- NA

    # use uniform window size: 2 weeks, on step 02's grid of non-overlapping
    # windows, except that the last one runs to gEnd
    window_size <- 14
    nwindow <- max(round((gEnd - gStart + 1) / window_size), 1)

    for (iwindow in 1:nwindow) {
      window_start = gStart + window_size*(iwindow-1)
      window_end = min(gStart + window_size*iwindow - 1, gEnd)
      if (iwindow == nwindow & window_end != gEnd) {
        window_end <- gEnd
      }
      # Skip a window if no need to estimate ER due to all daytime, or the soil is too cold (TS <= 1C)
      id <- which(dplyr::between(night_pattern$DOY, window_start, window_end) & night_pattern$TSp > 1.0)
      if ( length(id) == 0 ) { next }

      # extend the range to get TS~ER curve, because we need to predict for warming future
      data <- a_measure_night_complete_control |> dplyr::filter(dplyr::between(.data$DOY, .env$window_start - 7, .env$window_end + 7) & .data$TS > 1.0)
      if (name_site %in% c('US-ICt', 'US-ICh', 'US-ICs')) {
        nobs_threshold = 90
      } else {
        nobs_threshold = 200
      }
      # if not enough observed data in control year, extend the window
      extend_days <- 0
      while(nrow(data) < nobs_threshold) {
        extend_days <- extend_days + 7
        data <- a_measure_night_complete_control |> dplyr::filter(dplyr::between(.data$DOY, .env$window_start - .env$extend_days, .env$window_end + .env$extend_days) & .data$TS > 1.0)
        # some conditions to break out to avoid dead loop
        if (extend_days > (gEnd - gStart) / 2.0) {break}
      }

      mod <- brms::brm(brms::bf(frmu, param, nl = TRUE),
                       prior = priors + prior_sigma, data = data,
                       stanvars = tas_stanvars(prior_defaults, priors, brms::bf(frmu, param, nl = TRUE), data),
                       seed = tas_seed(name_site, "scenario", control_year, window_start, window_end),
                       iter = sampler$pooled_iter, cores = sampler$cores, chains = sampler$chains, backend = "cmdstanr",
                       control = list(adapt_delta = 0.95, max_treedepth = 15), refresh = 0)

      df_present <- data.frame(TS=night_pattern$TSp[id])
      night_pattern$NEEp[id] <- stats::fitted(mod, newdata=df_present)[, "Estimate"]
      df_future <- data.frame(TS=night_pattern$TSf[id])
      night_pattern$NEEf[id] <- stats::fitted(mod, newdata=df_future)[, "Estimate"]
    }
    # NEE during growing season only
    df_NEE_p_f <- rbind(df_NEE_p_f, data.frame(NEE_p = mean(night_pattern$NEEp[night_pattern$DOY >= gStart & night_pattern$DOY <= gEnd], na.rm=T),
                                               NEE_f = mean(night_pattern$NEEf[night_pattern$DOY >= gStart & night_pattern$DOY <= gEnd], na.rm=T)))
  }
  df_NEE_p_f$ratio <- df_NEE_p_f$NEE_f / df_NEE_p_f$NEE_p

  # mean over the control years, leaving out the one with the smallest future/present ratio
  out$NEE_night_mod_p  <- mean(df_NEE_p_f$NEE_p[-which.min(df_NEE_p_f$ratio)])
  out$NEE_night_mod_f  <- mean(df_NEE_p_f$NEE_f[-which.min(df_NEE_p_f$ratio)])
  out$TSmin_c <- mean(temp_change$TAmnc) * out$TS_TA
  out$TSmin_c_gs <- mean(night_pattern$TAmnc[night_pattern$DOY >= gStart & night_pattern$DOY <= gEnd], na.rm=T) * out$TS_TA
  out
}

# 04_02's output table for one set of TAS estimates: the driver table (with
# its total TAS, TAS_tot) joined to the monthly minimum-temperature changes and
# each site's scenario results, plus future respiration with the site's total
# thermal response, NEE_night_mod_fa = NEE_night_mod_f * exp(TAS_tot *
# TSmin_c_gs); in the published column order.
scenario_data <- function(acclimation, tmin_month, scenario_rows) {
  out <- acclimation |>
    dplyr::left_join(tmin_month, by = "site_ID") |>
    dplyr::left_join(scenario_rows[, c("site_ID", "TS_TA", "TSmin_c", "TSmin_c_gs", "NEE_night_mod_p", "NEE_night_mod_f")], by = "site_ID")
  out$NEE_night_mod_fa <- out$NEE_night_mod_f * exp(out$TAS_tot * out$TSmin_c_gs)
  out
}
