# Effect of total thermal responses on future (2041-2060, SSP2-4.5) nighttime
# ecosystem respiration, per site: the present growing-season night pattern
# of soil temperature, shifted by the projected change in monthly minimum
# temperature (04_01), run through a temperature-respiration curve fitted in
# each 14-day window of a control year, with and without the site's TAS.
# Writes data-proc/analysis/acclimation_data_future_ssp245.csv. Slow: one
# brms fit per window per control year per site.
# Authors: Junna Wang, October, 2025

stopifnot(requireNamespace("brms"), requireNamespace("dplyr"), requireNamespace("zoo"))

dir_resp <- "data-proc/respiration"

feature_gs <- read.csv(file.path("data-proc", "features", "growing_season_features.csv"))
stopifnot("one growing-season row per site" = !anyDuplicated(feature_gs$site_ID))

acclimation <- read.csv(file.path("data-proc", "analysis", "acclimation_data.csv"))
Tmin_month <- read.csv(file.path("data-proc", "analysis", "Tmin_month_ssp245_wc.csv"))
acclimation <- acclimation |> dplyr::left_join(Tmin_month, by = "site_ID")

# the six result columns
acclimation$TS_TA <- 0            # slope of TS on TA
acclimation$TSmin_c <- 0          # monthly min soil temperature change
acclimation$TSmin_c_gs <- 0       # ... over the growing season
acclimation$NEE_night_mod_p <- 0
acclimation$NEE_night_mod_f <- 0
acclimation$NEE_night_mod_fa <- 0

priors <- brms::prior("normal(2, 5)", nlpar = "C0", lb = 0, ub = 10) +
  brms::prior("normal(0.1, 1)", nlpar = "alpha", lb = 0, ub = 0.2) +
  brms::prior("normal(-0.001, 0.1)", nlpar = "beta", lb = -0.02, ub = 0.0)
frmu <- NEE ~ exp(alpha * TS + beta * TS^2) * C0
param <- alpha + beta + C0 ~ 1
window_size <- 14

files <- list.files(path = dir_resp, pattern = "_ac.csv$", full.names = TRUE, recursive = TRUE)
for (i in seq_along(files)) {
  name_site <- sub("_ac\\.csv$", "", basename(files[i]))
  message(i, name_site)
  iacclimation <- which(acclimation$site_ID == name_site)
  ac <- read.csv(files[i])

  # use only the years with qualified data
  night_file <- file.path(dirname(files[i]), paste0(name_site, "_nightNEE.csv"))
  a_measure_night_complete <- read.csv(night_file)
  good_years <- unique(a_measure_night_complete$YEAR)

  gStart <- feature_gs$gStart[feature_gs$site_ID == name_site]
  gEnd <- feature_gs$gEnd[feature_gs$site_ID == name_site]

  # Control years: the three closest to the median growing-season TS among
  # those with more than the median number of observations.
  yearly_TSgs <- a_measure_night_complete |>
    dplyr::filter(dplyr::between(DOY, gStart, gEnd)) |>
    dplyr::group_by(YEAR) |>
    dplyr::summarise(n = dplyr::n(), TSgs = mean(TS))
  yearly_TSgs <- yearly_TSgs |>
    dplyr::mutate(close_mean = abs(TSgs - median(TSgs))) |>
    dplyr::filter(n > median(n)) |>
    dplyr::arrange(close_mean)

  # TS ~ TA slope above freezing; US-Tw1 (subtropical) on the nighttime table.
  tmp <- if (name_site != "US-Tw1") ac else a_measure_night_complete
  acclimation$TS_TA[iacclimation] <- coef(lm(data = dplyr::filter(tmp, TA > 0), TS ~ TA))[["TA"]]

  # The present nighttime pattern by DOY and time of day, and the future one.
  night_pattern <- ac |>
    dplyr::filter(YEAR %in% good_years & !daytime) |>
    dplyr::group_by(DOY, HOUR, MINUTE, MONTH) |>
    dplyr::summarise(TAp = mean(TA, na.rm = TRUE), TSp = mean(TS, na.rm = TRUE), .groups = "drop")
  night_pattern <- night_pattern[!duplicated(night_pattern[, c("DOY", "HOUR", "MINUTE")]), ]
  # Fill occasional gaps: TA by interpolation, TS from TA.
  if (anyNA(night_pattern$TAp)) night_pattern$TAp <- zoo::na.approx(night_pattern$TAp)
  if (anyNA(night_pattern$TSp)) {
    TSp_pred <- predict(lm(data = night_pattern, TSp ~ TAp), night_pattern)
    night_pattern$TSp[is.na(night_pattern$TSp)] <- TSp_pred[is.na(night_pattern$TSp)]
  }
  # Tmin1..Tmin12 by name, in month order.
  temp_change <- data.frame(MONTH = 1:12, TAmnc = as.numeric(acclimation[iacclimation, paste0("Tmin", 1:12)]))
  night_pattern <- night_pattern |> dplyr::left_join(temp_change, by = "MONTH")
  night_pattern$TAf <- night_pattern$TAp + night_pattern$TAmnc
  night_pattern$TSf <- night_pattern$TSp + night_pattern$TAmnc * acclimation$TS_TA[iacclimation]
  in_gs <- night_pattern$DOY >= gStart & night_pattern$DOY <= gEnd

  nobs_threshold <- if (name_site %in% c("US-ICt", "US-ICh", "US-ICs")) 90 else 200
  nwindow <- max(round((gEnd - gStart + 1) / window_size), 1)

  df_NEE_p_f <- data.frame()
  for (control_year in yearly_TSgs$YEAR[1:3]) {
    a_measure_night_complete_control <- a_measure_night_complete |> dplyr::filter(YEAR == control_year)
    NEEp <- rep(NA_real_, nrow(night_pattern))
    NEEf <- rep(NA_real_, nrow(night_pattern))

    for (iwindow in 1:nwindow) {
      window_start <- gStart + window_size * (iwindow - 1)
      window_end <- if (iwindow == nwindow) gEnd else min(gStart + window_size * iwindow, gEnd)

      # Nothing to estimate where the whole window is daytime (US-ICt, US-ICh,
      # US-ICs, FI-Sod), or where soil is too cold for reliable measurements.
      id <- which(dplyr::between(night_pattern$DOY, window_start, window_end) & night_pattern$TSp > 1.0)
      if (length(id) == 0) next

      # Fit the TS-ER curve on the control year over a window widened by a
      # week each side, and further until it has enough observations, since
      # it has to predict for a warmer future.
      extend_days <- 7
      repeat {
        data <- a_measure_night_complete_control |>
          dplyr::filter(dplyr::between(DOY, window_start - extend_days, window_end + extend_days) & TS > 1.0)
        if (nrow(data) >= nobs_threshold || extend_days > (gEnd - gStart) / 2.0) break
        extend_days <- extend_days + 7
      }

      mod <- brms::brm(brms::bf(frmu, param, nl = TRUE),
                       prior = priors, data = data, iter = 2000, cores = 4, chains = 4, backend = "cmdstanr",
                       control = list(adapt_delta = 0.95, max_treedepth = 15), refresh = 0)
      NEEp[id] <- fitted(mod, newdata = data.frame(TS = night_pattern$TSp[id]))[, "Estimate"]
      NEEf[id] <- fitted(mod, newdata = data.frame(TS = night_pattern$TSf[id]))[, "Estimate"]
    }
    # NEE during the growing season only
    df_NEE_p_f <- rbind(df_NEE_p_f, data.frame(NEE_p = mean(NEEp[in_gs], na.rm = TRUE),
                                               NEE_f = mean(NEEf[in_gs], na.rm = TRUE)))
  }
  df_NEE_p_f$ratio <- df_NEE_p_f$NEE_f / df_NEE_p_f$NEE_p

  # Drop the control year with the lowest future/present ratio; average the rest.
  acclimation$NEE_night_mod_p[iacclimation] <- mean(df_NEE_p_f$NEE_p[-which.min(df_NEE_p_f$ratio)])
  acclimation$NEE_night_mod_f[iacclimation] <- mean(df_NEE_p_f$NEE_f[-which.min(df_NEE_p_f$ratio)])
  acclimation$TSmin_c[iacclimation] <- mean(temp_change$TAmnc) * acclimation$TS_TA[iacclimation]
  acclimation$TSmin_c_gs[iacclimation] <- mean(night_pattern$TAmnc[in_gs], na.rm = TRUE) * acclimation$TS_TA[iacclimation]
  acclimation$NEE_night_mod_fa[iacclimation] <- acclimation$NEE_night_mod_f[iacclimation] *
    exp(acclimation$TAS_tot[iacclimation] * acclimation$TSmin_c_gs[iacclimation])
}

dir.create("data-proc/analysis", recursive = TRUE, showWarnings = FALSE)
write.csv(acclimation, file = file.path("data-proc", "analysis", "acclimation_data_future_ssp245.csv"), row.names = FALSE)
