# Workflow step 03_01: the environmental and biological conditions that may
# drive thermal response strength (Junna Wang, 2025), as functions:
#
#   site_climate_stats()   one site's NEE totals and air temperature
#                          statistics from its step-01 tables (the 03_01 loop)
#   soil_carbon()          soil organic carbon: AmeriFlux BIF measurements, else
#                          the GSOC map corrected against them
#   spectral_indices()     site means of MODIS EVI, NDVI, LAI and GPP
#   driver_data()          the table 03_02 analyses (acclimation_data.csv),
#                          for a given set of total and direct TAS estimates

# Annual and diel NEE totals and air/soil temperature statistics of one site,
# over the years kept in its nighttime table. Returns one row with the
# columns 03_01's stat.climate table had.
site_climate_stats <- function(site_id, ac, a_measure_night_complete) {
  ac <- ac[order(ac$YEAR, ac$DOY, ac$HOUR, ac$MINUTE), ]
  good_years <- unique(a_measure_night_complete$YEAR)

  # I have to gap fill TA, TS, and NEE, if needed.
  if (!"TA_gf" %in% colnames(ac)) {
    T_gf <- ac |> dplyr::group_by(.data$DOY, .data$HOUR, .data$MINUTE) |>
      dplyr::summarise(TA_gf=mean(.data$TA, na.rm=T), TS_gf=mean(.data$TS, na.rm=T), NEE_gf=mean(.data$NEE, na.rm=T), .groups = 'drop')  # for gap fill only.
    ac   <- ac |> dplyr::left_join(T_gf, by=c("DOY", "HOUR", "MINUTE"))
    if (sum(is.na(ac$TA)) > 0) {
      ac$TA[is.na(ac$TA)] <- ac$TA_gf[is.na(ac$TA)]
    }
    if (sum(is.na(ac$TS)) > 0) {
      ac$TS[is.na(ac$TS)] <- ac$TS_gf[is.na(ac$TS)]
    }
    if (sum(is.na(ac$NEE)) > 0) {
      ac$NEE[is.na(ac$NEE)] <- ac$NEE_gf[is.na(ac$NEE)]
    }
  }
  ac_good <- ac |> dplyr::filter(.data$YEAR %in% .env$good_years)

  # Do NEE separately: only the selected years
  annual <- ac_good |> dplyr::group_by(.data$YEAR) |> dplyr::summarise(NEE=sum(.data$NEE_uStar_f, na.rm=T), .groups = 'drop')
  annual_day   <- ac_good |> dplyr::filter(.data$daytime) |> dplyr::group_by(.data$YEAR) |> dplyr::summarise(NEE=sum(.data$NEE_uStar_f, na.rm=T), .groups = 'drop')
  annual_night <- ac_good |> dplyr::filter(!.data$daytime) |> dplyr::group_by(.data$YEAR) |> dplyr::summarise(NEE=sum(.data$NEE_uStar_f, na.rm=T), .groups = 'drop')
  # time step (minutes), from the first two records
  dt <- abs(ac$MINUTE[2] + ac$HOUR[2] * 60 - ac$MINUTE[1] - ac$HOUR[1] * 60)

  out <- data.frame(site_ID = site_id)
  out$NEE <- mean(annual$NEE) * dt * 60 / 1000000 * 12        ### g C / m2;
  out$NEE_day <- mean(annual_day$NEE) * dt * 60 / 1000000 * 12    ### g C / m2;
  out$NEE_night <- mean(annual_night$NEE) * dt * 60 / 1000000 * 12  ### g C / m2;

  # mean diel cycle of each DOY, interpolated over gaps
  data_yearly  <- ac_good |> dplyr::group_by(.data$DOY, .data$HOUR, .data$MINUTE, .data$daytime) |> dplyr::summarise(NEE=mean(.data$NEE_uStar_f, na.rm=T), .groups = 'drop')
  # remove duplicate rows
  data_yearly  <- data_yearly[!duplicated(data_yearly[,c('DOY', 'HOUR', 'MINUTE')]), ]
  data_yearly$time <- data_yearly$DOY + data_yearly$HOUR / 24 + data_yearly$MINUTE / 60 / 24
  data_yearly <- data_yearly |> dplyr::arrange(.data$time)
  data_yearly$NEE.appr <- zoo::na.approx(data_yearly$NEE, data_yearly$time, na.rm = FALSE)
  # dealing with missing head and end data
  num_min <- min(which(!is.na(data_yearly$NEE.appr)))
  num_max <- max(which(!is.na(data_yearly$NEE.appr)))
  if (num_min > 1) {
    data_yearly$NEE.appr[1:(num_min-1)] <- data_yearly$NEE.appr[num_min]
  }
  if (num_max < nrow(data_yearly)) {
    data_yearly$NEE.appr[(num_max+1):nrow(data_yearly)] <- data_yearly$NEE.appr[num_max]
  }

  # air and soil temperature
  data_yearly_t  <- ac_good |> dplyr::group_by(.data$DOY) |> dplyr::summarise(TS=mean(.data$TS, na.rm=T), TA=mean(.data$TA, na.rm=T), .groups = 'drop')
  data_1day      <- ac_good |> dplyr::group_by(.data$YEAR, .data$DOY) |>
    dplyr::summarise(TSrange=max(.data$TS, na.rm=T)-min(.data$TS, na.rm=T), TArange=max(.data$TA, na.rm=T)-min(.data$TA, na.rm=T),
                     TS=mean(.data$TS, na.rm=T), TA=mean(.data$TA, na.rm=T), .groups = 'drop')
  data_1year_sd  <- data_1day |> dplyr::group_by(.data$YEAR) |> dplyr::summarise(TS=stats::sd(.data$TS, na.rm=T), TA=stats::sd(.data$TA, na.rm=T), .groups = 'drop')
  data_1year_mn  <- data_1day |> dplyr::group_by(.data$YEAR) |> dplyr::summarise(TS=mean(.data$TS, na.rm=T), TA=mean(.data$TA, na.rm=T), .groups = 'drop')
  data_1year_rg  <- data_1day |> dplyr::group_by(.data$YEAR) |> dplyr::summarise(TS=max(.data$TS, na.rm=T)-min(.data$TS, na.rm=T), TA=max(.data$TA, na.rm=T)-min(.data$TA, na.rm=T), .groups = 'drop')
  # MA: mean annual; SS: seasonal variation; IA: inter-annual variation; DR: daily range; RS: seasonal range
  out$MATA <- mean(data_yearly_t$TA)
  out$SSTA <- mean(data_1year_sd$TA)
  out$IATA <- stats::sd(data_1year_mn$TA)
  out$DRTA <- mean(data_1day$TArange)
  out$RSTA <- mean(data_1year_rg$TA)
  out$MATS <- mean(data_yearly_t$TS)
  out$SSTS <- mean(data_1year_sd$TS)
  out$IATS <- stats::sd(data_1year_mn$TS)
  out$DRTS <- mean(data_1day$TSrange, na.rm=T)
  out$RSTS <- mean(data_1year_rg$TS)
  out$NEE2 <- sum(data_yearly$NEE.appr) * dt * 60 / 1000000 * 12  ### g CO2 / m2
  out$NEE_day2 <- sum(data_yearly$NEE.appr[data_yearly$daytime], na.rm=T) * dt * 60 / 1000000 * 12   ### na.rm for tundra sites
  out$NEE_night2 <- sum(data_yearly$NEE.appr[!data_yearly$daytime], na.rm=T) * dt * 60 / 1000000 * 12

  # Warming rate: linear trend of annual mean air temperature
  data_annual  <- ac_good |> dplyr::group_by(.data$YEAR) |> dplyr::summarise(TA=mean(.data$TA, na.rm=T), TS=mean(.data$TS, na.rm=T), .groups = 'drop')
  if (site_id == "CH-Aws") {
    data_annual <- data_annual |> dplyr::filter(.data$YEAR >= 2015)
  }
  trend <- summary(stats::lm(data = data_annual, TA ~ YEAR))$coefficient[2, c(1, 4)]
  out$warm_rate <- unname(trend[1])
  out$warm_ratep <- unname(trend[2])
  out
}

# Soil organic carbon per site: measured (AmeriFlux BIF, g C kg-1) where
# available, otherwise the GSOC map (t/ha) corrected by its regression on the
# measured values.
soil_carbon <- function(site_info, bif_file, gsoc_file) {
  BIF <- utils::read.csv(bif_file)
  BIF.site <- BIF |> dplyr::filter(.data$SITE_ID %in% site_info$site_ID) |> dplyr::filter(.data$VARIABLE == 'SOIL_CHEM_C_ORG') |>
    dplyr::group_by(.data$SITE_ID) |> dplyr::summarise(soc_obs = mean(as.numeric(.data$DATAVALUE), na.rm=T), .groups = 'drop')
  stat.soil <- data.frame(site_ID=site_info$site_ID)
  GSOCmap <- terra::rast(gsoc_file)
  xy <- data.frame(x=site_info$LONG, y=site_info$LAT)
  stat.soil$GSOC <- terra::extract(GSOCmap, xy)$GSOCmap1.5.0
  tmp <- BIF.site |> dplyr::left_join(stat.soil, by=c('SITE_ID'='site_ID'))
  model <- stats::lm(data=tmp, GSOC ~ soc_obs)
  BIF.site$GSOC_obs <- stats::predict(model, tmp[c("soc_obs")])
  stat.soil <- stat.soil |> dplyr::left_join(BIF.site, by=c('site_ID'='SITE_ID'))
  stat.soil |> dplyr::mutate(SOC = dplyr::case_when(is.na(.data$GSOC_obs) ~ .data$GSOC,
                                                    !is.na(.data$GSOC_obs) ~ .data$GSOC_obs))
}

# Site means of MODIS EVI and NDVI (MOD13A2), Fpar and LAI (MOD15A2H) and GPP
# (MYD17A2HGF, kg C m-2 yr-1): monthly means of good-quality values,
# interpolated over months without any, then averaged.
spectral_indices <- function(mod13_file, mod15_file, myd17_file) {
  file1    <- utils::read.csv(mod13_file)  ##EVI and NDVI
  good_vi <- file1$MOD13A2_061__1_km_16_days_VI_Quality_MODLAND_Description %in% c("VI produced, good quality", "VI produced, but check other QA")
  file1$MOD13A2_061__1_km_16_days_EVI[!good_vi]  <- NA
  file1$MOD13A2_061__1_km_16_days_NDVI[!good_vi] <- NA
  file1$Date <- as.Date(file1$Date, format = "%m/%d/%y")
  file1_interp <- file1 |> dplyr::mutate(month = lubridate::month(.data$Date)) |> dplyr::group_by(.data$ID, .data$month) |>
    dplyr::summarise(NDVI = max(mean(.data$MOD13A2_061__1_km_16_days_NDVI, na.rm=T), 0.0), n_NDVI = sum(!is.na(.data$MOD13A2_061__1_km_16_days_NDVI)),
                     EVI = max(mean(.data$MOD13A2_061__1_km_16_days_EVI, na.rm=T), 0.0), n_EVI = sum(!is.na(.data$MOD13A2_061__1_km_16_days_EVI)), .groups = 'drop')
  file1_interp <- file1_interp |> dplyr::group_by(.data$ID) |>
    dplyr::mutate(NDVI_interp = zoo::na.approx(.data$NDVI, rule = 2), EVI_interp = zoo::na.approx(.data$EVI, rule = 2))

  file2    <- utils::read.csv(mod15_file)  ## Fpar and LAI
  bad_lai <- file2$MOD15A2H_061_FparLai_QC_MODLAND_Description == "Other Quality (back-up algorithm or fill values)"
  file2$MOD15A2H_061_Fpar_500m[bad_lai] <- NA
  file2$MOD15A2H_061_Lai_500m[bad_lai] <- NA
  file2$Date <- as.Date(file2$Date, format = "%m/%d/%y")
  file2_interp <- file2 |> dplyr::mutate(month = lubridate::month(.data$Date)) |> dplyr::group_by(.data$ID, .data$month) |>
    dplyr::summarise(Fpar = max(mean(.data$MOD15A2H_061_Fpar_500m, na.rm=T), 0.0), n_Fpar = sum(!is.na(.data$MOD15A2H_061_Fpar_500m)),
                     Lai = max(mean(.data$MOD15A2H_061_Lai_500m, na.rm=T), 0.0), n_Lai = sum(!is.na(.data$MOD15A2H_061_Lai_500m)), .groups = 'drop')
  file2_interp <- file2_interp |> dplyr::group_by(.data$ID) |>
    dplyr::mutate(Fpar_interp = zoo::na.approx(.data$Fpar, rule = 2), Lai_interp = zoo::na.approx(.data$Lai, rule = 2))

  file3  <- utils::read.csv(myd17_file)  ##GPP
  file3$MYD17A2HGF_061_Gpp_500m[file3$MYD17A2HGF_061_Psn_QC_500m_MODLAND_Description == "Other quality (back-up algorithm or fill values)"] <- NA
  file3$Date <- as.Date(file3$Date, format = "%m/%d/%y")
  file3_interp <- file3 |> dplyr::mutate(month = lubridate::month(.data$Date)) |> dplyr::group_by(.data$ID, .data$month) |>
    dplyr::summarise(GPP = max(mean(.data$MYD17A2HGF_061_Gpp_500m, na.rm=T), 0.0), n_GPP = sum(!is.na(.data$MYD17A2HGF_061_Gpp_500m)), .groups = 'drop')
  file3_interp <- file3_interp |> dplyr::group_by(.data$ID) |> dplyr::mutate(GPP_interp = zoo::na.approx(.data$GPP, rule = 2))

  VI <- file1_interp |> dplyr::group_by(.data$ID) |> dplyr::summarise(EVI=mean(.data$EVI_interp, na.rm=T), NDVI=mean(.data$NDVI_interp, na.rm=T))
  LAI <- file2_interp |> dplyr::group_by(.data$ID) |> dplyr::summarise(Fpar=mean(.data$Fpar_interp, na.rm=T), LAI=mean(.data$Lai_interp, na.rm=T))
  # GPP: kgC/m2/8d -> convert to kgC/m2/year
  GPP <- file3_interp |> dplyr::group_by(.data$ID) |> dplyr::summarise(GPP=mean(.data$GPP_interp, na.rm=T)/8*365.25)
  VI |> dplyr::left_join(LAI, by='ID') |> dplyr::left_join(GPP, by="ID")
}

# The driver-analysis table (03_01's acclimation_data.csv) for one set of TAS
# estimates: site metadata, climate statistics, MODIS indices, soil carbon,
# and total (TAS_tot) and direct (TAS) TAS with their p-values.
driver_data <- function(site_info, climate_stats, spectral, soil, tas_total, tas_direct) {
  tas_total <- tas_total |> dplyr::rename("TAS_tot" = "TAS", "TAS_totp" = "TASp")
  site_info[, c("site_ID", "LAT", "LONG", "ELEV", "IGBP", "Climate_class", "MAP")] |>
    dplyr::left_join(climate_stats[, c("site_ID", "NEE", "NEE_day", "NEE_night", "MATA", "SSTA", "IATA", "DRTA", "warm_rate", "warm_ratep")], by = "site_ID") |>
    dplyr::left_join(spectral[, c("ID", "EVI", "NDVI", "LAI", "GPP")], by=c("site_ID" = "ID")) |>
    dplyr::left_join(soil[, c('site_ID', 'SOC')], by = "site_ID") |>
    dplyr::left_join(tas_total[, c('site_ID', 'TAS_tot', 'TAS_totp')], by = "site_ID") |>
    dplyr::left_join(tas_direct[, c('site_ID', 'TAS', 'TASp')], by = "site_ID")
}

# Workflow step 03_02: random-forest attribution of thermal response strength
# to its drivers (elevation, mean annual air temperature, LAI, soil carbon and
# warming rate), for direct TAS, total TAS and apparent TAS (total - direct).
# Returns list(varImp, partial): mean permutation importance (%) over 50
# forests, and partial dependence with bootstrap quantiles (200 forests on
# climate-and-vegetation-stratified bootstrap samples). Random numbers are
# drawn in the published script's order from `seed` (985), so results are
# reproducible; the caret tuning step is kept only because it draws from the
# same random stream before the published forests.
driver_analysis <- function(acclimation, seed = 985) {
  acclimation$TAS_app <- acclimation$TAS_tot - acclimation$TAS
  acclimation$climate_vegetation <- paste0(acclimation$Climate_class, '_', acclimation$IGBP)
  data_TAS <- acclimation[, c("TAS", "ELEV", "MATA", "LAI", "SOC", "warm_rate")]

  set.seed(seed)
  invisible(caret::train(form = TAS ~ ., data = data_TAS, method = 'rf',
                         trControl = caret::trainControl(method = "cv", number = 5), tuneLength = 5))

  # direct TAS: bootstrap partial dependence, then importance
  partial_direct <- rf_partial_bootstrap(acclimation[, c("TAS", "ELEV", "MATA", "LAI", "SOC", "warm_rate", "climate_vegetation")], "TAS", data_TAS)
  imp_direct <- rf_importance(acclimation[, c("TAS", "ELEV", "MATA", "LAI", "SOC", "warm_rate")], "TAS")
  # total TAS: importance, then bootstrap partial dependence
  data_tot <- acclimation[, c("TAS_tot", "LAI", "ELEV", "MATA", "SOC", "warm_rate")]
  imp_total <- rf_importance(data_tot, "TAS_tot")
  partial_total <- rf_partial_bootstrap(acclimation[, c("TAS_tot", "ELEV", "MATA", "LAI", "SOC", "warm_rate", "climate_vegetation")], "TAS_tot", data_tot)
  # apparent TAS (total - direct): importance, then bootstrap partial dependence
  data_app <- acclimation[, c("TAS_app", "LAI", "ELEV", "MATA", "SOC", "warm_rate")]
  imp_app <- rf_importance(data_app, "TAS_app")
  partial_app <- rf_partial_bootstrap(acclimation[, c("TAS_app", "ELEV", "MATA", "LAI", "SOC", "warm_rate", "climate_vegetation")], "TAS_app", data_app)

  list(
    varImp = rbind(data.frame(TRS_type = "TAS_direct", imp_direct), data.frame(TRS_type = "TAS_tot", imp_total),
                   data.frame(TRS_type = "TAS_app", imp_app)),
    partial = rbind(data.frame(TRS_type = "TAS_direct", partial_direct), data.frame(TRS_type = "TAS_total", partial_total),
                    data.frame(TRS_type = "TAS_app", partial_app))
  )
}

# Mean non-negative permutation importance (type 1, scaled), as % of the
# total, over `times` forests of `response` on the other columns of `data`.
rf_importance <- function(data, response, times = 50) {
  formula <- stats::as.formula(paste(response, "~ ."))
  ri_var <- matrix(NA_real_, nrow = times, ncol = ncol(data) - 1)
  for (i in seq_len(times)) {
    rf0 <- randomForest::randomForest(formula = formula, data = data, do.trace = FALSE, mtry = 1, nodesize = 30, ntree = 500, importance = TRUE)
    ri_nonnegative <- pmax(randomForest::importance(rf0, type = 1, scale = TRUE), 0)
    ri_var[i, ] <- ri_nonnegative / sum(ri_nonnegative) * 100
  }
  data.frame(var = rownames(randomForest::importance(rf0, type = 1, scale = TRUE)), varImp = colMeans(ri_var, na.rm = TRUE))
}

# Partial dependence of `response` on each predictor, with 2.5/5/50/95/97.5%
# quantiles over forests fitted to `times` bootstrap samples of `data_strat`
# stratified by climate_vegetation, evaluated over `pred_data`.
rf_partial_bootstrap <- function(data_strat, response, pred_data, times = 200) {
  formula <- stats::as.formula(paste(response, "~ ."))
  vars <- c(elev = "ELEV", mat = "MATA", lai = "LAI", soc = "SOC", warm_rate = "warm_rate")
  strat_bootstrap <- rsample::bootstraps(data_strat, times = times, strata = "climate_vegetation")
  x <- NULL
  n_x <- NULL
  y <- vector("list", times)
  for (i in seq_len(times)) {
    data_sample <- rsample::analysis(strat_bootstrap$splits[[i]])[, 1:6]
    rf <- randomForest::randomForest(formula = formula, data = data_sample, do.trace = FALSE, mtry = 1, nodesize = 30, ntree = 500, importance = TRUE)
    # (do.call: partialPlot() takes the variable's name from substitute(x.var))
    pd <- lapply(vars, function(v) do.call(randomForest::partialPlot, list(rf, pred.data = pred_data, x.var = v, plot = FALSE)))
    if (i == 1) {
      x <- unlist(lapply(pd, `[[`, "x"), use.names = FALSE)
      # 51 grid points per predictor, or fewer with fewer distinct values
      n_x <- vapply(pd, function(p) length(p$x), integer(1))
    }
    y[[i]] <- unlist(lapply(pd, `[[`, "y"), use.names = FALSE)
  }
  y <- do.call(cbind, y)
  q <- t(apply(y, 1, stats::quantile, c(0.025, 0.05, 0.5, 0.95, 0.975)))
  partial <- data.frame(var = rep(names(vars), times = n_x), x = x,
                        y025 = q[, 1], y050 = q[, 2], y = q[, 3], y950 = q[, 4], y975 = q[, 5])
  partial$x_norm <- stats::ave(partial$x, partial$var, FUN = function(v) as.vector(scale(v)))
  partial
}
