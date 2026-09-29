# Drivers of direct, total and apparent thermal response strength, by random
# forest: relative importance of each predictor, and bootstrapped partial
# dependence. Reads data-proc/analysis/acclimation_data.csv; writes
# varImp_plot.csv and partial_plot.csv there.
# Authors: Junna Wang, October, 2025

stopifnot(
  requireNamespace("dplyr"), requireNamespace("car"), requireNamespace("caret"),
  requireNamespace("randomForest"), requireNamespace("rsample")
)

acclimation <- read.csv(file.path("data-proc", "analysis", "acclimation_data.csv"))

#------------------------do some explorations first-----------------------------
summary(lm(data = acclimation, TAS ~ IGBP))
summary(lm(data = acclimation, TAS ~ Climate_class))
summary(aov(TAS ~ IGBP, data = acclimation))
summary(aov(TAS ~ Climate_class, data = acclimation))
summary(lm(data = acclimation, TAS_tot ~ IGBP))
summary(lm(data = acclimation, TAS_tot ~ Climate_class))

# Do grouped climate and vegetation classes change the picture?
table(acclimation$Climate_class)
table(acclimation$IGBP)
acclimation <- acclimation |>
  dplyr::mutate(Climate_class_new = dplyr::case_when(
    Climate_class %in% c("Bsh", "Bsk") ~ "Bs",
    Climate_class %in% c("Bwk") ~ "Bw",
    Climate_class %in% c("Cfa", "Cfb", "Cfc") ~ "Cf",
    Climate_class %in% c("Csa") ~ "Csa",
    Climate_class %in% c("Csb") ~ "Csb",
    Climate_class %in% c("Dfa", "Dfb") ~ "Df",
    Climate_class %in% c("Dfc, Dfd", "Dwc") ~ "Subartic",
    Climate_class %in% c("ET") ~ "ET",
    Climate_class %in% c("Af") ~ "Af",
    Climate_class %in% c("Am") ~ "Am"
  )) |>
  # DBF and DNF are deciduous forests; SAV and WSA are savannas.
  dplyr::mutate(IGBP_new = dplyr::case_when(
    IGBP %in% c("CSH") ~ "CSH", IGBP %in% c("DBF") ~ "DBF", IGBP %in% c("EBF") ~ "EBF",
    IGBP %in% c("ENF") ~ "ENF", IGBP %in% c("GRA") ~ "GRA", IGBP %in% c("MF") ~ "MF",
    IGBP %in% c("OSH") ~ "OSH", IGBP %in% c("SAV", "WSA") ~ "SAV", IGBP %in% c("WET") ~ "WET"
  ))
summary(lm(data = acclimation, TAS_tot ~ Climate_class_new))
summary(lm(data = acclimation, TAS_tot ~ IGBP_new))
summary(lm(data = acclimation, TAS ~ Climate_class_new))
summary(lm(data = acclimation, TAS ~ IGBP_new))

#------------------------correlation among predictor variables------------------
data.cor <- acclimation[, c("ELEV", "MAP", "NEE", "NEE_day", "NEE_night", "MATA",
                            "SSTA", "IATA", "DRTA", "warm_rate", "warm_ratep",
                            "EVI", "NDVI", "LAI", "GPP", "TAS_tot")]
cor(data.cor)

#---------------contribution of direct and apparent thermal responses to TAS_tot----------
acclimation$TAS_app <- acclimation$TAS_tot - acclimation$TAS
(var(acclimation$TAS_app) + cov(acclimation$TAS_app, acclimation$TAS)) / var(acclimation$TAS_tot)
(var(acclimation$TAS) + cov(acclimation$TAS_app, acclimation$TAS)) / var(acclimation$TAS_tot)

#----------------------------simple linear regression-----------------------
summary(lm(data = acclimation, TAS ~ MATA + ELEV + LAI + SOC + warm_rate))
summary(lm(data = acclimation, TAS_tot ~ MATA + ELEV + LAI + SOC + warm_rate))

#----------------------random forests, as in the manuscript-------------------
data_TAS <- acclimation[, c("TAS", "ELEV", "MATA", "LAI", "SOC", "warm_rate")]
# variance inflation factors of the predictors
car::vif(lm(data = data_TAS, TAS ~ .))

# Every result below depends on the order of random draws after this seed,
# so neither the calls that consume them nor their order may change. The
# caret CV (it only tunes mtry) is one of them: it is kept for its draws.
set.seed(985)
invisible(caret::train(
  form = TAS ~ ., data = data_TAS, method = "rf",
  trControl = caret::trainControl(method = "cv", number = 5), tuneLength = 5
))

acclimation$climate_vegetation <- paste0(acclimation$Climate_class, "_", acclimation$IGBP)

fit_rf <- function(formula, data) {
  randomForest::randomForest(formula = formula, data = data, do.trace = FALSE,
                             mtry = 1, nodesize = 30, ntree = 500, importance = TRUE)
}

# Partial dependence of `response` on each predictor, with a bootstrap
# interval: 200 forests on stratified (climate x vegetation) resamples, each
# evaluated over `pred_data`.
boot_partial <- function(response, pred_data, times = 200) {
  vars <- c(elev = "ELEV", mat = "MATA", lai = "LAI", soc = "SOC", warm_rate = "warm_rate")
  strat_bootstrap <- rsample::bootstraps(
    acclimation[, c(response, vars, "climate_vegetation")],
    times = times, strata = climate_vegetation
  )
  formula <- stats::as.formula(paste(response, "~ ."))
  curves <- lapply(seq_len(times), function(i) {
    message(response, " bootstrap ", i)
    data_sample <- rsample::analysis(strat_bootstrap$splits[[i]])
    # The stratifier is not a predictor.
    data_sample <- data_sample[, setdiff(names(data_sample), "climate_vegetation")]
    rf <- fit_rf(formula, data_sample)
    # do.call: partialPlot() reads `x.var` with substitute().
    lapply(vars, function(v) {
      do.call(randomForest::partialPlot, list(rf, pred.data = pred_data, x.var = v, plot = FALSE))
    })
  })
  do.call(rbind, lapply(names(vars), function(label) {
    x <- curves[[1]][[label]]$x
    y <- sapply(curves, function(curve) curve[[label]]$y)
    q <- t(apply(y, 1, quantile, c(0.025, 0.05, 0.5, 0.95, 0.975)))
    data.frame(var = label, x = x, y025 = q[, 1], y050 = q[, 2], y = q[, 3], y950 = q[, 4], y975 = q[, 5],
               x_norm = as.numeric(scale(x)))
  }))
}

# Relative importance (%, negative importances as zero) of each predictor of
# `data`'s first column, averaged over 50 forests.
rf_importance <- function(trs_type, data) {
  formula <- stats::as.formula(paste(names(data)[[1]], "~ ."))
  ri <- matrix(NA_real_, nrow = 50, ncol = ncol(data) - 1)
  for (i in 1:50) {
    rf0 <- fit_rf(formula, data)
    ri_nonnegative <- pmax(randomForest::importance(rf0, type = 1, scale = TRUE), 0)
    ri[i, ] <- ri_nonnegative / sum(ri_nonnegative) * 100
  }
  print(caret::postResample(pred = predict(rf0, data), obs = data[[1]]))
  data.frame(
    TRS_type = trs_type,
    var = rownames(randomForest::importance(rf0, type = 1, scale = TRUE)),
    varImp = colMeans(ri, na.rm = TRUE)
  )
}

# The draws run in this order: direct bootstrap, direct importance, total
# importance, total bootstrap, apparent importance, apparent bootstrap.
partial_direct <- boot_partial("TAS", data_TAS)
varImp_direct <- rf_importance("TAS_direct", data_TAS)
data_TAS_tot <- acclimation[, c("TAS_tot", "LAI", "ELEV", "MATA", "SOC", "warm_rate")]
varImp_total <- rf_importance("TAS_tot", data_TAS_tot)
partial_total <- boot_partial("TAS_tot", data_TAS_tot)
data_TAS_app <- acclimation[, c("TAS_app", "LAI", "ELEV", "MATA", "SOC", "warm_rate")]
varImp_app <- rf_importance("TAS_app", data_TAS_app)
partial_app <- boot_partial("TAS_app", data_TAS_app)

partial_output <- rbind(
  data.frame(TRS_type = "TAS_direct", partial_direct),
  data.frame(TRS_type = "TAS_total", partial_total),
  data.frame(TRS_type = "TAS_app", partial_app)
)
varImp_output <- rbind(varImp_direct, varImp_total, varImp_app)

dir.create("data-proc/analysis", recursive = TRUE, showWarnings = FALSE)
write.csv(partial_output, file = file.path("data-proc", "analysis", "partial_plot.csv"), row.names = FALSE)
write.csv(varImp_output, file = file.path("data-proc", "analysis", "varImp_plot.csv"), row.names = FALSE)
