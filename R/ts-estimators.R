# The soil-temperature estimators, in one registry.
#
# Every model that estimates soil temperature from other columns (stage A's,
# the fill's) has the same shape:
#
#   family      the kind of function of which predictors ("lm:TA",
#               "rf:TA+NETRAD", "fixed:TA"); what provenance records
#   predictors  the columns `fit` and `predict` read
#   fit(train)  -> a model
#   predict(model, newdata) -> one value per row of `newdata`, NA where a
#               predictor is missing (never a shorter vector)

#' Construct a soil-temperature estimator
#'
#' @param family The kind of function of which predictors, as `ts_family()`
#'   spells it.
#' @param predictors Character vector of the columns `fit` and `predict` read.
#' @param fit Function of a training data frame, returning a model.
#' @param predict Function of a model and `newdata`, returning one value per row
#'   of `newdata`.
#' @return A `ts_estimator`: a list of `family`, `predictors`, `fit` and
#'   `predict`.
ts_estimator <- function(family, predictors, fit, predict) {
  structure(
    list(family = family, predictors = predictors, fit = fit, predict = predict),
    class = "ts_estimator"
  )
}

#' The family label of an estimator
#'
#' @param kind Kind of model: `"lm"`, `"rf"` or `"fixed"`.
#' @param predictors Character vector of predictor column names.
#' @return A string `"<kind>:<predictor>+<predictor>"`, e.g. `"rf:TA+NETRAD"`.
ts_family <- function(kind, predictors) paste0(kind, ":", paste(predictors, collapse = "+"))

#' Linear-regression soil-temperature estimator
#'
#' `lm(TS ~ <predictors>)`, fitted on the rows `fit_subset` keeps (above
#' freezing, recent years). `na.omit` makes NA rows in the input harmless.
#'
#' @param predictors Character vector of predictor column names.
#' @param fit_subset Function of the training data returning a logical row
#'   index, or `NULL` to fit on every row.
#' @return A `ts_estimator`.
lm_estimator <- function(predictors, fit_subset = NULL) {
  ts_estimator(
    family = ts_family("lm", predictors),
    predictors = predictors,
    fit = function(train) {
      if (!is.null(fit_subset)) train <- train[fit_subset(train), , drop = FALSE]
      stats::lm(stats::reformulate(predictors, "TS"), data = train, na.action = stats::na.omit)
    },
    predict = function(mod, newdata) {
      as.numeric(stats::predict(mod, newdata = newdata, na.action = stats::na.pass))
    }
  )
}

#' Fixed-coefficient linear soil-temperature estimator
#'
#' A line with coefficients from elsewhere (US-Cwt's neighbour, US-MBP's gap
#' fill); `fit` ignores its data.
#'
#' @param intercept Intercept of the line (degrees C).
#' @param slope Slope of the line on `predictor`.
#' @param predictor Name of the single predictor column.
#' @return A `ts_estimator`.
fixed_linear_estimator <- function(intercept, slope, predictor = "TA") {
  ts_estimator(
    family = ts_family("fixed", predictor),
    predictors = predictor,
    fit = function(train) list(intercept = intercept, slope = slope),
    predict = function(mod, newdata) mod$intercept + mod$slope * newdata[[predictor]]
  )
}

#' `ranger` forest for the fill
#'
#' Much faster than `randomForest`, and seeded.
#'
#' @param predictors Character vector of predictor column names.
#' @param num_trees Number of trees.
#' @return A `ts_estimator`. Its `fit` errors on fewer than 50 complete
#'   training rows.
rf_ranger_estimator <- function(predictors, num_trees = 200) {
  ts_estimator(
    family = ts_family("rf", predictors),
    predictors = predictors,
    fit = function(train) {
      ok <- stats::complete.cases(train[, c("TS", predictors), drop = FALSE])
      if (sum(ok) < 50) stop("fewer than 50 complete training rows")
      ranger::ranger(
        x = as.data.frame(train[ok, predictors, drop = FALSE]),
        y = train$TS[ok],
        num.trees = num_trees,
        num.threads = 1,
        seed = 222
      )
    },
    predict = function(mod, newdata) {
      # ranger drops incomplete rows; give them NA instead.
      out <- rep(NA_real_, nrow(newdata))
      ok <- stats::complete.cases(newdata[, mod$forest$independent.variable.names, drop = FALSE])
      if (any(ok)) {
        out[ok] <- stats::predict(
          mod, data = as.data.frame(newdata[ok, , drop = FALSE]), num.threads = 1
        )$predictions
      }
      out
    }
  )
}

#' The manuscript's random-forest soil-temperature estimator
#'
#' The manuscript's forest, as workflow 01_01 fitted it: `randomForest` on at
#' most 60,000 random rows split 70/30, with the original's train/test report
#' (via `message()`). Unseeded here; targets seeds each target.
#'
#' @param predictors Character vector of predictor column names.
#' @param max_rows Most rows sampled from the training data.
#' @return A `ts_estimator`.
rf_manuscript_estimator <- function(predictors = c("TA", "NETRAD"), max_rows = 60000) {
  ts_estimator(
    family = ts_family("rf", predictors),
    predictors = predictors,
    fit = function(train) {
      sampled <- sample(seq_len(nrow(train)), size = min(max_rows, nrow(train)), replace = FALSE)
      ind <- sample(2, length(sampled), replace = TRUE, prob = c(0.7, 0.3))
      fit_rows <- stats::na.omit(train[sampled[ind == 1], ])
      test_rows <- stats::na.omit(train[sampled[ind == 2], ])
      formula <- stats::reformulate(predictors, "TS")

      rf <- randomForest::randomForest(formula = formula, data = fit_rows) # this takes lots of time
      lm <- stats::lm(formula, data = train)

      report <- function(label, x) {
        message(label, "\n", paste(utils::capture.output(print(x)), collapse = "\n"))
      }
      report("Training data performance by random forests:",
             caret::postResample(pred = stats::predict(rf, fit_rows), obs = fit_rows$TS))
      report("Testing data performance by random forests:",
             caret::postResample(pred = stats::predict(rf, test_rows), obs = test_rows$TS))
      # compare with linear regression
      report("Performance by linear regression:", summary(lm))
      # random forest is much better than lm;
      rf
    },
    predict = function(mod, newdata) as.numeric(stats::predict(mod, newdata))
  )
}

#' The registry of soil-temperature estimators
#'
#' @param num_trees Number of trees. Reaches only the `ranger` forests.
#' @return Named list of `ts_estimator`s: the manuscript's (`lm_ta`, `lm_ta_pos`,
#'   `lm_ta_recent`, `lm_ta_netrad`, `rf_ta_netrad_manuscript`) and the fill's.
ts_estimators <- function(num_trees = 200) {
  memory <- TS_FILL_FEATURES_MEMORY
  list(
    # -- the manuscript's
    lm_ta = lm_estimator("TA"),
    lm_ta_pos = lm_estimator("TA", fit_subset = function(d) !is.na(d$TA) & d$TA > 0),
    lm_ta_recent = lm_estimator("TA", fit_subset = function(d) !is.na(d$TA) & d$YEAR > 2021 & d$TA > 0),
    lm_ta_netrad = lm_estimator(c("TA", "NETRAD")),
    rf_ta_netrad_manuscript = rf_manuscript_estimator(c("TA", "NETRAD")),
    # -- the fill's
    rf_ta = rf_ranger_estimator("TA", num_trees),
    rf_ta_netrad = rf_ranger_estimator(c("TA", "NETRAD"), num_trees),
    lm_memory = lm_estimator(memory),
    rf_memory = rf_ranger_estimator(memory, num_trees),
    rf_memory_netrad = rf_ranger_estimator(c(memory, "NETRAD"), num_trees)
  )
}

#' The manuscript's `TS ~ TA` line, by name
#'
#' @param fit_data Data frame with `TS` and `TA` columns, as `ts_fit_data()`
#'   returns.
#' @return The fitted `lm`.
ts_ta_model <- function(fit_data) ts_estimators()$lm_ta$fit(fit_data)
#' Predict soil temperature from the manuscript's `TS ~ TA` line
#'
#' @param mod A model from `ts_ta_model()`.
#' @param ta Numeric vector of air temperature.
#' @return Numeric vector of predicted soil temperature, one per `ta`; NA where
#'   `ta` is.
predict_ts_from_ta <- function(mod, ta) ts_estimators()$lm_ta$predict(mod, data.frame(TA = ta))
