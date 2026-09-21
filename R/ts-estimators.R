# The soil-temperature estimators, in one registry.
#
# Every model that produces a soil-temperature estimate from other columns --
# the manuscript's random forest and its regressions, the fill's candidates,
# the two borrowed-coefficient lines -- is an entry here with the same shape:
#
#   family      what kind of function of which predictors: "lm:TA",
#               "rf:TA+NETRAD", "fixed:TA". Two estimators with the same family
#               produce the same kind of column, whatever fitted them; this is
#               what provenance records and what "did stage A already apply
#               this?" is asked of.
#   predictors  the columns `fit` and `predict` read.
#   fit(train)  -> a model
#   predict(model, newdata) -> numeric, one value per row of `newdata`, NA
#               wherever a predictor is missing -- never a shorter vector,
#               which would misalign the estimate with the rows it belongs to.
#
# Stage A (`fix_soil_temp()` and the readers) and the fill draw from the same
# registry, so the manuscript's estimators and the alternatives are compared
# on equal terms. The manuscript's forest is `rf_ta_netrad_manuscript`
# (randomForest, its own 60k/70:30 protocol); the fill's forests are `ranger`
# and are not bit-identical to it -- see `rf_ranger_estimator()`.

ts_estimator <- function(family, predictors, fit, predict) {
  structure(
    list(family = family, predictors = predictors, fit = fit, predict = predict),
    class = "ts_estimator"
  )
}

ts_family <- function(kind, predictors) paste0(kind, ":", paste(predictors, collapse = "+"))

# `lm(TS ~ <predictors>)`. `fit_subset` restricts the rows the model is fitted
# on -- above freezing, recent years -- and is part of what the estimator *is*,
# since the manuscript's `TS ~ TA` line differs between sites only in that.
# `na.omit` drops incomplete rows, so callers can pass a subset that still
# contains NA rows (as `ac[ac$TA > 0, ]` does) and get the same coefficients.
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

# A line with coefficients taken from elsewhere rather than fitted here: US-Cwt
# borrows a neighbouring site's `TS ~ TA`, US-MBP fills its gaps with one.
# `fit` ignores its argument, which is the point -- the provenance row records
# that nothing at this site determined these numbers.
fixed_linear_estimator <- function(intercept, slope, predictor = "TA") {
  ts_estimator(
    family = ts_family("fixed", predictor),
    predictors = predictor,
    fit = function(train) list(intercept = intercept, slope = slope),
    predict = function(mod, newdata) mod$intercept + mod$slope * newdata[[predictor]]
  )
}

# `ranger` random forest: the fill's candidates. An order of magnitude faster
# than `randomForest` on the same algorithm and seeded, so blocked
# cross-validation over eight methods is affordable and repeatable.
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
      # ranger has no NA handling, so rows with an incomplete feature vector
      # get NA rather than being silently dropped -- which would misalign the
      # prediction with the rows it belongs to.
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

# The manuscript's random forest, as workflow 01_01 fitted it: `randomForest`
# on at most 60,000 rows drawn at random from the record, split 70/30 into
# training and a held-out set whose performance is reported beside a linear
# model's. Not seeded here -- inside the pipeline every call is in a target
# and `targets` seeds it; tests/ts-swc-baseline.R seeds it itself.
#
# The train/test diagnostics are the original's and are the only report of
# how well a reconstruction fits, so they are kept; they go through
# `message()` so that a caller can silence them.
rf_manuscript_estimator <- function(predictors = c("TA", "NETRAD"), max_rows = 60000) {
  ts_estimator(
    family = ts_family("rf", predictors),
    predictors = predictors,
    fit = function(train) {
      # use a maximum of 60000 data to train and test the model; too many data
      # will cause RF super slow and may not improve accuracy.
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

# The registry. `num_trees` reaches only the `ranger` forests; the fill's tests
# turn it down to make the plumbing cheap to exercise.
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

# The manuscript's `TS ~ TA` regression, as three functions the readers and
# the step-02 substitution have always called. Thin names over the registry
# so that there is one implementation of the line.
ts_ta_model <- function(fit_data) ts_estimators()$lm_ta$fit(fit_data)
predict_ts_from_ta <- function(mod, ta) ts_estimators()$lm_ta$predict(mod, data.frame(TA = ta))
