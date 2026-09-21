# The estimator registry: one shape, one implementation of each line, and the
# manuscript's estimators reproduced exactly.

use_project_root()

est_fixture <- function(n = 600, seed = 7) {
  set.seed(seed)
  ta <- 10 + 8 * sin(seq(0, 6 * pi, length.out = n)) + rnorm(n, 0, 2)
  netrad <- 80 * pmax(sin(seq(0, 60 * pi, length.out = n)), 0) - 20 + rnorm(n, 0, 5)
  ts <- 0.6 * ta + 0.01 * netrad + 3 + rnorm(n, 0, 1)
  ta[seq(5, n, by = 41)] <- NA
  ts[seq(9, n, by = 53)] <- NA
  tibble::tibble(
    TIMESTAMP = as.POSIXct("2015-01-01", tz = "UTC") + 1800 * seq_len(n),
    YEAR = c(rep(2019L, n %/% 2), rep(2022L, n - n %/% 2)),
    DOY = rep(1:30, length.out = n), HOUR = rep(0:23, length.out = n), MINUTE = 0L,
    TS = ts, TA = ta, NETRAD = netrad
  )
}

test_that("every registry entry has the shape stage A and the fill rely on", {
  reg <- ts_estimators(num_trees = 20)
  expect_true(all(vapply(reg, inherits, logical(1), "ts_estimator")))
  for (nm in names(reg)) {
    e <- reg[[nm]]
    expect_named(e, c("family", "predictors", "fit", "predict"), info = nm)
    expect_match(e$family, "^(lm|rf|fixed):", info = nm)
    expect_true(all(e$predictors %in% c("TA", "NETRAD", TS_FILL_FEATURES_MEMORY)), info = nm)
  }
  # The families that matter downstream are spelled as provenance expects.
  expect_identical(reg$lm_ta$family, "lm:TA")
  expect_identical(reg$lm_ta_pos$family, "lm:TA")
  expect_identical(reg$lm_ta_netrad$family, "lm:TA+NETRAD")
  expect_identical(reg$rf_ta_netrad$family, "rf:TA+NETRAD")
  expect_identical(reg$rf_ta_netrad_manuscript$family, "rf:TA+NETRAD")
})

test_that("predict returns one value per row, NA where a predictor is missing", {
  d <- est_fixture()
  reg <- ts_estimators(num_trees = 20)
  for (nm in c("lm_ta", "lm_ta_pos", "lm_ta_netrad", "rf_ta", "rf_ta_netrad")) {
    p <- reg[[nm]]$predict(reg[[nm]]$fit(d), d)
    expect_length(p, nrow(d))
    expect_null(names(p))
    expect_true(all(is.na(p[is.na(d$TA)])), info = nm)
    expect_false(anyNA(p[!is.na(d$TA) & !is.na(d$NETRAD)]), info = nm)
  }
})

test_that("the lm estimators are the manuscript's regressions, coefficient for coefficient", {
  d <- est_fixture()
  reg <- ts_estimators()
  # `TS ~ TA` on every complete row
  expect_equal(coef(reg$lm_ta$fit(d)), coef(lm(TS ~ TA, data = d, na.action = na.omit)))
  # ...above freezing, as `ac[ac$TA > 0, ]` selected it: NA rows in, dropped again by na.omit
  expect_equal(coef(reg$lm_ta_pos$fit(d)), coef(lm(TS ~ TA, data = d[d$TA > 0, ], na.action = na.omit)))
  # ...on recent years above freezing, US-BZo's rule
  expect_equal(coef(reg$lm_ta_recent$fit(d)),
               coef(lm(TS ~ TA, data = d[d$YEAR > 2021 & d$TA > 0, ], na.action = na.omit)))
  # ...with net radiation, DE-Hte's and FR-FBn's
  expect_equal(coef(reg$lm_ta_netrad$fit(d)), coef(lm(TS ~ TA + NETRAD, data = d, na.action = na.omit)))
  # and the two public names are the registry's line
  expect_equal(coef(ts_ta_model(d)), coef(reg$lm_ta$fit(d)))
  expect_equal(predict_ts_from_ta(ts_ta_model(d), d$TA), reg$lm_ta$predict(reg$lm_ta$fit(d), d))
})

test_that("a fixed line predicts from its coefficients and fits nothing", {
  d <- est_fixture()
  e <- fixed_linear_estimator(intercept = 5.13873, slope = 0.64718)   # US-Cwt's
  expect_identical(e$family, "fixed:TA")
  # `fit` ignores the data: the same coefficients whatever it is shown.
  expect_identical(e$fit(d), e$fit(d[1:3, ]))
  expect_equal(e$predict(e$fit(d), d), d$TA * 0.64718 + 5.13873)
})

test_that("the manuscript's forest is randomForest under its own protocol", {
  # Transcribed from workflow 01_01's `predict_soil_temp()`: the same draws in
  # the same order under the same seed give the same forest.
  d <- est_fixture()
  e <- ts_estimators()$rf_ta_netrad_manuscript

  set.seed(222)
  got <- suppressMessages(e$predict(e$fit(d), d))

  set.seed(222)
  sampled <- sample(1:nrow(d), size = min(60000, nrow(d)), replace = FALSE)
  ind <- sample(2, length(sampled), replace = TRUE, prob = c(0.7, 0.3))
  train <- na.omit(d[sampled[ind == 1], ])
  rf <- randomForest::randomForest(formula = TS ~ TA + NETRAD, data = train)
  want <- unname(predict(rf, d))

  expect_equal(got, want)
  expect_length(got, nrow(d))
})

test_that("fix_soil_temp routes each estimate_ts_method to its registry entry and says so", {
  d <- est_fixture()
  a <- d
  names(a)[names(a) == "TS"] <- "TS_F_MDS_1"
  names(a)[names(a) == "TA"] <- "TA_F_MDS"
  a$TS_F_MDS_1_QC <- 0
  base <- list(site_ID = "X-Tst", source = "FLUXNET", estimate_Ts = TRUE, netrad_column = "NETRAD")

  lin <- suppressMessages(fix_soil_temp(a, c(base, estimate_ts_method = "linear regression")))
  expect_identical(attr(lin, "estimator"), "lm_ta_netrad")
  expect_identical(attr(lin, "family"), "lm:TA+NETRAD")
  expect_named(lin, c("TIMESTAMP", "TS_pred"))
  expect_equal(nrow(lin), nrow(a))
  expect_gt(attr(lin, "n_train"), 0)

  ta_only <- suppressMessages(fix_soil_temp(a, c(base, estimate_ts_method = NA_character_)))
  expect_identical(attr(ta_only, "estimator"), "lm_ta_pos")
  expect_identical(attr(ta_only, "family"), "lm:TA")
})
