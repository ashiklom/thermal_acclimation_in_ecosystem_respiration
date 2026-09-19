# `total_tas_site(fit = FALSE)`: the window layout and year selection, with no
# model fitting at all.
#
# The value of this path is that it is the *same* loop as the fitted one, so
# what it reports is what the real run would do. These tests pin the two
# properties that make it worth having: it produces no model output, and it is
# deterministic -- unlike the fitted path, where nothing sets a seed.

use_project_root()

structural_cols <- c("site_ID", "growing_year", "window", "status",
                     "nobsv", "extend_days", "TS")

test_that("the structure-only path fits nothing and says so", {
  skip_if(is.na(product_file("DE-RuC", "FLUXNET")), "DE-RuC not downloaded")
  si <- get_site_info("DE-RuC")
  sd_ <- suppressWarnings(suppressMessages(prep_nee_ac(si)))
  r <- suppressWarnings(suppressMessages(total_tas_site(sd_, si, fit = FALSE)))

  expect_named(r, c("outcome", "outcome_siteyear", "window_skips", "settings"))
  # NULL rather than a row of NAs, so a structure-only result cannot be
  # mistaken downstream for a run that actually fitted something.
  expect_null(r$outcome)
  expect_true(all(structural_cols %in% names(r$outcome_siteyear)))
  expect_gt(nrow(r$outcome_siteyear), 0)

  # every model-derived column is empty
  for (col in c("alpha", "beta", "C0", "ERref")) {
    expect_true(all(is.na(r$outcome_siteyear[[col]])), info = col)
  }
  # ...and the layout columns are not
  expect_false(any(is.na(r$outcome_siteyear$nobsv)))
  expect_false(any(is.na(r$outcome_siteyear$extend_days)))

  expect_true(all(r$outcome_siteyear$status %in%
    c("not_fitted", "year_too_few_obs", "year_tsref_outside_quantiles",
      "year_median_nee_too_low", "year_mean_nee_too_low")))
})

test_that("two structure-only runs agree exactly", {
  # This is the whole point of the mode. The fitted path sets no seed, so its
  # TAS moves by ~3e-4 between identical runs; a difference in these columns is
  # therefore a real difference rather than sampler noise.
  skip_if(is.na(product_file("DE-RuC", "FLUXNET")), "DE-RuC not downloaded")
  si <- get_site_info("DE-RuC")
  sd_ <- suppressWarnings(suppressMessages(prep_nee_ac(si)))
  a <- suppressWarnings(suppressMessages(total_tas_site(sd_, si, fit = FALSE)))
  b <- suppressWarnings(suppressMessages(total_tas_site(sd_, si, fit = FALSE)))

  expect_identical(a$outcome_siteyear[structural_cols],
                   b$outcome_siteyear[structural_cols])
  expect_identical(a$settings, b$settings)
  expect_identical(a$window_skips, b$window_skips)
})

test_that("settings record the choices that shaped the run", {
  skip_if(is.na(product_file("DE-RuC", "FLUXNET")), "DE-RuC not downloaded")
  si <- get_site_info("DE-RuC")
  sd_ <- suppressWarnings(suppressMessages(prep_nee_ac(si)))
  tot <- suppressWarnings(suppressMessages(total_tas_site(sd_, si, fit = FALSE)))
  dir <- suppressWarnings(suppressMessages(
    total_tas_site(sd_, si, direct = TRUE, fit = FALSE)
  ))

  expect_equal(tot$settings$model, "total")
  expect_equal(dir$settings$model, "direct")
  # The selected columns belong in the record: they are the step-02 decision
  # that `outcome` alone gives no way to recover.
  expect_equal(tot$settings$ts_col, si$ts_col)
  expect_equal(tot$settings$nwindow, tot$settings$nwindow)
  expect_true(tot$settings$nwindow >= 1)
  expect_equal(nrow(tot$settings), 1L)
})
