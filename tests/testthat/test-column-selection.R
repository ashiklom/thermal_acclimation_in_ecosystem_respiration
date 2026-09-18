# Choosing which soil-temperature and soil-water column the model is fitted on.
# The estimators themselves are tested in test-soil-temp-columns.R; this is
# about selection being unambiguous and failing by name when it cannot be.

use_project_root()

fake_tables <- function() {
  tibble::tibble(
    YEAR = 2010L, MONTH = 6L, DAY = 1L, DOY = 152L,
    TS = c(4, 5, 6),
    TS_measured = c(4, 5, 6),
    TS_linear = c(9, 10, 11),
    SWC = c(20, 21, 22),
    SWC_measured = c(20, 21, 22),
    SWC_era5 = c(30, 31, 32)
  )
}

# ------------------------------------------------------ soil temperature

test_that("resolve_ts_column materialises the requested column as TS", {
  dat <- fake_tables()
  expect_equal(resolve_ts_column(dat, "TS_measured")$TS, c(4, 5, 6))
  expect_equal(resolve_ts_column(dat, "TS_linear")$TS, c(9, 10, 11))
  # The source columns are left intact, so a later comparison is still possible.
  out <- resolve_ts_column(dat, "TS_linear")
  expect_equal(out$TS_measured, c(4, 5, 6))
  expect_equal(out$TS_linear, c(9, 10, 11))
})

test_that("a TS column the pipeline did not produce fails by name", {
  dat <- fake_tables()
  err <- expect_error(resolve_ts_column(dat, "TS_randomforest"), "TS_randomforest")
  # The message has to say what *is* available, or the next question is
  # unanswerable from the error alone.
  expect_match(conditionMessage(err), "TS_measured")
  expect_match(conditionMessage(err), "TS_linear")
})

test_that("ts_bounds_for returns the bounds belonging to the chosen column", {
  bounds <- tibble::tibble(
    ts_col = c("TS_measured", "TS_linear"),
    tStart = c(2.5, -1.25),
    tEnd = c(18.0, 21.5)
  )
  expect_equal(ts_bounds_for(bounds, "TS_measured"), list(tStart = 2.5, tEnd = 18.0))
  expect_equal(ts_bounds_for(bounds, "TS_linear"), list(tStart = -1.25, tEnd = 21.5))
})

test_that("bounds for an unproduced column are an error, not a silent fallback", {
  # Falling back to the first row would pair a substituted TS column with the
  # measured column's bounds -- the exact mismatch this design exists to stop.
  bounds <- tibble::tibble(ts_col = "TS_measured", tStart = 2.5, tEnd = 18.0)
  expect_error(ts_bounds_for(bounds, "TS_linear"), "TS_linear")
  expect_error(ts_bounds_for(bounds, "TS_linear"), "found 0")
})

# ------------------------------------------------------------ soil water

test_that("default_swc_col depends on the model as well as the site", {
  measured <- list(site_ID = "X-Msr", SWC_use = TRUE)
  none <- list(site_ID = "X-Non", SWC_use = FALSE)
  # Measured soil water is used whenever it exists, for either model.
  expect_equal(default_swc_col(measured, direct = FALSE), "SWC_measured")
  expect_equal(default_swc_col(measured, direct = TRUE), "SWC_measured")
  # Without it, only the direct model needs a fallback: soil water is in its
  # formula, and absent from the total model's.
  expect_true(is.na(default_swc_col(none, direct = FALSE)))
  expect_equal(default_swc_col(none, direct = TRUE), "SWC_era5")
})

test_that("resolve_swc_column materialises the requested column as SWC", {
  dat <- fake_tables()
  expect_equal(resolve_swc_column(dat, "SWC_measured", "X-Tst")$SWC, c(20, 21, 22))
  expect_equal(resolve_swc_column(dat, "SWC_era5", "X-Tst")$SWC, c(30, 31, 32))
  out <- resolve_swc_column(dat, "SWC_era5", "X-Tst")
  expect_equal(out$SWC_measured, c(20, 21, 22))
})

test_that("an absent soil-water column fails by name and names the site", {
  dat <- fake_tables()
  err <- expect_error(resolve_swc_column(dat, "SWC_satellite", "X-Tst"), "SWC_satellite")
  expect_match(conditionMessage(err), "X-Tst")
  expect_match(conditionMessage(err), "SWC_measured")
})

test_that("an empty ERA5 fallback tells the user what to run", {
  # This replaces the error `read_era5_swc()` used to raise at the same point
  # in the pipeline, so the failure mode is preserved rather than dropped.
  dat <- fake_tables()
  dat$SWC_era5 <- NA_real_
  err <- expect_error(resolve_swc_column(dat, "SWC_era5", "X-Tst"), "ERA5")
  expect_match(conditionMessage(err), "download_era5")
  expect_match(conditionMessage(err), "X-Tst")
})

test_that("an empty measured column is left to behave as before", {
  # Deliberately not guarded: scoping the emptiness check to the reanalysis
  # column keeps this change from altering any site that runs today.
  dat <- fake_tables()
  dat$SWC_measured <- NA_real_
  expect_silent(out <- resolve_swc_column(dat, "SWC_measured", "X-Tst"))
  expect_true(all(is.na(out$SWC)))
})
