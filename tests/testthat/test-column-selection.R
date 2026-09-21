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

test_that("materialise_ts_final leaves exactly one soil-temperature column", {
  dat <- fake_tables()
  expect_equal(materialise_ts_final(dat, "TS_measured")$TS_final, c(4, 5, 6))
  expect_equal(materialise_ts_final(dat, "TS_linear")$TS_final, c(9, 10, 11))
  # The candidates are gone, `TS` included: with them on the table "no
  # branching downstream" would be a convention, with them off it is a
  # property. Everything that is not soil temperature is untouched.
  out <- materialise_ts_final(dat, "TS_linear")
  expect_identical(ts_candidate_columns(out), "TS_final")
  expect_identical(setdiff(names(dat), names(out)), c("TS", "TS_measured", "TS_linear"))
  expect_equal(out$SWC_era5, dat$SWC_era5)
})

test_that("a TS column the pipeline did not produce fails by name", {
  dat <- fake_tables()
  err <- expect_error(materialise_ts_final(dat, "TS_randomforest"), "TS_randomforest")
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

test_that("total_tas_site refuses a site_info that belongs to another site", {
  # `site_info` is threaded in rather than read from the CSV, which means the
  # pairing is now something a caller can get wrong. A mismatch would fit one
  # site's observations against another site's declared `ts_col`/`SWC_use` --
  # it runs, produces numbers, and leaves nothing to notice downstream. The
  # guard is checked before any other work, so a stub is enough to reach it.
  stub <- list(feature_gs = tibble::tibble(site_ID = "NL-Loo"))
  expect_error(
    total_tas_site(stub, get_site_info("DE-RuC")),
    "site_info is for DE-RuC but site_data is for NL-Loo"
  )
  # ...and accepts the matching one: it gets past the guard and fails later on
  # the stub's missing tables, not on identity. Asserting which later error
  # comes first would just pin the order of the checks below it.
  expect_error(
    total_tas_site(stub, get_site_info("NL-Loo")),
    "^(?!.*site_info is for).*$", perl = TRUE
  )
})
