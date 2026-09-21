# `get_soil_temperature()`: the one place a run's soil temperature is decided.
#
# What is pinned here is the *contract*, not the estimators (those are in
# test-soil-temp-columns.R) or the strategies' choices (test-recipes.R): one
# column leaves, its bounds belong to it, its provenance is recorded, and the
# selection reproduces what `total_tas_site()` did before the seam existed.

use_project_root()

fake_site_data <- function(verdict = "GOOD") {
  tbl <- tibble::tibble(
    YEAR = 2010L, MONTH = 6L, DAY = 1L, DOY = 152L, HOUR = 1:3, MINUTE = 0L,
    TS = c(4, 5, 6), TS_measured = c(4, 5, 6), TS_linear = c(9, 10, 11),
    TA = c(6, 7, 8), NEE = c(1, 2, 3), SWC = c(20, 21, 22)
  )
  list(
    ac = tbl, nightNEE = tbl[1:2, ],
    feature_gs = tibble::tibble(site_ID = "X-Tst", gStart = 120, gEnd = 280),
    ts_bounds = tibble::tibble(
      ts_col = c("TS_measured", "TS_measured", "TS_measured", "TS_linear", "TS_linear"),
      definition = c("climatology", "halfhourly", "climatology", "halfhourly", "climatology"),
      native = c(TRUE, FALSE, FALSE, TRUE, FALSE),
      tStart = c(5, 1, 2, 0, 3), tEnd = c(20, 25, 19, 26, 22)
    ),
    ts_qc = tibble::tibble(site_ID = "X-Tst", verdict = verdict, flags = "")
  )
}
fake_site_info <- function(ts_col = "TS_measured") {
  list(site_ID = "X-Tst", ts_col = ts_col, estimate_Ts = FALSE)
}
fake_fill <- function(n_ac = 3, n_night = 2) {
  list(
    status = "ok", method = "rf_memory", cv_rmse = 0.8, degenerate = FALSE,
    truth_synthetic = FALSE,
    ac_ts = c(40, 50, 60)[seq_len(n_ac)], night_ts = c(40, 50)[seq_len(n_night)],
    ts_bounds = tibble::tibble(
      ts_col = "TS_memfill", definition = c("halfhourly", "climatology"),
      native = FALSE, tStart = c(-2, -1), tEnd = c(30, 29)
    )
  )
}

test_that("exactly one soil-temperature column leaves, on both tables", {
  soil <- get_soil_temperature(fake_site_data(), fake_site_info(), original_recipe())
  expect_identical(ts_candidate_columns(soil$ac), "TS_final")
  expect_identical(ts_candidate_columns(soil$nightNEE), "TS_final")
  expect_equal(soil$ac$TS_final, c(4, 5, 6))
  expect_equal(soil$nightNEE$TS_final, c(4, 5))
  # and the non-soil-temperature columns are exactly what came in
  expect_equal(soil$ac$TA, c(6, 7, 8))
  expect_equal(nrow(soil$ac), 3L)
})

test_that("the bounds belong to the column that was selected", {
  meas <- get_soil_temperature(fake_site_data(), fake_site_info("TS_measured"), original_recipe())
  lin <- get_soil_temperature(fake_site_data(), fake_site_info("TS_linear"), original_recipe())
  # `original` is `native` bounds: the measured column's climatology row and
  # the regressed column's half-hourly row -- the manuscript's asymmetry.
  expect_equal(c(meas$meta$tStart, meas$meta$tEnd), c(5, 20))
  expect_equal(c(lin$meta$tStart, lin$meta$tEnd), c(0, 26))
  expect_equal(lin$ac$TS_final, c(9, 10, 11))
  # A consistent-bounds recipe changes the row but not the column.
  hh <- get_soil_temperature(fake_site_data(), fake_site_info("TS_measured"),
                             get_recipe("memfill_hh"))
  expect_equal(c(hh$meta$tStart, hh$meta$tEnd), c(1, 25))
  expect_equal(hh$ac$TS_final, c(4, 5, 6))
})

test_that("the metadata row says what TS_final is and why", {
  soil <- get_soil_temperature(fake_site_data(), fake_site_info("TS_linear"), original_recipe())
  m <- soil$meta
  expect_equal(nrow(m), 1L)
  expect_identical(m$site_ID, "X-Tst")
  expect_identical(m$ts_col, "TS_linear")
  expect_identical(m$ts_strategy, "site_info")
  expect_match(m$ts_reason, "site_info")
  expect_identical(m$ts_verdict, "GOOD")
  expect_false(m$ts_measured_synthetic)
  expect_true(is.na(m$fill_method))
  expect_identical(m$bounds_strategy, "native")
})

test_that("the fill is attached only when selected, and its rows must align", {
  bad <- fake_site_data("BAD")
  si <- fake_site_info()
  r <- get_recipe("memfill")

  soil <- get_soil_temperature(bad, si, r, fill = fake_fill())
  expect_identical(soil$meta$ts_col, "TS_memfill")
  expect_equal(soil$ac$TS_final, c(40, 50, 60))
  expect_equal(soil$nightNEE$TS_final, c(40, 50))
  expect_identical(soil$meta$fill_method, "rf_memory")
  expect_equal(c(soil$meta$tStart, soil$meta$tEnd), c(-2, 30)) # native for a fill: half-hourly
  expect_identical(ts_candidate_columns(soil$ac), "TS_final")

  # A GOOD verdict never touches the fill, so a broken one is harmless there.
  good <- get_soil_temperature(fake_site_data("GOOD"), si, r, fill = fake_fill(n_ac = 1))
  expect_identical(good$meta$ts_col, "TS_measured")

  # A misaligned fill is refused rather than written in the wrong rows.
  expect_error(get_soil_temperature(bad, si, r, fill = fake_fill(n_ac = 2)))
  # No fill at all: the strategy falls back to the regression and records why
  # -- that is `choose_ts_col()`'s documented behaviour, not an error.
  fb <- get_soil_temperature(bad, si, r, fill = NULL)
  expect_identical(fb$meta$ts_col, "TS_linear")
  expect_match(fb$meta$ts_reason, "fell back")
  # Only an explicit request for the fill, with none supplied, is an error.
  expect_error(get_soil_temperature(bad, si, r, fill = NULL, ts_col = "TS_memfill"), "TS_memfill")
})

test_that("ts_col overrides the strategy and says so", {
  soil <- get_soil_temperature(fake_site_data(), fake_site_info("TS_measured"),
                               original_recipe(), ts_col = "TS_linear")
  expect_identical(soil$meta$ts_col, "TS_linear")
  expect_match(soil$meta$ts_reason, "argument")
  expect_equal(soil$ac$TS_final, c(9, 10, 11))
})

test_that("a site_data without ts_bounds is refused by name", {
  sd_ <- fake_site_data()
  sd_$ts_bounds <- NULL
  expect_error(get_soil_temperature(sd_, fake_site_info(), original_recipe()), "ts_bounds")
})

# The seam, on real data: `total_tas_site()` now reads `TS_final` from this
# function, and the structural run has to be what it was.
test_that("total_tas_site's soil temperature is get_soil_temperature's, at a real site", {
  skip_if(!site_raw_available("DE-RuC"), "DE-RuC not downloaded")
  si <- get_site_info("DE-RuC")
  sd_ <- prepped_site("DE-RuC")
  soil <- get_soil_temperature(sd_, si, original_recipe())
  r <- suppressWarnings(suppressMessages(total_tas_site(sd_, si, fit = FALSE)))
  expect_identical(r$settings$ts_col, soil$meta$ts_col)
  expect_equal(r$settings$tStart, soil$meta$tStart)
  expect_equal(r$settings$tEnd, soil$meta$tEnd)
  expect_identical(r$settings$ts_reason, soil$meta$ts_reason)
  # DE-RuC declares TS_linear, so the fit-facing column is the regression and
  # the qualification-facing one it replaced is no longer on the table.
  expect_identical(soil$meta$ts_col, "TS_linear")
  expect_false("TS_measured" %in% names(soil$ac))
})
