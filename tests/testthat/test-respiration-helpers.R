use_project_root()

test_that("parse_removed_years handles every form site_info uses", {
  expect_equal(parse_removed_years(NA_character_), numeric())
  expect_equal(parse_removed_years(""), numeric())
  expect_equal(parse_removed_years("2012"), 2012)
  expect_equal(parse_removed_years("2007, 2008, 2010"), c(2007, 2008, 2010))
  expect_equal(parse_removed_years("2007:2009"), c(2007, 2008, 2009))
  expect_equal(parse_removed_years("2008,  2012, 2013, 2022"), c(2008, 2012, 2013, 2022))
  expect_equal(parse_removed_years("2001, 2005:2007"), c(2001, 2005, 2006, 2007))
})

test_that("compute_gap_thresholds keeps the three original branches distinct", {
  # Tundra and tropical AmeriFlux sites (01_02b:385)
  expect_equal(
    compute_gap_thresholds(120, 260, "US-ICt"),
    list(gap_max_thresh = 60, gap_total_thresh = 0.8)
  )
  # Sparse EuroFlux sites (01_02a:210)
  expect_equal(
    compute_gap_thresholds(120, 260, "ZA-Kru"),
    list(gap_max_thresh = 60, gap_total_thresh = 0.7)
  )
  # Everything else, derived from the growing-season length
  expected_max <- max(31, 141 * 0.225)
  expect_equal(
    compute_gap_thresholds(120, 260, "DE-Tha"),
    list(gap_max_thresh = expected_max, gap_total_thresh = max(1 / 3, expected_max / 141))
  )
})

test_that("compute_gap_thresholds floors a short growing season at 31 days", {
  expect_equal(compute_gap_thresholds(180, 200, "DE-Tha")$gap_max_thresh, 31)
})

test_that("build_gs_dates puts one season start and one end in every year", {
  d <- build_gs_dates(120, 260, 2010, 2014, as.difftime(30, units = "mins"),
                      southern_hemisphere = FALSE)
  expect_equal(nrow(d), 10L)
  expect_equal(sort(unique(d$DOY)), c(120, 260))
  expect_equal(length(unique(lubridate::year(d$TIMESTAMP))), 5L)
  expect_equal(lubridate::yday(d$TIMESTAMP[d$DOY == 120]), rep(120, 5))
})

test_that("build_gs_dates unwraps southern-hemisphere DOY for the timestamp only", {
  d <- build_gs_dates(185, 380, 2010, 2012, as.difftime(30, units = "mins"),
                      southern_hemisphere = TRUE)
  # DOY stays in the shifted frame, because that is what the gap scan compares
  # against, but the timestamp has to be a real calendar date.
  expect_equal(sort(unique(d$DOY)), c(185, 380))
  expect_equal(sort(unique(lubridate::yday(d$TIMESTAMP))), c(14, 185))
})
