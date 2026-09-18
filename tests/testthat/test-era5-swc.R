use_project_root()

era5_path <- file.path("data-raw", "ERA5_daily_swc.csv")

test_that("volumetric fractions are converted to percent", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  write.csv(
    data.frame(time = c("1990-01-01", "1990-01-02"), site = "X-Tst", SWC = c(0.25, 0.4)),
    tmp, row.names = FALSE
  )
  swc <- read_era5_swc("X-Tst", tmp)
  expect_equal(names(swc), c("YEAR", "MONTH", "DAY", "SWC"))
  expect_equal(swc$SWC, c(25, 40))
  expect_equal(swc$YEAR, c(1990, 1990))
  expect_equal(swc$DAY, c(1, 2))
})

test_that("already-rescaled input is rejected rather than doubled", {
  # The regression this guards: the manuscript's file was in percent, the new
  # ERA5-Land download is in m3/m3, and nothing noticed the 100x change.
  tmp <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(time = "1990-01-01", site = "X-Tst", SWC = 28.6), tmp, row.names = FALSE)
  expect_error(read_era5_swc("X-Tst", tmp), "volumetric fraction")
})

test_that("an unknown site is an error, not a silent empty join", {
  skip_if_not(file.exists(era5_path), "ERA5 soil water download not present")
  expect_error(read_era5_swc("NO-Such", era5_path), "No ERA5 soil water data")
})

test_that("the converted US-Kon series matches the manuscript's own ERA5 file", {
  manuscript <- file.path(
    "..", "original", "Demo_code_data_for_1site", "ERA5_daily_swc_1990_2024_1site.csv"
  )
  skip_if_not(file.exists(era5_path), "ERA5 soil water download not present")
  skip_if_not(file.exists(manuscript), "manuscript reference ERA5 file not present")

  ours <- read_era5_swc("US-Kon", era5_path)
  theirs <- read.csv(manuscript)
  # Independent downloads over different spans, so compare distributions rather
  # than rows. This is the check that establishes the unit convention.
  expect_equal(mean(ours$SWC), mean(theirs$SWC), tolerance = 0.02)
  expect_equal(min(ours$SWC), min(theirs$SWC), tolerance = 0.02)
  expect_equal(max(ours$SWC), max(theirs$SWC), tolerance = 0.02)
})
