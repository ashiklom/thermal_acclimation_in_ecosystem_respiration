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

test_that("a site whose rows are all NA is named as such, not as a unit problem", {
  # ERA5-Land is masked to land and `scripts/download-era5-swc.py` takes the
  # nearest cell without consulting that mask, so a coastal site draws a sea
  # cell and gets a full date range of NaN. This has to read differently from
  # an absent site: re-downloading fixes the one and not the other. Before the
  # check existed it surfaced as `max(NA, na.rm = TRUE)` being -Inf and was
  # reported as "not a volumetric fraction", which sends the reader to the
  # units of a file that has no numbers in it at all.
  tmp <- withr::local_tempfile(fileext = ".csv")
  write.csv(
    data.frame(time = c("1990-01-01", "1990-01-02"), site = "X-Tst", SWC = NA_real_),
    tmp, row.names = FALSE
  )
  expect_error(read_era5_swc("X-Tst", tmp), "all values are NA")
  expect_error(read_era5_swc("X-Tst", tmp), "land mask")
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

# A step-01 result reduced to what the ERA5 join reads.
fake_prep <- function(days) {
  d <- as.Date(days)
  hh <- tibble::tibble(
    YEAR = lubridate::year(rep(d, each = 2)),
    MONTH = lubridate::month(rep(d, each = 2)),
    DAY = lubridate::day(rep(d, each = 2))
  )
  list(ac = hh, nightNEE = hh[1, ])
}

test_that("a site's ERA5 slice is clipped to its own flux days", {
  # The point of the clip: extending the file past a site's record must leave
  # that site's slice -- and so everything downstream of it -- unchanged.
  tbl <- tibble::tibble(
    time = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-01")),
    site = c("X-Tst", "X-Tst", "X-Tst", "Y-Tst"),
    SWC = c(0.1, 0.2, 0.3, 0.9)
  )
  prep <- fake_prep(c("2020-01-01", "2020-01-02"))
  short <- site_era5_swc(prep, "X-Tst", table = tbl[1:2, ])
  long <- site_era5_swc(prep, "X-Tst", table = tbl)
  expect_identical(short, long)
  expect_equal(long$SWC, c(10, 20))
})

test_that("the ERA5 join annotates rows and never multiplies them", {
  prep <- fake_prep(c("2020-01-01", "2020-01-02"))
  era5 <- tibble::tibble(YEAR = 2020, MONTH = 1, DAY = 1:2, SWC = c(10, 20))
  out <- attach_era5_swc(prep, era5)
  expect_equal(nrow(out$ac), 4)
  expect_equal(out$ac$SWC_era5, c(10, 10, 20, 20))
  expect_equal(out$nightNEE$SWC_era5, 10)
})

test_that("unavailable ERA5 becomes an all-NA column, not an error", {
  prep <- fake_prep("2020-01-01")
  tbl <- tibble::tibble(time = as.Date("2020-01-01"), site = "Y-Tst", SWC = 0.5)
  era5 <- suppressMessages(site_era5_swc(prep, "X-Tst", table = tbl))
  expect_null(era5)
  out <- attach_era5_swc(prep, era5)
  expect_true(all(is.na(out$ac$SWC_era5)))
  expect_true(all(is.na(out$nightNEE$SWC_era5)))
})

test_that("an errored step 01 stays NULL through the ERA5 join", {
  expect_null(attach_era5_swc(NULL, NULL))
})

test_that("latest_record_end tolerates errored sites", {
  expect_equal(latest_record_end(as.Date("2020-01-01"), NULL, as.Date("2021-06-30")), as.Date("2021-06-30"))
  expect_true(is.na(latest_record_end(NULL, NULL)))
})

test_that("a site's record ends at its last measured NEE, not its padding", {
  prep <- list(ac = tibble::tibble(
    YEAR = 2026, MONTH = c(8, 8, 12), DAY = c(21, 22, 31), NEE = c(1, 2, NA)
  ))
  expect_equal(site_record_end(prep), as.Date("2026-08-22"))
  prep$ac$NEE <- NA
  expect_true(is.na(site_record_end(prep)))
  expect_equal(latest_record_end(as.Date(NA), as.Date("2020-01-01")), as.Date("2020-01-01"))
})
