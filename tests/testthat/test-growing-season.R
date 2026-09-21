use_project_root()

ac <- synthetic_season()
si <- get_site_info("US-Kon")
si$gStart <- NA_integer_
si$gEnd <- NA_integer_

manuscript_fields <- c("gStart", "gEnd", "tStart", "tEnd")

test_that("each cut-off reproduces the original expression it came from", {
  for (variant in c("uncapped", "capped", "zero")) {
    expect_equal(
      lapply(detect_growing_season(ac, si, nee_threshold = variant)[manuscript_fields], unname),
      original_detect_growing_season(ac, variant),
      info = variant
    )
  }
})

test_that("the three cut-offs are not interchangeable", {
  # The regression the refactor shipped: AmeriFlux sites silently used the
  # EuroFlux cut-off. If these ever coincide, the fixture has stopped exercising
  # the difference and the test above proves nothing.
  spans <- vapply(
    c("uncapped", "capped", "zero"),
    \(v) {
      gs <- detect_growing_season(ac, si, nee_threshold = v)
      c(gs$gStart, gs$gEnd)
    },
    numeric(2)
  )
  expect_equal(nrow(unique(t(spans))), 3L)
})

test_that("nee_threshold is required rather than silently defaulted", {
  expect_error(detect_growing_season(ac, si))
})

test_that("an unrecognised nee_threshold is rejected", {
  expect_error(detect_growing_season(ac, si, nee_threshold = "whatever"))
})

test_that("site_info gStart/gEnd overrides take precedence, and the detected pair survives them", {
  plain <- detect_growing_season(ac, si, nee_threshold = "capped")
  si2 <- si
  si2$gStart <- 100L
  si2$gEnd <- 300L
  gs <- detect_growing_season(ac, si2, nee_threshold = "capped")
  expect_equal(gs$gStart, 100)
  expect_equal(gs$gEnd, 300)
  # What `force_detect` reads: the detector's answer, unchanged by the override.
  expect_equal(gs$gStart_detected, plain$gStart)
  expect_equal(gs$gEnd_detected, plain$gEnd)
  # ...and with no override declared the two pairs coincide.
  expect_equal(plain$gStart_detected, plain$gStart)
  expect_equal(plain$gEnd_detected, plain$gEnd)
})

test_that("gStart is floored by the first day-of-year at or above 0 C", {
  cold <- ac
  cold$TS <- cold$TS - 6
  gs <- detect_growing_season(cold, si, nee_threshold = "capped")
  first_thawed <- cold |>
    dplyr::summarise(TS = mean(.data$TS, na.rm = TRUE), .by = "DOY") |>
    dplyr::filter(.data$TS >= 0) |>
    dplyr::pull("DOY") |>
    min()
  expect_gte(gs$gStart, first_thawed)
})

test_that("the too-few-points guard names the site", {
  # This guard referenced an undefined `site_name`, so it raised
  # "object 'site_name' not found" instead of the message it meant to.
  flat <- ac
  flat$NEE <- 5
  expect_error(detect_growing_season(flat, si, nee_threshold = "capped"), "US-Kon")
})
