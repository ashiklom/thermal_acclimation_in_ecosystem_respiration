# AmeriFlux preparation: the FC mask, the soil-water branch, and the
# sunrise/sunset frame. All three shipped broken, and all three can be pinned
# down without touching REddyProc.

use_project_root()

test_that("the FC mask blanks NEE by position, not by value", {
  # `na_if(NEE, is.na(FC))` compares NEE against the mask coerced to 0/1, so it
  # blanked rows where NEE happened to equal 1 or 0. Those two values below are
  # what expose it.
  si <- get_site_info("US-CMW") # NEE = NEE_PI, FC = FC
  a <- synthetic_ameriflux(si, n = 4)
  a[[si$NEE]] <- c(1, 2, 0, 5)
  a[[si$FC]] <- c(NA, 7, NA, 9)

  ac <- suppressWarnings(suppressMessages(prep_ustar_df(a, si)))[["ac"]]

  expected <- c(1, 2, 0, 5)
  expected[is.na(a[[si$FC]])] <- NA # what the original did
  expect_equal(ac$NEE, expected)
  # and confirm the fixture really does discriminate the two implementations
  expect_false(isTRUE(all.equal(dplyr::na_if(c(1, 2, 0, 5), is.na(a[[si$FC]])), expected)))
})

swc_cases <- list(
  list(site = "US-Wkg", swc = "read", why = "SWC_use YES, column named"),
  list(site = "US-Kon", swc = "na", why = "SWC_use NO, column named -- discard anyway"),
  list(site = "US-Ha2", swc = "na", why = "SWC_use NO, no column -- must not error")
)
for (case in swc_cases) {
  test_that(sprintf("soil water for %s: %s", case$site, case$why), {
    si <- get_site_info(case$site)
    ac <- suppressWarnings(suppressMessages(prep_ustar_df(synthetic_ameriflux(si), si)))[["ac"]]
    expect_true("SWC" %in% names(ac))
    if (case$swc == "read") {
      expect_false(all(is.na(ac$SWC)))
    } else {
      expect_true(all(is.na(ac$SWC)))
    }
  })
}

test_that("sunlight times match the original's local-relabel convention", {
  # This comparison is what caught the day/night regression -- and the first
  # version of it compared only sunrise, which passed while sunset was wrong.
  # Both are asserted here, deliberately.
  dates <- seq.Date(as.Date("2015-01-01"), as.Date("2015-12-31"), by = 1)
  for (name_site in c("US-Kon", "US-Ho1", "US-Wkg", "US-ICt", "CA-Ca3", "US-Var")) {
    si <- get_site_info(name_site)
    want <- original_sunlight_times(si, dates)
    got <- site_sunlight_times(si, dates)
    for (event in c("sunrise", "sunset")) {
      # Polar sites legitimately have days with no sunrise or sunset, so the
      # missingness pattern is part of what has to agree.
      expect_equal(is.na(got[[event]]), is.na(want[[event]]),
                   info = paste(name_site, event, "missingness"))
      both <- !is.na(got[[event]]) & !is.na(want[[event]])
      expect_equal(
        max(abs(as.numeric(difftime(got[[event]][both], want[[event]][both], units = "mins")))),
        0,
        info = paste(name_site, event)
      )
    }
  }
})

test_that("sunrise and sunset belong to the same local day", {
  # Requesting the times in UTC returns a sunrise and a sunset from *different*
  # local days at these longitudes, which makes the daytime test unsatisfiable
  # for most of the year. Polar sites are excluded: they legitimately have days
  # with neither event.
  dates <- seq.Date(as.Date("2015-01-01"), as.Date("2015-12-31"), by = 1)
  for (name_site in c("US-Kon", "US-Wkg", "CA-Ca3")) {
    si <- get_site_info(name_site)
    got <- site_sunlight_times(si, dates)
    expect_true(all(got$sunset > got$sunrise, na.rm = TRUE), info = name_site)
    expect_equal(
      sum(as.Date(got$sunrise) != as.Date(got$sunset), na.rm = TRUE), 0L,
      info = name_site
    )
  }
})

test_that("local noon is daytime and local midnight is not", {
  # The end-to-end property the regression violated: before the fix, only 35% of
  # local-noon half-hours at US-Kon were flagged daytime.
  si <- get_site_info("US-Kon")
  ts <- synthetic_year_timestamps(2015)
  sun <- site_sunlight_times(si, seq.Date(as.Date("2015-01-01"), as.Date("2015-12-31"), by = 1))

  dat <- data.frame(TIMESTAMP = ts, DATE = as.Date(ts)) |>
    dplyr::left_join(sun, by = c("DATE" = "date"))
  dt <- as.difftime(0.5, units = "hours")
  daytime <- (dat$TIMESTAMP + dt / 2 >= dat$sunrise) & (dat$TIMESTAMP - dt / 2 <= dat$sunset)
  hour <- lubridate::hour(dat$TIMESTAMP) + lubridate::minute(dat$TIMESTAMP) / 60

  expect_equal(mean(daytime[hour >= 11.5 & hour <= 12.5], na.rm = TRUE), 1)
  expect_equal(mean(daytime[hour < 1 | hour > 23], na.rm = TRUE), 0)
  expect_equal(mean(daytime, na.rm = TRUE), 0.5, tolerance = 0.1)
})

test_that("a fractional UTC offset is refused rather than mis-classified", {
  # Etc/GMT zones exist only at whole-hour offsets. No AmeriFlux site is at a
  # half-hour offset today, so this guards a future addition.
  si <- get_site_info("US-Kon")
  si$LAT <- 47.5
  si$LONG <- -52.7 # Newfoundland, UTC-3:30
  expect_error(site_sunlight_times(si, as.Date("2015-06-01")), "fractional UTC offset")
})

# `prep_ustar_df()` and `prep_ameriflux()` used to hand results back to their
# callers as attributes on the returned data frame. These pin the named-list
# contract that replaced them: the fields have to be present, and they have to
# carry the right values. An attribute silently dropped by a dplyr verb would
# have surfaced much further downstream -- as a missing VPD inside REddyProc,
# or as a recomputed growing season disagreeing with the u-star season factor.
test_that("prep_ustar_df returns the table and the RH-conversion decision", {
  rh_site <- get_site_info("US-Kon") # RH named
  vpd_site <- get_site_info("US-GLE") # no RH, VPD named
  expect_false(is.na(rh_site$RH))
  expect_true(is.na(vpd_site$RH))

  for (case in list(list(si = rh_site, convert = TRUE, col = "RH"),
                    list(si = vpd_site, convert = FALSE, col = "VPD"))) {
    out <- suppressWarnings(suppressMessages(
      prep_ustar_df(synthetic_ameriflux(case$si), case$si)
    ))
    expect_named(out, c("ac", "convert_rh"))
    expect_identical(out$convert_rh, case$convert,
                     info = paste(case$si$site_ID, "convert_rh"))
    # The flag decides which humidity column `prep_ameriflux()` reads, so the
    # two have to agree: claiming a conversion with no RH column would fail
    # inside REddyProc instead of here.
    expect_true(case$col %in% names(out$ac), info = case$si$site_ID)
  }
})

# The one AmeriFlux branch with no implementation behind it. Removing the
# reader's exclusion from `pipeline_sites()` put these eight sites into
# `THERMAL_SITES=all`, where they fail individually; that is the intended
# behaviour, but only if the list stays honest about who is affected.
test_that("the unimplemented estimate_Ts sites are exactly what site_info declares", {
  si <- get_site_info()
  declared <- si$site_ID[si$estimate_Ts & grepl("AmeriFlux_BASE", si$source, fixed = TRUE)]
  expect_setequal(declared, SITES_ESTIMATE_TS_BLOCKED)
})

test_that("an estimate_Ts site fails by name rather than mid-reader", {
  si <- get_site_info(SITES_ESTIMATE_TS_BLOCKED[[1]])
  expect_true(si$estimate_Ts)
  expect_error(
    suppressWarnings(suppressMessages(prep_ustar_df(synthetic_ameriflux(si), si))),
    "estimate_Ts"
  )
})
