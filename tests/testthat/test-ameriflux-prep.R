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
    expect_named(out, c("ac", "convert_rh", "ts_provenance"))
    expect_identical(out$convert_rh, case$convert,
                     info = paste(case$si$site_ID, "convert_rh"))
    # The flag decides which humidity column `prep_ameriflux()` reads, so the
    # two have to agree: claiming a conversion with no RH column would fail
    # inside REddyProc instead of here.
    expect_true(case$col %in% names(out$ac), info = case$si$site_ID)
  }
})

# ------------------------------------------------ the estimate_Ts sites
#
# Eight AmeriFlux sites have no usable measured soil temperature and
# reconstruct the whole column from air temperature, five of them with net
# radiation as well. The reader used to `stop()` here, so this was the last
# branch with no execution behind it. These run the reconstruction on
# synthetic input -- fast, and enough to catch the failure that hid in it:
# the reader's soil-temperature block selected the per-site soil and air
# temperature columns without renaming them to `TS`/`TA`, which the gap fill
# read as "nothing missing" and the fit read as "object 'TA' not found".
estimate_ts_sites <- (\(si) si$site_ID[si$estimate_Ts &
  grepl("AmeriFlux_BASE", si$source, fixed = TRUE)])(get_site_info())

test_that("both AmeriFlux estimator branches are represented", {
  methods <- vapply(estimate_ts_sites, function(s) ts_estimate_method(get_site_info(s)), "")
  expect_setequal(unname(methods), c("NETRAD", "TA only"))
})

for (name_site in estimate_ts_sites) {
  test_that(sprintf("stage A reconstructs soil temperature at %s", name_site), {
    si <- get_site_info(name_site)
    withr::local_seed(4)
    a <- with_synthetic_soil_signal(synthetic_ameriflux(si, n = 48 * 30), si)
    out <- suppressWarnings(suppressMessages(
      qualification_soil_temperature(ameriflux_ts_input(a, si), si)
    ))

    expect_named(out, c("TS", "TS_QC", "provenance"))
    expect_length(out$TS, nrow(a))
    expect_type(out$TS, "double")
    # A reconstruction of nothing would also satisfy the shape checks.
    expect_false(all(is.na(out$TS)))
    expect_gt(stats::sd(out$TS, na.rm = TRUE), 0)
    expect_identical(out$provenance$ts_source, "reconstructed")
    expect_identical(out$provenance$stage_a_mode, "replace")
    expect_match(out$provenance$stage_a_estimator, if (is.na(si$netrad_column)) "lm_ta_pos" else "rf_ta_netrad_manuscript")
  })
}

test_that("the reader attaches the reconstruction as TS, one row per input row", {
  # US-Ha1 is the cheapest of the eight -- no net radiation, so the branch is
  # a regression rather than a forest -- and the join is what this pins:
  # stage A gap-fills predictors through day-of-year joins internally, where a
  # duplicated key would change the row count without changing the columns.
  si <- get_site_info("US-Ha1")
  expect_true(si$estimate_Ts)
  withr::local_seed(4)
  a <- with_synthetic_soil_signal(synthetic_ameriflux(si, n = 48 * 30), si)
  out <- suppressWarnings(suppressMessages(prep_ustar_df(a, si)))[["ac"]]

  expect_equal(nrow(out), nrow(a))
  expect_true("TS" %in% names(out))
  expect_false("TS_pred" %in% names(out))
  expect_false(all(is.na(out$TS)))
})

test_that("an unknown estimator is refused by name", {
  si <- get_site_info("US-Ha1")
  si$estimate_ts_method <- "kriging"
  a <- with_synthetic_soil_signal(synthetic_ameriflux(si, n = 96), si)
  expect_error(
    suppressWarnings(suppressMessages(fix_soil_temp(ameriflux_ts_input(a, si), si))),
    "estimate_ts_method"
  )
})

# ------------------------------------ declared columns vs. the actual record
#
# AmeriFlux BASE column names encode sensor position and processing level, and
# they change between releases. US-Ho1 and US-Ho2 declared `RH_PI_F_2_1_1`,
# which releases 15-5 and 10-5 no longer publish; `[[` on a tibble returns
# NULL for an absent name, so the column was never created and the failure
# surfaced inside REddyProc as a complaint about `RelHumidity_Percent`.
test_that("a declared column the record lacks is refused, by name, up front", {
  si <- get_site_info("US-Kon")
  a <- synthetic_ameriflux(si)
  a[[si$TA]] <- NULL

  expect_error(prep_ustar_df(a, si), "US-Kon")
  expect_error(prep_ustar_df(a, si), si$TA)
  # and the shortlist of what the record does offer, so the message is actionable
  expect_error(prep_ustar_df(a, si), "the record has")
})

test_that("the declared-column set follows the path the site actually takes", {
  # Soil temperature is not read where it is reconstructed, soil water is not
  # read where the site discards it, and exactly one of RH/VPD is consulted.
  reconstructed <- get_site_info("US-Ha1")
  expect_true(reconstructed$estimate_Ts)
  expect_false(reconstructed$TS %in% declared_ameriflux_columns(reconstructed))

  no_water <- get_site_info("US-Kon") # SWC_use NO, but names a column
  expect_false(is.na(no_water$SWC))
  expect_false(no_water$SWC %in% declared_ameriflux_columns(no_water))

  rh_site <- get_site_info("US-Kon")
  vpd_site <- get_site_info("US-GLE")
  expect_true(rh_site$RH %in% declared_ameriflux_columns(rh_site))
  expect_true(vpd_site$VPD %in% declared_ameriflux_columns(vpd_site))

  # A compound NEE declaration contributes every term.
  compound <- get_site_info("US-Kon")
  compound$NEE <- "FC_1_1_1 + SC_1_1_1"
  expect_true(all(c("FC_1_1_1", "SC_1_1_1") %in% declared_ameriflux_columns(compound)))
})

test_that("every AmeriFlux site's declared columns exist in its record", {
  # The check that would have caught US-Ho1 and US-Ho2 before a pipeline run
  # did. Reads only each archive's header, so it costs seconds, not minutes.
  si <- get_site_info()
  si <- si[vapply(seq_len(nrow(si)), function(i) site_reader(si[i, ]), "") == "ameriflux", ]
  skip_if(anyNA(vapply(si$site_ID, function(s) product_file(s, "AmeriFlux_BASE"), "")),
          "not every AmeriFlux BASE archive is downloaded")

  offenders <- character()
  for (i in seq_len(nrow(si))) {
    row <- si[i, ]
    cols <- ameriflux_base_header(row$site_ID)
    # US-Ha2's `_A_1_1` columns are built by `prep_ameriflux()` from two
    # locations before `prep_ustar_df()` runs, so they are not expected here.
    wanted <- setdiff(declared_ameriflux_columns(row), grep("_A_1_1$", declared_ameriflux_columns(row), value = TRUE))
    absent <- setdiff(wanted, cols)
    if (length(absent)) {
      offenders <- c(offenders, paste0(row$site_ID, ": ", paste(absent, collapse = ", ")))
    }
  }
  expect_equal(offenders, character())
})
