# The development site sample, and the contract it is supposed to satisfy.
#
# `DEV_SITES` exists to make a full `tar_make()` affordable while developing.
# That only works if the six sites really do cover the branches the comment
# beside them claims, so those claims are asserted here rather than left as
# prose. Shrinking the sample to save time then silently stops exercising a
# code path -- which is the failure mode worth guarding against.

use_project_root()

dev_info <- get_site_info() |>
  dplyr::filter(.data$site_ID %in% DEV_SITES)

test_that("pipeline_sites() defaults to the development sample", {
  expect_identical(pipeline_sites(scope = "dev"), DEV_SITES)
  expect_identical(pipeline_sites(), DEV_SITES) # THERMAL_SITES unset
})

test_that("pipeline_sites('all') is a superset that still excludes AmeriFlux", {
  all_sites <- pipeline_sites(scope = "all")
  expect_true(all(DEV_SITES %in% all_sites))
  expect_true(length(all_sites) > length(DEV_SITES))

  info <- get_site_info() |> dplyr::filter(.data$site_ID %in% all_sites)
  expect_false(any(grepl("AmeriFlux_BASE", info$source, fixed = TRUE)))
  expect_true(all(info$LAT > 0))
})

test_that("an unrecognised scope is an error, not a silent empty pipeline", {
  expect_error(pipeline_sites(scope = "six"), "THERMAL_SITES")
})

test_that("a DEV_SITES entry the pipeline cannot handle is caught", {
  # US-Kon is AmeriFlux BASE, AU-Tum is southern hemisphere. Either one would
  # otherwise produce targets that each fail on their own at download time.
  fake <- get_site_info() |> dplyr::filter(.data$site_ID != DEV_SITES[[1]])
  expect_error(pipeline_sites(scope = "dev", site_info = fake), DEV_SITES[[1]])
})

test_that("every DEV_SITES entry is real and resolvable", {
  expect_equal(nrow(dev_info), length(DEV_SITES))
  expect_setequal(dev_info$site_ID, DEV_SITES)
  for (s in DEV_SITES) {
    expect_no_error(site_sources(get_site_info(s)))
  }
})

# The coverage claims. Each expectation below corresponds to a line of the
# comment on DEV_SITES; if the sample is edited, this says what was lost.
test_that("the sample covers both soil-temperature column choices", {
  expect_setequal(dev_info$ts_col, c("TS_measured", "TS_linear"))
})

test_that("the sample covers both soil-water paths", {
  # SWC_use TRUE exercises measured soil water and the nighttime filter that
  # requires it; FALSE sends the direct model to the ERA5 fallback.
  expect_setequal(dev_info$SWC_use, c(TRUE, FALSE))
})

test_that("the sample covers every flux product the readers splice", {
  products <- unique(unlist(lapply(DEV_SITES, function(s) site_sources(get_site_info(s)))))
  expect_true(all(c("FLUXNET", "ICOS", "WW2020", "FLUXNET2015") %in% products))
  # and at least one single-product site, so the no-splice path is exercised too
  n_products <- vapply(DEV_SITES, function(s) length(site_sources(get_site_info(s))), integer(1))
  expect_true(any(n_products == 1))
  expect_true(any(n_products >= 3))
})

test_that("the sample covers both soil-temperature estimation methods", {
  # The random-forest (NETRAD) branch of fix_soil_temp() had no execution
  # behind it at all before DE-Akm joined this list.
  methods <- dev_info$estimate_ts_method[!is.na(dev_info$estimate_ts_method)]
  expect_true("NETRAD" %in% methods)
  expect_true("linear regression" %in% methods)
})

test_that("the sample covers the special growing-season cut-off", {
  expect_true(any(DEV_SITES %in% SITES_GS_NEE_ZERO))
})
