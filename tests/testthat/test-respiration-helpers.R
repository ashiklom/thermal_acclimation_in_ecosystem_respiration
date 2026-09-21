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
  d <- build_gs_dates(120, 260, 2010, 2014, as.difftime(30, units = "mins"))
  expect_equal(nrow(d), 10L)
  expect_equal(sort(unique(d$DOY)), c(120, 260))
  expect_equal(length(unique(lubridate::year(d$TIMESTAMP))), 5L)
  expect_equal(lubridate::yday(d$TIMESTAMP[d$DOY == 120]), rep(120, 5))
})

test_that("build_gs_dates unwraps a wrapped DOY for the timestamp only", {
  d <- build_gs_dates(185, 380, 2010, 2012, as.difftime(30, units = "mins"))
  # DOY stays in the shifted frame, because that is what the gap scan compares
  # against, but the timestamp has to be a real calendar date.
  expect_equal(sort(unique(d$DOY)), c(185, 380))
  expect_equal(sort(unique(lubridate::yday(d$TIMESTAMP))), c(14, 185))
})

# The wrapped day-of-year frame a site with a growing season across New Year
# runs in. Every southern-hemisphere claim downstream -- the growing year, the
# window layout, the partial-year trim -- rests on these three functions, so
# they are pinned here rather than only exercised through a site.
test_that("wrap_growing_doy is the identity unless an origin is declared", {
  doy <- c(1, 182, 183, 365, 366)
  expect_identical(wrap_growing_doy(doy, 1L), doy)
  expect_identical(wrap_growing_doy(doy, 0L), doy)
})

test_that("wrap_growing_doy makes a season across New Year contiguous", {
  # 1 July is the origin: everything before it belongs to the growing year
  # that started the previous July, and is pushed past the end of the year.
  expect_equal(wrap_growing_doy(c(182, 183, 184), 183L), c(548, 183, 184))
  # A wrapped series is monotone within a growing year, which is the property
  # `between(DOY, gStart, gEnd)` depends on.
  wrapped <- wrap_growing_doy(c(183, 300, 365, 1, 100, 182), 183L)
  expect_false(is.unsorted(wrapped))
})

test_that("unwrap_growing_doy inverts the wrap and leaves real DOYs alone", {
  expect_equal(unwrap_growing_doy(c(185, 366, 367, 548)), c(185, 366, 1, 182))
})

test_that("growing_year_of assigns the wrapped tail to the year it started in", {
  # 1 January 2011 (DOY 367 at an origin-183 site) belongs to the growing year
  # that began in July 2010.
  expect_equal(growing_year_of(c(183, 366, 367, 548), c(2010, 2010, 2011, 2011)),
               c(2010L, 2010L, 2010L, 2010L))
  # ...and an unwrapped site is untouched, whatever the year column's type.
  expect_equal(growing_year_of(c(1, 200, 366), c(2010, 2010, 2010)),
               rep(2010L, 3))
  expect_equal(growing_year_of(c(1L, 200L), c(2010L, 2010L)), c(2010L, 2010L))
})

test_that("growing_year_start defaults to 1 and refuses a non-DOY", {
  expect_identical(growing_year_start(list(site_ID = "x")), 1L)
  expect_identical(growing_year_start(list(site_ID = "x", growing_year_start = NA_integer_)), 1L)
  expect_identical(growing_year_start(list(site_ID = "x", growing_year_start = 183L)), 183L)
  expect_error(
    growing_year_start(list(site_ID = "x", growing_year_start = 400L)),
    "not a day of year"
  )
})

test_that("the sites declared with a wrapped growing year are the seasonal southern ones", {
  si <- get_site_info()
  wrapped <- si$site_ID[!is.na(si$growing_year_start)]
  expect_setequal(wrapped, c("AU-Tum", "ZA-Kru"))
  # Every wrapped site is southern...
  expect_true(all(si$LAT[si$site_ID %in% wrapped] < 0))
  # ...but not every southern site is wrapped: BR-Ma2 and BR-Sa1 sit on the
  # equator with their growing season pinned to the whole calendar year, and
  # the original workflows did not wrap them either.
  southern <- si$site_ID[si$LAT < 0]
  expect_setequal(setdiff(southern, wrapped), c("BR-Ma2", "BR-Sa1"))
  for (s in setdiff(southern, wrapped)) {
    expect_identical(growing_year_start(get_site_info(s)), 1L, info = s)
  }
})
