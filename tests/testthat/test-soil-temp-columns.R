# Soil-temperature column selection: the per-site declarations that replaced the
# hard-coded `site_TS_issue` vector, and the estimator they drive.

use_project_root()

# Transcribed from R/total_tas.R@b077bb9:17-20, before the list moved into
# site_info.csv. Kept verbatim here so that a hand-edit of the CSV that adds or
# drops a site has to be deliberate.
ORIGINAL_SITE_TS_ISSUE <- c(
  "BE-Bra", "CA-Cbo", "CA-Gro", "CA-Mer", "CA-Obs", "CA-TP3", "CH-Lae", "DE-RuC",
  "DE-SfS", "FI-Sod", "IT-Ren", "NL-Loo", "US-Bar", "US-BZB", "US-BZF", "US-BZS",
  "US-CMW", "US-GLE", "US-Ha2", "US-IB2", "US-Jo2", "US-KL2", "US-Kon", "US-LL1",
  "US-MBP", "US-Myb", "US-NC4", "US-Tw1", "US-ICt", "BE-Dor", "CA-TP4", "UK-AMo",
  "RU-Fyo", "ZA-Kru", "IT-Tor"
)

test_that("ts_col reproduces the original site_TS_issue list exactly", {
  si <- get_site_info()
  declared <- sort(si$site_ID[si$ts_col == "TS_linear"])
  expect_equal(declared, sort(ORIGINAL_SITE_TS_ISSUE))
})

test_that("US-Tw1 is the only site fitted on the nighttime table", {
  # The original singled it out: "slope will be too low if using ac data for
  # the subtropical wetland sites". Losing that would change its coefficients.
  si <- get_site_info()
  expect_equal(si$site_ID[!is.na(si$ts_linear_domain) & si$ts_linear_domain == "night"], "US-Tw1")
})

test_that("ts_col and ts_linear_domain cannot drift apart", {
  si <- get_site_info()
  expect_true(all(si$ts_col %in% c("TS_measured", "TS_linear")))
  # A domain without a selection is meaningless; a selection without a domain
  # has nothing to fit on.
  expect_equal(is.na(si$ts_linear_domain), si$ts_col == "TS_measured")
})

test_that("step-01 TA construction and step-02 selection stay disjoint", {
  # `SITES_TS_FROM_TA_*` build TS_measured because there is no usable measured
  # soil temperature; `ts_col == "TS_linear"` replaces a measured column later.
  # A site in both would be regressed twice, from different fits.
  si <- get_site_info()
  step02 <- si$site_ID[si$ts_col == "TS_linear"]
  expect_equal(intersect(c(SITES_TS_FROM_TA_RECENT, SITES_TS_FROM_TA_COLD), step02), character())
})
