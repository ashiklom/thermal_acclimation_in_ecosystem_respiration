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

# ------------------------------------------------- the estimator's semantics

# Verbatim transcription of R/total_tas.R@b077bb9:202-220, the block that
# `apply_ts_linear()` replaced. The refactor is checked against this rather
# than against itself.
original_ts_substitution <- function(ac, nightNEE, name_site, gStart, gEnd) {
  if (name_site %in% c("US-Tw1")) {
    mod_lm <- lm(data = nightNEE, TS ~ TA, na.action = na.omit)
  } else {
    mod_lm <- lm(data = ac[ac$TA > 0, ], TS ~ TA, na.action = na.omit)
  }
  TS_pred <- predict(mod_lm, newdata = data.frame(TA = nightNEE$TA), na.action = na.pass)
  nightNEE$TS[!is.na(TS_pred)] <- TS_pred[!is.na(TS_pred)]
  TS_pred <- predict(mod_lm, newdata = data.frame(TA = ac$TA), na.action = na.pass)
  ac$TS[!is.na(TS_pred)] <- TS_pred[!is.na(TS_pred)]
  ts_growing_season <- ac$TS[dplyr::between(ac$DOY, gStart, gEnd)]
  list(
    ac = ac, nightNEE = nightNEE,
    tStart = unname(quantile(ts_growing_season, 0.025, na.rm = TRUE)),
    tEnd = unname(quantile(ts_growing_season, 0.975, na.rm = TRUE))
  )
}

# TA is noisy and not a deterministic function of TS, so the regression is a
# genuine approximation and substituting it actually moves the values -- a
# fixture where TS = f(TA) exactly would make every test below vacuous. Rows
# with missing TA exist on purpose: they are what makes the overlay visible.
ts_fixture <- function(n = 900, seed = 42) {
  set.seed(seed)
  doy <- rep(seq(60, 320, length.out = n / 3), each = 3)[seq_len(n)]
  ta <- 12 * sin((doy - 100) / 365 * 2 * pi) + rnorm(n, 0, 3)
  ts <- 0.7 * ta + 4 + rnorm(n, 0, 1.5)
  ta[seq(1, n, by = 37)] <- NA_real_ # missing air temperature
  ac <- tibble::tibble(DOY = doy, TA = ta, TS = ts)
  night <- ac[seq(2, n, by = 5), ]
  night$TS <- night$TS - 0.4 # nighttime soil temperature runs cooler
  list(ac = ac, night = night)
}

test_that("apply_ts_linear matches the original substitution on the ac domain", {
  f <- ts_fixture()
  si <- list(site_ID = "X-Tst", ts_col = "TS_linear", ts_linear_domain = "ac")
  got <- apply_ts_linear(f$ac, f$night, si, gStart = 120, gEnd = 280)
  want <- original_ts_substitution(f$ac, f$night, "X-Tst", gStart = 120, gEnd = 280)
  expect_equal(got$ac$TS, want$ac$TS)
  expect_equal(got$nightNEE$TS, want$nightNEE$TS)
  expect_equal(got$tStart, want$tStart)
  expect_equal(got$tEnd, want$tEnd)
})

test_that("apply_ts_linear matches the original substitution on the night domain", {
  f <- ts_fixture()
  si <- list(site_ID = "US-Tw1", ts_col = "TS_linear", ts_linear_domain = "night")
  got <- apply_ts_linear(f$ac, f$night, si, gStart = 120, gEnd = 280)
  want <- original_ts_substitution(f$ac, f$night, "US-Tw1", gStart = 120, gEnd = 280)
  expect_equal(got$ac$TS, want$ac$TS)
  expect_equal(got$nightNEE$TS, want$nightNEE$TS)
  expect_equal(got$tStart, want$tStart)
})

test_that("the two fit domains are not interchangeable", {
  # If they were, `ts_linear_domain` would be dead weight and US-Tw1's
  # deliberate exception would be silently undone.
  f <- ts_fixture()
  ac_fit <- coef(ts_ta_model(ts_fit_data(f$ac, f$night, "ac")))
  night_fit <- coef(ts_ta_model(ts_fit_data(f$ac, f$night, "night")))
  expect_false(isTRUE(all.equal(ac_fit, night_fit)))
  # The `ac` domain must exclude sub-zero air temperature from the fit.
  expect_true(all(ts_fit_data(f$ac, f$night, "ac")$TA > 0, na.rm = TRUE))
  expect_equal(nrow(ts_fit_data(f$ac, f$night, "night")), nrow(f$night))
})

test_that("the substitution is an overlay, so measured TS survives missing TA", {
  f <- ts_fixture()
  si <- list(site_ID = "X-Tst", ts_col = "TS_linear", ts_linear_domain = "ac")
  got <- apply_ts_linear(f$ac, f$night, si, gStart = 120, gEnd = 280)

  no_ta <- is.na(f$ac$TA)
  expect_gt(sum(no_ta), 0)
  # Measured values kept exactly where there is no air temperature to predict
  # from, and no NA introduced there.
  expect_equal(got$ac$TS[no_ta], f$ac$TS[no_ta])
  expect_false(any(is.na(got$ac$TS)))
  # ... and genuinely replaced everywhere else.
  expect_false(isTRUE(all.equal(got$ac$TS[!no_ta], f$ac$TS[!no_ta])))
})

test_that("bounds are recomputed from the substituted column, over the growing season", {
  f <- ts_fixture()
  si <- list(site_ID = "X-Tst", ts_col = "TS_linear", ts_linear_domain = "ac")
  got <- apply_ts_linear(f$ac, f$night, si, gStart = 120, gEnd = 280)

  before <- ts_bounds(f$ac$TS, f$ac$DOY, 120, 280)
  # Regressing on TA compresses the distribution, so the bounds must move.
  expect_false(isTRUE(all.equal(got$tStart, before$tStart)))
  expect_false(isTRUE(all.equal(got$tEnd, before$tEnd)))
  # They describe the substituted ac column, restricted to the growing season.
  in_gs <- dplyr::between(f$ac$DOY, 120, 280)
  expect_equal(got$tStart, unname(quantile(got$ac$TS[in_gs], 0.025)))
  expect_equal(got$tEnd, unname(quantile(got$ac$TS[in_gs], 0.975)))
  # Not from the whole record, and not from the nighttime table.
  expect_false(isTRUE(all.equal(got$tStart, unname(quantile(got$ac$TS, 0.025)))))
  expect_false(isTRUE(all.equal(
    got$tStart, unname(quantile(got$nightNEE$TS[dplyr::between(got$nightNEE$DOY, 120, 280)], 0.025))
  )))
})

test_that("an undeclared or unknown fit domain fails by name", {
  f <- ts_fixture()
  expect_error(ts_fit_data(f$ac, f$night, NA_character_), "must declare ts_linear_domain")
  expect_error(ts_fit_data(f$ac, f$night, "whole_record"), "Unknown ts_linear_domain")
})

# ------------------------------------- the shared TS ~ TA fit, step-01 form

test_that("the consolidated fit reproduces the inline lm it replaced", {
  # `prep_ustar_df()` and `fix_soil_temp()` used to write
  # `lm(data = d[d$TA > 0, ], TS ~ TA, na.action = na.omit)` followed by
  # `predict(mod, newdata = data.frame(TA = d$TA))`, replacing TS wholesale.
  set.seed(3)
  d <- data.frame(TA = c(rnorm(200, 8, 6), NA, NA))
  d$TS <- 0.6 * d$TA + 3 + rnorm(nrow(d))
  d$TS[c(5, 11, 40)] <- NA

  want_mod <- lm(data = d[d$TA > 0, ], TS ~ TA, na.action = na.omit)
  want <- predict(want_mod, newdata = data.frame(TA = d$TA))

  got <- replace_ts(predict_ts_from_ta(ts_ta_model(d[d$TA > 0, ]), d$TA))
  expect_equal(unname(got), unname(want))
  # Wholesale replacement keeps the NAs, unlike the overlay form. (`predict()`
  # names its result by row; the names carry no information here.)
  expect_equal(unname(is.na(got)), is.na(d$TA))
})
