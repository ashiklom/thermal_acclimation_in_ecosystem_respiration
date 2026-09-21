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

# ------------------------------- the domain resolver for the diagnostic column

test_that("a declared TS_linear site's domain is passed through untouched", {
  # Including NA. `TS_linear` is now built at every site, so the resolver is
  # what stands between a half-filled site_info row and a silent default: a
  # site that *selects* the column and forgets to say which rows to fit on
  # must still reach `ts_fit_data()`'s error, not quietly get "ac".
  expect_equal(
    ts_linear_domain_for(list(ts_col = "TS_linear", ts_linear_domain = "night")),
    "night"
  )
  expect_equal(
    ts_linear_domain_for(list(ts_col = "TS_linear", ts_linear_domain = "ac")),
    "ac"
  )
  expect_true(is.na(
    ts_linear_domain_for(list(ts_col = "TS_linear", ts_linear_domain = NA_character_))
  ))
})

test_that("a diagnostic TS_linear column defaults to the ac domain", {
  # At a site that selects measured soil temperature the column is inert
  # unless a sensitivity run names it, so a missing declaration is expected
  # rather than an error. "ac" is what 34 of the 35 declared sites use.
  expect_equal(
    ts_linear_domain_for(list(ts_col = "TS_measured", ts_linear_domain = NA_character_)),
    "ac"
  )
  # An explicit declaration still wins, so a measured-TS site can be pinned to
  # the nighttime domain for a comparison without editing this function.
  expect_equal(
    ts_linear_domain_for(list(ts_col = "TS_measured", ts_linear_domain = "night")),
    "night"
  )
})

test_that("every site in site_info resolves to a usable domain", {
  si <- get_site_info()
  domains <- vapply(seq_len(nrow(si)), function(i) ts_linear_domain_for(si[i, ]), "")
  expect_true(all(domains %in% c("ac", "night")))
  # The declared sites keep exactly the domains the CSV gives them, and every
  # other site gets the default.
  declared <- si$ts_col == "TS_linear"
  expect_equal(domains[declared], si$ts_linear_domain[declared])
  expect_true(all(domains[!declared] == "ac"))
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

# --------------------------------------------- FI-Sod's pre-2006 rebuild

FI_SOD_2015_HH <- file.path(
  "data-raw", "FLUXNET2015", "FI-Sod",
  "FLX_FI-Sod_FLUXNET2015_FULLSET_HH_2001-2014_1-4.csv"
)

test_that("the date windows reproduce the original's row windows exactly", {
  # The decisive test. `FI_SOD_EARLY_WINDOW` / `FI_SOD_LATE_WINDOW` are
  # `a[1:24383, ]` and `a[90000:245000, ]` resolved against the FLUXNET2015
  # release the manuscript was written on. If that translation is right, the
  # two must agree to the last bit on that file.
  skip_if_not(file.exists(FI_SOD_2015_HH), "FI-Sod FLUXNET2015 not downloaded")

  a <- readr::read_csv(
    FI_SOD_2015_HH,
    col_select = c("TIMESTAMP_START", "TS_F_MDS_1", "TS_F_MDS_2", "TS_F_MDS_1_QC"),
    col_types = readr::cols(TIMESTAMP_START = "c", .default = "d"),
    progress = FALSE
  )
  a[a == -9999] <- NA
  a$YEAR <- as.integer(substr(a$TIMESTAMP_START, 1, 4))

  # The row ranges only mean anything if the record is the length the original
  # assumed, so assert that rather than discovering it later.
  expect_equal(nrow(a), 245424)

  original <- local({
    d <- a
    mod1 <- lm(data = d[1:24383, ], TS_F_MDS_2 ~ TS_F_MDS_1)
    pred2 <- predict(mod1, data.frame(TS_F_MDS_1 = d$TS_F_MDS_1[d$YEAR <= 2005]))
    mod2 <- lm(data = d[90000:245000, ], TS_F_MDS_1 ~ TS_F_MDS_2)
    d$TS_F_MDS_1[d$YEAR <= 2005] <- predict(mod2, data.frame(TS_F_MDS_2 = pred2))
    d$TS_F_MDS_1_QC[d$YEAR <= 2005] <- 2
    d
  })
  got <- recalibrate_fi_sod_soil_temp(a)

  expect_equal(got$TS_F_MDS_1, original$TS_F_MDS_1)
  expect_equal(got$TS_F_MDS_1_QC, original$TS_F_MDS_1_QC)
})

test_that("a year boundary instead of the windows would change the science", {
  # Guards the comment in R/prepare-site-data.R with a number. If someone
  # "simplifies" the windows to `YEAR <= 2005` / `YEAR > 2005`, this is the
  # size of the mistake.
  skip_if_not(file.exists(FI_SOD_2015_HH), "FI-Sod FLUXNET2015 not downloaded")

  a <- readr::read_csv(
    FI_SOD_2015_HH,
    col_select = c("TIMESTAMP_START", "TS_F_MDS_1", "TS_F_MDS_2", "TS_F_MDS_1_QC"),
    col_types = readr::cols(TIMESTAMP_START = "c", .default = "d"),
    progress = FALSE
  )
  a[a == -9999] <- NA
  a$YEAR <- as.integer(substr(a$TIMESTAMP_START, 1, 4))
  bad <- a$YEAR <= 2005

  by_year <- local({
    to_deep <- lm(TS_F_MDS_2 ~ TS_F_MDS_1, data = a[bad, ], na.action = na.omit)
    to_shallow <- lm(TS_F_MDS_1 ~ TS_F_MDS_2, data = a[!bad, ], na.action = na.omit)
    predict(to_shallow, data.frame(
      TS_F_MDS_2 = predict(to_deep, data.frame(TS_F_MDS_1 = a$TS_F_MDS_1[bad]))
    ))
  })
  by_window <- recalibrate_fi_sod_soil_temp(a)$TS_F_MDS_1[bad]

  rms <- sqrt(mean((unname(by_year) - by_window)^2, na.rm = TRUE))
  expect_gt(rms, 5) # measured at 8.13 C; the point is that it is not small
})

# A synthetic record straddling the windows, for the paths that do not need
# the real file.
fi_sod_fixture <- function(start = "200101010000", n = 48 * 2600, seed = 7) {
  set.seed(seed)
  t0 <- lubridate::ymd_hm(start)
  stamps <- format(t0 + lubridate::minutes(30 * (seq_len(n) - 1)), "%Y%m%d%H%M")
  year <- as.integer(substr(stamps, 1, 4))
  deep <- 8 + 6 * sin(seq(0, 2 * pi * n / (48 * 365), length.out = n)) + rnorm(n, 0, 0.5)
  shallow <- deep + rnorm(n, 0, 0.8)
  early <- year <= 2005
  shallow[early] <- 1.6 * deep[early] - 3.5 + rnorm(sum(early), 0, 1.2)
  tibble::tibble(
    TIMESTAMP_START = stamps, YEAR = year,
    TS_F_MDS_1 = shallow, TS_F_MDS_2 = deep,
    TS_F_MDS_1_QC = 0, TS_F_MDS_2_QC = 0
  )
}

test_that("the rebuild flags what it rebuilt and leaves the rest alone", {
  a <- fi_sod_fixture()
  out <- recalibrate_fi_sod_soil_temp(a)
  bad <- a$YEAR <= 2005
  expect_true(all(out$TS_F_MDS_1_QC[bad] == 2))
  expect_equal(out$TS_F_MDS_1[!bad], a$TS_F_MDS_1[!bad])
  expect_equal(out$TS_F_MDS_1_QC[!bad], a$TS_F_MDS_1_QC[!bad])
  # And it actually corrects: the rebuilt values should track the deep sensor
  # better than the raw early ones, which is what the late relationship says.
  expect_lt(
    mean(abs(out$TS_F_MDS_1[bad] - a$TS_F_MDS_2[bad])),
    mean(abs(a$TS_F_MDS_1[bad] - a$TS_F_MDS_2[bad]))
  )
})

test_that("a record that misses either window is skipped, not an error", {
  # FI-Sod as downloaded from the shuttle alone: 2023-2024, which overlaps
  # neither window. The original indexed rows 90000:245000 here and aborted
  # with "0 (non-NA) cases", taking the whole site down.
  recent <- fi_sod_fixture(start = "202301010000", n = 48 * 700)
  expect_message(out <- recalibrate_fi_sod_soil_temp(recent), "skipping")
  expect_equal(out, recent)
})

test_that("the skip message says what the record actually holds", {
  recent <- fi_sod_fixture(start = "202301010000", n = 48 * 700)
  msg <- paste(capture_messages(recalibrate_fi_sod_soil_temp(recent)), collapse = " ")
  expect_match(msg, "2023-2024")
  expect_match(msg, "200101010000")
})

# ------------------------------------ the estimator declaration vs. the lists
#
# `fix_soil_temp()` used to pick its estimator from two hard-coded site lists
# while site_info.csv already carried the same information in
# `estimate_ts_method`. It now reads the declaration, and these hold the
# declaration to what the lists said -- so a disagreement is a test failure
# rather than a site quietly fitted without its net radiation.
test_that("estimate_ts_method reproduces the estimator site lists verbatim", {
  si <- get_site_info()
  method <- vapply(seq_len(nrow(si)), function(i) ts_estimate_method(si[i, ]), "")

  # The random-forest list from workflow 01_01, and from `fix_soil_temp()`
  # before the declaration replaced it.
  expect_setequal(
    si$site_ID[method == "NETRAD"],
    c("DE-Akm", "FR-Pue", "US-Los", "US-SRG", "CA-Man", "US-Ced", "US-Ho1", "CZ-RAJ")
  )
  # The two sites with too little soil temperature to train a forest on.
  expect_setequal(si$site_ID[method == "linear regression"], c("DE-Hte", "FR-FBn"))
  # And the declaration is exactly `netrad_column`, which is where it comes from.
  expect_identical(method == "NETRAD", !is.na(si$netrad_column))
})

test_that("the reconstruction runs at exactly the estimate_Ts sites", {
  # `prep_fluxnet_family()` had its own copy of the list too. Both readers now
  # branch on `estimate_Ts`, so this is the whole population.
  si <- get_site_info()
  expect_setequal(
    si$site_ID[si$estimate_Ts],
    c("US-Ha1", "US-Los", "US-Ho1", "US-Ho2", "US-PFa", "US-SRG", "CA-Man", "US-Ced",
      "FR-Fon", "CH-Dav", "DE-Akm", "DE-Hte", "FR-Bil", "FR-Pue", "FR-FBn", "CZ-RAJ")
  )
  # Every AmeriFlux one declares the per-site columns the estimator reads;
  # without them `fix_soil_temp()` would select nothing and fail on `TA`.
  est <- si[si$estimate_Ts, ]
  reader <- vapply(seq_len(nrow(est)), function(i) site_reader(est[i, ]), "")
  expect_false(any(is.na(est$TS[reader == "ameriflux"])))
  expect_false(any(is.na(est$TA[reader == "ameriflux"])))
})
