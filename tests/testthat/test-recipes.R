# Recipes: the registry, its validation, and the strategies each axis resolves
# to. Nothing here touches flux data; the equivalence of the `original` recipe
# to the pre-recipe code is pinned by tests/ts-swc-baseline.R and
# test-structure-only.R.

use_project_root()

test_that("the registry reads, validates, and contains original", {
  r <- read_recipes()
  expect_true("original" %in% r$recipe_id)
  expect_true(all(names(RECIPE_AXES) %in% names(r)))
  expect_false(anyDuplicated(r$recipe_id) > 0)
  # every row is a valid recipe
  for (id in r$recipe_id) expect_s3_class(get_recipe(id), "recipe")
})

test_that("the CSV's original row agrees with original_recipe()", {
  csv <- get_recipe("original")
  ref <- original_recipe()
  for (axis in names(RECIPE_AXES)) expect_identical(csv[[axis]], ref[[axis]], info = axis)
})

test_that("an unknown strategy or a bad id fails by name", {
  expect_error(
    new_recipe("x", ts = "magic", season = "detect", bounds = "native",
               swc = "site_info", year_qc = "site_info"),
    "axis 'ts' is 'magic'"
  )
  expect_error(
    new_recipe("Bad-Id", ts = "site_info", season = "detect", bounds = "native",
               swc = "site_info", year_qc = "site_info"),
    "recipe_id must be"
  )
  bad <- get_recipe("memfill")
  bad$season <- "lunar"
  expect_error(validate_recipe(bad), "season")
})

test_that("every current recipe shares one prep key", {
  # This is the assumption _targets.R asserts; the test explains what it
  # protects. Two recipes with different prep keys need two site_data targets.
  keys <- vapply(read_recipes()$recipe_id, function(id) recipe_prep_key(get_recipe(id)), "")
  expect_length(unique(keys), 1)
  expect_identical(unname(unique(keys)), "site_info")
})

test_that("pipeline_recipes and pipeline_models scope by env var semantics", {
  all_ids <- read_recipes()$recipe_id
  expect_identical(pipeline_recipes("all"), all_ids)
  expect_identical(pipeline_recipes("dev"), DEV_RECIPES)
  expect_true(all(DEV_RECIPES %in% all_ids))
  expect_identical(pipeline_recipes("original, memfill"), c("original", "memfill"))
  expect_error(pipeline_recipes("original,nonesuch"), "nonesuch")

  expect_identical(pipeline_models("total,direct"), c("total", "direct"))
  expect_identical(pipeline_models("total"), "total")
  expect_error(pipeline_models("total,indirect"), "indirect")
})

test_that("fit_settings has a full and a fast profile and nothing else", {
  full <- fit_settings("full")
  fast <- fit_settings("fast")
  expect_identical(full$profile, "full")
  expect_true(full$retry)
  expect_false(fast$retry)
  expect_lt(fast$iter, full$iter)
  expect_lt(fast$chains, full$chains)
  # The full profile is the manuscript's configuration.
  expect_identical(full$iter, 1000)
  expect_identical(full$chains, 4)
  expect_identical(full$prior_iter, 2000)
  expect_error(fit_settings("medium"), "medium")
})

# ------------------------------------------------------------- strategies

fake_site_data <- function(verdict = "GOOD", flags = "") {
  list(
    ts_qc = tibble::tibble(verdict = verdict, flags = flags),
    feature_gs = tibble::tibble(gStart = 120, gEnd = 280)
  )
}
fake_fill <- function(ok = TRUE) {
  if (ok) list(status = "ok", method = "rf_memory", ac_ts = c(1, 2)) else list(status = "too few rows")
}

test_that("choose_ts_col follows the recipe and the verdict", {
  si <- list(ts_col = "TS_linear")
  r <- function(ts) new_recipe("r", ts = ts, season = "detect", bounds = "native",
                               swc = "site_info", year_qc = "site_info")

  expect_identical(choose_ts_col(r("site_info"), fake_site_data("BAD"), si)$ts_col, "TS_linear")
  expect_identical(choose_ts_col(r("site_info"), fake_site_data("GOOD"), si)$ts_col, "TS_linear")

  expect_identical(choose_ts_col(r("screen_best"), fake_site_data("GOOD"), si)$ts_col, "TS_measured")
  expect_identical(choose_ts_col(r("screen_best"), fake_site_data("BAD", "airlike"), si)$ts_col, "TS_linear")
  expect_match(choose_ts_col(r("screen_best"), fake_site_data("BAD", "airlike"), si)$reason, "airlike")

  expect_identical(choose_ts_col(r("memory_fill"), fake_site_data("GOOD"), si, fake_fill())$ts_col, "TS_measured")
  expect_identical(choose_ts_col(r("memory_fill"), fake_site_data("BAD"), si, fake_fill())$ts_col, "TS_memfill")
  # no usable fill: fall back, and say so
  fb <- choose_ts_col(r("memory_fill"), fake_site_data("BAD"), si, fake_fill(ok = FALSE))
  expect_identical(fb$ts_col, "TS_linear")
  expect_match(fb$reason, "fell back")
  expect_match(fb$reason, "too few rows")
  fb2 <- choose_ts_col(r("memory_fill"), fake_site_data("BAD"), si, NULL)
  expect_identical(fb2$ts_col, "TS_linear")
})

test_that("a site_data without a verdict is refused, not defaulted", {
  r <- get_recipe("screened")
  expect_error(choose_ts_col(r, list(feature_gs = NULL), list(ts_col = "TS_measured")), "ts_qc")
})

test_that("choose_window_season changes only the span", {
  fg <- tibble::tibble(gStart = 120, gEnd = 280)
  d <- choose_window_season(get_recipe("original"), fg)
  w <- choose_window_season(get_recipe("noseason"), fg)
  expect_identical(c(d$gStart, d$gEnd), c(120, 280))
  expect_identical(c(w$gStart, w$gEnd), c(1, 366))
})

test_that("choose_swc_col: era5 is direct-only, site_info defers to the declaration", {
  si_yes <- list(SWC_use = TRUE)
  si_no <- list(SWC_use = FALSE)
  r_orig <- get_recipe("original")
  r_era5 <- new_recipe("e", ts = "site_info", season = "detect", bounds = "native",
                       swc = "era5", year_qc = "site_info")
  expect_true(is.na(choose_swc_col(r_era5, si_yes, direct = FALSE)$swc_col))
  expect_identical(choose_swc_col(r_era5, si_yes, direct = TRUE)$swc_col, "SWC_era5")
  expect_identical(choose_swc_col(r_orig, si_yes, direct = TRUE)$swc_col,
                   default_swc_col(si_yes, TRUE))
  expect_identical(choose_swc_col(r_orig, si_no, direct = TRUE)$swc_col,
                   default_swc_col(si_no, TRUE))
})

# ---------------------------------------------------------- bounds table

test_that("ts_bounds_for resolves native rows and named definitions", {
  tb <- tibble::tibble(
    ts_col = c("TS_measured", "TS_measured", "TS_measured", "TS_linear", "TS_linear"),
    definition = c("climatology", "halfhourly", "climatology", "halfhourly", "climatology"),
    native = c(TRUE, FALSE, FALSE, TRUE, FALSE),
    tStart = c(5, 1, 4, 0, 3), tEnd = c(20, 25, 21, 26, 22)
  )
  expect_identical(ts_bounds_for(tb, "TS_measured"), list(tStart = 5, tEnd = 20))
  expect_identical(ts_bounds_for(tb, "TS_linear"), list(tStart = 0, tEnd = 26))
  expect_identical(ts_bounds_for(tb, "TS_measured", "halfhourly"), list(tStart = 1, tEnd = 25))
  expect_identical(ts_bounds_for(tb, "TS_linear", "climatology"), list(tStart = 3, tEnd = 22))
  expect_error(ts_bounds_for(tb, "TS_memfill"), "TS_memfill")
  expect_error(ts_bounds_for(tb, "TS_measured", "lunar"), "lunar")

  # a pre-definition table still resolves by column alone
  legacy <- tibble::tibble(ts_col = "TS_measured", tStart = 2, tEnd = 19)
  expect_identical(ts_bounds_for(legacy, "TS_measured"), list(tStart = 2, tEnd = 19))
  expect_error(ts_bounds_for(legacy, "TS_measured", "halfhourly"), "predates")
})

test_that("the climatology band is narrower than the half-hourly one on the same data", {
  # Averaging over years and over the day removes variance before the
  # quantile is taken; finding F4 measured this at 10 vs 17 C over 44 sites.
  set.seed(1)
  doy <- rep(1:365, each = 48)
  yrs <- 4
  doy <- rep(doy, yrs)
  hour <- rep(rep(0:47 / 2, 365), yrs)
  ts <- 12 - 10 * cos(2 * pi * doy / 365) + 3 * sin(2 * pi * hour / 24) + rnorm(length(doy), 0, 1.5)
  rows <- ts_bounds_rows(ts, doy, 120, 280, "X")
  hh <- rows[rows$definition == "halfhourly", ]
  cl <- rows[rows$definition == "climatology", ]
  expect_lt(cl$tEnd - cl$tStart, hh$tEnd - hh$tStart)
  expect_false(any(rows$native))
})

# ------------------------------------------------------------- verdict

test_that("ts_verdict_from names the rule that fired", {
  base <- tibble::tibble(amp_ratio = 0.3, lag_unwrapped = 2, stuck_frac = 0.01, coverage = 0.9)
  expect_identical(ts_verdict_from(base)$verdict, "GOOD")
  expect_identical(ts_verdict_from(base)$flags, "")

  air <- dplyr::mutate(base, amp_ratio = 1.1, lag_unwrapped = 0.2)
  expect_identical(ts_verdict_from(air)$verdict, "BAD")
  expect_identical(ts_verdict_from(air)$flags, "airlike")

  two <- dplyr::mutate(base, stuck_frac = 0.5, coverage = 0.1)
  expect_identical(ts_verdict_from(two)$flags, "stuck,coverage")

  # a strongly damped sensor is not "flat", and a real lag is not "leads"
  deep <- dplyr::mutate(base, amp_ratio = 0.05, lag_unwrapped = 9)
  expect_identical(ts_verdict_from(deep)$verdict, "GOOD")
})

test_that("the stuck-value test exempts the zero curtain", {
  # Six weeks pinned at 0.0 C under snow, then a normal signal.
  x <- c(rep(0.0, 2000), 5 + sin(seq_len(2000) / 20))
  st <- stuck_stats(x)
  expect_lt(st[["frac"]], 0.01)
  # ...but the same run at 4.0 C is a stuck logger.
  y <- c(rep(4.0, 2000), 5 + sin(seq_len(2000) / 20))
  expect_gt(stuck_stats(y)[["frac"]], 0.4)
})
