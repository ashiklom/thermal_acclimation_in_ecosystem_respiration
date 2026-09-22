# `get_soil_temperature()`: the one place a run's soil temperature is decided.
#
# What is pinned here is the *contract*, not the estimators (those are in
# test-soil-temp-columns.R) or the strategies' choices (test-recipes.R): one
# column leaves, its bounds belong to it, its provenance is recorded, and the
# selection reproduces what `total_tas_site()` did before the seam existed.

use_project_root()

fake_site_data <- function(verdict = "GOOD") {
  tbl <- tibble::tibble(
    YEAR = 2010L, MONTH = 6L, DAY = 1L, DOY = 152L, HOUR = 1:3, MINUTE = 0L,
    TS = c(4, 5, 6), TS_measured = c(4, 5, 6), TS_linear = c(9, 10, 11),
    TA = c(6, 7, 8), NEE = c(1, 2, 3), SWC = c(20, 21, 22)
  )
  list(
    ac = tbl, nightNEE = tbl[1:2, ],
    feature_gs = tibble::tibble(site_ID = "X-Tst", gStart = 120, gEnd = 280),
    ts_bounds = tibble::tibble(
      ts_col = c("TS_measured", "TS_measured", "TS_measured", "TS_linear", "TS_linear"),
      definition = c("climatology", "halfhourly", "climatology", "halfhourly", "climatology"),
      native = c(TRUE, FALSE, FALSE, TRUE, FALSE),
      tStart = c(5, 1, 2, 0, 3), tEnd = c(20, 25, 19, 26, 22)
    ),
    ts_qc = tibble::tibble(site_ID = "X-Tst", verdict = verdict, flags = "")
  )
}
fake_site_info <- function(ts_col = "TS_measured", ts_source = "sensor") {
  list(site_ID = "X-Tst", ts_col = ts_col, estimate_Ts = FALSE, ts_source = ts_source)
}
fake_fill <- function(n_ac = 3, n_night = 2) {
  list(
    status = "ok", method = "rf_memory", cv_rmse = 0.8, degenerate = FALSE,
    truth_synthetic = FALSE,
    ac_ts = c(40, 50, 60)[seq_len(n_ac)], night_ts = c(40, 50)[seq_len(n_night)],
    ts_bounds = tibble::tibble(
      ts_col = "TS_memfill", definition = c("halfhourly", "climatology"),
      native = FALSE, tStart = c(-2, -1), tEnd = c(30, 29)
    )
  )
}

test_that("exactly one soil-temperature column leaves, on both tables", {
  soil <- get_soil_temperature(fake_site_data(), fake_site_info(), original_recipe())
  expect_identical(ts_candidate_columns(soil$ac), "TS_final")
  expect_identical(ts_candidate_columns(soil$nightNEE), "TS_final")
  expect_equal(soil$ac$TS_final, c(4, 5, 6))
  expect_equal(soil$nightNEE$TS_final, c(4, 5))
  # and the non-soil-temperature columns are exactly what came in
  expect_equal(soil$ac$TA, c(6, 7, 8))
  expect_equal(nrow(soil$ac), 3L)
})

test_that("the bounds belong to the column that was selected", {
  meas <- get_soil_temperature(fake_site_data(), fake_site_info("TS_measured"), original_recipe())
  lin <- get_soil_temperature(fake_site_data(), fake_site_info("TS_linear"), original_recipe())
  # `original` is `native` bounds: the measured column's climatology row and
  # the regressed column's half-hourly row -- the manuscript's asymmetry.
  expect_equal(c(meas$meta$tStart, meas$meta$tEnd), c(5, 20))
  expect_equal(c(lin$meta$tStart, lin$meta$tEnd), c(0, 26))
  expect_equal(lin$ac$TS_final, c(9, 10, 11))
  # A consistent-bounds recipe changes the row but not the column.
  hh <- get_soil_temperature(fake_site_data(), fake_site_info("TS_measured"),
                             get_recipe("memfill_hh"))
  expect_equal(c(hh$meta$tStart, hh$meta$tEnd), c(1, 25))
  expect_equal(hh$ac$TS_final, c(4, 5, 6))
})

test_that("the metadata row says what TS_final is and why", {
  soil <- get_soil_temperature(fake_site_data(), fake_site_info("TS_linear"), original_recipe())
  m <- soil$meta
  expect_equal(nrow(m), 1L)
  expect_identical(m$site_ID, "X-Tst")
  expect_identical(m$ts_col, "TS_linear")
  expect_identical(m$ts_strategy, "site_info")
  expect_match(m$ts_reason, "site_info")
  expect_identical(m$ts_verdict, "GOOD")
  expect_false(m$ts_measured_synthetic)
  expect_true(is.na(m$fill_method))
  expect_identical(m$bounds_strategy, "native")
})

test_that("the fill is attached only when selected, and its rows must align", {
  bad <- fake_site_data("BAD")
  si <- fake_site_info()
  r <- get_recipe("memfill")

  soil <- get_soil_temperature(bad, si, r, fill = fake_fill())
  expect_identical(soil$meta$ts_col, "TS_memfill")
  expect_equal(soil$ac$TS_final, c(40, 50, 60))
  expect_equal(soil$nightNEE$TS_final, c(40, 50))
  expect_identical(soil$meta$fill_method, "rf_memory")
  expect_equal(c(soil$meta$tStart, soil$meta$tEnd), c(-2, 30)) # native for a fill: half-hourly
  expect_identical(ts_candidate_columns(soil$ac), "TS_final")

  # A GOOD verdict never touches the fill, so a broken one is harmless there.
  good <- get_soil_temperature(fake_site_data("GOOD"), si, r, fill = fake_fill(n_ac = 1))
  expect_identical(good$meta$ts_col, "TS_measured")

  # A misaligned fill is refused rather than written in the wrong rows.
  expect_error(get_soil_temperature(bad, si, r, fill = fake_fill(n_ac = 2)))
  # No fill at all: the strategy falls back to the regression and records why
  # -- that is `choose_ts_col()`'s documented behaviour, not an error.
  fb <- get_soil_temperature(bad, si, r, fill = NULL)
  expect_identical(fb$meta$ts_col, "TS_linear")
  expect_match(fb$meta$ts_reason, "fell back")
  # Only an explicit request for the fill, with none supplied, is an error.
  expect_error(get_soil_temperature(bad, si, r, fill = NULL, ts_col = "TS_memfill"), "TS_memfill")
})

test_that("ts_col overrides the strategy and says so", {
  soil <- get_soil_temperature(fake_site_data(), fake_site_info("TS_measured"),
                               original_recipe(), ts_col = "TS_linear")
  expect_identical(soil$meta$ts_col, "TS_linear")
  expect_match(soil$meta$ts_reason, "argument")
  expect_equal(soil$ac$TS_final, c(9, 10, 11))
})

# ---------------------------------------------- no truth, no second method
#
# Where step 01's column is not measured soil temperature at every row, a
# variant has nothing to fit its alternative against, so it keeps that column.

test_that("a variant keeps step 01's column where ts_source leaves no truth", {
  bad <- fake_site_data("BAD")
  synthetic <- fake_site_info(ts_source = "reconstructed")

  scr <- get_soil_temperature(bad, synthetic, get_recipe("screened"))
  expect_identical(scr$meta$ts_col, "TS_measured")
  expect_true(scr$meta$ts_refused)
  expect_match(scr$meta$ts_reason, "reconstructed")
  expect_match(scr$meta$ts_reason, "screen_best strategy wanted TS_linear")
  expect_equal(scr$ac$TS_final, c(4, 5, 6))

  # memory_fill: the fill is not attached even though one is supplied.
  mf <- get_soil_temperature(bad, synthetic, get_recipe("memfill"), fill = fake_fill())
  expect_identical(mf$meta$ts_col, "TS_measured")
  expect_true(mf$meta$ts_refused)
  expect_true(is.na(mf$meta$fill_method))
  expect_equal(mf$ac$TS_final, c(4, 5, 6))

  # The partial cases count as no truth too.
  for (src in c("recalibrated", "gapfill_ta", "lm_ta_cold", "borrowed_site", "ta_substitute")) {
    r <- get_soil_temperature(bad, fake_site_info(ts_source = src), get_recipe("screened"))
    expect_true(r$meta$ts_refused, info = src)
  }
})

test_that("the refusal does not fire where it should not", {
  bad <- fake_site_data("BAD")
  # A sensor, a second depth, the PI's gap fill: measured at every row.
  for (src in c("sensor", "sensor_depth2", "gapfill_pi")) {
    r <- get_soil_temperature(bad, fake_site_info(ts_source = src), get_recipe("screened"))
    expect_identical(r$meta$ts_col, "TS_linear", info = src)
    expect_false(r$meta$ts_refused, info = src)
  }
  # The manuscript's own strategy is a declaration and is exempt.
  orig <- get_soil_temperature(bad, fake_site_info("TS_linear", ts_source = "reconstructed"), original_recipe())
  expect_identical(orig$meta$ts_col, "TS_linear")
  expect_false(orig$meta$ts_refused)
  # A variant that would have chosen TS_measured anyway has nothing to refuse.
  good <- get_soil_temperature(fake_site_data("GOOD"), fake_site_info(ts_source = "reconstructed"),
                               get_recipe("screened"))
  expect_identical(good$meta$ts_col, "TS_measured")
  expect_false(good$meta$ts_refused)
  # An explicit ts_col is a sensitivity run and is honoured.
  ovr <- get_soil_temperature(bad, fake_site_info(ts_source = "reconstructed"), get_recipe("screened"),
                              ts_col = "TS_linear")
  expect_identical(ovr$meta$ts_col, "TS_linear")
  expect_false(ovr$meta$ts_refused)
})

test_that("the fill declines where there is no truth, before doing any work", {
  # A site_data with nothing in it: if the truth check did not come first,
  # this would fail on the missing tables rather than return a status.
  out <- suppressMessages(fill_soil_temp(list(), fake_site_info(ts_source = "reconstructed")))
  expect_match(out$status, "no measured truth")
  expect_match(out$status, "reconstructed")
  expect_null(out$ac_ts)
  expect_false(fill_available(out))
})

test_that("a site_data without ts_bounds is refused by name", {
  sd_ <- fake_site_data()
  sd_$ts_bounds <- NULL
  expect_error(get_soil_temperature(sd_, fake_site_info(), original_recipe()), "ts_bounds")
})

# The seam, on real data: `total_tas_site()` now reads `TS_final` from this
# function, and the structural run has to be what it was.
test_that("total_tas_site's soil temperature is get_soil_temperature's, at a real site", {
  skip_if(!site_raw_available("DE-RuC"), "DE-RuC not downloaded")
  si <- get_site_info("DE-RuC")
  sd_ <- prepped_site("DE-RuC")
  soil <- get_soil_temperature(sd_, si, original_recipe())
  r <- suppressWarnings(suppressMessages(total_tas_site(sd_, si, fit = FALSE)))
  expect_identical(r$settings$ts_col, soil$meta$ts_col)
  expect_equal(r$settings$tStart, soil$meta$tStart)
  expect_equal(r$settings$tEnd, soil$meta$tEnd)
  expect_identical(r$settings$ts_reason, soil$meta$ts_reason)
  # DE-RuC declares TS_linear, so the fit-facing column is the regression and
  # the qualification-facing one it replaced is no longer on the table.
  expect_identical(soil$meta$ts_col, "TS_linear")
  expect_false("TS_measured" %in% names(soil$ac))
})

# ============================================================== stage A
#
# `qualification_soil_temperature()`: one arm per `ts_source`, each on a
# synthetic input where the answer is known, and the provenance row that
# travels with it. The manuscript's numbers at real sites are the frozen
# oracle's business (tests/ts-swc-baseline.R); this is about each arm doing
# what its level says and nothing else.

stage_a_input <- function(n = 480, seed = 3) {
  set.seed(seed)
  t <- seq_len(n)
  ta <- 12 + 8 * sin(2 * pi * t / 48) + 4 * t / n + rnorm(n, 0, 0.5)
  ts <- 10 + 3 * sin(2 * pi * (t - 6) / 48) + 3 * t / n + rnorm(n, 0, 0.2)
  ts[seq(7, n, by = 40)] <- NA                         # sensor gaps
  stamp <- as.POSIXct("2019-01-01", tz = "UTC") + 1800 * (t - 1)
  tibble::tibble(
    TIMESTAMP = stamp + 900,
    TIMESTAMP_START = format(stamp, "%Y%m%d%H%M"),
    YEAR = c(rep(2019L, n / 2), rep(2023L, n / 2)),
    # Two "years" of the same five days, so that every day-of-year x time slot
    # has two rows and a predictor gap in one can be filled from the other.
    DOY = rep(rep(1:5, each = 48), 2), HOUR = rep(rep(0:23, each = 2), 10), MINUTE = rep(c(0L, 30L), n / 2),
    TS_sensor = ts, TS_sensor_QC = ifelse(is.na(ts), 3, 0),
    TA = ta, TA_QC = 0,
    NETRAD = 120 * pmax(sin(2 * pi * t / 48), 0) - 30,
    TS_depth2 = ts - 1.5, TS_depth2_QC = 1,
    TS_pi = ts + 0.1
  )
}
stage_a_site <- function(ts_source, ..., site_ID = "X-Tst") {
  base <- list(site_ID = site_ID, source = "FLUXNET", ts_source = ts_source,
               estimate_Ts = ts_source == "reconstructed", estimate_ts_method = NA_character_,
               netrad_column = NA_character_, TS = "TS_1", TA = "TA_1")
  # `...` overrides rather than appends: a duplicated name would make `[[`
  # return the default and the override silently unused.
  utils::modifyList(base, list(...), keep.null = TRUE)
}
run_arm <- function(ts_source, input = stage_a_input(), ...) {
  suppressMessages(qualification_soil_temperature(input, stage_a_site(ts_source, ...)))
}

test_that("every arm returns TS, TS_QC and one provenance row, aligned to the input", {
  input <- stage_a_input()
  for (src in setdiff(names(TS_SOURCES), "recalibrated")) {
    out <- run_arm(src, input, estimate_ts_method = if (src == "reconstructed") "linear regression" else NA_character_)
    expect_named(out, c("TS", "TS_QC", "provenance"), info = src)
    expect_length(out$TS, nrow(input))
    expect_length(out$TS_QC, nrow(input))
    expect_equal(nrow(out$provenance), 1L, info = src)
    expect_identical(out$provenance$ts_source, src)
    expect_identical(out$provenance$ts_truth, unname(TS_SOURCES[[src]]))
  }
})

test_that("sensor passes the sensor through, and says it did nothing", {
  input <- stage_a_input()
  out <- run_arm("sensor", input)
  expect_identical(out$TS, input$TS_sensor)
  expect_identical(out$TS_QC, input$TS_sensor_QC)
  expect_identical(out$provenance$stage_a_mode, "none")
  expect_true(is.na(out$provenance$stage_a_estimator))
})

test_that("the swaps take the column their level names, wholesale", {
  input <- stage_a_input()
  d2 <- run_arm("sensor_depth2", input)
  expect_identical(d2$TS, input$TS_depth2)
  expect_identical(d2$TS_QC, input$TS_depth2_QC)   # the second depth's own flag
  expect_identical(d2$provenance$stage_a_mode, "replace")

  ta <- run_arm("ta_substitute", input)
  expect_identical(ta$TS, input$TA)
  expect_identical(ta$TS_QC, input$TA_QC)
  expect_identical(ta$provenance$stage_a_family, "fixed:TA")
})

test_that("the gap-fills touch only the sensor's gaps", {
  input <- stage_a_input()
  gaps <- is.na(input$TS_sensor)
  expect_gt(sum(gaps), 0)

  pi <- run_arm("gapfill_pi", input)
  expect_identical(pi$TS[!gaps], input$TS_sensor[!gaps])
  expect_identical(pi$TS[gaps], input$TS_pi[gaps])
  expect_identical(pi$provenance$stage_a_mode, "fill_gaps")

  mbp <- run_arm("gapfill_ta", input)
  expect_identical(mbp$TS[!gaps], input$TS_sensor[!gaps])
  expect_equal(mbp$TS[gaps], input$TA[gaps] * 0.3688005 + 5.8670273)   # US-MBP's line
  expect_identical(mbp$provenance$stage_a_family, "fixed:TA")
})

test_that("the wholesale regressions are the registry's lines over the whole record", {
  input <- stage_a_input()
  reg <- ts_estimators()
  train <- tibble::tibble(TS = input$TS_sensor, TA = input$TA, YEAR = input$YEAR)

  cwt <- run_arm("borrowed_site", input)
  expect_equal(cwt$TS, input$TA * 0.64718 + 5.13873)
  expect_false(anyNA(cwt$TS))                                # nothing of the sensor survives
  expect_match(cwt$provenance$stage_a_note, "US-xGB")

  cold <- run_arm("lm_ta_cold", input)
  expect_equal(cold$TS, reg$lm_ta_pos$predict(reg$lm_ta_pos$fit(train), train))
  expect_identical(cold$provenance$stage_a_estimator, "lm_ta_pos")
  expect_identical(cold$provenance$stage_a_family, "lm:TA")
  expect_gt(cold$provenance$stage_a_n_train, 0)

  recent <- run_arm("lm_ta_recent", input)
  expect_equal(recent$TS, reg$lm_ta_recent$predict(reg$lm_ta_recent$fit(train), train))
  # fitted on 2023 only, so the coefficients differ from the cold arm's
  expect_false(isTRUE(all.equal(recent$TS, cold$TS)))
})

test_that("reconstructed routes on estimate_ts_method, gap-fills predictors, and sets the QC flag", {
  input <- stage_a_input()
  input$TA[c(3, 50)] <- NA                                   # predictor gaps the climatology fills

  lin <- run_arm("reconstructed", input, estimate_ts_method = "linear regression")
  expect_identical(lin$provenance$stage_a_estimator, "lm_ta_netrad")
  expect_identical(lin$provenance$stage_a_family, "lm:TA+NETRAD")
  expect_identical(lin$provenance$stage_a_mode, "replace")
  expect_false(anyNA(lin$TS))                                # every gap filled, every row predicted
  # the sensor's poor/missing flags become "good gap-filled"; its good ones stay
  expect_true(all(lin$TS_QC[input$TS_sensor_QC == 3] == 2))
  expect_true(all(lin$TS_QC[input$TS_sensor_QC == 0] == 0))

  ta_only <- run_arm("reconstructed", input)
  expect_identical(ta_only$provenance$stage_a_estimator, "lm_ta_pos")

  # DE-Hte trains on the second depth and the provenance says so
  hte <- run_arm("reconstructed", input, site_ID = "DE-Hte", estimate_ts_method = "linear regression")
  expect_match(hte$provenance$stage_a_note, "second depth")
})

test_that("the PI gap-fill is skipped, and says so, when the release has no PI column", {
  # US-ICs's BASE 13-5 dropped TS_PI_1. The sensor is still the sensor; its
  # gaps stay gaps, and the provenance row records that the fill did not run.
  input <- stage_a_input()
  bare <- input[, setdiff(names(input), "TS_pi")]
  expect_message(
    out <- qualification_soil_temperature(bare, stage_a_site("gapfill_pi")),
    "left unfilled"
  )
  expect_identical(out$TS, input$TS_sensor)
  expect_identical(out$provenance$stage_a_mode, "none")
  expect_match(out$provenance$stage_a_note, "no PI gap-filled column")
})

test_that("an arm that needs a column the record lacks fails by name, up front", {
  input <- stage_a_input()
  bare <- input[, setdiff(names(input), c("TS_depth2", "TS_depth2_QC", "TS_pi", "NETRAD"))]
  expect_error(run_arm("sensor_depth2", bare), "TS_depth2")
  expect_error(run_arm("reconstructed", bare, estimate_ts_method = "NETRAD"), "NETRAD")
  # ...and a missing required column is refused before any arm runs
  expect_error(run_arm("sensor", input[, setdiff(names(input), "TA")]), "TA")
})

test_that("FI-Sod's recalibration is skipped, and says so, when its windows are not in the record", {
  # The fixture is 2019/2023; the recalibration windows are 2001-2006.
  out <- run_arm("recalibrated", stage_a_input(), site_ID = "FI-Sod")
  expect_identical(out$TS, stage_a_input()$TS_sensor)
  expect_match(out$provenance$stage_a_note, "skipped")
})

test_that("the reader inputs hand over the record's columns under the shared names", {
  # FLUXNET-family
  a <- tibble::tibble(
    TIMESTAMP = 1:3, TIMESTAMP_START = c("a", "b", "c"), YEAR = 1L, DOY = 1L, HOUR = 0L, MINUTE = 0L,
    TS_F_MDS_1 = c(1, 2, 3), TS_F_MDS_1_QC = c(0, 0, 3), TS_F_MDS_2 = c(0.5, 1.5, 2.5), TS_F_MDS_2_QC = 1,
    TA_F_MDS = c(4, 5, 6), TA_F_MDS_QC = 0, NETRAD = c(10, 20, 30)
  )
  fx <- fluxnet_ts_input(a, stage_a_site("sensor"))
  expect_true(all(TS_INPUT_REQUIRED %in% names(fx)))
  expect_identical(fx$TS_sensor, a$TS_F_MDS_1)
  expect_identical(fx$TS_depth2, a$TS_F_MDS_2)
  expect_identical(fx$TA, a$TA_F_MDS)
  expect_identical(fx$NETRAD, a$NETRAD)
  # a record with no shallow sensor is refused unless the site substitutes air temperature
  no_ts <- a[, setdiff(names(a), c("TS_F_MDS_1", "TS_F_MDS_1_QC"))]
  expect_error(fluxnet_ts_input(no_ts, stage_a_site("sensor")), "TS_F_MDS_1")
  expect_silent(fluxnet_ts_input(no_ts, stage_a_site("ta_substitute")))

  # AmeriFlux: the declared per-site columns, and TS_PI_1 where present
  si <- get_site_info("US-NR1")
  b <- synthetic_ameriflux(si, n = 4)
  b$TS_PI_1 <- c(9, 9, 9, 9)
  am <- ameriflux_ts_input(b, si)
  expect_true(all(TS_INPUT_REQUIRED %in% names(am)))
  expect_identical(am$TS_sensor, b[[si$TS]])
  expect_identical(am$TA, b[[si$TA]])
  expect_identical(am$TS_pi, b$TS_PI_1)
  expect_true(all(is.na(am$TS_sensor)) == FALSE)
  # no sensor declared: an all-NA sensor column, so every arm sees the right length
  cwt <- ameriflux_ts_input(synthetic_ameriflux(get_site_info("US-Cwt"), n = 4), get_site_info("US-Cwt"))
  expect_true(all(is.na(cwt$TS_sensor)))
  expect_length(cwt$TS_sensor, 4L)
})

# --------------------------------------------------- ts_qc = sensor (stage A)

test_that("ts_qc = sensor replaces the derived arms with the raw sensor, and keeps the measured ones", {
  input <- stage_a_input()
  for (src in c("reconstructed", "lm_ta_cold", "borrowed_site", "ta_substitute", "recalibrated", "gapfill_ta")) {
    out <- suppressMessages(qualification_soil_temperature(
      input, stage_a_site(src, estimate_ts_method = "linear regression"), ts_qc = "sensor"
    ))
    expect_identical(out$TS, input$TS_sensor, info = src)
    expect_identical(out$provenance$stage_a_arm, "sensor", info = src)
    expect_identical(out$provenance$ts_source, src, info = src)   # the declaration is still recorded
    expect_identical(out$provenance$ts_truth, "sensor", info = src)
    expect_identical(out$provenance$ts_qc, "sensor", info = src)
  }
  # arms whose result is measured at every row still run
  pi <- qualification_soil_temperature(input, stage_a_site("gapfill_pi"), ts_qc = "sensor")
  expect_identical(pi$provenance$stage_a_arm, "gapfill_pi")
  expect_identical(pi$TS[is.na(input$TS_sensor)], input$TS_pi[is.na(input$TS_sensor)])
  # and `manuscript` is the declaration, as before
  man <- suppressMessages(qualification_soil_temperature(
    input, stage_a_site("reconstructed", estimate_ts_method = "linear regression")
  ))
  expect_identical(man$provenance$stage_a_arm, "reconstructed")
  expect_identical(man$provenance$ts_truth, "none")
  expect_error(qualification_soil_temperature(input, stage_a_site("sensor"), ts_qc = "raw"), "arg")
})

test_that("stage B and the fill read the truth stage A produced, not the declaration", {
  bad <- fake_site_data("BAD")
  synthetic <- fake_site_info(ts_source = "reconstructed")
  # Under the manuscript prep the declaration and the provenance agree: refused.
  bad$ts_provenance <- tibble::tibble(site_ID = "X-Tst", ts_source = "reconstructed", ts_qc = "manuscript",
                                      stage_a_arm = "reconstructed", ts_truth = "none")
  expect_true(get_soil_temperature(bad, synthetic, get_recipe("screened"))$meta$ts_refused)
  # Under the sensor prep stage A ran the raw sensor: there is a truth, nothing to refuse.
  bad$ts_provenance <- tibble::tibble(site_ID = "X-Tst", ts_source = "reconstructed", ts_qc = "sensor",
                                      stage_a_arm = "sensor", ts_truth = "sensor")
  r <- get_soil_temperature(bad, synthetic, get_recipe("memfill_sensor"), fill = fake_fill())
  expect_false(r$meta$ts_refused)
  expect_identical(r$meta$ts_col, "TS_memfill")
  expect_false(r$meta$ts_measured_synthetic)
  # the fill likewise
  expect_match(suppressMessages(fill_soil_temp(
    list(ts_provenance = tibble::tibble(ts_truth = "none", stage_a_arm = "reconstructed")), synthetic
  ))$status, "no measured truth")
})
