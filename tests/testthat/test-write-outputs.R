# The files the `workflows/` scripts read: their shape, not their values.

use_project_root()

analysis_path <- function(f) file.path("data-proc", "analysis", f)
orig_path <- function(f) file.path("data-proc-original", f)

test_that("outcome tables match the manuscript's columns, in order", {
  for (f in c("outcome_temp.csv", "outcome_temp_water_gpp.csv",
              "outcome_siteyear_temp.csv", "outcome_siteyear_temp_water_gpp.csv")) {
    skip_if_not(file.exists(analysis_path(f)), paste(f, "not written yet"))
    skip_if_not(file.exists(orig_path(f)), paste("no frozen", f))
    ours <- names(read.csv(analysis_path(f), nrows = 1))
    orig <- names(read.csv(orig_path(f), nrows = 1))
    # Ours may carry extra trailing columns (`status`), but every manuscript
    # column has to be present and in the manuscript's order, so a diff against
    # data-proc-original/ lines up.
    expect_equal(ours[seq_along(orig)], orig, info = f)
  }
})

test_that("each outcome table is sorted and carries each site once", {
  # This used to assert the two files were row-for-row identical, because
  # `02_02` grafted the direct model's TAS onto the total model's table by
  # position. It now joins on site_ID, so the two legitimately differ: a site
  # can fit under one model and fail under the other, as IT-Noe did on the
  # 2026-09-24 run when its ERA5 fallback was unusable.
  #
  # What still has to hold is per-file: sorted, and one row per site. A
  # duplicate site would silently multiply rows through any join downstream,
  # and the sort is the column contract the `workflows/` scripts read against.
  #
  # A run scoped to one model (THERMAL_MODELS=total) writes the other file as
  # a header only, which reads as zero rows of logical columns; skip rather
  # than compare those.
  ft <- file.path("data-proc", "analysis", "outcome_temp.csv")
  fd <- file.path("data-proc", "analysis", "outcome_temp_water_gpp.csv")
  skip_if_not(file.exists(ft) && file.exists(fd), "not written yet")
  tot <- read.csv(ft)
  dir <- read.csv(fd)
  skip_if(nrow(tot) == 0 || nrow(dir) == 0, "one model was not in this run")
  expect_identical(tot$site_ID, sort(tot$site_ID))
  expect_identical(dir$site_ID, sort(dir$site_ID))
  expect_identical(anyDuplicated(tot$site_ID), 0L)
  expect_identical(anyDuplicated(dir$site_ID), 0L)
})

test_that("growing_season_features covers every site in the run", {
  # `04_02` looks every site up in it; a missing one gives `integer(0)` bounds.
  #
  # "The run" is the run that wrote the files, not this process's default
  # scope: THERMAL_SITES may well have been set differently when the pipeline
  # ran. The settings table is written by the same run, so it is the record
  # of which sites that was.
  f <- file.path("data-proc", "features", "growing_season_features.csv")
  s <- file.path("data-proc", "analysis", "variant_settings.csv")
  skip_if_not(file.exists(f), "not written yet")
  skip_if_not(file.exists(s), "variant_settings.csv not written yet")
  feats <- read.csv(f)
  run_sites <- unique(read.csv(s)$site_ID)
  expect_true(all(c("site_ID", "gStart", "gEnd", "tStart", "tEnd", "nyear") %in% names(feats)))
  expect_true(all(run_sites %in% feats$site_ID))
  expect_false(any(duplicated(feats$site_ID)))
  expect_false(any(is.na(feats$gStart) | is.na(feats$gEnd)))
})

test_that("collect_outcome sorts by site, which is the writers' column contract", {
  mk <- function(s) list(outcome = tibble::tibble(site_ID = s, TAS = seq_along(s)))
  got <- collect_outcome(mk("NL-Loo"), mk("DE-Akm"), mk("FI-Sod"))
  expect_identical(got$site_ID, c("DE-Akm", "FI-Sod", "NL-Loo"))
})

test_that("collect_feature_gs keeps one row per site", {
  mk <- function(s) list(feature_gs = tibble::tibble(site_ID = s, gStart = 1L, gEnd = 2L))
  got <- collect_feature_gs(mk("SE-Deg"), mk("DE-RuC"))
  expect_identical(got$site_ID, c("DE-RuC", "SE-Deg"))
  expect_equal(nrow(got), 2L)
})

test_that("collect_window_skips keeps its columns when nothing was skipped", {
  out <- collect_window_skips(list(window_skips = tibble::tibble()), NULL)
  expect_equal(nrow(out), 0L)
  expect_identical(names(out), names(WINDOW_SKIPS_EMPTY))
  # so the CSV the reports read has a header
  f <- withr::local_tempfile(fileext = ".csv")
  write_result_csv(out, f)
  expect_identical(names(read.csv(f)), names(WINDOW_SKIPS_EMPTY))
})

test_that("new site-years are those past the manuscript's last year, fitted only", {
  ms <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(site_ID = c("A", "A"), growing_year = c(2010, 2012)), ms, row.names = FALSE)
  sy <- tibble::tibble(
    site_ID = c("A", "A", "A", "A", "B"),
    recipe_id = "original", model = c("total", "total", "direct", "total", "total"),
    growing_year = c(2011L, 2013L, 2013L, 2014L, 2001L),
    ERref = c(1, 1, 1, NA, 1)
  )
  out <- collect_new_siteyears(sy, ms)
  # 2011 is inside the manuscript's span; 2014 was not fitted; B is new
  expect_equal(paste(out$site_ID, out$growing_year), c("A 2013", "B 2001"))
  expect_equal(out$models, c("direct,total", "total"))
  expect_true(is.na(out$manuscript_last_year[out$site_ID == "B"]))
  expect_equal(nrow(collect_new_siteyears(sy[0, ], ms)), 0)
})

test_that("the run and variant writers split the tables by the report that reads them", {
  withr::local_dir(withr::local_tempdir())
  fit <- function(site, recipe, model) list(
    outcome = tibble::tibble(site_ID = site, recipe_id = recipe, model = model, TAS = 1),
    outcome_siteyear = tibble::tibble(site_ID = site, recipe_id = recipe, model = model,
                                      growing_year = 2001L, window = "w1", ERref = 1),
    settings = tibble::tibble(site_ID = site, recipe_id = recipe, model = model, fit_profile = "fast"),
    window_skips = tibble::tibble()
  )
  ft <- collect_fit_tables(fit("A", "original", "total"), fit("A", "original", "direct"),
                           fit("A", "memfill", "total"), NULL)
  expect_named(ft, c("outcome", "siteyear", "settings", "window_skips"))
  expect_equal(nrow(ft$outcome), 3)

  sd <- list(feature_gs = tibble::tibble(site_ID = "A", gStart = 1),
             ts_qc = tibble::tibble(site_ID = "A", verdict = "OK"),
             ts_provenance = tibble::tibble(site_ID = "A", stage_a_arm = "sensor"))
  st <- collect_site_tables(sd)
  fl <- collect_fill_tables(list(site_ID = "A", status = "ok", method = "lm_ta"))

  ms <- "manuscript_siteyear.csv"
  write.csv(data.frame(site_ID = "A", growing_year = 2000), ms, row.names = FALSE)
  run <- write_run_csvs(ft, st, ms)
  var <- write_variant_csvs(ft, st, fl)
  expect_setequal(basename(run), paste0(c(
    "outcome_temp", "outcome_temp_water_gpp", "outcome_siteyear_temp",
    "outcome_siteyear_temp_water_gpp", "run_settings", "window_skips", "new_siteyears",
    "growing_season_features"
  ), ".csv"))
  expect_setequal(basename(var), paste0(c(
    "variant_outcome", "variant_siteyear", "variant_settings", "variant_window_skips",
    "ts_qc", "ts_provenance", "fill_cv", "fill_summary"
  ), ".csv"))
  expect_true(all(file.exists(c(run, var))))
  expect_true(file.path(DIR_FEATURES, "growing_season_features.csv") %in% run)

  # The manuscript layout is the `original` recipe and one model, its columns dropped
  tot <- read.csv(file.path(DIR_ANALYSIS, "outcome_temp.csv"))
  expect_equal(nrow(tot), 1)
  expect_false(any(c("recipe_id", "model", "fit_profile") %in% names(tot)))
  expect_equal(nrow(read.csv(file.path(DIR_ANALYSIS, "variant_outcome.csv"))), 3)
  expect_equal(read.csv(file.path(DIR_ANALYSIS, "new_siteyears.csv"))$growing_year, 2001)
})
