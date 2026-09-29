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
  # This table had no producer anywhere in the repo, and the copy on disk held 8
  # sites, so `04_02` failed with `integer(0)` bounds for everything else.
  # `load_growing_season_features()` checks only that the file exists.
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
  expect_identical(names(out), WINDOW_SKIP_COLS)
  # so the CSV the reports read has a header
  f <- withr::local_tempfile(fileext = ".csv")
  write_result_csv(out, f)
  expect_identical(names(read.csv(f)), WINDOW_SKIP_COLS)
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
