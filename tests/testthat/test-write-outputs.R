# The files the `workflows/` scripts read.
#
# These used not to exist at all: the DAG ended at in-memory objects and both
# `02_02` and `03_01` failed at their first `read.csv`. What is asserted here is
# the shape those scripts depend on, not the values.

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

test_that("the two outcome tables share row order", {
  # `02_02` grafts the direct model's TAS onto the total model's table by
  # position, so the two files must agree on row order. A run scoped to one
  # model (THERMAL_MODELS=total) writes the other file as a header only, and
  # there is then no order to compare; skip rather than compare a character
  # column with an empty logical one.
  ft <- file.path("data-proc", "analysis", "outcome_temp.csv")
  fd <- file.path("data-proc", "analysis", "outcome_temp_water_gpp.csv")
  skip_if_not(file.exists(ft) && file.exists(fd), "not written yet")
  tot <- read.csv(ft)
  dir <- read.csv(fd)
  skip_if(nrow(tot) == 0 || nrow(dir) == 0, "one model was not in this run")
  expect_identical(tot$site_ID, dir$site_ID)
  expect_identical(tot$site_ID, sort(tot$site_ID))
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

test_that("collect_outcome sorts by site so the positional graft is safe", {
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
