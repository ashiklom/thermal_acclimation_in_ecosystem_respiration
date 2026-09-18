#!/usr/bin/env Rscript
# Unit tests for the data-preparation layer. Run from the project root:
#   pixi run test        (or)   Rscript tests/run-tests.R
#
# `tests/testthat/setup.R` resets the working directory to the project root,
# because the pipeline addresses its inputs relative to there.

library(testthat)
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
