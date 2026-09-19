#!/usr/bin/env Rscript
#
# The growing-season temperature bounds `tStart`/`tEnd` are computed from two
# different populations depending on which soil-temperature column they
# describe, and nothing says so.
#
#   * `TS_measured` takes them from `detect_growing_season()`, which is
#     `quantile(tmp[[ts_col]], c(0.025, 0.975))` where `tmp` is the
#     *day-of-year climatology* -- `ac` summarised `.by = "DOY"` and then
#     filtered by the NEE cut-off. Averaging over years and over the day
#     removes the diurnal and interannual variance before the quantile is
#     taken.
#   * `TS_linear` takes them from `ts_bounds()`, which is
#     `quantile(ts[between(doy, gStart, gEnd)], c(0.025, 0.975))` on the raw
#     *half-hourly* values.
#
# A half-hourly distribution is far wider than the climatology of the same
# data, so the second definition yields a lower `tStart` and a higher `tEnd`
# whatever column it is applied to. The bounds gate `total_tas_window()`'s
# window-skip test, so selecting a column silently changes the definition of
# the gate as well as the data it is applied to.
#
# `docs/derived-columns.md` records that at US-Kon the bounds move from
# 18.47/26.73 to 12.93/31.33 on substitution, and attributes the move to the
# substitution. This script separates the two causes: how much of that gap is
# the column changing, and how much is only the definition changing.
#
#   pixi run Rscript scripts/ts-bounds-definition.R

suppressMessages({
  library(dplyr)
  library(targets)
})
tar_source()
source("scripts/ts-rework-common.R")

args <- commandArgs(trailingOnly = TRUE)
sites <- parse_sites_arg(args, measured_ts_sites())
outfile <- parse_opt(args, "--out", file.path("data-proc", "ts-rework", "ts-bounds-definition.csv"))

one <- function(name_site) {
  sd_ <- step01_cached(name_site)
  if (inherits(sd_, "baseline_error")) {
    return(tibble::tibble(site_ID = name_site, status = "step01 failed"))
  }
  fg <- sd_$feature_gs
  b_meas <- tryCatch(ts_bounds_for(sd_$ts_bounds, "TS_measured"), error = function(e) NULL)
  b_lin <- tryCatch(ts_bounds_for(sd_$ts_bounds, "TS_linear"), error = function(e) NULL)
  if (is.null(b_meas) || is.null(b_lin)) {
    return(tibble::tibble(site_ID = name_site, status = "missing a bounds row"))
  }

  # The half-hourly definition applied to the *measured* column: the control
  # that separates the definition change from the column change.
  hh_meas <- ts_bounds(sd_$ac$TS_measured, sd_$ac$DOY, fg$gStart, fg$gEnd)

  tibble::tibble(
    site_ID = name_site,
    status = "ok",
    # as the pipeline reports them
    tStart_meas_pipeline = b_meas$tStart,   # climatology definition
    tEnd_meas_pipeline = b_meas$tEnd,
    tStart_lin_pipeline = b_lin$tStart,     # half-hourly definition
    tEnd_lin_pipeline = b_lin$tEnd,
    # the same measured column under the other definition
    tStart_meas_halfhourly = max(hh_meas$tStart, 0.0),
    tEnd_meas_halfhourly = hh_meas$tEnd,
    # decomposition of the reported move
    d_tStart_total = b_lin$tStart - b_meas$tStart,
    d_tStart_definition = max(hh_meas$tStart, 0.0) - b_meas$tStart,
    d_tStart_column = b_lin$tStart - max(hh_meas$tStart, 0.0),
    d_tEnd_total = b_lin$tEnd - b_meas$tEnd,
    d_tEnd_definition = hh_meas$tEnd - b_meas$tEnd,
    d_tEnd_column = b_lin$tEnd - hh_meas$tEnd,
    range_meas_pipeline = b_meas$tEnd - b_meas$tStart,
    range_meas_halfhourly = hh_meas$tEnd - max(hh_meas$tStart, 0.0),
    range_lin_pipeline = b_lin$tEnd - b_lin$tStart
  )
}

res <- dplyr::bind_rows(lapply(sites, function(s) {
  tryCatch(one(s), error = function(e) tibble::tibble(site_ID = s, status = conditionMessage(e)))
}))
dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(res, outfile)

ok <- dplyr::filter(res, .data$status == "ok")
cat("\n  ok:", nrow(ok), "of", nrow(res), "\n\n")
if (nrow(ok)) {
  f <- function(x) sprintf("%+6.2f  [%+6.2f, %+6.2f]", stats::median(x),
                           stats::quantile(x, 0.1), stats::quantile(x, 0.9))
  cat("  Shift in tStart (C), median [10th, 90th]\n")
  cat("    total (as the pipeline sees it) ", f(ok$d_tStart_total), "\n")
  cat("    from the definition change only ", f(ok$d_tStart_definition), "\n")
  cat("    from the column change only     ", f(ok$d_tStart_column), "\n\n")
  cat("  Shift in tEnd (C)\n")
  cat("    total (as the pipeline sees it) ", f(ok$d_tEnd_total), "\n")
  cat("    from the definition change only ", f(ok$d_tEnd_definition), "\n")
  cat("    from the column change only     ", f(ok$d_tEnd_column), "\n\n")
  cat("  Width of the admissible band (C), median\n")
  cat(sprintf("    measured, climatology definition  %6.2f\n", stats::median(ok$range_meas_pipeline)))
  cat(sprintf("    measured, half-hourly definition  %6.2f\n", stats::median(ok$range_meas_halfhourly)))
  cat(sprintf("    linear,   half-hourly definition  %6.2f\n", stats::median(ok$range_lin_pipeline)))
  cat("\n  Wrote", outfile, "\n")
}
