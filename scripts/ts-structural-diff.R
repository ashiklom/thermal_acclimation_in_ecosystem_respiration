#!/usr/bin/env Rscript
#
# What does substituting TS ~ TA for measured soil temperature do to the
# *structure* of a run, before any model is fitted?
#
# `total_tas_site(fit = FALSE)` walks the real window and year loop with no
# Stan sampling: it reports which windows were skipped and why, which years
# were rejected and why, how far each window had to be extended, and how many
# observations each cell ended up with. Those decisions all read the soil
# temperature column -- the window-skip gate compares the window mean against
# the column's own growing-season percentiles, and `year_rejection()` requires
# the window reference temperature to sit inside the year's 2.5/97.5 quantiles
# of it. So the substitution can change which data the model sees even where
# it barely changes the numbers.
#
# This runs in seconds per site and is exact: it is the same loop, not a
# transcription of it.
#
#   pixi run Rscript scripts/ts-structural-diff.R
#   pixi run Rscript scripts/ts-structural-diff.R --sites DE-Tha,US-Kon

suppressMessages({
  library(dplyr)
  library(targets)
})
tar_source()
source("scripts/ts-rework-common.R")

args <- commandArgs(trailingOnly = TRUE)
sites <- parse_sites_arg(args, measured_ts_sites())
workers <- as.integer(parse_opt(args, "--workers", "4"))
outdir <- parse_opt(args, "--out", file.path("data-proc", "ts-rework"))

structure_for <- function(sd_, site_info, ts_col) {
  suppressWarnings(suppressMessages(
    total_tas_site(sd_, site_info, direct = FALSE, ts_col = ts_col, fit = FALSE)
  ))
}

site_diff <- function(name_site) {
  message("==== ", name_site, " ====")
  sd_ <- step01_cached(name_site)
  if (inherits(sd_, "baseline_error")) {
    return(list(summary = tibble::tibble(site_ID = name_site, status = "step01 failed")))
  }
  si <- get_site_info(name_site)

  m <- tryCatch(structure_for(sd_, si, "TS_measured"), error = function(e) e)
  l <- tryCatch(structure_for(sd_, si, "TS_linear"), error = function(e) e)
  if (inherits(m, "error") || inherits(l, "error")) {
    return(list(summary = tibble::tibble(
      site_ID = name_site,
      status = paste0(
        "structure failed: ",
        if (inherits(m, "error")) paste("measured:", conditionMessage(m)) else "",
        if (inherits(l, "error")) paste("linear:", conditionMessage(l)) else ""
      )
    )))
  }

  # The cells that would have been fitted. Under `fit = FALSE` a surviving
  # cell is stamped `"not_fitted"` -- the four `year_rejection()` reasons
  # occupy the same field, and a window that was skipped outright contributes
  # no row at all -- so "survived" is this string and not `is.na()`.
  fitted_cells <- function(r) {
    os <- r$outcome_siteyear
    paste(os$window, os$growing_year)[!is.na(os$status) & os$status == "not_fitted"]
  }
  fm <- fitted_cells(m)
  fl <- fitted_cells(l)

  reasons <- function(r, which) {
    os <- r$outcome_siteyear
    tibble::tibble(site_ID = name_site, col = which, status = os$status) |>
      dplyr::count(.data$site_ID, .data$col, .data$status, name = "n")
  }

  skips <- dplyr::bind_rows(
    dplyr::mutate(m$window_skips, col = "TS_measured"),
    dplyr::mutate(l$window_skips, col = "TS_linear")
  )

  summary <- tibble::tibble(
    site_ID = name_site,
    status = "ok",
    nwindow = m$settings$nwindow,
    control_year_m = m$settings$control_year,
    control_year_l = l$settings$control_year,
    control_year_changed = m$settings$control_year != l$settings$control_year,
    tStart_m = m$settings$tStart, tEnd_m = m$settings$tEnd,
    tStart_l = l$settings$tStart, tEnd_l = l$settings$tEnd,
    n_window_skips_m = nrow(m$window_skips),
    n_window_skips_l = nrow(l$window_skips),
    n_cells_m = length(fm),
    n_cells_l = length(fl),
    n_cells_lost = length(setdiff(fm, fl)),
    n_cells_gained = length(setdiff(fl, fm)),
    n_cells_shared = length(intersect(fm, fl)),
    jaccard = if (length(union(fm, fl))) length(intersect(fm, fl)) / length(union(fm, fl)) else NA_real_,
    # `TS` on the outcome rows is the window reference temperature the TAS
    # regression is run against, so its spread is the thing that moves TAS.
    ref_ts_sd_m = stats::sd(m$outcome_siteyear$TS, na.rm = TRUE),
    ref_ts_sd_l = stats::sd(l$outcome_siteyear$TS, na.rm = TRUE),
    ref_ts_sd_ratio = stats::sd(l$outcome_siteyear$TS, na.rm = TRUE) /
      stats::sd(m$outcome_siteyear$TS, na.rm = TRUE)
  )

  list(
    summary = summary,
    reasons = dplyr::bind_rows(reasons(m, "TS_measured"), reasons(l, "TS_linear")),
    skips = if (nrow(skips)) dplyr::mutate(skips, site_ID = name_site) else NULL
  )
}

safe <- function(s) tryCatch(site_diff(s), error = function(e) {
  list(summary = tibble::tibble(site_ID = s, status = paste("error:", conditionMessage(e))))
})

cat("Sites:", length(sites), " workers:", workers, "\n")
res <- if (workers > 1) parallel::mclapply(sites, safe, mc.cores = workers) else lapply(sites, safe)

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
summ <- dplyr::bind_rows(lapply(res, `[[`, "summary"))
readr::write_csv(summ, file.path(outdir, "ts-structural-diff.csv"))
readr::write_csv(dplyr::bind_rows(lapply(res, `[[`, "reasons")),
                 file.path(outdir, "ts-structural-reasons.csv"))
readr::write_csv(dplyr::bind_rows(lapply(res, `[[`, "skips")),
                 file.path(outdir, "ts-structural-skips.csv"))

ok <- dplyr::filter(summ, .data$status == "ok")
cat("\n  ok:", nrow(ok), " failed:", nrow(summ) - nrow(ok), "\n")
if (nrow(ok)) {
  cat("  sites where the fitted (window x year) cell set changes:",
      sum(ok$n_cells_lost + ok$n_cells_gained > 0), "of", nrow(ok), "\n")
  cat("  sites where the control year changes:", sum(ok$control_year_changed), "\n")
  cat("  total cells lost:", sum(ok$n_cells_lost),
      " gained:", sum(ok$n_cells_gained),
      " shared:", sum(ok$n_cells_shared), "\n")
  cat("  median reference-TS sd ratio (linear / measured):",
      sprintf("%.3f", stats::median(ok$ref_ts_sd_ratio, na.rm = TRUE)), "\n")
}
