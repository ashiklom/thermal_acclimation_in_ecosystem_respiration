#!/usr/bin/env Rscript
#
# How different is a TS ~ TA regression from the soil temperature it replaces?
#
# 35 of 117 sites fit the thermal-response model on `TS_linear` -- measured
# soil temperature overlaid with a linear regression on air temperature -- and
# a further 23 have soil temperature reconstructed from air temperature in
# step 01. Nothing in the pipeline records what that substitution does to the
# predictor, because at the sites where it is applied there is no measured
# column left to compare against.
#
# This measures it where there *is* a truth: the sites whose soil temperature
# is genuinely measured. `prep_nee_ac()` now builds `TS_linear` at every site
# (inert unless selected), so both columns exist side by side.
#
# Scoring is `ts_reconstruction_metrics()`, shared with the gap-filling
# cross-validation, so the substitution the pipeline already performs is one
# row of the same table as every candidate replacement for it. See that
# function for why the metrics are what they are.
#
#   pixi run Rscript scripts/ts-linear-vs-measured.R
#   pixi run Rscript scripts/ts-linear-vs-measured.R --sites DE-Tha,US-Kon
#   pixi run Rscript scripts/ts-linear-vs-measured.R --workers 6

suppressMessages({
  library(dplyr)
  library(targets)
})
tar_source()
source("scripts/ts-rework-common.R")

args <- commandArgs(trailingOnly = TRUE)
sites <- parse_sites_arg(args, measured_ts_sites())
workers <- as.integer(parse_opt(args, "--workers", "4"))
outfile <- parse_opt(args, "--out", file.path("data-proc", "ts-rework", "ts-linear-vs-measured.csv"))

site_metrics <- function(name_site) {
  sd_ <- step01_cached(name_site)
  if (inherits(sd_, "baseline_error")) {
    return(tibble::tibble(site_ID = name_site, status = paste("step01:", as.character(sd_))))
  }
  if (!"TS_linear" %in% names(sd_$ac)) {
    return(tibble::tibble(site_ID = name_site, status = "no TS_linear column"))
  }

  fg <- sd_$feature_gs
  gStart <- fg$gStart
  gEnd <- fg$gEnd

  # Growing season only: everything `total_tas_site()` does is confined to it,
  # and the rest of the record would otherwise dominate the point statistics
  # with values the model never sees.
  gs <- sd_$ac |> dplyr::filter(dplyr::between(.data$DOY, gStart, gEnd))
  min_obs_day <- if (length(unique(gs$MINUTE)) > 1) 40 else 20

  m <- ts_reconstruction_metrics(
    gs, gs$TS_measured, gs$TS_linear, gStart, gEnd, min_obs_day = min_obs_day
  )

  # The *actual* regression `apply_ts_linear()` used, refitted through the same
  # entry points rather than approximated. Fitting `TS ~ TA` on the
  # growing-season subset instead would report coefficients that produced none
  # of the numbers above: the pipeline fits on the whole of `ac` above
  # freezing, or at US-Tw1 on the nighttime table.
  fit_dat <- ts_fit_data(sd_$ac, sd_$nightNEE, ts_linear_domain_for(get_site_info(name_site)))
  fit_dat$TS <- fit_dat$TS_measured
  fit <- ts_ta_model(fit_dat)

  # The window-skip gate. `total_tas_window()` drops a window whose mean TS
  # falls outside [max(tStart, 2), tEnd], with the bounds taken from the
  # *selected* column -- so selecting a column moves both the window means and
  # the bounds they are tested against. See scripts/ts-bounds-definition.R for
  # why most of that movement is not the column at all.
  bnd <- function(col) {
    r <- tryCatch(ts_bounds_for(sd_$ts_bounds, col), error = function(e) NULL)
    if (is.null(r)) list(tStart = NA_real_, tEnd = NA_real_) else r
  }
  bm <- bnd("TS_measured")
  bl <- bnd("TS_linear")
  win <- window_cells(gs, gStart, gEnd, "TS_measured") |>
    dplyr::select("iwindow", "growing_year", m_mean = "cell_mean") |>
    dplyr::inner_join(
      window_cells(gs, gStart, gEnd, "TS_linear") |>
        dplyr::select("iwindow", "growing_year", l_mean = "cell_mean"),
      by = c("iwindow", "growing_year")
    ) |>
    dplyr::filter(is.finite(.data$m_mean), is.finite(.data$l_mean)) |>
    dplyr::summarise(wm_m = mean(.data$m_mean), wm_l = mean(.data$l_mean), .by = "iwindow")
  keep_m <- dplyr::between(win$wm_m, max(bm$tStart, 2.0), bm$tEnd)
  keep_l <- dplyr::between(win$wm_l, max(bl$tStart, 2.0), bl$tEnd)

  tibble::tibble(
    site_ID = name_site,
    status = "ok",
    nyear = fg$nyear,
    nwindow = nrow(win),
    ta_slope = unname(coef(fit)[["TA"]]),
    ta_intercept = unname(coef(fit)[["(Intercept)"]]),
    ta_r2 = summary(fit)$r.squared
  ) |>
    dplyr::bind_cols(m) |>
    dplyr::mutate(
      tStart_m = bm$tStart, tEnd_m = bm$tEnd,
      tStart_l = bl$tStart, tEnd_l = bl$tEnd,
      n_windows_kept_m = sum(keep_m, na.rm = TRUE),
      n_windows_kept_l = sum(keep_l, na.rm = TRUE),
      n_windows_flipped = sum(keep_m != keep_l, na.rm = TRUE)
    )
}

safe_metrics <- function(name_site) {
  message("==== ", name_site, " ====")
  tryCatch(
    site_metrics(name_site),
    error = function(e) tibble::tibble(site_ID = name_site, status = paste("error:", conditionMessage(e)))
  )
}

cat("Sites:", length(sites), " workers:", workers, "\n")
res <- if (workers > 1) parallel::mclapply(sites, safe_metrics, mc.cores = workers) else lapply(sites, safe_metrics)
out <- dplyr::bind_rows(res)

dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(out, outfile)
cat("\nWrote", outfile, "-", nrow(out), "rows\n")

ok <- dplyr::filter(out, .data$status == "ok")
cat("\n  ok:", nrow(ok), " failed:", nrow(out) - nrow(ok), "\n")
if (nrow(ok)) {
  q <- function(x) sprintf("%7.3f %7.3f %7.3f", stats::quantile(x, 0.25, na.rm = TRUE),
                           stats::median(x, na.rm = TRUE), stats::quantile(x, 0.75, na.rm = TRUE))
  cat("\n", sprintf("%-32s %7s %7s %7s", "", "p25", "med", "p75"), "\n")
  row <- function(lab, x) cat(" ", sprintf("%-32s", lab), q(x), "\n")
  row("TS~TA slope", ok$ta_slope)
  row("TS~TA R2", ok$ta_r2)
  row("RMSE (C)", ok$rmse)
  row("diurnal amp ratio, measured", ok$amp_ratio_truth)
  row("diurnal amp ratio, linear", ok$amp_ratio_pred)
  row("diurnal amp inflation", ok$amp_inflation)
  row("phase lag, measured (h)", ok$lag_truth_h)
  row("WITHIN-cell sd ratio (alpha axis)", ok$within_sd_ratio)
  row("ACROSS-year spread ratio (TAS axis)", ok$across_year_spread_ratio)
  row("windows flipped by the gate", ok$n_windows_flipped)
}
