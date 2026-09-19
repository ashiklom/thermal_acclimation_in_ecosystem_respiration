#!/usr/bin/env Rscript
#
# Does substituting a TS ~ TA regression for measured soil temperature change
# the estimated thermal-response strength?
#
# Everything measured so far is structural: the regression inflates the
# within-cell spread of soil temperature, widens the quality gates and shifts
# the control year. None of that is TAS. This runs the real thing --
# `total_tas_site()` with Stan sampling -- on the same site under each column
# and reports the difference.
#
# A third run repeats the *measured* column unchanged. TAS comes out of a
# chain of several hundred MCMC fits with no fixed seed, so two runs of the
# identical configuration do not agree exactly, and without knowing that
# spread a difference between the columns cannot be called real. The
# replicate is the noise floor the column effect has to clear.
#
#   pixi run Rscript scripts/ts-tas-sensitivity.R --sites SE-Nor --workers 1
#   pixi run Rscript scripts/ts-tas-sensitivity.R --workers 4

suppressMessages({
  library(dplyr)
  library(targets)
})
tar_source()
source("scripts/ts-rework-common.R")

args <- commandArgs(trailingOnly = TRUE)
# Chosen to span the within-cell spread inflation measured by
# scripts/ts-linear-vs-measured.R, at sites cheap enough to fit twice over:
#
#   DE-Tha  within 1.24, measured amplitude ratio 0.49  -- mild, healthy sensor
#   US-ICh  within 1.43, 0.32                           -- moderate
#   US-Ro4  within 2.00, 0.32                           -- strong
#   SE-Nor  within 2.57, 0.15                           -- strongest, cheapest
#   US-Whs  within 0.85, 1.56                           -- control: the measured
#           sensor is already air-like, so the substitution should do little
DEFAULT_SITES <- c("SE-Nor", "US-ICh", "US-Ro4", "DE-Tha", "US-Whs")
sites <- parse_sites_arg(args, DEFAULT_SITES)
workers <- as.integer(parse_opt(args, "--workers", "4"))
direct <- parse_flag(args, "--direct")
outdir <- parse_opt(args, "--out", file.path("data-proc", "ts-rework"))

one_run <- function(sd_, si, ts_col, label) {
  t0 <- Sys.time()
  r <- tryCatch(
    suppressWarnings(total_tas_site(sd_, si, direct = direct, ts_col = ts_col, fit = TRUE)),
    error = function(e) e
  )
  if (inherits(r, "error")) {
    return(tibble::tibble(
      site_ID = si$site_ID, run = label, ts_col = ts_col,
      status = paste("error:", conditionMessage(r))
    ))
  }
  os <- r$outcome_siteyear
  r$outcome |>
    dplyr::mutate(
      run = label, ts_col = ts_col, status = "ok",
      model = if (direct) "direct" else "total",
      tStart = r$settings$tStart, tEnd = r$settings$tEnd,
      n_cells = sum(!is.na(os$alpha)),
      n_window_skips = nrow(r$window_skips),
      alpha_mean = mean(os$alpha, na.rm = TRUE),
      alpha_sd = stats::sd(os$alpha, na.rm = TRUE),
      beta_mean = mean(os$beta, na.rm = TRUE),
      ref_ts_sd = stats::sd(os$TS, na.rm = TRUE),
      lnRatio_sd = stats::sd(os$lnRatio, na.rm = TRUE),
      minutes = as.numeric(difftime(Sys.time(), t0, units = "mins")),
      .before = "RMSE"
    )
}

site_runs <- function(name_site) {
  message("######## ", name_site, " ########")
  sd_ <- step01_cached(name_site)
  if (inherits(sd_, "baseline_error")) {
    return(tibble::tibble(site_ID = name_site, status = "step01 failed"))
  }
  si <- get_site_info(name_site)
  dplyr::bind_rows(
    one_run(sd_, si, "TS_measured", "measured"),
    one_run(sd_, si, "TS_linear", "linear"),
    # Same configuration as the first run. Any difference is sampler noise.
    one_run(sd_, si, "TS_measured", "measured_replicate")
  )
}

safe <- function(s) tryCatch(site_runs(s), error = function(e) {
  tibble::tibble(site_ID = s, status = paste("error:", conditionMessage(e)))
})

cat("Sites:", paste(sites, collapse = ","), " workers:", workers,
    " model:", if (direct) "direct" else "total", "\n")
t0 <- Sys.time()
res <- if (workers > 1) parallel::mclapply(sites, safe, mc.cores = workers) else lapply(sites, safe)
out <- dplyr::bind_rows(res)
cat("elapsed:", format(Sys.time() - t0), "\n")

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
outfile <- file.path(outdir, paste0("ts-tas-sensitivity-", if (direct) "direct" else "total", ".csv"))
readr::write_csv(out, outfile)
cat("Wrote", outfile, "\n\n")

ok <- dplyr::filter(out, .data$status == "ok")
if (nrow(ok)) {
  print(as.data.frame(dplyr::select(
    ok, "site_ID", "run", "TAS", "TASp", "R2", "RMSE",
    "control_year", "n_cells", "n_window_skips", "alpha_mean", "minutes"
  )), row.names = FALSE, digits = 4)

  w <- ok |>
    dplyr::select("site_ID", "run", "TAS") |>
    tidyr::pivot_wider(names_from = "run", values_from = "TAS")
  if (all(c("measured", "linear", "measured_replicate") %in% names(w))) {
    w <- w |> dplyr::mutate(
      column_effect = .data$linear - .data$measured,
      noise_floor = .data$measured_replicate - .data$measured,
      ratio = abs(.data$column_effect) / pmax(abs(.data$noise_floor), 1e-12)
    )
    cat("\n  TAS: column effect against the sampler-noise floor\n\n")
    print(as.data.frame(w), row.names = FALSE, digits = 4)
  }
}
