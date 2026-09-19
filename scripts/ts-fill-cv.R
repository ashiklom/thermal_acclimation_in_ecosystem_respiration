#!/usr/bin/env Rscript
#
# Blocked cross-validation of soil-temperature reconstruction methods.
#
# Two questions, one harness.
#
#   1. How optimistic is the random 70/30 split `predict_soil_temp()` uses?
#      Running the *same* model under `random` and under contiguous blocking
#      answers it as a measurement rather than an assertion.
#
#   2. Which method should a given site use? Scored on what the thermal-
#      response model does with the column -- the within-cell spread that
#      identifies `alpha` and the across-year spread of cell means that TAS
#      is the slope against -- not on pooled RMSE, which is blind to both.
#
# Each (method, blocking) pair produces one out-of-fold prediction per row,
# assembled into a complete series. That series is a simulation of "what if
# this site's soil temperature had had to be reconstructed", and it is scored
# with `ts_reconstruction_metrics()` -- the same scorer used on the
# `TS_linear` substitution the pipeline already performs, so the incumbent is
# one row of the same table.
#
#   pixi run Rscript scripts/ts-fill-cv.R --sites DE-Tha --workers 1
#   pixi run Rscript scripts/ts-fill-cv.R --workers 8
#   pixi run Rscript scripts/ts-fill-cv.R --blockings random,year,multiyear,season

suppressMessages({
  library(dplyr)
  library(targets)
})
tar_source()
source("scripts/ts-rework-common.R")
source("scripts/ts-fill-methods.R")

args <- commandArgs(trailingOnly = TRUE)
sites <- parse_sites_arg(args, measured_ts_sites())
workers <- as.integer(parse_opt(args, "--workers", "4"))
outfile <- parse_opt(args, "--out", file.path("data-proc", "ts-rework", "ts-fill-cv.csv"))
blockings <- strsplit(parse_opt(args, "--blockings", "random,year,multiyear"), ",")[[1]]
max_train <- as.integer(parse_opt(args, "--max-train", "20000"))

stopifnot(all(blockings %in% names(TS_FILL_BLOCKINGS)))

# Training rows are capped, as `predict_soil_temp()` caps them at 60,000, so
# that a 20-year half-hourly record does not make the random forest the
# bottleneck. The cap is applied inside each fold, after the held-out block is
# removed, so it cannot leak.
subsample <- function(train, n, seed) {
  if (nrow(train) <= n) return(train)
  set.seed(seed)
  train[sort(sample(nrow(train), n)), , drop = FALSE]
}

site_cv <- function(name_site) {
  message("==== ", name_site, " ====")
  sd_ <- step01_cached(name_site)
  if (inherits(sd_, "baseline_error")) {
    return(tibble::tibble(site_ID = name_site, status = "step01 failed"))
  }
  fg <- sd_$feature_gs
  gStart <- fg$gStart; gEnd <- fg$gEnd

  dat <- sd_$ac
  dat$TS <- dat$TS_measured
  dat <- ts_fill_add_features(dat)
  have_netrad <- "NETRAD" %in% names(dat) && any(!is.na(dat$NETRAD))
  methods <- ts_fill_methods(have_netrad)

  in_gs <- dplyr::between(dat$DOY, gStart, gEnd)
  gs <- dat[in_gs, , drop = FALSE]

  score <- function(pred_full, method, blocking) {
    m <- tryCatch(
      ts_reconstruction_metrics(gs, gs$TS_measured, pred_full[in_gs], gStart, gEnd),
      error = function(e) NULL
    )
    if (is.null(m)) return(NULL)
    tibble::tibble(site_ID = name_site, status = "ok", method = method,
                   blocking = blocking, have_netrad = have_netrad) |>
      dplyr::bind_cols(m)
  }

  out <- list()

  # The incumbent, exactly as the pipeline builds it: fitted on the whole
  # record and predicted everywhere. It is *in-sample* by construction, which
  # is the honest label for it -- and is itself part of the finding, because
  # the pipeline has no out-of-sample estimate of it at all.
  if ("TS_linear" %in% names(dat)) {
    out[[length(out) + 1]] <- score(dat$TS_linear, "pipeline_TS_linear", "in_sample")
  }

  for (bl in blockings) {
    blocks <- TS_FILL_BLOCKINGS[[bl]](dat)
    # A block that removes the entire record leaves nothing to train on.
    blocks <- Filter(function(i) length(i) < nrow(dat), blocks)
    if (!length(blocks)) next
    for (mn in names(methods)) {
      meth <- methods[[mn]]
      pred <- rep(NA_real_, nrow(dat))
      for (bi in seq_along(blocks)) {
        idx <- blocks[[bi]]
        train <- subsample(dat[-idx, , drop = FALSE], max_train, seed = 222 + bi)
        if (sum(!is.na(train$TS)) < 50) next
        mod <- tryCatch(meth$fit(train), error = function(e) NULL)
        if (is.null(mod)) next
        p <- tryCatch(meth$predict(mod, dat[idx, , drop = FALSE]), error = function(e) NULL)
        if (!is.null(p) && length(p) == length(idx)) pred[idx] <- p
      }
      out[[length(out) + 1]] <- score(pred, mn, bl)
    }
  }

  res <- dplyr::bind_rows(out)
  if (!nrow(res)) return(tibble::tibble(site_ID = name_site, status = "no results"))
  res
}

safe <- function(s) tryCatch(site_cv(s), error = function(e) {
  tibble::tibble(site_ID = s, status = paste("error:", conditionMessage(e)))
})

cat("Sites:", length(sites), " workers:", workers,
    " blockings:", paste(blockings, collapse = ","), "\n")
t0 <- Sys.time()
res <- if (workers > 1) parallel::mclapply(sites, safe, mc.cores = workers) else lapply(sites, safe)
out <- dplyr::bind_rows(res)
cat("elapsed:", format(Sys.time() - t0), "\n")

dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(out, outfile)
cat("Wrote", outfile, "-", nrow(out), "rows\n")

ok <- dplyr::filter(out, .data$status == "ok")
if (nrow(ok)) {
  cat("\n  median across sites, by method x blocking\n\n")
  smry <- ok |>
    dplyr::summarise(
      nsite = dplyr::n(),
      rmse = stats::median(.data$rmse, na.rm = TRUE),
      bias = stats::median(.data$bias, na.rm = TRUE),
      sd_ratio = stats::median(.data$sd_ratio, na.rm = TRUE),
      within = stats::median(.data$within_sd_ratio, na.rm = TRUE),
      across = stats::median(.data$across_year_spread_ratio, na.rm = TRUE),
      amp_infl = stats::median(.data$amp_inflation, na.rm = TRUE),
      .by = c("blocking", "method")
    ) |>
    dplyr::arrange(.data$blocking, .data$rmse)
  print(as.data.frame(smry), row.names = FALSE, digits = 3)
}
