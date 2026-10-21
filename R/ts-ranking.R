# Quality of the candidate soil-temperature columns (refactor-v2 stage S16),
# and the estimators carried forward to the fits.
#
# For each site and candidate column (R/ts-columns.R):
#
#   coverage           share of the record where the column is usable, and
#                      the column's nighttime fit records and years
#   agreement with the sensor, on its real readings (TS_real where TS_truth):
#     estimators       out of fold: each year predicted by the estimator
#                      trained on the other years (ts_cv_metrics())
#     TS_reconstructed, TS_manuscript   as they are (not out of fold)
#     rmse, bias (column − sensor), r2, sd_ratio (sd column / sd sensor),
#     cell_sd          mean within day-of-year x hour cell sd of the error,
#     year_sd          sd of the yearly mean error (stability across years)
#   plausibility against air temperature, over the growing season
#   (gStart..gEnd of step 01):
#     amp_ratio        mean daily range of the column / of air temperature
#     lag_h            hour of the column's mean daily maximum minus air
#                      temperature's (circular, -12..12)
#     offset           mean column − air temperature
#     stuck            share of values in runs of >= 6 equal values (3 h at
#                      half-hourly records)
#     spikes           share of steps > 5 deg C between consecutive records
#   and their distance from the sensor's own (d_amp, d_lag, d_offset), where
#   the site has a sensor.
#
# select_ts_methods() keeps the estimators that, at some site and under
# some metric, are best or within a margin of the third best (TS_MARGIN), and
# the four reference columns.

TS_REFERENCE_COLUMNS <- c("TS_original", "TS_reconstructed", "TS_manuscript", "TS_linear")
# a value counts as "close to the top 3" if within 10% of the third best
# (lower-is-better metrics) or 0.01 of it (r2)
TS_MARGIN <- list(relative = 0.10, r2 = 0.01)

ts_error_metrics <- function(pred, obs, doy, hour, year) {
  ok <- !is.na(pred) & !is.na(obs)
  if (sum(ok) < 2) {
    return(data.frame(n = sum(ok), rmse = NA_real_, bias = NA_real_, r2 = NA_real_, sd_ratio = NA_real_, cell_sd = NA_real_, year_sd = NA_real_))
  }
  e <- pred[ok] - obs[ok]
  cell <- paste(doy[ok], hour[ok])
  cell_sd <- tapply(e, cell, stats::sd)
  year_bias <- tapply(e, year[ok], mean)
  data.frame(n = sum(ok), rmse = sqrt(mean(e^2)), bias = mean(e), r2 = stats::cor(pred[ok], obs[ok])^2,
             sd_ratio = stats::sd(pred[ok]) / stats::sd(obs[ok]),
             cell_sd = mean(cell_sd, na.rm = TRUE), year_sd = if (length(year_bias) > 1) stats::sd(year_bias) else NA_real_)
}

# Leave-one-year-out predictions of every estimator at the sensor's real
# data, and their error metrics (one row per estimator). Forests are trained
# on at most 20,000 records here (60,000 for the columns themselves).
ts_cv_metrics <- function(step01, sensor, settings) {
  base <- step01$ts_base
  s <- sensor[match(base$TIMESTAMP, sensor$TIMESTAMP), ]
  truth <- ts_truth(s, base$YEAR, settings$ts_sensor_first_year)
  estimators <- ts_estimators()
  empty <- function(name) cbind(data.frame(column = paste0("TS_", name), n_years_cv = 0L), ts_error_metrics(numeric(), numeric(), NULL, NULL, NULL))
  years <- sort(unique(base$YEAR[truth]))
  if (length(years) < 2) return(do.call(rbind, lapply(names(estimators), empty)))

  ta <- ts_gapfill_doy(base$TA, base$DOY, base$HOUR, base$MINUTE)
  dat <- ts_memory_features(data.frame(YEAR = base$YEAR, MONTH = base$MONTH, DAY = base$DAY, HOUR = base$HOUR, MINUTE = base$MINUTE, TA = ta))
  dat$NETRAD <- ts_gapfill_doy(s$NETRAD, base$DOY, base$HOUR, base$MINUTE)
  dat$YEAR <- base$YEAR
  dat$TS <- s$TS_real
  rows <- which(truth)
  do.call(rbind, lapply(names(estimators), function(name) {
    est <- estimators[[name]]
    est$max_train <- 20000
    oof <- rep(NA_real_, length(rows))
    for (y in years) {
      held <- base$YEAR[rows] == y
      pred <- ts_fit_predict(est, dat, truth & base$YEAR != y, seed = tas_seed(step01$ts_params$site_ID, "ts-cv", name, y))
      oof[held] <- pred[rows[held]]
    }
    cbind(data.frame(column = paste0("TS_", name), n_years_cv = length(years)),
          ts_error_metrics(oof, s$TS_real[rows], base$DOY[rows], base$HOUR[rows], base$YEAR[rows]))
  }))
}

# Agreement of the reference columns with the sensor's real readings (as
# they are).
ts_reference_metrics <- function(table) {
  rows <- which(table$TS_truth)
  do.call(rbind, lapply(c("TS_reconstructed", "TS_manuscript"), function(column) {
    cbind(data.frame(column = column, n_years_cv = NA_integer_),
          ts_error_metrics(table[[column]][rows], table$TS_real[rows], table$DOY[rows], table$HOUR[rows], table$YEAR[rows]))
  }))
}

# Plausibility of each column against air temperature over the growing
# season (rows of `table`, the site's ts_columns table, with TA).
ts_plausibility <- function(table, gStart, gEnd) {
  gs <- dplyr::between(table$DOY, gStart, gEnd)
  date <- lubridate::make_date(table$YEAR, table$MONTH, table$DAY)
  hour <- table$HOUR + table$MINUTE / 60
  daily_range <- function(x) {
    r <- tapply(x[gs], date[gs], function(v) if (sum(!is.na(v)) >= 12) diff(range(v, na.rm = TRUE)) else NA_real_)
    mean(r, na.rm = TRUE)
  }
  peak_hour <- function(x) {
    h <- tapply(x[gs], hour[gs], mean, na.rm = TRUE)
    if (all(is.na(h))) NA_real_ else as.numeric(names(h)[which.max(h)])
  }
  wrap <- function(d) ((d + 12) %% 24) - 12
  ta_range <- daily_range(table$TA)
  ta_peak <- peak_hour(table$TA)
  columns <- setdiff(grep("^TS_", names(table), value = TRUE), c("TS_truth", "TS_real", grep("_keep$", names(table), value = TRUE)))
  out <- do.call(rbind, lapply(columns, function(column) {
    x <- table[[column]]
    if (all(is.na(x[gs]))) {
      return(data.frame(column = column, amp_ratio = NA_real_, lag_h = NA_real_, offset = NA_real_, stuck = NA_real_, spikes = NA_real_))
    }
    runs <- rle(x)
    in_stuck <- rep(runs$lengths >= 6 & !is.na(runs$values), runs$lengths)
    steps <- abs(diff(x))
    data.frame(column = column, amp_ratio = daily_range(x) / ta_range, lag_h = wrap(peak_hour(x) - ta_peak),
               offset = mean(x[gs] - table$TA[gs], na.rm = TRUE),
               stuck = mean(in_stuck[gs & !is.na(x)]), spikes = mean(steps[gs[-1]] > 5, na.rm = TRUE))
  }))
  sensor <- out[out$column == "TS_original", ]
  out$d_amp <- abs(log(out$amp_ratio / sensor$amp_ratio))
  out$d_lag <- abs(wrap(out$lag_h - sensor$lag_h))
  out$d_offset <- abs(out$offset - sensor$offset)
  out
}

# All quality metrics of a site's columns: one row per column.
ts_quality <- function(site_id, table, summary, cv, feature) {
  agreement <- rbind(cv, ts_reference_metrics(table))
  plaus <- ts_plausibility(table, feature$gStart, feature$gEnd)
  out <- summary[, intersect(c("column", "coverage", "truth_coverage", "n_night", "n_years"), names(summary))] |>
    dplyr::left_join(agreement, by = "column") |>
    dplyr::left_join(plaus, by = "column")
  cbind(site_ID = site_id, out)
}

# Metrics that rank the estimators (lower is better, except r2), as
# functions of the quality table.
TS_RANK_METRICS <- list(
  rmse = function(q) q$rmse, abs_bias = function(q) abs(q$bias), r2 = function(q) q$r2,
  sd_ratio = function(q) abs(q$sd_ratio - 1), cell_sd = function(q) q$cell_sd, year_sd = function(q) q$year_sd,
  d_amp = function(q) q$d_amp, d_lag = function(q) q$d_lag, d_offset = function(q) q$d_offset
)

# Rank the estimators at every site under every metric: one row per site,
# metric and estimator, with its value, rank, and whether it is best or
# within the margin of the third best ("top").
rank_ts_columns <- function(quality) {
  estimators <- paste0("TS_", names(ts_estimators()))
  q <- quality[quality$column %in% estimators, ]
  do.call(rbind, lapply(split(q, q$site_ID), function(qs) {
    do.call(rbind, lapply(names(TS_RANK_METRICS), function(metric) {
      v <- TS_RANK_METRICS[[metric]](qs)
      ok <- !is.na(v)
      if (!any(ok)) return(NULL)
      higher_better <- metric == "r2"
      score <- if (higher_better) -v else v
      rank <- rep(NA_integer_, length(v))
      rank[ok] <- rank(score[ok], ties.method = "min")
      third <- sort(score[ok])[min(3, sum(ok))]
      close <- if (higher_better) score <= third + TS_MARGIN$r2 else score <= third + TS_MARGIN$relative * abs(third)
      data.frame(site_ID = qs$site_ID, metric = metric, column = qs$column, value = v, rank = rank,
                 top = ok & (rank == 1 | close))
    }))
  }))
}

# The columns the fits are run with: the reference columns and every
# estimator that is "top" somewhere.
select_ts_methods <- function(ranking) {
  winners <- unique(ranking$column[ranking$top])
  estimators <- paste0("TS_", names(ts_estimators()))
  c(TS_REFERENCE_COLUMNS, setdiff(estimators[estimators %in% winners], TS_REFERENCE_COLUMNS))
}
