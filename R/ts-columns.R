# Candidate soil-temperature columns for each site (refactor-v2 stage S15).
#
# The manuscript used one soil temperature per site: the sensor, repaired or
# reconstructed in step 01 at some sites, and replaced in step 02 by a
# regression on air temperature at 35. Here every site gets every candidate,
# each qualified the way step 01 qualifies its own (R/ts-qualify.R), so that
# a fit can use any of them:
#
#   TS_original       the site's soil-temperature sensor (data/site_settings.csv
#                     ts_sensor), as recorded; usable where step 01 would use
#                     it (FLUXNET-format quality flag 0-2, or present); all
#                     NA where the site has no sensor
#   TS_reconstructed  step 01's soil temperature (the sensor, repaired or
#                     reconstructed per site)
#   TS_manuscript     what the fits used: TS_reconstructed, replaced at the
#                     regression sites by step 02's regression on air
#                     temperature (on the same nighttime records)
#   TS_linear         lm(TS ~ TA) on TA > 0 (the regression of step 02, fitted
#                     to the sensor)
#   TS_<estimator>    the other estimators (ts_estimators()), trained on
#                     the sensor and predicted over the whole record
#
# Estimators are trained only on real sensor data (ts_truth()): the column
# of real readings (site_settings ts_sensor_truth: the sensor, or the
# non-filled counterpart of a PI gap-filled one), measured rather than
# gap-filled where the source flags it (quality flag 0), and from
# ts_sensor_first_year on. A site without such data, or with too few
# records of it, has all-NA estimator columns. Their predictors (air
# temperature, net radiation) are gap-filled first with the day-of-year x
# time-of-day mean, as 01_01 did.
#
# Air temperature, NEE, u* filtering and the NEE-uptake days are the same for
# every column (step 01's); what depends on the column is where it is usable,
# the growing-season start and temperature range, the years kept and the
# control year.

#' Minimum number of training records
#'
#' Minimum number of sensor records to train an estimator on.
#' @noRd
TS_MIN_TRAIN <- 1000

#' Read a site's soil-temperature sensor
#'
#' The site's sensor, its real readings and net radiation, by timestamp, from the raw record
#' (-9999 read as missing). A column the settings leave empty is all NA.
#'
#' @param site The site's row of data/site_info.csv.
#' @param settings The site's row of data/site_settings.csv (ts_sensor, ts_sensor_truth,
#'   netrad).
#' @param base_dir Root of the raw site data.
#' @return A data frame, one row per record: TIMESTAMP, TS_sensor, TS_sensor_QC (the sensor's
#'   quality flag; NA at AmeriFlux BASE sites), TS_real, NETRAD.
#' @noRd
read_ts_sensor <- function(site, settings, base_dir = path_original("SiteData")) {
  if (site$source == "AmeriFlux_BASE") {
    a <- read_ameriflux_base(site$site_ID, base_dir)
    a[a == -9999] <- NA
    timestamp <- a$TIMESTAMP
    qc <- rep(NA_real_, nrow(a))
  } else {
    a <- read_fluxnet_record(site$site_ID, site$source, base_dir)
    a[a == -9999] <- NA
    dt <- lubridate::ymd_hm(a$TIMESTAMP_START[2]) - lubridate::ymd_hm(a$TIMESTAMP_START[1])
    timestamp <- lubridate::ymd_hm(a$TIMESTAMP_START) + dt / 2
    qc <- if (settings$ts_sensor != "") a[[paste0(settings$ts_sensor, "_QC")]] else rep(NA_real_, nrow(a))
  }
  data.frame(
    TIMESTAMP = timestamp,
    TS_sensor = if (settings$ts_sensor != "") a[[settings$ts_sensor]] else NA_real_,
    TS_sensor_QC = qc,
    TS_real = if (settings$ts_sensor_truth != "") a[[settings$ts_sensor_truth]] else NA_real_,
    NETRAD = if (settings$netrad != "") a[[settings$netrad]] else NA_real_
  )
}

#' Records of real sensor data
#'
#' Which records of the sensor are real measurements to train on: a real reading, measured
#' rather than gap-filled where the source flags it (quality flag 0), from `first_year` on.
#'
#' @param sensor read_ts_sensor()'s table, matched to the record.
#' @param year The year of each record.
#' @param first_year First year of real sensor data (site_settings ts_sensor_first_year), or NA.
#' @return A logical vector, one value per record.
#' @noRd
ts_truth <- function(sensor, year, first_year) {
  ok <- !is.na(sensor$TS_real) & (is.na(sensor$TS_sensor_QC) | sensor$TS_sensor_QC == 0)
  if (!is.na(first_year)) ok <- ok & year >= first_year
  ok
}

#' Gap-fill by day of year and time of day
#'
#' Fill gaps in x with its mean at the same day of year and time of day.
#'
#' @param x Numeric vector.
#' @param DOY Day of year of each value.
#' @param HOUR Hour of each value.
#' @param MINUTE Minute of each value.
#' @return `x` with gaps filled; still NA where the day of year and time of day have no value,
#'   and unchanged if all NA.
#' @noRd
ts_gapfill_doy <- function(x, DOY, HOUR, MINUTE) {
  if (all(is.na(x))) return(x)
  key <- paste(DOY, HOUR, MINUTE)
  means <- tapply(x, key, mean, na.rm = TRUE)
  filled <- unname(means[key])
  x[is.na(x)] <- filled[is.na(x)]
  x[is.nan(x)] <- NA
  x
}

#' Air-temperature memory predictors
#'
#' Air-temperature "memory" of the soil and the seasonal and diurnal cycles: daily mean air
#' temperature, the previous day's mean and range, means over the last 3, 7 and 30 days (over a
#' complete calendar, the days with data), and harmonics of the calendar day and the hour.
#'
#' @param dat A data frame with YEAR, MONTH, DAY, HOUR, MINUTE and TA, one row per record.
#' @return A data frame with the columns TS_MEMORY_PREDICTORS, one row per row of `dat`.
#' @noRd
ts_memory_features <- function(dat) {
  date <- lubridate::make_date(dat$YEAR, dat$MONTH, dat$DAY)
  range_or_na <- function(x) if (sum(!is.na(x)) >= 4) diff(range(x, na.rm = TRUE)) else NA_real_
  mean_or_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  daily <- data.frame(date = date, TA = dat$TA) |>
    dplyr::group_by(.data$date) |>
    dplyr::summarise(TA_day = mean_or_na(.data$TA), TA_amp = range_or_na(.data$TA))
  daily <- data.frame(date = seq(min(date), max(date), by = "day")) |>
    dplyr::left_join(daily, by = "date") |>
    dplyr::arrange(.data$date)
  roll <- function(x, k) zoo::rollapply(x, width = k, FUN = mean_or_na, align = "right", partial = TRUE)
  daily$TA_m3 <- roll(daily$TA_day, 3)
  daily$TA_m7 <- roll(daily$TA_day, 7)
  daily$TA_m30 <- roll(daily$TA_day, 30)
  daily$TA_lag1 <- dplyr::lag(daily$TA_day)
  daily$TA_amp_lag1 <- dplyr::lag(daily$TA_amp)
  i <- match(date, daily$date)
  day_angle <- 2 * pi * lubridate::yday(date) / 365.25
  hour_angle <- 2 * pi * (dat$HOUR + dat$MINUTE / 60) / 24
  data.frame(
    TA = dat$TA, TA_day = daily$TA_day[i], TA_lag1 = daily$TA_lag1[i], TA_amp_lag1 = daily$TA_amp_lag1[i],
    TA_m3 = daily$TA_m3[i], TA_m7 = daily$TA_m7[i], TA_m30 = daily$TA_m30[i],
    doy_s1 = sin(day_angle), doy_c1 = cos(day_angle), doy_s2 = sin(2 * day_angle), doy_c2 = cos(2 * day_angle),
    hr_s1 = sin(hour_angle), hr_c1 = cos(hour_angle)
  )
}

#' Predictors of the memory estimators
#'
#' The columns of ts_memory_features().
#' @noRd
TS_MEMORY_PREDICTORS <- c("TA", "TA_day", "TA_lag1", "TA_amp_lag1", "TA_m3", "TA_m7", "TA_m30",
                          "doy_s1", "doy_c1", "doy_s2", "doy_c2", "hr_s1", "hr_c1")

#' The soil-temperature estimators
#'
#' The estimators: model kind ("lm" or "rf", a ranger random forest), predictors, and which
#' training records they use (NULL: all).
#'
#' @return A named list (by estimator) of list(kind, predictors, subset), `subset` a function of
#'   the data returning the usable training rows, or NULL.
#' @noRd
ts_estimators <- function() {
  above_freezing <- function(d) !is.na(d$TA) & d$TA > 0
  list(
    linear = list(kind = "lm", predictors = "TA", subset = above_freezing),
    lm_ta = list(kind = "lm", predictors = "TA", subset = NULL),
    lm_ta_recent = list(kind = "lm", predictors = "TA", subset = function(d) above_freezing(d) & d$YEAR > 2021),
    lm_ta_netrad = list(kind = "lm", predictors = c("TA", "NETRAD"), subset = NULL),
    rf_ta = list(kind = "rf", predictors = "TA", subset = NULL),
    rf_ta_netrad = list(kind = "rf", predictors = c("TA", "NETRAD"), subset = NULL),
    lm_memory = list(kind = "lm", predictors = TS_MEMORY_PREDICTORS, subset = NULL),
    rf_memory = list(kind = "rf", predictors = TS_MEMORY_PREDICTORS, subset = NULL),
    rf_memory_netrad = list(kind = "rf", predictors = c(TS_MEMORY_PREDICTORS, "NETRAD"), subset = NULL)
  )
}

#' Train an estimator and predict the record
#'
#' Train an estimator on the records in `train` (rows of `dat` with sensor truth) and predict
#' every row of `dat`; all NA if there are too few (fewer than TS_MIN_TRAIN) complete training
#' records. Forests: 200 trees on at most est$max_train (default 60,000) records.
#'
#' @param est One estimator of ts_estimators() (optionally with max_train).
#' @param dat A data frame, one row per record, with TS (the sensor's real readings) and the
#'   predictors.
#' @param train Whether each row of `dat` is sensor truth.
#' @param seed Random seed of the forest.
#' @return The predictions, one per row of `dat`; NA where a predictor is missing.
#' @noRd
ts_fit_predict <- function(est, dat, train, seed) {
  rows <- train & stats::complete.cases(dat[, est$predictors, drop = FALSE])
  if (!is.null(est$subset)) rows <- rows & est$subset(dat)
  out <- rep(NA_real_, nrow(dat))
  if (sum(rows) < TS_MIN_TRAIN) return(out)
  d <- dat[rows, c("TS", est$predictors), drop = FALSE]
  ok <- stats::complete.cases(dat[, est$predictors, drop = FALSE])
  if (est$kind == "lm") {
    mod <- stats::lm(stats::reformulate(est$predictors, "TS"), data = d)
    out[ok] <- stats::predict(mod, newdata = dat[ok, est$predictors, drop = FALSE])
  } else {
    set.seed(seed)
    max_train <- if (is.null(est$max_train)) 60000 else est$max_train
    if (nrow(d) > max_train) d <- d[sort(sample.int(nrow(d), max_train)), , drop = FALSE]
    mod <- ranger::ranger(x = d[, est$predictors, drop = FALSE], y = d$TS, num.trees = 200, num.threads = 1, seed = seed)
    out[ok] <- stats::predict(mod, data = dat[ok, est$predictors, drop = FALSE], num.threads = 1)$predictions
  }
  out
}

#' Soil temperature of the manuscript's fits
#'
#' The soil temperature the manuscript's fits used, over the whole record: at the regression
#' sites step 02's regression on air temperature (recovered exactly from the regressed
#' nighttime records of `manuscript_inputs`, the site's tas_inputs) where air temperature is
#' present, step 01's elsewhere.
#'
#' @param step01 The site's step-01 list.
#' @param settings The site's row of data/site_settings.csv (ts_regression).
#' @param manuscript_inputs The site's tas_inputs (prepare_tas_inputs()).
#' @return Soil temperature, one value per row of step01$ts_base.
#' @noRd
ts_manuscript_values <- function(step01, settings, manuscript_inputs) {
  ts <- step01$climatology$TS
  if (settings$ts_regression == "none") return(ts)
  # (step 02 replaced TS wherever TA is present)
  night <- manuscript_inputs$night[!is.na(manuscript_inputs$night$TA), ]
  coef <- stats::coef(stats::lm(TS ~ TA, data = night))
  pred <- coef[[1]] + coef[[2]] * step01$ts_base$TA
  ts[!is.na(pred)] <- pred[!is.na(pred)]
  ts
}

#' Every candidate column of a site
#'
#' Every candidate column of a site: list(values, ok, truth): one column per candidate and one
#' row per record of step01$ts_base (the candidate's value and whether step 01 would use it),
#' and which records hold real sensor data. `sensor` is read_ts_sensor()'s table, `settings`
#' the site's row of site_settings, `manuscript_inputs` the site's tas_inputs.
#'
#' @param step01 The site's step-01 list.
#' @param sensor read_ts_sensor()'s table.
#' @param settings The site's row of data/site_settings.csv.
#' @param manuscript_inputs The site's tas_inputs (prepare_tas_inputs()).
#' @return list(values, ok, truth, real): data frames of the candidates' values and usability,
#'   whether each record is sensor truth (ts_truth()), and the sensor's real readings.
#' @noRd
ts_candidates <- function(step01, sensor, settings, manuscript_inputs) {
  base <- step01$ts_base
  params <- step01$ts_params
  s <- sensor[match(base$TIMESTAMP, sensor$TIMESTAMP), ]
  truth <- ts_truth(s, base$YEAR, settings$ts_sensor_first_year)
  fluxnet <- params$source == "fluxnet"

  values <- data.frame(TS_original = s$TS_sensor, TS_reconstructed = step01$climatology$TS,
                       TS_manuscript = ts_manuscript_values(step01, settings, manuscript_inputs))
  ok <- data.frame(TS_original = if (fluxnet) s$TS_sensor_QC %in% c(0, 1, 2) & !is.na(s$TS_sensor) else !is.na(s$TS_sensor),
                   TS_reconstructed = step01$ts_ok, TS_manuscript = step01$ts_ok)

  # predictors, gap-filled, and the sensor to train on
  ta <- ts_gapfill_doy(base$TA, base$DOY, base$HOUR, base$MINUTE)
  dat <- ts_memory_features(data.frame(YEAR = base$YEAR, MONTH = base$MONTH, DAY = base$DAY, HOUR = base$HOUR, MINUTE = base$MINUTE, TA = ta))
  dat$NETRAD <- ts_gapfill_doy(s$NETRAD, base$DOY, base$HOUR, base$MINUTE)
  dat$YEAR <- base$YEAR
  dat$TS <- s$TS_real
  estimators <- ts_estimators()
  for (name in names(estimators)) {
    column <- paste0("TS_", name)
    values[[column]] <- ts_fit_predict(estimators[[name]], dat, truth, seed = tas_seed(params$site_ID, "ts", name))
    ok[[column]] <- !is.na(values[[column]])
  }
  list(values = values, ok = ok, truth = truth, real = s$TS_real)
}

#' TAS inputs for one soil-temperature column
#'
#' The TAS inputs (as prepare_tas_inputs() returns them) with soil temperature `column`: for
#' TS_manuscript the site's tas_inputs; otherwise step 01's tables for that column
#' (ts_column_tables()), prepared without step 02's regression. NULL if no nighttime record or
#' no year qualifies.
#'
#' @param column Candidate column name, e.g. "TS_linear".
#' @param site The site's row of data/site_info.csv.
#' @param settings The site's row of data/site_settings.csv.
#' @param step01 The site's step-01 list.
#' @param candidates ts_candidates()'s list.
#' @param swc_ERA5 ERA5-Land daily soil water (read_era5_swc()).
#' @param manuscript_inputs The site's tas_inputs.
#' @return prepare_tas_inputs()'s list, or NULL.
#' @noRd
ts_column_inputs <- function(column, site, settings, step01, candidates, swc_ERA5, manuscript_inputs) {
  if (column == "TS_manuscript") return(manuscript_inputs)
  if (!any(step01$ts_base$night_ok & candidates$ok[[column]])) return(NULL)
  tables <- ts_column_tables(step01$ts_base, step01$ts_params, candidates$values[[column]], candidates$ok[[column]])
  if (nrow(tables$night) == 0) return(NULL)
  settings$ts_regression <- "none"
  prepare_tas_inputs(site, settings, tables$feature, tables$night, tables$ac, swc_ERA5, tables$climatology)
}

#' A site's candidates as one table
#'
#' A site's candidates as one table: the record's time stamp and air temperature, then for every
#' candidate its value and <column>_keep, whether the record is in that column's nighttime fit
#' input (`inputs`: ts_column_inputs() by column), and the sensor's real readings (TS_real,
#' where TS_truth).
#'
#' @param step01 The site's step-01 list.
#' @param candidates ts_candidates()'s list.
#' @param inputs ts_column_inputs() results, named by column.
#' @return A data frame, one row per record of step01$ts_base.
#' @noRd
ts_columns_table <- function(step01, candidates, inputs) {
  base <- step01$ts_base
  key <- function(d) paste(d$YEAR, d$MONTH, d$DAY, d$HOUR, d$MINUTE)
  out <- base[, c("TIMESTAMP", "YEAR", "MONTH", "DAY", "DOY", "HOUR", "MINUTE", "TA")]
  for (column in names(candidates$values)) {
    out[[column]] <- candidates$values[[column]]
    out[[paste0(column, "_keep")]] <- if (is.null(inputs[[column]])) FALSE else key(base) %in% key(inputs[[column]]$night)
  }
  out$TS_truth <- candidates$truth
  out$TS_real <- ifelse(candidates$truth, candidates$real, NA_real_)
  out
}

#' Summary of a site's candidate columns
#'
#' One row per candidate column of a site: its fit settings (the settings of its TAS inputs),
#' the number of nighttime fit records and years, and its coverage of the record and of the
#' sensor's real data.
#'
#' @param candidates ts_candidates()'s list.
#' @param inputs ts_column_inputs() results, named by column.
#' @return A data frame: the settings' columns, column, coverage, truth_coverage, n_night and
#'   n_years (only column and the coverages where a column has no inputs).
#' @noRd
ts_columns_summary <- function(candidates, inputs) {
  dplyr::bind_rows(lapply(names(candidates$values), function(column) {
    x <- inputs[[column]]
    v <- candidates$values[[column]]
    row <- data.frame(column = column,
                      coverage = mean(!is.na(v) & candidates$ok[[column]]),
                      truth_coverage = if (any(candidates$truth)) mean(!is.na(v[candidates$truth])) else NA_real_)
    if (is.null(x)) return(row)
    cbind(x$settings, row, n_night = nrow(x$night), n_years = length(unique(x$night$growing_year)))
  }))
}
