# Soil temperature, both stages (docs/soil-temperature.md):
#
#   Stage A, `qualification_soil_temperature()` (step 01): the column the QC
#     filters, season detector and year gap scan run on -- a sensor after its
#     per-site repairs, or a reconstruction. Leaves step 01 as `TS_measured`,
#     with a provenance row.
#   Stage B, `get_soil_temperature()` (step 02): selects one candidate under
#     the recipe, attaches the fill if asked, looks up its bounds, and drops
#     every other candidate. What leaves is `TS_final` and a `meta` row;
#     downstream there is one soil-temperature column and no branching.

#' Stage B: select the soil-temperature column under a recipe
#'
#' @param site_data The site's step-01 result, from `prep_nee_ac()`.
#' @param site_info One row of the site declaration table.
#' @param recipe A recipe: one strategy per `RECIPE_AXES` axis; `NULL` for
#'   `original_recipe()`.
#' @param fill The site's `fill_soil_temp()` result, or `NULL`.
#' @param ts_col Soil-temperature column to use whatever the recipe says, for
#'   sensitivity runs; `NULL` to let the strategy choose.
#' @return A list: `ac` and `nightNEE`, with `TS_final` in place of every
#'   soil-temperature column, and `meta`, a one-row tibble of the choice, its
#'   reason, provenance and bounds (`tStart`, `tEnd`).
get_soil_temperature <- function(site_data, site_info, recipe = NULL, fill = NULL,
                                 ts_col = NULL) {
  recipe <- recipe %||% original_recipe()
  name_site <- site_info[["site_ID"]]
  ac <- site_data[["ac"]]
  night <- site_data[["nightNEE"]]

  ts_bounds_all <- site_data[["ts_bounds"]]

  # `ts_col` overrides the strategy, for sensitivity runs.
  override <- ts_col
  choice <- if (is.null(override)) {
    choose_ts_col(recipe, site_data, site_info, fill)
  } else {
    list(ts_col = override, reason = "ts_col argument")
  }
  ts_col <- choice[["ts_col"]]

  # No second method where there is no measured truth to fit it against: at
  # the 27 sites whose `TS_measured` has non-sensor rows, every variant
  # alternative would be a model fitted to a reconstruction. A variant keeps
  # step 01's column there and says why. Exempt: the manuscript's `site_info`
  # strategy (a declaration) and an explicit `ts_col` (a sensitivity run).
  refused <- FALSE
  if (is.null(override) && !identical(recipe$ts, "site_info") &&
      identical(stage_a_truth(site_data), "none") &&
      !identical(ts_col, "TS_measured")) {
    refused <- TRUE
    choice <- list(
      ts_col = "TS_measured",
      reason = sprintf(
        paste0("kept step 01's column: ts_source = %s leaves no measured soil ",
               "temperature to fit a second method against (%s strategy wanted %s)"),
        stage_a_arm(site_data), recipe$ts, ts_col
      )
    )
    ts_col <- "TS_measured"
  }

  # `TS_memfill` comes from its own per-site target (`fill_soil_temp()`),
  # aligned with `site_data` row for row (asserted), and carries its own bounds
  # rows.
  fill_method <- NA_character_
  fill_cv_rmse <- NA_real_
  fill_degenerate <- NA
  fill_truth_synthetic <- NA
  if (identical(ts_col, "TS_memfill")) {
    if (!fill_available(fill)) {
      stop(name_site, ": recipe selected TS_memfill but no usable fill was supplied (",
           fill_status(fill), ").")
    }
    stopifnot(
      length(fill$ac_ts) == nrow(ac),
      length(fill$night_ts) == nrow(night)
    )
    ac[["TS_memfill"]] <- fill$ac_ts
    night[["TS_memfill"]] <- fill$night_ts
    ts_bounds_all <- dplyr::bind_rows(ts_bounds_all, fill$ts_bounds)
    fill_method <- fill$method
    fill_cv_rmse <- fill$cv_rmse
    fill_degenerate <- isTRUE(fill$degenerate)
    fill_truth_synthetic <- isTRUE(fill$truth_synthetic)
  }

  bounds <- choose_bounds(recipe, ts_bounds_all, ts_col)

  ac <- materialise_ts_final(ac, ts_col)
  night <- materialise_ts_final(night, ts_col)

  ts_qc <- site_data[["ts_qc"]]
  prov <- site_data[["ts_provenance"]]
  meta <- tibble::tibble(
    site_ID = name_site,
    ts_col = ts_col,
    ts_strategy = recipe$ts,
    ts_reason = choice[["reason"]],
    ts_verdict = ts_qc$verdict[[1]],
    ts_flags = ts_qc$flags[[1]],
    # Whether step 01's "measured" column is a reconstruction; strategies
    # cannot undo that (docs/ts-variants.html, V4).
    ts_measured_synthetic = identical(stage_a_truth(site_data), "none"),
    ts_source = ts_source(site_info),
    ts_refused = refused,
    # What stage A did to make `TS_measured`.
    stage_a_estimator = prov[["stage_a_estimator"]],
    stage_a_family = prov[["stage_a_family"]],
    stage_a_mode = prov[["stage_a_mode"]],
    fill_method = fill_method,
    fill_cv_rmse = fill_cv_rmse,
    fill_degenerate = fill_degenerate,
    fill_truth_synthetic = fill_truth_synthetic,
    bounds_definition = bounds[["definition"]],
    bounds_reason = bounds[["reason"]],
    tStart = bounds[["tStart"]],
    tEnd = bounds[["tEnd"]]
  )

  list(ac = ac, nightNEE = night, meta = meta)
}

#' Whether stage A left measured soil temperature at this site
#'
#' What stage A actually did at this site, from its provenance row -- under
#' `ts_qc = sensor` that is not what `ts_source` declares.
#'
#' @param site_data The site's step-01 result, from `prep_nee_ac()`.
#' @return `"sensor"` or `"none"`.
stage_a_truth <- function(site_data) site_data[["ts_provenance"]][["ts_truth"]][[1]]
#' The stage-A arm that ran at this site
#'
#' @param site_data The site's step-01 result, from `prep_nee_ac()`.
#' @return A `TS_SOURCES` name.
stage_a_arm <- function(site_data) site_data[["ts_provenance"]][["stage_a_arm"]][[1]]

#' Materialise the chosen column as `TS_final`
#'
#' `TS_final`, and no other soil-temperature column, so that "no branching
#' downstream" is a checked property (test-soil-temperature.R) rather than a
#' convention.
#'
#' @param dat A half-hourly table, `ac` or `nightNEE`.
#' @param ts_col Name of the soil-temperature column to keep.
#' @return `dat` with `TS_final` a copy of `ts_col` and every other `TS` and
#'   `TS_*` column dropped.
materialise_ts_final <- function(dat, ts_col) {
  if (!ts_col %in% names(dat)) {
    stop(
      "Requested TS column ", shQuote(ts_col), " is not present. Available: ",
      paste(ts_candidate_columns(dat), collapse = ", "),
      ". It has to be produced by `prep_nee_ac()` or attached from the fill."
    )
  }
  dat[["TS_final"]] <- dat[[ts_col]]
  dat[setdiff(names(dat), setdiff(ts_candidate_columns(dat), "TS_final"))]
}

#' Every soil-temperature column on a table
#'
#' @param dat A data frame.
#' @return Character vector of column names: `TS` and anything `TS_*`.
ts_candidate_columns <- function(dat) {
  grep("^TS($|_)", names(dat), value = TRUE)
}

# =========================================================== stage A
#
# Both readers hand in the same shape (`*_ts_input()`) and get back `TS`,
# `TS_QC` and a provenance row. The arm is `ts_source` in site_info.csv
# (`TS_SOURCES` in R/constants.R); the only site names tested are the
# manuscript's three training-target rules in the `reconstructed` arm.

#' The columns every reader provides
#'
#' `TIMESTAMP_START` is the 12-digit stamp both products carry; FI-Sod's
#' recalibration windows are expressed in it.
#'
#' Optional, where the record has them: TS_sensor_QC, TA_QC, NETRAD,
#' TS_depth2, TS_depth2_QC, TS_pi. An arm that needs one and lacks it fails
#' by name.
TS_INPUT_REQUIRED <- c("TIMESTAMP", "TIMESTAMP_START", "YEAR", "DOY", "HOUR", "MINUTE", "TS_sensor", "TA")

#' The FLUXNET-family record's columns, under the shared names
#'
#' `TS_F_MDS_1` is the shallow sensor and `TS_F_MDS_2` the second depth;
#' `TA_F_MDS` carries its own QC flag, which the air-temperature substitute
#' inherits.
#'
#' @param a The site's spliced FLUXNET-format half-hourly record, with the
#'   timestamp columns added.
#' @param site_info One row of the site declaration table.
#' @return The stage-A input: a tibble of the `TS_INPUT_REQUIRED` columns plus
#'   `TS_sensor_QC` and `TA_QC`, and `NETRAD`, `TS_depth2` and `TS_depth2_QC`
#'   where the record has them.
fluxnet_ts_input <- function(a, site_info) {
  absent <- rep(NA_real_, nrow(a))
  out <- tibble::tibble(
    TIMESTAMP = a$TIMESTAMP, TIMESTAMP_START = a$TIMESTAMP_START,
    YEAR = a$YEAR, DOY = a$DOY, HOUR = a$HOUR, MINUTE = a$MINUTE,
    TS_sensor = a[["TS_F_MDS_1"]] %||% absent,
    TS_sensor_QC = a[["TS_F_MDS_1_QC"]] %||% absent,
    TA = a$TA_F_MDS,
    TA_QC = a[["TA_F_MDS_QC"]] %||% absent
  )
  if (!is.null(a[["NETRAD"]])) out$NETRAD <- a$NETRAD
  if (!is.null(a[["TS_F_MDS_2"]])) {
    out$TS_depth2 <- a$TS_F_MDS_2
    out$TS_depth2_QC <- a[["TS_F_MDS_2_QC"]] %||% absent
  }
  # Say so here, rather than later as "no observations after the filter".
  if (is.null(a[["TS_F_MDS_1"]]) && ts_source(site_info) != "ta_substitute") {
    stop(
      site_info[["site_ID"]], ": the spliced record has no TS_F_MDS_1 column. Declared ",
      "products: ", paste(site_sources(site_info), collapse = " + "),
      ". Check that all of them are downloaded."
    )
  }
  out
}

#' The AmeriFlux BASE record's columns, under the shared names
#'
#' The declared sensor and air temperature, net radiation (`netrad_column`, else
#' a bare `NETRAD`), and the PI's gap-filled `TS_PI_1` where present. No QC
#' flags.
#'
#' @param a The site's AmeriFlux BASE half-hourly record, with the timestamp
#'   columns added.
#' @param site_info One row of the site declaration table.
#' @return The stage-A input: a tibble of the `TS_INPUT_REQUIRED` columns, plus
#'   `NETRAD` and `TS_pi` where the record has them.
ameriflux_ts_input <- function(a, site_info) {
  n <- nrow(a)
  out <- tibble::tibble(
    TIMESTAMP = a$TIMESTAMP, TIMESTAMP_START = a$TIMESTAMP_START,
    YEAR = a$YEAR, DOY = a$DOY, HOUR = a$HOUR, MINUTE = a$MINUTE,
    TS_sensor = if (!is.na(site_info$TS)) a[[site_info$TS]] else rep(NA_real_, n),
    TA = a[[site_info$TA]]
  )
  netrad_column <- site_info[["netrad_column"]]
  if (is.na(netrad_column) && "NETRAD" %in% names(a)) netrad_column <- "NETRAD"
  if (!is.na(netrad_column)) {
    if (!netrad_column %in% names(a)) {
      stop(
        site_info[["site_ID"]], " declares netrad_column = ", shQuote(netrad_column),
        ", which is not in its AmeriFlux record."
      )
    }
    out$NETRAD <- a[[netrad_column]]
  }
  if ("TS_PI_1" %in% names(a)) out$TS_pi <- a$TS_PI_1
  out
}

#' One row describing what stage A did
#'
#' Carried in site_data as `ts_provenance` and copied into every fit's settings
#' by `get_soil_temperature()`.
#'
#' @param site_info One row of the site declaration table.
#' @param estimator Name of the estimator or swap that ran, or NA.
#' @param family The estimator's family label (`ts_family()`), or NA.
#' @param mode The `write_back_ts()` mode, `"none"` if nothing was written, or
#'   NA.
#' @param n_train Number of complete training rows, or NA.
#' @param note Free-text note, or NA.
#' @return A one-row tibble: `site_ID`, `ts_source`, `ts_truth` and the
#'   `stage_a_*` fields.
ts_provenance_row <- function(site_info, estimator = NA_character_, family = NA_character_,
                              mode = NA_character_, n_train = NA_integer_, note = NA_character_) {
  tibble::tibble(
    site_ID = site_info[["site_ID"]],
    ts_source = ts_source(site_info),
    ts_truth = ts_measured_truth(site_info),
    stage_a_estimator = estimator,
    stage_a_family = family,
    stage_a_mode = mode,
    stage_a_n_train = as.integer(n_train),
    stage_a_note = note
  )
}

#' Fail unless the stage-A input has the columns an arm needs
#'
#' @param input The stage-A input, from `*_ts_input()`.
#' @param cols Character vector of required column names.
#' @param site_info One row of the site declaration table.
#' @param why What needs the columns, for the error message.
#' @return Called for its side effect (an error naming the absent columns);
#'   returns `NULL`, invisibly.
need_input <- function(input, cols, site_info, why) {
  absent <- setdiff(cols, names(input))
  if (length(absent)) {
    stop(
      site_info[["site_ID"]], ": ", why, " needs ", paste(absent, collapse = ", "),
      ", which the record does not provide."
    )
  }
}

#' Stage A: the soil-temperature column step 01 qualifies on
#'
#' @param input The stage-A input, from `fluxnet_ts_input()` or
#'   `ameriflux_ts_input()`.
#' @param site_info One row of the site declaration table.
#' @param ts_qc Soil-temperature qualification strategy, `"manuscript"` or
#'   `"sensor"` (see `RECIPE_AXES`).
#' @return A list of `TS` and `TS_QC`, aligned to `input`'s rows, and
#'   `provenance`, a `ts_provenance_row()` with `ts_qc`, `stage_a_arm` and
#'   `ts_truth` set to what ran.
qualification_soil_temperature <- function(input, site_info, ts_qc = "manuscript") {
  name_site <- site_info[["site_ID"]]
  absent <- setdiff(TS_INPUT_REQUIRED, names(input))
  if (length(absent)) {
    stop(name_site, ": the stage-A input lacks ", paste(absent, collapse = ", "), ".")
  }
  qc <- input[["TS_sensor_QC"]] %||% rep(NA_real_, nrow(input))

  # Which arm runs: the declaration, or under `ts_qc = sensor` the raw sensor
  # wherever the declared arm would leave non-sensor rows.
  ts_qc <- match.arg(ts_qc, RECIPE_AXES[["ts_qc"]])
  declared <- ts_source(site_info)
  arm <- if (identical(ts_qc, "sensor") && !identical(TS_SOURCES[[declared]], "sensor")) "sensor" else declared

  fitted <- function(estimator_name, mode, est = ts_estimators()[[estimator_name]], note = NA_character_) {
    train <- tibble::tibble(TS = input$TS_sensor, TA = input$TA, YEAR = input$YEAR)
    if ("NETRAD" %in% est$predictors) {
      need_input(input, "NETRAD", site_info, estimator_name)
      train$NETRAD <- input$NETRAD
    }
    mod <- est$fit(train)
    list(
      TS = write_back_ts(input$TS_sensor, est$predict(mod, train), mode),
      TS_QC = qc,
      provenance = ts_provenance_row(
        site_info, estimator = estimator_name, family = est$family, mode = mode,
        n_train = sum(stats::complete.cases(train[, c("TS", est$predictors), drop = FALSE])),
        note = note
      )
    )
  }

  out <- switch(
    arm,
    sensor = list(
      TS = input$TS_sensor, TS_QC = qc,
      provenance = ts_provenance_row(site_info, mode = "none")
    ),
    # use TS of second layer because the first layer is incomplete
    sensor_depth2 = {
      need_input(input, "TS_depth2", site_info, "the second-depth swap")
      list(
        TS = write_back_ts(input$TS_sensor, input$TS_depth2, "replace"),
        TS_QC = input$TS_depth2_QC %||% qc,
        provenance = ts_provenance_row(site_info, estimator = "swap_depth2", mode = "replace")
      )
    },
    # use PI gap-filled data
    gapfill_pi = if ("TS_pi" %in% names(input)) {
      list(
        TS = write_back_ts(input$TS_sensor, input$TS_pi, "fill_gaps"), TS_QC = qc,
        provenance = ts_provenance_row(site_info, estimator = "swap_pi_gapfill", mode = "fill_gaps")
      )
    } else {
      # Releases can drop the PI column (US-ICs BASE 13-5). The sensor is
      # still the sensor, so skip loudly and record it.
      message(name_site, ": no TS_PI_1 in this release; the sensor's gaps are left unfilled.")
      list(
        TS = input$TS_sensor, TS_QC = qc,
        provenance = ts_provenance_row(site_info, estimator = "swap_pi_gapfill", mode = "none",
                                       note = "skipped: no PI gap-filled column in this release")
      )
    },
    recalibrated = recalibrate_fi_sod_soil_temp(input, site_info),
    # a tropical site: air temperature stands in for soil
    ta_substitute = list(
      TS = write_back_ts(input$TS_sensor, input$TA, "replace"),
      TS_QC = input$TA_QC %||% qc,
      provenance = ts_provenance_row(site_info, estimator = "swap_TA", family = "fixed:TA", mode = "replace")
    ),
    # this site has no TS measurements, so we used TS-TA relationships from
    # nearby US-xGB of the same DBF category.
    borrowed_site = fitted("fixed_us_cwt", "replace",
                           est = fixed_linear_estimator(intercept = 5.13873, slope = 0.64718),
                           note = "coefficients from US-xGB"),
    # this site only missed a few TS data, so only estimate these missing data.
    gapfill_ta = fitted("fixed_us_mbp", "fill_gaps",
                        est = fixed_linear_estimator(intercept = 5.8670273, slope = 0.3688005)),
    # recent data is more accurate
    lm_ta_recent = fitted("lm_ta_recent", "replace"),
    # cold area, use TA above 0 for growing season
    lm_ta_cold = fitted("lm_ta_pos", "replace"),
    reconstructed = fix_soil_temp(input, site_info),
    stop(name_site, ": no stage-A arm for ts_source = ", shQuote(arm))
  )

  # What was asked for, what ran, and whether that leaves a measured column
  # (the field the refuse rule and the fill read).
  out$provenance$ts_qc <- ts_qc
  out$provenance$stage_a_arm <- arm
  out$provenance$ts_truth <- unname(TS_SOURCES[[arm]])
  out
}

# ---------------------------------------------------------- reconstructed

#' Reconstruct soil temperature from air temperature
#'
#' Workflow 01_01, transcribed: the whole column is the estimator's prediction
#' from air temperature (and net radiation, where present), predictors first
#' gap-filled from their DOY x time-of-day climatology. The estimator is
#' `estimate_ts_method` in site_info.csv.
#'
#' @param input The stage-A input, from `*_ts_input()`.
#' @param site_info One row of the site declaration table.
#' @return A list of `TS` and `TS_QC`, aligned to `input`'s rows, and
#'   `provenance`, a `ts_provenance_row()`.
fix_soil_temp <- function(input, site_info) {
  name_site <- site_info[["site_ID"]]
  data <- tibble::tibble(
    TIMESTAMP = input$TIMESTAMP, YEAR = input$YEAR, DOY = input$DOY,
    HOUR = input$HOUR, MINUTE = input$MINUTE,
    TS = input$TS_sensor, TA = input$TA
  )
  if ("NETRAD" %in% names(input)) data$NETRAD <- input$NETRAD

  # The manuscript's training-target rules: what the estimator is fitted on.
  if (name_site == "DE-Hte") {
    # too few data in TS_F_MDS_1, so use TS_F_MDS_2
    need_input(input, "TS_depth2", site_info, "DE-Hte's training target")
    data$TS <- input$TS_depth2
  } else if (name_site == "FR-Bil") {
    # abnormal Soil temperature data before 2021
    data$TS[data$YEAR < 2021] <- NA
  } else if (name_site == "FR-Pue") {
    data$TS[data$YEAR < 2016] <- NA
  }
  if (all(is.na(data$TS))) {
    stop(
      name_site, " needs reconstructed soil temperature, but has no measured soil ",
      "temperature anywhere in its record to train the estimator on. Declared products: ",
      paste(site_sources(site_info), collapse = " + "), ". Check that all of them are downloaded."
    )
  }

  method <- ts_estimate_method(site_info)
  estimator_name <- switch(
    method,
    # Net radiation is available, so the random forest can use it.
    "NETRAD" = "rf_ta_netrad_manuscript",
    # One or two years of TS and incomplete NETRAD: too little to train on.
    "linear regression" = "lm_ta_netrad",
    # No net radiation at this site at all.
    "TA only" = "lm_ta_pos",
    stop(
      name_site, " declares estimate_ts_method = ", shQuote(method),
      ", which is not one of \"NETRAD\", \"linear regression\", or empty."
    )
  )
  est <- ts_estimators()[[estimator_name]]
  if ("NETRAD" %in% est$predictors) need_input(input, "NETRAD", site_info, estimator_name)

  data$TA <- write_back_ts(data$TA, doy_hour_climatology(data, "TA"), "fill_gaps")
  if ("NETRAD" %in% names(data)) {
    data$NETRAD <- write_back_ts(data$NETRAD, doy_hour_climatology(data, "NETRAD"), "fill_gaps")
  }

  mod <- est$fit(data)
  pred <- est$predict(mod, data)

  # As the original: flag 2 ("good gap-filled") where the sensor's flag was
  # missing or 3; flags 0 and 1 are kept.
  qc <- if ("TS_sensor_QC" %in% names(input)) {
    dplyr::if_else(is.na(input$TS_sensor_QC) | input$TS_sensor_QC == 3, 2, input$TS_sensor_QC)
  } else {
    rep(NA_real_, nrow(input))
  }

  list(
    TS = write_back_ts(input$TS_sensor, pred, "replace"),
    TS_QC = qc,
    provenance = ts_provenance_row(
      site_info, estimator = estimator_name, family = est$family, mode = "replace",
      n_train = sum(stats::complete.cases(data[, c("TS", est$predictors), drop = FALSE])),
      note = if (name_site == "DE-Hte") "trained on the second depth" else NA_character_
    )
  )
}

#' Day-of-year x time-of-day climatology of a column
#'
#' The mean of `col` at each day-of-year x hour x minute, aligned to `dat`'s
#' rows: the value the original gap-filled a predictor with. NaN where a slot
#' has no observations at all, which `write_back_ts()` writes through as NA.
#'
#' @param dat Data frame with `DOY`, `HOUR`, `MINUTE` and `col`.
#' @param col Name of the column to average.
#' @return Numeric vector, one value per row of `dat`; NA where the slot has no
#'   observations.
doy_hour_climatology <- function(dat, col) {
  key <- c("DOY", "HOUR", "MINUTE")
  clim <- dat |>
    dplyr::summarise(.clim = mean(.data[[col]], na.rm = TRUE), .by = dplyr::all_of(key))
  out <- dplyr::left_join(dat[, key], clim, by = key)[[".clim"]]
  out[is.nan(out)] <- NA_real_
  out
}

#' The site's soil-temperature reconstruction method
#'
#' `estimate_ts_method` (derived in scripts/revise-site-info.R), with empty
#' spelled "TA only" so every branch of `fix_soil_temp()` has a name.
#'
#' @param site_info One row of the site declaration table.
#' @return The declared method (`"NETRAD"` or `"linear regression"`), or
#'   `"TA only"` where none is declared.
ts_estimate_method <- function(site_info) {
  declared <- site_info[["estimate_ts_method"]]
  if (is.null(declared) || length(declared) != 1 || is.na(declared)) return("TA only")
  declared
}

# ------------------------------------------------------------ recalibrated

#' Last year of FI-Sod's unreliable shallow-sensor record
#'
#' FI-Sod's shallow sensor is unreliable before 2006; the original rebuilt it
#' by chaining two regressions between depths, fitted on row ranges of one
#' release:
#'
#'   mod1 <- lm(data = a[1:24383, ],      TS_F_MDS_2 ~ TS_F_MDS_1)
#'   mod2 <- lm(data = a[90000:245000, ], TS_F_MDS_1 ~ TS_F_MDS_2)
#'
#' Row ranges select different dates -- or nothing -- on any other release.
#' These windows are those ranges resolved to timestamps on the manuscript's
#' file (FLUXNET2015 FULLSET HH, 2001-2014, 245,424 gap-free rows), so they
#' reproduce its coefficients exactly; a test checks that. Year boundaries
#' instead would be wrong: they move the early slope from 0.865 to 0.307 and
#' the rebuilt soil temperature by 8.13 C RMS.
FI_SOD_TS_BAD_THROUGH <- 2005
#' FI-Sod's early recalibration window
#'
#' a[1:24383, ] and a[90000:245000, ] of FLX_FI-Sod_FLUXNET2015_FULLSET_HH_2001-2014_1-4.csv
FI_SOD_EARLY_WINDOW <- c("200101010000", "200205232300")
#' FI-Sod's late recalibration window
FI_SOD_LATE_WINDOW <- c("200602182330", "201412230330")

#' Recalibrate FI-Sod's shallow soil temperature
#'
#' FI-Sod before 2006: early-window shallow -> deep, then good-period deep ->
#' shallow.
#'
#' @param input The stage-A input, with `TS_depth2`.
#' @param site_info One row of the site declaration table.
#' @return A list of `TS` and `TS_QC`, aligned to `input`'s rows, and
#'   `provenance`, a `ts_provenance_row()`. The sensor is returned unchanged
#'   where the record does not cover both windows and a year through 2005.
recalibrate_fi_sod_soil_temp <- function(input, site_info) {
  name_site <- site_info[["site_ID"]]
  need_input(input, c("TS_depth2"), site_info, "the FI-Sod recalibration")
  shallow <- input$TS_sensor
  deep <- input$TS_depth2
  qc <- input[["TS_sensor_QC"]] %||% rep(NA_real_, nrow(input))

  early <- in_timestamp_window(input$TIMESTAMP_START, FI_SOD_EARLY_WINDOW)
  late <- in_timestamp_window(input$TIMESTAMP_START, FI_SOD_LATE_WINDOW)
  # Verbatim from the original: fitted on windows, applied to whole years.
  bad <- input$YEAR <= FI_SOD_TS_BAD_THROUGH

  complete_pairs <- function(i) sum(!is.na(shallow[i]) & !is.na(deep[i]))
  n_early <- complete_pairs(early)
  n_late <- complete_pairs(late)

  # A record that does not reach back past 2006 needs no recalibration.
  if (n_early == 0 || n_late == 0 || !any(bad)) {
    message(
      name_site, ": skipping the pre-", FI_SOD_TS_BAD_THROUGH + 1,
      " soil temperature recalibration. It needs complete shallow/deep pairs in ",
      FI_SOD_EARLY_WINDOW[[1]], "-", FI_SOD_EARLY_WINDOW[[2]],
      " and ", FI_SOD_LATE_WINDOW[[1]], "-", FI_SOD_LATE_WINDOW[[2]],
      "; this record has ", n_early, " and ", n_late, ", spanning ",
      min(input$YEAR), "-", max(input$YEAR), "."
    )
    return(list(
      TS = shallow, TS_QC = qc,
      provenance = ts_provenance_row(site_info, estimator = "recalibration_depth2", mode = "none",
                                     note = "skipped: recalibration windows not in record")
    ))
  }

  pairs <- data.frame(TS_F_MDS_1 = shallow, TS_F_MDS_2 = deep)
  # Early-window shallow -> deep, then good-period deep -> shallow.
  to_deep <- lm(TS_F_MDS_2 ~ TS_F_MDS_1, data = pairs[early, ], na.action = na.omit)
  to_shallow <- lm(TS_F_MDS_1 ~ TS_F_MDS_2, data = pairs[late, ], na.action = na.omit)

  deep_est <- predict(to_deep, data.frame(TS_F_MDS_1 = shallow[bad]))
  rebuilt <- rep(NA_real_, length(shallow))
  rebuilt[bad] <- predict(to_shallow, data.frame(TS_F_MDS_2 = deep_est))
  qc[bad] <- 2

  list(
    # An overlay of the rebuilt years over the sensor: exactly `ts[bad] <- ...`.
    TS = write_back_ts(shallow, rebuilt, "overlay"),
    TS_QC = qc,
    provenance = ts_provenance_row(
      site_info, estimator = "recalibration_depth2", family = "lm:TS_depth2", mode = "overlay",
      n_train = n_late, note = sprintf("rows through %d rebuilt", FI_SOD_TS_BAD_THROUGH)
    )
  )
}
