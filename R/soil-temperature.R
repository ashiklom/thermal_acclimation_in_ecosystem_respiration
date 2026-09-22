# Soil temperature: both stages, in one file.
#
# Soil temperature reaches the model through two stages, and the manuscript's
# logic requires that they stay two:
#
#   Stage A, `qualification_soil_temperature()` (step 01): the
#     *qualification-facing* column. A sensor after its per-site repairs, or a
#     reconstruction where the site has no usable sensor. The QC filters, the
#     growing-season detector and the year gap scan all run on it, and the
#     manuscript fitted its 35-site `TS ~ TA` regression on the years that
#     scan qualified. It leaves step 01 as `TS_measured`, beside every
#     candidate step 01 can produce, with a provenance row saying what it is.
#
#   Stage B, `get_soil_temperature()` (step 02): the *fit-facing* column. One
#     candidate is selected under
#     the recipe's strategy, the per-site fill is attached if the strategy asks
#     for it, the temperature bounds are re-derived on the selected column, and
#     every other candidate is dropped so that nothing downstream can reach
#     for one. What leaves is `TS_final` and a metadata row saying what it is.
#
# Downstream of this function -- the window layout, the control year, both
# models -- there is exactly one soil-temperature column and no branching on
# site, origin or strategy. Anything that needs to know where `TS_final` came
# from reads `meta`, not the table.
get_soil_temperature <- function(site_data, site_info, recipe = NULL, fill = NULL,
                                 ts_col = NULL) {
  recipe <- recipe %||% original_recipe()
  name_site <- site_info[["site_ID"]]
  ac <- site_data[["ac"]]
  night <- site_data[["nightNEE"]]

  # Step 01 produced every candidate this site offers along with the bounds
  # belonging to each, so the column and its bounds are selected together and
  # cannot disagree.
  ts_bounds_all <- site_data[["ts_bounds"]]
  if (is.null(ts_bounds_all)) {
    stop(
      name_site, ": this site_data was built before soil-temperature columns ",
      "were carried explicitly, so it has no `ts_bounds`. Rebuild it with ",
      "`prep_nee_ac()`, or delete the stale `_targets/` store."
    )
  }

  # `ts_col` overrides the strategy, for sensitivity runs that compare
  # estimation methods against each other on the same site.
  override <- ts_col
  choice <- if (is.null(override)) {
    choose_ts_col(recipe, site_data, site_info, fill)
  } else {
    list(ts_col = override, reason = "ts_col argument")
  }
  ts_col <- choice[["ts_col"]]

  # No second method where there is no measured truth to fit it against.
  #
  # At 27 sites the column step 01 calls `TS_measured` has rows that are not
  # a sensor reading -- a whole-column reconstruction at 25 of them, a
  # partial one at FI-Sod and US-MBP (`ts_source` in site_info.csv says
  # which). Every variant strategy's alternative to that column is a model
  # fitted *to* it: `TS_linear` regresses it on air temperature, `TS_memfill`
  # is cross-validated against it. Fitted to a reconstruction, either one
  # returns a function of the same predictors wearing a skill score that
  # measures how well a regression reproduces a regression. So a variant
  # keeps step 01's column at these sites and says why. The manuscript's own
  # strategy is exempt: `ts_col` there is a declaration, and its two
  # legitimate double-applications (FI-Sod, US-MBP) are the manuscript's.
  # An explicit `ts_col` argument is a sensitivity run and is honoured.
  #
  # The prep-stage `ts_qc = sensor` strategy is where a variant gets to act
  # at these sites instead: qualify on the raw sensor, and there is a truth.
  refused <- FALSE
  if (is.null(override) && !identical(recipe$ts, "site_info") &&
      identical(stage_a_truth(site_data, site_info), "none") &&
      !identical(ts_col, "TS_measured")) {
    refused <- TRUE
    choice <- list(
      ts_col = "TS_measured",
      reason = sprintf(
        paste0("kept step 01's column: ts_source = %s leaves no measured soil ",
               "temperature to fit a second method against (%s strategy wanted %s)"),
        stage_a_arm(site_data, site_info), recipe$ts, ts_col
      )
    )
    ts_col <- "TS_measured"
  }

  # The reconstructed column is attached here, not in step 01, because it is
  # produced by its own per-site target (`fill_soil_temp()`) and only a
  # `memory_fill` recipe reads it. Its rows align with `site_data` by
  # construction -- the fill was computed from the same tables -- and that is
  # asserted rather than assumed. Its native bounds definition is the
  # half-hourly one, like the other reconstructed column's.
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
    fill_rows <- fill$ts_bounds
    fill_rows$native <- fill_rows$definition == "halfhourly"
    ts_bounds_all <- dplyr::bind_rows(ts_bounds_all, fill_rows)
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
    ts_verdict = if (!is.null(ts_qc)) ts_qc$verdict[[1]] else NA_character_,
    ts_flags = if (!is.null(ts_qc)) ts_qc$flags[[1]] else NA_character_,
    # Whether the column step 01 called measured is a reconstruction it made,
    # whatever recipe is in force. Strategies select downstream of those
    # reconstructions and cannot undo them (ts-variants.html, V4).
    ts_measured_synthetic = identical(stage_a_truth(site_data, site_info), "none"),
    ts_source = ts_source(site_info),
    ts_refused = refused,
    # What stage A did to make `TS_measured`, from the provenance row step 01
    # carries; NA on a site_data built before it did.
    stage_a_estimator = prov[["stage_a_estimator"]] %||% NA_character_,
    stage_a_family = prov[["stage_a_family"]] %||% NA_character_,
    stage_a_mode = prov[["stage_a_mode"]] %||% NA_character_,
    fill_method = fill_method,
    fill_cv_rmse = fill_cv_rmse,
    fill_degenerate = fill_degenerate,
    fill_truth_synthetic = fill_truth_synthetic,
    bounds_strategy = recipe$bounds,
    bounds_reason = bounds[["reason"]],
    tStart = bounds[["tStart"]],
    tEnd = bounds[["tEnd"]]
  )

  list(ac = ac, nightNEE = night, meta = meta)
}

# What stage A actually did at this site, from the provenance row step 01
# carries -- under `ts_qc = sensor` that is not what `ts_source` declares.
# Falls back to the declaration for a site_data built before the row existed.
stage_a_truth <- function(site_data, site_info) {
  prov <- site_data[["ts_provenance"]]
  if (!is.null(prov) && "ts_truth" %in% names(prov)) prov[["ts_truth"]][[1]] else ts_measured_truth(site_info)
}
stage_a_arm <- function(site_data, site_info) {
  prov <- site_data[["ts_provenance"]]
  if (!is.null(prov) && "stage_a_arm" %in% names(prov)) prov[["stage_a_arm"]][[1]] else ts_source(site_info)
}

# `TS_final`, and no other soil-temperature column.
#
# The candidates are dropped on purpose, not for tidiness: as long as
# `TS_measured` or `TS_linear` is still on the table, a later stage can read
# it, and "no branching downstream" is a convention rather than a property.
# With one column left it is checkable -- tests/testthat/test-soil-temperature.R
# asserts it -- and a stage that wants to know the column's origin has to ask
# `meta`, which is the only place the answer is recorded correctly.
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

# Every soil-temperature column on a table: `TS` and anything `TS_*`.
ts_candidate_columns <- function(dat) {
  grep("^TS($|_)", names(dat), value = TRUE)
}

# =========================================================== stage A
#
# The qualification-facing column: what step 01's QC filters, growing-season
# detector and year gap scan run on, and what leaves step 01 as `TS_measured`.
# A sensor after its per-site repairs, or a reconstruction where the site has
# no usable sensor -- as the manuscript's `01_01` and `01_02` scripts made it.
#
# Both readers hand in the same shape, `*_ts_input()` below, and get back
# `TS`, `TS_QC` and a provenance row. Which arm runs is `ts_source` in
# site_info.csv (see `TS_SOURCES` in R/constants.R for what each level is);
# nothing here tests a site name except the three training-target rules
# inside the `reconstructed` arm, which are the manuscript's and are named as
# such.

# The columns every reader provides. `TIMESTAMP_START` is the 12-digit stamp
# both products carry; FI-Sod's recalibration windows are expressed in it.
TS_INPUT_REQUIRED <- c("TIMESTAMP", "TIMESTAMP_START", "YEAR", "DOY", "HOUR", "MINUTE", "TS_sensor", "TA")
# Provided where the record has them; an arm that needs one and lacks it
# fails by name.
TS_INPUT_OPTIONAL <- c("TS_sensor_QC", "TA_QC", "NETRAD", "TS_depth2", "TS_depth2_QC", "TS_pi")

# The FLUXNET-family record's columns, under the shared names. `TS_F_MDS_1` is
# the shallow sensor and `TS_F_MDS_2` the second depth; `TA_F_MDS` carries its
# own QC flag, which the air-temperature substitute inherits.
fluxnet_ts_input <- function(a, site_info) {
  pick <- function(col) if (col %in% names(a)) a[[col]] else NULL
  out <- tibble::tibble(
    TIMESTAMP = a$TIMESTAMP, TIMESTAMP_START = a$TIMESTAMP_START,
    YEAR = a$YEAR, DOY = a$DOY, HOUR = a$HOUR, MINUTE = a$MINUTE,
    TS_sensor = pick("TS_F_MDS_1") %||% rep(NA_real_, nrow(a)),
    TS_sensor_QC = pick("TS_F_MDS_1_QC") %||% rep(NA_real_, nrow(a)),
    TA = a$TA_F_MDS,
    TA_QC = pick("TA_F_MDS_QC") %||% rep(NA_real_, nrow(a))
  )
  if (!is.null(pick("NETRAD"))) out$NETRAD <- a$NETRAD
  if (!is.null(pick("TS_F_MDS_2"))) {
    out$TS_depth2 <- a$TS_F_MDS_2
    out$TS_depth2_QC <- pick("TS_F_MDS_2_QC") %||% rep(NA_real_, nrow(a))
  }
  # A site that is supposed to have a sensor and whose spliced record has no
  # `TS_F_MDS_1` at all -- FR-Fon's Warm Winter 2020 archive, say -- has to
  # say so here rather than fail later as "no observations after the filter".
  if (is.null(pick("TS_F_MDS_1")) && !ts_source(site_info) %in% c("ta_substitute")) {
    stop(
      site_info[["site_ID"]], ": the spliced record has no TS_F_MDS_1 column. Declared ",
      "products: ", paste(site_sources(site_info), collapse = " + "),
      ". Check that all of them are downloaded."
    )
  }
  out
}

# The AmeriFlux BASE record's columns, under the shared names. The sensor and
# air-temperature columns are the ones site_info.csv declares for the site
# (`check_declared_columns()` has already confirmed they exist); net radiation
# is `netrad_column` where declared, a bare `NETRAD` where present; `TS_PI_1`
# is the PI's gap-filled soil temperature where the record carries one. There
# is no QC flag on any of them.
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

# One row describing what stage A did, carried in site_data as
# `ts_provenance` and copied into every fit's settings by
# `get_soil_temperature()`.
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

need_input <- function(input, cols, site_info, why) {
  absent <- setdiff(cols, names(input))
  if (length(absent)) {
    stop(
      site_info[["site_ID"]], ": ", why, " needs ", paste(absent, collapse = ", "),
      ", which the record does not provide."
    )
  }
}

qualification_soil_temperature <- function(input, site_info, ts_qc = "manuscript") {
  name_site <- site_info[["site_ID"]]
  absent <- setdiff(TS_INPUT_REQUIRED, names(input))
  if (length(absent)) {
    stop(name_site, ": the stage-A input lacks ", paste(absent, collapse = ", "), ".")
  }
  n <- nrow(input)
  qc <- if ("TS_sensor_QC" %in% names(input)) input$TS_sensor_QC else rep(NA_real_, n)

  # Which arm runs. `manuscript` is the declaration. `sensor` keeps the arms
  # whose result is measured at every row -- the sensor itself, the second
  # depth, the PI gap-fill -- and replaces every other arm with the raw
  # declared sensor, so that qualification, the screen and the fill's truth
  # are all measurements. A site whose raw sensor is too sparse to qualify a
  # year then fails step 01, which is the honest result for that variant.
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
      # The PI's gap-filled product is a column the data provider may stop
      # publishing: US-ICs's BASE 13-5 has none, where 9-5 did. Skipping
      # loudly beats erroring, as with FI-Sod's recalibration -- the sensor
      # is still the sensor, only its gaps stay gaps -- and the provenance
      # row says the manuscript's fill did not happen here.
      message(name_site, ": no TS_PI_1 in this release; the sensor's gaps are left unfilled.")
      list(
        TS = input$TS_sensor, TS_QC = qc,
        provenance = ts_provenance_row(site_info, estimator = "swap_pi_gapfill", mode = "none",
                                       note = "skipped: no PI gap-filled column in this release")
      )
    },
    recalibrated = recalibrate_fi_sod_soil_temp(input, site_info),
    # use air temperature for this tropical site so that all tropical sites,
    # we used bottom air temperature.
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

  # The provenance row says what was declared, what was asked for, which arm
  # actually ran, and -- the field the refuse rule and the fill read -- whether
  # what ran leaves a measured column.
  out$provenance$ts_qc <- ts_qc
  out$provenance$stage_a_arm <- arm
  out$provenance$ts_truth <- unname(TS_SOURCES[[arm]])
  out
}

# ---------------------------------------------------------- reconstructed
#
# Workflow `01_01_estimate_soil_temperature_at_some_sites.R`, transcribed: the
# whole column is the estimator's prediction from air temperature and, where
# the site has it, net radiation, with the predictors gap-filled from their
# own day-of-year x time-of-day climatology first. Which estimator is
# `estimate_ts_method` in site_info.csv -- see `ts_estimate_method()`.
fix_soil_temp <- function(input, site_info) {
  name_site <- site_info[["site_ID"]]
  data <- tibble::tibble(
    TIMESTAMP = input$TIMESTAMP, YEAR = input$YEAR, DOY = input$DOY,
    HOUR = input$HOUR, MINUTE = input$MINUTE,
    TS = input$TS_sensor, TA = input$TA
  )
  if ("NETRAD" %in% names(input)) data$NETRAD <- input$NETRAD

  # The manuscript's three training-target rules, by site. They restrict what
  # the estimator is *fitted on*, not what it predicts, and are the one place
  # stage A still tests a site name.
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

  # The predictors are gap-filled from their own day-of-year x time-of-day
  # climatology before anything is fitted, as the original did.
  data$TA <- write_back_ts(data$TA, doy_hour_climatology(data, "TA"), "fill_gaps")
  if ("NETRAD" %in% names(data)) {
    data$NETRAD <- write_back_ts(data$NETRAD, doy_hour_climatology(data, "NETRAD"), "fill_gaps")
  }

  mod <- est$fit(data)
  pred <- est$predict(mod, data)

  # The QC flag, where the record has one: a reconstructed value is "good
  # gap-filled" (2) wherever the sensor's flag was missing or poor, as the
  # original set it. Where the flag was 0 or 1 it is kept, so the manuscript's
  # QC filter admits the same rows it did.
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

# The mean of `col` at each day-of-year x hour x minute, aligned to `dat`'s
# rows: the value the original gap-filled a predictor with. NaN where a slot
# has no observations at all, which `write_back_ts()` writes through as NA.
doy_hour_climatology <- function(dat, col) {
  key <- c("DOY", "HOUR", "MINUTE")
  clim <- dat |>
    dplyr::summarise(.clim = mean(.data[[col]], na.rm = TRUE), .by = dplyr::all_of(key))
  out <- dplyr::left_join(dat[, key], clim, by = key)[[".clim"]]
  out[is.nan(out)] <- NA_real_
  out
}

# Which estimator reconstructs a site's soil temperature, from the declaration
# rather than a site list. `estimate_ts_method` is derived in
# `scripts/revise-site-info.R`: "NETRAD" wherever a `netrad_column` is named,
# "linear regression" at the two sites with too little soil temperature to
# train a forest on, and empty otherwise. Empty is spelled out here so that
# every branch of `fix_soil_temp()` has a name to report.
ts_estimate_method <- function(site_info) {
  declared <- site_info[["estimate_ts_method"]]
  if (is.null(declared) || length(declared) != 1 || is.na(declared)) return("TA only")
  declared
}

# ------------------------------------------------------------ recalibrated
#
# FI-Sod's shallow sensor is unreliable before 2006, and the manuscript rebuilt
# it by chaining two regressions between the sensor depths: early-window
# shallow -> deep, then good-period deep -> shallow. (The `recalibrate_fi_sod_*`
# constants and their rationale are unchanged from before this function took
# the shared stage-A input; see the block comment above them.)
recalibrate_fi_sod_soil_temp <- function(input, site_info) {
  name_site <- site_info[["site_ID"]]
  need_input(input, c("TS_depth2"), site_info, "the FI-Sod recalibration")
  shallow <- input$TS_sensor
  deep <- input$TS_depth2
  qc <- if ("TS_sensor_QC" %in% names(input)) input$TS_sensor_QC else rep(NA_real_, nrow(input))

  in_window <- function(w) input$TIMESTAMP_START >= w[[1]] & input$TIMESTAMP_START <= w[[2]]
  early <- in_window(FI_SOD_EARLY_WINDOW)
  late <- in_window(FI_SOD_LATE_WINDOW)
  # The rows being rebuilt. This predicate is verbatim from the original, which
  # fitted on windows but applied to whole years.
  bad <- input$YEAR <= FI_SOD_TS_BAD_THROUGH

  complete_pairs <- function(i) sum(!is.na(shallow[i]) & !is.na(deep[i]))
  n_early <- complete_pairs(early)
  n_late <- complete_pairs(late)

  # Both relationships have to be estimable. Skipping loudly beats erroring: a
  # record that does not reach back past 2006 needs no recalibration and should
  # not take the whole site down with it.
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
