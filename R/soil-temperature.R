# The single point at which a run's soil temperature is decided.
#
# Soil temperature reaches the model through two stages, and the manuscript's
# logic requires that they stay two:
#
#   Stage A, in the readers (step 01): the *qualification-facing* column. A
#     sensor after its per-site repairs, or a reconstruction where the site has
#     no usable sensor. The QC filters, the growing-season detector and the
#     year gap scan all run on it, and the manuscript fitted its 35-site
#     `TS ~ TA` regression on the years that scan qualified. It leaves step 01
#     as `TS_measured`, beside every candidate step 01 can produce.
#
#   Stage B, here: the *fit-facing* column. One candidate is selected under
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
      identical(ts_measured_truth(site_info), "none") &&
      !identical(ts_col, "TS_measured")) {
    refused <- TRUE
    choice <- list(
      ts_col = "TS_measured",
      reason = sprintf(
        paste0("kept step 01's column: ts_source = %s leaves no measured soil ",
               "temperature to fit a second method against (%s strategy wanted %s)"),
        ts_source(site_info), recipe$ts, ts_col
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
    ts_measured_synthetic = ts_measured_is_synthetic(site_info),
    ts_source = ts_source(site_info),
    ts_refused = refused,
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
