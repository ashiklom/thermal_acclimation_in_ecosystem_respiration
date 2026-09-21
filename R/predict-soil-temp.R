.data <- rlang::.data

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

fix_soil_temp <- function(a, site_info) {
  name_site <- site_info[["site_ID"]]

  if (site_reader(site_info) == "ameriflux") {
    # Renamed, not just selected. The AmeriFlux columns are named per site
    # (`TS_PI_1_1_A`, `T_SONIC_1_1_1`, ...), and everything below this point --
    # the gap fills, the formulae, `predict_soil_temp()` -- is written against
    # `TS` and `TA`. Selecting them under their original names left `data$TA`
    # NULL, which `sum(is.na(NULL))` reports as zero missing values, so the gap
    # fill quietly did nothing and the fit failed with "object 'TA' not found".
    # Nothing reached this branch to notice: the only sites that need it are
    # the `estimate_Ts` ones, and the reader stopped before calling it.
    data <- a |>
      dplyr::select(
        "TIMESTAMP", "YEAR", "DOY", "HOUR", "MINUTE",
        TS = dplyr::all_of(site_info$TS),
        TA = dplyr::all_of(site_info$TA)
      )
    # Which column holds net radiation is declared per site in
    # site_info.csv. It used to be an if/else here as well, duplicating the
    # mapping that `scripts/revise-site-info.R` already builds -- the two
    # agreed, but only by hand.
    netrad_column <- site_info[["netrad_column"]]
    if (!is.na(netrad_column)) {
      if (!netrad_column %in% names(a)) {
        stop(
          name_site, " declares netrad_column = ", shQuote(netrad_column),
          ", which is not in its AmeriFlux record."
        )
      }
      data$NETRAD <- a[[netrad_column]]
    }
    
  } else {
    # TS_F_MDS_1 is the training target here, not the output: the model learns
    # TS from TA (and NETRAD) on the hours where TS was measured, then predicts
    # the rest. So some measured TS has to be present somewhere in the record.
    # A site whose only downloaded product omits soil temperature entirely --
    # FR-Fon's Warm Winter 2020 archive, for instance -- needs its later
    # products fetched before this can run.
    needed <- c("TS_F_MDS_1", "TA_F_MDS", "NETRAD")
    absent <- setdiff(needed, names(a))
    if (length(absent)) {
      stop(
        name_site, " needs predicted soil temperature, but the spliced record ",
        "has no ", paste(absent, collapse = ", "), " column. Declared products: ",
        paste(site_sources(site_info), collapse = " + "),
        ". Check that all of them are downloaded."
      )
    }
    data <- a |>
      dplyr::select(
        "TIMESTAMP", "YEAR", "DOY", "HOUR", "MINUTE",
        TS = "TS_F_MDS_1",
        TA = "TA_F_MDS",
        NETRAD = "NETRAD"
      )

    if (name_site == 'DE-Hte') {
      # too few data in TS_F_MDS_1, so use TS_F_MDS_2
      data$TS <- a$TS_F_MDS_2
    } else if (name_site == 'FR-Bil') {
      # abnormal Soil temperature data before 2021
      data$TS[data$YEAR < 2021] <- NA
    } else if (name_site == "FR-Pue") {
      data$TS[data$YEAR < 2016] <- NA
    }
  }

  # Gapfill missing air temperature data
  if (sum(is.na(data$TA)) > 0) {
    TA_gf <- data |>
      dplyr::summarise(TA_gf = mean(.data$TA, na.rm = TRUE), .by = c("DOY", "HOUR", "MINUTE"))
    data <- data |> dplyr::left_join(TA_gf, by = c("DOY", "HOUR", "MINUTE"))
    data$TA[is.na(data$TA)] <- data$TA_gf[is.na(data$TA)]
  }

  if ("NETRAD" %in% colnames(data) && (sum(is.na(data$NETRAD)) > 0)) {
    # Gapfill missing NETRAD data
    gf_netrad <- data |>
      dplyr::summarise(NETRAD_gf = mean(.data$NETRAD, na.rm = TRUE), .by = c("DOY", "HOUR", "MINUTE"))
    data <- data |> dplyr::left_join(gf_netrad, by = c("DOY", "HOUR", "MINUTE"))
    data$NETRAD[is.na(data$NETRAD)] <- data$NETRAD_gf[is.na(data$NETRAD)]
  }

  # The two site lists this replaced -- the random-forest sites and the two
  # linear-regression ones -- were exactly `estimate_ts_method == "NETRAD"`
  # and `== "linear regression"`. Keeping both was how a site could be given a
  # net radiation column and still be silently fitted without it; it is also
  # what would have had to be edited, by hand, to route the AmeriFlux sites
  # here. tests/testthat/test-soil-temp-columns.R pins the two against each
  # other, so the equivalence is checked rather than asserted in prose.
  method <- ts_estimate_method(site_info)
  data$TS_pred <- switch(
    method,
    # Net radiation is available, so the random forest can use it.
    "NETRAD" = predict_soil_temp(data, use_NETRAD = TRUE)[["TS_pred"]],
    # One or two years of TS and incomplete NETRAD: too little to train on.
    "linear regression" = predict(lm(data = data, TS ~ TA + NETRAD), data),
    # No net radiation at this site at all.
    "TA only" = replace_ts(predict_ts_from_ta(ts_ta_model(data[data$TA > 0, ]), data$TA)),
    stop(
      name_site, " declares estimate_ts_method = ", shQuote(method),
      ", which is not one of \"NETRAD\", \"linear regression\", or empty."
    )
  )

  # Only the prediction and its key. Every caller joins it back onto the table
  # it came from, and the intermediate gap-fill columns are not theirs to see.
  data[, c("TIMESTAMP", "TS_pred")]
}


# NB no seed by default. The random 70/30 split and the forest itself are
# unseeded here because every pipeline call arrives inside a target, and
# `targets` already derives a deterministic per-target seed from the target's
# name -- so a pipeline run reproduces, while `set.seed(222)` at this depth
# would instead pin every site to the same draw. Pass `seed` when calling this
# outside the pipeline and reproducibility is wanted.
predict_soil_temp <- function(data, use_NETRAD, seed = NULL) {
  # separate data into trained or tested
  if (!is.null(seed)) {
    set.seed(seed)    # default value = 222
  }
  # use a maximum of 60000 data to train and test the model; too many data will
  # cause RF super slow and may not improve accuracy.
  sampled <- sample(seq_len(nrow(data)), size = min(60000, nrow(data)), replace = FALSE)
  ind <- sample(2, length(sampled), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[sampled[ind == 1], ]
  test <- data[sampled[ind == 2], ]
  # remove NA data
  train <- na.omit(train)
  test  <- na.omit(test)

  if (use_NETRAD) {
    rf <- randomForest::randomForest(formula = TS ~ TA + NETRAD, data = train) # this takes lots of time
    lm <- lm(data = data, TS ~ TA + NETRAD)
  } else {
    rf <- randomForest::randomForest(formula = TS ~ TA, data = train) # this takes lots of time
    lm <- lm(data = data, TS ~ TA)
  }

  y0 <- predict(rf, train)
  y1 <- predict(rf, test)

  # Through `message()`, not `print()`, so that a caller can silence them.
  # These are the original's diagnostics and worth keeping -- they are the
  # only report of how well the reconstruction fits -- but they run once per
  # `estimate_Ts` site, and printing a full `lm` summary straight to stdout
  # from inside a test or a `tar_make()` worker buries everything else.
  report <- function(label, x) {
    message(label, "\n", paste(utils::capture.output(print(x)), collapse = "\n"))
  }
  report("Training data performance by random forests:", caret::postResample(pred = y0, obs = train$TS))
  report("Testing data performance by random forests:", caret::postResample(pred = y1, obs = test$TS))
  # compare with linear regression
  report("Performance by linear regression:", summary(lm))
  # random forest is much better than lm;

  # do the prediction
  data$TS_pred <- predict(rf, data)

  data[, c("TIMESTAMP", "TS_pred")]
}
