.data <- rlang::.data

fix_soil_temp <- function(a, site_info) {
  name_site <- site_info[["site_ID"]]

  if (site_info$source == "AmeriFlux_BASE") {
    data <- a |>
      dplyr::select(
        "TIMESTAMP", "YEAR", "DOY", "HOUR", "MINUTE",
        dplyr::all_of(c(site_info$TS, site_info$TA))
      )
    if (name_site %in% c('US-Los', "US-Ced")) {
      data$NETRAD <- a$NETRAD_1_1_1
    } else if (name_site %in% c("US-SRG", "CA-Man")) {
      data$NETRAD <- a$NETRAD
    } else if (name_site %in% c("US-Ho1")) {
      data$NETRAD <- a$NETRAD_2_1_1
    } 
  } else if (site_info$source %in% c("TERN", "ICOS", "FLUXNET", "FLUXNET2015")) {
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
  } else {
    stop("Unknown data source: ", site_info$source)
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

  if (name_site %in% c("DE-Akm", "FR-Pue", "US-Los", "US-SRG", "CA-Man", "US-Ced", "US-Ho1", "CZ-RAJ")) {
    # use NETRAD because it is available
    use_NETRAD <- TRUE
    data <- predict_soil_temp(data, use_NETRAD)
  } else if (name_site %in% c("DE-Hte", "FR-FBn")) {
    # use simple linear regression because these sites have 1-year or two-year TS, and NETRAD is incomplete
    lm <- lm(data = data, TS ~ TA + NETRAD)
    data$TS_pred <- predict(lm, data)
  } else {
    # no netrad at this site
    lm <- lm(data = data[data$TA > 0, ], TS ~ TA)
    data$TS_pred <- predict(lm, data)
  }

  data
}


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

  message("Training data performance by random forests: ")
  print(caret::postResample(pred = y0, obs = train$TS))

  message("Testing data performance by random forests: ")
  print(caret::postResample(pred = y1, obs = test$TS))

  # compare with linear regression
  message("Performance by linear regression: ")
  print(summary(lm))
  # random forest is much better than lm;

  # do the prediction
  data$TS_pred <- predict(rf, data)

  data[, c("TIMESTAMP", "TS_pred")]
}
