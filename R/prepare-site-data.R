# source("R/constants.R")
# source("R/utils.R")
# source("R/respiration_helpers.R")
# name_site <- "AU-Tum"
# name_site <- "FI-Hyy"

prepare_nee_ac_old <- function(name_site) {
  site_info <- get_site_info(name_site)

  a <- read_site_a(name_site)

  if (site_info$source == "AmeriFlux_BASE") {
    a <- amf_read_base(files_AmeriFlux_BASE[grepl(name_site, files_AmeriFlux_BASE)], parse_timestamp=TRUE, unzip = T)
    data <- data.frame(TIMESTAMP = a$TIMESTAMP, YEAR = a$YEAR, DOY = a$DOY, HOUR = a$HOUR, MINUTE = a$MINUTE, 
                       TS = a[, trimws(site_info$TS[id])], TA = a[, trimws(site_info$TA[id])])
    if (name_site %in% c('US-Los', "US-Ced")) {
      data$NETRAD <- a$NETRAD_1_1_1
    } else if (name_site %in% c("US-SRG", "CA-Man")) {
      data$NETRAD <- a$NETRAD
    } else if (name_site %in% c("US-Ho1")) {
      data$NETRAD <- a$NETRAD_2_1_1
    } 
  } else if (site_info$source == "ICOS") {

  } else if (site_info$source == "TERN") {

  } else if (site_info$source == "FLUXNET") {
    
  } else {
    stop("Unkonwn source: ", site_info$source)
  }

  a_measure_night_complete <- a |>
    dplyr::filter(
      !is.na(.data$TA_F_MDS),
      !is.na(.data$TS_F_MDS),
      .data$TS_F_MDS_1_QC %in% c(0, 1, 2),
      .data$NEE_VUT_REF_QC == 0,
      .data$NIGHT == 1,
      .data$NEE_VUT_REF > -5 & .data$NEE_VUT_REF < 30
    )

  # select useful column and year to output
  ac <- a[, c("YEAR", "MONTH", "DAY", "DOY", "HOUR", "MINUTE")]
  ac$NEE <- a$NEE_VUT_REF
  ac$NEE_QC <- a$NEE_VUT_REF_QC
  ac$TA <- a$TA_F_MDS
  ac$TS <- a$TS_F_MDS_1
  ac$SWC <- a$SWC_F_MDS_1
  ac$NEE_uStar_f <- a$NEE_VUT_REF
  ac$daytime <- a$NIGHT != 1
  ac$SW_IN <- a$SW_IN_F_MDS
  ac$GPP_DT <- a$GPP_DT_VUT_REF
  
  # only keep years between start year and end year
  iStart = a_measure_night_complete$YEAR[1]
  iEnd   = a_measure_night_complete$YEAR[nrow(a_measure_night_complete)]
  ac <- ac |> dplyr::filter(dplyr::between(.data$YEAR, iStart, iEnd))

  feature_gs <- tibble::tibble(
    site_ID = name_site,
    gStart = gStart,
    gEnd = gEnd,
    tStart = max(tStart, 0.0),
    tEnd = tEnd,
    nyear = length(good_years)
  )

  list(
    ac = ac,
    nightNEE = a_measure_night_complete
  )
}


prep_nee_ac <- function(name_site) {
  site_info <- get_site_info(name_site)

  if (site_info$source == "AmeriFlux_BASE") {
    files_AmeriFlux_BASE <- list.files(
      file.path(DIR_RAWDATA, "Ameriflux"), pattern = "^AMF_.*_BASE.*\\.zip$", full.names = TRUE, recursive = TRUE
    )
    file_path <- files_AmeriFlux_BASE[grepl(name_site, files_AmeriFlux_BASE)]
    stopifnot(length(file_path) == 1)
    a <- amf_read_base(files_AmeriFlux_BASE[grepl(name_site, files_AmeriFlux_BASE)], parse_timestamp = TRUE, unzip = TRUE)
    a[a == -9999] <- NA
  } else if (site_info$source %in% c("FLUXNET", "FLUXNET2015")) {
    file_path <- list.files(
      file.path(DIR_RAWDATA, "FLUXNET", name_site),
      pattern = "_FLUXMET_(HH|HR)_", full.names = TRUE, recursive = TRUE
    )
    stopifnot(length(file_path) == 1)
  } else if (site_info$source == "TERN") {
    file_path <- file.path(DIR_RAWDATA, "TERN", name_site, sprintf(
      "%s_TERN_L3_FLUXNET_HH.csv",
      name_site
    ))
    a <- read.csv(file_path, stringsAsFactors = FALSE)
    a[a == -9999] <- NA
  } else if (site_info$source == "ICOS") {
    file_path <- file.path(DIR_RAWDATA, "ICOS", name_site, sprintf(
      "%s_ICOS_L2_FLUXNET_HH.csv",
      name_site
    ))
    a <- read.csv(file_path, stringsAsFactors = FALSE)
    a[a == -9999] <- NA
  }

  stopifnot(file.exists(file_path))


  dt <- lubridate::ymd_hm(a$TIMESTAMP_START[2]) - lubridate::ymd_hm(a$TIMESTAMP_START[1])
  a$TIMESTAMP <- lubridate::ymd_hm(a$TIMESTAMP_START) + dt / 2
  a$YEAR <- lubridate::year(a$TIMESTAMP)
  a$MONTH <- lubridate::month(a$TIMESTAMP)
  a$DAY <- lubridate::day(a$TIMESTAMP)
  a$DOY <- lubridate::yday(a$TIMESTAMP)
  a$HOUR <- lubridate::hour(a$TIMESTAMP)
  a$MINUTE <- lubridate::minute(a$TIMESTAMP)

  # a <- fix_soil_temp(a, name_site)
  if (name_site == "CZ-Stn") {
    # use TS of second layer because the first layer is incomplete
    a$TS_F_MDS_1 <- a$TS_F_MDS_2  
    a$TS_F_MDS_1_QC <- a$TS_F_MDS_2_QC
  } else if (name_site %in% c("FR-Fon", "CH-Dav", "DE-Akm", "DE-Hte", "FR-Bil", "FR-Pue", "FR-FBn", "CZ-RAJ")) {
    # TODO: Need the random forest soil temperature fits here
    stop("special case not implemented yet")
  } else if (name_site == "FI-Sod") {
    # TODO: Custom soil temperature
    stop("special case not implemented yet")
  } else if (name_site == "GF-Guy") {
    # use air temperature for this tropical site so that all tropical sites, we used bottom air temperature. 
    a$TS_F_MDS_1 <- a$TA_F_MDS
    a$TS_F_MDS_1_QC <- a$TA_F_MDS_QC
  }

  if (name_site == "FR-Pue") {
    # FLUXNET data do not include soil data for Site FR-Pue, but site PI provided the SWC data
    stop("site-specific logic not implemented")
  }

  southern_hemisphere <- FALSE
  if (site_info[["LAT"]] < 0) {
    # TODO: Work out the southern hemisphere logic
    stop("southern hemisphere sites not implemented yet")
    a$DOY[a$DOY < 183] <- a$DOY[a$DOY < 183] + 366
    southern_hemisphere <- TRUE
  }

  # check if SWC was missed. 
  SWC <- TRUE
  if (!'SWC_F_MDS_1' %in% colnames(a)) {
    a$SWC_F_MDS_1 <- NA_real_
    SWC <- FALSE
  }
  if (!site_info$SWC_use) {
    SWC <- FALSE
  }

  measured <- a |>
    dplyr::filter(
      !is.na(.data$TA_F_MDS),
      .data$TS_F_MDS_1_QC %in% c(0, 1, 2),
      # NOTE: Normally, the QC flag check should be enough to catch NA values.
      # But in some cases, NA values still slip through, so we filter
      # explicitly here as well.
      !is.na(.data$TS_F_MDS_1),
      .data$NEE_VUT_REF_QC == 0,
      .data$NIGHT == 1,
      .data$NEE_VUT_REF > -5,
      .data$NEE_VUT_REF < 30
    ) |>
    tibble::as_tibble()

  if (nrow(measured) == 0) {
    stop(name_site, " has no observations after the TERN nighttime quality filter.")
  }

  yearly <- a |>
    dplyr::summarise(
      NEE = mean(NEE_VUT_REF, na.rm = TRUE),
      TS = mean(TS_F_MDS_1, na.rm = TRUE),
      .by = "DOY",
    ) |>
    dplyr::arrange(.data$DOY)

  gs <- detect_growing_season(yearly, site_info)
  gStart <- gs$gStart
  gEnd <- gs$gEnd
  tStart <- gs$tStart
  tEnd <- gs$tEnd

  good_years <- get_good_years(measured, gStart, gEnd, dt, name_site)

  measured_final <- measured |>
    dplyr::mutate(growing_year = dplyr::if_else(DOY <= 366, YEAR, YEAR - 1)) |>
    dplyr::filter(growing_year %in% good_years) |>
    dplyr::mutate(
      .data$YEAR,
      .data$MONTH,
      .data$DAY,
      .data$DOY,
      .data$HOUR,
      .data$MINUTE,
      TA = "TA_F_MDS",
      TS = "TS_F_MDS_1",
      SWC = "SWC_F_MDS_1",
      SWC = if ("SWC_F_MDS_1" %in% names(a)) SWC_F_MDS_1 else NA_real_ NEE = "NEE_VUT_REF",
      .keep = "none"
    )

  stopifnot(
    all(!is.na(measured_final[["TA"]])),
    all(!is.na(measured_final[["TS"]])),
    all(!is.na(measured_final[["NEE"]]))
  )

  iStart <- min(measured_final[["YEAR"]])
  iEnd <- max(measured_final[["YEAR"]])

  ac <- a |>
    dplyr::filter(dplyr::between(.data$YEAR, iStart, iEnd)) |>
    dplyr::mutate(
      .data$YEAR,
      .data$MONTH,
      .data$DAY,
      .data$DOY,
      .data$HOUR,
      .data$MINUTE,
      NEE = .data$NEE_VUT_REF,
      NEE_QC = .data$NEE_VUT_REF_QC,
      TA = .data$TA_F_MDS,
      TS = .data$TS_F_MDS_1,
      SWC = if ("SWC_F_MDS_1" %in% names(a)) .data$SWC_F_MDS_1 else NA_real_,
      NEE_uStar_f = .data$NEE_VUT_REF,
      daytime = .data$NIGHT != 1,
      SW_IN = if ("SW_IN_F_MDS" %in% names(a)) .data$SW_IN_F_MDS else NA_real_,
      GPP_DT = if ("GPP_DT_VUT_REF" %in% names(a)) .data$GPP_DT_VUT_REF else NA_real_,
      .keep = "none"
    )

  feature_gs <- tibble::tibble(
    site_ID = name_site,
    gStart = gStart,
    gEnd = gEnd,
    tStart = max(tStart, 0.0),
    tEnd = tEnd,
    nyear = length(good_years)
  )

  list(
    ac = ac,
    nightNEE = measured_final,
    feature_gs = feature_gs
  )

}



fix_soil_temp <- function(a, name_site) {
  # TODO: Figure out what `data` is here.
  if (!"TA_gf" %in% colnames(data)) {
    TA_gf <- data |>
      dplyr::group_by(DOY, HOUR, MINUTE) |>
      dplyr::summarise(TA_gf = mean(TA, na.rm = TRUE)) |>
      dplyr::ungroup()
    data <- data |> dplyr::left_join(TA_gf, by = c("DOY", "HOUR", "MINUTE"))
  }
  if (sum(is.na(data$TA)) > 0) {
    data$TA[is.na(data$TA)] <- data$TA_gf[is.na(data$TA)]
  }

  if ("NETRAD" %in% colnames(data)) {
    if (!"NETRAD_gf" %in% colnames(data)) {
      gf <- data |>
        dplyr::group_by(DOY, HOUR, MINUTE) |>
        dplyr::summarise(NETRAD_gf = mean(NETRAD, na.rm = TRUE)) |>
        dplyr::ungroup()
      data <- data |> left_join(gf, by = c("DOY", "HOUR", "MINUTE"))
    }
    if (sum(is.na(data$NETRAD)) > 0) {
      data$NETRAD[is.na(data$NETRAD)] <- data$NETRAD_gf[is.na(data$NETRAD)]
    }
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


predict_soil_temp <- function(data, use_NETRAD) {

  # separate data into trained or tested
  set.seed(222)
  # use a maximum of 60000 data to train and test the model; too many data will cause RF super slow and may not improve accuracy. 
  sampled <- sample(1:nrow(data), size = min(60000, nrow(data)), replace = FALSE)
  ind <- sample(2, length(sampled), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[sampled[ind==1],]        
  test  <- data[sampled[ind==2],]
  # remove NA data
  train <- na.omit(train)
  test  <- na.omit(test)
  #
  if (use_NETRAD) {
    rf <- randomForest(formula = TS ~ TA + NETRAD, data=train)  # this takes lots of time
    lm <- lm(data=data, TS ~ TA + NETRAD)
  } else {
    rf <- randomForest(formula = TS ~ TA, data=train)  # this takes lots of time
    lm <- lm(data=data, TS ~ TA)
  }
  
  y0 <- predict(rf, train)
  y1 <- predict(rf, test)
  
  print('Training data performance by random forests: ')
  print(postResample(pred=y0, obs=train$TS)) 
  
  print('Testing data performance by random forests: ')
  print(postResample(pred=y1, obs=test$TS))

  # compare with linear regression
  print('Performance by linear regression: ')
  print(summary(lm))
  # random forest is much better than lm;
  
  # do the prediction
  data$TS_pred <- predict(rf, data)  
  
  return(data[, c("TIMESTAMP", "TS_pred")])
}
