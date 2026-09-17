.data <- rlang::.data

prep_icos_tern_fluxnet <- function(site_info) {
  name_site <- site_info[["site_ID"]]
  if (site_info$source %in% c("FLUXNET", "FLUXNET2015")) {
    # TODO: Implement
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
  } else if (site_info$source == "ICOS") {
    file_path <- file.path(DIR_RAWDATA, "ICOS", name_site, sprintf(
      "%s_ICOS_L2_FLUXNET_HH.csv",
      name_site
    ))
  }

  a <- read.csv(file_path, stringsAsFactors = FALSE)
  a[a == -9999] <- NA

  dt <- lubridate::ymd_hm(a$TIMESTAMP_START[2]) - lubridate::ymd_hm(a$TIMESTAMP_START[1])
  a$TIMESTAMP <- lubridate::ymd_hm(a$TIMESTAMP_START) + dt / 2
  a$YEAR <- lubridate::year(a$TIMESTAMP)
  a$MONTH <- lubridate::month(a$TIMESTAMP)
  a$DAY <- lubridate::day(a$TIMESTAMP)
  a$DOY <- lubridate::yday(a$TIMESTAMP)
  a$HOUR <- lubridate::hour(a$TIMESTAMP)
  a$MINUTE <- lubridate::minute(a$TIMESTAMP)

  if (site_info[["LAT"]] < 0) {
    # TODO: Work out the southern hemisphere logic
    stop("southern hemisphere sites not implemented yet")
    a$DOY[a$DOY < 183] <- a$DOY[a$DOY < 183] + 366
  }

  if (name_site == "CZ-Stn") {
    # use TS of second layer because the first layer is incomplete
    a$TS_F_MDS_1 <- a$TS_F_MDS_2
    a$TS_F_MDS_1_QC <- a$TS_F_MDS_2_QC
  } else if (name_site == "FI-Sod") {
    # TODO: Use proper date filters here.
    mod1 <- lm(data = a[1:24383,], TS_F_MDS_2 ~ TS_F_MDS_1)
    pred2 <- predict(mod1, data.frame(TS_F_MDS_1 = a$TS_F_MDS_1[a$YEAR <= 2005]))
    mod2 <- lm(data = a[90000:245000,], TS_F_MDS_1 ~ TS_F_MDS_2)
    a$TS_F_MDS_1[a$YEAR <= 2005] <- predict(mod2, data.frame(TS_F_MDS_2 = pred2))
    a$TS_F_MDS_1_QC[a$YEAR <= 2005] <- 2
  } else if (name_site == "GF-Guy") {
    # use air temperature for this tropical site so that all tropical sites, we used bottom air temperature.
    a$TS_F_MDS_1 <- a$TA_F_MDS
    a$TS_F_MDS_1_QC <- a$TA_F_MDS_QC
  } else if (name_site %in% c("FR-Fon", "CH-Dav", "DE-Akm", "DE-Hte", "FR-Bil", "FR-Pue", "FR-FBn", "CZ-RAJ")) {
    message("Predicting soil temperature")
    ts_fit <- fix_soil_temp(a, site_info) |>
      dplyr::select("TIMESTAMP", TS_F_MDS_1 = "TS_pred")
    a <- a |>
      dplyr::select(-dplyr::any_of("TS_F_MDS_1")) |>
      dplyr::left_join(ts_fit, by = "TIMESTAMP") |>
      dplyr::mutate(TS_F_MDS_1_QC = dplyr::if_else(
        is.na(.data$TS_F_MDS_1_QC) | .data$TS_F_MDS_1_QC == 3,
        2,
        .data$TS_F_MDS_1_QC
      ))
  }

  if (name_site == "FR-Pue") {
    # FLUXNET data do not include soil data for Site FR-Pue, but site PI provided the SWC data
    stop("site-specific logic not implemented")
  }

  # Return a with some names standardized to match.
  result <- a |>
    dplyr::rename(
      NEE = "NEE_VUT_REF",
      TA = "TA_F_MDS",
      TS = "TS_F_MDS_1",
      TS_QC = "TS_F_MDS_1_QC",
      NEE_QC = "NEE_VUT_REF_QC"
    ) |>
    dplyr::mutate(
      SWC = if ("SWC_F_MDS_1" %in% names(a)) .data$SWC_F_MDS_1 else NA_real_,
      daytime = .data$NIGHT != 1,
      SW_IN = if ("SW_IN_F_MDS" %in% names(a)) .data$SW_IN_F_MDS else NA_real_,
      GPP_DT = if ("GPP_DT_VUT_REF" %in% names(a)) .data$GPP_DT_VUT_REF else NA_real_,
      NEE_uStar_f = .data$NEE
    )

  # Save dt as attribute for later use.
  attr(result, "dt") <- dt
  result

}


prep_nee_ac <- function(name_site) {
  site_info <- get_site_info(name_site)

  if (site_info$source == "AmeriFlux_BASE") {
    ac <- prep_ameriflux(site_info)
    measured <- ac |>
      dplyr::filter(
        !is.na(.data$NEE),
        !is.na(.data$TS),
        !is.na(.data$USTAR),
        .data$USTAR >= .data$uStarTh
      )

    # Only require soil water where it is actually measured. Sites with
    # `SWC_use == "NO"` carry an all-NA SWC column, so an unconditional filter
    # here would discard every observation.
    if (isTRUE(site_info$SWC_use)) {
      measured <- measured |> dplyr::filter(!is.na(.data$SWC))
    }

    keep_night <- if (name_site %in% SITES_LOW_LIGHT_NIGHT) {
      rlang::expr(.data$SW_IN < 10 | !.data$daytime)
    } else {
      rlang::expr(!.data$daytime)
    }
    measured <- measured |>
      dplyr::filter(!!keep_night, .data$NEE > -5, .data$NEE < 30) |>
      tibble::as_tibble()
  } else if (site_info$source %in% c("FLUXNET", "FLUXNET2015", "ICOS", "TERN")) {
    ac <- prep_icos_tern_fluxnet(site_info)
    measured <- ac |>
      dplyr::filter(
        !is.na(.data$TA),
        .data$TS_QC %in% c(0, 1, 2),
        # NOTE: Normally, the QC flag check should be enough to catch NA values.
        # But in some cases, NA values still slip through, so we filter
        # explicitly here as well.
        !is.na(.data$TS),
        .data$NEE_QC == 0,
        .data$NIGHT == 1,
        .data$NEE > -5,
        .data$NEE < 30
      ) |>
      tibble::as_tibble()
  } else {
    stop("Unkonwn site source: ", site_info$source)
  }

  dt <- attr(ac, "dt")
  stopifnot(!is.null(dt))

  if (nrow(measured) == 0) {
    stop(name_site, " has no observations after the nighttime quality filter.")
  }

  gs <- detect_growing_season(ac, site_info)
  gStart <- gs$gStart
  gEnd <- gs$gEnd
  tStart <- gs$tStart
  tEnd <- gs$tEnd

  good_years <- get_good_years(measured, gStart, gEnd, dt, name_site)

  measured_final <- measured |>
    dplyr::mutate(growing_year = dplyr::if_else(.data$DOY <= 366, .data$YEAR, .data$YEAR - 1)) |>
    dplyr::filter(.data$growing_year %in% good_years) |>
    dplyr::select(
      "YEAR", "MONTH", "DAY", "DOY", "HOUR", "MINUTE",
      "NEE", "TA", "TS", "SWC"
    )

  stopifnot(
    all(!is.na(measured_final[["TS"]])),
    all(!is.na(measured_final[["NEE"]]))
  )

  iStart <- min(measured_final[["YEAR"]])
  iEnd <- max(measured_final[["YEAR"]])

  ac_required <- c(
    "YEAR", "MONTH", "DAY", "DOY", "HOUR", "MINUTE",
    "NEE", "NEE_uStar_f", "TA", "TS", "SWC", "SW_IN", "daytime"
  )
  ac_optional <- c("NEE_QC", "GPP_DT")

  ac_final <- ac |>
    dplyr::filter(dplyr::between(.data$YEAR, iStart, iEnd)) |>
    dplyr::select(dplyr::all_of(ac_required), dplyr::any_of(ac_optional))

  feature_gs <- tibble::tibble(
    site_ID = name_site,
    gStart = gStart,
    gEnd = gEnd,
    tStart = max(tStart, 0.0),
    tEnd = tEnd,
    nyear = length(good_years)
  )

  list(
    ac = ac_final,
    nightNEE = measured_final,
    feature_gs = feature_gs
  )

}
