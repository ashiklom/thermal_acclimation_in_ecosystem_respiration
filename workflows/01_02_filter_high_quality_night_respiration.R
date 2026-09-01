# Prepare respiration data for temperature-respiration curve fitting.
# Unified dispatcher: processes AmeriFlux, EuroFlux, ICOS, and TERN sources.
# Outputs: {site}_ac.csv (gap-filled time series) and {site}_nightNEE.csv (filtered night NEE).
# Author: Junna Wang (original scripts), refactored into unified dispatcher.

library(librarian)
shelf(dplyr, lubridate, optparse, amerifluxr, suncalc, REddyProc, lutz, zoo)

source("R/utils.R")
source("R/respiration_helpers.R")

DIR_RAWDATA <- "data-raw"
DIR_PROC <- "data-proc/respiration"

# ---------------------------------------------------------------------------
# Internal helper: AmeriFlux-specific processing (~350 lines)
# ---------------------------------------------------------------------------
process_ameriflux_core <- function(name_site, site_info, a) {
  a[a == -9999] <- NA

  # US-Myb: remove first year, combine TS at two depths
  if (name_site == "US-Myb") {
    a <- a[17521:245424, ]
    a$TS_2_1_1[is.na(a$TS_2_1_1)] <- a$TS_2_2_1[is.na(a$TS_2_1_1)] * 0.9324892 + 0.9077066
  }

  # Day/night from suncalc
  sites <- amf_site_info()
  long_site <- sites$LOCATION_LONG[sites$SITE_ID == name_site]
  lat_site <- sites$LOCATION_LAT[sites$SITE_ID == name_site]
  tz_site <- tz_lookup_coords(lat = lat_site, lon = long_site, method = "accurate")
  tz <- tz_offset(as.Date("2000-01-01"), tz_site)

  sunrise_set <- getSunlightTimes(
    date = seq.Date(as.Date(a$TIMESTAMP[1]), as.Date(a$TIMESTAMP[nrow(a)]), by = 1),
    keep = c("sunrise", "sunset"),
    lat = lat_site, lon = long_site, tz = tz_site
  )

  # Force timezone to GMT for consistency with AmeriFlux database
  iday_sun <- which(!is.na(sunrise_set$sunrise))[1]
  sunrise_set$sunrise <- as.POSIXct(as.character(sunrise_set$sunrise), tz = "GMT") +
    difftime(sunrise_set$date[iday_sun], as_date(sunrise_set$sunset[iday_sun]))
  sunrise_set$sunset <- as.POSIXct(as.character(sunrise_set$sunset), tz = "GMT") +
    difftime(sunrise_set$date[iday_sun], as_date(sunrise_set$sunset[iday_sun]))

  southern_hemisphere <- FALSE
  if (site_info[["LAT"]] < 0) {
    a$DOY[a$DOY < 183] <- a$DOY[a$DOY < 183] + 366
    southern_hemisphere <- TRUE
  }

  ac <- a[, 1:7]
  ac$DATE <- as.Date(ac$TIMESTAMP)
  ac <- ac |> left_join(sunrise_set, by = c("DATE" = "date"))

  dt <- difftime(a$TIMESTAMP[2], a$TIMESTAMP[1], units = "hours")
  a$daytime <- a$TIMESTAMP + dt / 2.0 >= ac$sunrise & a$TIMESTAMP - dt / 2.0 <= ac$sunset

  # Arctic daylight handling
  if (name_site %in% c("US-ICt", "US-ICh", "US-ICs")) {
    a$daytime[is.na(a$daytime) & between(a$MONTH, 4, 8)] <- TRUE
    a$daytime[is.na(a$daytime) & !between(a$MONTH, 4, 8)] <- FALSE
  }

  # Site-specific year restrictions
  if (name_site == "US-CMW") {
    a <- a |> filter(YEAR > 2000)
    ac <- ac |> filter(YEAR > 2000)
  }
  if (name_site == "CA-Mer") {
    a <- a |> filter(YEAR > 1999)
    ac <- ac |> filter(YEAR > 1999)
  }

  # Site-specific TA corrections
  if (name_site == "US-Jo2") {
    a$TA[123453:124049] <- a$T_SONIC[123453:124049] * 1.0183316 - 0.9299
  }
  if (name_site == "US-BZS") {
    a_BZF <- amf_read_base(
      list.files(file.path(DIR_RAWDATA, "Ameriflux"), pattern = "US-BZF", full.names = TRUE, recursive = TRUE),
      parse_timestamp = TRUE, unzip = TRUE
    )
    a_BZF[a_BZF == -9999] <- NA
    df <- data.frame(
      BZS = a$TA_PI_F[between(a$YEAR, 2016, 2019)],
      BZF = a_BZF$TA_PI_F[between(a_BZF$YEAR, 2016, 2019)]
    )
    mod <- lm(data = df, BZS ~ BZF)
    a$TA_PI_F[between(a$YEAR, 2012, 2014)] <- predict(
      mod, newdata = data.frame(BZF = a_BZF$TA_PI_F[between(a_BZF$YEAR, 2012, 2014)])
    )
  }

  if ("DATE" %in% colnames(ac)) {
    ac <- subset(ac, select = -c(DATE, lat, lon, sunrise, sunset))
  }

  # US-Ha2: multi-tower merge
  if (name_site == "US-Ha2") {
    a$FC_A_1_1 <- a$FC_1_1_1
    a$FC_A_1_1[is.na(a$FC_A_1_1)] <- a$FC_2_1_1[is.na(a$FC_A_1_1)]
    a$TA_A_1_1 <- a$TA_1_1_1
    a$TA_A_1_1[is.na(a$TA_A_1_1)] <- a$TA_2_1_1[is.na(a$TA_A_1_1)]
    a$PPFD_IN_A_1_1 <- a$PPFD_IN_1_1_1
    a$PPFD_IN_A_1_1[is.na(a$PPFD_IN_A_1_1)] <- a$PPFD_IN_2_1_1[is.na(a$PPFD_IN_A_1_1)]
    a$RH_A_1_1 <- a$RH_1_1_1
    a$RH_A_1_1[is.na(a$RH_A_1_1)] <- a$RH_2_1_1[is.na(a$RH_A_1_1)]
    a$USTAR_A_1_1 <- a$USTAR_1_1_1
    a$USTAR_A_1_1[is.na(a$USTAR_A_1_1)] <- a$USTAR_2_1_1[is.na(a$USTAR_A_1_1)]
  }

  # NEE variable mapping
  NEEvars <- trimws(unlist(strsplit(site_info[["NEE"]], "\\+")))
  for (iNEEvar in seq_along(NEEvars)) {
    if (iNEEvar == 1) {
      ac$NEE <- a[, NEEvars[iNEEvar]]
    } else {
      ac$NEE <- ac$NEE + a[, NEEvars[iNEEvar]]
    }
  }
  if (!is.na(site_info[["FC"]])) {
    ac$NEE[is.na(a[, site_info[["FC"]]])] <- NA
  }
  if (name_site == "CA-Man") {
    ac$NEE[is.na(ac$NEE)] <- a$FC[is.na(ac$NEE)] + a$SC[is.na(ac$NEE)]
  }

  # TA
  if (!is.na(site_info[["TA"]])) {
    ac$TA <- a[, site_info[["TA"]]]
  }

  # TS: estimated or measured
  if (site_info[["estimate_Ts"]]) {
    ts_rfp_file <- list.files(
      file.path("data-proc", "soil-temperature"),
      pattern = paste0("^", name_site, "_TS_rfp\\.csv$"), full.names = TRUE, recursive = TRUE
    )
    if (length(ts_rfp_file) != 1) {
      stop(name_site, ": expected one predicted soil-temperature file from workflow 01_01, found ", length(ts_rfp_file), ".")
    }
    df_TS <- read.csv(file = ts_rfp_file)
    df_TS$TIMESTAMP <- ymd_hms(df_TS$TIMESTAMP)
    df_TS <- left_join(data.frame(TIMESTAMP = ac$TIMESTAMP), df_TS, by = "TIMESTAMP")
    ac$TS <- df_TS$TS_pred
    rm(df_TS)
  } else {
    if (!is.na(site_info[["TS"]])) {
      ac$TS <- a[, site_info[["TS"]]]
    }
    if (name_site %in% c("US-NR1", "US-ICh", "US-ICs")) {
      ac$TS[is.na(ac$TS)] <- a$TS_PI_1[is.na(ac$TS)]
    } else if (name_site == "US-Cwt") {
      ac$TS <- ac$TA * 0.64718 + 5.13873
    } else if (name_site == "US-MBP") {
      ac$TS[is.na(ac$TS)] <- ac$TA[is.na(ac$TS)] * 0.3688005 + 5.8670273
    } else if (name_site == "US-BZo") {
      mod_lm <- lm(data = ac[ac$YEAR > 2021 & ac$TA > 0, ], TS ~ TA, na.action = na.omit)
      ac$TS <- predict(mod_lm, newdata = data.frame(TA = ac$TA))
    } else if (name_site %in% c("CA-ARB", "CA-ARF", "CA-KLP", "US-Rms", "US-SRS", "US-ChR")) {
      mod_lm <- lm(data = ac[ac$TA > 0, ], TS ~ TA, na.action = na.omit)
      ac$TS <- predict(mod_lm, newdata = data.frame(TA = ac$TA))
    }
  }

  # SWC
  if (site_info[["SWC_use"]]) {
    ac$SWC <- a[, site_info[["SWC"]]]
    if (name_site == "US-NR1") {
      ac$SWC[is.na(ac$SWC)] <- a$SWC_PI_1[is.na(ac$SWC)]
    }
    x <- ac$SWC
    rle_x <- rle(is.na(x))
    min_gap <- min(rle_x$lengths[rle_x$values == TRUE])
    message("  minimum soil gaps: ", min_gap)
    if (min_gap <= 8) {
      ac$SWC <- na.approx(x, na.rm = FALSE, maxgap = 8)
    }
  } else {
    ac$SWC <- NA
  }

  # SW_IN
  ac$SW_IN <- a[, site_info[["SW_IN"]]]
  if (substr(site_info[["SW_IN"]], 1, 4) == "PPFD") {
    ac$SW_IN <- ac$SW_IN / 2.3
  }
  if (name_site == "US-Jo2") {
    ac$SW_IN[which(is.na(ac$SW_IN))] <- ac$SW_IN[which(is.na(ac$SW_IN)) + 365 * 48 * 4]
  } else if (name_site == "US-KM4") {
    ac$SW_IN[is.na(ac$SW_IN)] <- a$SW_IN_PI_F[is.na(ac$SW_IN)]
  }

  # USTAR
  ac$USTAR <- a[, site_info[["USTAR"]]]

  # RH or VPD
  if (!is.na(site_info[["RH"]])) {
    ac$RH <- a[, site_info[["RH"]]]
    convert_RH_VPD <- TRUE
  } else {
    ac$VPD <- a[, site_info[["VPD"]]]
    convert_RH_VPD <- FALSE
  }

  ac$daytime <- a$daytime

  # U-star filtering
  NEE_yearly <- ac |> group_by(DOY) |> summarise(NEE = mean(NEE, na.rm = TRUE), TS = mean(TS, na.rm = TRUE), .groups = "drop")

  # Growing season detection (AmeriFlux threshold: min(NEE)*0.2)
  gs <- detect_growing_season(
    NEE_yearly, site_info, nee_col = "NEE", ts_col = "TS",
    filter_fn = function(x) x$NEE < min(x$NEE, na.rm = TRUE) * 0.2,
    site_name = name_site
  )
  gStart <- gs$gStart
  gEnd <- gs$gEnd
  tStart <- gs$tStart
  tEnd <- gs$tEnd

  # Season start/end for REddyProc
  years <- unique(ac$YEAR)
  seasonStarts <- data.frame(DOY = rep(c(gStart, gEnd), length(years)), YEAR = rep(years, each = 2))

  ac_u <- data.frame(DateTime = ymd_hm(a$TIMESTAMP_END))
  ac_u$NEE <- ac$NEE
  ac_u$Rg <- ac$SW_IN
  ac_u$Tair <- ac$TA
  ac_u$Ustar <- ac$USTAR

  if (convert_RH_VPD) {
    ac_u$rH <- ac$RH
    ac_u$VPD <- fCalcVPDfromRHandTair(ac_u$rH, ac_u$Tair)
  } else {
    ac_u$VPD <- ac$VPD
  }

  if (dt == 1) {
    EProc <- sEddyProc$new(name_site, ac_u, c("NEE", "Rg", "Tair", "VPD", "Ustar"), DTS.n = 24)
  } else {
    EProc <- sEddyProc$new(name_site, ac_u, c("NEE", "Rg", "Tair", "VPD", "Ustar"))
  }
  EProc$sSetLocationInfo(LatDeg = lat_site, LongDeg = long_site, TimeZoneHour = tz$utc_offset_h)

  if (name_site == "US-ChR") {
    uStarTh <- EProc$sEstUstarThold()
    EProc$sMDSGapFillAfterUstar("NEE", FillAll = FALSE, isVerbose = FALSE)
    ac$NEE_uStar_f <- EProc$sExportResults()$NEE_uStar_f
    ac_u <- ac_u |> mutate(seasonYear = year(DateTime)) |>
      left_join(uStarTh[uStarTh$aggregationMode == "year", c(2, 4)], by = "seasonYear")
  } else {
    ac_u$season <- usCreateSeasonFactorYdayYear(ac_u$DateTime - 15 * 60, starts = seasonStarts)
    uStarTh <- EProc$sEstUstarThold(seasonFactor = ac_u$season)
    EProc$sMDSGapFillAfterUstar("NEE", FillAll = FALSE, isVerbose = FALSE)
    ac$NEE_uStar_f <- EProc$sExportResults()$NEE_uStar_f
    ac_u <- ac_u |> left_join(uStarTh[, 3:4], by = "season")
  }
  ac$uStarTh <- ac_u$uStar

  # Night filter after U-star
  if (site_info[["SWC_use"]]) {
    a_measure <- ac |> filter(!is.na(NEE) & !is.na(TS) & !is.na(SWC) & !is.na(USTAR)) |> filter(USTAR >= uStarTh)
  } else {
    a_measure <- ac |> filter(!is.na(NEE) & !is.na(TS) & !is.na(USTAR)) |> filter(USTAR >= uStarTh)
  }

  if (name_site %in% c("US-ICh", "US-ICt", "US-ICs")) {
    a_measure_night_complete <- a_measure |> filter(SW_IN < 10 | !daytime) |> filter(NEE > -5 & NEE < 30)
  } else {
    a_measure_night_complete <- a_measure |> filter(!daytime) |> filter(NEE > -5 & NEE < 30)
  }

  # Gap thresholds
  gap_thresholds <- compute_gap_thresholds(gStart, gEnd, name_site, "AmeriFlux_BASE")

  # Year-gap filtering
  good_years <- filter_good_years(
    a_measure_night_complete, gStart, gEnd,
    gap_thresholds$gap_max_thresh, gap_thresholds$gap_total_thresh,
    southern_hemisphere, dt, name_site
  )

  years2remove <- parse_removed_years(site_info[["year_removed"]])
  if (length(years2remove) >= 1) {
    good_years <- setdiff(good_years, years2remove)
  }

  # US-Ha1/US-GLE: min TS threshold
  if (name_site %in% c("US-Ha1", "US-GLE")) {
    tStart <- max(tStart, 2.0)
    a_measure_night_complete <- a_measure_night_complete |> filter(TS >= 2.0)
  }

  a_measure_night_complete <- a_measure_night_complete |>
    filter(YEAR %in% good_years) |>
    dplyr::select(c(YEAR, MONTH, DAY, DOY, HOUR, MINUTE, NEE, TA, TS, SWC))

  iStart <- a_measure_night_complete$YEAR[1]
  iEnd <- a_measure_night_complete$YEAR[nrow(a_measure_night_complete)]
  ac <- ac |> filter(between(YEAR, iStart, iEnd)) |>
    dplyr::select(c(YEAR, MONTH, DAY, DOY, HOUR, MINUTE, NEE_uStar_f, TA, TS, SWC, SW_IN, daytime))

  # Gap-fill TA, TS, NEE
  T_gf <- ac |> group_by(DOY, HOUR, MINUTE) |>
    summarise(TA_gf = mean(TA, na.rm = TRUE), TS_gf = mean(TS, na.rm = TRUE), NEE_gf = mean(NEE_uStar_f, na.rm = TRUE), .groups = "drop")
  for (i in 4:6) {
    na.gf <- which(is.na(T_gf[, i]))
    for (j in na.gf) {
      T_gf[j, i] <- T_gf[max(j - 24 / as.numeric(dt), 1), i]
    }
  }
  if (!"TA_gf" %in% colnames(ac)) {
    ac <- ac |> left_join(T_gf, by = c("DOY", "HOUR", "MINUTE"))
  }
  if (sum(is.na(ac$TA)) > 0) {
    ac$TA[is.na(ac$TA)] <- ac$TA_gf[is.na(ac$TA)]
  }
  if (sum(is.na(ac$TS)) > 0) {
    ac$TS[is.na(ac$TS)] <- ac$TS_gf[is.na(ac$TS)]
  }
  if (sum(is.na(ac$NEE_uStar_f)) > 0) {
    ac$NEE_uStar_f[is.na(ac$NEE_uStar_f)] <- ac$NEE_gf[is.na(ac$NEE_uStar_f)]
  }

  list(
    ac = ac,
    a_measure_night_complete = a_measure_night_complete,
    feature = data.frame(
      site_ID = name_site, gStart = gStart, gEnd = gEnd,
      tStart = max(tStart, 0.0), tEnd = tEnd, nyear = length(good_years)
    )
  )
}

# ---------------------------------------------------------------------------
# Main process_site function
# ---------------------------------------------------------------------------
process_site <- function(name_site, overwrite = FALSE) {
  site_info <- get_site_info(name_site)
  source_type <- site_info[["source"]]
  output_dir <- file.path(DIR_PROC, name_site)
  output_files <- file.path(output_dir, paste0(name_site, c("_ac.csv", "_nightNEE.csv")))

  if (!overwrite && all(file.exists(output_files))) {
    message("Skipping ", name_site, ": respiration outputs already exist.")
    return(NULL)
  }

  if (source_type == "AmeriFlux_BASE") {
    # --- AmeriFlux ---
    files_AmeriFlux_BASE <- list.files(
      file.path(DIR_RAWDATA, "Ameriflux"), pattern = "^AMF_.*_BASE.*\\.zip$", full.names = TRUE, recursive = TRUE
    )
    input_file <- files_AmeriFlux_BASE[grepl(name_site, files_AmeriFlux_BASE)]
    if (length(input_file) != 1) {
      stop(name_site, ": expected one AmeriFlux BASE zip, found ", length(input_file), ".")
    }
    a <- amf_read_base(input_file, parse_timestamp = TRUE, unzip = TRUE)
    result <- process_ameriflux_core(name_site, site_info, a)

    write_respiration_outputs(result$ac, result$a_measure_night_complete, output_dir, name_site)
    result$feature

  } else if (source_type %in% c("FLUXNET", "FLUXNET2015")) {
    # --- EuroFlux (FLUXNET shuttle) ---
    input_file <- list.files(
      file.path(DIR_RAWDATA, "FLUXNET", name_site),
      pattern = "_FLUXMET_(HH|HR)_", full.names = TRUE, recursive = TRUE
    )
    if (length(input_file) != 1) {
      stop(name_site, ": expected one fluxnet-shuttle FLUXMET_HH file, found ", length(input_file), ".")
    }
    a <- read.csv(input_file)
    a[a == -9999] <- NA
    dt <- ymd_hm(a$TIMESTAMP_START[2]) - ymd_hm(a$TIMESTAMP_START[1])
    a <- add_timestamp_columns(a, dt)

    southern_hemisphere <- FALSE
    if (site_info[["LAT"]] < 0) {
      a <- adjust_southern_hemisphere(a, site_info[["LAT"]])
      southern_hemisphere <- TRUE
    }

    # TS corrections
    if (name_site == "CZ-Stn") {
      a$TS_F_MDS_1 <- a$TS_F_MDS_2
      a$TS_F_MDS_1_QC <- a$TS_F_MDS_2_QC
    } else if (site_info[["estimate_Ts"]]) {
      ts_rfp_file <- list.files(
        file.path("data-proc", "soil-temperature"),
        pattern = paste0("^", name_site, "_TS_rfp\\.csv$"), full.names = TRUE, recursive = TRUE
      )
      if (length(ts_rfp_file) != 1) {
        stop(name_site, ": expected one predicted soil-temperature file from workflow 01_01, found ", length(ts_rfp_file), ".")
      }
      df_TS <- read.csv(file = ts_rfp_file)
      df_TS$TIMESTAMP <- ymd_hms(df_TS$TIMESTAMP)
      df_TS <- left_join(data.frame(TIMESTAMP = a$TIMESTAMP), df_TS, by = "TIMESTAMP")
      a$TS_F_MDS_1 <- df_TS$TS_pred
      a$TS_F_MDS_1_QC[is.na(a$TS_F_MDS_1_QC) | a$TS_F_MDS_1_QC == 3] <- 2
      rm(df_TS)
    } else if (name_site == "FI-Sod") {
      mod1 <- lm(data = a[1:24383, ], TS_F_MDS_2 ~ TS_F_MDS_1)
      pred2 <- predict(mod1, data.frame(TS_F_MDS_1 = a$TS_F_MDS_1[a$YEAR <= 2005]))
      mod2 <- lm(data = a[90000:245000, ], TS_F_MDS_1 ~ TS_F_MDS_2)
      a$TS_F_MDS_1[a$YEAR <= 2005] <- predict(mod2, data.frame(TS_F_MDS_2 = pred2))
      a$TS_F_MDS_1_QC[a$YEAR <= 2005] <- 2
    } else if (name_site == "GF-Guy") {
      a$TS_F_MDS_1 <- a$TA_F_MDS
      a$TS_F_MDS_1_QC <- a$TA_F_MDS_QC
    }

    # Night quality filter
    a_measure_night_complete <- a |>
      filter(!is.na(TA_F_MDS), TS_F_MDS_1_QC %in% c(0, 1, 2),
             NEE_VUT_REF_QC == 0, NIGHT == 1,
             NEE_VUT_REF > -5, NEE_VUT_REF < 30)

    # Growing season detection (EuroFlux threshold: max(min(NEE)*0.2, -0.8), FI-Sod/DE-RuC: NEE < 0.0)
    NEE_yearly <- a |> group_by(DOY) |> summarise(NEE = mean(NEE_VUT_REF, na.rm = TRUE), TS = mean(TS_F_MDS_1, na.rm = TRUE), .groups = "drop")
    filter_fn <- function(x) {
      if (name_site %in% c("FI-Sod", "DE-RuC")) {
        x$NEE < 0.0
      } else {
        x$NEE < max(min(x$NEE, na.rm = TRUE) * 0.2, -0.8)
      }
    }
    gs <- detect_growing_season(NEE_yearly, site_info, filter_fn = filter_fn, site_name = name_site)
    gStart <- gs$gStart
    gEnd <- gs$gEnd
    tStart <- gs$tStart
    tEnd <- gs$tEnd

    # CH-Dav: min TS threshold
    if (name_site == "CH-Dav") {
      tStart <- max(tStart, 2)
      a_measure_night_complete <- a_measure_night_complete |> filter(TS_F_MDS_1 >= 2.0)
    }

    # Gap thresholds
    gap_thresholds <- compute_gap_thresholds(gStart, gEnd, name_site, source_type)

    # Year-gap filtering
    good_years <- filter_good_years(
      a_measure_night_complete, gStart, gEnd,
      gap_thresholds$gap_max_thresh, gap_thresholds$gap_total_thresh,
      southern_hemisphere, dt, name_site
    )

    years2remove <- parse_removed_years(site_info[["year_removed"]])
    if (length(years2remove) >= 1) {
      good_years <- setdiff(good_years, years2remove)
    }

    # Filter and rename columns
    a_measure_night_complete <- a_measure_night_complete |>
      mutate(growing_year = case_when(DOY <= 366 ~ YEAR, TRUE ~ YEAR - 1)) |>
      filter(growing_year %in% good_years)
    a_measure_night_complete$TA <- a_measure_night_complete$TA_F_MDS
    a_measure_night_complete$TS <- a_measure_night_complete$TS_F_MDS_1
    a_measure_night_complete$SWC <- a_measure_night_complete$SWC_F_MDS_1
    a_measure_night_complete$NEE <- a_measure_night_complete$NEE_VUT_REF
    a_measure_night_complete <- a_measure_night_complete |>
      dplyr::select(c(YEAR, MONTH, DAY, DOY, HOUR, MINUTE, NEE, TA, TS, SWC))

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

    iStart <- a_measure_night_complete$YEAR[1]
    iEnd <- a_measure_night_complete$YEAR[nrow(a_measure_night_complete)]
    ac <- ac |> filter(between(YEAR, iStart, iEnd))

    write_respiration_outputs(ac, a_measure_night_complete, output_dir, name_site)
    data.frame(
      site_ID = name_site, gStart = gStart, gEnd = gEnd,
      tStart = max(tStart, 0.0), tEnd = tEnd, nyear = length(good_years)
    )

  } else if (source_type == "ICOS") {
    # --- ICOS ---
    input_file <- list.files(
      file.path(DIR_RAWDATA, "ICOS"),
      pattern = paste0(name_site, "_ICOS_L2_FLUXNET_HH\\.csv$"), full.names = TRUE, recursive = TRUE
    )
    if (length(input_file) != 1) {
      stop("Expected one normalized ICOS file for ", name_site, ", found ", length(input_file), ".")
    }
    a <- read.csv(input_file, stringsAsFactors = FALSE)
    a[a == -9999] <- NA

    dt <- as.numeric(ymd_hm(a$TIMESTAMP_START[2]) - ymd_hm(a$TIMESTAMP_START[1]), units = "hours")
    a <- add_timestamp_columns(a, dt * 1800)

    southern_hemisphere <- FALSE
    if (site_info[["LAT"]] < 0) {
      a <- adjust_southern_hemisphere(a, site_info[["LAT"]])
      southern_hemisphere <- TRUE
    }

    a_measure_night_complete <- a |>
      filter(!is.na(TA_F_MDS), TS_F_MDS_1_QC %in% c(0, 1, 2),
             NEE_VUT_REF_QC == 0, NIGHT == 1,
             NEE_VUT_REF > -5, NEE_VUT_REF < 30)
    if (nrow(a_measure_night_complete) == 0) {
      stop(name_site, " has no observations after the ICOS nighttime quality filter.")
    }
    a_measure_night_complete <- adjust_south_hemisphere_measured(a_measure_night_complete, site_info[["LAT"]])

    NEE_yearly <- a |> group_by(DOY) |> summarise(NEE = mean(NEE_VUT_REF, na.rm = TRUE), TS = mean(TS_F_MDS_1, na.rm = TRUE), .groups = "drop")
    gs <- detect_growing_season(NEE_yearly, site_info, site_name = name_site)
    gStart <- gs$gStart
    gEnd <- gs$gEnd
    tStart <- gs$tStart
    tEnd <- gs$tEnd

    gap_thresholds <- compute_gap_thresholds(gStart, gEnd, name_site, source_type)

    good_years <- filter_good_years(
      a_measure_night_complete, gStart, gEnd,
      gap_thresholds$gap_max_thresh, gap_thresholds$gap_total_thresh,
      southern_hemisphere, dt * 1800, name_site
    )
    years2remove <- parse_removed_years(site_info[["year_removed"]])
    good_years <- setdiff(good_years, years2remove)

    a_measure_night_complete <- a_measure_night_complete |>
      mutate(growing_year = if_else(DOY <= 366, YEAR, YEAR - 1)) |>
      filter(growing_year %in% good_years) |>
      transmute(YEAR, MONTH, DAY, DOY, HOUR, MINUTE,
                NEE = NEE_VUT_REF, TA = TA_F_MDS, TS = TS_F_MDS_1,
                SWC = if ("SWC_F_MDS_1" %in% names(a)) SWC_F_MDS_1 else NA_real_)

    ac <- a |>
      transmute(YEAR, MONTH, DAY, DOY, HOUR, MINUTE,
                NEE = NEE_VUT_REF, NEE_QC = NEE_VUT_REF_QC,
                TA = TA_F_MDS, TS = TS_F_MDS_1,
                SWC = if ("SWC_F_MDS_1" %in% names(a)) SWC_F_MDS_1 else NA_real_,
                NEE_uStar_f = NEE_VUT_REF, daytime = NIGHT != 1,
                SW_IN = if ("SW_IN_F_MDS" %in% names(a)) SW_IN_F_MDS else NA_real_,
                GPP_DT = NA_real_) |>
      filter(YEAR >= min(a_measure_night_complete$YEAR), YEAR <= max(a_measure_night_complete$YEAR))

    write_respiration_outputs(ac, a_measure_night_complete, output_dir, name_site)
    data.frame(
      site_ID = name_site, gStart = gStart, gEnd = gEnd,
      tStart = max(tStart, 0), tEnd = tEnd, nyear = length(good_years)
    )

  } else if (source_type == "TERN") {
    # --- TERN ---
    input_file <- list.files(
      file.path(DIR_RAWDATA, "TERN"),
      pattern = paste0(name_site, "_TERN_"), full.names = TRUE, recursive = TRUE
    )
    if (length(input_file) != 1) {
      stop("Expected one normalized TERN file for ", name_site, ", found ", length(input_file), ".")
    }
    a <- read.csv(input_file, stringsAsFactors = FALSE)
    a[a == -9999] <- NA

    dt <- as.numeric(ymd_hm(a$TIMESTAMP_START[2]) - ymd_hm(a$TIMESTAMP_START[1]), units = "hours")
    a <- add_timestamp_columns(a, dt * 1800)

    measured <- a |>
      filter(!is.na(TA_F_MDS), TS_F_MDS_1_QC %in% c(0, 1, 2), NEE_VUT_REF_QC == 0,
             NIGHT == 1, NEE_VUT_REF > -5, NEE_VUT_REF < 30)
    if (nrow(measured) == 0) {
      stop(name_site, " has no observations after the TERN nighttime quality filter.")
    }

    yearly <- a |> group_by(DOY) |> summarise(NEE = mean(NEE_VUT_REF, na.rm = TRUE), TS = mean(TS_F_MDS_1, na.rm = TRUE), .groups = "drop")
    gs <- detect_growing_season(yearly, site_info, site_name = name_site)
    gStart <- gs$gStart
    gEnd <- gs$gEnd
    tStart <- gs$tStart
    tEnd <- gs$tEnd

    good_years <- unique(measured$YEAR)
    ac <- a |> transmute(YEAR, MONTH, DAY, DOY, HOUR, MINUTE, NEE = NEE_VUT_REF,
                          NEE_QC = NEE_VUT_REF_QC, TA = TA_F_MDS, TS = TS_F_MDS_1,
                          SWC = if ("SWC_F_MDS_1" %in% names(a)) SWC_F_MDS_1 else NA_real_,
                          NEE_uStar_f = NEE_VUT_REF, daytime = NIGHT != 1,
                          SW_IN = if ("SW_IN_F_MDS" %in% names(a)) SW_IN_F_MDS else NA_real_,
                          GPP_DT = NA_real_)
    measured <- measured |>
      mutate(growing_year = if_else(DOY <= 366, YEAR, YEAR - 1)) |>
      filter(growing_year %in% good_years) |>
      transmute(YEAR, MONTH, DAY, DOY, HOUR, MINUTE, NEE = NEE_VUT_REF,
                TA = TA_F_MDS, TS = TS_F_MDS_1,
                SWC = if ("SWC_F_MDS_1" %in% names(a)) SWC_F_MDS_1 else NA_real_)

    write_respiration_outputs(ac, measured, output_dir, name_site)
    data.frame(
      site_ID = name_site, gStart = gStart, gEnd = gEnd,
      tStart = max(tStart, 0), tEnd = tEnd, nyear = length(good_years)
    )

  } else {
    warning("Unknown source type '", source_type, "' for site ", name_site, "; skipping.")
    NULL
  }
}

# ---------------------------------------------------------------------------
# CLI and site iteration
# ---------------------------------------------------------------------------
if (!interactive()) {
  option_list <- list(
    make_option("--sites", type = "character", default = NULL,
                help = "Comma-separated list of site IDs to process"),
    make_option("--overwrite", action = "store_true", default = FALSE,
                help = "Overwrite existing output files")
  )
  parser <- OptionParser(
    description = "Prepare respiration data for temperature-respiration curve fitting",
    option_list = option_list
  )
  parsed <- parse_args(parser, commandArgs(trailingOnly = TRUE))

  overwrite <- parsed$overwrite
  requested_sites <- if (!is.null(parsed$sites)) trimws(unlist(strsplit(parsed$sites, ","))) else NULL

  all_site_info <- get_site_info()
  candidate_sites <- all_site_info$site_ID
  sites <- if (is.null(requested_sites)) candidate_sites else trimws(unlist(strsplit(requested_sites, ",")))
  unknown_sites <- setdiff(sites, candidate_sites)
  if (length(unknown_sites) > 0) {
    stop("Sites not found in site_info.csv: ", paste(unknown_sites, collapse = ", "))
  }

  feature_file <- file.path("data-proc", "features", "growing_season_features.csv")
  dir.create(dirname(feature_file), recursive = TRUE, showWarnings = FALSE)
  feature_gs <- if (file.exists(feature_file)) {
    read.csv(feature_file)
  } else {
    data.frame(site_ID = character(), gStart = double(), gEnd = double(),
               tStart = double(), tEnd = double(), nyear = integer())
  }

  if (overwrite) {
    feature_gs <- feature_gs[!feature_gs$site_ID %in% sites, , drop = FALSE]
  }

  for (name_site in sites) {
    output_files <- file.path(DIR_PROC, name_site, paste0(name_site, c("_ac.csv", "_nightNEE.csv")))
    already_aggregated <- name_site %in% feature_gs$site_ID
    if (!overwrite && (all(file.exists(output_files)) || already_aggregated)) {
      message("Skipping ", name_site, ": already processed.")
      next
    }
    message("Processing site ", name_site)
    feature <- process_site(name_site, overwrite)
    if (!is.null(feature)) {
      feature_gs <- bind_rows(feature_gs, feature)
    }
  }

  write.csv(feature_gs, file = feature_file, row.names = FALSE)
}
