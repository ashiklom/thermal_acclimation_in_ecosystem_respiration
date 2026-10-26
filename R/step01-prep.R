# Workflow step 01: prepare each site's data for the respiration fits
# (Junna Wang, 2025).
#
# estimate_ts_site() is the per-site body of 01_01 (soil temperature
# estimated from air temperature, and net radiation where available, at the
# 16 sites with estimate_Ts == "YES"); it returns the table written to
# TS_RandomForest/<site>_TS_rfp.csv. prep_site_euroflux() and
# prep_site_ameriflux() are the per-site bodies of 01_02a (FLUXNET/ICOS
# sites) and 01_02b (AmeriFlux BASE sites); each returns list(ac = <the
# site's half-hourly table>, night = <its filtered nighttime table>,
# feature = <its row of the growing-season features>, climatology = <the
# half-hourly DOY, TA and TS from which step 01 takes tStart/tEnd, over the
# whole record, with `season` marking the days of year whose mean NEE marks
# the growing season>, ts_base and ts_params = <the record and settings
# that do not depend on soil temperature, from which ts_column_tables() in
# R/ts-qualify.R makes the other four, as it does here from the site's soil
# temperature (climatology$TS), usable where ts_ok>). `site` is the site's
# row of data/site_info.csv; `base_dir` holds the raw site data (R/read-raw.R);
# `ts_rf` is the site's 01_01 table (needed where soil temperature is
# estimated); `coordinates` holds the AmeriFlux sites' coordinates
# (data/ameriflux_coordinates.csv).

#' Combine the sites' growing-season features
#'
#' The growing-season feature table: per-site rows bound in site order onto the typed empty
#' table the loops started from.
#'
#' @param rows The sites' feature rows (the `feature` of each site's step-01 list), in site
#'   order.
#' @return A data frame: site_ID, gStart, gEnd, tStart, tEnd, nyear.
#' @noRd
combine_features <- function(rows) {
  feature_gs <- data.frame(site_ID=character(), gStart=double(), gEnd=double(), tStart=double(), tEnd=double(), nyear=integer())  # growing season feature
  for (row in rows) feature_gs <- dplyr::bind_rows(feature_gs, row)
  feature_gs
}

#' Predict soil temperature with a random forest
#'
#' A function to predict soil temperature (01_01): a random forest of TS on TA, and NETRAD if
#' `use_NETRAD`, trained on 70% of a sample of at most 60,000 records (seed 222) and tested on
#' the rest. It prints the forest's training and testing performance and, for comparison, a
#' linear regression fitted to all of `data`, then predicts every row.
#'
#' @param data A data frame with TIMESTAMP, TS, TA and, if used, NETRAD (the inputs: TS, TA
#'   and the optional NETRAD).
#' @param use_NETRAD Whether net radiation is a predictor.
#' @return A data frame of TIMESTAMP and the predicted TS_pred, one row per row of `data`.
#' @noRd
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
    rf <- randomForest::randomForest(formula = TS ~ TA + NETRAD, data=train)  # this takes lots of time
    lm <- lm(data=data, TS ~ TA + NETRAD)
  } else {
    rf <- randomForest::randomForest(formula = TS ~ TA, data=train)  # this takes lots of time
    lm <- lm(data=data, TS ~ TA)
  }

  y0 <- predict(rf, train)
  y1 <- predict(rf, test)

  print('Training data performance by random forests: ')
  print(caret::postResample(pred=y0, obs=train$TS))

  print('Testing data performance by random forests: ')
  print(caret::postResample(pred=y1, obs=test$TS))

  # compare with linear regression
  print('Performance by linear regression: ')
  print(summary(lm))
  # random forest is much better than lm;

  # do the prediction
  data$TS_pred <- predict(rf, data)

  return(data[, c("TIMESTAMP", "TS_pred")])
}

#' Estimate a site's soil temperature (01_01)
#'
#' The per-site body of 01_01: soil temperature estimated from air temperature, and net
#' radiation where available, at the 16 sites with estimate_Ts == "YES". Air temperature and
#' net radiation are gap-filled with their day-of-year x time-of-day means; soil temperature is
#' then predicted by a random forest (predict_soil_temp()) at the sites with net radiation, by
#' lm(TS ~ TA + NETRAD) at DE-Hte and FR-FBn (one or two years of soil temperature, incomplete
#' net radiation), and by lm(TS ~ TA) fitted where TA > 0 elsewhere.
#'
#' @param site The site's row of data/site_info.csv.
#' @param base_dir Root of the raw site data (R/read-raw.R).
#' @return The table written to TS_RandomForest/<site>_TS_rfp.csv: TIMESTAMP and TS_pred at the
#'   random-forest sites; the site's record (TIMESTAMP, YEAR, DOY, HOUR, MINUTE, TS, TA, ...)
#'   with TS_pred added at the others.
#' @noRd
estimate_ts_site <- function(site, base_dir = path_original("SiteData")) {
  name_site <- site$site_ID

  if (site$source %in% c("AmeriFlux_BASE", "AmeriFlux_FLUXNET")) {
    a <- read_ameriflux_base(name_site, base_dir)
    a <- a[order(a$TIMESTAMP), ]
    data <- data.frame(TIMESTAMP = a$TIMESTAMP, YEAR = a$YEAR, DOY = a$DOY, HOUR = a$HOUR, MINUTE = a$MINUTE,
                       TS = a[, trimws(site$TS)], TA = a[, trimws(site$TA)])
    if (name_site %in% c('US-Los', "US-Ced")) {
      data$NETRAD <- a$NETRAD_1_1_1
    } else if (name_site %in% c("US-SRG", "CA-Man")) {
      data$NETRAD <- a$NETRAD
    } else if (name_site %in% c("US-Ho1")) {
      data$NETRAD <- a$NETRAD_2_1_1
    }
  } else {
    a <- read_fluxnet_record(name_site, site$source, base_dir)
    # -9999 means missing information.
    a[a==-9999] <- NA

    dt <- lubridate::ymd_hm(a$TIMESTAMP_START[2]) - lubridate::ymd_hm(a$TIMESTAMP_START[1])
    a$TIMESTAMP  <- lubridate::ymd_hm(a$TIMESTAMP_START) + dt / 2
    a$DOY <- lubridate::yday(a$TIMESTAMP)
    data <- data.frame(TIMESTAMP = a$TIMESTAMP, YEAR = lubridate::year(a$TIMESTAMP), DOY = a$DOY,
                       HOUR = lubridate::hour(a$TIMESTAMP), MINUTE = lubridate::minute(a$TIMESTAMP), TS = a$TS_F_MDS_1, TA = a$TA_F_MDS, NETRAD=a$NETRAD)
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


  # gap fill TA use DOY method if needed.
  if (!"TA_gf" %in% colnames(data)) {
    TA_gf <- data |> dplyr::group_by(.data$DOY, .data$HOUR, .data$MINUTE) |> dplyr::summarise(TA_gf=mean(.data$TA, na.rm=T))
    data   <- data |> dplyr::left_join(TA_gf, by=c("DOY", "HOUR", "MINUTE"))
  }
  if (sum(is.na(data$TA)) > 0) {
    data$TA[is.na(data$TA)] <- data$TA_gf[is.na(data$TA)]
  }

  # gap fill NETRAD if using NETRAD method
  if ("NETRAD" %in% names(data)) {
    if (!"NETRAD_gf" %in% colnames(data)) {
      gf <- data |> dplyr::group_by(.data$DOY, .data$HOUR, .data$MINUTE) |> dplyr::summarise(NETRAD_gf=mean(.data$NETRAD, na.rm=T))
      data   <- data |> dplyr::left_join(gf, by=c("DOY", "HOUR", "MINUTE"))
    }
    if (sum(is.na(data$NETRAD)) > 0) {
      data$NETRAD[is.na(data$NETRAD)] <- data$NETRAD_gf[is.na(data$NETRAD)]
    }
  }

  if (name_site %in% c("DE-Akm", "FR-Pue", "US-Los", "US-SRG", "CA-Man", "US-Ced", "US-Ho1", "CZ-RAJ")) {
    # use NETRAD because it is available
    use_NETRAD = TRUE
    data <- predict_soil_temp(data, use_NETRAD)
  } else if (name_site %in% c("DE-Hte", "FR-FBn")) {
    # use simple linear regression because these sites have 1-year or two-year TS, and NETRAD is incomplete
    lm <- lm(data=data, TS ~ TA + NETRAD)
    data$TS_pred <- predict(lm, data)
  } else {
    # no netrad at this site
    lm <- lm(data=data[data$TA>0, ], TS ~ TA)
    data$TS_pred <- predict(lm, data)
  }

  data
}

#' Prepare a FLUXNET-format site's step-01 tables (01_02a)
#'
#' The per-site body of 01_02a (FLUXNET/ICOS sites). Reads and combines the site's record and
#' takes soil temperature from TS_F_MDS_1, with site-specific choices (the second layer at
#' CZ-Stn; the 01_01 estimate `ts_rf` at FR-Fon, CH-Dav, DE-Akm, DE-Hte, FR-Bil, FR-Pue, FR-FBn
#' and CZ-RAJ; the first years rebuilt from the second profile at FI-Sod; air temperature at
#' GF-Guy) and the PI soil water at FR-Pue. It wraps the day of year at the southern-hemisphere
#' sites, applies the nighttime filters that do not involve soil temperature, finds the
#' NEE-uptake days and the growing-season end, and passes the record to ts_column_tables()
#' (R/ts-qualify.R).
#'
#' @param site The site's row of data/site_info.csv.
#' @param ts_rf The site's 01_01 table (needed where soil temperature is estimated), or NULL.
#' @param base_dir Root of the raw site data (R/read-raw.R).
#' @return list(ac = <the site's half-hourly table>, night = <its filtered nighttime table>,
#'   feature = <its row of the growing-season features>, climatology = <the half-hourly DOY,
#'   TA and TS from which step 01 takes tStart/tEnd, over the whole record, with `season`
#'   marking the days of year whose mean NEE marks the growing season>, ts_base and ts_params =
#'   <the record and settings that do not depend on soil temperature, from which
#'   ts_column_tables() makes the other four, as it does here from the site's soil
#'   temperature>, ts_ok = <where that soil temperature is usable: quality flag 0-2>).
#' @noRd
prep_site_euroflux <- function(site, ts_rf = NULL, base_dir = path_original("SiteData")) {
  data_source <- site$source
  name_site <- site$site_ID

  # connecting multiple data sources
  a <- read_fluxnet_record(name_site, data_source, base_dir)
  # soil drivers a spliced record took from later products (R/read-raw.R)
  filled <- attr(a, "filled")
  # -9999 means missing information.
  a[a==-9999] <- NA
  # products with only the constant-u* (CUT) NEE (e.g. TERN's ONEFlux runs):
  # NEE and daytime GPP from that
  nee_cut <- !"NEE_VUT_REF" %in% names(a)
  if (nee_cut) {
    if (!"NEE_CUT_REF" %in% names(a)) stop(name_site, ": the record has neither NEE_VUT_REF nor NEE_CUT_REF", call. = FALSE)
    a$NEE_VUT_REF <- a$NEE_CUT_REF
    a$NEE_VUT_REF_QC <- a$NEE_CUT_REF_QC
    a$GPP_DT_VUT_REF <- a$GPP_DT_CUT_REF
  }

  # add more columns
  dt <- lubridate::ymd_hm(a$TIMESTAMP_START[2]) - lubridate::ymd_hm(a$TIMESTAMP_START[1])
  a$TIMESTAMP  <- lubridate::ymd_hm(a$TIMESTAMP_START) + dt / 2
  a$YEAR  <- lubridate::year(a$TIMESTAMP)
  a$MONTH <- lubridate::month(a$TIMESTAMP)
  a$DAY   <- lubridate::day(a$TIMESTAMP)
  a$DOY   <- lubridate::yday(a$TIMESTAMP)
  a$HOUR  <- lubridate::hour(a$TIMESTAMP)
  a$MINUTE <- lubridate::minute(a$TIMESTAMP)

  # read soil temperature data, or use specific soil temperature data for some sites.
  if (name_site %in% c('CZ-Stn')) {
    a$TS_F_MDS_1 <- a$TS_F_MDS_2  # use TS of second layer because the first layer is incomplete
    a$TS_F_MDS_1_QC <- a$TS_F_MDS_2_QC
  } else if (name_site %in% c("FR-Fon", "CH-Dav", "DE-Akm", "DE-Hte", "FR-Bil", "FR-Pue", "FR-FBn", "CZ-RAJ")) {
    stopifnot(!is.null(ts_rf))
    df_TS <- ts_rf
    if (!inherits(df_TS$TIMESTAMP, "POSIXct")) df_TS$TIMESTAMP <- lubridate::ymd_hms(df_TS$TIMESTAMP)
    df_TS <- dplyr::left_join(data.frame(TIMESTAMP=a$TIMESTAMP), df_TS, by = "TIMESTAMP")
    a$TS_F_MDS_1 <- df_TS$TS_pred
    a$TS_F_MDS_1_QC[is.na(a$TS_F_MDS_1_QC) | a$TS_F_MDS_1_QC == 3] <- 2
    rm(df_TS)
  } else if (name_site == "FI-Sod") {
    # the first 5 year TS measurements was off, likely because sensor changed locations.
    # in 2001, measurements in soil profile 2 are correct, so building relations between profiles 1 and 2
    # (2001-01-01 to 2002-05-23, rows 1:24383 of the FLUXNET2015 record)
    mod1 <- lm(data = a[a$TIMESTAMP_START <= 200205232300, ], TS_F_MDS_2 ~ TS_F_MDS_1)
    pred2 <- predict(mod1, data.frame(TS_F_MDS_1 = a$TS_F_MDS_1[a$YEAR <= 2005]))
    # (2006-02-18 23:30 to 2014-12-23 03:30, rows 90000:245000 of the FLUXNET2015 record)
    mod2 <- lm(data = a[a$TIMESTAMP_START >= 200602182330 & a$TIMESTAMP_START <= 201412230330, ], TS_F_MDS_1 ~ TS_F_MDS_2)
    a$TS_F_MDS_1[a$YEAR <= 2005] <- predict(mod2, data.frame(TS_F_MDS_2 = pred2))
    a$TS_F_MDS_1_QC[a$YEAR <= 2005] <- 2
  } else if (name_site == "GF-Guy") {
    # use air temperature for this tropical site so that all tropical sites, we used bottom air temperature.
    a$TS_F_MDS_1 <- a$TA_F_MDS
    a$TS_F_MDS_1_QC <- a$TA_F_MDS_QC
  }

  # FLUXNET data do not include soil data for Site FR-Pue, but site PI provided the SWC data
  if (name_site == 'FR-Pue') {
    swc_2003_2015 <- read.table(file.path(base_dir, "soil_water_content_FR-Pue", "FR-Pue_SWC_Control_Corrected_2003-2015.csv"), sep=';', header=T)
    swc_2016_2020 <- read.table(file.path(base_dir, "soil_water_content_FR-Pue", "FR-Pue_SWC_Control_Corrected_2016-2020.csv"), sep=';', header=T)
    swc_2003_2020 <- dplyr::bind_rows(swc_2003_2015, swc_2016_2020)
    swc_2003_2020$TIMESTAMP_START <- lubridate::dmy_hm(swc_2003_2020$date_time)

    a <- a |> dplyr::mutate(TIMESTAMP_START = lubridate::ymd_hm(a$TIMESTAMP_START)) |>
    dplyr::left_join(swc_2003_2020[, c("TIMESTAMP_START", "SWC_Mean")], by="TIMESTAMP_START")
    # times 100 to convert SWC into percentage
    a$SWC_Mean <- a$SWC_Mean*100
    a$SWC_F_MDS_1[!is.na(a$SWC_Mean)] <- a$SWC_Mean[!is.na(a$SWC_Mean)]
    a$SWC_F_MDS_1_QC[!is.na(a$SWC_Mean)] <- 1
  }

  # Southern Hemisphere, so we need to change DOY values (the site table's
  # season_wrap: the growing season spans New Year)
  southern_hemisphere = FALSE
  if (!isTRUE(site$season_wrap) && !isFALSE(site$season_wrap)) stop(name_site, ": season_wrap must be TRUE or FALSE", call. = FALSE)
  if (site$season_wrap) {
    a$DOY[a$DOY < 183] <- a$DOY[a$DOY < 183] + 366
    southern_hemisphere = TRUE
  }
  #
  # check if SWC was missed.
  SWC <- TRUE
  if (!'SWC_F_MDS_1' %in% colnames(a)) {
    a$SWC_F_MDS_1 <- NA
    SWC <- FALSE
  }
  if (site$SWC_use == 'NO') {
    SWC <- FALSE
  }
  #
  # we decided to relax the SWC constrains: the nighttime filters that do
  # not involve soil temperature (whose own filter, a quality flag of 0-2, is
  # in ts_column_tables() below)
  night_ok <- !is.na(a$TA_F_MDS) & a$NEE_VUT_REF_QC %in% 0 & a$NIGHT %in% 1 & a$NEE_VUT_REF > -5 & a$NEE_VUT_REF < 30
  night_ok[is.na(night_ok)] <- FALSE
  # |> dplyr::filter(SWC_F_MDS_1_QC %in% c(0, 1, 2))

  # decide which years data to exclude: rule 1: maximum data gaps during growing season < 1 month; rule2: large disturbance years due to fire, insect breakout
  # estimate growing season DOY
  NEE_yearly  <- a |> dplyr::group_by(.data$DOY) |> dplyr::summarise(NEE=mean(.data$NEE_VUT_REF, na.rm=T)) |>
    dplyr::arrange(.data$DOY)

  # I will use the mean of the first three values, and the mean of the last three values
  if (name_site %in% c('FI-Sod', 'DE-RuC')) {
    tmp <- NEE_yearly |> dplyr::filter(.data$NEE < 0.0)
  } else {
    tmp <- NEE_yearly |> dplyr::filter(.data$NEE < max(min(NEE_yearly$NEE, na.rm=T) * 0.2, -0.8))
  }

  gEnd   <- as.integer(mean(tmp$DOY[(nrow(tmp)-6)])) + 4
  if (!is.na(site$gEnd)) {
    gEnd <- as.numeric(site$gEnd)
  }

  # years to remove due to PI reported disturbances and bad data quality
  year_str <- site$year_removed
  year_parts <- strsplit(year_str, ",")[[1]]
  years2remove <- unlist(lapply(year_parts, function(part) {
    if (grepl(":", part)) {
      rng <- as.numeric(strsplit(part, ":")[[1]])
      seq(rng[1], rng[2])
    } else {
      as.numeric(part)
    }
  }))

  ts_base <- data.frame(TIMESTAMP = a$TIMESTAMP, YEAR = a$YEAR, MONTH = a$MONTH, DAY = a$DAY, DOY = a$DOY, HOUR = a$HOUR, MINUTE = a$MINUTE,
                        NEE = a$NEE_VUT_REF, NEE_QC = a$NEE_VUT_REF_QC, TA = a$TA_F_MDS, SWC = a$SWC_F_MDS_1,
                        NEE_uStar_f = a$NEE_VUT_REF, daytime = a$NIGHT != 1, SW_IN = a$SW_IN_F_MDS)
  ts_base$GPP_DT <- a$GPP_DT_VUT_REF
  ts_base$night_ok <- night_ok
  ts_params <- list(
    site_ID = name_site, source = "fluxnet", southern_hemisphere = southern_hemisphere,
    dt_secs = as.numeric(dt, units = "secs"), uptake_doy = tmp$DOY,
    # In general, growing-season temperature should be > 0 C (ts_season())
    gStart_nee = as.integer(mean(tmp$DOY[7])) - 4, gStart_site = site$gStart, gEnd = gEnd,
    # some sites NEE measurements under TS < 2C was inaccurate, so for these sites tStart should be greater than 2C
    tstart_floor = if (name_site %in% c('CH-Dav')) 2 else NA, floor_before_gaps = TRUE,
    # use larger gap threshold because lack of data at these Tropical and Tundra sites.
    gap_fixed = if (name_site %in% c('ZA-Kru', 'FI-Sod', 'GF-Guy')) c(60, 0.7) else NULL,
    years_removed = years2remove
  )
  if (!is.null(filled) && nrow(filled)) ts_params$filled <- filled
  if (nee_cut) ts_params$nee_column <- "NEE_CUT_REF"
  ts_ok <- a$TS_F_MDS_1_QC %in% c(0, 1, 2)
  out <- ts_column_tables(ts_base, ts_params, a$TS_F_MDS_1, ts_ok)
  list(ac = out$ac, night = out$night, feature = out$feature, climatology = out$climatology, ts_base = ts_base, ts_params = ts_params, ts_ok = ts_ok)
}

#' Prepare an AmeriFlux BASE site's step-01 tables (01_02b)
#'
#' The per-site body of 01_02b (AmeriFlux BASE sites). Reads the site's record, labels each
#' record day or night from sunrise and sunset at the site's coordinates in its standard time
#' (sun_times_published()), applies site-specific repairs and the variables chosen in
#' data/site_info.csv (NEE, FC, TA, TS, SWC, SW_IN, USTAR, RH or VPD), estimates u* thresholds
#' and gap-fills NEE with REddyProc, applies the nighttime filters that do not involve soil
#' temperature, and passes the record to ts_column_tables() (R/ts-qualify.R).
#'
#' @param site The site's row of data/site_info.csv.
#' @param ts_rf The site's 01_01 table (needed where soil temperature is estimated), or NULL.
#' @param coordinates The AmeriFlux sites' coordinates as used for the published outputs
#'   (data/ameriflux_coordinates.csv: site_ID, LAT, LONG).
#' @param base_dir Root of the raw site data (R/read-raw.R).
#' @return The same list as prep_site_euroflux(); ts_ok is where soil temperature is present.
#' @noRd
prep_site_ameriflux <- function(site, ts_rf = NULL, coordinates = utils::read.csv(file.path("data", "ameriflux_coordinates.csv")),
                                base_dir = path_original("SiteData")) {
  data_source <- site$source
  name_site <- site$site_ID

  # read data
  a <- read_ameriflux_base(name_site, base_dir)
  a <- a[order(a$TIMESTAMP), ]
  rownames(a) <- NULL
  a[a==-9999] <- NA
  if (name_site == "US-Myb") {
    # use data from the second year, because lots of missing NEE in the first year.
    # (rows 17521:245424 of the BASE record: 2011-2023; the record now runs
    # through 2024, which the published row range left out)
    a <- a[dplyr::between(a$YEAR, 2011, 2023), ]
    # combine TS data at two depth
    a$TS_2_1_1[is.na(a$TS_2_1_1)] <- a$TS_2_2_1[is.na(a$TS_2_1_1)] * 0.9324892 + 0.9077066
  }

  ######################################decide day or night for every TIMESTAMP#######################################
  # get site long and lat
  # site coordinates as used for the published outputs (see
  # scripts/snapshot-ameriflux-coordinates.R), not the live AmeriFlux API
  long_site  <- coordinates$LONG[coordinates$site_ID==name_site]
  lat_site   <- coordinates$LAT[coordinates$site_ID==name_site]
  stopifnot(length(long_site) == 1, length(lat_site) == 1)

  # get the exact time zone, not using daylight saving so choose a winter date
  tz <- lutz::tz_offset(as.Date("2000-01-01"), lutz::tz_lookup_coords(lat=lat_site, lon=long_site, method='accurate'))
  # Fixed-offset zone for the site's standard time. 'Etc/GMT+N' is the tz
  # database name of the POSIX 'GMT+N' zone (N hours *west* of UTC), which
  # current lubridate/timechange no longer accepts.
  stopifnot(tz$utc_offset_h == round(tz$utc_offset_h))
  if (tz$utc_offset_h > 0) {
    tz_site  <- paste0('Etc/GMT-', tz$utc_offset_h)
  } else {
    tz_site  <- paste0('Etc/GMT+', -tz$utc_offset_h)
  }

  # get sunrise and sunset time, choosing each date's solar day the way the
  # published outputs did (see R/sun-times.R)
  sunrise_set <- sun_times_published(
    date = seq.Date(as.Date(min(a$TIMESTAMP)), as.Date(max(a$TIMESTAMP)), by = 1),       # Date format: YYYY-MM-DD
    lat = lat_site,
    lon = long_site,
    tz  = tz_site
  )

  # Force the time zone to GMT in order to be consistent with the date format in AmeriFlux database
  # Find a day with estimated sunrise and sunset
  iday_sun <- which(!is.na(sunrise_set$sunrise))[1]
  # ensure each row represents the same date
  sunrise_set$sunrise <- as.POSIXct(as.character(sunrise_set$sunrise), tz='GMT') + difftime(sunrise_set$date[iday_sun], lubridate::as_date(sunrise_set$sunset[iday_sun]))
  sunrise_set$sunset <- as.POSIXct(as.character(sunrise_set$sunset), tz='GMT') + difftime(sunrise_set$date[iday_sun], lubridate::as_date(sunrise_set$sunset[iday_sun]))

  # day of year is never shifted for AmeriFlux sites: the original script's
  # list of sites whose growing season wraps the year end was empty here
  # (prep_site_euroflux() shifted AU-Tum and ZA-Kru; its list's BR-Sa1 is an
  # AmeriFlux site, so was not shifted either, as in the original scripts)
  if (!isFALSE(site$season_wrap)) stop(name_site, ": season_wrap must be FALSE at AmeriFlux BASE sites (not implemented)", call. = FALSE)
  southern_hemisphere = FALSE

  # create ancillary df
  ac <- a[, c("YEAR", "MONTH", "DAY", "DOY", "HOUR", "MINUTE", "TIMESTAMP")]
  ac$DATE <- as.Date(ac$TIMESTAMP)
  ac <- ac |> dplyr::left_join(sunrise_set, by = c('DATE' = 'date'))

  # determine: day-time or night-time
  dt <- difftime(a$TIMESTAMP[2], a$TIMESTAMP[1], units = "hours")
  a$daytime <- a$TIMESTAMP + dt/2.0 >= ac$sunrise & a$TIMESTAMP - dt/2.0 <= ac$sunset

  # special cases: sites in Arctic do not have sunrise and sunset sometime of a year
  if (name_site %in% c('US-ICt', 'US-ICh', "US-ICs")) {
    a$daytime[is.na(a$daytime) & dplyr::between(a$MONTH, 4, 8)] <- TRUE
    a$daytime[is.na(a$daytime) & !dplyr::between(a$MONTH, 4, 8)] <- FALSE
  }

  if (name_site == "US-CMW") {
    # only use data after 2000
    a <- a |> dplyr::filter(.data$YEAR > 2000)
    ac <- ac |> dplyr::filter(.data$YEAR > 2000)
  }
  if (name_site == "CA-Mer") {
    # only use data after 1999, the first two-year data have some problems.
    a <- a |> dplyr::filter(.data$YEAR > 1999)
    ac <- ac |> dplyr::filter(.data$YEAR > 1999)
  }
  if (name_site == "US-Jo2") {
    # TA data from 123453~124049 has problems use Tsonic data
    # (rows 123453:124049: 2017-01-15 22:15 to 2017-01-28 08:15)
    bad_ta <- a$TIMESTAMP >= as.POSIXct("2017-01-15 22:15:00", tz = "GMT") & a$TIMESTAMP <= as.POSIXct("2017-01-28 08:15:00", tz = "GMT")
    a$TA[bad_ta] <- a$T_SONIC[bad_ta] * 1.0183316 - 0.9299
  }
  if (name_site == "US-BZS") {
    # TA data in 2012-2014 is not accurate, so use nearby US-BZF's TA data.
    a_BZF <- read_ameriflux_base('US-BZF', base_dir)
    a_BZF[a_BZF==-9999] <- NA
    # pair the two towers' half-hours by timestamp
    BZF_TA <- a_BZF$TA_PI_F[match(a$TIMESTAMP, a_BZF$TIMESTAMP)]
    df <- data.frame(BZS = a$TA_PI_F, BZF = BZF_TA)[dplyr::between(a$YEAR, 2016, 2019), ]
    mod <- lm(data=df, BZS ~ BZF)
    replace_ta <- dplyr::between(a$YEAR, 2012, 2014)
    a$TA_PI_F[replace_ta] <- predict(mod, newdata = data.frame(BZF = BZF_TA[replace_ta]))
  }
  ######################################decide which variables to use#######################################
  # modify the dataframe ac
  if ("DATE" %in% colnames(ac)) {
    ac = subset(ac, select=-c(DATE, lat, lon, sunrise, sunset))
  }

  if (name_site == 'US-Ha2') {
  # combine data from two locations
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

  NEEvars <- trimws(unlist(strsplit(site$NEE, "\\+")))
  for (iNEEvar in 1:length(NEEvars)) {
    if (iNEEvar == 1) {
      ac$NEE <- a[, NEEvars[iNEEvar]]
    } else {
      ac$NEE <- ac$NEE + a[, NEEvars[iNEEvar]]
    }
  }

  if (site$FC != "") {
    # remove the NEE data without FC measurements
    ac$NEE[is.na(a[, trimws(site$FC)])]  <- NA
  }

  # combine NEE time series and FC time series; because either one is incomplete
  if (name_site == "CA-Man") {
    ac$NEE[is.na(ac$NEE)] <- a$FC[is.na(ac$NEE)] + a$SC[is.na(ac$NEE)]
  }

  # air temperature
  if (site$TA != "") {
    ac$TA  <- a[, trimws(site$TA)]
  }

  # soil temperature TS
  if (site$estimate_Ts == 'YES') {
    stopifnot(!is.null(ts_rf))
    df_TS <- ts_rf
    if (!inherits(df_TS$TIMESTAMP, "POSIXct")) df_TS$TIMESTAMP <- lubridate::ymd_hms(df_TS$TIMESTAMP)
    df_TS <- dplyr::left_join(data.frame(TIMESTAMP=ac$TIMESTAMP), df_TS, by = "TIMESTAMP")
    ac$TS <- df_TS$TS_pred
    rm(df_TS)
  } else {
    if (site$TS != "") {
      ac$TS  <- a[, trimws(site$TS)]
    }
    # deal with special cases
    if (name_site %in% c('US-NR1', 'US-ICh', "US-ICs")) {
      # use PI gap-filled data (where the release has it: later US-ICs
      # releases dropped TS_PI_1)
      if ("TS_PI_1" %in% names(a)) {
        ac$TS[is.na(ac$TS)] <- a$TS_PI_1[is.na(ac$TS)]
      } else {
        message(name_site, ": no TS_PI_1 in this release; sensor gaps not filled")
      }
    } else if (name_site == "US-Cwt") {
      # this site has no TS measurements, so we used TS-TA relationships from nearby US-xGB of the same DBF category.
      ac$TS  <- ac$TA * 0.64718 + 5.13873
    } else if (name_site == "US-MBP") {
      # this site only missed a few TS data, so only estimate these missing data.
      ac$TS[is.na(ac$TS)] <- ac$TA[is.na(ac$TS)] * 0.3688005 + 5.8670273
    } else if (name_site == "US-BZo") {
      # recent data is more accurate
      mod_lm <- lm(data = ac[ac$YEAR > 2021 & ac$TA > 0, ], TS ~ TA, na.action = na.omit)
      ac$TS <- predict(mod_lm, newdata = data.frame(TA = ac$TA))
    } else if (name_site %in% c("CA-ARB", "CA-ARF", "CA-KLP", "US-Rms", "US-SRS", "US-ChR")) {
      # cold area, use TA above 0 for growing season
      mod_lm <- lm(data = ac[ac$TA > 0, ], TS ~ TA, na.action = na.omit)
      ac$TS <- predict(mod_lm, newdata = data.frame(TA = ac$TA))
    }
  }

  # soil water SWC
  if (site$SWC_use == 'YES') {
    ac$SWC <- a[, trimws(site$SWC)]
    # deal with special cases
    if (name_site == 'US-NR1') {
      # use PI gap-filled data
      ac$SWC[is.na(ac$SWC)] <- a$SWC_PI_1[is.na(ac$SWC)]
    }
    # interpolate SWC data if necessary because TS sensor has different frequency with other data
    x <- ac$SWC
    rle_x <- rle(is.na(x))
    min_gap <- min(rle_x$lengths[rle_x$values == TRUE])
    print(paste0('minimum soil gaps: ', min_gap))
    if (min_gap <= 8) {
      # Perform interpolation on eligible gaps only
      ac$SWC <- zoo::na.approx(x, na.rm = FALSE, maxgap = 8)
    }
  } else {
    ac$SWC <- NA
  }

  # SW_IN
  ac$SW_IN  <- a[, trimws(site$SW_IN)]
  if (substr(site$SW_IN, 1, 4) == 'PPFD') {
    # convert PPFD to SW_IN
    ac$SW_IN  <- ac$SW_IN / 2.3
  }

  # Special cases: US-Jo2: interpolating SW_IN, otherwise ReddyProc does not work for years before 2013 because of no SW_IN data.
  if (name_site == "US-Jo2") {
    # fill each gap with the value 4 x 365 days later (365*48*4 half-hours)
    later <- match(ac$TIMESTAMP + 4 * 365 * 86400, ac$TIMESTAMP)
    ac$SW_IN[is.na(ac$SW_IN)] <- ac$SW_IN[later[is.na(ac$SW_IN)]]
  } else if (name_site == 'US-KM4') {
    # use PI gap-filled data
    ac$SW_IN[is.na(ac$SW_IN)] <- a$SW_IN_PI_F[is.na(ac$SW_IN)]
  }

  # USTAR
  ac$USTAR  <- a[, trimws(site$USTAR)]

  # RH or VPD
  if (site$RH != '') {
    ac$RH  <- a[, trimws(site$RH)]
    convert_RH_VPD <- TRUE
  } else {
    ac$VPD <- a[, trimws(site$VPD)]
    convert_RH_VPD <- FALSE
  }

  ac$daytime <- a$daytime

  ##########################################do Ustar filtering#############################################
  NEE_yearly <- ac |> dplyr::group_by(.data$DOY) |> dplyr::summarise(NEE=mean(.data$NEE, na.rm=T), TS=mean(.data$TS, na.rm=T)) |>
    dplyr::arrange(.data$DOY)

  # I will use the mean of the first three values, and the mean of the last three values
  tmp    <- NEE_yearly |> dplyr::filter(.data$NEE < min(NEE_yearly$NEE, na.rm=T) * 0.2)
  #
  gStart <- as.integer(mean(tmp$DOY[7])) - 4
  gEnd   <- as.integer(mean(tmp$DOY[(nrow(tmp)-6)])) + 4

  # In general, growing-season temperature should be > 0 C: no earlier than
  # the first day of year whose mean soil temperature is >= 0 C
  gStart <- max(gStart, min(NEE_yearly$DOY[which(NEE_yearly$TS >= 0)]))
  if (!is.na(site$gStart)) {
    gStart <- as.numeric(site$gStart)
  }
  if (!is.na(site$gEnd)) {
    gEnd <- as.numeric(site$gEnd)
  }
  #
  # Create seasonStarts with two columns: one is day, and the other is year!
  years  <- sort(unique(ac$YEAR))
  seasonStarts <- data.frame(DOY = rep(c(gStart, gEnd), length(years)), YEAR=rep(years, each=2))
  #
  # create the data frame
  ac_u       <- data.frame(DateTime = lubridate::ymd_hm(a$TIMESTAMP_END))   # The first dataset has to be 00:30
  ac_u$NEE   <- ac$NEE
  ac_u$Rg    <- ac$SW_IN
  ac_u$Tair  <- ac$TA
  ac_u$Ustar <- ac$USTAR

  if (convert_RH_VPD) {
    ac_u$rH    <- ac$RH
    ac_u$VPD <- REddyProc::fCalcVPDfromRHandTair(ac_u$rH, ac_u$Tair)
  } else {
    ac_u$VPD <- ac$VPD
  }

  if (dt == 1) {
    EProc <- REddyProc::sEddyProc$new(name_site, ac_u, c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'), DTS.n=24)
  } else {
    EProc <- REddyProc::sEddyProc$new(name_site, ac_u, c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
  }
  # Rg should not have negative values.
  EProc$sSetLocationInfo(LatDeg = lat_site, LongDeg = long_site, TimeZoneHour = tz$utc_offset_h)
  #
  # estimated Ustar threshold is too small for this site, using yearly estimate
  if (name_site == "US-ChR") {
    # use default season, and yearly threshold
    uStarTh <- EProc$sEstUstarThold()

    EProc$sMDSGapFillAfterUstar('NEE', FillAll = FALSE, isVerbose = FALSE)
    ac$NEE_uStar_f <- EProc$sExportResults()$NEE_uStar_f
    #
    ac_u <- ac_u |> dplyr::mutate(seasonYear = lubridate::year(.data$DateTime)) |>
      dplyr::left_join(uStarTh[uStarTh$aggregationMode == 'year', c("seasonYear", "uStar")], by="seasonYear")
  } else {
    ac_u$season <- REddyProc::usCreateSeasonFactorYdayYear(
      ac_u$DateTime - 15*60, starts = seasonStarts)  # it sets back 15 min.
    #
    uStarTh <- EProc$sEstUstarThold(seasonFactor = ac_u$season)

    # gap fill NEE, air temperature and soil temperature
    # By default the gap-filling uses annually aggregated estimates of uStar-Threshold.
    # we can also use a different threshold for each of the defined seasons, by calling the two functions
    # EProc$useSeaonsalUStarThresholds()
    # EProc$sGetUstarScenarios()
    # I only want annually aggregated estimated, because some sites have no seasonal estimates
    EProc$sMDSGapFillAfterUstar('NEE', FillAll = FALSE, isVerbose = FALSE)
    ac$NEE_uStar_f <- EProc$sExportResults()$NEE_uStar_f
    #
    ac_u <- ac_u |> dplyr::left_join(uStarTh[, c("season", "uStar")], by="season")
  }
  ac$uStarTh <- ac_u$uStar

  #
  #########################################end with Ustar filtering#######################################
  # the nighttime filters that do not involve soil temperature (whose own,
  # a value being present, is in ts_column_tables() below)
  if (site$SWC_use == 'YES') {
    night_ok <- !is.na(ac$NEE) & !is.na(ac$SWC) & !is.na(ac$USTAR) & ac$USTAR >= ac$uStarTh
  } else {
    night_ok <- !is.na(ac$NEE) & !is.na(ac$USTAR) & ac$USTAR >= ac$uStarTh
  }
  if (name_site %in% c("US-ICh", "US-ICt", "US-ICs")) {
    # tundra sites take advantage of data with little light
    night_ok <- night_ok & (ac$SW_IN < 10 | !ac$daytime) & ac$NEE > -5 & ac$NEE < 30
  } else {
    night_ok <- night_ok & !ac$daytime & ac$NEE > -5 & ac$NEE < 30
  }
  night_ok[is.na(night_ok)] <- FALSE

  # years to remove due to PI reported disturbances and bad data quality
  year_str <- site$year_removed
  year_parts <- strsplit(year_str, ",")[[1]]
  years2remove <- unlist(lapply(year_parts, function(part) {
    if (grepl(":", part)) {
      rng <- as.numeric(strsplit(part, ":")[[1]])
      seq(rng[1], rng[2])
    } else {
      as.numeric(part)
    }
  }))

  ts_base <- ac[, c("TIMESTAMP", "YEAR", "MONTH", "DAY", "DOY", "HOUR", "MINUTE", "NEE", "TA", "SWC", "NEE_uStar_f", "SW_IN", "daytime")]
  ts_base$night_ok <- night_ok
  ts_params <- list(
    site_ID = name_site, source = "ameriflux", southern_hemisphere = southern_hemisphere,
    dt_secs = as.numeric(dt, units = "secs"), uptake_doy = tmp$DOY,
    # In general, growing-season temperature should be > 0 C (ts_season())
    gStart_nee = as.integer(mean(tmp$DOY[7])) - 4, gStart_site = site$gStart, gEnd = gEnd,
    # we can only use TS > 2C for some sites, because data quality issue below TS < 2C
    tstart_floor = if (name_site %in% c("US-Ha1", "US-GLE")) 2 else NA, floor_before_gaps = FALSE,
    # use larger gap threshold because lack of data at these Tundra and tropical sites.
    gap_fixed = if (name_site %in% c("US-ICt", "US-ICh", "US-ICs", "BR-Ma2", "BR-Sa1")) c(60, 0.8) else NULL,
    years_removed = years2remove
  )
  ts_ok <- !is.na(ac$TS)
  out <- ts_column_tables(ts_base, ts_params, ac$TS, ts_ok)
  list(ac = out$ac, night = out$night, feature = out$feature, climatology = out$climatology, ts_base = ts_base, ts_params = ts_params, ts_ok = ts_ok)
}

#' Read a site's 01_01 soil-temperature table
#'
#' A site's 01_01 soil-temperature table, or NULL where soil temperature is not estimated.
#' "published": the outputs that came with the manuscript (original-data/TS_RandomForest;
#' 01_01's random forests do not reproduce them exactly on current package versions);
#' "regenerated": 01_01's output in data-proc/TS_RandomForest.
#'
#' @param site The site's row of data/site_info.csv.
#' @param source "published" or "regenerated".
#' @return A data frame, or NULL.
#' @noRd
read_ts_rf <- function(site, source = c("published", "regenerated")) {
  source <- match.arg(source)
  if (site$estimate_Ts != "YES") return(NULL)
  if (source == "published") {
    utils::read.csv(path_original("TS_RandomForest", paste0(site$site_ID, "_TS_rfp.csv")))
  } else {
    read_table(path_proc("TS_RandomForest", paste0(site$site_ID, "_TS_rfp.parquet")))
  }
}
