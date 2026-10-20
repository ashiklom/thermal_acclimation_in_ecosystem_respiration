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
# feature = <its row of the growing-season features>). `site` is the site's
# row of data/site_info.csv; `raw_files` is raw_site_files().

# Paths of every raw site-data file, by product.
raw_site_files <- function() {
  list(
    AmeriFlux_BASE = list.files(path_original("SiteData", "AmeriFlux_BASE"), pattern=".zip$", full.names = T),
    FLUXNET2015 = list.files(path=path_original('SiteData', 'FLUXNET2015', "unzip"), pattern = "_FLUXNET2015_FULLSET_(HH|HR)_", full.names = TRUE),
    FLUXNET2020 = list.files(path=path_original('SiteData', 'FLUXNET2020', "unzip"), pattern = "_FLUXNET2015_FULLSET_(HH|HR)_", full.names = TRUE),
    ICOS_after2020 = list.files(path=path_original('SiteData', 'ICOS_after2020'), pattern = ".csv$", full.names = TRUE),
    FLUXNET2025 = list.files(path_original("SiteData", "FLUXNET07202025", "unzip"), pattern=".csv$", full.names = T),
    ICOS2025 = list.files(path_original("SiteData", "Ecosystem final quality (L2) product in ETC-Archive format - release 2025-1", "unzip"), pattern=".csv$", full.names = T)
  )
}

# The growing-season feature table: per-site rows bound in site order onto
# the typed empty table the loops started from.
combine_features <- function(rows) {
  feature_gs <- data.frame(site_ID=character(), gStart=double(), gEnd=double(), tStart=double(), tEnd=double(), nyear=integer())  # growing season feature
  for (row in rows) feature_gs <- dplyr::bind_rows(feature_gs, row)
  feature_gs
}

#--------A function to predict soil temperature
# inputs: a data frame data with 2-3 columns (TS, TA, NETRAD), the last column is optional
# output: the same data frame data, but with one added column TS_pred
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

estimate_ts_site <- function(site, raw_files) {
  files_AmeriFlux_BASE <- raw_files$AmeriFlux_BASE
  files_FLUXNET2015 <- raw_files$FLUXNET2015
  files_FLUXNET2020 <- raw_files$FLUXNET2020
  files_ICOS_after2020 <- raw_files$ICOS_after2020
  files_FLUXNET2025 <- raw_files$FLUXNET2025
  files_ICOS2025 <- raw_files$ICOS2025

  name_site <- site$site_ID
  
  if (site$source %in% c("AmeriFlux_BASE", "AmeriFlux_FLUXNET")) {
    a <- amerifluxr::amf_read_base(files_AmeriFlux_BASE[grepl(name_site, files_AmeriFlux_BASE)], parse_timestamp=TRUE, unzip = T)
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
    data_source <- site$source
    data_source <- unlist(strsplit(data_source, "_"))
    for (isource in 1:length(data_source)) {
      if (data_source[isource] == "FLUXNET2015") {
        files <- files_FLUXNET2015
      } else if (data_source[isource] == "FLUXNET2020") {
        files <- files_FLUXNET2020
      } else if (data_source[isource] == "ICOS2020") {
        files <- files_ICOS_after2020
      } else if (data_source[isource] == "FLUXNET2025") {
        files <- files_FLUXNET2025
      } else if (data_source[isource] == "ICOS2025") {
        files <- files_ICOS2025
      }
      file  <- files[grepl(name_site, files)]
      a_tmp <- read.csv(file)
      if (isource == 1) {
        a <- a_tmp
      } else {
        # need to check if they can connect correctly, or use bind_rows. 
        irow_start <- which(a_tmp$TIMESTAMP_START == a$TIMESTAMP_START[nrow(a)]) + 1
        if (is.numeric(irow_start) && length(irow_start) == 0) {
          irow_start = 1
        }
        a <- dplyr::bind_rows(a, a_tmp[irow_start:nrow(a_tmp), ])
      }
    }
    rm(a_tmp)
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
    TA_gf <- data |> dplyr::group_by(DOY, HOUR, MINUTE) |> dplyr::summarise(TA_gf=mean(TA, na.rm=T))
    data   <- data |> dplyr::left_join(TA_gf, by=c("DOY", "HOUR", "MINUTE"))
  }
  if (sum(is.na(data$TA)) > 0) {
    data$TA[is.na(data$TA)] <- data$TA_gf[is.na(data$TA)]
  }
  
  # gap fill NETRAD if using NETRAD method
  if ("NETRAD" %in% names(data)) {
    if (!"NETRAD_gf" %in% colnames(data)) {
      gf <- data |> dplyr::group_by(DOY, HOUR, MINUTE) |> dplyr::summarise(NETRAD_gf=mean(NETRAD, na.rm=T))
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

prep_site_euroflux <- function(site, raw_files) {
  files_AmeriFlux_BASE <- raw_files$AmeriFlux_BASE
  files_FLUXNET2015 <- raw_files$FLUXNET2015
  files_FLUXNET2020 <- raw_files$FLUXNET2020
  files_ICOS_after2020 <- raw_files$ICOS_after2020
  files_FLUXNET2025 <- raw_files$FLUXNET2025
  files_ICOS2025 <- raw_files$ICOS2025

  data_source <- site$source
  name_site <- site$site_ID
  
  # connecting multiple data sources
  data_source <- unlist(strsplit(data_source, "_"))
  for (isource in 1:length(data_source)) {
    if (data_source[isource] == "FLUXNET2015") {
      files <- files_FLUXNET2015
    } else if (data_source[isource] == "FLUXNET2020") {
      files <- files_FLUXNET2020
    } else if (data_source[isource] == "FLUXNET2025") {
      files <- files_FLUXNET2025
    } else if (data_source[isource] == "ICOS2025") {
      files <- files_ICOS2025
    }
    file  <- files[grepl(name_site, files)]
    a_tmp <- read.csv(file)
    if (isource == 1) {
      a <- a_tmp
    } else {
      # need to check if they can connect correctly, or use bind_rows. 
      irow_start <- which(a_tmp$TIMESTAMP_START == a$TIMESTAMP_START[nrow(a)]) + 1
      if (is.numeric(irow_start) && length(irow_start) == 0) {
        irow_start = 1
      }
      a <- dplyr::bind_rows(a, a_tmp[irow_start:nrow(a_tmp), ])
    }
  }
  rm(a_tmp)
  # -9999 means missing information.
  a[a==-9999] <- NA
  
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
    df_TS <- read.csv(file=path_proc("TS_RandomForest", paste0(name_site, '_TS_rfp.csv')))
    df_TS$TIMESTAMP <- lubridate::ymd_hms(df_TS$TIMESTAMP)
    df_TS <- dplyr::left_join(data.frame(TIMESTAMP=a$TIMESTAMP), df_TS, by = "TIMESTAMP")
    a$TS_F_MDS_1 <- df_TS$TS_pred
    a$TS_F_MDS_1_QC[is.na(a$TS_F_MDS_1_QC) | a$TS_F_MDS_1_QC == 3] <- 2
    rm(df_TS)
  } else if (name_site == "FI-Sod") {
    # the first 5 year TS measurements was off, likely because sensor changed locations. 
    # in 2001, measurements in soil profile 2 are correct, so building relations between profiles 1 and 2
    mod1 <- lm(data = a[1:24383,], TS_F_MDS_2 ~ TS_F_MDS_1)
    pred2 <- predict(mod1, data.frame(TS_F_MDS_1 = a$TS_F_MDS_1[a$YEAR <= 2005]))
    mod2 <- lm(data = a[90000:245000,], TS_F_MDS_1 ~ TS_F_MDS_2)
    a$TS_F_MDS_1[a$YEAR <= 2005] <- predict(mod2, data.frame(TS_F_MDS_2 = pred2))
    a$TS_F_MDS_1_QC[a$YEAR <= 2005] <- 2
  } else if (name_site == "GF-Guy") {
    # use air temperature for this tropical site so that all tropical sites, we used bottom air temperature. 
    a$TS_F_MDS_1 <- a$TA_F_MDS
    a$TS_F_MDS_1_QC <- a$TA_F_MDS_QC
  }
  
  # FLUXNET data do not include soil data for Site FR-Pue, but site PI provided the SWC data
  if (name_site == 'FR-Pue') {
    swc_2003_2015 <- read.table(path_original("SiteData", "soil_water_content_FR-Pue", "FR-Pue_SWC_Control_Corrected_2003-2015.csv"), sep=';', header=T)
    swc_2016_2020 <- read.table(path_original("SiteData", "soil_water_content_FR-Pue", "FR-Pue_SWC_Control_Corrected_2016-2020.csv"), sep=';', header=T)
    swc_2003_2020 <- dplyr::bind_rows(swc_2003_2015, swc_2016_2020)
    swc_2003_2020$TIMESTAMP_START <- lubridate::dmy_hm(swc_2003_2020$date_time)
    
    a <- a |> dplyr::mutate(TIMESTAMP_START = lubridate::ymd_hm(a$TIMESTAMP_START)) |> 
    dplyr::left_join(swc_2003_2020[, c("TIMESTAMP_START", "SWC_Mean")], by="TIMESTAMP_START")
    # times 100 to convert SWC into percentage
    a$SWC_Mean <- a$SWC_Mean*100
    a$SWC_F_MDS_1[!is.na(a$SWC_Mean)] <- a$SWC_Mean[!is.na(a$SWC_Mean)]
    a$SWC_F_MDS_1_QC[!is.na(a$SWC_Mean)] <- 1
  }
  
  # Southern Hemisphere, so we need to change DOY values
  southern_hemisphere = FALSE
  if (name_site %in% c('AU-Tum', 'ZA-Kru', "BR-Sa1")) {
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
  # we decided to relax the SWC constrains
  a_measure_night_complete <- a |> dplyr::filter(!is.na(TA_F_MDS)) |> dplyr::filter(TS_F_MDS_1_QC %in% c(0, 1, 2)) |> dplyr::filter(NEE_VUT_REF_QC == 0) |> dplyr::filter(NIGHT == 1) |> dplyr::filter(NEE_VUT_REF > -5 & NEE_VUT_REF < 30) 
  # |> dplyr::filter(SWC_F_MDS_1_QC %in% c(0, 1, 2))
  
  yStart <- a_measure_night_complete$YEAR[1]
  yEnd   <- a_measure_night_complete$YEAR[nrow(a_measure_night_complete)]
  
  # decide which years data to exclude: rule 1: maximum data gaps during growing season < 1 month; rule2: large disturbance years due to fire, insect breakout
  # estimate growing season DOY
  NEE_yearly  <- a |> dplyr::group_by(DOY) |> dplyr::summarise(NEE=mean(NEE_VUT_REF, na.rm=T), TS=mean(TS_F_MDS_1, na.rm=T))
  
  # I will use the mean of the first three values, and the mean of the last three values
  if (name_site %in% c('FI-Sod', 'DE-RuC')) {
    tmp <- NEE_yearly |> dplyr::filter(NEE < 0.0)
  } else {
    tmp <- NEE_yearly |> dplyr::filter(NEE < max(min(NEE_yearly$NEE, na.rm=T) * 0.2, -0.8))
  }
  
  gStart <- as.integer(mean(tmp$DOY[7])) - 4
  gEnd   <- as.integer(mean(tmp$DOY[(nrow(tmp)-6)])) + 4
  
  # In general, growing-season temperature should be > 0 C
  gStart <- max(gStart, min(which(NEE_yearly$TS >= 0)))
  if (!is.na(site$gStart)) {
    gStart <- as.numeric(site$gStart)
  }
  if (!is.na(site$gEnd)) {
    gEnd <- as.numeric(site$gEnd)
  }
  
  # temperature range of growing season
  tStart <- quantile(tmp$TS, c(0.025), na.rm=T)
  tEnd   <- quantile(tmp$TS, c(0.975), na.rm=T) 
  
  # some sites NEE measurements under TS < 2C was inaccurate, so for these sites tStart should be greater than 2C
  if (name_site %in% c('CH-Dav')) {
    tStart <- max(tStart, 2)
    a_measure_night_complete <- a_measure_night_complete |> dplyr::filter(TS_F_MDS_1 >= 2.0)
  }
  
  # max gap and total large gap threshold
  if (name_site %in% c('ZA-Kru', 'FI-Sod', 'GF-Guy')) {
    # use larger gap threshold because lack of data at these Tropical and Tundra sites. 
    gap_max_thresh <- 60
    gap_total_thresh <- 0.7   
  } else {
    gap_max_thresh <- max(31, (gEnd-gStart+1) * 0.225)
    gap_total_thresh <- max(1/3, gap_max_thresh / (gEnd-gStart+1))
  }
  
  # find long gap years
  # I need to check if this works for the southern hemisphere.
  a_check_gaps <- a_measure_night_complete[, c("TIMESTAMP", "DOY")]
  
  # adding the start and end of growing season date
  if (southern_hemisphere) {
    gStart_original <- ifelse(gStart > 366, gStart-366, gStart)
    gEnd_original <- ifelse(gEnd > 366, gEnd-366, gEnd)
    a_gs_dates <- data.frame(TIMESTAMP = c(as.POSIXct((gStart_original - 1) * 86400 + as.numeric(dt/2, units = "secs"), origin = paste0(yStart:yEnd, "-01-01"), tz = "UTC"), 
                                           as.POSIXct((gEnd_original - 1) * 86400 + as.numeric(dt/2, units = "secs"), origin = paste0(yStart:yEnd, "-01-01"), tz = "UTC")), 
                             DOY = c(rep(gStart, yEnd - yStart + 1), rep(gEnd, yEnd - yStart + 1)))
  } else {
    a_gs_dates <- data.frame(TIMESTAMP = c(as.POSIXct((gStart - 1) * 86400 + as.numeric(dt/2, units = "secs"), origin = paste0(yStart:yEnd, "-01-01"), tz = "UTC"), 
                                           as.POSIXct((gEnd - 1) * 86400 + as.numeric(dt/2, units = "secs"), origin = paste0(yStart:yEnd, "-01-01"), tz = "UTC")), 
                             DOY = c(rep(gStart, yEnd - yStart + 1), rep(gEnd, yEnd - yStart + 1)))
  }
  a_check_gaps <- a_check_gaps |> rbind(a_gs_dates) |> unique() |> dplyr::arrange(TIMESTAMP)

  good_years <- a_check_gaps |> 
    dplyr::mutate(growing_year = dplyr::case_when(DOY <= 366 ~ lubridate::year(TIMESTAMP), 
                                    TRUE ~ lubridate::year(TIMESTAMP) - 1)) |> 
    dplyr::arrange(growing_year, TIMESTAMP) |>  # <-- Sort before lag
    dplyr::group_by(growing_year) |>
    dplyr::filter(DOY >= gStart & DOY <= gEnd) |> 
    dplyr::mutate(lag_date = dplyr::lag(TIMESTAMP)) |> 
    dplyr::mutate(gap = as.numeric(difftime(TIMESTAMP, lag_date, units = "days"))) |> 
    dplyr::mutate(gap_large = ifelse(gap > 14, gap, 0)) |>
    dplyr::filter(!is.na(gap)) |>
    dplyr::summarise(gap_max = max(gap, na.rm=T), gap_total = sum(gap_large, na.rm=T) / (gEnd - gStart + 1)) |> 
    dplyr::filter(gap_max < gap_max_thresh &  gap_total < gap_total_thresh) |>
    dplyr::distinct(growing_year) |> dplyr::pull()
  
  years2remove_automation <- setdiff(yStart:yEnd, good_years)
  
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
  
  if (length(years2remove) >= 1) {
    good_years <- setdiff(good_years, years2remove)
  }
  
  
  # select good_years based on growing year
  a_measure_night_complete <- a_measure_night_complete  |> 
    dplyr::mutate(growing_year = dplyr::case_when(DOY <= 366 ~ YEAR, 
                                    TRUE ~ YEAR - 1)) |> 
    dplyr::filter(growing_year %in% good_years)
  # change column names
  a_measure_night_complete$TA <- a_measure_night_complete$TA_F_MDS
  a_measure_night_complete$TS <- a_measure_night_complete$TS_F_MDS_1
  a_measure_night_complete$SWC <- a_measure_night_complete$SWC_F_MDS_1
  a_measure_night_complete$NEE <- a_measure_night_complete$NEE_VUT_REF
  
  a_measure_night_complete <- a_measure_night_complete |> dplyr::select(c(YEAR, MONTH, DAY, DOY, HOUR, MINUTE, NEE, TA, TS, SWC))
  
  # select useful column and year to output
  ac <- a[, c("YEAR", "MONTH", "DAY", "DOY", "HOUR", "MINUTE")]
  ac$NEE <- a$NEE_VUT_REF
  ac$NEE_QC  <- a$NEE_VUT_REF_QC
  ac$TA  <- a$TA_F_MDS
  ac$TS  <- a$TS_F_MDS_1
  ac$SWC <- a$SWC_F_MDS_1
  ac$NEE_uStar_f <- a$NEE_VUT_REF
  ac$daytime <- a$NIGHT!=1
  ac$SW_IN <- a$SW_IN_F_MDS
  ac$GPP_DT <- a$GPP_DT_VUT_REF
  
  # only keep years between start year and end year
  iStart = a_measure_night_complete$YEAR[1]
  iEnd   = a_measure_night_complete$YEAR[nrow(a_measure_night_complete)]
  ac <- ac |> dplyr::filter(dplyr::between(YEAR, iStart, iEnd))
  
  
  feature <- data.frame(site_ID=name_site, gStart=gStart, gEnd=gEnd, tStart=max(tStart, 0.0), tEnd=tEnd, nyear=length(good_years))

  list(ac = ac, night = a_measure_night_complete, feature = feature)
}

prep_site_ameriflux <- function(site, raw_files) {
  files_AmeriFlux_BASE <- raw_files$AmeriFlux_BASE
  files_FLUXNET2015 <- raw_files$FLUXNET2015
  files_FLUXNET2020 <- raw_files$FLUXNET2020
  files_ICOS_after2020 <- raw_files$ICOS_after2020
  files_FLUXNET2025 <- raw_files$FLUXNET2025
  files_ICOS2025 <- raw_files$ICOS2025

  data_source <- site$source
  name_site <- site$site_ID

  # read data
  a <- amerifluxr::amf_read_base(files_AmeriFlux_BASE[grepl(name_site, files_AmeriFlux_BASE)], parse_timestamp=TRUE, unzip = T)
  a[a==-9999] <- NA
  if (name_site == "US-Myb") {
    # use data from the second year, because lots of missing NEE in the first year. 
    a <- a[17521:245424, ]
    # combine TS data at two depth
    a$TS_2_1_1[is.na(a$TS_2_1_1)] <- a$TS_2_2_1[is.na(a$TS_2_1_1)] * 0.9324892 + 0.9077066
  }

  ######################################decide day or night for every TIMESTAMP#######################################
  # get site long and lat
  # site coordinates as used for the published outputs (see
  # scripts/snapshot-ameriflux-coordinates.R), not the live AmeriFlux API
  sites <- utils::read.csv(file.path("data", "ameriflux_coordinates.csv"))
  long_site  <- sites$LONG[sites$site_ID==name_site]
  lat_site   <- sites$LAT[sites$site_ID==name_site]
  
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
    date = seq.Date(as.Date(a$TIMESTAMP[1]), as.Date(a$TIMESTAMP[nrow(a)]), by = 1),       # Date format: YYYY-MM-DD
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

  # if in southern Hemisphere, so we need to change DOY values
  southern_hemisphere = FALSE
  if (name_site %in% c()) {
    a$DOY[a$DOY < 183] <- a$DOY[a$DOY < 183] + 366
    southern_hemisphere = TRUE
  }
  
  # create ancillary df
  ac <- a[, 1:7]  
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
    a <- a |> dplyr::filter(YEAR > 2000)
    ac <- ac |> dplyr::filter(YEAR > 2000)
  }
  if (name_site == "CA-Mer") {
    # only use data after 1999, the first two-year data have some problems. 
    a <- a |> dplyr::filter(YEAR > 1999)
    ac <- ac |> dplyr::filter(YEAR > 1999)
  }
  if (name_site == "US-Jo2") {
    # TA data from 123453~124049 has problems use Tsonic data 
    a$TA[123453:124049] <- a$T_SONIC[123453:124049] * 1.0183316 - 0.9299
  }   
  if (name_site == "US-BZS") {
    # TA data in 2012-2014 is not accurate, so use nearby US-BZF's TA data.
    a_BZF <- amerifluxr::amf_read_base(files_AmeriFlux_BASE[grepl('US-BZF', files_AmeriFlux_BASE)], parse_timestamp=TRUE, unzip = T)
    a_BZF[a_BZF==-9999] <- NA
    df <- data.frame(BZS = a$TA_PI_F[dplyr::between(a$YEAR, 2016, 2019)], BZF = a_BZF$TA_PI_F[dplyr::between(a_BZF$YEAR, 2016, 2019)])
    mod <- lm(data=df, BZS ~ BZF)
    a$TA_PI_F[dplyr::between(a$YEAR, 2012, 2014)] <- predict(mod, newdata = data.frame(BZF = a_BZF$TA_PI_F[dplyr::between(a_BZF$YEAR, 2012, 2014)]))
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
    df_TS <- read.csv(file=path_proc("TS_RandomForest", paste0(name_site, '_TS_rfp.csv')))
    df_TS$TIMESTAMP <- lubridate::ymd_hms(df_TS$TIMESTAMP)
    df_TS <- dplyr::left_join(data.frame(TIMESTAMP=ac$TIMESTAMP), df_TS, by = "TIMESTAMP")
    ac$TS <- df_TS$TS_pred
    rm(df_TS)
  } else {
    if (site$TS != "") {
      ac$TS  <- a[, trimws(site$TS)]
    } 
    # deal with special cases
    if (name_site %in% c('US-NR1', 'US-ICh', "US-ICs")) {
      # use PI gap-filled data
      ac$TS[is.na(ac$TS)] <- a$TS_PI_1[is.na(ac$TS)]
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
    ac$SW_IN[which(is.na(ac$SW_IN))] <- ac$SW_IN[which(is.na(ac$SW_IN)) + 365*48*4]
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
  NEE_yearly <- ac |> dplyr::group_by(DOY) |> dplyr::summarise(NEE=mean(NEE, na.rm=T), TS=mean(TS, na.rm=T))

  # I will use the mean of the first three values, and the mean of the last three values
  tmp    <- NEE_yearly |> dplyr::filter(NEE < min(NEE_yearly$NEE, na.rm=T) * 0.2)
  #
  gStart <- as.integer(mean(tmp$DOY[7])) - 4
  gEnd   <- as.integer(mean(tmp$DOY[(nrow(tmp)-6)])) + 4
  
  # In general, growing-season temperature should be > 0 C. 
  gStart <- max(gStart, min(which(NEE_yearly$TS >= 0)))
  if (!is.na(site$gStart)) {
    gStart <- as.numeric(site$gStart)
  }
  if (!is.na(site$gEnd)) {
    gEnd <- as.numeric(site$gEnd)
  }
  #
  # temperature range of growing season
  tStart <- quantile(tmp$TS, c(0.025), na.rm=T)
  tEnd   <- quantile(tmp$TS, c(0.975), na.rm=T)
  #
  # Create seasonStarts with two columns: one is day, and the other is year!
  years  <- unique(ac$YEAR)
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
    ac_u <- ac_u |> dplyr::mutate(seasonYear = lubridate::year(DateTime)) |> 
      dplyr::left_join(uStarTh[uStarTh$aggregationMode == 'year', c(2, 4)], by="seasonYear")
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
    ac_u <- ac_u |> dplyr::left_join(uStarTh[,3:4], by="season")
  }
  ac$uStarTh <- ac_u$uStar  
  
  #
  #########################################end with Ustar filtering#######################################
  if (site$SWC_use == 'YES') {
    a_measure     <- ac |> dplyr::filter(!is.na(NEE) & !is.na(TS) & !is.na(SWC) & !is.na(USTAR)) |> dplyr::filter(USTAR >= uStarTh)
  } else {
    a_measure     <- ac |> dplyr::filter(!is.na(NEE) & !is.na(TS) & !is.na(USTAR)) |> dplyr::filter(USTAR >= uStarTh)
  }
  #
  if (name_site %in% c("US-ICh", "US-ICt", "US-ICs")) {
    # tundra sites take advantage of data with little light
    a_measure_night_complete <- a_measure |> dplyr::filter(SW_IN < 10 | !daytime) |> dplyr::filter(NEE > -5 & NEE < 30)
  } else {
    a_measure_night_complete <- a_measure |> dplyr::filter(!daytime) |> dplyr::filter(NEE > -5 & NEE < 30)
  }
  
  
  #########################################remove years with big gaps#######################################
  # max gap and total large gap threshold
  if (name_site %in% c("US-ICt", "US-ICh", "US-ICs", "BR-Ma2", "BR-Sa1")) {
    # use larger gap threshold because lack of data at these Tundra and tropical sites. 
    gap_max_thresh <- 60
    gap_total_thresh <- 0.8
  } else {
    gap_max_thresh <- max(31, (gEnd-gStart+1) * 0.225)
    gap_total_thresh <- max(1/3, gap_max_thresh / (gEnd-gStart+1))
  }

  # find long gap years
  a_check_gaps <- a_measure_night_complete[, c("TIMESTAMP", "DOY")]
  yStart <- a_measure_night_complete$YEAR[1]
  yEnd   <- a_measure_night_complete$YEAR[nrow(a_measure_night_complete)]

  # adding the start and end of growing season date
  if (southern_hemisphere) {
    gStart_original <- ifelse(gStart > 366, gStart-366, gStart)
    gEnd_original <- ifelse(gEnd > 366, gEnd-366, gEnd)
    a_gs_dates <- data.frame(TIMESTAMP = c(as.POSIXct((gStart_original - 1) * 86400 + as.numeric(dt/2, units = "secs"), origin = paste0(yStart:yEnd, "-01-01"), tz = "UTC"), 
                                           as.POSIXct((gEnd_original - 1) * 86400 + as.numeric(dt/2, units = "secs"), origin = paste0(yStart:yEnd, "-01-01"), tz = "UTC")), 
                             DOY = c(rep(gStart, yEnd - yStart + 1), rep(gEnd, yEnd - yStart + 1)))
  } else {
    a_gs_dates <- data.frame(TIMESTAMP = c(as.POSIXct((gStart - 1) * 86400 + as.numeric(dt/2, units = "secs"), origin = paste0(yStart:yEnd, "-01-01"), tz = "UTC"), 
                                           as.POSIXct((gEnd - 1) * 86400 + as.numeric(dt/2, units = "secs"), origin = paste0(yStart:yEnd, "-01-01"), tz = "UTC")), 
                             DOY = c(rep(gStart, yEnd - yStart + 1), rep(gEnd, yEnd - yStart + 1)))
  } 
  a_check_gaps <- a_check_gaps |> rbind(a_gs_dates) |> unique() |> dplyr::arrange(TIMESTAMP)

  good_years <- a_check_gaps |>
    dplyr::mutate(growing_year = dplyr::case_when(DOY <= 366 ~ lubridate::year(TIMESTAMP),
                                    TRUE ~ lubridate::year(TIMESTAMP) - 1)) |>
    dplyr::arrange(growing_year, TIMESTAMP) |>  # <-- Sort before lag
    dplyr::group_by(growing_year) |>
    dplyr::filter(DOY >= gStart & DOY <= gEnd) |>
    dplyr::mutate(lag_date = dplyr::lag(TIMESTAMP)) |>
    dplyr::mutate(gap = as.numeric(difftime(TIMESTAMP, lag_date, units = "days"))) |>
    dplyr::mutate(gap_large = ifelse(gap > 14, gap, 0)) |>
    dplyr::filter(!is.na(gap)) |>
    dplyr::summarise(gap_max = max(gap, na.rm=T), gap_total = sum(gap_large, na.rm=T) / (gEnd - gStart + 1)) |>
    dplyr::filter(gap_max < gap_max_thresh &  gap_total < gap_total_thresh) |>
    dplyr::distinct(growing_year) |> dplyr::pull()

  years2remove_automation <- setdiff(ac$YEAR[1]:ac$YEAR[nrow(ac)], good_years)
  
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
  
  if (length(years2remove) >= 1) {
    good_years <- setdiff(good_years, years2remove)
  }

  ############################prepare to output two datasets: a_measure_night_complete and ac#######################################
  # we can only use TS > 2C for some sites, because data quality issue below TS < 2C 
  if (name_site %in% c("US-Ha1", "US-GLE")) {
    tStart <- max(tStart, 2.0)
    a_measure_night_complete <- a_measure_night_complete |> dplyr::filter(TS >= 2.0)
  } 
  
  a_measure_night_complete <- a_measure_night_complete |> 
    dplyr::filter(YEAR %in% good_years) |>
    dplyr::select(c(YEAR, MONTH, DAY, DOY, HOUR, MINUTE, NEE, TA, TS, SWC))

  iStart = a_measure_night_complete$YEAR[1]
  iEnd   = a_measure_night_complete$YEAR[nrow(a_measure_night_complete)]
  ac <- ac |> dplyr::filter(dplyr::between(YEAR, iStart, iEnd)) |> dplyr::select(c(YEAR, MONTH, DAY, DOY, HOUR, MINUTE, NEE_uStar_f, TA, TS, SWC, SW_IN, daytime))

  # I have to gap fill TA, TS, NEE;
  T_gf <- ac |> dplyr::group_by(DOY, HOUR, MINUTE) |> dplyr::summarise(TA_gf=mean(TA, na.rm=T), TS_gf=mean(TS, na.rm=T), NEE_gf=mean(NEE_uStar_f, na.rm=T))  # for gap fill only.
  # ensure no NA values
  for (i in 4:6) {
    na.gf <- which(is.na(T_gf[,i]))
    for (j in na.gf) {
      T_gf[j, i] <- T_gf[max(j - 24/as.numeric(dt), 1), i]
    }
  }
  
  if (!"TA_gf" %in% colnames(ac)) {
    ac   <- ac |> dplyr::left_join(T_gf, by=c("DOY", "HOUR", "MINUTE"))
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


  feature <- data.frame(site_ID=name_site, gStart=gStart, gEnd=gEnd, tStart=max(tStart, 0.0), tEnd=tEnd, nyear=length(good_years))

  list(ac = ac, night = a_measure_night_complete, feature = feature)
}
