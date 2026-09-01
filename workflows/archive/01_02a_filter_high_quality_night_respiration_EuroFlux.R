# Purpose of this script: prepare data for fitting temperature-respiration curves for EuroFlux sites. 
# Output of this script: two .csv files. One for filtered night NEE, and the other for gap-filled whole time-series.
# Author: Junna Wang
#  It will take ~ 30 mins to finish running this script. 
#
# Input: half-hourly FLUXMET files downloaded by workflows/92-download-fluxnet.sh (fluxnet-shuttle),
# located at data-raw/FLUXNET/{site}/{EUF|ICOS}_{site}_FLUXNET_FLUXMET_HH_*.csv

# Special attention: 
# sites in southern hemisphere ('AU-Tum', 'ZA-Kru') have growing seasons spanning two years. 
# we added 11 European sites in Aug. 2025

library(librarian)
shelf(dplyr, lubridate)

library(optparse)

source("R/utils.R")

option_list <- list(
  make_option("--sites", type = "character", default = NULL,
              help = "Comma-separated list of site IDs to process"),
  make_option("--overwrite", action = "store_true", default = FALSE,
              help = "Overwrite existing output files")
)
parser <- OptionParser(description = "Prepare EuroFlux data for temperature-respiration curve fitting",
                       option_list = option_list)
parsed <- parse_args(parser, commandArgs(trailingOnly = TRUE))

overwrite <- parsed$overwrite
requested_sites <- if (!is.null(parsed$sites)) trimws(unlist(strsplit(parsed$sites, ","))) else NULL

####################Attention: change this directory based on your own directory of raw data
DIR_RAWDATA <- 'data-raw'
DIR_PROC <- 'data-proc/respiration/EuroFlux'
####################End Attention

files_FLUXNET <- list.files(file.path(DIR_RAWDATA, 'FLUXNET'), pattern = "^(EUF|ICOS)_[A-Za-z0-9-]+_FLUXNET_FLUXMET_HH_.*\\.csv$", full.names = TRUE, recursive = TRUE)

all_site_info <- get_site_info()

process_site <- function(name_site) {
  # name_site <- "CH-Lae"; overwrite <- TRUE
  site_info <- get_site_info(name_site)
  output_dir <- file.path(DIR_PROC, name_site)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_files <- file.path(
    output_dir,
    paste0(name_site, c('_ac.csv', '_nightNEE.csv'))
  )
  names(output_files) <- c("ac", "nightNEE")

  if (!overwrite && all(file.exists(output_files))) {
    message('Skipping ', name_site, ': respiration outputs already exist.')
    return(NULL)
  }

  if (site_info$source != "ICOS") {
    message('Skipping ', name_site, ': not an ICOS site.')
  }

  input_file <- list.files(
    file.path(DIR_RAWDATA, "FLUXNET", name_site),
    pattern = "_FLUXMET_(HH|HR)_",
    full.names = TRUE,
    recursive = TRUE
  )

  # fluxnet-shuttle provides one merged half-hourly FLUXMET file per site
  if (length(input_file) != 1) {
    stop(name_site, ': expected one fluxnet-shuttle FLUXMET_HH file, found ', length(input_file), '.')
  }
  a <- read.csv(input_file)
  # -9999 means missing information.
  a[a==-9999] <- NA
  
  # add more columns
  dt <- ymd_hm(a$TIMESTAMP_START[2]) - ymd_hm(a$TIMESTAMP_START[1])
  a$TIMESTAMP  <- ymd_hm(a$TIMESTAMP_START) + dt / 2
  a$YEAR  <- year(a$TIMESTAMP)
  a$MONTH <- month(a$TIMESTAMP)
  a$DAY   <- day(a$TIMESTAMP)
  a$DOY   <- yday(a$TIMESTAMP)
  a$HOUR  <- hour(a$TIMESTAMP)
  a$MINUTE <- minute(a$TIMESTAMP)

  # read soil temperature data, or use specific soil temperature data for some sites. 
  if (name_site %in% c('CZ-Stn')) {
    a$TS_F_MDS_1 <- a$TS_F_MDS_2  # use TS of second layer because the first layer is incomplete
    a$TS_F_MDS_1_QC <- a$TS_F_MDS_2_QC
  # } else if (name_site %in% c("FR-Fon", "CH-Dav", "DE-Akm", "DE-Hte", "FR-Bil", "FR-Pue", "FR-FBn", "CZ-RAJ")) {
  } else if (site_info[["estimate_Ts"]]) {
    ts_rfp_file <- list.files(file.path('data-proc', 'soil-temperature'), pattern = paste0('^', name_site, '_TS_rfp\\.csv$'), full.names = TRUE, recursive = TRUE)
    if (length(ts_rfp_file) != 1) {
      stop(name_site, ': expected one predicted soil-temperature file from workflow 01_01, found ', length(ts_rfp_file), '.')
    }
    df_TS <- read.csv(file=ts_rfp_file)
    df_TS$TIMESTAMP <- ymd_hms(df_TS$TIMESTAMP)
    df_TS <- left_join(data.frame(TIMESTAMP=a$TIMESTAMP), df_TS, by = "TIMESTAMP")
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
  
  # Southern Hemisphere, so we need to change DOY values
  southern_hemisphere = FALSE
  # if (name_site %in% c('AU-Tum', 'ZA-Kru', "BR-Sa1")) {
  if (site_info[["LAT"]] < 0) {
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
  if (site_info$SWC_use == 'NO') {
    SWC <- FALSE
  }
  
  # we decided to relax the SWC constrains
  a_measure_night_complete <- a %>% filter(!is.na(TA_F_MDS)) %>% filter(TS_F_MDS_1_QC %in% c(0, 1, 2)) %>% filter(NEE_VUT_REF_QC == 0) %>% filter(NIGHT == 1) %>% filter(NEE_VUT_REF > -5 & NEE_VUT_REF < 30) 
  
  yStart <- a_measure_night_complete$YEAR[1]
  yEnd   <- a_measure_night_complete$YEAR[nrow(a_measure_night_complete)]

  # decide which years data to exclude: rule 1: maximum data gaps during growing season < 1 month; rule2: large disturbance years due to fire, insect breakout
  # estimate growing season DOY
  NEE_yearly  <- a %>% group_by(DOY) %>% summarise(NEE=mean(NEE_VUT_REF, na.rm=T), TS=mean(TS_F_MDS_1, na.rm=T))
  NEE_yearly_night  <- a %>% filter(NIGHT==1) %>% group_by(DOY) %>% summarise(NEE=mean(NEE_VUT_REF, na.rm=T), TS=mean(TS_F_MDS_1, na.rm=T))
  
  # I will use the mean of the first three values, and the mean of the last three values
  if (name_site %in% c('FI-Sod', 'DE-RuC')) {
    tmp <- NEE_yearly %>% filter(NEE < 0.0)
  } else {
    tmp <- NEE_yearly %>% filter(NEE < max(min(NEE_yearly$NEE, na.rm=T) * 0.2, -0.8))
  }
  
  gStart <- as.integer(mean(tmp$DOY[7])) - 4
  gEnd   <- as.integer(mean(tmp$DOY[(nrow(tmp)-6)])) + 4
  
  # In general, growing-season temperature should be > 0 C
  gStart <- max(gStart, min(which(NEE_yearly$TS >= 0)))
  if (!is.na(site_info$gStart)) {
    gStart <- as.numeric(site_info$gStart)
  }
  if (!is.na(site_info$gEnd)) {
    gEnd <- as.numeric(site_info$gEnd)
  }
  
  # temperature range of growing season
  tStart <- quantile(tmp$TS, c(0.025), na.rm=T)
  tEnd   <- quantile(tmp$TS, c(0.975), na.rm=T) 
  
  # some sites NEE measurements under TS < 2C was inaccurate, so for these sites tStart should be greater than 2C
  if (name_site %in% c('CH-Dav')) {
    tStart <- max(tStart, 2)
    a_measure_night_complete <- a_measure_night_complete %>% filter(TS_F_MDS_1 >= 2.0)
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
  a_check_gaps <- a_check_gaps %>% rbind(a_gs_dates) %>% unique %>% arrange(TIMESTAMP)

  good_years <- a_check_gaps %>% 
    mutate(growing_year = case_when(DOY <= 366 ~ year(TIMESTAMP), 
                                    TRUE ~ year(TIMESTAMP) - 1)) %>% 
    arrange(growing_year, TIMESTAMP) %>%  # <-- Sort before lag
    group_by(growing_year) %>%
    filter(DOY >= gStart & DOY <= gEnd) %>% 
    mutate(lag_date = dplyr::lag(TIMESTAMP)) %>% 
    mutate(gap = as.numeric(difftime(TIMESTAMP, lag_date, units = "days"))) %>% 
    mutate(gap_large = ifelse(gap > 14, gap, 0)) %>%
    filter(!is.na(gap)) %>%
    summarise(gap_max = max(gap, na.rm=T), gap_total = sum(gap_large, na.rm=T) / (gEnd - gStart + 1)) %>% 
    filter(gap_max < gap_max_thresh &  gap_total < gap_total_thresh) %>%
    distinct(growing_year) %>% pull()
  
  # # ensure every degree of TS have measurements
  # bad_TS_years <- a_measure_night_complete %>% filter(between(TS_F_MDS_1, tStart, tEnd)) %>% 
  #   arrange(YEAR, TS_F_MDS_1) %>% group_by(YEAR) %>%
  #   mutate(gap = TS_F_MDS_1 - lag(TS_F_MDS_1)) %>%
  #   summarise(max_gap = max(gap, na.rm = TRUE)) %>%
  #   filter(max_gap > max(0.2 * (tEnd - tStart), 1.0)) %>% distinct(YEAR) %>% pull()
  # if (length(bad_TS_years) >= 1) {
  #   good_years <- setdiff(good_years, bad_TS_years)    
  # }
  
  years2remove_automation <- setdiff(yStart:yEnd, good_years)
  
  # years to remove due to PI reported disturbances and bad data quality
  year_str <- site_info$year_removed
  if (is.na(year_str)) {
    years2remove <- numeric()
  } else {
    year_parts <- strsplit(year_str, ",")[[1]]
    years2remove <- unlist(lapply(year_parts, function(part) {
      if (grepl(":", part)) {
        rng <- as.numeric(strsplit(part, ":")[[1]])
        seq(rng[1], rng[2])
      } else {
        as.numeric(part)
      }
    }))
  }
  
  if (length(years2remove) >= 1) {
    good_years <- setdiff(good_years, years2remove)
  }
  
  
  # select good_years based on growing year
  a_measure_night_complete <- a_measure_night_complete  %>% 
    mutate(growing_year = case_when(DOY <= 366 ~ YEAR, 
                                    TRUE ~ YEAR - 1)) %>% 
    filter(growing_year %in% good_years)
  # change column names
  a_measure_night_complete$TA <- a_measure_night_complete$TA_F_MDS
  a_measure_night_complete$TS <- a_measure_night_complete$TS_F_MDS_1
  a_measure_night_complete$SWC <- a_measure_night_complete$SWC_F_MDS_1
  a_measure_night_complete$NEE <- a_measure_night_complete$NEE_VUT_REF
  
  a_measure_night_complete <- a_measure_night_complete %>% dplyr::select(c(YEAR, MONTH, DAY, DOY, HOUR, MINUTE, NEE, TA, TS, SWC))
  
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
  ac <- ac %>% filter(between(YEAR, iStart, iEnd))
  
  # save the ac and a_measure_night_complete data:
  write.csv(ac, file=output_files["ac"], row.names = F)
  write.csv(a_measure_night_complete, file=output_files["nightNEE"], row.names = F)
  
  data.frame(site_ID=name_site, gStart=gStart, gEnd=gEnd, tStart=max(tStart, 0.0), tEnd=tEnd, nyear=length(good_years))
}

feature_file <- file.path(DIR_PROC, 'features', 'growing_season_feature_EuroFlux.csv')
feature_gs <- if (file.exists(feature_file)) read.csv(feature_file) else {
  data.frame(site_ID=character(), gStart=double(), gEnd=double(), tStart=double(), tEnd=double(), nyear=integer())
}
shuttle_sites <- sub("^(EUF|ICOS)_(.+)_FLUXNET_FLUXMET_HH_.*\\.csv$", "\\2", basename(files_FLUXNET))
candidate_sites <- intersect(all_site_info$site_ID[all_site_info$source != 'AmeriFlux_BASE'], shuttle_sites)
sites <- if (is.null(requested_sites)) candidate_sites else trimws(unlist(strsplit(requested_sites, ',')))
unknown_sites <- setdiff(sites, candidate_sites)
if (length(unknown_sites) > 0) {
  stop('Sites are not eligible for EuroFlux processing: ', paste(unknown_sites, collapse = ', '))
}

if (overwrite) {
  feature_gs <- feature_gs[!feature_gs$site_ID %in% sites, , drop = FALSE]
}

for (name_site in sites) {
  message("Processing site ", name_site)
  output_files <- file.path(DIR_PROC, name_site, paste0(name_site, c('_ac.csv', '_nightNEE.csv')))
  already_aggregated <- name_site %in% feature_gs$site_ID
  if (!overwrite && (all(file.exists(output_files)) || already_aggregated)) {
    message('Skipping ', name_site, ': already processed.')
    next
  }
  feature <- process_site(name_site)
  if (!is.null(feature)) {
    feature_gs <- bind_rows(feature_gs, feature)
  }
}

write.csv(feature_gs, file=feature_file, row.names = F)

# Sys.time()
