# title: Estimate Soil Temperature Based on Air Temp and net Radiation using Random Forests or linear regression for 16 sites

#  Description
#  this script intends to estimate surface soil temperature at sites with lots of missing soil temperature data
#  the method is random forests for 8 sites ("DE-Akm", "FR-Pue", "US-Los", "US-SRG", "CA-Man", "US-Ced", "US-Ho1", "CZ-RAJ"), 
#  but for the remaining sites, we used linear regression due to limited (1-2 years) TS observations or not enough observation of net radiation.
#   
#  Author: Junna Wang, July 2025
#  It will take ~40 min to finish running this script. 

library(librarian)
shelf(randomForest, caret, amerifluxr, lubridate, tidyverse, ranger)

rm(list=ls())

dir_rawdata <- 'data-raw'

args <- commandArgs(trailingOnly = TRUE)
overwrite <- '--overwrite' %in% args
site_arg <- args[grepl('^--sites=', args)]
positional_sites <- args[!grepl('^--', args)]
if (length(site_arg) > 1 || length(positional_sites) > 1) {
  stop('Provide at most one comma-separated site list.')
}
requested_sites <- if (length(site_arg) == 1) {
  sub('^--sites=', '', site_arg)
} else if (length(positional_sites) == 1) {
  positional_sites
} else {
  NULL
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

#-------------Predict soil temperature for 9 Ameriflux sites using the function above
site_info <- read.csv(file.path('data', 'site_info.csv'))

files_AmeriFlux_BASE <- list.files(file.path(dir_rawdata, "SiteData", "AmeriFlux_BASE"), pattern=".zip$", full.names = T)
#
files_ICOS <- list.files(
  file.path(dir_rawdata, "SiteData", "Ecosystem final quality (L2) product in ETC-Archive format - release 2025-1", "unzip"),
  pattern = "_ICOS_L2_FLUXNET_HH\\.csv$",
  full.names = TRUE
)
files_TERN <- list.files(file.path(dir_rawdata, "SiteData", "TERN", "unzip"), pattern="_TERN_[A-Z0-9]+_FLUXNET_HH\\.csv$", full.names = TRUE)

# 
id_estimate_TS <- which(site_info$estimate_Ts == 'YES')


process_site <- function(name_site, overwrite = FALSE) {
  id <- match(name_site, site_info$site_ID)
  output_file <- file.path(dir_rawdata, 'TS_RandomForest', paste0(name_site, '_TS_rfp.csv'))
  if (!overwrite && file.exists(output_file)) {
    message('Skipping ', name_site, ': output already exists.')
    return(invisible(NULL))
  }

  # TERN already supplies a measured top-soil temperature in the normalized
  # Normalized TERN file, so retain it under the output schema used downstream.
  tern_file <- files_TERN[grepl(paste0('/', name_site, '_TERN_'), files_TERN)]
  if (length(tern_file) == 1) {
    a <- read.csv(tern_file, stringsAsFactors = FALSE)
    a[a == -9999] <- NA
    if (!all(c('TIMESTAMP_START', 'TS_F_MDS_1') %in% names(a))) {
      stop(name_site, ' TERN file is missing TIMESTAMP_START or TS_F_MDS_1')
    }
    data <- data.frame(TIMESTAMP = ymd_hm(a$TIMESTAMP_START, tz = 'UTC') + 15 * 60,
                       TS_pred = a$TS_F_MDS_1)
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    write.csv(data, file = output_file, row.names = FALSE)
    return(invisible(NULL))
  }
  
  icos_file <- files_ICOS[grepl(paste0('/', name_site, '_ICOS_L2_FLUXNET_HH\\.csv$'), files_ICOS)]
  if (length(icos_file) == 1) {
    a <- read.csv(icos_file, stringsAsFactors = FALSE)
    a[a == -9999] <- NA
    required <- c('TIMESTAMP_START', 'TA_F_MDS', 'TS_F_MDS_1')
    if (!all(required %in% names(a))) {
      stop(name_site, ' ICOS file is missing required temperature columns')
    }
    timestamp <- ymd_hm(as.character(a$TIMESTAMP_START), quiet = TRUE, tz = 'UTC')
    data <- data.frame(
      TIMESTAMP = timestamp + 15 * 60,
      YEAR = year(timestamp),
      DOY = yday(timestamp),
      HOUR = hour(timestamp),
      MINUTE = minute(timestamp),
      TS = a$TS_F_MDS_1,
      TA = a$TA_F_MDS
    )
    if ('NETRAD' %in% names(a)) data$NETRAD <- a$NETRAD
  } else if (site_info$source[id] == "AmeriFlux_BASE") {
    input_files <- files_AmeriFlux_BASE[grepl(name_site, files_AmeriFlux_BASE)]
    if (length(input_files) != 1) stop(name_site, ' requires one AmeriFlux_BASE archive; found ', length(input_files))
    a <- amf_read_base(input_files, parse_timestamp=TRUE, unzip = T)
    data <- data.frame(TIMESTAMP = a$TIMESTAMP, YEAR = a$YEAR, DOY = a$DOY, HOUR = a$HOUR, MINUTE = a$MINUTE, 
                       TS = a[, trimws(site_info$TS[id])], TA = a[, trimws(site_info$TA[id])])
    if (name_site %in% c('US-Los', "US-Ced")) {
      data$NETRAD <- a$NETRAD_1_1_1
    } else if (name_site %in% c("US-SRG", "CA-Man")) {
      data$NETRAD <- a$NETRAD
    } else if (name_site %in% c("US-Ho1")) {
      data$NETRAD <- a$NETRAD_2_1_1
    } 
  } else {
    stop(name_site, ' has no AmeriFlux, ICOS, or TERN input file')
  }

  print(id)
  plot(data$TA)
  plot(data$TS)

  # gap fill TA use DOY method if needed. 
  if (!"TA_gf" %in% colnames(data)) {
    TA_gf <- data %>% group_by(DOY, HOUR, MINUTE) %>% summarise(TA_gf=mean(TA, na.rm=T))
    data   <- data %>% left_join(TA_gf, by=c("DOY", "HOUR", "MINUTE"))
  }
  if (sum(is.na(data$TA)) > 0) {
    data$TA[is.na(data$TA)] <- data$TA_gf[is.na(data$TA)]
  }

  plot(data$TA)
  
  # gap fill NETRAD if using NETRAD method
  if ("NETRAD" %in% names(data)) {
    if (!"NETRAD_gf" %in% colnames(data)) {
      gf <- data %>% group_by(DOY, HOUR, MINUTE) %>% summarise(NETRAD_gf=mean(NETRAD, na.rm=T))
      data   <- data %>% left_join(gf, by=c("DOY", "HOUR", "MINUTE"))
    }
    if (sum(is.na(data$NETRAD)) > 0) {
      data$NETRAD[is.na(data$NETRAD)] <- data$NETRAD_gf[is.na(data$NETRAD)]
    }
    plot(data$NETRAD)    
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
  plot(data$TS_pred)
  
  write.csv(data, file=output_file, row.names = F)
}

candidate_sites <- unique(c(site_info$site_ID[site_info$source == 'AmeriFlux_BASE'],
                            site_info$site_ID[id_estimate_TS],
                            sub('_ICOS_L2_FLUXNET_HH\\.csv$', '', basename(files_ICOS)),
                            sub('_TERN_[A-Z0-9]+_FLUXNET_HH\\.csv$', '', basename(files_TERN))))
sites <- if (is.null(requested_sites)) candidate_sites else trimws(unlist(strsplit(requested_sites, ',')))
unknown_sites <- setdiff(sites, candidate_sites)
if (length(unknown_sites) > 0) {
  stop('Sites are not eligible for AmeriFlux, ICOS, or TERN temperature estimation: ', paste(unknown_sites, collapse = ', '))
}
for (name_site in sites) {
  process_site(name_site, overwrite)
}

# all R2 should be higher than 0.83. 
# check TS prediction quality
# data <- read.csv(file.path('data-raw', 'TS_RandomForest', 'US-Ho2_TS_rfp.csv'))
# plot(ymd_hms(data$TIMESTAMP[140000:170000]), data$TS_pred[140000:170000])
# data %>% group_by(year(TIMESTAMP)) %>% summarise(TS=mean(TS_pred, na.rm=T))
#
