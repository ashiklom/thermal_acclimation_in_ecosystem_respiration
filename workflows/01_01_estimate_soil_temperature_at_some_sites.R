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

source("R/utils.R")

DIR_RAWDATA <- 'data-raw'
DIR_PROC <- 'data-proc/soil-temperature'
RANDOM_SEED <- 222

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

# requested_sites <- c("US-Uaf", "AU-Tum", "BE-Bra", "DE-Hai")

#--------A function to predict soil temperature
# inputs: a data frame data with 2-3 columns (TS, TA, NETRAD), the last column is optional
# output: the same data frame data, but with one added column TS_pred
predict_soil_temp <- function(data, use_NETRAD) {

  # separate data into trained or tested
  set.seed(RANDOM_SEED)
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
all_site_info <- get_site_info()

files_FLUXNET2015  <- list.files(path=file.path(DIR_RAWDATA, 'FLUXNET'), pattern = "_FLUXNET2015_FULLSET_(HH|HR)_", full.names = TRUE, recursive = TRUE)
files_FLUXNET2020  <- list.files(path=file.path(DIR_RAWDATA, 'FLUXNET'), pattern = "_FLUXNET2015_FULLSET_(HH|HR)_", full.names = TRUE, recursive = TRUE)
files_ICOS_after2020 <- list.files(
  file.path(DIR_RAWDATA, "ICOS"),
  pattern = "_ICOS_L2_FLUXNET_HH\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)
#
files_FLUXNET2025 <- list.files(file.path(DIR_RAWDATA, "FLUXNET"), pattern="FLUXMET_HH.*\\.csv$", full.names = T, recursive = TRUE)
files_ICOS2025 <- list.files(
  file.path(DIR_RAWDATA, "ICOS"),
  pattern = "_ICOS_L2_FLUXNET_HH\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)
files_ICOS <- list.files(
  file.path(DIR_RAWDATA, "ICOS"),
  pattern = "_ICOS_L2_FLUXNET_HH\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)
files_TERN <- list.files(file.path(DIR_RAWDATA, "TERN"), pattern="_TERN_[A-Z0-9]+_FLUXNET_HH\\.csv$", full.names = TRUE, recursive = TRUE)

# 
id_estimate_TS <- which(all_site_info$estimate_Ts == 'YES')


process_site <- function(name_site, overwrite = FALSE) {
  # name_site <- "AU-Tum"   # TERN example
  # name_site <- "BE-Bra"   # ICOS example
  # name_site <- "US-Ced"   # Ameriflux example
  # name_site <- "US-Uaf"
  site_info <- get_site_info(name_site)
  output_source <- site_info[["source"]]
  output_file <- file.path(DIR_PROC, name_site, paste0(name_site, '_TS_rfp.csv'))
  if (!overwrite && file.exists(output_file)) {
    message('Skipping ', name_site, ': output already exists.')
    return(invisible(NULL))
  }

  # TERN already supplies a measured top-soil temperature in the normalized
  # TERN file, so effectively, just change the column name and return; no
  # calculations necessary.
  if (output_source == "TERN") {
    tern_file <- file.path(DIR_RAWDATA, "TERN", name_site, sprintf(
      "%s_TERN_L3_FLUXNET_HH.csv",
      name_site
    ))
    stopifnot(file.exists(tern_file))
    a <- read.csv(tern_file, stringsAsFactors = FALSE)
    a[a == -9999] <- NA
    if (!all(c('TIMESTAMP_START', 'TS_F_MDS_1') %in% names(a))) {
      stop(name_site, ' TERN file is missing TIMESTAMP_START or TS_F_MDS_1')
    }
    data <- data.frame(
      TIMESTAMP = ymd_hm(a$TIMESTAMP_START, tz = 'UTC') + 15 * 60,
      TS_pred = a$TS_F_MDS_1
    )
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    write.csv(data, file = output_file, row.names = FALSE)
    return(invisible(NULL))
  }

  if (output_source == "ICOS") {

    icos_file <- file.path(DIR_RAWDATA, "ICOS", name_site, sprintf(
      "%s_ICOS_L2_FLUXNET_HH.csv",
      name_site
    ))
    stopifnot(file.exists(icos_file))
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

  } else if (output_source == "AmeriFlux_BASE") {

    if (is.na(site_info[["TS"]]) || is.na(site_info[["TA"]])) {
      message('Skipping ', name_site, ': missing TS or TA column name in site_info.')
      return(invisible(NULL))
    }
    # NOTE: Cannot rely on exact file paths here because versions vary. So need
    # to find the files. 
    input_files <- list.files(
      file.path(DIR_RAWDATA, "Ameriflux", name_site),
      pattern = sprintf("^AMF_%s_BASE.*\\.zip$", name_site),
      full.names = TRUE,
      recursive = TRUE
    )
    if (length(input_files) != 1) {
      stop(name_site, ' requires exactly one AmeriFlux_BASE archive; found ', length(input_files))
    }
    a <- amf_read_base(input_files, parse_timestamp=TRUE, unzip = T)
    if (name_site == 'US-Ha2') {
      a$TA_A_1_1 <- a$TA_1_1_1
      a$TA_A_1_1[is.na(a$TA_A_1_1)] <- a$TA_2_1_1[is.na(a$TA_A_1_1)]
    }
    data <- data.frame(TIMESTAMP = a$TIMESTAMP, YEAR = a$YEAR, DOY = a$DOY, HOUR = a$HOUR, MINUTE = a$MINUTE, 
                       TS = a[, trimws(site_info$TS)], TA = a[, trimws(site_info$TA)])
    if (!is.na(site_info$netrad_column)) {
      data$NETRAD <- a[[site_info$netrad_column]]
    }

  } else if (output_source == "FLUXNET")  {

    data_source <- site_info$source[id]
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
        a <- bind_rows(a, a_tmp[irow_start:nrow(a_tmp), ])
      }
    }
    rm(a_tmp)
    # -9999 means missing information.
    a[a==-9999] <- NA

    dt <- ymd_hm(a$TIMESTAMP_START[2]) - ymd_hm(a$TIMESTAMP_START[1])
    a$TIMESTAMP  <- ymd_hm(a$TIMESTAMP_START) + dt / 2
    a$DOY <- yday(a$TIMESTAMP)
    data <- data.frame(TIMESTAMP = a$TIMESTAMP, YEAR = year(a$TIMESTAMP), DOY = a$DOY,
                       HOUR = hour(a$TIMESTAMP), MINUTE = minute(a$TIMESTAMP), TS = a$TS_F_MDS_1, TA = a$TA_F_MDS, NETRAD=a$NETRAD)
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

  message("Preparing to fit data for ", shQuote(name_site))
  # if (interactive()) {
  #   plot(data$TA, pch = ".")
  #   plot(data$TS, pch = ".")
  # }

  # gap fill TA use DOY method if needed. 
  if (! "TA_gf" %in% colnames(data)) {
    TA_gf <- data %>% group_by(DOY, HOUR, MINUTE) %>% summarise(TA_gf=mean(TA, na.rm=T))
    data   <- data %>% left_join(TA_gf, by=c("DOY", "HOUR", "MINUTE"))
  }

  if (any(is.na(data$TA))) {
    data$TA[is.na(data$TA)] <- data$TA_gf[is.na(data$TA)]
  }

  # if (interactive()) {
  #   plot(data$TA, pch = ".")
  # }
  
  # gap fill NETRAD if using NETRAD method
  if ("NETRAD" %in% names(data)) {
    if (!"NETRAD_gf" %in% colnames(data)) {
      gf <- data %>% group_by(DOY, HOUR, MINUTE) %>% summarise(NETRAD_gf=mean(NETRAD, na.rm=T))
      data   <- data %>% left_join(gf, by=c("DOY", "HOUR", "MINUTE"))
    }
    if (any(is.na(data$NETRAD))) {
      data$NETRAD[is.na(data$NETRAD)] <- data$NETRAD_gf[is.na(data$NETRAD)]
    }
    # plot(data$NETRAD)    
  }
  
  estimate_ts_method <- site_info[["estimate_ts_method"]]
  if (!is.na(estimate_ts_method) && estimate_ts_method == "NETRAD") {
    # use NETRAD because it is available
    message("Estimating TS using NETRAD")
    data <- predict_soil_temp(data, use_NETRAD = TRUE)
  } else if (!is.na(estimate_ts_method) && estimate_ts_method == "linear regression") {
    # use simple linear regression because these sites have 1-year or two-year TS, and NETRAD is incomplete
    message("Estimating TS using linear regression")
    lm <- lm(data=data, TS ~ TA + NETRAD)
    data$TS_pred <- predict(lm, data)
  } else {
    # no netrad at this site
    message("Estimating TS using air temperature")
    lm <- lm(data=data[data$TA>0, ], TS ~ TA)
    data$TS_pred <- predict(lm, data)
  } 
  # plot(data$TS_pred, pch = ".")
  
  write.csv(data, file=output_file, row.names = F)
}

candidate_sites <- unique(c(all_site_info$site_ID[all_site_info$source == 'AmeriFlux_BASE'],
                            all_site_info$site_ID[id_estimate_TS],
                            sub('_ICOS_L2_FLUXNET_HH\\.csv$', '', basename(files_ICOS)),
                            sub('_TERN_[A-Z0-9]+_FLUXNET_HH\\.csv$', '', basename(files_TERN))))
sites <- if (is.null(requested_sites)) candidate_sites else trimws(unlist(strsplit(requested_sites, ',')))
unknown_sites <- setdiff(sites, candidate_sites)
if (length(unknown_sites) > 0) {
  stop('Sites are not eligible for AmeriFlux, ICOS, TERN, or FLUXNET temperature estimation: ', paste(unknown_sites, collapse = ', '))
}
for (name_site in sites) {
  process_site(name_site, overwrite)
}

# all R2 should be higher than 0.83. 
# check TS prediction quality
# data <- read.csv(file.path('data-proc', 'soil-temperature', 'Ameriflux', 'US-Ho2', 'US-Ho2_TS_rfp.csv'))
# plot(ymd_hms(data$TIMESTAMP[140000:170000]), data$TS_pred[140000:170000])
# data %>% group_by(year(TIMESTAMP)) %>% summarise(TS=mean(TS_pred, na.rm=T))
#
