# Prepare ICOS-only data for fitting temperature-respiration curves.
# Input files are normalized by workflows/93-download-icos.py.

library(librarian)
shelf(dplyr, lubridate)
rm(list = ls())

args <- commandArgs(trailingOnly = TRUE)
overwrite <- "--overwrite" %in% args
site_arg <- args[grepl("^--sites=", args)]
positional_sites <- args[!grepl("^--", args)]
if (length(site_arg) > 1 || length(positional_sites) > 1) {
  stop("Provide at most one comma-separated site list.")
}
requested_sites <- if (length(site_arg) == 1) {
  sub("^--sites=", "", site_arg)
} else if (length(positional_sites) == 1) {
  positional_sites
} else {
  NULL
}

dir_rawdata <- "data-raw"
dir_proc <- "data-proc/respiration/ICOS"
icos_dir <- file.path(dir_rawdata, "ICOS")
site_info <- read.csv(file.path("data-core", "site_info.csv"), stringsAsFactors = FALSE)
files_ICOS <- list.files(icos_dir, pattern = "_ICOS_L2_FLUXNET_HH\\.csv$", full.names = TRUE, recursive = TRUE)

parse_removed_years <- function(value) {
  if (is.na(value) || !nzchar(trimws(value))) return(numeric())
  unlist(lapply(strsplit(value, ",")[[1]], function(part) {
    part <- trimws(part)
    if (grepl(":", part)) {
      bounds <- as.numeric(strsplit(part, ":", fixed = TRUE)[[1]])
      seq(bounds[1], bounds[2])
    } else {
      as.numeric(part)
    }
  }))
}

process_site <- function(name_site) {
  id <- match(name_site, site_info$site_ID)
  input_file <- files_ICOS[grepl(paste0("/", name_site, "_ICOS_L2_FLUXNET_HH\\.csv$"), files_ICOS)]
  if (length(input_file) != 1) {
    stop("Expected one normalized ICOS file for ", name_site, ", found ", length(input_file), ".")
  }

  output_dir <- file.path(dir_proc, name_site)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_files <- file.path(output_dir, paste0(name_site, c("_ac.csv", "_nightNEE.csv")))
  if (!overwrite && all(file.exists(output_files))) {
    message("Skipping ", name_site, ": respiration outputs already exist.")
    return(NULL)
  }

  a <- read.csv(input_file, stringsAsFactors = FALSE)
  a[a == -9999] <- NA
  required <- c(
    "TIMESTAMP_START", "TA_F_MDS", "TS_F_MDS_1", "TS_F_MDS_1_QC",
    "NEE_VUT_REF", "NEE_VUT_REF_QC", "NIGHT"
  )
  missing <- setdiff(required, names(a))
  if (length(missing) > 0) stop(name_site, " ICOS file is missing: ", paste(missing, collapse = ", "))

  timestamp_start <- ymd_hm(as.character(a$TIMESTAMP_START), quiet = TRUE, tz = "UTC")
  if (anyNA(timestamp_start)) stop(name_site, " contains invalid TIMESTAMP_START values.")
  dt <- as.numeric(timestamp_start[2] - timestamp_start[1], units = "hours")
  a$TIMESTAMP <- timestamp_start + dt * 1800
  a$YEAR <- year(a$TIMESTAMP)
  a$MONTH <- month(a$TIMESTAMP)
  a$DAY <- day(a$TIMESTAMP)
  a$DOY <- yday(a$TIMESTAMP)
  a$HOUR <- hour(a$TIMESTAMP)
  a$MINUTE <- minute(a$TIMESTAMP)

  # ICOS normalized files already contain the measured first soil-temperature layer.
  a_measure_night_complete <- a |>
    filter(!is.na(TA_F_MDS), TS_F_MDS_1_QC %in% c(0, 1, 2),
           NEE_VUT_REF_QC == 0, NIGHT == 1,
           NEE_VUT_REF > -5, NEE_VUT_REF < 30)
  if (nrow(a_measure_night_complete) == 0) {
    stop(name_site, " has no observations after the ICOS nighttime quality filter.")
  }

  southern_hemisphere <- name_site %in% c("AU-Tum", "ZA-Kru")
  if (southern_hemisphere) a$DOY[a$DOY < 183] <- a$DOY[a$DOY < 183] + 366
  if (southern_hemisphere) {
    a_measure_night_complete$DOY[a_measure_night_complete$DOY < 183] <-
      a_measure_night_complete$DOY[a_measure_night_complete$DOY < 183] + 366
  }

  NEE_yearly <- a |>
    group_by(DOY) |>
    summarise(NEE = mean(NEE_VUT_REF, na.rm = TRUE), TS = mean(TS_F_MDS_1, na.rm = TRUE), .groups = "drop")
  tmp <- NEE_yearly |> filter(NEE < max(min(NEE_yearly$NEE, na.rm = TRUE) * 0.2, -0.8))
  if (nrow(tmp) < 8) stop(name_site, " has too few seasonal points to estimate growing season.")
  gStart <- as.integer(mean(tmp$DOY[7])) - 4
  gEnd <- as.integer(mean(tmp$DOY[nrow(tmp) - 6])) + 4
  nonnegative_ts <- which(NEE_yearly$TS >= 0)
  if (length(nonnegative_ts) > 0) gStart <- max(gStart, min(NEE_yearly$DOY[nonnegative_ts]))
  if (!is.na(site_info$gStart[id])) gStart <- as.numeric(site_info$gStart[id])
  if (!is.na(site_info$gEnd[id])) gEnd <- as.numeric(site_info$gEnd[id])
  tStart <- quantile(tmp$TS, 0.025, na.rm = TRUE)
  tEnd <- quantile(tmp$TS, 0.975, na.rm = TRUE)

  yStart <- min(a_measure_night_complete$YEAR)
  yEnd <- max(a_measure_night_complete$YEAR)
  a_check_gaps <- a_measure_night_complete[, c("TIMESTAMP", "DOY")]
  gs_dates <- do.call(rbind, lapply(yStart:yEnd, function(y) data.frame(
    TIMESTAMP = as.POSIXct(paste0(y, "-01-01"), tz = "UTC") + (c(gStart, gEnd) - 1) * 86400 + dt * 1800,
    DOY = c(gStart, gEnd)
  )))
  a_check_gaps <- bind_rows(a_check_gaps, gs_dates) |> distinct() |> arrange(TIMESTAMP)
  good_years <- a_check_gaps |>
    mutate(growing_year = if_else(DOY <= 366, year(TIMESTAMP), year(TIMESTAMP) - 1)) |>
    arrange(growing_year, TIMESTAMP) |>
    group_by(growing_year) |>
    filter(DOY >= gStart, DOY <= gEnd) |>
    mutate(gap = as.numeric(difftime(TIMESTAMP, lag(TIMESTAMP), units = "days"))) |>
    summarise(gap_max = max(gap, na.rm = TRUE),
              gap_total = sum(if_else(gap > 14, gap, 0), na.rm = TRUE) / (gEnd - gStart + 1),
              .groups = "drop") |>
    filter(gap_max < max(31, (gEnd - gStart + 1) * 0.225),
           gap_total < max(1 / 3, max(31, (gEnd - gStart + 1) * 0.225) / (gEnd - gStart + 1))) |>
    pull(growing_year)
  good_years <- setdiff(good_years, parse_removed_years(site_info$year_removed[id]))
  if (length(good_years) == 0) stop(name_site, " has no complete growing years.")

  a_measure_night_complete <- a_measure_night_complete |>
    mutate(growing_year = if_else(DOY <= 366, YEAR, YEAR - 1)) |>
    filter(growing_year %in% good_years) |>
    transmute(YEAR, MONTH, DAY, DOY, HOUR, MINUTE,
              NEE = NEE_VUT_REF, TA = TA_F_MDS, TS = TS_F_MDS_1,
              SWC = if ("SWC_F_MDS_1" %in% names(a)) SWC_F_MDS_1 else NA_real_)
  if (nrow(a_measure_night_complete) == 0) stop(name_site, " has no observations in complete growing years.")

  ac <- a |>
    transmute(YEAR, MONTH, DAY, DOY, HOUR, MINUTE,
              NEE = NEE_VUT_REF, NEE_QC = NEE_VUT_REF_QC,
              TA = TA_F_MDS, TS = TS_F_MDS_1,
              SWC = if ("SWC_F_MDS_1" %in% names(a)) SWC_F_MDS_1 else NA_real_,
              NEE_uStar_f = NEE_VUT_REF, daytime = NIGHT != 1,
              SW_IN = if ("SW_IN_F_MDS" %in% names(a)) SW_IN_F_MDS else NA_real_,
              GPP_DT = NA_real_) |>
    filter(YEAR >= min(a_measure_night_complete$YEAR), YEAR <= max(a_measure_night_complete$YEAR))
  write.csv(ac, output_files[1], row.names = FALSE)
  write.csv(a_measure_night_complete, output_files[2], row.names = FALSE)
  data.frame(site_ID = name_site, gStart, gEnd, tStart = max(tStart, 0), tEnd, nyear = length(good_years))
}

candidate_sites <- sub(
  "_ICOS_L2_FLUXNET_HH\\.csv$", "", basename(files_ICOS)
)
sites <- if (is.null(requested_sites)) candidate_sites else trimws(unlist(strsplit(requested_sites, ",")))
unknown_sites <- setdiff(sites, candidate_sites)
if (length(unknown_sites) > 0) stop("Sites are not configured for ICOS2025 processing: ", paste(unknown_sites, collapse = ", "))

feature_file <- file.path("data-proc", "features", "growing_season_feature_ICOS.csv")
feature_gs <- if (file.exists(feature_file)) read.csv(feature_file) else {
  data.frame(site_ID = character(), gStart = double(), gEnd = double(), tStart = double(), tEnd = double(), nyear = integer())
}
if (overwrite) feature_gs <- feature_gs[!feature_gs$site_ID %in% sites, , drop = FALSE]
for (name_site in sites) {
  if (!overwrite && name_site %in% feature_gs$site_ID) {
    message("Skipping ", name_site, ": growing-season feature already exists.")
    next
  }
  feature <- process_site(name_site)
  if (!is.null(feature)) feature_gs <- bind_rows(feature_gs, feature)
}
write.csv(feature_gs, feature_file, row.names = FALSE)
