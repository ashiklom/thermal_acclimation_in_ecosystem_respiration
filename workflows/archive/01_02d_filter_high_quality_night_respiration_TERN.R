# Prepare TERN data for fitting temperature-respiration curves.
# Input files are normalized by workflows/94-download-tern.py.

library(librarian)
shelf(dplyr, lubridate)
library(optparse)
rm(list = ls())

option_list <- list(
  make_option("--sites", type = "character", default = NULL,
              help = "Comma-separated list of site IDs to process"),
  make_option("--overwrite", action = "store_true", default = FALSE,
              help = "Overwrite existing output files")
)
parser <- OptionParser(description = "Prepare TERN data for temperature-respiration curve fitting",
                       option_list = option_list)
parsed <- parse_args(parser, commandArgs(trailingOnly = TRUE))

overwrite <- parsed$overwrite
requested_sites <- if (!is.null(parsed$sites)) trimws(unlist(strsplit(parsed$sites, ","))) else NULL

dir_rawdata <- "data-raw"
dir_proc <- "data-proc/respiration/TERN"
tern_dir <- file.path(dir_rawdata, "TERN")
files_TERN <- list.files(tern_dir, pattern = "_TERN_[A-Z0-9]+_FLUXNET_HH\\.csv$", full.names = TRUE, recursive = TRUE)
if (length(files_TERN) == 0) stop("No normalized TERN files found in ", tern_dir)

parse_removed_years <- function(value) {
  if (length(value) == 0 || is.na(value) || !nzchar(trimws(value))) return(numeric())
  unlist(lapply(strsplit(value, ",", fixed = TRUE)[[1]], function(part) {
    part <- trimws(part)
    if (grepl(":", part, fixed = TRUE)) {
      bounds <- as.numeric(strsplit(part, ":", fixed = TRUE)[[1]])
      seq(bounds[1], bounds[2])
    } else as.numeric(part)
  }))
}

process_site <- function(name_site) {
  input_file <- files_TERN[grepl(paste0("/", name_site, "_TERN_"), files_TERN)]
  if (length(input_file) != 1) stop("Expected one normalized TERN file for ", name_site, ", found ", length(input_file), ".")
  output_dir <- file.path(dir_proc, name_site)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_files <- file.path(output_dir, paste0(name_site, c("_ac.csv", "_nightNEE.csv")))
  if (!overwrite && all(file.exists(output_files))) return(NULL)

  a <- read.csv(input_file, stringsAsFactors = FALSE)
  a[a == -9999] <- NA
  required <- c("TIMESTAMP_START", "TA_F_MDS", "TS_F_MDS_1", "TS_F_MDS_1_QC", "NEE_VUT_REF", "NEE_VUT_REF_QC", "NIGHT")
  missing <- setdiff(required, names(a))
  if (length(missing) > 0) stop(name_site, " TERN file is missing: ", paste(missing, collapse = ", "))

  timestamp_start <- ymd_hm(as.character(a$TIMESTAMP_START), quiet = TRUE, tz = "UTC")
  if (anyNA(timestamp_start)) stop(name_site, " contains invalid timestamps.")
  dt <- as.numeric(timestamp_start[2] - timestamp_start[1], units = "hours")
  a$TIMESTAMP <- timestamp_start + dt * 1800
  a$YEAR <- year(a$TIMESTAMP); a$MONTH <- month(a$TIMESTAMP); a$DAY <- day(a$TIMESTAMP)
  a$DOY <- yday(a$TIMESTAMP); a$HOUR <- hour(a$TIMESTAMP); a$MINUTE <- minute(a$TIMESTAMP)

  measured <- a %>% filter(!is.na(TA_F_MDS), TS_F_MDS_1_QC %in% c(0, 1, 2), NEE_VUT_REF_QC == 0,
                            NIGHT == 1, NEE_VUT_REF > -5, NEE_VUT_REF < 30)
  if (nrow(measured) == 0) stop(name_site, " has no observations after the TERN nighttime quality filter.")
  yearly <- a %>% group_by(DOY) %>% summarise(NEE = mean(NEE_VUT_REF, na.rm = TRUE), TS = mean(TS_F_MDS_1, na.rm = TRUE), .groups = "drop")
  tmp <- yearly %>% filter(NEE < max(min(NEE, na.rm = TRUE) * 0.2, -0.8))
  if (nrow(tmp) < 8) stop(name_site, " has too few seasonal points to estimate growing season.")
  gStart <- as.integer(mean(tmp$DOY[7])) - 4; gEnd <- as.integer(mean(tmp$DOY[nrow(tmp) - 6])) + 4
  nonnegative_ts <- which(yearly$TS >= 0)
  if (length(nonnegative_ts) > 0) gStart <- max(gStart, min(yearly$DOY[nonnegative_ts]))
  tStart <- quantile(tmp$TS, 0.025, na.rm = TRUE); tEnd <- quantile(tmp$TS, 0.975, na.rm = TRUE)
  good_years <- unique(measured$YEAR)
  ac <- a %>% transmute(YEAR, MONTH, DAY, DOY, HOUR, MINUTE, NEE = NEE_VUT_REF,
                        NEE_QC = NEE_VUT_REF_QC, TA = TA_F_MDS, TS = TS_F_MDS_1,
                        SWC = if ("SWC_F_MDS_1" %in% names(a)) SWC_F_MDS_1 else NA_real_,
                        NEE_uStar_f = NEE_VUT_REF, daytime = NIGHT != 1,
                        SW_IN = if ("SW_IN_F_MDS" %in% names(a)) SW_IN_F_MDS else NA_real_, GPP_DT = NA_real_)
  measured <- measured %>% mutate(growing_year = if_else(DOY <= 366, YEAR, YEAR - 1)) %>% filter(growing_year %in% good_years) %>%
    transmute(YEAR, MONTH, DAY, DOY, HOUR, MINUTE, NEE = NEE_VUT_REF, TA = TA_F_MDS, TS = TS_F_MDS_1,
              SWC = if ("SWC_F_MDS_1" %in% names(a)) SWC_F_MDS_1 else NA_real_)
  write.csv(ac, output_files[1], row.names = FALSE)
  write.csv(measured, output_files[2], row.names = FALSE)
  data.frame(site_ID = name_site, gStart, gEnd, tStart = max(tStart, 0), tEnd, nyear = length(good_years))
}

candidate_sites <- sub("_TERN_[A-Z0-9]+_FLUXNET_HH\\.csv$", "", basename(files_TERN))
sites <- if (is.null(requested_sites)) candidate_sites else trimws(unlist(strsplit(requested_sites, ",", fixed = TRUE)))
unknown_sites <- setdiff(sites, candidate_sites)
if (length(unknown_sites) > 0) stop("Sites are not downloaded: ", paste(unknown_sites, collapse = ", "))
feature_file <- file.path("data-proc", "features", "growing_season_feature_TERN.csv")
feature_gs <- if (file.exists(feature_file)) read.csv(feature_file) else data.frame(site_ID=character(), gStart=double(), gEnd=double(), tStart=double(), tEnd=double(), nyear=integer())
if (overwrite) feature_gs <- feature_gs[!feature_gs$site_ID %in% sites, , drop = FALSE]
for (name_site in sites) if (overwrite || !name_site %in% feature_gs$site_ID) {
  feature <- process_site(name_site)
  if (!is.null(feature)) feature_gs <- bind_rows(feature_gs, feature)
}
write.csv(feature_gs, feature_file, row.names = FALSE)
