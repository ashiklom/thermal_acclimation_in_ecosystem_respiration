# Small real-data examples for the analysis website's documentation pages
# (docs-ts-estimators.html, docs-sensor-screen.html), read from the raw input
# data (not from any pipeline result): DE-Hai (Hainich, a deciduous broadleaf
# forest; FLUXNET2025 product), whose record has five soil-temperature sensors,
# TS_F_MDS_1 .. TS_F_MDS_5, numbered from the shallowest.
#
# Air temperature is TA_F_MDS (as step 01 uses it); soil temperature only where
# measured (quality flag 0, as ts_truth() trains on). Time stamps are the
# middle of each half hour, as step 01 sets them. The growing season of the
# screen is fixed here to June-August (day of year 152-243); the pipeline uses
# each site's own gStart..gEnd.
#
# Writes analysis-website/static/docs/:
#   de-hai-halfhourly.json  three summer weeks of half-hourly air temperature
#                           and the five sensors
#   de-hai-daily.json       daily means of air temperature and sensors 1 and 5
#                           over six years
#   de-hai-screen.json      the sensor screen (ts_screen_series()) of each of
#                           the five sensors: summary, yearly diagnostics,
#                           windows and the mean diurnal cycle
#
# Usage (from the repository root): pixi run Rscript scripts/make-docs-examples.R

for (f in c("paths", "read-raw", "ts-screen")) source(file.path("R", paste0(f, ".R")))

SITE <- "DE-Hai"
SENSORS <- paste0("TS_F_MDS_", 1:5)
DAILY_YEARS <- 2012:2017
HALFHOURLY_DAYS <- c(as.Date("2016-06-01"), as.Date("2016-06-21"))
SEASON <- c(152, 243)
out_dir <- file.path("analysis-website", "static", "docs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

site <- utils::read.csv(file.path("data", "site_info.csv"), fileEncoding = "UTF-8-BOM")
site <- site[site$site_ID == SITE, ]
a <- read_fluxnet_record(SITE, site$source)
a <- a[, c("TIMESTAMP_START", "TA_F_MDS", SENSORS, paste0(SENSORS, "_QC"))]
a[a == -9999] <- NA
for (s in SENSORS) a[[s]][!(a[[paste0(s, "_QC")]] %in% 0)] <- NA

dt <- lubridate::ymd_hm(a$TIMESTAMP_START[2]) - lubridate::ymd_hm(a$TIMESTAMP_START[1])
timestamp <- lubridate::ymd_hm(a$TIMESTAMP_START) + dt / 2
base <- data.frame(TIMESTAMP = timestamp, YEAR = lubridate::year(timestamp), DOY = lubridate::yday(timestamp),
                   HOUR = lubridate::hour(timestamp), MINUTE = lubridate::minute(timestamp), TA = a$TA_F_MDS)
date <- as.Date(timestamp)
r <- function(x, digits = 2) round(x, digits)
write <- function(x, name) {
  path <- file.path(out_dir, name)
  jsonlite::write_json(x, path, digits = 4, auto_unbox = TRUE, na = "null")
  message(path, ": ", round(file.size(path) / 1024), " KB")
}

# three summer weeks, half-hourly
hh <- date >= HALFHOURLY_DAYS[1] & date <= HALFHOURLY_DAYS[2]
write(c(list(site = SITE, time = format(timestamp[hh], "%Y-%m-%d %H:%M"), TA = r(base$TA[hh])),
        stats::setNames(lapply(SENSORS, function(s) r(a[[s]][hh])), SENSORS)),
      "de-hai-halfhourly.json")

# daily means (days with at least 40 of 48 values)
daily_mean <- function(x) {
  keep <- lubridate::year(date) %in% DAILY_YEARS
  n <- tapply(!is.na(x[keep]), date[keep], sum)
  m <- tapply(x[keep], date[keep], mean, na.rm = TRUE)
  ifelse(n >= 40, m, NA_real_)
}
days <- sort(unique(date[lubridate::year(date) %in% DAILY_YEARS]))
write(list(site = SITE, date = format(days), TA = r(unname(daily_mean(base$TA))),
           TS_F_MDS_1 = r(unname(daily_mean(a$TS_F_MDS_1))), TS_F_MDS_5 = r(unname(daily_mean(a$TS_F_MDS_5)))),
      "de-hai-daily.json")

# the screen of each sensor
dt_secs <- as.numeric(dt, units = "secs")
screen <- lapply(SENSORS, function(s) {
  x <- ts_screen_series(base, a[[s]], SEASON[1], SEASON[2], dt_secs)
  x$windows$start <- format(x$windows$start, "%Y-%m-%d %H:%M")
  x$windows$end <- format(x$windows$end, "%Y-%m-%d %H:%M")
  x
})
names(screen) <- SENSORS
write(list(site = SITE, season = SEASON, sensors = screen), "de-hai-screen.json")
