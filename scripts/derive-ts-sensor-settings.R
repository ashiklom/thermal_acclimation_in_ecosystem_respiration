# Add to data/site_settings.csv, for every site, the raw columns the
# soil-temperature candidates of R/ts-columns.R start from:
#
#   ts_sensor             the soil-temperature sensor ("" if the site has none):
#                         AmeriFlux sites' site_info$TS, except where that is
#                         an air temperature (BR-Ma2, BR-Sa1, used in place of
#                         soil temperature); FLUXNET-format sites' TS_F_MDS_1,
#                         or TS_F_MDS_2 where the manuscript took the second
#                         layer (CZ-Stn, DE-Hte)
#   ts_sensor_truth       the column holding the sensor's real readings, on which
#                         estimators are trained and scored ("" if none):
#                         ts_sensor itself, unless it is a PI gap-filled
#                         product (TS_PI_F_*), whose filled values cannot be
#                         told apart; then its non-filled counterpart
#                         (TS_PI_<position> or TS_1_1_1) if one exists and
#                         agrees with it wherever both are present (|diff| <
#                         0.05 deg C at 95% of those records), else none
#   ts_sensor_first_year  first year the sensor is trusted (NA: all): the
#                         manuscript replaced or discarded the earlier data at
#                         FI-Sod (sensor moved; before 2006), FR-Bil (before
#                         2021) and FR-Pue (before 2016)
#   netrad                net radiation ("" if none): the manuscript's column
#                         where it used one (US-Los, US-Ced, US-SRG, CA-Man,
#                         US-Ho1); otherwise the first present of NETRAD,
#                         NETRAD_PI_F, NETRAD_1_1_1, NETRAD_PI_F_1_1_1, and any
#                         other NETRAD_* column
#
# Sites of the site table (default data/site_info_new.csv) without a row in
# data/site_settings.csv (sites added later) get one, with the settings of a
# site without the manuscript's corrections: ts_regression "none",
# trim_last_growing_year = season_wrap (the wrapped season's last, incomplete
# year, as at AU-Tum and ZA-Kru), tstart_floor 0; the rows already there are
# left as they are. --all derives the four columns above for every site of the
# table instead (the manuscript's sites: --site-info=data/site_info.csv).
#
# Usage (from the repository root):
#   Rscript scripts/derive-ts-sensor-settings.R [--site-info=FILE] [--all]

source(file.path("R", "paths.R"))
source(file.path("R", "read-raw.R"))

args <- commandArgs(trailingOnly = TRUE)
hit <- grep("^--site-info=", args, value = TRUE)
site_info <- utils::read.csv(if (length(hit)) sub("^--site-info=", "", hit) else file.path("data", "site_info_new.csv"))
settings <- utils::read.csv(file.path("data", "site_settings.csv"))
all_sites <- "--all" %in% args
derive_sites <- if (all_sites) site_info$site_ID else setdiff(site_info$site_ID, settings$site_ID)

raw_columns <- function(site) {
  if (site$source == "AmeriFlux_BASE") {
    f <- raw_product_file(site$site_ID, "AmeriFlux_BASE")
    inner <- grep("_BASE_.*\\.csv$", utils::unzip(f, list = TRUE)$Name, value = TRUE)
    # half-hourly where there are several (as amerifluxr::amf_read_base() reads)
    if (length(inner) > 1) inner <- grep("_BASE_HH_", inner, value = TRUE)
    stopifnot(length(inner) == 1)
    con <- unz(f, inner)
    on.exit(close(con))
    # two metadata lines, then the header
    strsplit(readLines(con, n = 3)[3], ",")[[1]]
  } else {
    products <- source_products(site$source)
    unique(unlist(lapply(products, function(p) strsplit(readLines(raw_product_file(site$site_ID, p), n = 1), ",")[[1]])))
  }
}

manuscript_netrad <- c("US-Los" = "NETRAD_1_1_1", "US-Ced" = "NETRAD_1_1_1", "US-SRG" = "NETRAD",
                       "CA-Man" = "NETRAD", "US-Ho1" = "NETRAD_2_1_1")
first_year <- c("FI-Sod" = 2006, "FR-Bil" = 2021, "FR-Pue" = 2016)

derived <- do.call(rbind, lapply(derive_sites, function(s) {
  site <- site_info[site_info$site_ID == s, ]
  cols <- raw_columns(site)
  if (site$source == "AmeriFlux_BASE") {
    ts_sensor <- trimws(site$TS)
    if (startsWith(ts_sensor, "TA")) ts_sensor <- ""
  } else {
    ts_sensor <- if (s %in% c("CZ-Stn", "DE-Hte")) "TS_F_MDS_2" else "TS_F_MDS_1"
  }
  stopifnot(ts_sensor == "" || ts_sensor %in% cols)
  ts_sensor_truth <- ts_sensor
  if (grepl("^TS_PI_F_", ts_sensor)) {
    candidates <- intersect(c(sub("^TS_PI_F_", "TS_PI_", ts_sensor), "TS_1_1_1"), cols)
    a <- if (length(candidates)) read_ameriflux_base(s) else NULL
    agrees <- function(x) {
      both <- !is.na(a[[ts_sensor]]) & !is.na(a[[x]]) & a[[ts_sensor]] != -9999 & a[[x]] != -9999
      sum(both) > 1000 && mean(abs(a[[ts_sensor]][both] - a[[x]][both]) < 0.05) >= 0.95
    }
    ok <- Filter(agrees, candidates)
    ts_sensor_truth <- if (length(ok)) ok[[1]] else ""
    message(s, ": ", ts_sensor, " is gap-filled; real readings: ", if (ts_sensor_truth == "") "none" else ts_sensor_truth)
  }
  netrad <- if (s %in% names(manuscript_netrad)) manuscript_netrad[[s]] else {
    preferred <- intersect(c("NETRAD", "NETRAD_PI_F", "NETRAD_1_1_1", "NETRAD_PI_F_1_1_1"), cols)
    others <- grep("^NETRAD_", cols, value = TRUE)
    c(preferred, others, "")[1]
  }
  stopifnot(netrad == "" || netrad %in% cols)
  data.frame(site_ID = s, ts_sensor = ts_sensor, ts_sensor_truth = ts_sensor_truth,
             ts_sensor_first_year = if (s %in% names(first_year)) first_year[[s]] else NA_integer_,
             netrad = netrad)
}))

if (all_sites) {
  settings <- settings[, setdiff(names(settings), c("ts_sensor", "ts_sensor_truth", "ts_sensor_first_year", "netrad"))]
  settings <- merge(settings, derived, by = "site_ID", sort = FALSE)
  settings <- settings[match(site_info$site_ID, settings$site_ID), ]
} else if (length(derive_sites)) {
  wrap <- site_info$season_wrap[match(derived$site_ID, site_info$site_ID)]
  added <- data.frame(site_ID = derived$site_ID, ts_regression = "none", trim_last_growing_year = wrap, tstart_floor = 0L)
  settings <- rbind(settings, merge(added, derived, by = "site_ID", sort = FALSE)[, names(settings)])
  message(length(derive_sites), " sites added: ", paste(derive_sites, collapse = ", "))
}
utils::write.csv(settings, file.path("data", "site_settings.csv"), row.names = FALSE)
message("sites without a soil-temperature sensor: ", paste(settings$site_ID[settings$ts_sensor == ""], collapse = ", "))
message("sites without its real readings: ", paste(settings$site_ID[settings$ts_sensor_truth == ""], collapse = ", "))
message("sites without net radiation: ", paste(settings$site_ID[settings$netrad == ""], collapse = ", "))
