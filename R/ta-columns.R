# Air temperature in the soil-temperature role (refactor-v2 stage S22).
#
# The fit matrix (R/ts-fits.R) runs both TAS models with a "temperature"
# column in the soil-temperature role. These columns put air temperature in
# that role, for every site, including the added sites without soil
# temperature (data/site_info_ta_only.csv), which have only these:
#
#   TA_measured  air temperature as measured, not gap-filled: FLUXNET-format
#                sites' TA_F_MDS where TA_F_MDS_QC is 0; AmeriFlux BASE sites'
#                air temperature as step 01 reads it (the site table's TA,
#                with the manuscript's repairs; a PI gap-filled product such as
#                TA_PI_F cannot be told apart)
#   TA_mem3d     the mean of TA_measured's daily means over the last 3, 7 and
#   TA_mem7d     30 days (a complete calendar, the days with data), the
#   TA_mem30d    memory scales of the soil-temperature memory estimators
#                (ts_memory_features()). There each window is a predictor a
#                forest weights against the soil sensor; here, without a
#                sensor to weight against, each window is a column of its
#                own. As there, a window includes the whole current day,
#                hours after the record's too.
#
# Each column passes through the soil-temperature columns' machinery
# (ts_column_tables(), R/ts-qualify.R): where it is usable, the growing-season
# start, the tStart/tEnd climatology, the gap screen of the years, the 2 deg C
# floors and the control year. NEE, u* filtering and the growing-season end
# are step 01's, as for every column. The columns are not ranked against the
# soil sensor (R/ts-ranking.R); every site gets all four.
#
# Their fits, climate statistics and scenarios are targets of their own
# (ta_*, _targets.R) beside the soil-temperature columns' (ts_*), so that
# adding them leaves every existing fit as it is.

#' The air-temperature columns
#'
#' @noRd
TA_COLUMNS <- c("TA_measured", "TA_mem3d", "TA_mem7d", "TA_mem30d")

#' Read the table of the sites without soil temperature
#'
#' read_site_info(), with each column of the type the site table has (a column that is empty
#' throughout reads as logical; an empty text is "", as in the site table).
#'
#' @param path data/site_info_ta_only.csv.
#' @param site_info The site table (the pipeline's site_info).
#' @return The table.
#' @noRd
read_site_info_ta_only <- function(path, site_info) {
  x <- read_site_info(path)
  if (!identical(names(x), names(site_info))) stop(path, ": not the site table's columns", call. = FALSE)
  for (col in names(x)) {
    x[[col]] <- match.fun(paste0("as.", class(site_info[[col]])[1]))(x[[col]])
    if (is.character(x[[col]])) x[[col]][is.na(x[[col]])] <- ""
  }
  x
}

#' Quality flags of a site's air temperature
#'
#' FLUXNET-format sites' TA_F_MDS_QC by timestamp, from the raw record (as read_ts_sensor()
#' reads it); NULL at AmeriFlux BASE sites, whose air temperature carries no gap-fill flag.
#'
#' @param site The site's row of the site table.
#' @param base_dir Root of the raw site data.
#' @return A data frame of TIMESTAMP and TA_QC, or NULL.
#' @noRd
read_ta_qc <- function(site, base_dir = path_original("SiteData")) {
  if (site$source == "AmeriFlux_BASE") return(NULL)
  a <- read_fluxnet_record(site$site_ID, site$source, base_dir)
  if (!"TA_F_MDS_QC" %in% names(a)) stop(site$site_ID, ": the record has no TA_F_MDS_QC", call. = FALSE)
  dt <- lubridate::ymd_hm(a$TIMESTAMP_START[2]) - lubridate::ymd_hm(a$TIMESTAMP_START[1])
  data.frame(TIMESTAMP = lubridate::ymd_hm(a$TIMESTAMP_START) + dt / 2, TA_QC = a$TA_F_MDS_QC)
}

#' Every air-temperature column of a site
#'
#' The air-temperature columns (TA_COLUMNS) over step 01's record, in the layout of
#' ts_candidates() (values and usability, one row per record of step01$ts_base), so that
#' ts_column_inputs() and ts_step01_tables() take them as they take the soil-temperature
#' columns.
#'
#' @param step01 The site's step-01 list (ts_base is enough).
#' @param ta_qc read_ta_qc()'s table, or NULL.
#' @return list(values, ok): data frames, one column per TA_COLUMNS.
#' @noRd
ta_candidates <- function(step01, ta_qc) {
  base <- step01$ts_base
  measured <- base$TA
  if (!is.null(ta_qc)) {
    qc <- ta_qc$TA_QC[match(base$TIMESTAMP, ta_qc$TIMESTAMP)]
    measured[!qc %in% 0] <- NA
  }
  memory <- ts_memory_features(data.frame(YEAR = base$YEAR, MONTH = base$MONTH, DAY = base$DAY, HOUR = base$HOUR,
                                          MINUTE = base$MINUTE, TA = measured))
  values <- data.frame(TA_measured = measured, TA_mem3d = memory$TA_m3, TA_mem7d = memory$TA_m7, TA_mem30d = memory$TA_m30)
  stopifnot(identical(names(values), TA_COLUMNS))
  list(values = values, ok = as.data.frame(lapply(values, function(v) !is.na(v))))
}

#' Summary of a site's air-temperature columns
#'
#' One row per air-temperature column of a site, as ts_columns_summary() has for the
#' soil-temperature columns: the fit settings, the number of nighttime fit records and years,
#' and the column's coverage of the record.
#'
#' @param site_id Site ID.
#' @param candidates ta_candidates()'s list.
#' @param inputs ts_column_inputs() results, named by column.
#' @return A data frame.
#' @noRd
ta_columns_summary <- function(site_id, candidates, inputs) {
  dplyr::bind_rows(lapply(names(candidates$values), function(column) {
    x <- inputs[[column]]
    row <- data.frame(site_ID = site_id, column = column, coverage = mean(candidates$ok[[column]]))
    if (is.null(x)) return(row)
    cbind(row, x$settings[, setdiff(names(x$settings), names(row)), drop = FALSE],
          n_night = nrow(x$night), n_years = length(unique(x$night$growing_year)))
  }))
}
