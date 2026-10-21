# The workflows' tables on disk are parquet files: exact (doubles keep full
# precision, unlike CSV text) and typed. Tables are read back as plain data
# frames, not tibbles, so base R subsetting behaves as it did with read.csv().

#' Write a table as a parquet file
#'
#' Parquet keeps doubles at full precision (unlike CSV text) and keeps column types. The
#' directory is created if needed.
#'
#' @param x Data frame to write.
#' @param path Destination file.
#' @return `path`, invisibly.
#' @noRd
write_table <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(x, path)
  invisible(path)
}

#' Read a parquet table as a plain data frame
#'
#' Tables are read back as plain data frames, not tibbles, so base R subsetting behaves as it
#' did with read.csv().
#'
#' @param path Parquet file.
#' @return A data frame.
#' @noRd
read_table <- function(path) {
  as.data.frame(arrow::read_parquet(path))
}

#' Read one of a site's step-01 tables
#'
#' A site's step-01 half-hourly ("ac") or nighttime ("nightNEE") table, or its growing-season
#' climatology days ("climatology"), from `<dir>/<site>_<kind>.parquet`.
#'
#' @param site_id Site ID.
#' @param kind Which table: "ac", "nightNEE" or "climatology".
#' @param dir Directory of the step-01 parquet files.
#' @return A data frame.
#' @noRd
read_step01_table <- function(site_id, kind = c("ac", "nightNEE", "climatology"), dir = path_proc("RespirationData")) {
  kind <- match.arg(kind)
  read_table(file.path(dir, paste0(site_id, "_", kind, ".parquet")))
}

#' Read the growing-season features of all sites
#'
#' The growing-season features of all sites (EuroFlux, then AmeriFlux, as the step-02 scripts
#' always combined them), from growing_season_feature_EuropFlux.parquet and
#' growing_season_feature_AmeriFlux.parquet.
#'
#' @param dir Directory holding the two parquet files.
#' @return A data frame, one row per site (site_ID, gStart, gEnd, tStart, tEnd, nyear).
#' @noRd
read_growing_season_features <- function(dir = Sys.getenv("THERMAL_DATA_PROC", "data-proc")) {
  rbind(read_table(file.path(dir, "growing_season_feature_EuropFlux.parquet")),
        read_table(file.path(dir, "growing_season_feature_AmeriFlux.parquet")))
}
