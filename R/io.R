# The workflows' tables on disk are parquet files: exact (doubles keep full
# precision, unlike CSV text) and typed. Tables are read back as plain data
# frames, not tibbles, so base R subsetting behaves as it did with read.csv().

write_table <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(x, path)
  invisible(path)
}

read_table <- function(path) {
  as.data.frame(arrow::read_parquet(path))
}

# A site's step-01 half-hourly ("ac") or nighttime ("nightNEE") table, or
# its growing-season climatology days ("climatology").
read_step01_table <- function(site_id, kind = c("ac", "nightNEE", "climatology"), dir = path_proc("RespirationData")) {
  kind <- match.arg(kind)
  read_table(file.path(dir, paste0(site_id, "_", kind, ".parquet")))
}

# The growing-season features of all sites (EuroFlux, then AmeriFlux, as the
# step-02 scripts always combined them).
read_growing_season_features <- function(dir = Sys.getenv("THERMAL_DATA_PROC", "data-proc")) {
  rbind(read_table(file.path(dir, "growing_season_feature_EuropFlux.parquet")),
        read_table(file.path(dir, "growing_season_feature_AmeriFlux.parquet")))
}
