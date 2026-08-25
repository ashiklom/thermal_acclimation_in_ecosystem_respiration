load_growing_season_features <- function(data_dir = "data-proc/features") {
  feature_files <- file.path(
    data_dir,
    c(
      "growing_season_feature_AmeriFlux.csv",
      "growing_season_feature_ICOS.csv",
      "growing_season_feature_TERN.csv"
    )
  )
  missing_files <- feature_files[!file.exists(feature_files)]
  if (length(missing_files) > 0) {
    stop("Missing growing-season feature files: ", paste(missing_files, collapse = ", "))
  }
  features <- dplyr::bind_rows(lapply(feature_files, read.csv, stringsAsFactors = FALSE))
  features[!duplicated(features$site_ID), , drop = FALSE]
}
