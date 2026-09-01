load_growing_season_features <- function(data_dir = "data-proc/features") {
  feature_file <- file.path(data_dir, "growing_season_features.csv")
  if (!file.exists(feature_file)) {
    stop("Missing growing-season feature file: ", feature_file)
  }
  features <- read.csv(feature_file, stringsAsFactors = FALSE)
  features[!duplicated(features$site_ID), , drop = FALSE]
}
