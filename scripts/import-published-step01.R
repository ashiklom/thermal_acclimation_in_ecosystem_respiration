# Copy the published outputs of workflow step 01 into data-proc/, so that
# step 02 can run without re-running step 01 from the raw site data.
#
#   ../original-data/RespirationData/*.csv -> data-proc/RespirationData/
#   ../original-data/TS_RandomForest/*.csv -> data-proc/TS_RandomForest/
#   data/growing_season_feature_*.csv      -> data-proc/
#
# Files are copied, never linked, so a later step-01 run that writes into
# data-proc/ cannot modify the read-only originals.

source(file.path("R", "paths.R"))

copy_dir <- function(from, to) {
  files <- list.files(from, pattern = "\\.csv$", full.names = TRUE)
  stopifnot(length(files) > 0)
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(files, to, overwrite = TRUE, copy.date = TRUE)
  stopifnot(all(ok))
  message(length(files), " files: ", from, " -> ", to)
}

copy_dir(path_original("RespirationData"), path_proc("RespirationData"))
copy_dir(path_original("TS_RandomForest"), path_proc("TS_RandomForest"))
for (f in c("growing_season_feature_EuropFlux.csv", "growing_season_feature_AmeriFlux.csv")) {
  stopifnot(file.copy(file.path("data", f), path_proc(f), overwrite = TRUE, copy.date = TRUE))
}
