# Put the published outputs of workflow step 01 into data-proc/, so that
# step 02 can run without re-running step 01 from the raw site data:
#
#   ../original-data/RespirationData/*.csv -> data-proc/RespirationData/*.parquet (and *.csv)
#   data/growing_season_feature_*.csv      -> data-proc/growing_season_feature_*.parquet (and *.csv)
#
# The parquet tables hold exactly the values read.csv() reads from the
# published files. The CSV copies serve versions of the code before step-01
# outputs became parquet (v2-s08). Files are written, never linked, so a later
# step-01 run that writes into data-proc/ cannot touch the read-only originals.

source(file.path("R", "paths.R"))
source(file.path("R", "io.R"))

files <- list.files(path_original("RespirationData"), pattern = "\\.csv$", full.names = TRUE)
stopifnot(length(files) > 0)
for (f in files) {
  write_table(utils::read.csv(f), path_proc("RespirationData", sub("\\.csv$", ".parquet", basename(f))))
  stopifnot(file.copy(f, path_proc("RespirationData", basename(f)), overwrite = TRUE, copy.date = TRUE))
}
message(length(files), " step-01 tables imported into ", path_proc("RespirationData"))
for (f in c("growing_season_feature_EuropFlux.csv", "growing_season_feature_AmeriFlux.csv")) {
  write_table(utils::read.csv(file.path("data", f)), path_proc(sub("\\.csv$", ".parquet", f)))
  stopifnot(file.copy(file.path("data", f), path_proc(f), overwrite = TRUE, copy.date = TRUE))
}
