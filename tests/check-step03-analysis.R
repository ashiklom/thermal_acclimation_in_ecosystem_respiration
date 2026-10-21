# Run driver_analysis() (R/step03-drivers.R) on an acclimation_data.csv table
# and compare it with the varImp/partial tables the 03_02 script produced from
# the same table in the same environment (they must be identical: same
# random-number sequence).
#
# Usage: Rscript tests/check-step03-analysis.R DIR
#   DIR holds acclimation_data.csv, varImp_plot.csv and partial_plot.csv
#   from a run of workflows/03_02_identify_thermal_response_driver.R.

source(file.path("R", "step03-drivers.R"))
dir <- commandArgs(trailingOnly = TRUE)[1]
result <- driver_analysis(utils::read.csv(file.path(dir, "acclimation_data.csv")))
compare_csv <- function(new, file) {
  f <- tempfile(fileext = ".csv")
  utils::write.csv(new, f, row.names = FALSE)
  a <- utils::read.csv(f)
  b <- utils::read.csv(file.path(dir, file))
  ok <- identical(a, b)
  message(file, if (ok) ": identical" else ": DIFFERENT")
  if (!ok) print(all.equal(a, b))
  ok
}
ok <- c(compare_csv(result$varImp, "varImp_plot.csv"), compare_csv(result$partial, "partial_plot.csv"))
if (!all(ok)) stop("driver_analysis() differs from the 03_02 script")
