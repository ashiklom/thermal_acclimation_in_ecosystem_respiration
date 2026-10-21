# File locations used by the workflows. All paths are relative to the
# repository root, which is the working directory for every script.
#
# - `path_original()`: the published, read-only input data, a sibling
#   directory `../original-data` by default (override with the environment
#   variable THERMAL_ORIGINAL_DATA).
# - `path_raw()`: data this project downloads itself (gitignored `data-raw/`,
#   or THERMAL_DATA_RAW).
# - `path_proc()`: everything the workflows produce (gitignored `data-proc/`,
#   or THERMAL_DATA_PROC, e.g. to run a step into a scratch directory). The
#   parent directory is created on demand so writers need not do it.

path_original <- function(...) {
  file.path(Sys.getenv("THERMAL_ORIGINAL_DATA", file.path("..", "original-data")), ...)
}

path_raw <- function(...) {
  path <- file.path(Sys.getenv("THERMAL_DATA_RAW", "data-raw"), ...)
  for (dir in unique(dirname(path))) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path
}

path_proc <- function(...) {
  path <- file.path(Sys.getenv("THERMAL_DATA_PROC", "data-proc"), ...)
  for (dir in unique(dirname(path))) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path
}
