# Loaded once before the test files.
suppressMessages({
  library(dplyr)
  library(targets)
  library(withr)
})

# `tar_source()` and everything it loads address inputs relative to the project
# root, so load it from there.
withr::with_dir(normalizePath(file.path("..", "..")), tar_source())
