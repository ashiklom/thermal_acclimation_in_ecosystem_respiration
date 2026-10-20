# Load every R package the workflow uses; fail loudly if one is missing.

pkgs <- c(
  "amerifluxr", "bayesplot", "boot", "brms", "car", "caret", "cmdstanr",
  "corrplot", "dplyr", "fBasics", "ggplot2", "ggpubr", "gslnls", "lme4",
  "lubridate", "lutz", "mblm", "MuMIn", "nlme", "performance", "plotbiomes",
  "randomForest", "ranger", "REddyProc", "rsample", "suncalc", "terra",
  "tidyr", "tidyverse", "vip", "VSURF", "zoo",
  "arrow", "digest", "optparse", "targets", "tarchetypes", "crew",
  "crew.cluster", "qs2", "testthat", "RcppTOML"
)
ok <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
if (!all(ok)) stop("Missing R packages: ", paste(pkgs[!ok], collapse = ", "))
message("cmdstan: ", cmdstanr::cmdstan_path(), " (", cmdstanr::cmdstan_version(), ")")
message("All ", length(pkgs), " packages load.")
