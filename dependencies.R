#!/usr/bin/env Rscript

missing_pkg <- function(pkg) {
  !(pkg %in% rownames(installed.packages()))
}

if (missing_pkg("amerifluxr")) {
  remotes::install_github("chuhousen/amerifluxr", upgrade = "never")
}

if (missing_pkg("REddyProc")) {
  install.packages("REddyProc")
}

if (missing_pkg("gslnls")) {
  install.packages("gslnls")
}

if (missing_pkg("lutz")) {
  install.packages("lutz")
}
