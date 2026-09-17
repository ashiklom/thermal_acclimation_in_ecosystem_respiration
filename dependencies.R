#!/usr/bin/env Rscript

missing_pkg <- function(pkg) {
  !(pkg %in% rownames(installed.packages()))
}

if (missing_pkg("amerifluxr")) {
  remotes::install_github("chuhousen/amerifluxr", upgrade = "never")
}

cran_pkgs <- c("REddyProc", "gslnls", "lutz")

for (pkg in cran_pkgs) {
  if (missing_pkg(pkg)) {
    install.packages(pkg)
  }
}
