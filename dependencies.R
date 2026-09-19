#!/usr/bin/env Rscript

missing_pkg <- function(pkg) {
  !(pkg %in% rownames(installed.packages()))
}

if (missing_pkg("amerifluxr")) {
  remotes::install_github("chuhousen/amerifluxr", upgrade = "never")
}

# `base64url` is declared in pixi.toml (targets needs it to encode store paths),
# but the conda-forge osx-arm64 artifact
# `r-base64url-1.4-r45hbe92478_1008.conda` is a broken build: it ships the
# package *source* tree (src/, vignettes/, conda_build.sh) instead of an
# installed package, with no Meta/package.rds and no compiled .so. R therefore
# cannot load it -- `library(targets)` dies with "shared object 'base64url.so'
# not found" -- and it does not even appear in `installed.packages()`, so the
# `missing_pkg()` test below correctly treats it as absent.
#
# It is the only osx-arm64 build conda-forge offers for 1.4, so there is no
# version constraint that fixes it. Installing the real package from CRAN over
# the top is the fix. Re-running this script after a `rm -rf .pixi/envs` is
# what makes a fresh checkout work.
cran_pkgs <- c("REddyProc", "gslnls", "lutz", "base64url")

for (pkg in cran_pkgs) {
  if (missing_pkg(pkg)) {
    install.packages(pkg)
  }
}
