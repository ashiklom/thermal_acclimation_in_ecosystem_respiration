# Install the R packages that conda-forge does not provide into the pixi
# environment's library. Run with `pixi run r-dependencies`.

repos <- c(CRAN = "https://cloud.r-project.org")
lib <- .libPaths()[1]

# base64url (needed by targets) is on conda-forge, but the osx-arm64 build
# ships without Meta/package.rds and fails to load; a CRAN build replaces it.
cran <- c("gslnls", "REddyProc", "lutz", "base64url")
github <- c(amerifluxr = "chuhousen/amerifluxr", plotbiomes = "valentinitnelav/plotbiomes")

missing_cran <- cran[!vapply(cran, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_cran) > 0) {
  utils::install.packages(missing_cran, lib = lib, repos = repos)
}

for (pkg in names(github)) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    remotes::install_github(github[[pkg]], lib = lib, upgrade = "never", dependencies = NA)
  }
}

stopifnot(
  all(vapply(c(cran, names(github)), requireNamespace, logical(1), quietly = TRUE))
)
