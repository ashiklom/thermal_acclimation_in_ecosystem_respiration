# Download the WorldClim tmin rasters workflow 04_01 needs into
# data-raw/Climate/ (see download_worldclim() in R/download.R).
#
# Usage: Rscript scripts/download-worldclim.R

source(file.path("R", "paths.R"))
source(file.path("R", "download.R"))
invisible(download_worldclim())
