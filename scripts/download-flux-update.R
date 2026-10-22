# Download the latest flux-tower releases for every site (refactor-v2 stage
# S18) into data-raw/flux/ (R/download-flux.R): AmeriFlux BASE-BADM for the
# AmeriFlux sites, the FLUXNET-archive shuttle's product for the others.
# Needs internet access (on YCRC Bouchet, run it on a compute node) and,
# for AmeriFlux, _creds.toml (checked first).
#
# Usage (from the repository root):
#   Rscript scripts/download-flux-update.R [--sites=SITE1,SITE2] [--product=SHUTTLE]
# --product=SHUTTLE downloads the shuttle's product for every given site,
# AmeriFlux sites included (for comparisons; the update reads AmeriFlux
# sites' BASE releases). Writes data-raw/flux/download_manifest.csv: site,
# product, file, bytes, md5, downloaded (UTC time), or the error (rows of
# other sites and products are kept).

source(file.path("R", "paths.R"))
source(file.path("R", "download-flux.R"))

args <- commandArgs(trailingOnly = TRUE)
hit <- grep("^--sites=", args, value = TRUE)
site_info <- utils::read.csv(file.path("data", "site_info.csv"))
sites <- if (length(hit)) strsplit(sub("^--sites=", "", hit), ",")[[1]] else site_info$site_ID
stopifnot(all(sites %in% site_info$site_ID))
forced <- sub("^--product=", "", grep("^--product=", args, value = TRUE))
stopifnot(length(forced) <= 1, all(forced %in% "SHUTTLE"))
product_of <- function(s) {
  if (length(forced)) forced else if (site_info$source[site_info$site_ID == s] == "AmeriFlux_BASE") "AmeriFlux_BASE" else "SHUTTLE"
}

creds <- if (any(vapply(sites, product_of, "") == "AmeriFlux_BASE")) flux_credentials() else NULL
snapshot <- shuttle_snapshot()
message("shuttle snapshot: ", snapshot)

rows <- lapply(sites, function(s) {
  product <- product_of(s)
  file <- tryCatch(
    if (product == "AmeriFlux_BASE") download_ameriflux_base(s, creds) else download_shuttle(s, snapshot),
    error = function(e) structure(conditionMessage(e), class = "download_error")
  )
  if (inherits(file, "download_error")) {
    message(s, ": FAILED: ", file)
    return(data.frame(site_ID = s, product = product, file = NA, bytes = NA, md5 = NA, downloaded = NA, error = as.character(file)))
  }
  if (is.null(file)) {
    message(s, ": not in the shuttle catalogue")
    return(data.frame(site_ID = s, product = product, file = NA, bytes = NA, md5 = NA, downloaded = NA, error = "not available"))
  }
  message(s, ": ", basename(file))
  data.frame(site_ID = s, product = product, file = basename(file), bytes = file.size(file),
             md5 = unname(tools::md5sum(file)), downloaded = format(Sys.time(), tz = "UTC", usetz = TRUE), error = NA)
})
manifest <- do.call(rbind, rows)
# keep the other sites' and products' rows of an earlier run
path <- path_raw("flux", "download_manifest.csv")
if (file.exists(path)) {
  earlier <- utils::read.csv(path)
  key <- function(m) paste(m$site_ID, m$product)
  manifest <- rbind(earlier[!key(earlier) %in% key(manifest), ], manifest)
  manifest <- manifest[order(match(manifest$site_ID, site_info$site_ID)), ]
}
utils::write.csv(manifest, path, row.names = FALSE)
message(sum(!is.na(manifest$file)), " of ", nrow(manifest), " sites downloaded")
