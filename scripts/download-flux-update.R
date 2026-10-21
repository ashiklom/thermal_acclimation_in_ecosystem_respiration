# Download the latest flux-tower releases for every site (refactor-v2 stage
# S18) into data-raw/flux/ (R/download-flux.R): AmeriFlux BASE-BADM for the
# AmeriFlux sites, the FLUXNET-archive shuttle's product for the others.
# Needs internet access (on YCRC Bouchet, run it on a compute node) and,
# for AmeriFlux, _creds.toml (checked first).
#
# Usage (from the repository root):
#   Rscript scripts/download-flux-update.R [--sites=SITE1,SITE2]
# Writes data-raw/flux/download_manifest.csv: site, product, file, bytes,
# md5, downloaded (UTC time), or the error (rows of sites not downloaded
# this time are kept).

source(file.path("R", "paths.R"))
source(file.path("R", "download-flux.R"))

args <- commandArgs(trailingOnly = TRUE)
hit <- grep("^--sites=", args, value = TRUE)
site_info <- utils::read.csv(file.path("data", "site_info.csv"))
sites <- if (length(hit)) strsplit(sub("^--sites=", "", hit), ",")[[1]] else site_info$site_ID
stopifnot(all(sites %in% site_info$site_ID))

creds <- flux_credentials()
snapshot <- shuttle_snapshot()
message("shuttle snapshot: ", snapshot)

rows <- lapply(sites, function(s) {
  ameriflux <- site_info$source[site_info$site_ID == s] == "AmeriFlux_BASE"
  product <- if (ameriflux) "AmeriFlux_BASE" else "SHUTTLE"
  file <- tryCatch(
    if (ameriflux) download_ameriflux_base(s, creds) else download_shuttle(s, snapshot),
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
# keep the other sites' rows of an earlier run
path <- path_raw("flux", "download_manifest.csv")
if (file.exists(path)) {
  earlier <- utils::read.csv(path)
  manifest <- rbind(earlier[!earlier$site_ID %in% manifest$site_ID, ], manifest)
  manifest <- manifest[order(match(manifest$site_ID, site_info$site_ID)), ]
}
utils::write.csv(manifest, path, row.names = FALSE)
message(sum(!is.na(manifest$file)), " of ", nrow(manifest), " sites downloaded")
