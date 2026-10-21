# Downloads of the latest flux-tower releases (refactor-v2 stage S18), into
# path_raw("flux") (data-raw/flux/), one directory per product, laid out as
# R/read-raw.R reads them:
#
#   AmeriFlux_BASE/AMF_<site>_BASE-BADM_<version>.zip   amerifluxr (AmeriFlux
#                       account in _creds.toml: user_id, user_email)
#   SHUTTLE/unzip/<FLUXNET product>_FLUXMET_(HH|HR)_*.csv   the FLUXNET-archive
#                       shuttle (fluxnet-shuttle): AmeriFlux, ICOS and TERN
#                       ONEFlux products; the zips kept in SHUTTLE/
#
# Each download replaces the site's earlier file of that product (a product
# directory holds one file per site). Downloads need internet access (on
# YCRC Bouchet: a compute node).

# The AmeriFlux account in _creds.toml; stops if the file or a key is missing.
flux_credentials <- function(path = "_creds.toml") {
  if (!file.exists(path)) {
    stop("Downloading AmeriFlux data needs ", normalizePath(path, mustWork = FALSE),
         " with user_id and user_email (the AmeriFlux account); it is missing.", call. = FALSE)
  }
  creds <- RcppTOML::parseTOML(path)
  missing <- setdiff(c("user_id", "user_email"), names(creds))
  if (length(missing)) stop(path, " lacks ", paste(missing, collapse = ", "), call. = FALSE)
  creds
}

# Download a site's latest AmeriFlux BASE-BADM release; returns its path.
download_ameriflux_base <- function(site_id, creds = flux_credentials(), dir = path_raw("flux", "AmeriFlux_BASE")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  staging <- file.path(dir, paste0(".download-", site_id))
  unlink(staging, recursive = TRUE)
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE))
  path <- amerifluxr::amf_download_base(
    user_id = creds$user_id, user_email = creds$user_email, site_id = site_id, data_product = "BASE-BADM",
    data_policy = "CCBY4.0", agree_policy = TRUE, intended_use = "synthesis",
    intended_use_text = "Thermal acclimation of ecosystem respiration across flux sites (Yale)",
    out_dir = staging, verbose = FALSE
  )
  stopifnot(length(path) == 1, file.exists(path))
  old <- list.files(dir, pattern = paste0("^AMF_", gsub("-", "\\\\-", site_id), "_BASE-BADM_.*\\.zip$"), full.names = TRUE)
  file.remove(old)
  dest <- file.path(dir, basename(path))
  stopifnot(file.rename(path, dest))
  dest
}

# The FLUXNET-archive shuttle's catalogue (one row per site and data hub),
# written to `dir`; returns the snapshot file.
shuttle_snapshot <- function(dir = path_raw("flux", "SHUTTLE")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  before <- list.files(dir, pattern = "^fluxnet_shuttle_snapshot_.*\\.csv$")
  status <- system2("fluxnet-shuttle", c("--no-logfile", "listall", "-o", shQuote(dir)))
  if (status != 0) stop("fluxnet-shuttle listall failed (exit ", status, "); is there internet access?", call. = FALSE)
  new <- setdiff(list.files(dir, pattern = "^fluxnet_shuttle_snapshot_.*\\.csv$"), before)
  stopifnot(length(new) == 1)
  file.path(dir, new)
}

# Download a site's FLUXNET product from the shuttle and extract its
# half-hourly (or hourly) table; returns the table's path, or NULL if the
# shuttle has no product for the site.
download_shuttle <- function(site_id, snapshot, dir = path_raw("flux", "SHUTTLE")) {
  catalogue <- utils::read.csv(snapshot)
  if (!site_id %in% catalogue$site_id) return(NULL)
  product <- catalogue$fluxnet_product_name[catalogue$site_id == site_id]
  stopifnot(length(product) == 1)
  status <- system2("fluxnet-shuttle", c("--no-logfile", "download", "--snapshot-file", shQuote(snapshot),
                                        "--output-dir", shQuote(dir), "--quiet", "--sites", site_id))
  zip <- file.path(dir, product)
  if (status != 0 || !file.exists(zip)) stop("fluxnet-shuttle could not download ", product, " (exit ", status, ")", call. = FALSE)
  members <- utils::unzip(zip, list = TRUE)$Name
  table <- grep("_FLUXMET_(HH|HR)_.*\\.csv$", members, value = TRUE)
  if (length(table) != 1) stop(product, ": expected one FLUXMET HH/HR table, found ", length(table), call. = FALSE)
  unzip_dir <- file.path(dir, "unzip")
  dir.create(unzip_dir, showWarnings = FALSE)
  old <- list.files(unzip_dir, pattern = paste0("_", gsub("-", "\\\\-", site_id), "_.*\\.csv$"), full.names = TRUE)
  file.remove(old)
  utils::unzip(zip, files = table, exdir = unzip_dir, junkpaths = TRUE)
  file.path(unzip_dir, basename(table))
}
