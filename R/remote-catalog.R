# What the providers currently publish, and what we last fetched from them.
#
# The periodic run is built on one comparison per site and product: the
# provider's current `remote_id` (from `scan_remote_catalog()`) against the one
# recorded when the data on disk was downloaded (`local_remote_id()`). Equal,
# and nothing downstream of the site moves; different, and `download_site()`
# supersedes the local copy and fetches the new one.
#
# A `remote_id` is whatever identifies a release at that provider -- the
# archive filename for ICOS, WW2020 and FLUXNET, the version for TERN, the
# published year span for AmeriFlux BASE. See `scripts/check-data-updates.py`,
# which does the asking, for what each one can and cannot detect.

# Written into a site's product directory by `download_site()`.
REMOTE_ID_FILE <- ".remote_id"

# The catalogue lives in a file, written by `scripts/check-data-updates.py
# --catalog` -- normally from `scripts/scan-and-run.sh`, before targets is
# involved at all. That is what makes a no-change day cheap: the script leaves
# the file untouched when nothing changed, so `tar_outdated()` finds nothing to
# do, where an always-run scan target inside the pipeline would make every
# site look outdated on every run.
#
# One row per site and product: `site_ID`, `product`, `remote_id`. A provider
# that cannot be asked keeps its previous remote_id (or blank, the first
# time), which `download_site()` reads as "keep what we have": a bad day at a
# provider never invalidates anything.
REMOTE_CATALOG_CSV <- file.path(DIR_RAWDATA, "remote_catalog.csv")

# The catalogue path, running the scan first if there is none yet -- a fresh
# clone, or a `tar_make()` run without `scan-and-run.sh`. `site_info_path` is
# an argument so the target depends on it: a site added to site_info.csv has to
# be scanned before it can be downloaded.
ensure_remote_catalog <- function(site_info_path = SITE_INFO_CSV, path = REMOTE_CATALOG_CSV) {
  sites <- get_site_info(path = site_info_path)[["site_ID"]]
  if (file.exists(path) && all(sites %in% read_remote_catalog(path)$site_ID)) {
    return(path)
  }
  status <- system2("uv", c("run", "scripts/check-data-updates.py", "--catalog", path))
  if (!identical(status, 0L) || !file.exists(path)) {
    stop("Remote catalogue scan failed (exit ", status, "); see the output above.")
  }
  path
}

read_remote_catalog <- function(path = REMOTE_CATALOG_CSV) {
  readr::read_csv(path, col_types = "ccc", na = "", progress = FALSE)
}

# A site's slice of the catalogue as a named vector, product -> remote_id.
# This is what the per-site `site_remote` target holds: targets compares it by
# value, so a site whose providers published nothing new stays up to date.
remote_for_site <- function(catalog, name_site) {
  rows <- catalog[catalog$site_ID == name_site, ]
  stats::setNames(rows$remote_id, rows$product)
}

product_site_dir <- function(name_site, product) {
  file.path(DIR_RAWDATA, FLUX_PRODUCTS[[product]]$dir, name_site)
}

# Everything on disk that belongs to one site's copy of a product: its
# directory, and for FLUXNET the archive the shuttle leaves one level up.
product_local_paths <- function(name_site, product) {
  paths <- product_site_dir(name_site, product)
  if (identical(product, "FLUXNET")) {
    paths <- c(paths, list.files(
      file.path(DIR_RAWDATA, FLUX_PRODUCTS[[product]]$dir),
      pattern = sprintf("_%s_FLUXNET_.*[.]zip$", name_site), full.names = TRUE
    ))
  }
  paths[file.exists(paths)]
}

# The remote_id of the copy on disk: the one recorded at download, else the
# archive's filename, which is exact wherever the remote id is a filename.
local_remote_id <- function(name_site, product) {
  sidecar <- file.path(product_site_dir(name_site, product), REMOTE_ID_FILE)
  if (file.exists(sidecar)) {
    id <- trimws(readLines(sidecar, warn = FALSE))
    if (length(id) && nzchar(id[[1]])) return(id[[1]])
  }
  paths <- product_local_paths(name_site, product)
  is_dir <- dir.exists(paths)
  zips <- c(
    list.files(paths[is_dir], pattern = "[.]zip$"),
    basename(paths[!is_dir & grepl("[.]zip$", paths)])
  )
  if (length(zips)) sort(zips, decreasing = TRUE)[[1]] else NA_character_
}

record_remote_id <- function(name_site, product, remote_id) {
  if (is.na(remote_id)) return(invisible(NULL))
  dir <- product_site_dir(name_site, product)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(remote_id, file.path(dir, REMOTE_ID_FILE))
}

# Move a site's copy of a product to data-raw/_superseded/, stamped, so the
# downloader sees it absent and `product_file()` never sees two releases at
# once. Kept rather than deleted: with data-raw updated in place, these are
# the only way back to the inputs an earlier run -- the manuscript's, for one
# -- was made from. Returns what `restore_product()` needs to undo it.
supersede_product <- function(name_site, product) {
  from <- product_local_paths(name_site, product)
  dest <- file.path(DIR_RAWDATA, "_superseded", FLUX_PRODUCTS[[product]]$dir, name_site,
                    format(Sys.time(), "%Y%m%dT%H%M%S"))
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  to <- file.path(dest, basename(from))
  ok <- file.rename(from, to)
  if (!all(ok)) stop("Could not move ", paste(from[!ok], collapse = ", "), " to ", dest, ".")
  list(from = from, to = to)
}

restore_product <- function(moved, name_site, product) {
  if (!length(moved$from)) return(invisible(NULL))
  unlink(product_local_paths(name_site, product), recursive = TRUE)
  file.rename(moved$to, moved$from)
  unlink(dirname(moved$to[[1]]), recursive = TRUE)
}
