#' Download the WorldClim minimum-temperature rasters
#'
#' Download the WorldClim 2.1 minimum-temperature rasters that 04_01 needs into `dir`, keeping
#' existing complete files:
#'
#'   wc2.1_2.5m_tmin/              monthly series 2000-2020 (CRU-TS 4.06
#'                                 downscaled), one GeoTIFF per year-month
#'   wc2.1_2.5m_tmin_2041-2060/    SSP2-4.5 2041-2060, one 12-layer GeoTIFF
#'                                 per GCM
#'
#' Downloads go to a `.part` file renamed only on success, so an interrupted run never leaves a
#' truncated file that looks complete. Stops if baseline files are missing, or unexpected ones
#' present, after the download.
#'
#' @param dir Destination directory (data-raw/Climate by default).
#' @return list(baseline = <252 paths>, future = <13 paths>): the paths of the baseline and the
#'   future GeoTIFFs.
#' @noRd
download_worldclim <- function(dir = path_raw("Climate")) {
  baseline_url <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/hist/cts4.06/2.5m"
  future_url <- "https://geodata.ucdavis.edu/cmip6/2.5m"
  baseline_years <- 2000:2020
  # The baseline series is published as decade archives.
  baseline_zips <- c("2000-2009", "2010-2019", "2020-2021")
  # The 13 GCMs with 2.5-minute tmin under SSP2-4.5 (04_01: "13 global
  # circulation models").
  gcms <- c(
    "ACCESS-CM2", "BCC-CSM2-MR", "CMCC-ESM2", "EC-Earth3-Veg", "FIO-ESM-2-0",
    "GISS-E2-1-G", "HadGEM3-GC31-LL", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6",
    "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
  )

  baseline_dir <- file.path(dir, "wc2.1_2.5m_tmin")
  future_dir <- file.path(dir, "wc2.1_2.5m_tmin_2041-2060")
  zip_dir <- file.path(dir, "zips")
  for (d in c(baseline_dir, future_dir, zip_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

  fetch <- function(url, dest) {
    if (file.exists(dest)) return(invisible(dest))
    part <- paste0(dest, ".part")
    message("fetching ", url)
    old <- options(timeout = 7200)
    on.exit(options(old))
    status <- utils::download.file(url, part, mode = "wb", quiet = TRUE)
    if (status != 0 || !file.exists(part) || file.size(part) == 0) {
      unlink(part)
      stop("download failed: ", url)
    }
    file.rename(part, dest)
    invisible(dest)
  }

  # Baseline: keep exactly the 2000-2020 year-months.
  wanted <- sprintf("wc2.1_2.5m_tmin_%d-%02d.tif", rep(baseline_years, each = 12), rep(1:12, times = length(baseline_years)))
  if (!all(file.exists(file.path(baseline_dir, wanted)))) {
    for (decade in baseline_zips) {
      zip <- file.path(zip_dir, sprintf("wc2.1_cruts4.06_2.5m_tmin_%s.zip", decade))
      fetch(file.path(baseline_url, basename(zip)), zip)
      members <- utils::unzip(zip, list = TRUE)$Name
      keep <- members[basename(members) %in% wanted]
      utils::unzip(zip, files = keep, exdir = baseline_dir, junkpaths = TRUE, overwrite = FALSE)
    }
  }
  missing <- wanted[!file.exists(file.path(baseline_dir, wanted))]
  if (length(missing) > 0) stop("baseline files missing after download: ", paste(utils::head(missing), collapse = ", "))
  extra <- setdiff(list.files(baseline_dir, pattern = "\\.tif$"), wanted)
  if (length(extra) > 0) stop("unexpected files in ", baseline_dir, ": ", paste(utils::head(extra), collapse = ", "))

  # Future: one file per GCM.
  for (gcm in gcms) {
    name <- sprintf("wc2.1_2.5m_tmin_%s_ssp245_2041-2060.tif", gcm)
    fetch(file.path(future_url, gcm, "ssp245", name), file.path(future_dir, name))
  }

  message("WorldClim: ", length(wanted), " baseline and ", length(gcms), " future rasters in ", dir)
    list(baseline = file.path(baseline_dir, wanted),
         future = file.path(future_dir, sprintf("wc2.1_2.5m_tmin_%s_ssp245_2041-2060.tif", gcms)))
}
