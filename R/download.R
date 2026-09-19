# Fetch every product in a site's provenance list, returning the local paths.
#
# Most sites need more than one product: see `FLUX_PRODUCTS` in R/constants.R
# and docs/data-provenance.md for why.
download_site <- function(site_info, overwrite = FALSE) {
  name_site <- site_info[["site_ID"]]
  downloaders <- list(
    AmeriFlux_BASE = download_ameriflux,
    FLUXNET = download_fluxnet,
    FLUXNET2015 = download_fluxnet2015,
    ICOS = download_icos,
    WW2020 = download_ww2020,
    TERN = download_tern
  )

  paths <- character()
  for (product in site_sources(site_info)) {
    fn <- downloaders[[product]]
    if (is.null(fn)) {
      stop("No download method for product `", product, "` (site ", name_site, ").")
    }
    message("Fetching ", product, " for ", name_site)
    fn(name_site, overwrite = overwrite)
    got <- product_file(name_site, product)
    if (is.na(got)) {
      stop(
        "Downloaded ", product, " for ", name_site,
        " but no file matching ", shQuote(FLUX_PRODUCTS[[product]]$pattern),
        " appeared under ", file.path(DIR_RAWDATA, FLUX_PRODUCTS[[product]]$dir, name_site), "."
      )
    }
    paths <- c(paths, got)
  }
  paths
}

download_ameriflux <- function(name_site, overwrite = FALSE) {
  creds <- parse_toml("_creds.toml")
  outdir <- file.path("data-raw", "Ameriflux", name_site)
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  result_file <- list.files(outdir, sprintf(".*_%s_BASE-BADM_.*.zip", name_site), full.names = TRUE)
  if (length(result_file) > 1) {
    warning("Found multiple matching files in ", outdir, ". Check this for correctness.")
  }
  if (length(result_file) == 0 || overwrite) {
    result_file <- amerifluxr::amf_download_base(
      user_id = creds$user_id,
      user_email = creds$user_email,
      site_id = name_site,
      data_product = "BASE-BADM",
      data_policy = "CCBY4.0",
      agree_policy = TRUE,
      intended_use = "synthesis",
      intended_use_text = "Thermal acclimation in ecosystem respiration synthesis project",
      out_dir = outdir,
      verbose = TRUE
    )
  } else {
    message("Skipping download because file already exists.")
  }
  result_file
}

# Run an external downloader unless the product is already on disk. Presence is
# decided by `product_file()` -- the same lookup the readers use -- so a
# downloader that succeeds but leaves nothing readable is caught immediately
# rather than at model-fitting time.
run_if_missing <- function(name_site, product, command, args, overwrite) {
  if (!is.na(product_file(name_site, product)) && !overwrite) {
    message("  already present, skipping download")
    return(invisible(NULL))
  }
  status <- system2(command, args, stdout = TRUE, stderr = TRUE)
  code <- attr(status, "status")
  if (!is.null(code) && code != 0) {
    stop(
      "Download of ", product, " for ", name_site, " failed (exit ", code, "):\n",
      paste(utils::tail(status, 20), collapse = "\n")
    )
  }
  invisible(status)
}

download_icos <- function(name_site, overwrite = FALSE) {
  run_if_missing(
    name_site, "ICOS", "uv",
    c("run", "scripts/download-icos.py", "--product", "icos", "--sites", name_site,
      if (overwrite) "--overwrite"),
    overwrite
  )
}

# Warm Winter 2020 (1989-2020): the pre-labelling history for the sites whose
# ICOS and FLUXNET-Archive products both start too late.
download_ww2020 <- function(name_site, overwrite = FALSE) {
  run_if_missing(
    name_site, "WW2020", "uv",
    c("run", "scripts/download-icos.py", "--product", "ww2020", "--sites", name_site,
      if (overwrite) "--overwrite"),
    overwrite
  )
}

download_tern <- function(name_site, overwrite = FALSE) {
  run_if_missing(
    name_site, "TERN", "uv",
    c("run", "scripts/download-tern.py", "--sites", name_site,
      if (overwrite) "--overwrite"),
    overwrite
  )
}

download_fluxnet <- function(name_site, overwrite = FALSE) {
  run_if_missing(
    name_site, "FLUXNET", "bash",
    c("scripts/download-fluxnet.sh", "--sites", name_site,
      if (overwrite) "--overwrite"),
    overwrite
  )
}


# FLUXNET2015 is the one product here that cannot be fetched programmatically,
# and it is worth being precise about why rather than leaving a TODO.
#
# It is a static legacy release, not a service: there is no API, and the
# FLUXNET Shuttle -- which does have one -- federates AmeriFlux, ICOS and TERN
# only, none of which hold the pre-2015 record. Downloading requires an
# interactive login at fluxnet.org plus acceptance of the FLUXNET2015 Data
# Policy, which is granted per site-year (Tier 1 vs Tier 2). FluxDataKit, an R
# package built specifically around this dataset, reaches the same conclusion:
# "The data should be downloaded manually from the website data portal, and a
# login is required."
#
# So this "downloader" asks the user to do it, and tells them exactly what and
# where. That is more useful than a scraper that would break on the next
# redesign of the login form, and it does not pretend to hold a licence the
# user has to accept themselves.
download_fluxnet2015 <- function(name_site, overwrite = FALSE) {
  if (!is.na(product_file(name_site, "FLUXNET2015")) && !overwrite) {
    message("  already present, skipping download")
    return(invisible(NULL))
  }
  spec <- FLUX_PRODUCTS[["FLUXNET2015"]]
  outdir <- file.path(DIR_RAWDATA, spec$dir, name_site)
  stop(
    "FLUXNET2015 for ", name_site, " has to be downloaded by hand; it has no\n",
    "programmatic interface (see the comment above `download_fluxnet2015()`).\n\n",
    "  1. Sign in at https://fluxnet.org/login/ and accept the FLUXNET2015\n",
    "     Data Policy for the site-years you need.\n",
    "  2. From https://fluxnet.org/data/download-data/ fetch the FULLSET\n",
    "     archive for ", name_site, ", named like\n",
    "       FLX_", name_site, "_FLUXNET2015_FULLSET_<years>_<release>.zip\n",
    "  3. Unzip it into\n       ", outdir, "/\n",
    "     keeping the archive alongside the extracted files -- the filename\n",
    "     encodes the year span and release, which is what the update check\n",
    "     compares against.\n\n",
    "The reader needs the half-hourly table matching ", shQuote(spec$pattern), ".\n",
    "Re-run this once it is in place."
  )
}

# ---------------------------------------------------------------- WorldClim
#
# `04_01` needs minimum temperature on a 2.5 arc-minute grid for two periods: a
# 2000-2020 baseline and the 2041-2060 projection under SSP2-4.5, averaged over
# the CMIP6 GCMs. Both are public, unauthenticated downloads.
#
# The baseline is the CRU-TS-downscaled *monthly series*, not the 1970-2000
# climatology that `wc2.1_2.5m_tmin.zip` holds. The distinction matters: the
# script's own comment specifies 2000-2020, and using the climatology instead
# would silently shift every projected change by the warming between the two
# periods. The series ships as one GeoTIFF per year-month, which is exactly what
# `04_01`'s `grepl("<MM>.tif$")` glob expects -- it collects all the Januaries
# into one stack and averages them.
WORLDCLIM_BASE <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/hist/cts4.06/2.5m"
WORLDCLIM_CMIP6 <- "https://geodata.ucdavis.edu/cmip6/2.5m"
WORLDCLIM_DECADES <- c("2000-2009", "2010-2019", "2020-2021")
WORLDCLIM_BASELINE_YEARS <- 2000:2020

# The 13 GCMs WorldClim publishes tmin for at 2.5m under ssp245, matching the
# "13 global circulation models" in `04_01`'s header. GFDL-ESM4 is listed by
# WorldClim for other variables but has no tmin raster at this resolution.
WORLDCLIM_GCMS <- c(
  "ACCESS-CM2", "BCC-CSM2-MR", "CMCC-ESM2", "EC-Earth3-Veg", "FIO-ESM-2-0",
  "GISS-E2-1-G", "HadGEM3-GC31-LL", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6",
  "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
)

DIR_WORLDCLIM_BASELINE <- file.path(DIR_RAWDATA, "Climate", "wc2.1_2.5m_tmin")
DIR_WORLDCLIM_FUTURE <- file.path(DIR_RAWDATA, "Climate", "wc2.1_2.5m_tmin_2041-2060")

# Fetch to a `.part` file and rename on success, so an interrupted download can
# never be mistaken for a complete one by the presence check.
fetch_file <- function(url, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  part <- paste0(dest, ".part")
  status <- utils::download.file(url, part, mode = "wb", quiet = TRUE)
  if (!identical(status, 0L) || !file.exists(part) || file.size(part) == 0) {
    unlink(part)
    stop("Download failed: ", url)
  }
  file.rename(part, dest)
  dest
}

download_worldclim <- function(overwrite = FALSE) {
  # Baseline: one GeoTIFF per year-month, extracted flat.
  # The zip is named for the CRU-TS version it was downscaled from, but the
  # rasters inside are not: they are `wc2.1_2.5m_tmin_<YYYY>-<MM>.tif`.
  wanted <- as.vector(outer(
    WORLDCLIM_BASELINE_YEARS,
    sprintf("%02d", 1:12),
    function(y, m) sprintf("wc2.1_2.5m_tmin_%s-%s.tif", y, m)
  ))
  present <- list.files(DIR_WORLDCLIM_BASELINE, pattern = "[.]tif$")
  if (overwrite || !all(wanted %in% present)) {
    dir.create(DIR_WORLDCLIM_BASELINE, recursive = TRUE, showWarnings = FALSE)
    zipdir <- file.path(DIR_RAWDATA, "Climate", "_zips")
    dir.create(zipdir, recursive = TRUE, showWarnings = FALSE)
    for (decade in WORLDCLIM_DECADES) {
      zipname <- sprintf("wc2.1_cruts4.06_2.5m_tmin_%s.zip", decade)
      zippath <- file.path(zipdir, zipname)
      if (!file.exists(zippath)) {
        message("  WorldClim baseline: fetching ", zipname)
        fetch_file(file.path(WORLDCLIM_BASE, zipname), zippath)
      }
      utils::unzip(zippath, exdir = DIR_WORLDCLIM_BASELINE, junkpaths = TRUE)
    }
    # 2020-2021 brings 2021 with it; drop it so the baseline is exactly the
    # 2000-2020 the workflow documents.
    extra <- setdiff(list.files(DIR_WORLDCLIM_BASELINE, pattern = "[.]tif$"), wanted)
    if (length(extra)) {
      message("  WorldClim baseline: dropping ", length(extra), " file(s) outside 2000-2020")
      unlink(file.path(DIR_WORLDCLIM_BASELINE, extra))
    }
  }

  # Future: one 12-layer GeoTIFF per GCM.
  for (gcm in WORLDCLIM_GCMS) {
    fname <- sprintf("wc2.1_2.5m_tmin_%s_ssp245_2041-2060.tif", gcm)
    dest <- file.path(DIR_WORLDCLIM_FUTURE, fname)
    if (!overwrite && file.exists(dest)) next
    message("  WorldClim ssp245: fetching ", gcm)
    fetch_file(file.path(WORLDCLIM_CMIP6, gcm, "ssp245", fname), dest)
  }

  c(
    list.files(DIR_WORLDCLIM_BASELINE, pattern = "[.]tif$", full.names = TRUE),
    list.files(DIR_WORLDCLIM_FUTURE, pattern = "[.]tif$", full.names = TRUE)
  ) |> sort()
}

# ---------------------------------------------------------------- FAO GSOC
#
# Global Soil Organic Carbon map v1.5.0, used by `03_01` as the fallback soil
# carbon estimate wherever a site reports no measured SOIL_CHEM_C_ORG.
#
# The local filename matters: `03_01` indexes the extraction by layer name
# (`terra::extract(GSOCmap, xy)$GSOCmap1.5.0`), and terra derives that name from
# the file. Saving it under FAO's own name would silently yield NULL.
GSOC_URL <- paste0(
  "https://storage.googleapis.com/fao-gismgr-gsocseq-data/",
  "DATA/GSOCSEQ/MAP/GSOCSEQ.GSOCMAP1-5-0.tif"
)
GSOC_PATH <- file.path(DIR_RAWDATA, "GSOCmap1.5.0.tif")

download_gsoc <- function(overwrite = FALSE) {
  if (!overwrite && file.exists(GSOC_PATH)) return(GSOC_PATH)
  message("  GSOC: fetching GSOCmap v1.5.0 (~760 MB)")
  fetch_file(GSOC_URL, GSOC_PATH)
}

# ------------------------------------------------------- AmeriFlux BADM/BIF
#
# Site metadata, used by `03_01` for measured soil carbon. `amf_download_bif()`
# stamps the filename with the date it was produced, so the path is discovered
# rather than declared -- the workflow used to hard-code a datestamp that no
# longer existed.
download_ameriflux_bif <- function(overwrite = FALSE) {
  existing <- list.files(
    DIR_RAWDATA, pattern = "^AMF_AA-Net_BIF_.*[.](xlsx|csv)$", full.names = TRUE
  )
  if (!overwrite && length(existing)) {
    return(existing[which.max(file.mtime(existing))])
  }
  creds <- parse_toml("_creds.toml")
  amerifluxr::amf_download_bif(
    user_id = creds$user_id,
    user_email = creds$user_email,
    data_policy = "CCBY4.0",
    agree_policy = TRUE,
    intended_use = "synthesis",
    intended_use_text = "Thermal acclimation in ecosystem respiration synthesis project",
    out_dir = paste0(DIR_RAWDATA, "/"),
    verbose = TRUE
  )
}
