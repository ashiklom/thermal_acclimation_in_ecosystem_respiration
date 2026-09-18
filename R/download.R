# Fetch every product in a site's provenance list, returning the local paths.
#
# Most sites need more than one product: see `FLUX_PRODUCTS` in R/constants.R
# and docs/data-provenance.md for why.
download_site <- function(name_site, overwrite = FALSE) {
  site_info <- get_site_info(name_site)
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
