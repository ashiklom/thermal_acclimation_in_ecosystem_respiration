# Fetch every product in a site's provenance list, returning the local paths.
#
# Most sites need more than one product: see `FLUX_PRODUCTS` in R/constants.R
# and docs/data-provenance.md for why.
download_site <- function(name_site, overwrite = FALSE) {
  site_info <- get_site_info(name_site)
  downloaders <- list(
    AmeriFlux_BASE = download_ameriflux,
    FLUXNET = download_fluxnet,
    FLUXNET2015 = download_fluxnet,
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
