download_site <- function(name_site, overwrite = FALSE) {
  site_info <- get_site_info(name_site)
  site_source <- site_info$source
  fn <- list(
    "AmeriFlux_BASE" = download_ameriflux,
    "ICOS" = download_icos,
    "TERN" = download_tern,
    "FLUXNET" = download_fluxnet
  )[[site_source]]
  if (is.null(fn)) {
    stop("No download method for site ", name_site, " with source `", site_source, "`.")
  }
  fn(name_site, overwrite = overwrite)
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

download_icos <- function(name_site, overwrite = FALSE) {
  target <- file.path("data-raw", "ICOS", name_site, sprintf("%s_ICOS_L2_FLUXNET_HH.csv", name_site))
  if (!file.exists(target) || overwrite) {
    system2(
      "uv",
      c("run", "scripts/download-icos.py", "--sites", name_site, "--overwrite"),
      stdout = TRUE,
      stderr = TRUE
    )
  }
  target
}

download_tern <- function(name_site, overwrite = FALSE) {
  target <- file.path("data-raw", "TERN", name_site, sprintf("%s_ICOS_L3_FLUXNET_HH.csv", name_site))
  if (!file.exists(target) || overwrite) {
    system2(
      "uv",
      c("run", "scripts/download-tern.py", "--sites", name_site, "--overwrite"),
      stdout = TRUE,
      stderr = TRUE
    )
  }
  target
}

download_fluxnet <- function(name_site, overwrite = FALSE) {
  target <- file.path("data-raw", "FLUXNET", name_site, sprintf("%s_ICOS_L3_FLUXNET_HH.csv", name_site))
  if (!file.exists(target) || overwrite) {
    system2(
      "bash",
      c("scripts/download-fluxnet.sh", "--sites", name_site, "--overwrite"),
      stdout = TRUE,
      stderr = TRUE
    )
  }
  target
}
