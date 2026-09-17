# Purpose: Download AmeriFlux BASE-BADM data for sites in site_info.csv
# Usage:
#   pixi run Rscript workflows/91-download-ameriflux.R
#   pixi run Rscript workflows/91-download-ameriflux.R --sites US-Kon,US-Ha1
#   pixi run Rscript workflows/91-download-ameriflux.R --site_info path/to/site_info.csv
#   pixi run Rscript workflows/91-download-ameriflux.R --overwrite
#   pixi run Rscript workflows/91-download-ameriflux.R --credentials path/to/_creds.toml

library(amerifluxr)
library(optparse)

source("R/utils.R")

# Parse command line arguments
option_list <- list(
  make_option(c("-s", "--sites"), type = "character", default = NULL,
              help = "Comma-separated list of AmeriFlux site IDs to download (e.g., 'US-Kon,US-Ha1')"),
  make_option(c("-f", "--site_info"), type = "character", default = "data-core/site_info.csv",
              help = "Path to site_info.csv file [default: data-core/site_info.csv]"),
  make_option(c("-o", "--overwrite"), action = "store_true", default = FALSE,
              help = "Overwrite existing downloaded files [default: FALSE]"),
  make_option(c("-c", "--credentials"), type = "character", default = "./_creds.toml",
              help = "Path to TOML file with user_id and user_email [default: ./_creds.toml]"),
  make_option(c("-u", "--user_id"), type = "character", default = NULL,
              help = "AmeriFlux account user_id (overrides credentials file)"),
  make_option(c("-e", "--user_email"), type = "character", default = NULL,
              help = "AmeriFlux account user_email (overrides credentials file)"),
  make_option(c("-d", "--out_dir"), type = "character", default = "data-raw/Ameriflux",
              help = "Output directory for downloaded data [default: data-raw/Ameriflux]"),
  make_option(c("-p", "--data_policy"), type = "character", default = "CCBY4.0",
              help = "AmeriFlux data policy: 'CCBY4.0' or 'LEGACY' [default: CCBY4.0]")
)
parser <- OptionParser(description = "Download AmeriFlux BASE-BADM data for specified sites",
                       option_list = option_list)
opt <- parse_args(parser, commandArgs(trailingOnly = TRUE))

# Read credentials: CLI args override TOML file
creds <- list()
if (!is.null(opt$credentials) && file.exists(opt$credentials)) {
  message(sprintf("Reading credentials from %s", opt$credentials))
  creds <- parse_toml(opt$credentials)
}

user_id <- if (!is.null(opt$user_id)) opt$user_id else creds$user_id
user_email <- if (!is.null(opt$user_email)) opt$user_email else creds$user_email

if (is.null(user_id) || is.null(user_email)) {
  stop("AmeriFlux credentials are required. ",
       "Provide --user_id and --user_email, or set them in a credentials TOML file.")
}

message(sprintf("Using user_id: %s", user_id))

# Read site info
site_info <- read.csv(opt$site_info, stringsAsFactors = FALSE)

# Filter for AmeriFlux sites only
ameriflux_sites <- site_info[site_info$source == "AmeriFlux_BASE", ]

# If specific sites requested, filter further
if (!is.null(opt$sites)) {
  requested_sites <- trimws(unlist(strsplit(opt$sites, ",")))
  ameriflux_sites <- ameriflux_sites[ameriflux_sites$site_ID %in% requested_sites, ]
  if (nrow(ameriflux_sites) == 0) {
    stop("No matching AmeriFlux sites found in site_info.csv for: ", opt$sites)
  }
}

message(sprintf("Found %d AmeriFlux sites to download", nrow(ameriflux_sites)))

# Create output directory if it doesn't exist
if (!dir.exists(opt$out_dir)) {
  dir.create(opt$out_dir, recursive = TRUE)
}

# Check which sites already exist (unless overwrite)
sites_to_download <- ameriflux_sites$site_ID
if (!opt$overwrite) {
  existing_files <- list.files(opt$out_dir, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)
  existing_files <- basename(existing_files)
  existing_sites <- gsub("AMF_(.*)_BASE.*", "\\1", existing_files)
  already_present <- intersect(sites_to_download, existing_sites)
  if (length(already_present) > 0) {
    message(sprintf("Skipping %d sites already present in %s: %s",
                    length(already_present), opt$out_dir,
                    paste(already_present, collapse = ", ")))
    sites_to_download <- setdiff(sites_to_download, existing_sites)
  }
}

if (length(sites_to_download) == 0) {
  message("All sites already downloaded. Nothing to do.")
  quit(status = 0)
}

message(sprintf("Downloading %d sites: %s", length(sites_to_download),
                paste(sites_to_download, collapse = ", ")))

# Download data
downloaded_files <- amf_download_base(
  user_id = user_id,
  user_email = user_email,
  site_id = sites_to_download,
  data_product = "BASE-BADM",
  data_policy = opt$data_policy,
  agree_policy = TRUE,
  intended_use = "synthesis",
  intended_use_text = "Thermal acclimation in ecosystem respiration synthesis project",
  out_dir = opt$out_dir,
  verbose = TRUE
)

# Move downloaded files into site-specific directories.
downloaded_basenames <- basename(downloaded_files)
downloaded_sites <- sub("^AMF_([^_]+)_BASE.*", "\\1", downloaded_basenames)
target_paths <- file.path(opt$out_dir, downloaded_sites, downloaded_basenames)
target_dirs <- unique(dirname(target_paths))
lapply(target_dirs, dir.create, recursive = TRUE, showWarnings = FALSE)
files_needing_move <- downloaded_files[downloaded_files != target_paths]
if (length(files_needing_move) > 0) {
  for (i in which(downloaded_files != target_paths)) {
    file.rename(downloaded_files[i], target_paths[i])
  }
}

message(sprintf("Successfully downloaded %d files to %s", length(downloaded_files), opt$out_dir))
