library(amerifluxr)

source("R/utils.R")

credfile <- "_creds.toml"
stopifnot(file.exists(credfile))
creds <- parse_toml(credfile)

downloaded_files <- amf_download_bif(
  user_id = creds$user_id,
  user_email = creds$user_email,
  data_policy = "CCBY4.0",
  agree_policy = TRUE,
  intended_use = "synthesis",
  intended_use_text = "Thermal acclimation in ecosystem respiration synthesis project",
  out_dir = "data-raw/",
  verbose = TRUE
)
