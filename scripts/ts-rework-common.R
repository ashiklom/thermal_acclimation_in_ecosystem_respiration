# Shared helpers for the soil-temperature rework scripts.
#
# Sourced by the `scripts/ts-*.R` analyses rather than living in `R/`, because
# nothing in the targets pipeline depends on it and putting it in `R/` would
# make every edit here invalidate the step-01 caches keyed on that directory.

suppressMessages({
  library(dplyr)
  library(targets)
})

# ---------------------------------------------------------------- step-01 cache
#
# `prep_nee_ac()` costs 30-140 s a site (REddyProc dominates at the AmeriFlux
# sites), and these analyses run over 60+ sites repeatedly. The cache is keyed
# on a digest of everything under `R/`, so any pipeline edit invalidates it
# rather than silently reporting stale numbers.
#
# Deliberately the same directory, file name and record layout as
# `tests/ts-swc-baseline.R` uses, so the two share hits instead of each paying
# for the same site.
STEP01_CACHE_DIR <- file.path("data-proc", "ts-baseline-cache")

r_code_digest <- function() {
  files <- sort(list.files("R", pattern = "[.]R$", full.names = TRUE))
  paste(vapply(files, function(f) as.character(tools::md5sum(f)), ""), collapse = "")
}

step01_cached <- function(name_site, code_key = r_code_digest()) {
  dir.create(STEP01_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(STEP01_CACHE_DIR, paste0(name_site, ".rds"))
  if (file.exists(path)) {
    hit <- readRDS(path)
    if (identical(hit$code_key, code_key)) return(hit$value)
  }
  value <- tryCatch(
    suppressWarnings(suppressMessages(prep_nee_ac(get_site_info(name_site)))),
    error = function(e) structure(conditionMessage(e), class = "baseline_error")
  )
  saveRDS(list(code_key = code_key, value = value), path)
  value
}

# ---------------------------------------------------------------- site sets

# US-Cwt and US-MBP -- soil temperature built from air temperature by a bare
# `name_site ==` branch in R/ameriflux.R, invisible to site_info.csv -- now live
# in `SITES_TS_SYNTHETIC` in R/constants.R alongside GF-Guy, so the pipeline
# and these scripts agree on what "synthetic" means. Kept under the old name
# for the callers below.
SITES_TS_SYNTHETIC_AMERIFLUX <- setdiff(SITES_TS_SYNTHETIC, "GF-Guy")

# Sites whose soil temperature is genuinely measured: it is neither
# reconstructed in step 01 (`estimate_Ts`, `SITES_TS_FROM_TA_*`,
# `SITES_TS_SYNTHETIC_AMERIFLUX`) nor substituted in step 02 (`ts_col`).
# These are the only sites where a reconstruction can be scored against a
# truth, so they are the evaluation set for everything here.
#
# That this set has to be assembled from one CSV column, two constants in
# `R/constants.R` and two hard-coded branches in `R/ameriflux.R` is the
# argument for step 3 in miniature: "is this site's soil temperature real?"
# is not currently answerable from the data, only from five scattered
# declarations that no test relates to each other.
measured_ts_sites <- function(path = SITE_INFO_CSV) {
  si <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  si |>
    dplyr::filter(
      .data$ts_col == "TS_measured",
      .data$estimate_Ts == "NO",
      !.data$site_ID %in% c(
        SITES_TS_FROM_TA_RECENT, SITES_TS_FROM_TA_COLD,
        SITES_TS_SYNTHETIC_AMERIFLUX
      )
    ) |>
    dplyr::pull("site_ID")
}

# ---------------------------------------------------------------- CLI

# `--sites A,B,C` or `--sites all`; anything else falls back to `default`.
parse_sites_arg <- function(args, default) {
  i <- match("--sites", args)
  if (is.na(i) || i == length(args)) return(default)
  val <- args[[i + 1]]
  if (identical(val, "all")) return(default)
  strsplit(val, ",", fixed = TRUE)[[1]]
}

parse_flag <- function(args, flag) flag %in% args

parse_opt <- function(args, opt, default) {
  i <- match(opt, args)
  if (is.na(i) || i == length(args)) return(default)
  args[[i + 1]]
}

# `ts_reconstruction_metrics()`, `peak_hour()`, `wrap_lag()`,
# `daily_amplitudes()`, `tas_windows()` and `window_cells()` used to be defined
# here. They now live in R/ts-qc.R and R/ts-fill.R, which `tar_source()` loads
# before this file is sourced, so the analysis scripts and the pipeline share
# one definition of each.
