#!/usr/bin/env Rscript
#
# Why are the ICOS-sourced records shorter than the manuscript's, and does
# splicing FLUXNET + ICOS recover them?
#
# Background. ICOS publishes two different ecosystem products:
#
#   * "Ecosystem final quality (L2) product in ETC-Archive format" (datatype
#     etcL2Fluxnet), which `scripts/download-icos.py` requests. By design it
#     covers only the period since each station was ICOS-labelled, so a station
#     labelled in 2019 has a product starting in 2019 however long it has been
#     running. https://www.icos-cp.eu/data-products
#   * FLUXNET-format products carrying the full history -- Warm Winter 2020
#     (1989-2020) and Drought-2018 (1989-2018) -- which FLUXNET shuttle serves
#     as a single merged per-site archive.
#
# The original workflow used both: every one of these sites was
# `FLUXNET2025_ICOS2025` or `FLUXNET2020_ICOS2025`, i.e. a FLUXNET release
# spliced with the ICOS release. The refactor kept the ICOS half only.
#
# This script reports, per site, the span of whatever sources are on disk, the
# span FLUXNET shuttle advertises, and the original's qualifying-year count, then
# splices the local sources with the original's own rule to show what is
# recovered. Run from the project root:
#
#   Rscript scripts/audit-icos-coverage.R

suppressMessages({
  library(dplyr)
})

NEEDED <- c(
  "TIMESTAMP_START", "NEE_VUT_REF", "NEE_VUT_REF_QC", "TA_F_MDS", "TA_F_MDS_QC",
  "TS_F_MDS_1", "TS_F_MDS_1_QC", "SWC_F_MDS_1", "SW_IN_F_MDS", "NIGHT"
)

read_source <- function(path, label) {
  dat <- read.csv(path)
  dat[dat == -9999] <- NA
  keep <- intersect(NEEDED, names(dat))
  dat <- dat[, keep, drop = FALSE]
  for (col in setdiff(NEEDED, keep)) dat[[col]] <- NA_real_
  dat$src <- label
  dat$TIMESTAMP_START <- as.character(dat$TIMESTAMP_START)
  dat[order(dat$TIMESTAMP_START), ]
}

# Whatever is on disk for this site, in either product.
site_sources <- function(site) {
  out <- list()
  fluxnet <- list.files(
    file.path("data-raw", "FLUXNET", site),
    pattern = "_FLUXMET_(HH|HR)_", full.names = TRUE, recursive = TRUE
  )
  if (length(fluxnet) == 1) out[["FLUXNET"]] <- read_source(fluxnet, "FLUXNET")
  icos <- file.path("data-raw", "ICOS", site, sprintf("%s_ICOS_L2_FLUXNET_HH.csv", site))
  if (file.exists(icos)) out[["ICOS"]] <- read_source(icos, "ICOS")
  out
}

# The original splice rule (01_02a_filter_high_quality_night_respiration_EuroFlux.R:57-67):
# append each later source only from the first timestamp after the previous
# source's end.
stitch <- function(sources) {
  sources <- sources[order(vapply(sources, function(d) d$TIMESTAMP_START[1], ""))]
  acc <- sources[[1]]
  for (i in seq_along(sources)[-1]) {
    nxt <- sources[[i]]
    acc <- dplyr::bind_rows(acc, nxt[nxt$TIMESTAMP_START > acc$TIMESTAMP_START[nrow(acc)], ])
  }
  acc
}

year_of <- function(dat) as.integer(substr(dat$TIMESTAMP_START, 1, 4))

main <- function() {
  site_info <- read.csv(file.path("data-core", "site_info.csv"))
  orig <- read.csv(file.path("data-proc-original", "growing_season_feature_EuropFlux.csv"))
  snapshot_file <- sort(list.files("data-raw", "^fluxnet_shuttle_snapshot_.*[.]csv$",
                                   full.names = TRUE))
  shuttle <- if (length(snapshot_file)) {
    read.csv(tail(snapshot_file, 1))
  } else {
    NULL
  }

  sites <- site_info$site_ID[site_info$source == "ICOS"]
  for (site in sites) {
    sources <- site_sources(site)
    shut <- if (!is.null(shuttle) && site %in% shuttle$site_id) {
      row <- shuttle[match(site, shuttle$site_id), ]
      sprintf("%d-%d (%2d)", row$first_year, row$last_year, row$last_year - row$first_year + 1)
    } else {
      "unknown"
    }
    orig_ny <- orig$nyear[match(site, orig$site_ID)]

    if (!length(sources)) {
      cat(sprintf("%-7s no local sources | shuttle advertises %s | orig_nyear %s\n",
                  site, shut, orig_ny))
      next
    }
    have <- vapply(names(sources), function(n) {
      y <- year_of(sources[[n]])
      sprintf("%s %d-%d", n, min(y), max(y))
    }, "")
    combined <- stitch(sources)
    y <- year_of(combined)
    night_years <- combined |>
      dplyr::filter(!is.na(.data$NEE_VUT_REF), .data$NIGHT == 1) |>
      dplyr::mutate(Y = as.integer(substr(.data$TIMESTAMP_START, 1, 4))) |>
      dplyr::pull("Y") |>
      unique() |>
      length()
    cat(sprintf(
      "%-7s %-38s -> %d-%d (%2d yr) | yrs with night NEE %2d | shuttle %s | orig_nyear %2d  %s\n",
      site, paste(have, collapse = " + "), min(y), max(y), max(y) - min(y) + 1,
      night_years, shut, orig_ny,
      if (!is.na(orig_ny) && night_years >= orig_ny) "RECOVERED" else "short"
    ))
  }
}

main()
