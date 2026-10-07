# ERA5-Land layer-1 soil water: the file, a site's slice of it, its join onto
# step 01's tables, and keeping the file current.

#' Scale factor from ERA5-Land soil water (m3/m3) to percent
#'
#' Soil water is a percent everywhere in the analysis, like the tower columns
#' and the `Hs` prior; ERA5-Land's `swvl1` is m3/m3, rescaled on read.
#' `data-raw/` keeps the provider's units.
ERA5_SWC_TO_PERCENT <- 100

#' Path to the ERA5-Land daily soil water file
ERA5_SWC_CSV <- file.path(DIR_RAWDATA, "ERA5_daily_swc.csv")

#' The whole ERA5 table, every site
#'
#' Parsed once per pipeline run and sliced per site by `read_era5_swc(table = )`,
#' rather than re-read for each of 117 sites.
#'
#' @param path Path to the ERA5 soil water CSV.
#' @return Tibble with `time` (Date), `site` (site ID) and `SWC` (m3/m3).
load_era5_table <- function(path = ERA5_SWC_CSV) {
  readr::read_csv(
    path,
    col_types = readr::cols(
      time = readr::col_date(),
      site = readr::col_character(),
      SWC = readr::col_double()
    ),
    progress = FALSE
  )
}

#' Read one site's ERA5 soil water
#'
#' @param name_site Site ID.
#' @param path Path to the ERA5 soil water CSV; names the file in error
#'   messages.
#' @param table If given, the file's already parsed contents
#'   (`load_era5_table()`).
#' @return Tibble of daily `YEAR`, `MONTH`, `DAY` and `SWC` (percent). Errors if
#'   the site is absent, has duplicate dates, is all NA, or is not in m3/m3.
read_era5_swc <- function(name_site, path = ERA5_SWC_CSV, table = NULL) {
  if (is.null(table)) table <- load_era5_table(path)
  swc <- dplyr::filter(table, .data$site == name_site)

  if (nrow(swc) == 0) {
    stop("No ERA5 soil water data for site ", name_site, " in ", path, ".")
  }

  # One row per site-day, or the daily join would multiply rows.
  if (anyDuplicated(swc[c("time")])) {
    stop(
      "ERA5 soil water for ", name_site, " has duplicate dates in ", path,
      ". Expected one row per site-day."
    )
  }

  # All NA: the nearest cell is sea. The site needs a `COASTAL_SITES` entry
  # in scripts/download-era5-swc.py. Checked before the unit guard, which
  # would misreport it.
  if (all(is.na(swc$SWC))) {
    stop(
      "ERA5 data found but all values are NA for site ", name_site, " in ", path,
      " (", nrow(swc), " rows, ", min(swc$time), " to ", max(swc$time), "). ",
      "The nearest ERA5-Land cell is most likely outside the land mask. ",
      "Run `scripts/find-coastal-land-pixel.py ", name_site, "` and add the ",
      "result to COASTAL_SITES in scripts/download-era5-swc.py, then ",
      "re-download; re-downloading on its own will not change this."
    )
  }

  # Volumetric soil water is well below 1; anything else is the wrong unit.
  swc_max <- max(swc$SWC, na.rm = TRUE)
  if (!is.finite(swc_max) || swc_max > 1.5) {
    stop(
      "ERA5 soil water for ", name_site, " has a maximum of ", signif(swc_max, 4),
      ", which is not a volumetric fraction (m3/m3). Expected values within ",
      "[0, 1]; check the units in ", path, "."
    )
  }

  swc |>
    dplyr::mutate(
      date = as.Date(.data$time),
      YEAR = lubridate::year(.data$date),
      MONTH = lubridate::month(.data$date),
      DAY = lubridate::day(.data$date),
      SWC = .data$SWC * ERA5_SWC_TO_PERCENT
    ) |>
    dplyr::select("YEAR", "MONTH", "DAY", "SWC")
}

#' One site's ERA5 soil water, clipped to its flux record
#'
#' ERA5 soil water for one site, clipped to the days its flux record covers,
#' so extending the file for another site leaves this slice -- and everything
#' downstream of it -- unchanged. Missing reanalysis returns NULL rather than
#' failing: only the direct model needs it, and `resolve_swc_column()`
#' complains there.
#'
#' @param prep Step 01's result for the site, from `prep_nee_ac()`; its `ac`
#'   gives the days.
#' @param name_site Site ID.
#' @param path Path to the ERA5 soil water CSV.
#' @param table The parsed file, if the caller has it.
#' @return Tibble of daily `YEAR`, `MONTH`, `DAY` and `SWC` (percent) on the
#'   record's days, or NULL if the site's ERA5 data could not be read.
site_era5_swc <- function(prep, name_site, path = ERA5_SWC_CSV, table = NULL) {
  era5 <- tryCatch(
    read_era5_swc(name_site, path = path, table = table),
    error = function(e) {
      message("  ERA5 soil water unavailable (", conditionMessage(e), ")")
      NULL
    }
  )
  if (is.null(era5)) return(NULL)
  days <- dplyr::distinct(prep[["ac"]], .data$YEAR, .data$MONTH, .data$DAY)
  dplyr::semi_join(era5, days, by = c("YEAR", "MONTH", "DAY"))
}

#' Join a site's ERA5 soil water onto step 01's tables
#'
#' As `SWC_era5`, in percent, beside the measured column.
#'
#' @param prep Step 01's result for one site, from `prep_nee_ac()`.
#' @param era5 The site's ERA5 soil water, from `site_era5_swc()`, or NULL.
#' @return `prep`, with `SWC_era5` added to `ac` and `nightNEE` (all NA if
#'   `era5` is NULL).
attach_era5_swc <- function(prep, era5) {
  for (tbl in c("ac", "nightNEE")) {
    dat <- prep[[tbl]]
    if (is.null(era5)) {
      dat[["SWC_era5"]] <- NA_real_
    } else {
      n <- nrow(dat)
      dat <- dplyr::left_join(
        dat, dplyr::rename(era5, SWC_era5 = "SWC"),
        by = c("YEAR", "MONTH", "DAY")
      )
      # A daily table joined onto half-hourly rows must annotate, never multiply.
      stopifnot(nrow(dat) == n)
    }
    prep[[tbl]] <- dat
  }
  prep
}

#' Keep the ERA5 soil water file current
#'
#' One file for every site, extended by `scripts/download-era5-swc.py` only
#' when a site is missing from it or some site's flux record runs past its end
#' (`through`). The script is incremental and leaves an up-to-date file alone.
#'
#' @param through Date the file must reach (the latest flux record end), or NA
#'   for no requirement.
#' @param site_info_path Path to site_info.csv.
#' @param path Path to the ERA5 soil water CSV.
#' @return `path`, once the file covers every site and `through`. Errors if the
#'   extraction script fails.
ensure_era5_coverage <- function(through, site_info_path = SITE_INFO_CSV, path = ERA5_SWC_CSV) {
  sites <- get_site_info(path = site_info_path)[["site_ID"]]
  if (file.exists(path)) {
    have <- load_era5_table(path)
    # All-NA counts as missing, as in the script (a sea cell).
    missing_sites <- setdiff(sites, have$site[!is.na(have$SWC)])
    last <- max(have$time)
    if (!length(missing_sites) && (is.na(through) || last >= through)) {
      return(path)
    }
    message(
      "  ERA5: file ends ", last, "; flux data runs to ", through,
      if (length(missing_sites)) paste0("; ", length(missing_sites), " site(s) missing")
    )
  } else {
    message("  ERA5: ", path, " not found; extracting every site")
  }
  args <- "scripts/download-era5-swc.py"
  if (!is.na(through)) args <- c(args, "--through", format(as.Date(through)))
  status <- system2("python", args, stdout = "", stderr = "")
  if (!identical(status, 0L)) stop("ERA5 extraction failed (exit ", status, ").")
  path
}

#' The latest of the sites' record ends
#'
#' An errored site contributes NULL (`error = "null"`), and if every site
#' errored there is no date to extend to.
#'
#' @param ... Each site's record end (Date, from `site_record_end()`), or NULL.
#' @return Date: the latest end, or NA if there is none.
latest_record_end <- function(...) {
  ends <- do.call(c, list(...))
  if (!length(ends) || all(is.na(ends))) return(as.Date(NA))
  max(ends, na.rm = TRUE)
}

#' Last day with measured NEE
#'
#' Not the last row, since step 01 pads the record to whole years.
#'
#' @param prep Step 01's result for one site, from `prep_nee_ac()`.
#' @return Date; NA if the site has no measured NEE.
site_record_end <- function(prep) {
  ac <- prep[["ac"]]
  ac <- ac[!is.na(ac$NEE), ]
  if (!nrow(ac)) return(as.Date(NA))
  max(as.Date(ISOdate(ac$YEAR, ac$MONTH, ac$DAY)))
}
