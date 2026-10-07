#' Replace sentinels and implausible values with NA
#'
#' Everything that is not a measurement, removed in one place: the documented
#' -9999 sentinel, plus anything outside `IMPLAUSIBLE_BOUNDS`. Both readers --
#' `read_spliced_products()` for the FLUXNET-format products and
#' `prep_ameriflux()` for AmeriFlux BASE -- go through here, so a sentinel found
#' in one product's files is caught in the other's too.
#'
#' Out-of-bound values become NA, not the bound: a reading of 33457 % says
#' nothing about the humidity, and REddyProc gap-fills NA as it does -9999.
#' The `warning()` lands in `tar_meta(fields = "warnings")`, so a new sentinel
#' in a future release shows up in the run's record.
#'
#' @param dat Data frame of half-hourly flux data.
#' @return `dat`, with -9999 and out-of-bound values set to NA.
drop_sentinels <- function(dat) {
  dat[dat == -9999] <- NA
  for (prefix in names(IMPLAUSIBLE_BOUNDS)) {
    bound <- IMPLAUSIBLE_BOUNDS[[prefix]]
    cols <- grep(paste0("^", prefix, "($|_)"), names(dat), value = TRUE)
    cols <- cols[!grepl("_QC$", cols)]
    for (col in cols) {
      x <- dat[[col]]
      if (!is.numeric(x)) next
      bad <- !is.na(x) & (x < bound[[1]] | x > bound[[2]])
      if (!any(bad)) next
      warning(
        sum(bad), " value(s) in ", col, " outside [", bound[[1]], ", ",
        bound[[2]], "] set to NA: ",
        paste(utils::head(sort(unique(x[bad])), 5), collapse = ", "),
        call. = FALSE
      )
      x[bad] <- NA
      dat[[col]] <- x
    }
  }
  dat
}

#' Read the site declaration table
#'
#' Reads site_info.csv with a fixed column specification, recodes the
#' `YES`/`NO` columns to logical, and checks that every site declares a known
#' `ts_source`.
#'
#' @param site_ID Site ID to return, or `NULL` for every site.
#' @param path Path to site_info.csv. A parameter so the pipeline can hand in
#'   the `format = "file"` target, making an edit to the CSV invalidate the
#'   sites that read it.
#' @return A tibble of site declarations: one row for `site_ID`, or every row
#'   if `site_ID` is `NULL`.
get_site_info <- function(site_ID = NULL, path = SITE_INFO_CSV) {
  site_info_cols <- readr::cols(
    site_ID = "c",
    LAT = "d",
    LONG = "d",
    ELEV = "d",
    IGBP = "c",
    Climate_class = "c",
    MAP = "d",
    source = "c",
    estimate_Ts = "c",
    year_removed = "c",
    NEE = "c",
    FC = "c",
    TA = "c",
    TS = "c",
    SWC = "c",
    SW_IN = "c",
    USTAR = "c",
    RH = "c",
    VPD = "c",
    comment = "c",
    gStart = "i",
    gEnd = "i",
    SWC_use = "c",
    estimate_ts_method = "c",
    netrad_column = "c",
    ts_col = "c",
    ts_linear_domain = "c",
    growing_year_start = "i",
    ts_source = "c"
  )

  dat <- readr::read_csv(path, col_types = site_info_cols)

  dat_clean <- dat |>
    dplyr::mutate(
      estimate_Ts = dplyr::recode_values(
        .data$estimate_Ts,
        "YES" ~ TRUE,
        "NO" ~ FALSE
      ),
      SWC_use = dplyr::recode_values(
        .data$SWC_use,
        "YES" ~ TRUE,
        "NO" ~ FALSE
      )
    )

  # `ts_source` has to be declared at every site: a missing level would make
  # the readers' dispatch and the fill's truth check disagree silently.
  bad_source <- dat_clean$site_ID[is.na(dat_clean$ts_source) | !dat_clean$ts_source %in% names(TS_SOURCES)]
  if (length(bad_source)) {
    stop(
      path, ": ts_source is missing or unknown at ", paste(bad_source, collapse = ", "),
      ". Levels: ", paste(names(TS_SOURCES), collapse = ", "),
      ". Re-run scripts/revise-site-info.R."
    )
  }

  if (is.null(site_ID)) return(dat_clean)

  result <- dat_clean |>
    dplyr::filter(.data$site_ID == .env$site_ID)
  if (nrow(result) == 0) {
    stop("Site ", shQuote(site_ID), " not found in site_info.csv")
  }
  result
}

#' The ordered provenance list for a site
#'
#' Oldest product first. See `FLUX_PRODUCTS` in R/constants.R for why this is a
#' list and not a scalar.
#'
#' @param site_info One row of the site declaration table.
#' @return Character vector of `FLUX_PRODUCTS` names, oldest product first.
site_sources <- function(site_info) {
  sources <- trimws(unlist(strsplit(site_info[["source"]], "+", fixed = TRUE)))
  unknown <- setdiff(sources, names(FLUX_PRODUCTS))
  if (length(unknown)) {
    stop(
      "Site ", site_info[["site_ID"]], " names unknown data product(s): ",
      paste(shQuote(unknown), collapse = ", "),
      ". Known products: ", paste(names(FLUX_PRODUCTS), collapse = ", "), "."
    )
  }
  sources
}

#' The day of year a site's growing year begins
#'
#' Northern-hemisphere sites run on the calendar year, so DOY 1 and no wrapping.
#' A site whose growing season straddles New Year declares the DOY it begins on
#' instead, and `wrap_growing_doy()` shifts the earlier part of each calendar
#' year past DOY 366 so that the season is one contiguous interval. AU-Tum and
#' ZA-Kru declare 183.
#'
#' This is declared per site rather than derived from `LAT < 0` because the two
#' are different questions. BR-Ma2 and BR-Sa1 are south of the equator but have
#' no temperature seasonality: their growing season is pinned to the whole
#' calendar year (gStart 1, gEnd 366), and wrapping them would put those bounds
#' outside the data the windows tile. The original workflows wrapped by an
#' explicit site list for that reason -- and wrapped no AmeriFlux site at all.
#'
#' @param site_info One row of the site declaration table.
#' @return Integer day of year in 1--366; `1L` if the site declares none.
growing_year_start <- function(site_info) {
  declared <- site_info[["growing_year_start"]]
  if (is.null(declared) || length(declared) != 1 || is.na(declared)) return(1L)
  declared <- as.integer(declared)
  if (declared < 1 || declared > 366) {
    stop(
      "Site ", site_info[["site_ID"]], " declares growing_year_start = ",
      declared, ", which is not a day of year."
    )
  }
  declared
}

#' Which reader handles this site
#'
#' AmeriFlux BASE needs its own path (u-star filtering, per-site column names);
#' everything else is FLUXNET-format.
#'
#' @param site_info One row of the site declaration table.
#' @return `"ameriflux"` or `"fluxnet_family"`.
site_reader <- function(site_info) {
  if ("AmeriFlux_BASE" %in% site_sources(site_info)) "ameriflux" else "fluxnet_family"
}

#' Where a product's half-hourly table for a site lives
#'
#' @param site Site ID.
#' @param product Name of a `FLUX_PRODUCTS` entry.
#' @return Path to the table, or `NA_character_` if absent. Errors if more than
#'   one file matches.
product_file <- function(site, product) {
  spec <- FLUX_PRODUCTS[[product]]
  if (is.null(spec)) stop("Unknown data product: ", product)
  hits <- list.files(
    file.path(DIR_RAWDATA, spec$dir, site),
    pattern = spec$pattern, full.names = TRUE, recursive = TRUE
  )
  if (length(hits) == 0) return(NA_character_)
  if (length(hits) > 1) {
    stop(
      "Found ", length(hits), " candidate ", product, " files for ", site, ":\n",
      paste(" ", hits, collapse = "\n"),
      "\nExpected exactly one. Remove the stale copies."
    )
  }
  hits
}

#' Which timestamps lie in a window
#'
#' YYYYMMDDHHMM is exact as a double, so the comparison is numeric.
#'
#' @param ts 12-digit TIMESTAMP_START values: character, or numeric as
#'   `amf_read_base()` parses them.
#' @param window Length-2 vector of 12-digit timestamps bounding the window,
#'   inclusive; NA is an open end.
#' @return Logical vector, `TRUE` where `ts` lies in `window`.
in_timestamp_window <- function(ts, window) {
  ts <- as.numeric(ts)
  lo <- as.numeric(window[[1]])
  hi <- as.numeric(window[[2]])
  (is.na(lo) | ts >= lo) & (is.na(hi) | ts <= hi)
}

#' Which sites the pipeline builds targets for
#'
#' A site that cannot be processed -- no raw data obtainable (ZA-Kru), or a
#' reader branch that is still a `stop()` (FR-Pue) -- fails as its own target
#' and shows as a gap in the results, rather than being silently left out here.
#'
#' Called while the pipeline is constructed (`tar_map()` needs the names), so
#' it cannot be a target and reads site_info.csv directly.
#'
#' @param scope `"dev"` (the default, `DEV_SITES`), `"all"` (every row of
#'   site_info.csv), or a comma-separated list of site IDs.
#' @param site_info The site declaration table.
#' @return Character vector of site IDs.
pipeline_sites <- function(scope = Sys.getenv("THERMAL_SITES", "dev"),
                           site_info = get_site_info()) {
  known <- site_info[["site_ID"]]

  if (identical(scope, "all")) return(known)
  if (!identical(scope, "dev")) {
    wanted <- trimws(strsplit(scope, ",")[[1]])
    unknown <- setdiff(wanted, known)
    if (length(unknown)) {
      stop(
        "THERMAL_SITES names ", length(unknown), " site(s) that are not in ",
        SITE_INFO_CSV, ": ", paste(shQuote(unknown), collapse = ", "),
        ". Use \"dev\", \"all\", or a comma-separated list of site IDs."
      )
    }
    return(wanted)
  }

  # A typo here would otherwise surface as per-site download failures.
  unknown <- setdiff(DEV_SITES, known)
  if (length(unknown)) {
    stop(
      "DEV_SITES names ", length(unknown), " site(s) that are not in ",
      SITE_INFO_CSV, ": ", paste(shQuote(unknown), collapse = ", "), "."
    )
  }
  DEV_SITES
}
