get_site_info <- function(site_ID = NULL) {
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
    netrad_column = "c"
  )

  dat <- readr::read_csv(
    file.path("data-core", "site_info.csv"),
    col_types = site_info_cols
  )

  # Do some cleanup
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

  if (is.null(site_ID)) return(dat_clean)

  result <- dat_clean |>
    dplyr::filter(.data$site_ID == .env$site_ID)
  if (nrow(result) == 0) {
    stop("Site ", shQuote(site_ID), " not found in site_info.csv")
  }
  result
}

# The ordered provenance list for a site, oldest product first. See
# `FLUX_PRODUCTS` in R/constants.R for why this is a list and not a scalar.
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

# Which reader handles this site. AmeriFlux BASE needs its own path (u-star
# filtering, per-site column names); everything else is FLUXNET-format.
site_reader <- function(site_info) {
  if ("AmeriFlux_BASE" %in% site_sources(site_info)) "ameriflux" else "fluxnet_family"
}

# Where a given product's half-hourly table for a site lives, or NA if absent.
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

# Helper: parse simple TOML key = "value" lines
parse_toml <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- grep("=", lines, value = TRUE)
  lines <- gsub("#.*$", "", lines)
  lines <- trimws(lines)
  keep <- nchar(lines) > 0
  lines <- lines[keep]
  result <- list()
  for (line in lines) {
    m <- regmatches(line, regexec("^([a-zA-Z0-9_]+)\\s*=\\s*\"(.*)\"", line))[[1]]
    if (length(m) == 3) {
      result[[m[2]]] <- m[3]
    }
  }
  result
}
