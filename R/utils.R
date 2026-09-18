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
    netrad_column = "c",
    ts_col = "c",
    ts_linear_domain = "c"
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

# Which sites the pipeline builds targets for.
#
# `"dev"` (the default) is the small representative sample in `DEV_SITES`;
# `"all"` is every site the FLUXNET-family reader handles. The full list is
# matched by substring because `source` is a `+`-separated provenance list
# (e.g. "WW2020+FLUXNET+ICOS"); see `FLUX_PRODUCTS` in R/constants.R.
#
# This is called while the pipeline is being *constructed*, because `tar_map()`
# needs the site names in order to generate target names. So it cannot itself
# be a target, and reads site_info.csv directly.
pipeline_sites <- function(scope = Sys.getenv("THERMAL_SITES", "dev"),
                           site_info = get_site_info()) {
  handled <- site_info |>
    dplyr::filter(
      !grepl("AmeriFlux_BASE", .data$source, fixed = TRUE),
      .data$LAT > 0
    ) |>
    dplyr::pull("site_ID")

  if (identical(scope, "all")) return(handled)
  if (!identical(scope, "dev")) {
    stop("THERMAL_SITES must be \"dev\" or \"all\", not ", shQuote(scope), ".")
  }

  # A typo in DEV_SITES would otherwise produce a pipeline whose targets each
  # fail separately at download time, and under `error = "continue"` that looks
  # much like a data problem.
  unknown <- setdiff(DEV_SITES, handled)
  if (length(unknown)) {
    stop(
      "DEV_SITES names ", length(unknown), " site(s) this pipeline does not ",
      "handle: ", paste(shQuote(unknown), collapse = ", "),
      ". They have to be northern-hemisphere and not AmeriFlux BASE."
    )
  }
  DEV_SITES
}
