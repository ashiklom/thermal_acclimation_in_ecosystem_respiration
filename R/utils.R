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
