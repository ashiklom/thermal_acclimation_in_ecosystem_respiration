# Fixtures and oracles shared across test files.
#
# These only define functions, which matters: testthat sources `helper-*.R`
# before `setup.R`, so nothing here may touch the filesystem or call pipeline
# code at load time.

# Point the working directory at the project root for the rest of the calling
# file. The pipeline addresses every input relative to the root
# (`data-core/site_info.csv`, `data-raw/...`), but testthat sources each test
# file with `chdir = TRUE`, i.e. from `tests/testthat/`. Call this once at the
# top of any test file that reaches real inputs; it unwinds when the file ends.
use_project_root <- function(envir = parent.frame()) {
  withr::local_dir(normalizePath(file.path("..", "..")), .local_envir = envir)
}

# Whether every product a site declares is on disk, for `skip_if()`.
site_raw_available <- function(name_site) {
  products <- site_sources(get_site_info(name_site))
  all(!vapply(products, function(p) is.na(product_file(name_site, p)), logical(1)))
}

# Step 01 for a real site, computed once per test session.
#
# `prep_nee_ac()` is cheap at a FLUXNET-family site -- a few seconds -- and not
# at an AmeriFlux one, where REddyProc's u-star estimation and gap fill
# dominate and US-Kon takes over a minute. The real-data tests want both
# readers, and re-running step 01 inside each `test_that()` would put minutes
# on the suite, so the result is memoised for the session. Read-only by
# convention: a test that needs to alter the tables copies first.
.prepped_cache <- new.env(parent = emptyenv())
prepped_site <- function(name_site) {
  hit <- .prepped_cache[[name_site]]
  if (!is.null(hit)) return(hit)
  out <- suppressWarnings(suppressMessages(prep_nee_ac(get_site_info(name_site))))
  .prepped_cache[[name_site]] <- out
  out
}

# One site per reader, for the tests that run the real thing end to end.
# DE-RuC is the cheapest FLUXNET-family site; US-Kon is the AmeriFlux
# reference -- its step-01 features reproduce the manuscript's
# growing_season_feature_AmeriFlux.csv row exactly, so a difference there is a
# real one. Nothing below is site-specific: these are properties of the path.
READER_SITES <- c(fluxnet_family = "DE-RuC", ameriflux = "US-Kon")

# The column names in a site's AmeriFlux BASE archive, read from the header
# alone. `amf_read_base()` would parse the whole table -- tens of seconds and
# hundreds of MB per site -- and the declaration check only needs the names.
#
# An archive can hold more than one BASE table (BR-Sa1 ships an older hourly
# one alongside its half-hourly), so the half-hourly file wins, as it does in
# the reader.
ameriflux_base_header <- function(name_site) {
  zip <- product_file(name_site, "AmeriFlux_BASE")
  inside <- utils::unzip(zip, list = TRUE)$Name
  csv <- grep("_BASE_HH_.*[.]csv$", inside, value = TRUE)
  if (!length(csv)) csv <- grep("_BASE_.*[.]csv$", inside, value = TRUE)
  # Read the first few lines in one call. An `unz()` connection that is not
  # explicitly opened is opened and closed again by every read, so reading it
  # a line at a time returns line 1 forever.
  con <- unz(zip, csv[[1]])
  on.exit(close(con), add = TRUE)
  head_lines <- readLines(con, n = 5L, warn = FALSE)
  header <- head_lines[!startsWith(head_lines, "#")]
  if (!length(header)) {
    stop(name_site, ": no header line in the first 5 lines of ", csv[[1]], ".")
  }
  trimws(strsplit(header[[1]], ",", fixed = TRUE)[[1]])
}

# A seasonal NEE/TS curve with every day-of-year present.
#
# Completeness is load-bearing. `detect_growing_season()` floors gStart with the
# first DOY whose mean TS >= 0, and the original indexed that by row position
# rather than by DOY value. Those agree only when no DOY is missing, so a gappy
# fixture would confound this comparison with a separate difference that was
# deliberately left unfixed (see the DOY-index finding in claude-suggestions).
synthetic_season <- function(nee_min_target = -10, years = 4) {
  doy <- 1:365
  nee <- 3 + (nee_min_target - 3) * exp(-((doy - 190)^2) / (2 * 55^2))
  ts <- 12 - 14 * cos(2 * pi * (doy - 15) / 365)
  data.frame(DOY = rep(doy, years), NEE = rep(nee, years), TS = rep(ts, years))
}

# Growing-season detection as the original scripts computed it, transcribed.
# This is the oracle for `detect_growing_season()`; `variant` picks which of the
# three mutually incompatible cut-offs the original applied where.
original_detect_growing_season <- function(ac, variant) {
  nee_yearly <- ac |>
    dplyr::group_by(.data$DOY) |>
    dplyr::summarise(
      NEE = mean(.data$NEE, na.rm = TRUE),
      TS = mean(.data$TS, na.rm = TRUE),
      .groups = "drop"
    )
  tmp <- switch(variant,
    # 01_02b_filter_high_quality_night_respiration_AmeriFlux.R:270
    uncapped = nee_yearly |>
      dplyr::filter(.data$NEE < min(nee_yearly$NEE, na.rm = TRUE) * 0.2),
    # 01_02a_filter_high_quality_night_respiration_EuroFlux.R:184
    capped = nee_yearly |>
      dplyr::filter(.data$NEE < max(min(nee_yearly$NEE, na.rm = TRUE) * 0.2, -0.8)),
    # 01_02a_filter_high_quality_night_respiration_EuroFlux.R:182 (FI-Sod, DE-RuC)
    zero = nee_yearly |> dplyr::filter(.data$NEE < 0.0)
  )
  gStart <- as.integer(mean(tmp$DOY[7])) - 4
  gEnd <- as.integer(mean(tmp$DOY[(nrow(tmp) - 6)])) + 4
  gStart <- max(gStart, min(which(nee_yearly$TS >= 0)))
  list(
    gStart = gStart,
    gEnd = gEnd,
    tStart = unname(quantile(tmp$TS, 0.025, na.rm = TRUE)),
    tEnd = unname(quantile(tmp$TS, 0.975, na.rm = TRUE))
  )
}

# Sunrise/sunset as the original AmeriFlux script computed them
# (01_02b_filter_high_quality_night_respiration_AmeriFlux.R:57-78): request the
# times in the site's fixed-offset local zone, then relabel them as GMT via an
# `as.character()` round-trip, with a correction for days whose sunset lands on
# another date.
#
# One deviation: the original built zone names like "GMT+6", which current
# lubridate rejects outright ("CCTZ: Unrecognized output timezone"). "Etc/GMT+6"
# is the same fixed offset under a name that still resolves.
original_sunlight_times <- function(site_info, dates) {
  tz <- lutz::tz_offset(
    as.Date("2000-01-01"),
    lutz::tz_lookup_coords(
      lat = site_info[["LAT"]], lon = site_info[["LONG"]], method = "accurate"
    )
  )
  tz_site <- sprintf("Etc/GMT%+d", -as.integer(tz$utc_offset_h))
  sun <- suncalc::getSunlightTimes(
    date = dates, keep = c("sunrise", "sunset"),
    lat = site_info[["LAT"]], lon = site_info[["LONG"]], tz = tz_site
  )
  i <- which(!is.na(sun$sunrise))[1]
  corr <- difftime(sun$date[i], lubridate::as_date(sun$sunset[i]))
  data.frame(
    date = sun$date,
    sunrise = as.POSIXct(as.character(sun$sunrise), tz = "GMT") + corr,
    sunset = as.POSIXct(as.character(sun$sunset), tz = "GMT") + corr
  )
}

# A minimal AmeriFlux BASE frame: the timestamp columns `amf_read_base()` adds,
# plus every flux column the site names in `site_info`. Enough to drive
# `prep_ustar_df()` without touching REddyProc.
synthetic_ameriflux <- function(site_info, n = 96) {
  ts <- as.POSIXct("2015-06-01 00:15:00", tz = "GMT") + (seq_len(n) - 1) * 1800
  a <- data.frame(
    YEAR = lubridate::year(ts), MONTH = lubridate::month(ts), DAY = lubridate::day(ts),
    DOY = lubridate::yday(ts), HOUR = lubridate::hour(ts), MINUTE = lubridate::minute(ts),
    TIMESTAMP = ts, TIMESTAMP_START = format(ts - 900, "%Y%m%d%H%M"),
    TIMESTAMP_END = format(ts + 900, "%Y%m%d%H%M"),
    daytime = rep(c(FALSE, TRUE), each = n / 2)
  )
  named <- unlist(site_info[c("NEE", "FC", "TA", "TS", "SWC", "SW_IN", "USTAR",
                              "RH", "VPD", "netrad_column")])
  for (spec in stats::na.omit(named)) {
    for (nm in trimws(unlist(strsplit(spec, "\\+")))) a[[nm]] <- seq_len(n) / 10
  }
  a
}

# A plausible soil/air temperature pair for the `estimate_Ts` reconstruction,
# written over whatever `synthetic_ameriflux()` put in the site's declared
# columns. A ramp in every column would make `TS ~ TA` an exact identity and
# `TA + NETRAD` rank-deficient, so a reconstruction of it proves nothing: this
# gives soil temperature a damped, lagged version of air temperature, which is
# the relationship the estimator is supposed to find.
with_synthetic_soil_signal <- function(a, site_info) {
  t <- seq_len(nrow(a))
  diurnal <- sin(2 * pi * t / 48)
  drift <- t / nrow(a)
  a[[site_info$TA]] <- 12 + 8 * diurnal + 6 * drift + stats::rnorm(length(t), 0, 0.5)
  if (!is.na(site_info$netrad_column)) {
    a[[site_info$netrad_column]] <- 120 * pmax(diurnal, 0) - 30
  }
  # Damped and lagged by three hours, as a buried sensor would be.
  lagged <- c(rep(diurnal[[1]], 6), utils::head(diurnal, -6))
  a[[site_info$TS]] <- 11 + 2.5 * lagged + 5 * drift + stats::rnorm(length(t), 0, 0.2)
  a
}

# Half-hourly timestamps over a whole year, in the GMT-labelled local frame that
# `amf_read_base()` produces.
synthetic_year_timestamps <- function(year = 2015) {
  seq(
    as.POSIXct(sprintf("%d-01-01 00:15:00", year), tz = "GMT"),
    as.POSIXct(sprintf("%d-12-31 23:45:00", year), tz = "GMT"),
    by = 1800
  )
}
