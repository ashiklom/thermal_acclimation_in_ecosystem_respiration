# Step 01's AmeriFlux BASE reader.

# Site-specific windows as inclusive `TIMESTAMP_START` bounds (NA = open),
# converted from the original's row ranges on the releases then on disk
# (US-Myb BASE-BADM 17-5, US-Jo2 2-5; both gap-free), so each selects exactly
# the rows the range did:
#
#   US-Myb  a[17521:245424, ]   2011-01-01 00:00 .. (open)
#   US-Jo2  a[123453:124049]    2017-01-15 22:00 .. 2017-01-28 08:00
#
# US-Myb's upper row was just the end of the record then (the intent is "drop
# the first year"), so the end is left open.
US_MYB_WINDOW <- c("201101010000", NA)
US_JO2_BAD_TA_WINDOW <- c("201701152200", "201701280800")

# A site's AmeriFlux BASE table, sentinels removed. A tibble, not
# `amf_read_base()`'s data.frame: `a[[NA_character_]]` then errors at the
# lookup instead of silently yielding NULL. REddyProc gives identical results
# on either.
read_ameriflux_base <- function(name_site) {
  path <- product_file(name_site, "AmeriFlux_BASE")
  if (is.na(path)) stop("No AmeriFlux BASE archive for ", name_site, " under ", DIR_RAWDATA, ".")
  amerifluxr::amf_read_base(path, parse_timestamp = TRUE, unzip = TRUE) |>
    tibble::as_tibble() |>
    drop_sentinels()
}

prep_ameriflux <- function(site_info, ts_qc = "manuscript") {
  name_site <- site_info[["site_ID"]]
  message("Reading Ameriflux data...")
  a <- read_ameriflux_base(name_site)

  if (name_site == "US-Myb") {
    # use data from the second year, because lots of missing NEE in the first year.
    a <- a[in_timestamp_window(a$TIMESTAMP_START, US_MYB_WINDOW), ]
    # combine TS data at two depth
    a$TS_2_1_1[is.na(a$TS_2_1_1)] <- a$TS_2_2_1[is.na(a$TS_2_1_1)] * 0.9324892 + 0.9077066
  }

  long_site <- site_info[["LONG"]]
  lat_site <- site_info[["LAT"]]

  sunrise_set <- site_sunlight_times(
    site_info,
    seq.Date(as.Date(min(a$TIMESTAMP)), as.Date(max(a$TIMESTAMP)), by = 1)
  )

  a <- a |>
    dplyr::mutate(DATE = as.Date(.data$TIMESTAMP)) |>
    dplyr::left_join(sunrise_set, by = c("DATE" = "date"))

  a$DOY <- wrap_growing_doy(a$DOY, growing_year_start(site_info))

  # determine: day-time or night-time
  dt <- difftime(a$TIMESTAMP[2], a$TIMESTAMP[1], units = "hours")
  a$daytime <- ((a$TIMESTAMP + dt / 2.0) >= a$sunrise) & (a$TIMESTAMP - dt / 2.0 <= a$sunset)

  # Arctic sites have days with no sunrise or sunset: classify those by month.
  if (name_site %in% SITES_LOW_LIGHT_NIGHT) {
    a$daytime[is.na(a$daytime) & dplyr::between(a$MONTH, 4, 8)] <- TRUE
    a$daytime[is.na(a$daytime) & !dplyr::between(a$MONTH, 4, 8)] <- FALSE
  }

  if (name_site == "US-CMW") {
    # only use data after 2000
    a <- a |> dplyr::filter(.data$YEAR > 2000)
  }
  if (name_site == "CA-Mer") {
    # only use data after 1999, the first two-year data have some problems. 
    a <- a |> dplyr::filter(.data$YEAR > 1999)
  }
  if (name_site == "US-Jo2") {
    # TA data in this window has problems; use Tsonic data
    bad_ta <- in_timestamp_window(a$TIMESTAMP_START, US_JO2_BAD_TA_WINDOW)
    a$TA[bad_ta] <- a$T_SONIC[bad_ta] * 1.0183316 - 0.9299
  }

  if (name_site == "US-BZS") {
    # TA data in 2012-2014 is not accurate, so use nearby US-BZF's TA data.
    # Pair the two sites on the timestamp, not on position.
    bzf_ta <- read_ameriflux_base("US-BZF") |>
      dplyr::select("TIMESTAMP", BZF = "TA_PI_F")
    calib <- a |>
      dplyr::select("TIMESTAMP", "YEAR", BZS = "TA_PI_F") |>
      dplyr::inner_join(bzf_ta, by = "TIMESTAMP") |>
      dplyr::filter(dplyr::between(.data$YEAR, 2016, 2019))
    mod <- lm(BZS ~ BZF, data = calib)
    # `predict.lm()` keeps NA rows, so the result lines up with `gap`.
    gap <- dplyr::between(a$YEAR, 2012, 2014)
    a$TA_PI_F[gap] <- predict(
      mod,
      newdata = data.frame(
        BZF = bzf_ta$BZF[match(a$TIMESTAMP[gap], bzf_ta$TIMESTAMP)]
      )
    )
  }

  if (name_site == "US-Ha2") {
    # Combine the two towers: the first where it has data, else the second.
    for (v in c("FC", "TA", "PPFD_IN", "RH", "USTAR")) {
      a[[paste0(v, "_A_1_1")]] <- dplyr::coalesce(a[[paste0(v, "_1_1_1")]], a[[paste0(v, "_2_1_1")]])
    }
  }

  # Prepare data frame (ac) for u-star filtering
  ustar <- prep_ustar_df(a, site_info, ts_qc = ts_qc)
  ac <- ustar[["ac"]]

  # ---------------------------------------------------- u-star filtering
  gs <- detect_growing_season(
    ac, site_info,
    nee_col = "NEE", ts_col = "TS",
    nee_threshold = "uncapped"
  )
  gStart <- gs$gStart
  gEnd <- gs$gEnd

  years <- unique(ac[["YEAR"]])
  # REddyProc wants real days of year, so unwrap the season bounds.
  seasonStarts <- lapply(
    years,
    \(y) tibble::tibble(DOY = unwrap_growing_doy(c(gStart, gEnd)), year = y)
  ) |>
    dplyr::bind_rows()

  ac_u <- ac |>
    dplyr::mutate(
      DateTime = lubridate::ymd_hm(.data$TIMESTAMP_END),
      NEE = .data$NEE,
      Rg = .data$SW_IN,
      Tair = .data$TA,
      Ustar = .data$USTAR,
      .keep = "none"
    )

  if (ustar[["convert_rh"]]) {
    ac_u$rH    <- ac$RH
    ac_u$VPD <- REddyProc::fCalcVPDfromRHandTair(ac_u$rH, ac_u$Tair)
  } else {
    ac_u$VPD <- ac$VPD
  }

  DTS <- 24 / as.numeric(dt, units = "hours")
  EProc <- REddyProc::sEddyProc$new(name_site, ac_u, c("NEE", "Rg", "Tair", "VPD", "Ustar"), DTS = DTS)
  EProc$sSetLocationInfo(LatDeg = lat_site, LongDeg = long_site, TimeZoneHour = site_utc_offset(site_info))

  if (name_site == "US-ChR") {
    # use default season, and yearly threshold
    uStarTh <- EProc$sEstUstarThold()
    # `many-to-one`: the positional `ac$uStarTh <- ac_u$uStar` below needs
    # the join to leave the row count alone (one threshold per year).
    ac_u <- ac_u |>
      dplyr::mutate(seasonYear = lubridate::year(.data$DateTime)) |>
      dplyr::left_join(
        uStarTh[uStarTh$aggregationMode == "year", c("seasonYear", "uStar")],
        by = "seasonYear", relationship = "many-to-one"
      )
  } else {
    ac_u$season <- REddyProc::usCreateSeasonFactorYdayYear(
      ac_u$DateTime - 15*60,  # it sets back 15 min.
      starts = seasonStarts
    )
    uStarTh <- EProc$sEstUstarThold(seasonFactor = ac_u$season)
    ac_u <- ac_u |>
      dplyr::left_join(uStarTh[, c("season", "uStar")], by = "season",
                       relationship = "many-to-one")
  }
  # Positional: `ac_u` descends from `ac` row for row and both joins are
  # guarded many-to-one.
  ac$uStarTh <- ac_u$uStar

  # Gap-fill NEE with the annually aggregated u-star threshold (the default):
  # some sites have no seasonal estimates.
  EProc$sMDSGapFillAfterUstar("NEE", FillAll = FALSE, isVerbose = FALSE)
  ac$NEE_uStar_f <- EProc$sExportResults()$NEE_uStar_f

  # `gs` is returned, not recomputed downstream, because the u-star season
  # factor was built from it.
  list(ac = ac, dt = dt, gs = gs, ts_provenance = ustar[["ts_provenance"]])
}

# Every per-site column this function is about to read, for the path this site
# takes. `TS` is read wherever it is declared -- at a reconstructed site it is
# the estimator's training target -- `SWC` only where the site keeps soil
# water, and exactly one of `RH`/`VPD` is consulted.
#
# `netrad_column` is deliberately absent: `prep_ustar_df()` carries net
# radiation forward only if it happens to be there, while `fix_soil_temp()`
# raises its own error when a site that needs it does not have it.
declared_ameriflux_columns <- function(site_info) {
  fields <- c("NEE", "FC", "TA", "SW_IN", "USTAR", "TS",
              if (isTRUE(site_info$SWC_use)) "SWC",
              if (!is.na(site_info$RH)) "RH" else "VPD")
  declared <- unlist(site_info[fields])
  declared <- declared[!is.na(declared)]
  # `NEE` is a `+`-separated sum at 26 sites; every term has to be present.
  unique(trimws(unlist(strsplit(declared, "+", fixed = TRUE))))
}

# AmeriFlux BASE column names encode sensor position and processing level,
# and change between releases (US-Ho1/US-Ho2 lost `RH_PI_F_2_1_1`). Check the
# declared columns up front, naming the site, field and column and what the
# record offers instead -- otherwise the failure surfaces deep in REddyProc.
check_declared_columns <- function(a, site_info) {
  name_site <- site_info[["site_ID"]]
  wanted <- declared_ameriflux_columns(site_info)
  absent <- setdiff(wanted, names(a))
  if (!length(absent)) return(invisible(NULL))

  # Same measurement, different position or processing level: the shortlist
  # someone re-declaring the column would want to choose from.
  near <- function(col) {
    stem <- sub("_.*$", "", col)
    hits <- grep(paste0("^", stem, "(_|$)"), names(a), value = TRUE)
    if (length(hits)) paste(hits, collapse = ", ") else "none"
  }
  stop(
    name_site, " declares ", length(absent), " column(s) that its AmeriFlux ",
    "BASE record does not contain:\n",
    paste0("  ", absent, "  --  the record has: ", vapply(absent, near, ""),
           collapse = "\n"),
    "\nRe-declare them in data-core/site_info.csv and in ",
    "scripts/revise-site-info.R, which rebuilds it."
  )
}

prep_ustar_df <- function(a, site_info, ts_qc = "manuscript") {
  name_site <- site_info[["site_ID"]]
  check_declared_columns(a, site_info)

  ac <- a |>
    dplyr::select("YEAR", "MONTH", "DAY", "DOY", "HOUR", "MINUTE", "TIMESTAMP", "TIMESTAMP_END")

  # `NEE` may be a `+`-separated sum of columns.
  NEEvars <- trimws(unlist(strsplit(site_info$NEE, "\\+")))
  ac$NEE <- Reduce(`+`, lapply(NEEvars, function(v) a[[v]]))

  # remove the NEE data without FC measurements
  if (!is.na(site_info[["FC"]])) {
    # NB: not `na_if()`, which compares values -- here we are masking by
    # position, using a logical index drawn from a different column.
    ac$NEE[is.na(a[[site_info[["FC"]]]])] <- NA_real_
  }

  # combine NEE time series and FC time series; because either one is incomplete
  if (name_site == "CA-Man") {
    ac$NEE[is.na(ac$NEE)] <- a$FC[is.na(ac$NEE)] + a$SC[is.na(ac$NEE)]
  }

  # air temperature
  if (!is.na(site_info$TA)) {
    ac$TA <- a[[site_info$TA]]
  }

  # Net radiation where the file has it, for the radiation-driven fill
  # candidates: the declared column, else a bare `NETRAD`.
  netrad_column <- site_info[["netrad_column"]]
  if (is.na(netrad_column) && "NETRAD" %in% names(a)) {
    netrad_column <- "NETRAD"
  }
  if (!is.na(netrad_column) && netrad_column %in% names(a)) {
    ac$NETRAD <- a[[netrad_column]]
  }

  # Soil temperature, stage A (R/soil-temperature.R): this reader only hands
  # over the record's columns under the shared names.
  soil <- qualification_soil_temperature(ameriflux_ts_input(a, site_info), site_info, ts_qc = ts_qc)
  stopifnot(length(soil[["TS"]]) == nrow(ac))
  ac$TS <- soil[["TS"]]

  # Soil water, keyed on the flag rather than on `site_info$SWC`: 22 of the 33
  # AmeriFlux `SWC_use = NO` sites name a column that is deliberately unused.
  if (isTRUE(site_info$SWC_use)) {
    ac$SWC <- a[[site_info$SWC]]
    # deal with special cases
    if (name_site == "US-NR1") {
      # use PI gap-filled data
      ac$SWC[is.na(ac$SWC)] <- a$SWC_PI_1[is.na(ac$SWC)]
    }
    # interpolate SWC data if necessary because TS sensor has different frequency with other data
    x <- ac$SWC
    rle_x <- rle(is.na(x))
    min_gap <- min(rle_x$lengths[rle_x$values == TRUE])
    message(paste0("minimum soil gaps: ", min_gap))
    if (min_gap <= 8) {
      # Perform interpolation on eligible gaps only
      ac$SWC <- zoo::na.approx(x, na.rm = FALSE, maxgap = 8)
    }
  } else {
    ac$SWC <- NA_real_
  }

  # SW_IN
  ac$SW_IN <- a[[site_info$SW_IN]]
  if (substr(site_info$SW_IN, 1, 4) == "PPFD") {
    # convert PPFD to SW_IN
    ac$SW_IN  <- ac$SW_IN / 2.3
  }

  # US-Jo2 has no SW_IN before 2013, which REddyProc cannot run without: fill
  # it from the same half-hour four years (365 * 48 * 4 rows) later.
  if (name_site == "US-Jo2") {
    ac$SW_IN[is.na(ac$SW_IN)] <- ac$SW_IN[which(is.na(ac$SW_IN)) + 365 * 48 * 4]
  } else if (name_site == "US-KM4") {
    # use PI gap-filled data
    ac$SW_IN[is.na(ac$SW_IN)] <- a$SW_IN_PI_F[is.na(ac$SW_IN)]
  }

  # USTAR
  ac$USTAR <- a[[site_info$USTAR]]

  # RH or VPD. Whether VPD must be derived from RH is part of the result, not
  # an attribute on the table, which a dplyr verb could drop.
  convert_rh <- !is.na(site_info$RH)
  if (convert_rh) {
    ac$RH <- a[[site_info$RH]]
  } else {
    ac$VPD <- a[[site_info$VPD]]
  }

  ac$daytime <- a$daytime

  list(ac = ac, convert_rh = convert_rh, ts_provenance = soil[["provenance"]])
}


# Sunrise and sunset for `dates`, in the frame AmeriFlux timestamps use.
#
# BASE timestamps are local standard time, parsed by `amf_read_base()` as
# GMT: a local clock reading with a UTC label. `getSunlightTimes()`'s `tz`
# also decides which solar day's events come back -- in UTC, sunrise and
# sunset fall on different local days here -- so ask in the site's standard
# time and relabel (not convert) to UTC, as the original did. `Etc/GMT` zones
# are fixed-offset with an inverted sign, and exist only at whole hours
# (every AmeriFlux site is UTC-4 to UTC-9), hence the check.
site_sunlight_times <- function(site_info, dates) {
  offset <- site_utc_offset(site_info)
  if (offset != round(offset)) {
    stop(
      "Site ", site_info[["site_ID"]], " has a fractional UTC offset (",
      offset, " h), which cannot be expressed as an Etc/GMT zone. ",
      "Day/night classification needs a fixed-offset zone for this site."
    )
  }
  tz_site <- sprintf("Etc/GMT%+d", -as.integer(offset))

  sunrise_set <- suncalc::getSunlightTimes(
    date = dates,
    keep = c("sunrise", "sunset"),
    lat = site_info[["LAT"]],
    lon = site_info[["LONG"]],
    tz = tz_site
  )
  sunrise_set$sunrise <- lubridate::force_tz(sunrise_set$sunrise, "UTC")
  sunrise_set$sunset <- lubridate::force_tz(sunrise_set$sunset, "UTC")
  sunrise_set
}

# The site's standard-time UTC offset in hours (no daylight saving).
site_utc_offset <- function(site_info) {
  zone <- lutz::tz_lookup_coords(lat = site_info[["LAT"]], lon = site_info[["LONG"]], method = "accurate")
  lutz::tz_offset(as.Date("2000-01-01"), zone)$utc_offset_h
}
