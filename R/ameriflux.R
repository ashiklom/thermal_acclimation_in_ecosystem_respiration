# Custom pre-processing for Ameriflux_BASE data

prep_ameriflux <- function(site_info, ts_qc = "manuscript") {
  name_site <- site_info[["site_ID"]]
  files_AmeriFlux_BASE <- list.files(
    file.path(DIR_RAWDATA, "Ameriflux"), pattern = "^AMF_.*_BASE.*\\.zip$", full.names = TRUE, recursive = TRUE
  )
  file_path <- files_AmeriFlux_BASE[grepl(name_site, files_AmeriFlux_BASE)]
  stopifnot(length(file_path) == 1)
  message("Reading Ameriflux data...")
  # `amf_read_base()` returns a base data.frame, and everything below reaches
  # into it with column names taken from `site_info` (`a[[site_info$SW_IN]]`,
  # `FC`, `TA`, `TS`, `SWC`, `USTAR`, ...). On a data.frame a missing or NA name
  # yields NULL silently: the column is simply never created, and the complaint
  # surfaces much later in `prep_nee_ac()` as an absent `SWC` pointing nowhere
  # near the cause. A tibble raises "Can't extract column with
  # `NA_character_`" at the point of use instead, which makes every one of
  # those lookups self-checking rather than relying on the per-field invariant
  # in `scripts/revise-site-info.R`. Verified safe for the pipeline: REddyProc
  # accepts a tibble and returns identical u-star thresholds and gap-fills, and
  # `SW_IN`/`USTAR` -- the two lookups that are not guarded by `!is.na()` -- are
  # populated for all 71 AmeriFlux sites.
  a <- amerifluxr::amf_read_base(
    file_path,
    parse_timestamp = TRUE,
    unzip = TRUE
  ) |>
    tibble::as_tibble()
  a <- drop_sentinels(a)

  if (name_site == "US-Myb") {
    # use data from the second year, because lots of missing NEE in the first year.
    # TODO: Use proper filter here, not magic numbers
    a <- a[17521:245424, ]
    # combine TS data at two depth
    a$TS_2_1_1[is.na(a$TS_2_1_1)] <- a$TS_2_2_1[is.na(a$TS_2_1_1)] * 0.9324892 + 0.9077066
  }

  long_site <- site_info[["LONG"]]
  lat_site <- site_info[["LAT"]]

  tz <- lutz::tz_offset(
    as.Date("2000-01-01"),
    lutz::tz_lookup_coords(lat = lat_site, lon = long_site, method = "accurate")
  )

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

  # special cases: sites in Arctic do not have sunrise and sunset sometime of a year
  # TODO: Lat-based filter instead?
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
    # TA data from 123453~124049 has problems use Tsonic data 
    # TODO: Use timestamps here instead.
    a$TA[123453:124049] <- a$T_SONIC[123453:124049] * 1.0183316 - 0.9299
  }   

  if (name_site == "US-BZS") {
    # TA data in 2012-2014 is not accurate, so use nearby US-BZF's TA data.
    bzf_file <- files_AmeriFlux_BASE[grepl("US-BZF", files_AmeriFlux_BASE)]
    stopifnot(length(bzf_file) == 1)
    a_BZF <- amerifluxr::amf_read_base(bzf_file, parse_timestamp = TRUE, unzip = TRUE) |>
      tibble::as_tibble()
    a_BZF <- drop_sentinels(a_BZF)
    # Pair the two sites on the timestamp, not on position. The regression this
    # replaces built its data frame from two independently subset vectors, so it
    # depended on the 2016-2019 slices of the two BASE releases coming out the
    # same length and in the same order -- a property of the files as they
    # happen to be published, not one either file guarantees. A single extra
    # half-hour at either site would have silently paired every row afterwards
    # with the wrong timestamp.
    bzf_ta <- a_BZF |>
      dplyr::select("TIMESTAMP", BZF = "TA_PI_F")
    calib <- a |>
      dplyr::select("TIMESTAMP", "YEAR", BZS = "TA_PI_F") |>
      dplyr::inner_join(bzf_ta, by = "TIMESTAMP") |>
      dplyr::filter(dplyr::between(.data$YEAR, 2016, 2019))
    mod <- lm(BZS ~ BZF, data = calib)
    # Same pairing for the prediction. `predict.lm()` keeps NA rows rather than
    # dropping them, so the result lines up with `gap` row for row.
    gap <- dplyr::between(a$YEAR, 2012, 2014)
    a$TA_PI_F[gap] <- predict(
      mod,
      newdata = data.frame(
        BZF = bzf_ta$BZF[match(a$TIMESTAMP[gap], bzf_ta$TIMESTAMP)]
      )
    )
  }

  if (name_site == 'US-Ha2') {
    # combine data from two locations
    a$FC_A_1_1 <- a$FC_1_1_1
    a$FC_A_1_1[is.na(a$FC_A_1_1)] <- a$FC_2_1_1[is.na(a$FC_A_1_1)]
    a$TA_A_1_1 <- a$TA_1_1_1
    a$TA_A_1_1[is.na(a$TA_A_1_1)] <- a$TA_2_1_1[is.na(a$TA_A_1_1)]
    a$PPFD_IN_A_1_1 <- a$PPFD_IN_1_1_1
    a$PPFD_IN_A_1_1[is.na(a$PPFD_IN_A_1_1)] <- a$PPFD_IN_2_1_1[is.na(a$PPFD_IN_A_1_1)]
    a$RH_A_1_1 <- a$RH_1_1_1
    a$RH_A_1_1[is.na(a$RH_A_1_1)] <- a$RH_2_1_1[is.na(a$RH_A_1_1)]
    a$USTAR_A_1_1 <- a$USTAR_1_1_1
    a$USTAR_A_1_1[is.na(a$USTAR_A_1_1)] <- a$USTAR_2_1_1[is.na(a$USTAR_A_1_1)]
  }

  # Prepare data frame (ac) for u-star filtering
  ustar <- prep_ustar_df(a, site_info, ts_qc = ts_qc)
  ac <- ustar[["ac"]]

  #l###############################################################################
  # Begin U-star filtering
  ################################################################################
  gs <- detect_growing_season(
    ac, site_info,
    nee_col = "NEE", ts_col = "TS",
    nee_threshold = "uncapped"
  )
  gStart <- gs$gStart
  gEnd <- gs$gEnd
  tStart <- gs$tStart
  tEnd <- gs$tEnd

  years <- unique(ac[["YEAR"]])
  # REddyProc wants real days of year, so a wrapped bound has to come back
  # under 366 before it is handed over. No AmeriFlux site declares a wrapped
  # growing year today; this is here so that one could.
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
  EProc$sSetLocationInfo(LatDeg = lat_site, LongDeg = long_site, TimeZoneHour = tz$utc_offset_h)

  if (name_site == "US-ChR") {
    # use default season, and yearly threshold
    uStarTh <- EProc$sEstUstarThold()

    EProc$sMDSGapFillAfterUstar("NEE", FillAll = FALSE, isVerbose = FALSE)
    ac$NEE_uStar_f <- EProc$sExportResults()$NEE_uStar_f
    #
    ac_u <- ac_u |>
      dplyr::mutate(seasonYear = lubridate::year(.data$DateTime)) |>
      dplyr::left_join(uStarTh[uStarTh$aggregationMode == "year", c(2, 4)], by = "seasonYear")
  } else {
    ac_u$season <- REddyProc::usCreateSeasonFactorYdayYear(
      ac_u$DateTime - 15*60,  # it sets back 15 min.
      starts = seasonStarts
    )
    uStarTh <- EProc$sEstUstarThold(seasonFactor = ac_u$season)

    # gap fill NEE, air temperature and soil temperature
    # By default the gap-filling uses annually aggregated estimates of uStar-Threshold.
    # we can also use a different threshold for each of the defined seasons, by calling the two functions
    # EProc$useSeaonsalUStarThresholds()
    # EProc$sGetUstarScenarios()
    # I only want annually aggregated estimated, because some sites have no seasonal estimates
    EProc$sMDSGapFillAfterUstar("NEE", FillAll = FALSE, isVerbose = FALSE)
    ac$NEE_uStar_f <- EProc$sExportResults()$NEE_uStar_f
    #
    # TODO: Use column names, not column indices
    ac_u <- ac_u |>
      dplyr::left_join(uStarTh[, c("season", "uStar")], by = "season")
  }
  ac$uStarTh <- ac_u$uStar

  # `dt` and `gs` are results of this function, not properties of the table, so
  # they are returned as such. `gs` in particular has to be handed back rather
  # than recomputed downstream: it is the growing season the u-star season
  # factor was built from, and a second call that disagreed with it would put
  # the seasonal thresholds and the growing-season bounds out of step.
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

# AmeriFlux BASE column names carry the sensor's position and processing
# level, and they change between releases: US-Ho1 and US-Ho2 declared
# `RH_PI_F_2_1_1`, which release 15-5 and 10-5 no longer publish. Read through
# `[[` on a tibble, an absent name yields NULL and the column is simply never
# created, so the complaint used to surface inside REddyProc as "Missing
# specified columns in dataset: RelHumidity_Percent" -- nowhere near the site
# declaration that caused it, and with nothing to act on.
#
# Checked in one place, up front, naming the site, the field, the column and
# what the record does offer instead. The declarations themselves live in
# site_info.csv and `scripts/revise-site-info.R`.
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

  NEEvars <- trimws(unlist(strsplit(site_info$NEE, "\\+")))
  for (iNEEvar in seq_along(NEEvars)) {
    if (iNEEvar == 1) {
      ac$NEE <- a[[NEEvars[iNEEvar]]]
    } else {
      ac$NEE <- ac$NEE + a[[NEEvars[iNEEvar]]]
    }
  }

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

  # Net radiation, where the BASE file has it. Stage A reads it from the raw
  # record for the `rf:TA+NETRAD` reconstruction; carrying it forward on `ac`
  # is what lets the radiation-driven reconstructions be cross-validated
  # against the air-temperature-only ones on equal terms downstream.
  #
  # The declared column wins; a bare `NETRAD` is the fallback, because most
  # BASE files that have net radiation call it that and only the five sites
  # that needed a disambiguated replicate say so in site_info.csv.
  netrad_column <- site_info[["netrad_column"]]
  if (is.na(netrad_column) && "NETRAD" %in% names(a)) {
    netrad_column <- "NETRAD"
  }
  if (!is.na(netrad_column) && netrad_column %in% names(a)) {
    ac$NETRAD <- a[[netrad_column]]
  }

  # Soil temperature, stage A: the sensor after its per-site repairs, or a
  # reconstruction where the site has none. Which is `ts_source` in
  # site_info.csv; see R/soil-temperature.R. The reader's only job here is to
  # hand over the record's columns under the shared names. The result is
  # `TS`, i.e. `TS_measured` downstream, which is what the original treated it
  # as at every one of these sites.
  soil <- qualification_soil_temperature(ameriflux_ts_input(a, site_info), site_info, ts_qc = ts_qc)
  stopifnot(length(soil[["TS"]]) == nrow(ac))
  ac$TS <- soil[["TS"]]

  # Soil water
  # `SWC_use` is recoded to a logical by `get_site_info()`, and is never NA, so
  # this has to test the value rather than its presence. Sites flagged "NO"
  # deliberately discard soil water even where a column is available (22 of the
  # 33 AmeriFlux "NO" sites do name one), which is why this keys on the flag and
  # not on whether `site_info$SWC` is populated.
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

  # Special cases: US-Jo2: interpolating SW_IN, otherwise ReddyProc does not
  # work for years before 2013 because of no SW_IN data.
  if (name_site == "US-Jo2") {
    # TODO: More robust implementation! This just grabs the following year's data.
    ac$SW_IN[is.na(ac$SW_IN)] <- ac$SW_IN[which(is.na(ac$SW_IN)) + 365 * 48 * 4]
  } else if (name_site == "US-KM4") {
    # use PI gap-filled data
    ac$SW_IN[is.na(ac$SW_IN)] <- a$SW_IN_PI_F[is.na(ac$SW_IN)]
  }

  # USTAR
  ac$USTAR <- a[[site_info$USTAR]]

  # RH or VPD. Whether VPD has to be derived from relative humidity is decided
  # by the site's column mapping here and needed by the caller, so it is part of
  # this function's result rather than an attribute riding on the table: any
  # dplyr verb that dropped attributes would have turned it into a missing-VPD
  # error inside REddyProc, several steps away from the cause.
  convert_rh <- !is.na(site_info$RH)
  if (convert_rh) {
    ac$RH <- a[[site_info$RH]]
  } else {
    ac$VPD <- a[[site_info$VPD]]
  }

  ac$daytime <- a$daytime

  list(ac = ac, convert_rh = convert_rh, ts_provenance = soil[["provenance"]])
}


# Sunrise and sunset for `dates`, expressed in the frame that AmeriFlux
# timestamps use.
#
# AmeriFlux BASE timestamps are local standard time (no daylight saving), and
# `amf_read_base` parses them with `tz = "GMT"`. So `a$TIMESTAMP` is a local
# clock reading wearing a UTC label, and sunrise/sunset have to be put in that
# same frame before they can be compared against it.
#
# The timezone passed to `getSunlightTimes()` is not merely a display choice: it
# also decides which solar day's events get returned. Asking for UTC yields a
# sunrise and a sunset belonging to *different* local days at these longitudes,
# which makes the daytime test unsatisfiable for most of the year. So request the
# times in the site's local standard time, then relabel (not convert) them to
# UTC. This reproduces the original workflow, which did the relabel via an
# `as.character()` round-trip.
#
# `Etc/GMT` zones are fixed-offset (no DST, which is what we want here) and use
# the inverted POSIX sign convention, hence the negation. They only exist at
# whole-hour offsets; every AmeriFlux site is currently UTC-4 to UTC-9, and we
# would rather fail loudly than silently mis-classify if that ever changes.
site_sunlight_times <- function(site_info, dates) {
  tz <- lutz::tz_offset(
    as.Date("2000-01-01"),
    lutz::tz_lookup_coords(
      lat = site_info[["LAT"]], lon = site_info[["LONG"]], method = "accurate"
    )
  )
  if (tz$utc_offset_h != round(tz$utc_offset_h)) {
    stop(
      "Site ", site_info[["site_ID"]], " has a fractional UTC offset (",
      tz$utc_offset_h, " h), which cannot be expressed as an Etc/GMT zone. ",
      "Day/night classification needs a fixed-offset zone for this site."
    )
  }
  tz_site <- sprintf("Etc/GMT%+d", -as.integer(tz$utc_offset_h))

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
