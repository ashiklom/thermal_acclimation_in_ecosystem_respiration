.data <- rlang::.data

# Read every product named in a site's provenance list and splice them into one
# record, oldest first.
#
# The splice rule is the original workflow's
# (01_02a_filter_high_quality_night_respiration_EuroFlux.R:57-67): append each
# later product only from the first timestamp after the running record's end, so
# the earlier (longer-history) product wins wherever they overlap. All the
# products are FLUXNET-format, so their columns already agree.
# Concatenate per-product tables into one record.
#
# Each element of `parts` must be sorted by TIMESTAMP_START. Products are
# ordered by their own first timestamp rather than by the caller's order, so a
# mis-ordered `source` string cannot silently truncate the record; then each
# later product contributes only the rows after the running record's end, so the
# earlier (longer-history) product wins wherever two overlap.
splice_products <- function(parts) {
  if (length(parts) == 0) stop("Nothing to splice.")
  parts <- parts[order(vapply(parts, function(d) d$TIMESTAMP_START[1], ""))]
  combined <- parts[[1]]
  for (i in seq_along(parts)[-1]) {
    nxt <- parts[[i]]
    tail_start <- combined$TIMESTAMP_START[nrow(combined)]
    combined <- dplyr::bind_rows(combined, nxt[nxt$TIMESTAMP_START > tail_start, ])
  }
  combined
}


read_spliced_products <- function(site_info) {
  name_site <- site_info[["site_ID"]]
  wanted <- site_sources(site_info)

  parts <- list()
  for (product in wanted) {
    path <- product_file(name_site, product)
    if (is.na(path)) {
      message("  ", product, ": not downloaded, skipping")
      next
    }
    # `FLUXNET_COL_TYPES` guarantees character timestamps -- which the ordering
    # below and `splice_products()` both depend on -- and doubles everywhere
    # else. A column that violates the contract surfaces in `problems()`
    # instead of quietly changing type.
    dat <- readr::read_csv(path, col_types = FLUXNET_COL_TYPES, progress = FALSE)
    dat[dat == -9999] <- NA
    dat <- dat[order(dat$TIMESTAMP_START), ]
    message(
      "  ", product, ": ", nrow(dat), " rows, ",
      substr(dat$TIMESTAMP_START[1], 1, 4), "-",
      substr(dat$TIMESTAMP_START[nrow(dat)], 1, 4)
    )
    parts[[product]] <- dat
  }

  if (length(parts) == 0) {
    stop(
      name_site, " has none of its declared products on disk (",
      paste(wanted, collapse = ", "), "). Run the matching download first."
    )
  }

  combined <- splice_products(parts)
  if (length(parts) > 1) {
    message(
      "  spliced -> ", nrow(combined), " rows, ",
      substr(combined$TIMESTAMP_START[1], 1, 4), "-",
      substr(combined$TIMESTAMP_START[nrow(combined)], 1, 4)
    )
  }
  combined
}


prep_fluxnet_family <- function(site_info) {
  name_site <- site_info[["site_ID"]]
  a <- read_spliced_products(site_info)

  dt <- lubridate::ymd_hm(a$TIMESTAMP_START[2]) - lubridate::ymd_hm(a$TIMESTAMP_START[1])
  a$TIMESTAMP <- lubridate::ymd_hm(a$TIMESTAMP_START) + dt / 2
  a$YEAR <- lubridate::year(a$TIMESTAMP)
  a$MONTH <- lubridate::month(a$TIMESTAMP)
  a$DAY <- lubridate::day(a$TIMESTAMP)
  a$DOY <- lubridate::yday(a$TIMESTAMP)
  a$HOUR <- lubridate::hour(a$TIMESTAMP)
  a$MINUTE <- lubridate::minute(a$TIMESTAMP)

  if (site_info[["LAT"]] < 0) {
    # TODO: Work out the southern hemisphere logic
    stop("southern hemisphere sites not implemented yet")
    a$DOY[a$DOY < 183] <- a$DOY[a$DOY < 183] + 366
  }

  if (name_site == "CZ-Stn") {
    # use TS of second layer because the first layer is incomplete
    a$TS_F_MDS_1 <- a$TS_F_MDS_2
    a$TS_F_MDS_1_QC <- a$TS_F_MDS_2_QC
  } else if (name_site == "FI-Sod") {
    # TODO: Use proper date filters here.
    mod1 <- lm(data = a[1:24383,], TS_F_MDS_2 ~ TS_F_MDS_1)
    pred2 <- predict(mod1, data.frame(TS_F_MDS_1 = a$TS_F_MDS_1[a$YEAR <= 2005]))
    mod2 <- lm(data = a[90000:245000,], TS_F_MDS_1 ~ TS_F_MDS_2)
    a$TS_F_MDS_1[a$YEAR <= 2005] <- predict(mod2, data.frame(TS_F_MDS_2 = pred2))
    a$TS_F_MDS_1_QC[a$YEAR <= 2005] <- 2
  } else if (name_site == "GF-Guy") {
    # use air temperature for this tropical site so that all tropical sites, we used bottom air temperature.
    a$TS_F_MDS_1 <- a$TA_F_MDS
    a$TS_F_MDS_1_QC <- a$TA_F_MDS_QC
  } else if (name_site %in% c("FR-Fon", "CH-Dav", "DE-Akm", "DE-Hte", "FR-Bil", "FR-Pue", "FR-FBn", "CZ-RAJ")) {
    message("Predicting soil temperature")
    ts_fit <- fix_soil_temp(a, site_info) |>
      dplyr::select("TIMESTAMP", TS_F_MDS_1 = "TS_pred")
    a <- a |>
      dplyr::select(-dplyr::any_of("TS_F_MDS_1")) |>
      dplyr::left_join(ts_fit, by = "TIMESTAMP") |>
      dplyr::mutate(TS_F_MDS_1_QC = dplyr::if_else(
        is.na(.data$TS_F_MDS_1_QC) | .data$TS_F_MDS_1_QC == 3,
        2,
        .data$TS_F_MDS_1_QC
      ))
  }

  if (name_site == "FR-Pue") {
    # FLUXNET data do not include soil data for Site FR-Pue, but site PI provided the SWC data
    stop("site-specific logic not implemented")
  }

  # Return a with some names standardized to match.
  result <- a |>
    dplyr::rename(
      NEE = "NEE_VUT_REF",
      TA = "TA_F_MDS",
      TS = "TS_F_MDS_1",
      TS_QC = "TS_F_MDS_1_QC",
      NEE_QC = "NEE_VUT_REF_QC"
    ) |>
    dplyr::mutate(
      SWC = if ("SWC_F_MDS_1" %in% names(a)) .data$SWC_F_MDS_1 else NA_real_,
      daytime = .data$NIGHT != 1,
      SW_IN = if ("SW_IN_F_MDS" %in% names(a)) .data$SW_IN_F_MDS else NA_real_,
      GPP_DT = if ("GPP_DT_VUT_REF" %in% names(a)) .data$GPP_DT_VUT_REF else NA_real_,
      NEE_uStar_f = .data$NEE
    )

  # Save dt as attribute for later use.
  attr(result, "dt") <- dt
  result

}


prep_nee_ac <- function(name_site) {
  site_info <- get_site_info(name_site)

  if (site_reader(site_info) == "ameriflux") {
    ac <- prep_ameriflux(site_info)
    gs <- attr(ac, "gs")
    stopifnot(!is.null(gs))
    measured <- ac |>
      dplyr::filter(
        !is.na(.data$NEE),
        !is.na(.data$TS),
        !is.na(.data$USTAR),
        .data$USTAR >= .data$uStarTh
      )

    # Only require soil water where it is actually measured. Sites with
    # `SWC_use == "NO"` carry an all-NA SWC column, so an unconditional filter
    # here would discard every observation.
    if (isTRUE(site_info$SWC_use)) {
      measured <- measured |> dplyr::filter(!is.na(.data$SWC))
    }

    keep_night <- if (name_site %in% SITES_LOW_LIGHT_NIGHT) {
      rlang::expr(.data$SW_IN < 10 | !.data$daytime)
    } else {
      rlang::expr(!.data$daytime)
    }
    measured <- measured |>
      dplyr::filter(!!keep_night, .data$NEE > -5, .data$NEE < 30) |>
      tibble::as_tibble()
  } else {
    ac <- prep_fluxnet_family(site_info)
    gs <- detect_growing_season(
      ac, site_info,
      nee_threshold = if (name_site %in% SITES_GS_NEE_ZERO) "zero" else "capped"
    )
    measured <- ac |>
      dplyr::filter(
        !is.na(.data$TA),
        .data$TS_QC %in% c(0, 1, 2),
        # NOTE: Normally, the QC flag check should be enough to catch NA values.
        # But in some cases, NA values still slip through, so we filter
        # explicitly here as well.
        !is.na(.data$TS),
        .data$NEE_QC == 0,
        .data$NIGHT == 1,
        .data$NEE > -5,
        .data$NEE < 30
      ) |>
      tibble::as_tibble()
  }

  dt <- attr(ac, "dt")
  stopifnot(!is.null(dt))

  if (nrow(measured) == 0) {
    stop(name_site, " has no observations after the nighttime quality filter.")
  }

  gStart <- gs$gStart
  gEnd <- gs$gEnd
  tStart <- gs$tStart
  tEnd <- gs$tEnd

  # A few sites have unreliable NEE below 2 C, so both the reported temperature
  # floor and the observations themselves are truncated there. The two original
  # workflows truncated at different points and that is preserved here: for
  # CH-Dav the cold observations are dropped *before* the data-gap scan, so they
  # can change which years qualify, while for US-Ha1 and US-GLE they are dropped
  # after it. Unifying the two would silently change results at three sites.
  truncate_cold <- name_site %in% SITES_TS_MIN_2C
  is_ameriflux <- site_reader(site_info) == "ameriflux"
  if (truncate_cold) {
    tStart <- max(tStart, TS_MIN_VALID)
    if (!is_ameriflux) {
      measured <- measured |> dplyr::filter(.data$TS >= TS_MIN_VALID)
    }
  }

  good_years <- get_good_years(measured, gStart, gEnd, dt, name_site)

  if (truncate_cold && is_ameriflux) {
    measured <- measured |> dplyr::filter(.data$TS >= TS_MIN_VALID)
  }

  measured_final <- measured |>
    dplyr::mutate(growing_year = dplyr::if_else(.data$DOY <= 366, .data$YEAR, .data$YEAR - 1)) |>
    dplyr::filter(.data$growing_year %in% good_years) |>
    dplyr::select(
      "YEAR", "MONTH", "DAY", "DOY", "HOUR", "MINUTE",
      "NEE", "TA", "TS", "SWC"
    )

  stopifnot(
    all(!is.na(measured_final[["TS"]])),
    all(!is.na(measured_final[["NEE"]]))
  )

  iStart <- min(measured_final[["YEAR"]])
  iEnd <- max(measured_final[["YEAR"]])

  ac_required <- c(
    "YEAR", "MONTH", "DAY", "DOY", "HOUR", "MINUTE",
    "NEE", "NEE_uStar_f", "TA", "TS", "SWC", "SW_IN", "daytime"
  )
  ac_optional <- c("NEE_QC", "GPP_DT")

  ac_final <- ac |>
    dplyr::filter(dplyr::between(.data$YEAR, iStart, iEnd)) |>
    dplyr::select(dplyr::all_of(ac_required), dplyr::any_of(ac_optional))

  # ---------------------------------------------------- TS column variants
  #
  # Everything above this point uses measured soil temperature, and that
  # ordering is load-bearing: the QC filter, the data-gap scan, the
  # growing-season detection and the TS >= 2 C truncation all ran on the
  # measured column. Estimated soil temperature is *added alongside* it here
  # rather than replacing it, so the second pipeline step selects a column
  # instead of recomputing one, and so the alternatives can be compared.
  #
  # This is where the estimation belongs rather than in `total_tas_site()`
  # because the regression is fitted on `ac`/`nightNEE` -- step-01 products --
  # and because fitting it in step 02 meant refitting it identically for the
  # total and direct model runs.
  ac_final[["TS_measured"]] <- ac_final[["TS"]]
  measured_final[["TS_measured"]] <- measured_final[["TS"]]

  # Bounds per TS column, not as free-standing scalars. They are the 2.5/97.5
  # percentiles of growing-season soil temperature and they gate the
  # window-skip test downstream, so they only mean anything paired with the
  # column they were derived from. Keying them by column name makes selecting a
  # column and selecting its bounds a single, atomic act.
  ts_bounds_tbl <- tibble::tibble(
    ts_col = "TS_measured",
    tStart = unname(max(tStart, 0.0)),
    tEnd = unname(tEnd)
  )

  if (identical(site_info[["ts_col"]], "TS_linear")) {
    substituted <- apply_ts_linear(ac_final, measured_final, site_info, gStart, gEnd)
    ac_final[["TS_linear"]] <- substituted$ac[["TS"]]
    measured_final[["TS_linear"]] <- substituted$nightNEE[["TS"]]
    ts_bounds_tbl <- dplyr::bind_rows(
      ts_bounds_tbl,
      tibble::tibble(
        ts_col = "TS_linear",
        tStart = substituted$tStart,
        tEnd = substituted$tEnd
      )
    )
  }

  # `TS` must still be the measured column on the way out of step 01. Every
  # filter above ran on it, and the second step selects a variant explicitly;
  # substituting here would make those filters describe data that no longer
  # exists.
  stopifnot(
    identical(ac_final[["TS"]], ac_final[["TS_measured"]]),
    identical(measured_final[["TS"]], measured_final[["TS_measured"]])
  )

  # ------------------------------------------------- SWC column variants
  #
  # Measured soil water and the ERA5-Land reanalysis are carried side by side
  # rather than one replacing the other. The second step used to drop `SWC` and
  # join the reanalysis in its place, which made the two impossible to compare
  # and meant the same join ran again for every model variant. Both columns are
  # in PERCENT; see `ERA5_SWC_TO_PERCENT`.
  ac_final[["SWC_measured"]] <- ac_final[["SWC"]]
  measured_final[["SWC_measured"]] <- measured_final[["SWC"]]

  # Absent reanalysis is not fatal here: the total model never reads soil
  # water, so only the direct model is entitled to complain, and it does --
  # see `resolve_swc_column()`.
  era5 <- tryCatch(
    read_era5_swc(name_site),
    error = function(e) {
      message("  ERA5 soil water unavailable (", conditionMessage(e), ")")
      NULL
    }
  )
  if (is.null(era5)) {
    ac_final[["SWC_era5"]] <- NA_real_
    measured_final[["SWC_era5"]] <- NA_real_
  } else {
    era5 <- dplyr::rename(era5, SWC_era5 = "SWC")
    n_ac <- nrow(ac_final)
    n_night <- nrow(measured_final)
    ac_final <- dplyr::left_join(ac_final, era5, by = c("YEAR", "MONTH", "DAY"))
    measured_final <- dplyr::left_join(measured_final, era5, by = c("YEAR", "MONTH", "DAY"))
    # A daily table joined onto half-hourly rows must annotate, never multiply.
    stopifnot(nrow(ac_final) == n_ac, nrow(measured_final) == n_night)
  }

  declared_ts <- site_info[["ts_col"]]
  if (!declared_ts %in% ts_bounds_tbl[["ts_col"]]) {
    stop(
      name_site, " declares ts_col = ", shQuote(declared_ts),
      ", which this step does not produce. Available: ",
      paste(ts_bounds_tbl[["ts_col"]], collapse = ", "), "."
    )
  }

  feature_gs <- tibble::tibble(
    site_ID = name_site,
    gStart = gStart,
    gEnd = gEnd,
    # `unname()` because these arrive from `quantile()` still carrying its
    # "2.5%"/"97.5%" names, which then differ from the same numbers in
    # `ts_bounds` for no reason anyone would enjoy debugging.
    tStart = unname(max(tStart, 0.0)),
    tEnd = unname(tEnd),
    nyear = length(good_years)
  )

  list(
    ac = ac_final,
    nightNEE = measured_final,
    feature_gs = feature_gs,
    ts_bounds = ts_bounds_tbl
  )

}
