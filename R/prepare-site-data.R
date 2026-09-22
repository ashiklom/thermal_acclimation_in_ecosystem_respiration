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
    dat <- drop_sentinels(dat)
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


# FI-Sod's shallow soil temperature sensor is unreliable before 2006. The
# original re-derived it by chaining two regressions between the two sensor
# depths, and expressed the fitting periods as row ranges into one specific
# data release:
#
#   mod1 <- lm(data = a[1:24383, ],      TS_F_MDS_2 ~ TS_F_MDS_1)
#   mod2 <- lm(data = a[90000:245000, ], TS_F_MDS_1 ~ TS_F_MDS_2)
#
# Row ranges do not survive a re-download. On the FLUXNET-Archive product alone
# FI-Sod spans 2023-2024, so `90000:245000` lay entirely past the end of the
# table and `lm()` aborted with "0 (non-NA) cases" -- the site could not be
# processed at all. On a record of a *different* length it would not error,
# which is worse: it would quietly select different dates.
#
# The windows below are those same row ranges resolved to timestamps against
# the release the manuscript was written on: FLUXNET2015 FULLSET HH for
# FI-Sod, 2001-2014, which is exactly 245,424 contiguous half-hourly rows --
# note that the original's upper bound of 245000 all but exhausts it. Because
# the record is gap-free, row index and timestamp map one-to-one, so this is a
# faithful translation rather than an interpretation, and it reproduces the
# original's coefficients exactly on that file. There is a test for that.
#
# Using year boundaries instead -- fit on everything before 2006, and on
# everything after -- looks tidier and is wrong. Measured on the same file, the
# early relationship changes from slope 0.865 to slope 0.307, and the rebuilt
# pre-2006 soil temperature moves by 8.13 C RMS (mean +6.97 C, max 13.5 C) on
# values spanning -8 to 23.6 C. The early sensor's behaviour evidently does not
# hold steady across 2001-2005, so which part of that period the relationship
# is learned from matters a great deal.
FI_SOD_TS_BAD_THROUGH <- 2005
# a[1:24383, ] and a[90000:245000, ] of FLX_FI-Sod_FLUXNET2015_FULLSET_HH_2001-2014_1-4.csv
FI_SOD_EARLY_WINDOW <- c("200101010000", "200205232300")
FI_SOD_LATE_WINDOW <- c("200602182330", "201412230330")

prep_fluxnet_family <- function(site_info, ts_qc = "manuscript") {
  name_site <- site_info[["site_ID"]]
  a <- read_spliced_products(site_info)

  dt <- lubridate::ymd_hm(a$TIMESTAMP_START[2]) - lubridate::ymd_hm(a$TIMESTAMP_START[1])
  a$TIMESTAMP <- lubridate::ymd_hm(a$TIMESTAMP_START) + dt / 2
  a$YEAR <- lubridate::year(a$TIMESTAMP)
  a$MONTH <- lubridate::month(a$TIMESTAMP)
  a$DAY <- lubridate::day(a$TIMESTAMP)
  a$DOY <- wrap_growing_doy(lubridate::yday(a$TIMESTAMP), growing_year_start(site_info))
  a$HOUR <- lubridate::hour(a$TIMESTAMP)
  a$MINUTE <- lubridate::minute(a$TIMESTAMP)

  # Soil temperature, stage A: the sensor after its per-site repairs, or a
  # reconstruction where the site has none. Which is `ts_source` in
  # site_info.csv; see R/soil-temperature.R. The reader's only job here is to
  # hand over the record's columns under the shared names.
  soil <- qualification_soil_temperature(fluxnet_ts_input(a, site_info), site_info, ts_qc = ts_qc)
  a$TS <- soil[["TS"]]
  a$TS_QC <- soil[["TS_QC"]]

  if (name_site == "FR-Pue") {
    # FLUXNET data do not include soil data for Site FR-Pue, but site PI provided the SWC data
    stop("site-specific logic not implemented")
  }

  # Return a with some names standardized to match.
  result <- a |>
    dplyr::rename(
      NEE = "NEE_VUT_REF",
      TA = "TA_F_MDS",
      NEE_QC = "NEE_VUT_REF_QC"
    ) |>
    dplyr::mutate(
      SWC = if ("SWC_F_MDS_1" %in% names(a)) .data$SWC_F_MDS_1 else NA_real_,
      daytime = .data$NIGHT != 1,
      SW_IN = if ("SW_IN_F_MDS" %in% names(a)) .data$SW_IN_F_MDS else NA_real_,
      GPP_DT = if ("GPP_DT_VUT_REF" %in% names(a)) .data$GPP_DT_VUT_REF else NA_real_,
      NEE_uStar_f = .data$NEE
    )

  list(ac = result, dt = dt, ts_provenance = soil[["provenance"]])
}


# `site_info` is the site's row from site_info.csv, passed in rather than read
# here. The pipeline reads it once per site and threads it through, so every
# stage is guaranteed to see the same declaration: `prep_nee_ac()`,
# `get_good_years()` and `total_tas_site()` between them re-parsed the CSV
# three times per site, and nothing prevented two of those reads from
# straddling an edit to it.
# `recipe` carries the methodology choices. Only the axes in
# `RECIPE_PREP_AXES` matter here -- every other axis is resolved in
# `total_tas_site()`, and this function produces every candidate column and
# both bounds definitions so that it can. Two recipes with the same
# `recipe_prep_key()` therefore share one result of this function, which is
# what keeps a sites x recipes grid affordable.
prep_nee_ac <- function(site_info, recipe = original_recipe()) {
  name_site <- site_info[["site_ID"]]

  # The one prep axis has one strategy. This is where a `computed` year
  # qualification would branch; see docs/recipes.md for what it needs.
  if (!identical(recipe$year_qc, "site_info")) {
    stop("year_qc strategy ", shQuote(recipe$year_qc), " is not implemented in prep_nee_ac().")
  }

  # Both readers return `list(ac =, dt =, ...)`. The AmeriFlux one also returns
  # the growing season it detected, because the u-star filtering it ran already
  # depended on it.
  if (site_reader(site_info) == "ameriflux") {
    prepared <- prep_ameriflux(site_info, ts_qc = recipe$ts_qc)
    ac <- prepared[["ac"]]
    gs <- prepared[["gs"]]
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
    prepared <- prep_fluxnet_family(site_info, ts_qc = recipe$ts_qc)
    ac <- prepared[["ac"]]
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

  dt <- prepared[["dt"]]

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

  good_years <- get_good_years(measured, gStart, gEnd, dt, site_info)

  if (truncate_cold && is_ameriflux) {
    measured <- measured |> dplyr::filter(.data$TS >= TS_MIN_VALID)
  }

  measured_final <- measured |>
    dplyr::mutate(growing_year = growing_year_of(.data$DOY, .data$YEAR)) |>
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
  # `NETRAD` is optional because only some products and some AmeriFlux BASE
  # files carry it. It is carried so that reconstructions driven by radiation
  # can be scored against air-temperature-only ones without re-reading the raw
  # record; nothing in the current model formulae reads it.
  ac_optional <- c("NEE_QC", "GPP_DT", "NETRAD")

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
  # The *native* row for the measured column is the manuscript's own number:
  # percentiles of the day-of-year climatology from `detect_growing_season()`,
  # floored at 0 C and, at the SITES_TS_MIN_2C sites, at 2 C. It is not a pure
  # function of the column, which is why it is written here rather than by
  # `ts_bounds_rows()`. The two non-native rows are the consistent
  # definitions a recipe can select instead (finding F4).
  ts_bounds_tbl <- dplyr::bind_rows(
    tibble::tibble(
      ts_col = "TS_measured",
      definition = "climatology",
      native = TRUE,
      tStart = unname(max(tStart, 0.0)),
      tEnd = unname(tEnd)
    ),
    ts_bounds_rows(ac_final[["TS_measured"]], ac_final[["DOY"]], gStart, gEnd, "TS_measured")
  )

  # `TS_linear` is built at *every* site, not only the 35 that select it.
  #
  # The reason is that the substitution it performs is the largest unmeasured
  # assumption in this analysis -- at 35 sites "soil temperature" is a linear
  # function of air temperature -- and the only way to size the effect is to
  # fit sites that have good measured soil temperature *both* ways and compare.
  # That comparison needs the column to exist at sites that do not use it.
  #
  # Building it changes nothing about what a normal run fits: `ts_col` still
  # comes from site_info.csv, `resolve_ts_column()` still selects by name, and
  # the assertion below still requires `TS` to leave step 01 as the measured
  # column. The extra column is inert until something asks for it by name.
  declared_linear <- identical(site_info[["ts_col"]], "TS_linear")
  substituted <- tryCatch(
    apply_ts_linear(ac_final, measured_final, site_info, gStart, gEnd),
    error = function(e) {
      # A site that *selects* TS_linear cannot proceed without it. A site that
      # only gets it as a diagnostic can: an un-fittable regression (no
      # overlapping TA and TS, say) means the comparison is unavailable there,
      # not that the site is broken.
      if (declared_linear) {
        stop(
          name_site, " declares ts_col = \"TS_linear\" but the TS ~ TA fit ",
          "failed: ", conditionMessage(e)
        )
      }
      message("  TS_linear diagnostic unavailable (", conditionMessage(e), ")")
      NULL
    }
  )
  if (!is.null(substituted)) {
    ac_final[["TS_linear"]] <- substituted$ac[["TS"]]
    measured_final[["TS_linear"]] <- substituted$nightNEE[["TS"]]
    # The regressed column's native definition is the half-hourly one -- the
    # manuscript computed it with `ts_bounds()` -- so that row is native and
    # the climatology row is the alternative.
    linear_rows <- ts_bounds_rows(ac_final[["TS_linear"]], ac_final[["DOY"]], gStart, gEnd, "TS_linear")
    stopifnot(isTRUE(all.equal(
      linear_rows$tStart[linear_rows$definition == "halfhourly"], substituted$tStart
    )))
    linear_rows$native <- linear_rows$definition == "halfhourly"
    ts_bounds_tbl <- dplyr::bind_rows(ts_bounds_tbl, linear_rows)
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
    # The detector's own answer, before any site_info override. Equal to
    # gStart/gEnd at the 93 sites that declare none; the `force_detect` season
    # strategy lays its windows out on these instead.
    gStart_detected = gs$gStart_detected,
    gEnd_detected = gs$gEnd_detected,
    # `unname()` because these arrive from `quantile()` still carrying its
    # "2.5%"/"97.5%" names, which then differ from the same numbers in
    # `ts_bounds` for no reason anyone would enjoy debugging.
    tStart = unname(max(tStart, 0.0)),
    tEnd = unname(tEnd),
    nyear = length(good_years),
    # The DOY origin these bounds are expressed in. `choose_window_season()`
    # needs it to lay out a whole-year span in the same coordinates, and
    # carrying it here keeps the recipe strategies from re-reading site_info.
    growing_year_start = growing_year_start(site_info)
  )

  # Quality of the column a run will treat as measured, as this function
  # leaves it -- after the site-specific column choices above. The verdict is
  # what the `screen_best` and `memory_fill` strategies branch on.
  ts_qc <- ts_quality(ac_final, ts_col = "TS_measured", ta_col = "TA") |>
    dplyr::mutate(site_ID = name_site, .before = 1)

  list(
    ac = ac_final,
    nightNEE = measured_final,
    feature_gs = feature_gs,
    ts_bounds = ts_bounds_tbl,
    ts_qc = ts_qc,
    # What stage A did to make `TS_measured`: one row, from the reader.
    ts_provenance = prepared[["ts_provenance"]]
  )

}
