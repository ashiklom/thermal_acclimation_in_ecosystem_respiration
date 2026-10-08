#' The rlang `.data` pronoun
.data <- rlang::.data

#' Splice per-product tables into one record
#'
#' Concatenate per-product tables (each sorted by TIMESTAMP_START) into one
#' record, by the original workflow's rule (01_02a...EuroFlux.R:57-67): each
#' later product contributes only rows after the running record's end, so the
#' earlier, longer-history product wins wherever two overlap. Products are
#' ordered by their own first timestamp, not the caller's order, so a
#' mis-ordered `source` string cannot truncate the record.
#'
#' @param parts List of per-product half-hourly flux tables, each with a
#'   character `TIMESTAMP_START`.
#' @return One data frame: the spliced record.
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

#' Read every product in a site's provenance list and splice them
#'
#' @param site_info One row of the site declaration table.
#' @return Data frame of the site's spliced FLUXNET-format half-hourly record,
#'   sentinels removed. Errors if none of its products is on disk.
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
    # Character timestamps, doubles elsewhere; see `FLUXNET_COL_TYPES`.
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


#' Read and prepare one FLUXNET-format site
#'
#' @param site_info One row of the site declaration table.
#' @param ts_qc Soil-temperature qualification strategy, `"manuscript"` or
#'   `"sensor"` (see `RECIPE_AXES`).
#' @return A list: `ac`, the half-hourly table with timestamp columns, stage A's
#'   `TS` and `TS_QC`, and standardized `NEE`, `TA`, `NEE_QC`, `SWC`, `SW_IN`,
#'   `GPP_DT`, `NEE_uStar_f` and `daytime`; `dt`, the time step (difftime); and
#'   `ts_provenance`, stage A's provenance row.
prep_fluxnet_family <- function(site_info, ts_qc = "manuscript") {
  name_site <- site_info[["site_ID"]]
  a <- read_spliced_products(site_info)

  dt <- lubridate::ymd_hm(a$TIMESTAMP_START[2]) - lubridate::ymd_hm(a$TIMESTAMP_START[1])
  a <- add_timestamp_columns(a, dt)
  a$DOY <- wrap_growing_doy(a$DOY, growing_year_start(site_info))

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


#' Step 01 for one site
#'
#' This produces every candidate column and both bounds definitions, so recipes
#' with the same `recipe_prep_key()` share one result.
#'
#' @param site_info The site's row of site_info.csv, read once per site by the
#'   pipeline so every stage sees the same declaration.
#' @param recipe A recipe. Only the recipe's `RECIPE_PREP_AXES` matter here, and
#'   `recipe` may be just those (which is what the pipeline passes).
#' @param era5 A path, a site's table from `read_era5_swc()`, or NULL to leave
#'   soil water off -- the pipeline passes NULL and attaches it with
#'   `attach_era5_swc()`, so extending the ERA5 file does not re-run this.
#' @return A list: `ac`, the half-hourly table for the qualifying years, with
#'   `TS_measured`, `TS_linear` where it could be fitted, and `SWC_measured`;
#'   `nightNEE`, the quality-filtered nighttime observations in the good growing
#'   years; `feature_gs`, a one-row tibble of the growing season and
#'   temperature bounds; `ts_bounds`, the temperature bounds per TS column and
#'   definition; `ts_qc`, the soil temperature verdict; and `ts_provenance`,
#'   stage A's provenance row. With `era5`, `ac` and `nightNEE` also carry
#'   `SWC_era5`.
prep_nee_ac <- function(site_info, recipe = original_recipe(), era5 = ERA5_SWC_CSV) {
  name_site <- site_info[["site_ID"]]

  # Where a `computed` year qualification would branch; see docs/recipes.md.
  if (!identical(recipe$year_qc, "site_info")) {
    stop("year_qc strategy ", shQuote(recipe$year_qc), " is not implemented in prep_nee_ac().")
  }

  # Both readers return `list(ac =, dt =, ts_provenance =)`; the AmeriFlux one
  # also returns the growing season its u-star filtering was built on.
  is_ameriflux <- site_reader(site_info) == "ameriflux"
  if (is_ameriflux) {
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

    # Only where soil water is used: elsewhere the column is all NA.
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
        # QC flag 0-2 does not guarantee a value; check for NA explicitly.
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

  # Sites with unreliable NEE below 2 C: truncate the temperature floor and
  # the observations. The original workflows truncated at different points,
  # preserved here -- before the gap scan at CH-Dav (so it can change which
  # years qualify), after it at US-Ha1 and US-GLE.
  truncate_cold <- name_site %in% SITES_TS_MIN_2C
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
  # `NETRAD` where the record has it, for the radiation-driven fill candidates.
  ac_optional <- c("NEE_QC", "GPP_DT", "NETRAD")

  ac_final <- ac |>
    dplyr::filter(dplyr::between(.data$YEAR, iStart, iEnd)) |>
    dplyr::select(dplyr::all_of(ac_required), dplyr::any_of(ac_optional))

  # ---------------------------------------------------- TS column variants
  #
  # Everything above ran on stage A's column, and that ordering is
  # load-bearing (docs/soil-temperature.md). Estimates are added alongside it,
  # so step 02 selects a column instead of recomputing one. It is still `TS`
  # here because `apply_ts_linear()` reads it by that name; it leaves this
  # section as `TS_measured`.

  # Bounds are keyed by TS column, so a column and its bounds are selected
  # together. The measured column's *native* row is the manuscript's number
  # (the DOY-climatology percentiles from `detect_growing_season()`, floored
  # at 0 C, or 2 C at SITES_TS_MIN_2C); it is not a pure function of the
  # column, so it is written here rather than by `ts_bounds_rows()`.
  ts_bounds_tbl <- dplyr::bind_rows(
    tibble::tibble(
      ts_col = "TS_measured",
      definition = "climatology",
      native = TRUE,
      tStart = unname(max(tStart, 0.0)),
      tEnd = unname(tEnd)
    ),
    ts_bounds_rows(ac_final[["TS"]], ac_final[["DOY"]], gStart, gEnd, "TS_measured")
  )

  # `TS_linear` is built at every site, not only the 35 that select it, so
  # the substitution can be measured where there is a real sensor to compare
  # against. It is inert unless selected.
  declared_linear <- identical(site_info[["ts_col"]], "TS_linear")
  substituted <- tryCatch(
    apply_ts_linear(ac_final, measured_final, site_info, gStart, gEnd),
    error = function(e) {
      # Fatal only where the site selects TS_linear.
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
    # The regressed column's native bounds are the half-hourly ones.
    linear_rows <- ts_bounds_rows(ac_final[["TS_linear"]], ac_final[["DOY"]], gStart, gEnd, "TS_linear")
    stopifnot(isTRUE(all.equal(
      linear_rows$tStart[linear_rows$definition == "halfhourly"], substituted$tStart
    )))
    linear_rows$native <- linear_rows$definition == "halfhourly"
    ts_bounds_tbl <- dplyr::bind_rows(ts_bounds_tbl, linear_rows)
  }

  # Renamed rather than copied: a bare `TS` would be a second name for the
  # same column, and nothing downstream reads it.
  ac_final <- dplyr::rename(ac_final, TS_measured = "TS")
  measured_final <- dplyr::rename(measured_final, TS_measured = "TS")

  # ------------------------------------------------- SWC column variants
  #
  # `SWC_measured` here; `SWC_era5` beside it, from `attach_era5_swc()`.
  ac_final[["SWC_measured"]] <- ac_final[["SWC"]]
  measured_final[["SWC_measured"]] <- measured_final[["SWC"]]

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
    # Before any site_info override; the `force_detect` strategy reads these.
    gStart_detected = gs$gStart_detected,
    gEnd_detected = gs$gEnd_detected,
    # `unname()`: `quantile()` names them "2.5%"/"97.5%".
    tStart = unname(max(tStart, 0.0)),
    tEnd = unname(tEnd),
    nyear = length(good_years),
    # The DOY origin of these bounds, for `choose_window_season()`.
    growing_year_start = growing_year_start(site_info),
    # The record's time step, as the reader measured it.
    dt_minutes = as.numeric(dt, units = "mins")
  )

  # The verdict the `screen_best`/`memory_fill` strategies branch on.
  ts_qc <- ts_quality(ac_final, dt_hours = as.numeric(dt, units = "hours")) |>
    dplyr::mutate(site_ID = name_site, .before = 1)

  out <- list(
    ac = ac_final,
    nightNEE = measured_final,
    feature_gs = feature_gs,
    ts_bounds = ts_bounds_tbl,
    ts_qc = ts_qc,
    # What stage A did to make `TS_measured`: one row, from the reader.
    ts_provenance = prepared[["ts_provenance"]]
  )

  if (is.null(era5)) return(out)
  if (is.character(era5)) era5 <- site_era5_swc(out, name_site, path = era5)
  attach_era5_swc(out, era5)
}
