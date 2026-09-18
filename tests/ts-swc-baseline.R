#!/usr/bin/env Rscript
#
# Frozen oracle for the TS/SWC column refactor.
#
# The manuscript shipped `growing_season_feature_*.csv`, which records step-01
# features only. Nothing external records what soil temperature or soil water
# look like *after* the step-02 manipulation in `total_tas_site()`, so before
# moving that manipulation anywhere there has to be a reference for what it
# currently produces.
#
# `original_*_step02()` below are verbatim transcriptions of
# `R/total_tas.R@b077bb9` lines 183-220, deliberately not calling any function
# in `R/`. That is the point: as the real code is refactored, this file does not
# move with it, so it keeps testing the same thing. The site lists and the ERA5
# read are transcribed for the same reason.
#
#   pixi run ts-baseline                    # refresh/verify all default sites
#   Rscript tests/ts-swc-baseline.R --sites US-Kon,US-Tw1
#   Rscript tests/ts-swc-baseline.R --write  # (re)write the committed baseline
#
# Without --write it compares against tests/fixtures/ts-swc-baseline.csv and
# exits non-zero on any difference beyond TOLERANCE.

suppressMessages({
  library(dplyr)
  library(targets)
})
tar_source()

TOLERANCE <- 1e-8

# ---------------------------------------------------------------- the oracle

# Transcribed from R/total_tas.R@b077bb9:17-20. Hard-coded here on purpose:
# this list is moving into site_info.csv, and the oracle must not move with it.
ORIGINAL_SITE_TS_ISSUE <- c(
  "BE-Bra", "CA-Cbo", "CA-Gro", "CA-Mer", "CA-Obs", "CA-TP3", "CH-Lae", "DE-RuC",
  "DE-SfS", "FI-Sod", "IT-Ren", "NL-Loo", "US-Bar", "US-BZB", "US-BZF", "US-BZS",
  "US-CMW", "US-GLE", "US-Ha2", "US-IB2", "US-Jo2", "US-KL2", "US-Kon", "US-LL1",
  "US-MBP", "US-Myb", "US-NC4", "US-Tw1", "US-ICt", "BE-Dor", "CA-TP4", "UK-AMo",
  "RU-Fyo", "ZA-Kru", "IT-Tor"
)

# Transcribed from R/total_tas.R@b077bb9:31-58 (read_era5_swc + the mutate in
# its caller), including the *100 rescaling to percent.
original_era5_swc <- function(name_site, path = file.path("data-raw", "ERA5_daily_swc.csv")) {
  swc <- readr::read_csv(
    path,
    col_types = readr::cols(time = "D", site = "c", SWC = "d"),
    progress = FALSE
  ) |>
    dplyr::filter(.data$site == name_site)
  stopifnot(nrow(swc) > 0)
  swc |>
    dplyr::mutate(
      date = as.Date(.data$time),
      YEAR = lubridate::year(.data$date),
      MONTH = lubridate::month(.data$date),
      DAY = lubridate::day(.data$date),
      SWC = .data$SWC * 100
    ) |>
    dplyr::select("YEAR", "MONTH", "DAY", "SWC")
}

# Transcribed from R/total_tas.R@b077bb9:183-199.
original_swc_step02 <- function(ac, nightNEE, name_site, SWC_use, direct) {
  if (SWC_use) {
    nightNEE <- nightNEE |> dplyr::filter(!is.na(.data$SWC))
  } else if (direct) {
    nightNEE[["SWC"]] <- NULL
    ac[["SWC"]] <- NULL
    swc_ERA5 <- original_era5_swc(name_site)
    nightNEE <- nightNEE |> dplyr::left_join(swc_ERA5, by = c("YEAR", "MONTH", "DAY"))
    ac <- ac |> dplyr::left_join(swc_ERA5, by = c("YEAR", "MONTH", "DAY"))
    SWC_use <- TRUE
  }
  list(ac = ac, nightNEE = nightNEE, SWC_use = SWC_use)
}

# Transcribed from R/total_tas.R@b077bb9:201-220. Note the two behaviours that
# are easy to lose in a rewrite: the fit domain differs for US-Tw1, and the
# assignment is an *overlay* -- measured TS survives wherever TA is missing,
# because `predict()` returns NA there and only non-NA predictions are written.
original_ts_step02 <- function(ac, nightNEE, name_site, gStart, gEnd, tStart, tEnd) {
  if (name_site %in% ORIGINAL_SITE_TS_ISSUE) {
    if (name_site %in% c("US-Tw1")) {
      mod_lm <- lm(data = nightNEE, TS ~ TA, na.action = na.omit)
    } else {
      mod_lm <- lm(data = ac[ac$TA > 0, ], TS ~ TA, na.action = na.omit)
    }
    TS_pred <- predict(mod_lm, newdata = data.frame(TA = nightNEE$TA), na.action = na.pass)
    nightNEE$TS[!is.na(TS_pred)] <- TS_pred[!is.na(TS_pred)]
    TS_pred <- predict(mod_lm, newdata = data.frame(TA = ac$TA), na.action = na.pass)
    ac$TS[!is.na(TS_pred)] <- TS_pred[!is.na(TS_pred)]

    ts_growing_season <- ac$TS[dplyr::between(ac$DOY, gStart, gEnd)]
    tStart <- quantile(ts_growing_season, 0.025, na.rm = TRUE)
    tEnd <- quantile(ts_growing_season, 0.975, na.rm = TRUE)
  }
  list(ac = ac, nightNEE = nightNEE, tStart = tStart, tEnd = tEnd)
}

# ------------------------------------------------------------------ sites

# One site per code path that the TS/SWC refactor touches.
DEFAULT_SITES <- c(
  "US-Kon", # step-02 rewrite, `ac` fit domain; SWC_use NO -> ERA5 when direct
  "US-Tw1", # step-02 rewrite, `night` fit domain (the one site that differs)
  "US-MBP", # step-02 rewrite on top of a step-01 TA gap-fill (collision)
  "US-GLE", # step-02 rewrite on top of the TS >= 2 C truncation (collision)
  "US-BZo", # step-01 wholesale TS from TA, recent years only; no step-02 rewrite
  "CA-ARB", # step-01 wholesale TS from TA, cold-site domain
  "US-Wkg", # no TS rewrite; SWC_use YES, so the measured SWC filter applies
  "FI-Sod", # step-02 rewrite on top of the step-01 layer-2 chain (collision)
  "NL-Loo", # step-02 rewrite, fluxnet_family, SWC_use YES
  "DE-Tha", # no TS rewrite, fluxnet_family, SWC_use YES
  "FR-Fon", # step-01 predicted soil temperature (random forest path)
  "CH-Dav"  # step-01 predicted soil temperature; SWC_use NO -> ERA5 when direct
)

args <- commandArgs(trailingOnly = TRUE)
write_mode <- "--write" %in% args
sites <- if (length(args) >= 2 && "--sites" %in% args) {
  trimws(unlist(strsplit(args[[which(args == "--sites") + 1]], ",")))
} else {
  DEFAULT_SITES
}

# ------------------------------------------------------------- summarising

# Distribution digest rather than whole tables: enough to detect any change in
# values or row membership, small enough to commit and read in a diff.
digest_col <- function(x) {
  if (is.null(x)) return(c(n = NA_real_, mean = NA_real_, sd = NA_real_, q025 = NA_real_, q975 = NA_real_))
  ok <- x[!is.na(x)]
  if (length(ok) == 0) return(c(n = 0, mean = NA_real_, sd = NA_real_, q025 = NA_real_, q975 = NA_real_))
  c(
    n = length(ok),
    mean = mean(ok),
    sd = stats::sd(ok),
    q025 = unname(quantile(ok, 0.025)),
    q975 = unname(quantile(ok, 0.975))
  )
}

digest_table <- function(dat, prefix, cols) {
  out <- list()
  out[[paste0(prefix, "_rows")]] <- nrow(dat)
  for (cl in cols) {
    d <- digest_col(if (cl %in% names(dat)) dat[[cl]] else NULL)
    for (nm in names(d)) out[[paste0(prefix, "_", cl, "_", nm)]] <- unname(d[[nm]])
  }
  out
}

# Step 01 is slow (REddyProc dominates: 30-140s a site), and this script is
# run repeatedly while the TS/SWC columns move. Its output is cached, keyed by
# a digest of everything under R/ so that any change to the pipeline
# invalidates the cache automatically rather than silently verifying stale
# results -- which would defeat the entire point of the file.
CACHE_DIR <- file.path("data-proc", "ts-baseline-cache")
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
r_digest <- function() {
  files <- sort(list.files("R", pattern = "[.]R$", full.names = TRUE))
  paste(vapply(files, function(f) as.character(tools::md5sum(f)), ""), collapse = "")
}
CODE_KEY <- r_digest()

cached_step01 <- function(name_site) {
  path <- file.path(CACHE_DIR, paste0(name_site, ".rds"))
  if (file.exists(path)) {
    hit <- readRDS(path)
    if (identical(hit$code_key, CODE_KEY)) return(hit$value)
  }
  value <- tryCatch(
    suppressWarnings(suppressMessages(prep_nee_ac(name_site))),
    error = function(e) structure(conditionMessage(e), class = "baseline_error")
  )
  saveRDS(list(code_key = CODE_KEY, value = value), path)
  value
}

# Does the pipeline's *selection* path reproduce the frozen oracle's
# *substitution*? This is the check that the refactor is equivalent, and unlike
# the digests above it is computed fresh on both sides in the same run.
#
# The comparison deliberately runs the oracle on the unmodified step-01 tables,
# bypassing the SWC block. Both the oracle's substitution and the pipeline's
# selection are row-wise, so they commute with the SWC block's row filter; the
# frozen digests cover the combined path.
selection_matches_oracle <- function(sd_, site_info, name_site) {
  ts_col <- site_info[["ts_col"]]
  fg <- sd_$feature_gs
  ora <- original_ts_step02(
    sd_$ac, sd_$nightNEE, name_site, fg$gStart, fg$gEnd, fg$tStart, fg$tEnd
  )
  sel_ac <- resolve_ts_column(sd_$ac, ts_col)
  sel_night <- resolve_ts_column(sd_$nightNEE, ts_col)
  rng <- ts_bounds_for(sd_$ts_bounds, ts_col)

  same <- function(a, b) isTRUE(all.equal(a, b, tolerance = TOLERANCE))
  checks <- c(
    ac_TS = same(sel_ac$TS, ora$ac$TS),
    night_TS = same(sel_night$TS, ora$nightNEE$TS),
    tStart = same(rng$tStart, unname(ora$tStart)),
    tEnd = same(rng$tEnd, unname(ora$tEnd)),
    # Step 01 must hand over measured TS untouched, whatever variants it also
    # produced; every filter it applied was computed on that column.
    ts_is_measured = same(sd_$ac$TS, sd_$ac$TS_measured),
    # And the measured bounds must still equal the ones feature_gs reports, so
    # the default path is unchanged for the 82 sites that take it.
    measured_bounds = same(
      ts_bounds_for(sd_$ts_bounds, "TS_measured"),
      list(tStart = fg$tStart, tEnd = fg$tEnd)
    )
  )
  list(
    ok = all(checks),
    failed = paste(names(checks)[!checks], collapse = ","),
    ts_col = ts_col
  )
}

rows <- list()
selection <- list()
for (name_site in sites) {
  cat(sprintf("\n==== %s ====\n", name_site))
  t0 <- Sys.time()
  sd_ <- cached_step01(name_site)
  if (inherits(sd_, "baseline_error")) {
    cat("  ERROR:", as.character(sd_), "\n")
    rows[[name_site]] <- tibble::tibble(
      site_ID = name_site, direct = NA, status = "error",
      note = substr(as.character(sd_), 1, 120)
    )
    next
  }
  si <- get_site_info(name_site)

  sel <- tryCatch(
    selection_matches_oracle(sd_, si, name_site),
    error = function(e) list(ok = FALSE, failed = paste("error:", conditionMessage(e)), ts_col = si$ts_col)
  )
  selection[[name_site]] <- tibble::tibble(
    site_ID = name_site, ts_col = sel$ts_col, ok = sel$ok, failed = sel$failed
  )
  cat(sprintf(
    "  selection vs oracle (%s): %s%s\n", sel$ts_col,
    if (sel$ok) "match" else "MISMATCH", if (nzchar(sel$failed)) paste0(" [", sel$failed, "]") else ""
  ))

  # Step-01 digest: catches any change to what step 01 produces, independently
  # of the step-02 manipulation applied on top.
  step01 <- c(
    digest_table(sd_$ac, "s1ac", c("TS", "TA", "SWC", "NEE", "NEE_uStar_f")),
    digest_table(sd_$nightNEE, "s1night", c("TS", "TA", "SWC", "NEE"))
  )

  for (direct in c(FALSE, TRUE)) {
    swc <- original_swc_step02(sd_$ac, sd_$nightNEE, name_site, si$SWC_use, direct)
    ts <- original_ts_step02(
      swc$ac, swc$nightNEE, name_site,
      sd_$feature_gs$gStart, sd_$feature_gs$gEnd,
      sd_$feature_gs$tStart, sd_$feature_gs$tEnd
    )
    rows[[paste(name_site, direct)]] <- tibble::tibble(
      site_ID = name_site,
      direct = direct,
      status = "ok",
      note = NA_character_,
      gStart = sd_$feature_gs$gStart,
      gEnd = sd_$feature_gs$gEnd,
      nyear = sd_$feature_gs$nyear,
      swc_use_out = swc$SWC_use,
      tStart_out = unname(ts$tStart),
      tEnd_out = unname(ts$tEnd),
      !!!step01,
      !!!digest_table(ts$ac, "acx", c("TS", "SWC")),
      !!!digest_table(ts$nightNEE, "nightx", c("TS", "SWC"))
    )
  }
  cat(sprintf(
    "  [%.0fs] ac %d rows, night %d rows; tStart/tEnd (total) %.4f / %.4f\n",
    as.numeric(difftime(Sys.time(), t0, units = "secs")),
    nrow(sd_$ac), nrow(sd_$nightNEE),
    rows[[paste(name_site, FALSE)]]$tStart_out,
    rows[[paste(name_site, FALSE)]]$tEnd_out
  ))
}

report <- dplyr::bind_rows(rows) |> dplyr::arrange(.data$site_ID, .data$direct)
outfile <- file.path("tests", "fixtures", "ts-swc-baseline.csv")

sel_report <- dplyr::bind_rows(selection)
selection_failed <- FALSE
if (nrow(sel_report)) {
  cat("\n============ SELECTION vs ORACLE ============\n")
  print(as.data.frame(sel_report), row.names = FALSE)
  selection_failed <- any(!sel_report$ok)
  cat(sprintf(
    "\n  %d of %d sites reproduce the oracle by column selection\n",
    sum(sel_report$ok), nrow(sel_report)
  ))
}

if (write_mode || !file.exists(outfile)) {
  readr::write_csv(report, outfile)
  cat("\n  WROTE baseline:", outfile, "(", nrow(report), "rows )\n")
  quit(status = if (selection_failed) 1 else 0)
}

# ------------------------------------------------------------- comparison

ref <- readr::read_csv(outfile, show_col_types = FALSE)
key <- c("site_ID", "direct")
common_sites <- intersect(report$site_ID, ref$site_ID)
cmp <- dplyr::inner_join(
  report |> dplyr::filter(.data$site_ID %in% common_sites),
  ref |> dplyr::filter(.data$site_ID %in% common_sites),
  by = key, suffix = c("_new", "_ref")
)

num_cols <- setdiff(names(report)[vapply(report, is.numeric, logical(1))], key)
diffs <- list()
for (cl in num_cols) {
  a <- cmp[[paste0(cl, "_new")]]
  b <- cmp[[paste0(cl, "_ref")]]
  if (is.null(a) || is.null(b)) next
  bad <- which(!((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) <= TOLERANCE * pmax(1, abs(b)))))
  for (i in bad) {
    diffs[[length(diffs) + 1]] <- tibble::tibble(
      site_ID = cmp$site_ID[i], direct = cmp$direct[i], column = cl,
      ref = b[i], new = a[i]
    )
  }
}

cat("\n================ BASELINE COMPARISON ================\n")
cat(sprintf("  %d site-runs compared across %d numeric columns\n", nrow(cmp), length(num_cols)))
if (length(diffs) == 0) {
  cat("  IDENTICAL to the committed baseline.\n")
  missing <- setdiff(ref$site_ID, report$site_ID)
  if (length(missing)) {
    cat("  (not run this time: ", paste(unique(missing), collapse = ", "), ")\n", sep = "")
  }
  quit(status = if (selection_failed) 1 else 0)
}
print(as.data.frame(dplyr::bind_rows(diffs)), row.names = FALSE)
cat(sprintf("\n  %d DIFFERENCES from baseline\n", length(diffs)))
quit(status = 1)
