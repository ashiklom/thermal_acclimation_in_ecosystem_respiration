#!/usr/bin/env Rscript
#
# A soil-temperature quality screen computed from the data alone, and its
# validation against the project's 117 hand-made calls.
#
# WHY IT READS RAW DATA. Every diagnostic here is computed on the raw
# spliced record, before `prep_nee_ac()` runs. That is the whole point: step
# 01 already applies the manual fixes -- CZ-Stn's depth swap, FI-Sod's
# recalibration, GF-Guy's air-temperature substitution, `fix_soil_temp()`'s
# regressions -- so screening its output would be marking its own homework.
# A new site has none of those fixes, and the raw record is all it has.
#
# WHAT IT COMPUTES. Shallow soil temperature is a damped, phase-lagged
# version of the surface forcing. For a sinusoidal forcing of angular
# frequency w, amplitude decays as exp(-z/d) and phase lags by z/d radians,
# where d = sqrt(2*kappa/w) is the damping depth. That gives two *independent*
# estimates of the same dimensionless depth z/d:
#
#     z/d from amplitude = -log(A_TS / A_TA)
#     z/d from lag       = lag_hours * 2*pi/24
#
# A real buried sensor satisfies both and they agree. A sensor lying on the
# surface, pulled out of the ground, or reporting air temperature under
# another name has amplitude ratio near 1 and lag near 0. A dead or
# mislabelled-depth sensor has amplitude ratio near 0 with no matching lag.
# The ratio of the two estimates is therefore a check no single threshold
# gives, and it needs no metadata at all -- which is what makes it work at a
# site the project has never seen.
#
# The thresholds below are first-pass and deliberately crude. Calibrating
# them against the network -- what *is* a normal amplitude ratio for a Dfb
# evergreen needleleaf site? -- is the point of running this over all 117.
#
#   pixi run Rscript scripts/ts-qc-screen.R --workers 8
#   pixi run Rscript scripts/ts-qc-screen.R --sites CZ-Stn,GF-Guy --workers 1

suppressMessages({
  library(dplyr)
  library(targets)
})
tar_source()
source("scripts/ts-rework-common.R")

args <- commandArgs(trailingOnly = TRUE)
workers <- as.integer(parse_opt(args, "--workers", "4"))
outdir <- parse_opt(args, "--out", file.path("data-proc", "ts-rework"))
all_sites <- readr::read_csv(SITE_INFO_CSV, show_col_types = FALSE, progress = FALSE)$site_ID
sites <- parse_sites_arg(args, all_sites)

CACHE <- file.path("data-proc", "ts-qc-cache")

# ------------------------------------------------------------- raw readers

# The raw record with no pipeline fixes applied, plus which columns in it are
# soil temperature and which is air temperature.
raw_record <- function(site_info) {
  name_site <- site_info[["site_ID"]]
  if (site_reader(site_info) == "ameriflux") {
    files <- list.files(
      file.path(DIR_RAWDATA, "Ameriflux"),
      pattern = "^AMF_.*_BASE.*\\.zip$", full.names = TRUE, recursive = TRUE
    )
    fp <- files[grepl(name_site, files)]
    if (length(fp) != 1) stop("expected one BASE archive, found ", length(fp))
    a <- amerifluxr::amf_read_base(fp, parse_timestamp = TRUE, unzip = TRUE) |>
      tibble::as_tibble()
    a[a == -9999] <- NA
    ts_cols <- grep("^TS(_|$)", names(a), value = TRUE)
    ta_cols <- grep("^TA(_|$)", names(a), value = TRUE)
    qc_of <- function(col) NA_character_
  } else {
    a <- read_spliced_products(site_info)
    a$TIMESTAMP <- lubridate::ymd_hm(a$TIMESTAMP_START)
    ts_cols <- grep("^TS_F_MDS_[0-9]+$", names(a), value = TRUE)
    ta_cols <- intersect(c("TA_F_MDS", "TA_F", "TA_ERA"), names(a))
    qc_of <- function(col) {
      q <- paste0(col, "_QC")
      if (q %in% names(a)) q else NA_character_
    }
  }

  a$YEAR <- lubridate::year(a$TIMESTAMP)
  a$DOY <- lubridate::yday(a$TIMESTAMP)
  a$HOUR <- lubridate::hour(a$TIMESTAMP)

  # Air temperature: the declared column when there is one, otherwise the
  # best-covered candidate. The fallback is what a new site would get, and it
  # is exercised whenever `site_info$TA` is absent.
  declared_ta <- site_info[["TA"]]
  ta_col <- if (!is.na(declared_ta) && declared_ta %in% names(a)) {
    declared_ta
  } else if (length(ta_cols)) {
    ta_cols[which.max(vapply(ta_cols, function(c) sum(!is.na(a[[c]])), 0L))]
  } else {
    NA_character_
  }
  if (is.na(ta_col)) stop("no air temperature column found")

  # Soil temperature columns that carry enough data to say anything about.
  ts_cols <- ts_cols[vapply(ts_cols, function(c) sum(!is.na(a[[c]])) > 1000, TRUE)]
  if (!length(ts_cols)) stop("no soil temperature column with >1000 observations")

  list(a = a, ts_cols = ts_cols, ta_col = ta_col, qc_of = qc_of)
}

# ------------------------------------------------------------- diagnostics

# Longest run of exactly-repeated consecutive values, and the fraction of
# observations sitting in a run of at least `min_run`.
#
# The near-zero exemption is not optional. A shallow sensor pinned at 0 C for
# weeks under snow is the zero curtain -- latent heat of fusion buffering the
# soil at the freezing point -- and is the most physically real signal in the
# record, not a stuck logger. Flagging it would condemn every seasonally
# snow-covered site in the network.
stuck_stats <- function(x, min_run = 6, zero_band = 0.5) {
  ok <- !is.na(x)
  if (sum(ok) < 10) return(c(max_run = NA_real_, frac = NA_real_))
  v <- x[ok]
  r <- rle(v)
  near_zero <- abs(r$values) < zero_band
  lens <- r$lengths
  lens[near_zero] <- 1L
  c(
    max_run = max(lens),
    frac = sum(lens[lens >= min_run]) / length(v)
  )
}

site_qc <- function(name_site) {
  message("==== ", name_site, " ====")
  si <- get_site_info(name_site)
  rr <- raw_record(si)
  a <- rr$a
  ta <- a[[rr$ta_col]]
  dt_hours <- as.numeric(median(diff(head(a$TIMESTAMP, 100)), units = "hours"))
  min_obs_day <- max(4, floor(0.8 * 24 / dt_hours))

  per_col <- lapply(rr$ts_cols, function(cl) {
    ts <- a[[cl]]
    d <- tibble::tibble(YEAR = a$YEAR, DOY = a$DOY, HOUR = a$HOUR, .ts = ts, .ta = ta)

    amps <- daily_amplitudes(d, c(".ts", ".ta"), min_obs_day)
    amp_ratio <- unname(amps[[".ts"]] / amps[[".ta"]])

    hourly <- d |>
      dplyr::summarise(t = mean(.data$.ts, na.rm = TRUE), a = mean(.data$.ta, na.rm = TRUE),
                       .by = "HOUR") |>
      dplyr::arrange(.data$HOUR)
    lag_h <- wrap_lag(peak_hour(hourly$t, hourly$HOUR) - peak_hour(hourly$a, hourly$HOUR))

    # The two independent estimates of z/d, and their ratio.
    zd_amp <- if (is.finite(amp_ratio) && amp_ratio > 0) -log(amp_ratio) else NA_real_
    zd_lag <- lag_h * 2 * pi / 24
    consistency <- if (is.finite(zd_amp) && is.finite(zd_lag) && zd_lag > 0.02) {
      zd_amp / zd_lag
    } else {
      NA_real_
    }

    st <- stuck_stats(ts)
    dts <- diff(ts)
    qc_col <- rr$qc_of(cl)

    tibble::tibble(
      site_ID = name_site,
      ts_col = cl,
      n = sum(!is.na(ts)),
      span_years = diff(range(a$YEAR[!is.na(ts)])) + 1,
      coverage = sum(!is.na(ts)) / nrow(a),
      frac_qc0 = if (!is.na(qc_col)) mean(a[[qc_col]][!is.na(ts)] == 0, na.rm = TRUE) else NA_real_,
      mean_ts = mean(ts, na.rm = TRUE),
      mean_offset = mean(ts, na.rm = TRUE) - mean(ta, na.rm = TRUE),
      sd_ts = stats::sd(ts, na.rm = TRUE),
      amp_ts = unname(amps[[".ts"]]),
      amp_ta = unname(amps[[".ta"]]),
      amp_ratio = amp_ratio,
      n_amp_days = unname(amps[["n_days"]]),
      lag_h = lag_h,
      zd_amp = zd_amp,
      zd_lag = zd_lag,
      depth_consistency = consistency,
      stuck_max_run = unname(st[["max_run"]]),
      stuck_frac = unname(st[["frac"]]),
      # A spike faster than soil thermal inertia allows at 5-10 cm.
      spike_rate = mean(abs(dts) > 2 * dt_hours, na.rm = TRUE)
    )
  })

  out <- dplyr::bind_rows(per_col)

  # Cross-depth coherence. Amplitude must fall and lag must rise with depth,
  # so the columns ordered by name should be ordered the same way by
  # amplitude. Where they are not, the depths are mislabelled -- which is the
  # CZ-Stn / DE-Hte / FI-Sod class of problem, currently fixed by hand.
  out$depth_order_ok <- if (nrow(out) > 1) {
    all(diff(out$amp_ratio[order(out$ts_col)]) <= 1e-9)
  } else {
    NA
  }
  out$n_ts_cols <- nrow(out)
  out
}

safe <- function(s) {
  path <- file.path(CACHE, paste0(s, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  v <- tryCatch(site_qc(s), error = function(e) {
    tibble::tibble(site_ID = s, ts_col = NA_character_, status = conditionMessage(e))
  })
  dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)
  saveRDS(v, path)
  v
}

cat("Sites:", length(sites), " workers:", workers, "\n")
t0 <- Sys.time()
res <- if (workers > 1) parallel::mclapply(sites, safe, mc.cores = workers) else lapply(sites, safe)
out <- dplyr::bind_rows(res)
cat("elapsed:", format(Sys.time() - t0), "\n")

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(out, file.path(outdir, "ts-qc-screen.csv"))
cat("Wrote", file.path(outdir, "ts-qc-screen.csv"), "-", nrow(out), "rows\n")
