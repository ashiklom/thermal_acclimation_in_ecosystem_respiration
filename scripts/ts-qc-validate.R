#!/usr/bin/env Rscript
#
# Does a screen computed from the data alone reproduce the project's 117
# hand-made soil-temperature calls?
#
# Reads `ts-qc-screen.csv` and applies the verdict rules, so that thresholds
# can be tuned without re-reading 23 GB of raw flux data. The expensive
# measurement and the cheap rule-fitting are deliberately separate files.
#
# THE LABEL. A site is treated as hand-flagged if the project reconstructs or
# replaces its soil temperature by any of the five mechanisms that exist:
# `ts_col == "TS_linear"`, `estimate_Ts == "YES"`, membership of
# `SITES_TS_FROM_TA_RECENT`, `SITES_TS_FROM_TA_COLD`, or
# `SITES_TS_SYNTHETIC_AMERIFLUX`.
#
# That label is noisy, and the disagreements matter as much as the agreements.
# `ts_col = "TS_linear"` was not applied on a single consistent criterion --
# some of those sites have short records, some have sensors that look wrong,
# some are unexplained -- so a site the screen passes and a human failed is
# not automatically a miss. The output lists both directions for inspection
# rather than reporting an accuracy and stopping.
#
#   pixi run Rscript scripts/ts-qc-validate.R

suppressMessages({
  library(dplyr)
  library(targets)
})
tar_source()
source("scripts/ts-rework-common.R")

args <- commandArgs(trailingOnly = TRUE)
infile <- parse_opt(args, "--in", file.path("data-proc", "ts-rework", "ts-qc-screen.csv"))
outfile <- parse_opt(args, "--out", file.path("data-proc", "ts-rework", "ts-qc-validate.csv"))

qc <- readr::read_csv(infile, show_col_types = FALSE, progress = FALSE)
si <- readr::read_csv(SITE_INFO_CSV, show_col_types = FALSE, progress = FALSE)

# The column the pipeline actually uses, which is the one a verdict is about.
primary_col <- function(site_ID, declared_ts, reader) {
  if (identical(reader, "ameriflux")) declared_ts else "TS_F_MDS_1"
}
si$reader <- vapply(seq_len(nrow(si)), function(i) site_reader(si[i, ]), "")
si$primary <- vapply(
  seq_len(nrow(si)),
  function(i) primary_col(si$site_ID[i], si$TS[i], si$reader[i]),
  ""
)

# `ts_col` means different things in the two tables -- the screened column in
# one, the pipeline's selection declaration in the other -- so the screen's is
# renamed before the join rather than left to `.x`/`.y` suffixes.
d <- qc |>
  dplyr::rename(qc_column = "ts_col") |>
  dplyr::inner_join(
    dplyr::select(si, "site_ID", "primary", "IGBP", "Climate_class", "LAT",
                  "ts_col", "estimate_Ts", "reader"),
    by = "site_ID"
  ) |>
  dplyr::filter(.data$qc_column == .data$primary)

# ---------------------------------------------------------------- the rules
#
# Revised after a first pass over all 117 sites, which falsified two of the
# three structural rules as originally specified. Both corrections are kept
# visible rather than quietly folded in.
#
# (1) PHASE IS ONLY KNOWN MODULO 24 HOURS. A strongly damped sensor lags by
#     more than half a day, and the harmonic phase then wraps: DE-Hte's
#     measured lag of -7.8 h is +16.2 h. Wrapping to [-12, 12] and reading
#     the result as "soil leads air" is simply wrong. The amplitude gives an
#     independent prior on the lag -- lag = (z/d) * 24 / 2*pi -- so it can
#     pick the right branch.
#
# (2) z/d FROM AMPLITUDE AND z/d FROM LAG DO NOT AGREE, AND SHOULD NOT.
#     The theory assumes the forcing is applied at the soil surface. Air
#     temperature is not the soil surface: a canopy and a surface boundary
#     layer sit in between, and they attenuate the diurnal amplitude a great
#     deal while adding little phase lag. So the amplitude ratio carries
#     canopy attenuation *plus* soil damping while the lag carries soil
#     damping alone, and z/d from amplitude systematically exceeds z/d from
#     lag. Measured over the network the ratio has median 2.58 and an
#     interquartile range of 1.83-3.85, nowhere near the 1.0 the clean theory
#     predicts.
#
#     The check survives, but as a *relative* one: flag a site whose ratio
#     sits outside the network's own central range rather than outside a
#     theoretical band. That is the right form for it anyway, because it is
#     what a new site can be judged against.
#
# (3) COLUMN NAME ORDER IS NOT DEPTH ORDER IN AMERIFLUX. `TS_F_MDS_1/2/3` do
#     increase in depth, but AmeriFlux's `TS_<h>_<v>_<r>` puts the *vertical*
#     index second, so `TS_1_1_1` and `TS_2_1_1` are two horizontal positions
#     at the same depth. Sorting by name conflated replicates with depths and
#     fired the rule at 55 of 94 sites. Parsing the vertical index fixes it.

# Depth rank: NA where the naming carries no depth information.
depth_rank <- function(col) {
  m <- regmatches(col, regexec("^TS_F_MDS_([0-9]+)$", col))[[1]]
  if (length(m) == 2) return(as.numeric(m[2]))
  m <- regmatches(col, regexec("^TS_([0-9]+)_([0-9]+)_([0-9]+)$", col))[[1]]
  if (length(m) == 4) return(as.numeric(m[3]))  # vertical index
  NA_real_
}

# Amplitude must fall with depth. Evaluated per site over the columns whose
# names carry a depth, and only where at least two distinct depths exist.
depth_order_flag <- qc |>
  dplyr::mutate(rank = vapply(.data$ts_col, depth_rank, 0.0)) |>
  dplyr::filter(!is.na(.data$rank), !is.na(.data$amp_ratio)) |>
  dplyr::summarise(
    n_depths = dplyr::n_distinct(.data$rank),
    # Spearman correlation of amplitude against depth: should be negative.
    depth_amp_cor = if (dplyr::n_distinct(.data$rank) > 1) {
      suppressWarnings(stats::cor(.data$rank, .data$amp_ratio, method = "spearman"))
    } else NA_real_,
    .by = "site_ID"
  )

d <- dplyr::left_join(d, depth_order_flag, by = "site_ID")

# Resolve the 24-hour phase ambiguity using the amplitude-implied lag, then
# recompute z/d from the unwrapped value.
d <- d |>
  dplyr::mutate(
    lag_expected = .data$zd_amp * 24 / (2 * pi),
    lag_unwrapped = .data$lag_h + 24 * round((.data$lag_expected - .data$lag_h) / 24),
    zd_lag_uw = .data$lag_unwrapped * 2 * pi / 24,
    consistency_uw = dplyr::if_else(
      is.finite(.data$zd_amp) & is.finite(.data$zd_lag_uw) & .data$zd_lag_uw > 0.02,
      .data$zd_amp / .data$zd_lag_uw, NA_real_
    )
  )

AIRLIKE_AMP <- 0.85     # buried soil damps; this much amplitude is not soil
AIRLIKE_LAG <- 0.75     # ... and it should lag, in hours
FLAT_AMP <- 0.03        # no diurnal signal at all
STUCK_FRAC <- 0.10      # a tenth of the record in repeated-value runs
COVERAGE_MIN <- 0.40
# Relative, from the network's own distribution rather than from theory.
cons_q <- stats::quantile(d$consistency_uw, c(0.05, 0.95), na.rm = TRUE)

d <- d |>
  dplyr::mutate(
    # --- hard flags: a physical impossibility or an unusable record --------
    flag_airlike = .data$amp_ratio > AIRLIKE_AMP & abs(.data$lag_unwrapped) < AIRLIKE_LAG,
    flag_flat = .data$amp_ratio < FLAT_AMP,
    flag_stuck = !is.na(.data$stuck_frac) & .data$stuck_frac > STUCK_FRAC,
    flag_leads = !is.na(.data$lag_unwrapped) & .data$lag_unwrapped < -0.75,
    flag_coverage = .data$coverage < COVERAGE_MIN,
    # --- soft flags: unusual for this network, not impossible --------------
    flag_inconsistent = !is.na(.data$consistency_uw) &
      (.data$consistency_uw < cons_q[[1]] | .data$consistency_uw > cons_q[[2]]),
    flag_depth_order = !is.na(.data$depth_amp_cor) & .data$depth_amp_cor > 0,
    n_hard = rowSums(dplyr::across(c("flag_airlike", "flag_flat", "flag_stuck",
                                     "flag_leads", "flag_coverage")), na.rm = TRUE),
    n_soft = rowSums(dplyr::across(c("flag_inconsistent", "flag_depth_order")), na.rm = TRUE),
    verdict = dplyr::case_when(
      .data$n_hard > 0 ~ "BAD",
      .data$n_soft > 0 ~ "SUSPECT",
      TRUE ~ "GOOD"
    ),
    hand_flagged = .data$ts_col == "TS_linear" |
      .data$estimate_Ts == "YES" |
      .data$site_ID %in% c(SITES_TS_FROM_TA_RECENT, SITES_TS_FROM_TA_COLD,
                           SITES_TS_SYNTHETIC_AMERIFLUX)
  )

readr::write_csv(d, outfile)
cat("Wrote", outfile, "-", nrow(d), "sites with a primary column screened\n\n")

cat("--- distribution of the diagnostics over", nrow(d), "sites ---\n")
q <- function(lab, x) cat(sprintf("  %-24s %7.3f %7.3f %7.3f %7.3f %7.3f\n", lab,
  stats::quantile(x, .1, na.rm = TRUE), stats::quantile(x, .25, na.rm = TRUE),
  stats::median(x, na.rm = TRUE),
  stats::quantile(x, .75, na.rm = TRUE), stats::quantile(x, .9, na.rm = TRUE)))
cat(sprintf("  %-24s %7s %7s %7s %7s %7s\n", "", "p10","p25","med","p75","p90"))
q("amplitude ratio", d$amp_ratio)
q("lag, wrapped (h)", d$lag_h)
q("lag, unwrapped (h)", d$lag_unwrapped)
q("z/d from amplitude", d$zd_amp)
q("z/d from lag", d$zd_lag)
q("consistency (amp/lag), unwrapped", d$consistency_uw)
q("mean TS - TA (C)", d$mean_offset)
q("stuck fraction", d$stuck_frac)
q("coverage", d$coverage)

cat("\n--- verdict x hand-flagged ---\n")
print(as.data.frame(dplyr::count(d, verdict, hand_flagged)), row.names = FALSE)

cat("\n--- which rule fired ---\n")
print(as.data.frame(d |>
  dplyr::summarise(n_sites = dplyr::n(),
                   dplyr::across(dplyr::starts_with("flag_"), ~sum(.x, na.rm = TRUE)),
                   .by = "hand_flagged")), row.names = FALSE)

cat("\n--- screen says BAD, project treats as measured (candidate misses by the project) ---\n")
print(as.data.frame(d |> dplyr::filter(.data$verdict == "BAD", !.data$hand_flagged) |>
  dplyr::select("site_ID","IGBP","Climate_class","amp_ratio","lag_unwrapped","stuck_frac","coverage") |>
  dplyr::arrange(dplyr::desc(.data$amp_ratio))), row.names = FALSE, digits = 3)

cat("\n--- screen says GOOD, project reconstructs (the screen cannot see the reason) ---\n")
print(as.data.frame(d |> dplyr::filter(.data$verdict == "GOOD", .data$hand_flagged) |>
  dplyr::select("site_ID","IGBP","Climate_class","amp_ratio","lag_unwrapped","consistency_uw","coverage","span_years","frac_qc0") |>
  dplyr::arrange(.data$amp_ratio)), row.names = FALSE, digits = 3)
