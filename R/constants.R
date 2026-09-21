DIR_RAWDATA <- "data-raw"

# The site declaration table. Named here rather than spelled out at each
# reader so the pipeline can register it as a `format = "file"` target.
SITE_INFO_CSV <- file.path("data-core", "site_info.csv")

# Flux data products, keyed by the token used in `site_info$source`.
#
# A site's `source` is a `+`-separated, ordered provenance list -- oldest
# product first -- because no single product covers the full record at most
# sites. This mirrors the original workflow, whose `source` strings were
# underscore-joined for the same reason (`FLUXNET2025_ICOS2025`).
#
# The reason it is needed: the ICOS "Ecosystem final quality (L2) product in
# ETC-Archive format" covers only the period since a station was ICOS-labelled,
# so a station labelled in 2019 has an L2 product starting in 2019 however long
# it has run. The pre-label history is in the FLUXNET-format products. See
# docs/data-provenance.md and scripts/audit-icos-coverage.R.
#
#   dir     -- directory under data-raw/ holding one sub-directory per site
#   pattern -- regex matching the half-hourly table inside a site directory
#
# The patterns are anchored on `.csv` because the downloaders keep the archive
# they extracted from alongside the table: the archive filename encodes the
# product, year span and release, which is what `scripts/check-data-updates.py`
# compares against the provider.
FLUX_PRODUCTS <- list(
  # Warm Winter 2020 (1989-2020): FLUXNET2015 FULLSET format, deepest history.
  WW2020 = list(dir = "WW2020", pattern = "_FLUXNET2015_FULLSET_(HH|HR)_.*[.]csv$"),
  # FLUXNET-Archive product via fluxnet-shuttle: the merged full-record product.
  FLUXNET = list(dir = "FLUXNET", pattern = "_FLUXMET_(HH|HR)_.*[.]csv$"),
  # FLUXNET2015 (through 2014): the last static release, and for some sites the
  # only source of their early record. Acquired by hand -- see `download.R` --
  # so it gets its own directory rather than sharing the shuttle's.
  FLUXNET2015 = list(dir = "FLUXNET2015", pattern = "_FLUXNET2015_FULLSET_(HH|HR)_.*[.]csv$"),
  # ICOS ETC L2: labelled period only, but reaches later than the others.
  ICOS = list(dir = "ICOS", pattern = "_FLUXMET_(HH|HR)_.*[.]csv$"),
  TERN = list(dir = "TERN", pattern = "_TERN_L3_FLUXNET_HH.*[.]csv$"),
  AmeriFlux_BASE = list(dir = "Ameriflux", pattern = "^AMF_.*_BASE.*[.]zip$")
)

# Column contract for every FLUXNET-format half-hourly table (FLUXNET-Archive,
# ICOS ETC L2, TERN L3, Warm Winter 2020). Reading these with type *guessing*
# has two failure modes that this spec closes:
#
#   * The timestamps are 12-digit stamps like 199601010000. Guessed, they come
#     back as doubles, and `splice_products()` orders and compares them as
#     strings -- so the previous code converted them back with `as.character()`
#     and depended on R not choosing scientific notation. Declaring them
#     character makes the contract the splice relies on explicit.
#   * These files are mostly sentinel-filled, and many columns are -9999 for
#     their entire length. Guessed *with* `-9999` treated as NA, such a column
#     is typed `logical`, and a later `TS >= TS_MIN_VALID` filter then silently
#     compares against a logical NA. `.default = col_double()` keeps it double.
#
# Sentinel removal stays a numeric comparison (`dat[dat == -9999] <- NA`) rather
# than moving into readr's `na` argument, because `na` matches the raw string:
# all 110 tables currently on disk write a bare `-9999`, but a future release
# writing `-9999.0` would slip straight through a string match.
FLUXNET_COL_TYPES <- readr::cols(
  TIMESTAMP_START = readr::col_character(),
  TIMESTAMP_END = readr::col_character(),
  .default = readr::col_double()
)

# Arctic tundra sites with periods of the year where the whole day is daytime
# (or night). Two consequences: sunrise/sunset are undefined on those days and
# have to be filled in by month, and the nighttime respiration filter also keeps
# low-light daytime observations, since otherwise these sites would contribute
# almost no data in peak growing season.
SITES_LOW_LIGHT_NIGHT <- c("US-ICt", "US-ICh", "US-ICs")

# The EuroFlux workflow used a plain `NEE < 0` growing-season cut-off at these
# two sites instead of the usual proportional one. See `detect_growing_season()`.
SITES_GS_NEE_ZERO <- c("FI-Sod", "DE-RuC")

# Sites where measured NEE below 2 C is unreliable. Both the reported
# growing-season temperature floor and the observations themselves are
# truncated there.
TS_MIN_VALID <- 2.0
SITES_TS_MIN_2C <- c("CH-Dav", "US-Ha1", "US-GLE")

# What the column step 01 leaves as `TS_measured` actually is, per site --
# declared in site_info.csv as `ts_source`, one level per mechanism the readers
# apply. The value is the level's answer to the only question downstream
# needs: does this column contain measured soil temperature at every row it
# has, so that a reconstruction can be scored against it?
#
#   sensor         the declared sensor, untouched
#   sensor_depth2  a different depth of the same profile (CZ-Stn) -- a sensor
#   gapfill_pi     the sensor, with its gaps taken from the PI's gap-filled
#                  product (US-NR1, US-ICh, US-ICs): the data provider's own
#                  MDS fill, the same thing `TS_F_MDS_1` already is at every
#                  FLUXNET-family site
#   recalibrated   FI-Sod: the pre-2006 third of the record rebuilt by
#                  chaining two regressions between depths
#   gapfill_ta     US-MBP: gaps filled from air temperature
#   ta_substitute  GF-Guy: air temperature, wholesale
#   borrowed_site  US-Cwt: `TA * 0.647 + 5.14`, coefficients from a neighbour
#   lm_ta_recent   US-BZo: `TS ~ TA` fitted on recent years, wholesale
#   lm_ta_cold     six AmeriFlux sites: `TS ~ TA` fitted above freezing, wholesale
#   reconstructed  the 16 `estimate_Ts` sites: `fix_soil_temp()`'s estimator,
#                  named in `estimate_ts_method`, wholesale
#
# "none" is deliberately strict: a column that is a sensor reading at *most*
# rows still has rows that are not, and a fill cross-validated against it is
# partly scoring itself against a regression. FI-Sod and US-MBP fall on that
# side for that reason; relaxing it is a one-word change here.
TS_SOURCES <- c(
  sensor        = "sensor",
  sensor_depth2 = "sensor",
  gapfill_pi    = "sensor",
  recalibrated  = "none",
  gapfill_ta    = "none",
  ta_substitute = "none",
  borrowed_site = "none",
  lm_ta_recent  = "none",
  lm_ta_cold    = "none",
  reconstructed = "none"
)

ts_source <- function(site_info) {
  src <- site_info[["ts_source"]]
  if (length(src) != 1 || is.na(src) || !src %in% names(TS_SOURCES)) {
    stop(
      "Site ", site_info[["site_ID"]], " declares ts_source = ",
      if (length(src) == 1) shQuote(src) else paste0("length ", length(src)),
      "; must be one of ", paste(shQuote(names(TS_SOURCES)), collapse = ", "),
      ". It is derived in scripts/revise-site-info.R."
    )
  }
  src
}

# "sensor" or "none": whether `TS_measured` is measured soil temperature at
# every row, and so can serve as the truth a reconstruction is scored against.
ts_measured_truth <- function(site_info) {
  unname(TS_SOURCES[[ts_source(site_info)]])
}

# The development site sample: what `_targets.R` runs by default.
#
# Running every northern-hemisphere FLUXNET-family site end to end is not a
# development loop. Cost is roughly (qualifying years x 14-day windows) Stan
# fits per site per model, and the full list of 44 comes to several thousand.
# These six are picked to be cheap *and* to cover every branch the pipeline
# has, so a change that breaks one of them shows up in minutes rather than
# after an overnight run:
#
#   DE-RuC   40  the cheapest site available; TS_linear selection; measured
#                soil water; one of the two SITES_GS_NEE_ZERO cut-offs
#   DE-Hte   63  fix_soil_temp()'s linear-regression fallback, and SWC_use NO,
#                so the direct model takes the ERA5 path
#   DE-Akm   78  fix_soil_temp()'s random-forest (NETRAD) branch
#   FI-Sod  100  a three-product splice (FLUXNET2015+FLUXNET+ICOS), the
#                pre-2006 soil-temperature recalibration, the second NEE-zero
#                site, and the sparse gap profile
#   SE-Deg  176  Warm Winter 2020 in the splice
#   NL-Loo  176  a two-product splice (FLUXNET+ICOS); TS_linear
#   US-Kon   84  the AmeriFlux reader: u-star filtering and gap fill, the
#                compound `FC + SC` column read, RH -> VPD conversion, and the
#                uncapped growing-season cut-off. Its step-01 features
#                reproduce the manuscript's AmeriFlux row exactly.
#
# The number is (original nyear x round(growing-season length / 14)) taken from
# the manuscript's own growing_season_feature_*.csv -- a proxy for how many
# fits a site costs, not a runtime. Step 01 for the six FLUXNET-family sites
# together takes about 40 seconds; US-Kon adds a little over a minute on its
# own, because REddyProc's u-star estimation and gap fill dominate an
# AmeriFlux site and no other reader pays for them.
#
# Two branches are deliberately left out because both are expensive: CH-Dav's
# pre-gap-scan TS >= 2 C truncation (364) and GF-Guy's year-round growing
# season with air temperature standing in for soil (520). Both are pinned by
# tests/ts-swc-baseline.R instead, which needs no model fits.
#
# Set THERMAL_SITES=all to run the full list. See `pipeline_sites()`.
DEV_SITES <- c("DE-RuC", "DE-Hte", "DE-Akm", "FI-Sod", "SE-Deg", "NL-Loo", "US-Kon")
