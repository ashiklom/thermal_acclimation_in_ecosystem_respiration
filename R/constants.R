DIR_RAWDATA <- "data-raw"

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

# AmeriFlux sites with no usable measured soil temperature, where `TS` is
# *constructed* from air temperature during step 01 -- so this is how their
# `TS_measured` column comes to exist, not a later substitution for it. The two
# lists differ only in the rows the regression is fitted on: US-BZo uses its
# recent years because the earlier record is unreliable, the cold sites use all
# rows above freezing. Disjoint from the `ts_col == "TS_linear"` sites, which
# are a step-02 concern.
SITES_TS_FROM_TA_RECENT <- c("US-BZo")
SITES_TS_FROM_TA_COLD <- c("CA-ARB", "CA-ARF", "CA-KLP", "US-Rms", "US-SRS", "US-ChR")
