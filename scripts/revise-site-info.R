library(dplyr)
library(readr)
library(tibble)

sites <- read_csv("data-core/site_info_orig.csv")

# sites |>
#   count(source)
#
# sites |>
#   filter(grepl("ICOS", source)) |>
#   select(site_ID, source, estimate_Ts) |>
#   arrange(site_ID) |>
#   print(n = Inf)
#
# icos_sites <- sites |>
#   filter(grepl("ICOS", source)) |>
#   arrange(site_ID) |>
#   pull(site_ID)
#
# icos_dirs <- list.files("data-raw/ICOS")

tern_sites <- read_csv("data-core/tern_fluxnet_site_mapping.csv")
netrad_sites <- tribble(
  ~site_ID, ~netrad_column,
  "CA-Man", "NETRAD",
  "CZ-RAJ", "NETRAD",
  "DE-Akm", "NETRAD",
  "FR-Pue", "NETRAD",
  "US-Ced", "NETRAD_1_1_1",
  "US-Ho1", "NETRAD_2_1_1",
  "US-Los", "NETRAD_1_1_1",
  "US-SRG", "NETRAD"
)
# Humidity columns re-declared because the AmeriFlux BASE release the project
# now holds no longer publishes the one `site_info_orig.csv` names.
#
# US-Ho1 and US-Ho2 both asked for `RH_PI_F_2_1_1`, absent from releases 15-5
# and 10-5. Both records do carry `VPD_PI_1_1_1`, PI-provided at the same
# measurement position as the declared air temperature (`T_SONIC_1_1_1`), so
# the sites are switched to the VPD path instead of the RH path. That is the
# better of the two available substitutions: REddyProc wants VPD, and taking
# it directly avoids deriving it from a relative humidity measured at a
# different height. The surviving RH replicates -- `RH_1_1_1`, `RH_PI_2_1_A`
# and friends -- are the alternative if a PI advises otherwise.
#
# Humidity reaches only REddyProc's u-star gap fill, so the choice moves
# `NEE_uStar_f` and, through it, the direct model's GPP proxy. It is not a
# predictor in either respiration model.
redeclared_humidity <- tribble(
  ~site_ID, ~RH_new,          ~VPD_new,
  "US-Ho1", NA_character_,    "VPD_PI_1_1_1",
  "US-Ho2", NA_character_,    "VPD_PI_1_1_1"
)

# What each site's `TS_measured` column actually is -- see `TS_SOURCES` in
# R/constants.R for the levels and what each means for whether the column can
# serve as a truth. Every site not named here is `sensor`; every `estimate_Ts`
# site is `reconstructed` (its estimator is `estimate_ts_method`). These rows
# are the readers' hard-coded branches, written down: the `name_site ==` arms
# in `prep_fluxnet_family()` and `prep_ustar_df()` and the `SITES_TS_FROM_TA_*`
# lists in R/constants.R. The readers still dispatch on those for now; a test
# holds this column to them until they switch to it.
ts_source_sites <- tribble(
  ~site_ID, ~ts_source,
  "CZ-Stn", "sensor_depth2",
  "FI-Sod", "recalibrated",
  "GF-Guy", "ta_substitute",
  "US-Cwt", "borrowed_site",
  "US-MBP", "gapfill_ta",
  "US-NR1", "gapfill_pi",
  "US-ICh", "gapfill_pi",
  "US-ICs", "gapfill_pi",
  "US-BZo", "lm_ta_recent",
  "CA-ARB", "lm_ta_cold",
  "CA-ARF", "lm_ta_cold",
  "CA-KLP", "lm_ta_cold",
  "US-Rms", "lm_ta_cold",
  "US-SRS", "lm_ta_cold",
  "US-ChR", "lm_ta_cold"
)

ts_regression_sites <- c("DE-Hte", "FR-FBn")

# Sites whose growing year does not start on 1 January, and the day of year it
# starts on instead. `wrap_growing_doy()` shifts everything earlier in the
# calendar year past DOY 366 so that a season straddling New Year is one
# contiguous interval -- which is what makes gStart/gEnd, the 14-day windows
# and the gap scan well defined at these sites.
#
# Declared, not derived from `LAT < 0`. BR-Ma2 and BR-Sa1 are also south of the
# equator, but they have no temperature seasonality: their growing season is
# the whole calendar year, and wrapping them would move their pinned gStart/
# gEnd outside the data. The original workflows wrapped by an explicit site
# list for the same reason.
wrapped_growing_year <- tribble(
  ~site_ID, ~growing_year_start,
  "AU-Tum", 183L,
  "ZA-Kru", 183L
)

# Sites whose soil temperature the second pipeline step replaces with a TS ~ TA
# regression. This was the hard-coded `site_TS_issue` vector in R/total_tas.R;
# it is a per-site property, so it belongs here with the other per-site
# switches. A site is in the list iff `ts_col == "TS_linear"`.
#
# `ts_linear_domain` is the data the regression is fitted on, and it is a
# separate axis rather than an implementation detail: every site but one is
# fitted on the `ac` table restricted to TA > 0, while US-Tw1 is fitted on the
# nighttime table because -- per the original's comment -- "slope will be too
# low if using ac data for the subtropical wetland sites". Collapsing the two
# would change that site's coefficients.
ts_linear_sites <- c(
  "BE-Bra", "CA-Cbo", "CA-Gro", "CA-Mer", "CA-Obs", "CA-TP3", "CH-Lae", "DE-RuC",
  "DE-SfS", "FI-Sod", "IT-Ren", "NL-Loo", "US-Bar", "US-BZB", "US-BZF", "US-BZS",
  "US-CMW", "US-GLE", "US-Ha2", "US-IB2", "US-Jo2", "US-KL2", "US-Kon", "US-LL1",
  "US-MBP", "US-Myb", "US-NC4", "US-Tw1", "US-ICt", "BE-Dor", "CA-TP4", "UK-AMo",
  "RU-Fyo", "ZA-Kru", "IT-Tor"
)
ts_linear_night_sites <- c("US-Tw1")

# Provenance, oldest product first. `source` is a `+`-separated ordered list
# because no single product covers the full record at most of these sites.
#
# The ICOS ETC L2 product covers only each station's ICOS-labelled period, so on
# its own it truncated 21 of these 26 records -- DE-Tha to 7 years against the 28
# the manuscript used. The FLUXNET-Archive product (via fluxnet-shuttle) carries
# the full history and is the primary source; ICOS is spliced on top because it
# reaches later at some sites (UK-AMo: shuttle ends 2024, ICOS reaches 2026).
#
# Four sites are short in both current products and need Warm Winter 2020
# (1989-2020) underneath as well. Measured recovery, WW2020 + current:
#   GF-Guy  2004-2020 + 2017-2026 -> 23 yr (needs 20)
#   SE-Deg  2001-2020 + 2018-2025 -> 25 yr (needs 22)
#   FR-Fon  2005-2020 + 2018-2025 -> 21 yr (needs 18)
#   SE-Nor  2014-2020 + 2017-2025 -> 12 yr (needs 11)
# IT-Noe is deliberately not in this list: it is absent from Warm Winter 2020
# altogether, so it stays short at ~5 of 11 years. See docs/data-provenance.md.
ww2020_sites <- c("GF-Guy", "SE-Deg", "FR-Fon", "SE-Nor")

sites_v2 <- sites |>
  left_join(netrad_sites, by = "site_ID") |>
  mutate(
    source = case_when(
      # Replace all FLUXNET + ICOS sites with ICOS-only
      grepl("ICOS", .data$source) ~ "ICOS",
      # TERN sites use TERN directly
      .data$site_ID %in% tern_sites[["fluxnet_site"]] ~ "TERN",
      # All sites with FLUXNET 2020/2025 can use FLUXNET shuttle
      grepl("FLUXNET2020|FLUXNET2025", .data$source) ~ "FLUXNET",
      # A few more specific sites can use FLUXNET shuttle
      .data$site_ID %in% c("FI-Sod", "IT-SRo") ~ "FLUXNET",
      # (FI-Sod picks up FLUXNET2015 and ICOS below.)
      TRUE ~ .data$source
    ),
    source = case_when(
      .data$source == "ICOS" & .data$site_ID %in% ww2020_sites ~ "WW2020+FLUXNET+ICOS",
      .data$source == "ICOS" ~ "FLUXNET+ICOS",
      # FI-Sod's shuttle product is only 2023-2024 and it is absent from Warm
      # Winter 2020, so its early record has to come from FLUXNET2015
      # (2001-2014, acquired by hand). ICOS ETC-Archive L2 reaches 2025 where
      # the shuttle copy stops at 2024. The 2015-2022 stretch has no public
      # product at all -- see docs/data-provenance.md.
      .data$site_ID == "FI-Sod" ~ "FLUXNET2015+FLUXNET+ICOS",
      TRUE ~ .data$source
    ),
    estimate_ts_method = case_when(
      !is.na(.data$netrad_column) ~ "NETRAD",
      .data$site_ID %in% ts_regression_sites ~ "linear regression",
      TRUE ~ NA_character_
    ),
    ts_col = if_else(.data$site_ID %in% ts_linear_sites, "TS_linear", "TS_measured"),
    ts_linear_domain = case_when(
      !.data$site_ID %in% ts_linear_sites ~ NA_character_,
      .data$site_ID %in% ts_linear_night_sites ~ "night",
      TRUE ~ "ac"
    )
  ) |>
  left_join(wrapped_growing_year, by = "site_ID") |>
  left_join(redeclared_humidity, by = "site_ID") |>
  mutate(
    RH = if_else(.data$site_ID %in% redeclared_humidity$site_ID, .data$RH_new, .data$RH),
    VPD = if_else(.data$site_ID %in% redeclared_humidity$site_ID, .data$VPD_new, .data$VPD)
  ) |>
  select(-"RH_new", -"VPD_new") |>
  left_join(ts_source_sites, by = "site_ID") |>
  mutate(ts_source = case_when(
    .data$estimate_Ts == "YES" ~ "reconstructed",
    !is.na(.data$ts_source) ~ .data$ts_source,
    TRUE ~ "sensor"
  ))

# A site cannot be both reconstructed and one of the hand-coded arms: the
# readers apply exactly one, and the declaration has to say which.
stopifnot(!any(ts_source_sites$site_ID %in% sites_v2$site_ID[sites_v2$estimate_Ts == "YES"]))
missing_source <- setdiff(ts_source_sites$site_ID, sites_v2$site_ID)
if (length(missing_source) > 0) {
  stop(
    "ts_source_sites names sites that are not in site_info: ",
    paste(missing_source, collapse = ", ")
  )
}

missing_redeclared <- setdiff(redeclared_humidity$site_ID, sites_v2$site_ID)
if (length(missing_redeclared) > 0) {
  stop(
    "redeclared_humidity names sites that are not in site_info: ",
    paste(missing_redeclared, collapse = ", ")
  )
}

missing_wrapped <- setdiff(wrapped_growing_year$site_ID, sites_v2$site_ID)
if (length(missing_wrapped) > 0) {
  stop(
    "wrapped_growing_year names sites that are not in site_info: ",
    paste(missing_wrapped, collapse = ", ")
  )
}

# Guard the two new columns against drifting out of step: a fit domain without a
# selection is meaningless, and a selection without a domain has nothing to fit.
stopifnot(
  all(is.na(sites_v2$ts_linear_domain) == (sites_v2$ts_col == "TS_measured")),
  all(sites_v2$ts_col %in% c("TS_measured", "TS_linear")),
  all(sites_v2$ts_linear_domain %in% c("ac", "night") | is.na(sites_v2$ts_linear_domain))
)
# Every named site must exist, or a rename upstream would silently shrink the list.
missing_ts_sites <- setdiff(c(ts_linear_sites, ts_linear_night_sites), sites_v2$site_ID)
if (length(missing_ts_sites) > 0) {
  stop(
    "ts_linear_sites names sites that are not in site_info: ",
    paste(missing_ts_sites, collapse = ", ")
  )
}

# sites_v2 |>
#   count(source)

# Guard the invariant `prep_ustar_df()` depends on: any AmeriFlux site we intend
# to read soil water for must name the column to read. Only AmeriFlux consults
# this field -- ICOS/TERN/FLUXNET use the standardised `SWC_F_MDS_1` -- so the
# check is scoped to that source. Failing here names the offending sites. The
# alternative is worse than it sounds: `a[[NA_character_]]` on the base
# data.frame `amf_read_base()` returns is NULL rather than an error, so the
# column is dropped silently and the complaint surfaces much later, in
# `prep_nee_ac()`, as a missing `SWC` column pointing nowhere near the cause.
missing_swc_col <- sites_v2 |>
  filter(
    grepl("AmeriFlux_BASE", .data$source, fixed = TRUE),
    .data$SWC_use == "YES",
    is.na(.data$SWC)
  )
if (nrow(missing_swc_col) > 0) {
  stop(
    "AmeriFlux sites marked `SWC_use == \"YES\"` with no `SWC` column name: ",
    paste(missing_swc_col[["site_ID"]], collapse = ", ")
  )
}

write_csv(sites_v2, "data-core/site_info.csv", na="")
