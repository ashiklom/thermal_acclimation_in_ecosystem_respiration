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
  left_join(wrapped_growing_year, by = "site_ID")

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
