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
      TRUE ~ .data$source
    ),
    source = case_when(
      .data$source == "ICOS" & .data$site_ID %in% ww2020_sites ~ "WW2020+FLUXNET+ICOS",
      .data$source == "ICOS" ~ "FLUXNET+ICOS",
      TRUE ~ .data$source
    ),
    estimate_ts_method = case_when(
      !is.na(.data$netrad_column) ~ "NETRAD",
      .data$site_ID %in% ts_regression_sites ~ "linear regression",
      TRUE ~ NA_character_
    )
  )

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
    .data$source == "AmeriFlux_BASE",
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
