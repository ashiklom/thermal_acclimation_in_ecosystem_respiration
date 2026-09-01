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
    estimate_ts_method = case_when(
      !is.na(.data$netrad_column) ~ "NETRAD",
      .data$site_ID %in% ts_regression_sites ~ "linear regression",
      TRUE ~ NA_character_
    )
  )

# sites_v2 |>
#   count(source)

write_csv(sites_v2, "data-core/site_info.csv", na="")
