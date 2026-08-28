library(dplyr)
library(readr)

sites <- read_csv("data-core/site_info_orig.csv")

sites |>
  count(source)

sites |>
  filter(grepl("ICOS", source)) |>
  select(site_ID, source, estimate_Ts) |>
  arrange(site_ID) |>
  print(n = Inf)

icos_sites <- sites |>
  filter(grepl("ICOS", source)) |>
  arrange(site_ID) |>
  pull(site_ID)

icos_dirs <- list.files("data-raw/ICOS")

icos_sites == icos_dirs

tern_sites <- read_csv("data-core/tern_fluxnet_site_mapping.csv")
sites_v2 <- sites |>
  mutate(source = case_when(
    # Replace all FLUXNET + ICOS sites with ICOS-only
    grepl("ICOS", source) ~ "ICOS",
    # TERN sites use TERN directly
    site_ID %in% tern_sites[["fluxnet_site"]] ~ "TERN",
    # All sites with FLUXNET 2020/2025 can use FLUXNET shuttle
    grepl("FLUXNET2020|FLUXNET2025", source) ~ "FLUXNET",
    # A few more specific sites can use FLUXNET shuttle
    site_ID %in% c("FI-Sod", "IT-SRo") ~ "FLUXNET",
    TRUE ~ source
  ))

sites_v2 |>
  count(source)

write_csv(sites_v2, "data-core/site_info.csv", na="")
