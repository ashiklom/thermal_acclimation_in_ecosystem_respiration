#!/usr/bin/env Rscript

# Compare annual NEE and soil-temperature cycles across one TERN site and two ICOS sites.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(readr)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)
sites_arg <- args[grepl("^--sites=", args)]
sites <- if (length(sites_arg) == 1) {
  trimws(strsplit(sub("^--sites=", "", sites_arg), ",", fixed = TRUE)[[1]])
} else {
  c("AU-Tum", "FR-FBn", "FR-Fon")
}
if (length(sites) != 3) stop("Exactly three sites are required.")

project_dir <- normalizePath(".", mustWork = TRUE)
tern_dir <- file.path(project_dir, "data-raw", "TERN")
icos_dir <- file.path(project_dir, "data-raw", "ICOS")
feature_tern <- read_csv(file.path(project_dir, "data-proc", "features", "growing_season_feature_TERN.csv"), show_col_types = FALSE)
feature_icos <- read_csv(file.path(project_dir, "data-proc", "features", "growing_season_feature_ICOS.csv"), show_col_types = FALSE)

find_input <- function(site) {
  pattern <- if (site == "AU-Tum") {
    "^AU-Tum_TERN_[A-Z0-9]+_FLUXNET_HH\\.csv$"
  } else {
    paste0("^", site, "_ICOS_L2_FLUXNET_HH\\.csv$")
  }
  directory <- if (site == "AU-Tum") tern_dir else icos_dir
  files <- list.files(directory, pattern = pattern, full.names = TRUE, recursive = TRUE)
  if (length(files) != 1) {
    stop("Expected one normalized input for ", site, "; found ", length(files), ".")
  }
  files
}

read_site <- function(site) {
  input <- find_input(site)
  data <- read_csv(input, na = c("-9999", "NA", ""), show_col_types = FALSE) |>
    mutate(
      TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START), tz = "UTC"),
      YEAR = year(TIMESTAMP_START),
      DOY = yday(TIMESTAMP_START),
      # Use a non-leap-year coordinate so all years share a common x-axis.
      DOY = pmin(DOY, 365L)
    ) |>
    transmute(site = site, YEAR, DOY, NEE = NEE_VUT_REF, TS = TS_F_MDS_1)

  data |>
    group_by(site, YEAR, DOY) |>
    summarise(
      NEE = mean(NEE, na.rm = TRUE),
      TS = mean(TS, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      NEE = if_else(is.nan(NEE), NA_real_, NEE),
      TS = if_else(is.nan(TS), NA_real_, TS)
    )
}

features <- bind_rows(
  feature_tern |>
    filter(site_ID %in% sites) |>
    mutate(source = "TERN"),
  feature_icos |>
    filter(site_ID %in% sites) |>
    mutate(source = "ICOS")
) |>
  filter(site_ID %in% sites) |>
  distinct(site_ID, .keep_all = TRUE)

if (nrow(features) != 3) {
  stop("Growing-season features are missing for: ", paste(setdiff(sites, features$site_ID), collapse = ", "))
}

series <- bind_rows(lapply(sites, read_site)) |>
  pivot_longer(c(NEE, TS), names_to = "variable", values_to = "value") |>
  mutate(
    variable = recode(variable, NEE = "NEE", TS = "Soil temperature"),
    site = factor(site, levels = sites),
    variable = factor(variable, levels = c("NEE", "Soil temperature"))
  )

climatology <- series |>
  group_by(site, DOY, variable) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
  mutate(value = if_else(is.nan(value), NA_real_, value))

thresholds <- series |>
  filter(variable == "NEE") |>
  group_by(site) |>
  summarise(
    nee_threshold = max(min(value, na.rm = TRUE) * 0.2, -0.8),
    .groups = "drop"
  ) |>
  mutate(variable = factor("NEE", levels = levels(series$variable)))

shade <- features |>
  transmute(
    site = factor(site_ID, levels = sites),
    xmin = gStart,
    xmax = pmin(gEnd, 365),
    ymin = -Inf,
    ymax = Inf
  )

plot_data <- series |>
  left_join(shade |> select(site, xmin, xmax), by = "site")

plot <- ggplot(plot_data, aes(x = DOY, y = value, group = factor(YEAR), colour = factor(YEAR))) +
  geom_rect(
    data = shade,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "grey70",
    alpha = 0.25
  ) +
  geom_line(linewidth = 0.25, linetype = "dashed", na.rm = TRUE) +
  geom_line(
    data = climatology,
    mapping = aes(x = DOY, y = value),
    inherit.aes = FALSE,
    colour = "black",
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  geom_hline(
    data = thresholds,
    aes(yintercept = nee_threshold),
    inherit.aes = FALSE,
    linetype = "dotted",
    colour = "grey30"
  ) +
  geom_hline(
    data = tibble(
      site = factor(sites, levels = sites),
      variable = factor("Soil temperature", levels = levels(series$variable)),
      yintercept = 0
    ),
    aes(yintercept = yintercept),
    inherit.aes = FALSE,
    linetype = "dotted",
    colour = "grey30"
  ) +
  facet_grid(variable ~ site, scales = "free_y", switch = "y") +
  scale_x_continuous(breaks = c(1, 60, 121, 182, 244, 305, 365), limits = c(1, 365)) +
  scale_colour_viridis_d(guide = "none") +
  labs(
    title = "Annual NEE and soil-temperature cycles",
    subtitle = "Thin dashed lines: individual years; thick black line: interannual daily climatology",
    x = "Day of year",
    y = NULL,
    caption = "Grey shading marks the source-specific identified growing season. Dotted lines show the NEE threshold and 0 degrees C soil temperature."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.placement = "outside",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

output <- file.path(project_dir, "figures", "growing-season-comparison-AU-Tum-ICOS.png")
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
ggsave(output, plot, width = 13, height = 7.5, units = "in", dpi = 300)
message("Wrote ", output)
