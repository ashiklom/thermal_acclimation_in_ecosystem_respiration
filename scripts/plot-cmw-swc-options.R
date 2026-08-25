#!/usr/bin/env Rscript

library(ggplot2)
library(dplyr)
library(tidyr)

input <- "data/cmw_swc_option_timeseries.csv"
if (!file.exists(input)) {
  stop("Run scripts/audit-cmw-swc-options.R first.")
}

swc <- read.csv(input, stringsAsFactors = FALSE) %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, tz = "UTC"))

long <- swc %>%
  select(TIMESTAMP, YEAR, DOY, sensor_1, sensor_2, mean_of_sensors) %>%
  pivot_longer(c(sensor_1, sensor_2, mean_of_sensors), names_to = "option", values_to = "SWC") %>%
  mutate(option = recode(
    option,
    sensor_1 = "SWC_1_6_1",
    sensor_2 = "SWC_1_7_1",
    mean_of_sensors = "Mean of both fields"
  ))

overview <- long %>%
  filter(TIMESTAMP >= as.POSIXct("2015-01-01", tz = "UTC")) %>%
  ggplot(aes(TIMESTAMP, SWC, colour = option)) +
  geom_line(linewidth = 0.25, na.rm = TRUE) +
  facet_wrap(~ option, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c("SWC_1_6_1" = "#176b87", "SWC_1_7_1" = "#c45532", "Mean of both fields" = "#4c4c4c")) +
  labs(title = "US-CMW soil water content options", subtitle = "Half-hourly records, 2015-2021", x = NULL, y = "Raw SWC value") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))
ggsave("figures/cmw-swc-options-overview.png", overview, width = 11, height = 8, dpi = 180)

scatter <- swc %>%
  filter(!is.na(sensor_1), !is.na(sensor_2)) %>%
  ggplot(aes(sensor_1, sensor_2, colour = YEAR)) +
  geom_point(alpha = 0.12, size = 0.35) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "#333333") +
  coord_equal() +
  scale_colour_viridis_c() +
  labs(title = "The two US-CMW SWC fields are not interchangeable", x = "SWC_1_6_1", y = "SWC_1_7_1", colour = "Year") +
  theme_minimal(base_size = 11)
ggsave("figures/cmw-swc-options-scatter.png", scatter, width = 7, height = 6, dpi = 180)

annual <- long %>%
  group_by(YEAR, option) %>%
  summarise(SWC = mean(SWC, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(YEAR, SWC, colour = option)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.3) +
  scale_colour_manual(values = c("SWC_1_6_1" = "#176b87", "SWC_1_7_1" = "#c45532", "Mean of both fields" = "#4c4c4c")) +
  labs(title = "Annual mean SWC under alternative mappings", x = NULL, y = "Annual mean raw SWC", colour = "Option") +
  theme_minimal(base_size = 11)
ggsave("figures/cmw-swc-options-annual.png", annual, width = 9, height = 5, dpi = 180)
