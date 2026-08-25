#!/usr/bin/env Rscript

library(amerifluxr)
library(dplyr)

dir.create("figures", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

archive <- file.path("data-raw", "AMF_US-CMW_BASE-BADM_3-5.zip")
if (!file.exists(archive)) {
  stop("Missing US-CMW archive: ", archive)
}

raw <- amf_read_base(archive, parse_timestamp = TRUE, unzip = TRUE)
candidate_names <- c("SWC_1_6_1", "SWC_1_7_1")
missing_candidates <- setdiff(candidate_names, names(raw))
if (length(missing_candidates) > 0) {
  stop("Expected candidate fields are missing: ", paste(missing_candidates, collapse = ", "))
}

swc <- raw %>%
  filter(YEAR > 2000) %>%
  transmute(
    TIMESTAMP,
    YEAR,
    DOY,
    sensor_1 = .data[[candidate_names[1]]],
    sensor_2 = .data[[candidate_names[2]]]
  ) %>%
  mutate(
    mean_of_sensors = rowMeans(cbind(sensor_1, sensor_2), na.rm = TRUE),
    mean_of_sensors = ifelse(is.nan(mean_of_sensors), NA, mean_of_sensors),
    difference = sensor_1 - sensor_2,
    both_present = complete.cases(sensor_1, sensor_2)
  )

summary <- bind_rows(lapply(c("sensor_1", "sensor_2", "mean_of_sensors"), function(option) {
  value <- swc[[option]]
  data.frame(
    option = option,
    field = c(candidate_names, "mean(sensor_1, sensor_2)")[match(option, c("sensor_1", "sensor_2", "mean_of_sensors"))],
    n = sum(!is.na(value)),
    fraction_present = mean(!is.na(value)),
    minimum = min(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    maximum = max(value, na.rm = TRUE)
  )
}))

write.csv(summary, "data/cmw_swc_option_audit.csv", row.names = FALSE)
write.csv(swc, "data/cmw_swc_option_timeseries.csv", row.names = FALSE)

cat("US-CMW SWC option audit\n")
print(summary)
cat("\nBoth fields present:", sum(swc$both_present), "of", nrow(swc), "rows\n")
cat("Correlation when both present:", cor(swc$sensor_1[swc$both_present], swc$sensor_2[swc$both_present]), "\n")
cat("Mean sensor_1 - sensor_2 difference:", mean(swc$difference, na.rm = TRUE), "\n")
