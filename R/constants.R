DIR_RAWDATA <- "data-raw"

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
