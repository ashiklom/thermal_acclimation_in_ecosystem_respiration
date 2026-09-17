DIR_RAWDATA <- "data-raw"

# Arctic tundra sites with periods of the year where the whole day is daytime
# (or night). Two consequences: sunrise/sunset are undefined on those days and
# have to be filled in by month, and the nighttime respiration filter also keeps
# low-light daytime observations, since otherwise these sites would contribute
# almost no data in peak growing season.
SITES_LOW_LIGHT_NIGHT <- c("US-ICt", "US-ICh", "US-ICs")
