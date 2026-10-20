# Sunrise and sunset for each date, chosen the way the published step-01
# outputs chose them.
#
# The published AmeriFlux day/night split (01_02b) was computed with suncalc
# 0.5.1, which located each date's solar day from noon on the clock of the
# machine running R (Sys.timezone()). The authors' machine ran in
# Asia/Shanghai (original/session-info.txt), so every date was evaluated at
# 04:00 UTC. At most AmeriFlux sites the solar noon nearest that instant is
# the previous local day's, so the published outputs use the previous day's
# sunrise and sunset there (a shift of a minute or two, which moves the
# day/night label of a few boundary half-hours per year). suncalc >= 0.5.2
# no longer depends on the machine's time zone, so this function makes the
# published choice explicit: for each date, the sunrise and sunset of the
# solar day whose solar noon is nearest to 12:00 Asia/Shanghai on that date.
#
# Returns the same columns as suncalc::getSunlightTimes(keep = c("sunrise",
# "sunset")): date, lat, lon, sunrise, sunset, with times in time zone `tz`.
sun_times_published <- function(date, lat, lon, tz) {
  reference <- as.POSIXct(paste(date, "12:00:00"), tz = "Asia/Shanghai")
  offsets <- c(-1, 0, 1)
  candidates <- lapply(offsets, function(k) {
    suncalc::getSunlightTimes(
      date = date + k, keep = c("solarNoon", "sunrise", "sunset"),
      lat = lat, lon = lon, tz = tz
    )
  })
  distance <- matrix(
    vapply(candidates, function(x) abs(as.numeric(difftime(x$solarNoon, reference, units = "secs"))), numeric(length(date))),
    ncol = length(offsets)
  )
  chosen <- cbind(seq_along(date), apply(distance, 1, which.min))
  pick_time <- function(var) {
    values <- matrix(vapply(candidates, function(x) as.numeric(x[[var]]), numeric(length(date))), ncol = length(offsets))
    .POSIXct(values[chosen], tz = tz)
  }
  sunrise <- pick_time("sunrise")
  sunset <- pick_time("sunset")
  data.frame(date = date, lat = lat, lon = lon, sunrise = sunrise, sunset = sunset)
}
