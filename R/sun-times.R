# Sunrise and sunset for each date, computed the way the published step-01
# outputs computed them.
#
# The published AmeriFlux day/night split (01_02b) was made with suncalc
# 0.5.1, whose getSunlightTimes() evaluated each date at noon on the clock of
# the machine running R (Sys.timezone()) and returned the sunrise and sunset
# of the solar day nearest that instant. The authors' machine ran in
# Asia/Shanghai (original/session-info.txt), so every date was evaluated at
# 04:00 UTC; at most AmeriFlux sites the nearest solar day is then the
# previous local day, so the published outputs use the previous day's
# sunrise and sunset there (a shift of a minute or two, which moves the
# day/night label of a few boundary half-hours per year; far more at sites
# like BR-Ma2, where 04:00 UTC falls almost exactly between two solar noons).
# suncalc >= 0.5.2 evaluates dates at 12:00 UTC and then corrects some rows,
# so this function calls suncalc's core routine, .getTimes(), at the instant
# 0.5.1 used. At the same instants it returns exactly what 0.5.1 returned
# (checked against 0.5.1 for an Arctic, a tropical and a mid-latitude site).
# .getTimes() is internal to suncalc; pixi.toml pins the suncalc version.
#
# Returns the same columns as suncalc::getSunlightTimes(keep = c("sunrise",
# "sunset")): date, lat, lon, sunrise, sunset, with times in time zone `tz`.
sun_times_published <- function(date, lat, lon, tz) {
  instant <- lubridate::force_tz(lubridate::as_datetime(date) + lubridate::hours(12), "Asia/Shanghai")
  times <- suncalc:::.getTimes(date = instant, lat = lat, lng = lon, tz = "UTC")
  sunrise <- times$sunrise
  sunset <- times$sunset
  attr(sunrise, "tzone") <- tz
  attr(sunset, "tzone") <- tz
  data.frame(date = date, lat = lat, lon = lon, sunrise = sunrise, sunset = sunset)
}
