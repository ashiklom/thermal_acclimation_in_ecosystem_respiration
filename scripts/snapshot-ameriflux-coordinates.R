# Write data/ameriflux_coordinates.csv: the site coordinates 01_02b uses to
# compute sunrise and sunset at the AmeriFlux BASE sites.
#
# 01_02b used to fetch them from the live AmeriFlux API
# (amerifluxr::amf_site_info()) on every run, so its day/night split could
# change whenever AmeriFlux edited a site's metadata. This snapshot holds the
# coordinates that reproduce the published step-01 outputs exactly: the API's
# values as of 2026-10-20, except at US-Ced and US-Slt, whose API coordinates
# have changed since the published run (with today's values a handful of
# half-hours within a second of sunrise/sunset flip). Their values here are
# the ones in data/site_info.csv, which reproduce the published outputs.
#
# Usage: Rscript scripts/snapshot-ameriflux-coordinates.R

site_info <- utils::read.csv(file.path("data", "site_info.csv"))
sites <- site_info$site_ID[site_info$source == "AmeriFlux_BASE"]
api <- amerifluxr::amf_site_info()
coords <- api[api$SITE_ID %in% sites, c("SITE_ID", "LOCATION_LAT", "LOCATION_LONG")]
names(coords) <- c("site_ID", "LAT", "LONG")
stopifnot(setequal(coords$site_ID, sites))

changed_since_published <- c("US-Ced", "US-Slt")
for (s in changed_since_published) {
  coords[coords$site_ID == s, c("LAT", "LONG")] <- site_info[site_info$site_ID == s, c("LAT", "LONG")]
}
coords$source <- ifelse(coords$site_ID %in% changed_since_published,
                        "data/site_info.csv (API value at the published run)", "AmeriFlux API 2026-10-20")
coords <- coords[match(sites, coords$site_ID), ]
utils::write.csv(coords, file.path("data", "ameriflux_coordinates.csv"), row.names = FALSE)
