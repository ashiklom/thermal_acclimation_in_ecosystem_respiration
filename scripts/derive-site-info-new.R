# The site tables the pipeline runs on (THERMAL_SITE_INFO, _targets.R):
#
#   data/site_info_new.csv       (default project) the latest data: the 117
#                                manuscript sites as data/site_info_update.csv
#                                has them, then the sites added from the
#                                FLUXNET shuttle (../original-data/
#                                site_info_shuttle10262026.csv, source
#                                FLUXNET_Shuttle) with source "SHUTTLE", those
#                                whose products have no soil temperature last
#   data/site_info_original.csv  (TAR_PROJECT=original) the manuscript's sites
#                                and input data, data/site_info.csv
#
# Both gain
#   - season_wrap: TRUE where the growing season spans New Year, so that
#     step 01 counts the days from 1 July (the manuscript's AU-Tum and ZA-Kru;
#     added sites south of 10 S); FALSE elsewhere
#   - added: the date a site was added ("" for the manuscript's sites)
#   - soil_temperature: FALSE where the site's products have no soil
#     temperature at all (added sites without TS_F_MDS_1): such a site is run
#     with the air-temperature columns only (R/ta-columns.R)
# and every FLUXNET-format site (not AmeriFlux BASE) gets its NEE column, which
# step 01 reads (prep_site_euroflux()): NEE_VUT_REF where one of its products has
# it (read_fluxnet_record() combines their columns), else NEE_CUT_REF (e.g.
# TERN's ONEFlux runs).
# The added sites' Koppen classes take the manuscript's spelling (Bsk, not
# BSk), and a MAP of 0 (no value) becomes NA.
#
# Sites of the shuttle list that are not added, with the reason, go to
# data/site_info_excluded.csv: one already in the table (CA-KLP), one the
# shuttle catalogue does not have, and, once downloaded
# (scripts/download-flux-update.R), one whose product lacks a column step 01
# needs other than soil temperature. Where an added site declares SWC_use = YES but its product has no
# soil water, SWC_use becomes NO (the ERA5 soil water), recorded in
# data/site_info_overrides.csv.
#
# Usage (from the repository root; rerun after downloads):
#   Rscript scripts/derive-site-info-new.R

source(file.path("R", "paths.R"))
source(file.path("R", "read-raw.R"))

added_on <- "2026-10-26"
shuttle_list <- path_original("site_info_shuttle10262026.csv")
read_table <- function(path) utils::read.csv(path, check.names = FALSE, fileEncoding = "UTF-8-BOM")

published <- read_table(file.path("data", "site_info.csv"))
latest <- read_table(file.path("data", "site_info_update.csv"))
shuttle <- read_table(shuttle_list)
stopifnot(identical(published$site_ID, latest$site_ID), identical(names(published), names(latest)),
          identical(names(shuttle), names(published)))

# the manuscript's (step 01); its list's BR-Sa1 is an AmeriFlux site, which
# the original scripts did not wrap
wrapped <- c("AU-Tum", "ZA-Kru")
manuscript_columns <- function(x) {
  x$season_wrap <- x$site_ID %in% wrapped
  x$added <- ""
  x$soil_temperature <- TRUE
  x
}

fluxnet_nee <- function(site_id, source) {
  header <- unlist(lapply(source_products(source), function(product) {
    names(utils::read.csv(raw_product_file(site_id, product), nrows = 1, check.names = FALSE))
  }))
  nee <- intersect(c("NEE_VUT_REF", "NEE_CUT_REF"), header)
  if (!length(nee)) stop(site_id, ": none of its products has NEE_VUT_REF or NEE_CUT_REF")
  nee[1]
}
with_nee <- function(x, skip = character()) {
  fluxnet <- which(x$source != "AmeriFlux_BASE" & !x$site_ID %in% skip)
  stopifnot(all(is.na(x$NEE[fluxnet]) | x$NEE[fluxnet] == ""))
  x$NEE[fluxnet] <- mapply(fluxnet_nee, x$site_ID[fluxnet], x$source[fluxnet])
  x
}

excluded <- data.frame(site_ID = character(), reason = character())
exclude <- function(site_id, reason) {
  message(site_id, ": not added, ", reason)
  excluded[nrow(excluded) + 1, ] <<- list(site_id, reason)
}

new <- shuttle[shuttle$source == "FLUXNET_Shuttle", ]
for (s in intersect(new$site_ID, published$site_ID)) exclude(s, "already a manuscript site (listed twice)")
new <- new[!new$site_ID %in% published$site_ID, ]
stopifnot(!anyDuplicated(new$site_ID))

# the shuttle catalogue (the newest snapshot written by fluxnet-shuttle listall)
snapshots <- c(list.files(path_raw("flux-catalog"), "^fluxnet_shuttle_snapshot_.*\\.csv$", full.names = TRUE),
               list.files(path_raw("flux", "SHUTTLE"), "^fluxnet_shuttle_snapshot_.*\\.csv$", full.names = TRUE))
stopifnot(length(snapshots) > 0)
catalogue <- utils::read.csv(snapshots[order(basename(snapshots), decreasing = TRUE)][1])
for (s in setdiff(new$site_ID, catalogue$site_id)) exclude(s, "not in the FLUXNET shuttle catalogue")
new <- new[new$site_ID %in% catalogue$site_id, ]

new$source <- "SHUTTLE"
new$Climate_class <- ifelse(startsWith(new$Climate_class, "B"),
                            paste0("B", tolower(substring(new$Climate_class, 2))), new$Climate_class)
new$MAP[!is.na(new$MAP) & new$MAP == 0] <- NA
new$season_wrap <- new$LAT < -10
new$added <- added_on

# the downloaded products: columns step 01 needs (prep_site_euroflux())
overrides <- data.frame(site_ID = character(), field = character(), declared = character(), used = character(),
                        reason = character())
manifest_path <- path_raw("flux", "download_manifest.csv")
manifest <- if (file.exists(manifest_path)) utils::read.csv(manifest_path) else data.frame(site_ID = character(), product = character(), file = character())
required <- c("TIMESTAMP_START", "TA_F_MDS", "TA_F_MDS_QC", "NIGHT", "SW_IN_F_MDS")
not_downloaded <- character()
keep <- rep(TRUE, nrow(new))
new$soil_temperature <- TRUE
for (i in seq_len(nrow(new))) {
  s <- new$site_ID[i]
  if (!s %in% manifest$site_ID[manifest$product == "SHUTTLE" & !is.na(manifest$file)]) {
    not_downloaded <- c(not_downloaded, s)
    next
  }
  h <- names(utils::read.csv(raw_product_file(s, "SHUTTLE"), nrows = 1, check.names = FALSE))
  nee <- intersect(c("NEE_VUT_REF", "NEE_CUT_REF"), h)
  missing <- setdiff(required, h)
  if (!length(nee)) missing <- c(missing, "NEE_VUT_REF or NEE_CUT_REF")
  if (length(missing)) {
    exclude(s, paste("shuttle product lacks", paste(missing, collapse = ", ")))
    keep[i] <- FALSE
    next
  }
  new$soil_temperature[i] <- all(c("TS_F_MDS_1", "TS_F_MDS_1_QC") %in% h)
  if (new$SWC_use[i] == "YES" && !"SWC_F_MDS_1" %in% h) {
    overrides[nrow(overrides) + 1, ] <- list(s, "SWC_use", "YES", "NO", "shuttle product has no SWC_F_MDS_1: ERA5 soil water")
    new$SWC_use[i] <- "NO"
  }
}
# (those without soil temperature last, the order they were added in)
new <- new[keep, ]
new <- new[order(!new$soil_temperature), ]
if (length(not_downloaded)) message(length(not_downloaded), " added sites not downloaded yet (unchecked): ",
                                    paste(not_downloaded, collapse = " "))

site_info_new <- with_nee(rbind(manuscript_columns(latest), new), skip = not_downloaded)
utils::write.csv(site_info_new, file.path("data", "site_info_new.csv"), row.names = FALSE)
utils::write.csv(with_nee(manuscript_columns(published)), file.path("data", "site_info_original.csv"), row.names = FALSE)
utils::write.csv(excluded, file.path("data", "site_info_excluded.csv"), row.names = FALSE)
utils::write.csv(overrides, file.path("data", "site_info_overrides.csv"), row.names = FALSE)
message(nrow(site_info_new), " sites (", nrow(new), " added, ", sum(new$season_wrap), " of them wrapped); ",
        sum(!new$soil_temperature), " added without soil temperature (",
        paste(new$site_ID[!new$soil_temperature], collapse = " "), "); ",
        nrow(excluded), " excluded; ", nrow(overrides), " overrides")
