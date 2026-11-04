# The site tables the pipeline runs on (THERMAL_SITE_INFO, _targets.R):
#
#   data/site_info_new.csv       (default project) the latest data: the sites of
#                                the site list data/site_info_2026-11-03.csv,
#                                with its fields and its choice of source
#   data/site_info_original.csv  (TAR_PROJECT=original) the manuscript's sites
#                                and input data, data/site_info.csv
#
# The list's sources become the pipeline's (R/read-raw.R):
#   - FLUXNET_Shuttle at a manuscript site with published FLUXNET-format
#     products: the shuttle product spliced with them, as
#     data/site_info_update.csv has it ("SHUTTLE+<products>"; they fill the soil
#     drivers the shuttle lacks). Where data/site_info_update.csv found the
#     shuttle product unusable (AU-Tum, FR-FBn, DE-Akm), the published products,
#     recorded in data/site_info_overrides.csv
#   - FLUXNET_Shuttle elsewhere: "SHUTTLE" (the manuscript's AmeriFlux sites
#     too). A manuscript site whose shuttle product step 01 cannot read keeps
#     AmeriFlux_BASE, recorded in the overrides
#   - published product names, AmeriFlux_BASE: as listed; AmeriFlux BASE sites
#     with the columns renamed since the published releases
#     (data/site_info_update_remaps.csv), every declared column checked
#   - self_compile: "SELF", with `fit` FALSE: no flux record yet, so no per-site
#     targets, only the rows of the tables of all sites (ERA5, MODIS, ...)
# FLUXNET-format sites' AmeriFlux column fields are emptied, and every
# FLUXNET-format site gets its NEE column, which step 01 reads
# (prep_site_euroflux()): NEE_VUT_REF where one of its products has it
# (read_fluxnet_record() combines their columns), else NEE_CUT_REF (e.g.
# TERN's ONEFlux runs).
#
# Both tables gain
#   - season_wrap: TRUE where the growing season spans New Year, so that
#     step 01 counts the days from 1 July (the manuscript's AU-Tum and ZA-Kru;
#     added sites south of 10 S); FALSE elsewhere
#   - added: the date a site was added: "" for the manuscript's sites,
#     2026-10-26 for the sites of the shuttle list of that day
#     (../original-data/site_info_shuttle10262026.csv), 2026-11-03 for the rest
#   - soil_temperature: FALSE where a site reading the shuttle product alone
#     has no soil temperature (no TS_F_MDS_1): such a site is run with the
#     air-temperature columns only (R/ta-columns.R)
#   - fit: FALSE where the site's flux record is not available yet
# The list has no estimate_Ts: NO everywhere (no random-forest soil
# temperature); the original project keeps the manuscript's.
# Koppen classes take the manuscript's spelling (Bsk, not BSk), and a MAP of 0
# (no value) becomes NA.
#
# Sites of the list that are not added, with the reason, go to
# data/site_info_excluded.csv: added sites the shuttle catalogue does not have
# or, once downloaded (scripts/download-flux-update.R), whose product lacks a
# column step 01 needs other than soil temperature. Where a site reading the
# shuttle product alone declares SWC_use = YES but the product has no soil
# water, SWC_use becomes NO (the ERA5 soil water), recorded in
# data/site_info_overrides.csv. Added sites not downloaded yet go in unchecked.
#
# Rows: the manuscript's sites in the manuscript's order, then each added
# date's sites in the list's order, those without soil temperature after
# those with, those without a flux record last.
#
# Usage (from the repository root; rerun after downloads):
#   Rscript scripts/derive-site-info-new.R

source(file.path("R", "paths.R"))
source(file.path("R", "read-raw.R"))
source(file.path("R", "site-columns.R"))

site_list <- file.path("data", "site_info_2026-11-03.csv")
list_date <- "2026-11-03"
earlier_lists <- c("2026-10-26" = path_original("site_info_shuttle10262026.csv"))
read_table <- function(path) utils::read.csv(path, check.names = FALSE, fileEncoding = "UTF-8-BOM")

published <- read_table(file.path("data", "site_info.csv"))
latest <- read_table(file.path("data", "site_info_update.csv"))
listed <- read_table(site_list)
remaps <- utils::read.csv(file.path("data", "site_info_update_remaps.csv"))
stopifnot(identical(published$site_ID, latest$site_ID), identical(names(published), names(latest)),
          all(names(listed) %in% names(published)), !anyDuplicated(listed$site_ID),
          all(published$site_ID %in% listed$site_ID))
manifest_path <- path_raw("flux", "download_manifest.csv")
manifest <- if (file.exists(manifest_path)) utils::read.csv(manifest_path) else
  data.frame(site_ID = character(), product = character(), file = character())

# the manuscript's (step 01); its list's BR-Sa1 is an AmeriFlux site, which
# the original scripts did not wrap
wrapped <- c("AU-Tum", "ZA-Kru")
base_fields <- c("NEE", "FC", "TA", "TS", "SWC", "SW_IN", "USTAR", "RH", "VPD")

excluded <- data.frame(site_ID = character(), reason = character())
exclude <- function(site_id, reason) {
  message(site_id, ": not added, ", reason)
  excluded[nrow(excluded) + 1, ] <<- list(site_id, reason)
}
overrides <- data.frame(site_ID = character(), field = character(), declared = character(), used = character(),
                        reason = character())
override <- function(site_id, field, declared, used, reason) {
  message(site_id, ": ", field, " ", declared, " -> ", used, " (", reason, ")")
  overrides[nrow(overrides) + 1, ] <<- list(site_id, field, declared, used, reason)
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
  fluxnet <- which(x$source != "AmeriFlux_BASE" & x$source != "SELF" & !x$site_ID %in% skip)
  stopifnot(all(is.na(x$NEE[fluxnet]) | x$NEE[fluxnet] == ""))
  x$NEE[fluxnet] <- mapply(fluxnet_nee, x$site_ID[fluxnet], x$source[fluxnet])
  x
}

# the shuttle catalogue (the newest snapshot written by fluxnet-shuttle listall)
snapshots <- c(list.files(path_raw("flux-catalog"), "^fluxnet_shuttle_snapshot_.*\\.csv$", full.names = TRUE),
               list.files(path_raw("flux", "SHUTTLE"), "^fluxnet_shuttle_snapshot_.*\\.csv$", full.names = TRUE))
stopifnot(length(snapshots) > 0)
catalogue <- utils::read.csv(snapshots[order(basename(snapshots), decreasing = TRUE)][1])

# a site's shuttle product as step 01 reads it alone (prep_site_euroflux()):
# list(problem = why it cannot be used, or "not downloaded"), else list(header =)
required <- c("TIMESTAMP_START", "TA_F_MDS", "TA_F_MDS_QC", "NIGHT", "SW_IN_F_MDS")
shuttle_check <- function(s) {
  if (!s %in% catalogue$site_id) return(list(problem = "not in the FLUXNET shuttle catalogue"))
  if (!s %in% manifest$site_ID[manifest$product == "SHUTTLE" & !is.na(manifest$file)]) return(list(problem = "not downloaded"))
  h <- names(utils::read.csv(raw_product_file(s, "SHUTTLE"), nrows = 1, check.names = FALSE))
  missing <- setdiff(required, h)
  if (!length(intersect(c("NEE_VUT_REF", "NEE_CUT_REF"), h))) missing <- c(missing, "NEE_VUT_REF or NEE_CUT_REF")
  if (length(missing)) return(list(problem = paste("shuttle product lacks", paste(missing, collapse = ", "))))
  list(header = h)
}

x <- listed
x$estimate_Ts <- if (is.null(listed$estimate_Ts)) "NO" else ifelse(listed$estimate_Ts %in% "YES", "YES", "NO")
x <- x[, names(published)]
manuscript <- x$site_ID %in% published$site_ID
x$added <- ""
for (d in names(earlier_lists)) {
  earlier <- read_table(earlier_lists[[d]])$site_ID
  x$added[!manuscript & x$added == "" & x$site_ID %in% earlier] <- d
}
x$added[!manuscript & x$added == ""] <- list_date
x$season_wrap <- ifelse(manuscript, x$site_ID %in% wrapped, x$LAT < -10)
x$soil_temperature <- TRUE
x$fit <- TRUE
x$Climate_class <- ifelse(startsWith(x$Climate_class, "B"), paste0("B", tolower(substring(x$Climate_class, 2))), x$Climate_class)
x$MAP[!is.na(x$MAP) & x$MAP == 0] <- NA
for (f in base_fields) x[[f]] <- ifelse(is.na(x[[f]]), "", as.character(x[[f]]))

keep <- rep(TRUE, nrow(x))
not_downloaded <- character()
for (i in seq_len(nrow(x))) {
  s <- x$site_ID[i]
  declared <- x$source[i]
  published_source <- published$source[published$site_ID == s]
  if (declared == "self_compile") {
    x$source[i] <- "SELF"
    x$fit[i] <- FALSE
    next
  }
  if (declared == "FLUXNET_Shuttle" && manuscript[i] && published_source != "AmeriFlux_BASE") {
    # spliced with the published products where data/site_info_update.csv uses the shuttle
    used <- latest$source[latest$site_ID == s]
    x$source[i] <- used
    if (!startsWith(used, "SHUTTLE+")) override(s, "source", declared, used, "shuttle product unusable alongside the published record (scripts/derive-site-info-update.R)")
  } else if (declared == "FLUXNET_Shuttle") {
    check <- shuttle_check(s)
    if (!is.null(check$problem) && manuscript[i]) {
      override(s, "source", declared, "AmeriFlux_BASE", check$problem)
      x$source[i] <- "AmeriFlux_BASE"
    } else if (identical(check$problem, "not downloaded")) {
      x$source[i] <- "SHUTTLE"
      not_downloaded <- c(not_downloaded, s)
    } else if (!is.null(check$problem)) {
      exclude(s, check$problem)
      keep[i] <- FALSE
      next
    } else {
      x$source[i] <- "SHUTTLE"
      x$soil_temperature[i] <- all(c("TS_F_MDS_1", "TS_F_MDS_1_QC") %in% check$header)
      if (x$SWC_use[i] == "YES" && !"SWC_F_MDS_1" %in% check$header) {
        override(s, "SWC_use", "YES", "NO", "shuttle product has no SWC_F_MDS_1: ERA5 soil water")
        x$SWC_use[i] <- "NO"
      }
    }
  }
  if (x$source[i] == "AmeriFlux_BASE") {
    # the columns renamed since the published releases
    for (r in which(remaps$site_ID == s)) {
      value <- trimws(x[[remaps$field[r]]][i])
      if (identical(value, trimws(remaps$published[r]))) x[[remaps$field[r]]][i] <- remaps$update[r] else
        stopifnot(identical(value, trimws(remaps$update[r])))
    }
  } else {
    for (f in base_fields) x[[f]][i] <- ""
  }
}
x <- x[keep, ]
if (length(not_downloaded)) message(length(not_downloaded), " added sites not downloaded yet (unchecked): ",
                                    paste(not_downloaded, collapse = " "))
problems <- base_column_problems(x, manifest)
if (length(problems)) stop("declared columns missing from the BASE releases:\n", paste(problems, collapse = "\n"), call. = FALSE)

group <- ifelse(x$added == "", 0, match(x$added, c(names(earlier_lists), list_date)))
x <- x[order(group, match(x$site_ID, published$site_ID), !x$fit, !x$soil_temperature), ]
site_info_new <- with_nee(x, skip = not_downloaded)

original <- published
original$season_wrap <- original$site_ID %in% wrapped
original$added <- ""
original$soil_temperature <- TRUE
original$fit <- TRUE

utils::write.csv(site_info_new, file.path("data", "site_info_new.csv"), row.names = FALSE)
utils::write.csv(with_nee(original), file.path("data", "site_info_original.csv"), row.names = FALSE)
utils::write.csv(excluded, file.path("data", "site_info_excluded.csv"), row.names = FALSE)
utils::write.csv(overrides, file.path("data", "site_info_overrides.csv"), row.names = FALSE)
added <- site_info_new[site_info_new$added != "", ]
message(nrow(site_info_new), " sites (", nrow(added), " added, ", sum(added$season_wrap), " of them wrapped); ",
        sum(!site_info_new$soil_temperature), " without soil temperature (",
        paste(site_info_new$site_ID[!site_info_new$soil_temperature], collapse = " "), "); ",
        sum(!site_info_new$fit), " without a flux record; sources: ",
        paste(names(table(site_info_new$source)), table(site_info_new$source), collapse = ", "), "; ",
        nrow(excluded), " excluded; ", nrow(overrides), " overrides")
