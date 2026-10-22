# data/site_info_update.csv: data/site_info.csv for the data update
# (refactor-v2 stage S18), with
#   - `source`: FLUXNET-format sites with a usable FLUXNET-archive shuttle
#     product read it first and splice their published products around it
#     ("SHUTTLE+<products>", R/read-raw.R); others unchanged. Usable: it has
#     every column step 01 reads (e.g. TERN's lacks NEE_VUT_REF: only the
#     constant-u* NEE_CUT_*) and the published record's time step
#   - the columns of the AmeriFlux BASE releases that were renamed or
#     dropped since the published ones, from data/site_info_update_remaps.csv
#     (site_ID, field, published, update, reason)
# and checks that every column a site declares exists in its new record
# (stops, listing similar columns, if not).
#
# Usage (from the repository root, after scripts/download-flux-update.R):
#   Rscript scripts/derive-site-info-update.R

source(file.path("R", "paths.R"))
source(file.path("R", "read-raw.R"))

site_info <- utils::read.csv(file.path("data", "site_info.csv"), check.names = FALSE)
remaps <- utils::read.csv(file.path("data", "site_info_update_remaps.csv"))
manifest <- utils::read.csv(path_raw("flux", "download_manifest.csv"))
update <- site_info

# sources
fluxnet_columns <- c("TIMESTAMP_START", "NEE_VUT_REF", "NEE_VUT_REF_QC", "TA_F_MDS", "TS_F_MDS_1", "TS_F_MDS_1_QC",
                     "NIGHT", "SW_IN_F_MDS")
first_rows <- function(path) utils::read.csv(path, nrows = 3)
time_step <- function(x) diff(as.numeric(as.POSIXct(as.character(x$TIMESTAMP_START[1:2]), format = "%Y%m%d%H%M", tz = "UTC"))) / 60
usable_shuttle <- function(s) {
  if (!s %in% manifest$site_ID[manifest$product == "SHUTTLE" & !is.na(manifest$file)]) return(FALSE)
  new <- first_rows(raw_product_file(s, "SHUTTLE"))
  published <- first_rows(raw_product_file(s, source_products(site_info$source[site_info$site_ID == s])[1]))
  sensor <- if (s %in% c("CZ-Stn", "DE-Hte")) c("TS_F_MDS_2", "TS_F_MDS_2_QC") else character()
  missing <- setdiff(c(fluxnet_columns, sensor), names(new))
  if (length(missing)) {
    message(s, ": shuttle product not used, it lacks ", paste(missing, collapse = ", "))
    return(FALSE)
  }
  if (time_step(new) != time_step(published)) {
    message(s, ": shuttle product not used, its time step (", time_step(new), " min) is not the published record's (",
            time_step(published), " min)")
    return(FALSE)
  }
  TRUE
}
fluxnet <- update$source != "AmeriFlux_BASE" & vapply(update$site_ID, function(s) {
  site_info$source[site_info$site_ID == s] != "AmeriFlux_BASE" && usable_shuttle(s)
}, logical(1))
update$source[fluxnet] <- paste("SHUTTLE", gsub("_", "+", update$source[fluxnet]), sep = "+")

# renamed columns
for (i in seq_len(nrow(remaps))) {
  r <- remaps[i, ]
  row <- update$site_ID == r$site_ID
  stopifnot(sum(row) == 1, r$field %in% names(update), identical(trimws(update[[r$field]][row]), trimws(r$published)))
  update[[r$field]][row] <- r$update
}

# every declared column must exist in the new AmeriFlux records
header <- function(site_id) {
  f <- raw_product_file(site_id, "AmeriFlux_BASE", path_raw("flux"))
  inner <- grep("_BASE_.*\\.csv$", utils::unzip(f, list = TRUE)$Name, value = TRUE)
  if (length(inner) > 1) inner <- grep("_BASE_HH_", inner, value = TRUE)
  con <- unz(f, inner)
  on.exit(close(con))
  strsplit(readLines(con, n = 3)[3], ",")[[1]]
}
# columns step 01 builds itself, by the raw columns it builds them from
built <- list("US-Ha2" = c(FC_A_1_1 = "FC_1_1_1 FC_2_1_1", TA_A_1_1 = "TA_1_1_1 TA_2_1_1",
                           PPFD_IN_A_1_1 = "PPFD_IN_1_1_1 PPFD_IN_2_1_1", RH_A_1_1 = "RH_1_1_1 RH_2_1_1",
                           USTAR_A_1_1 = "USTAR_1_1_1 USTAR_2_1_1"))
declared <- function(row) {
  fields <- c("NEE", "FC", "TA", "TS", "SW_IN", "USTAR", if (row$SWC_use == "YES") "SWC", if (row$RH != "") "RH" else "VPD")
  cols <- unlist(lapply(fields, function(f) trimws(strsplit(row[[f]], "\\+")[[1]])))
  cols <- cols[cols != ""]
  b <- built[[row$site_ID]]
  if (is.null(b)) return(unique(cols))
  unique(c(setdiff(cols, names(b)), unlist(strsplit(unname(b[intersect(cols, names(b))]), " "))))
}
# columns step 01 reads at particular sites beyond those declared
extra_columns <- list("US-NR1" = c("TS_PI_1", "SWC_PI_1"), "US-ICh" = "TS_PI_1", "US-ICs" = "TS_PI_1",
                      "US-KM4" = "SW_IN_PI_F", "US-Jo2" = "T_SONIC", "US-Myb" = "TS_2_2_1")
# of which step 01 can do without (it then skips that site-specific fix)
optional_columns <- c("TS_PI_1")
problems <- character()
for (s in update$site_ID[update$source == "AmeriFlux_BASE"]) {
  row <- update[update$site_ID == s, ]
  if (!s %in% manifest$site_ID[!is.na(manifest$file)]) {
    problems <- c(problems, paste0(s, ": no new release downloaded"))
    next
  }
  h <- header(s)
  for (col in setdiff(extra_columns[[s]], h)) {
    if (col %in% optional_columns) message(s, ": ", col, " missing; step 01 skips its gap fill") else
      problems <- c(problems, paste0(s, ": ", col, " (read by step 01 at this site) missing"))
  }
  for (col in setdiff(declared(row), h)) {
    stem <- sub("_.*$", "", col)
    problems <- c(problems, paste0(s, ": ", col, " missing; similar: ", paste(grep(paste0("^", stem, "_"), h, value = TRUE), collapse = " ")))
  }
}
if (length(problems)) stop("declared columns missing from the new releases:\n", paste(problems, collapse = "\n"), call. = FALSE)

utils::write.csv(update, file.path("data", "site_info_update.csv"), row.names = FALSE)
message("sources changed at ", sum(update$source != site_info$source), " sites; ", nrow(remaps), " columns remapped")
