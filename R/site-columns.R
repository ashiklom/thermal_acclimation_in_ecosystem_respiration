# Checks of the raw columns a site table declares, for the scripts that derive
# the site tables (scripts/derive-site-info-update.R, scripts/derive-site-info-new.R).

#' Problems with the columns AmeriFlux BASE sites declare
#'
#' For every AmeriFlux_BASE site of a site table: the columns it declares (NEE, FC, TA, TS, SW_IN,
#' USTAR, SWC where SWC_use is YES, RH or else VPD; "+"-joined sums split) and the columns step 01
#' reads at particular sites must exist in its downloaded BASE release (path_raw("flux")).
#' Columns step 01 builds itself (US-Ha2's averages of two sensors) count as the raw columns it
#' builds them from; a missing optional column only gets a message.
#'
#' @param site_info A site table.
#' @param manifest data-raw/flux/download_manifest.csv.
#' @return A character vector of problems, one per missing column or missing download (empty: none).
#' @noRd
base_column_problems <- function(site_info, manifest) {
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
  for (s in site_info$site_ID[site_info$source == "AmeriFlux_BASE"]) {
    row <- site_info[site_info$site_ID == s, ]
    if (!s %in% manifest$site_ID[manifest$product == "AmeriFlux_BASE" & !is.na(manifest$file)]) {
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
  problems
}
