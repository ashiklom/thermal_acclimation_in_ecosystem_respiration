# The AmeriFlux BASE releases the manuscript used against the latest ones,
# and both against the FLUXNET-archive shuttle's ONEFlux product (refactor-v2
# stage S18; the analysis website's flux-products page).
#
# For each AmeriFlux site, step 01's record table (ts_base: NEE as the site's
# declared columns build it, air temperature, soil water, and night_ok: the
# manuscript's night rule with its own REddyProc u* threshold) on the
# published data (main targets store) and on the latest data (update store),
# over the time they share:
#   release        NEE, TA and SWC agreement (n, share identical within
#                  1e-6, r, slope new on old, mean difference new - old),
#                  and the night records each keeps (both, only old, only
#                  new; mean NEE of each one's own selection)
#   per year       the same for NEE, so reprocessed years show
#   shuttle        on the hours the shuttle's NEE_VUT_REF (else NEE_CUT_REF)
#                  is an observation (QC 0): the share of them equal (1e-6)
#                  to the old release's NEE and to the new one's
#
# Usage (on Bouchet, from a checkout; stores and data-raw given):
#   Rscript scripts/compare-ameriflux-releases.R --store=_targets --update-store=_targets_update
#       [--sites=A,B] [--out=data-proc/flux-products] [--cores=8]
# Writes ameriflux_releases.csv and ameriflux_releases_yearly.csv to --out.

for (f in c("paths", "read-raw")) source(file.path("R", paste0(f, ".R")))

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit) else default
}
site_info <- utils::read.csv(file.path("data", "site_info.csv"))
ameriflux <- site_info$site_ID[site_info$source == "AmeriFlux_BASE"]
sites <- if (nzchar(opt("sites", ""))) strsplit(opt("sites", ""), ",")[[1]] else ameriflux
store <- opt("store", "_targets")
update_store <- opt("update-store", "_targets_update")
out_dir <- opt("out", path_proc("flux-products"))
cores <- as.integer(opt("cores", "8"))
TOL <- 1e-6

#' A site's ts_base from a store, keyed by midpoint time (UTC clock)
base_table <- function(site_id, store) {
  b <- targets::tar_read_raw(paste0("step01_", gsub("-", "_", site_id)), store = store)$ts_base
  data.frame(time = as.POSIXct(format(b$TIMESTAMP, "%Y-%m-%d %H:%M:%S"), tz = "UTC"), NEE = b$NEE, TA = b$TA, SWC = b$SWC,
             night_ok = b$night_ok)
}

agree <- function(x, y, prefix) {
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]; y <- y[ok]
  out <- c(n = length(x), identical = if (length(x)) mean(abs(y - x) < TOL) else NA, r = NA, slope = NA,
           diff = if (length(x)) mean(y - x) else NA)
  if (length(x) >= 10 && stats::sd(x) > 0) {
    out[["r"]] <- stats::cor(x, y)
    out[["slope"]] <- unname(stats::coef(stats::lm(y ~ x))[2])
  }
  stats::setNames(as.list(out), paste0(prefix, "_", names(out)))
}

release_file <- function(dir, site_id) {
  f <- list.files(dir, pattern = paste0("^AMF_", gsub("-", "\\\\-", site_id), "_BASE-BADM_.*\\.zip$"))
  if (length(f)) f[1] else NA
}

compare_site <- function(site_id) {
  old <- base_table(site_id, store)
  new <- base_table(site_id, update_store)
  j <- merge(old, new, by = "time", suffixes = c("_old", "_new"))
  year <- as.POSIXlt(j$time)$year + 1900L
  span <- function(t) paste(range(as.POSIXlt(t)$year + 1900L), collapse = "-")
  row <- data.frame(site_ID = site_id, release_old = release_file(path_original("SiteData", "AmeriFlux_BASE"), site_id),
                    release_new = release_file(path_raw("flux", "AmeriFlux_BASE"), site_id),
                    years_old = span(old$time), years_new = span(new$time),
                    years_common = if (nrow(j)) span(j$time) else NA, n_common = nrow(j))
  row <- cbind(row, as.data.frame(c(agree(j$NEE_old, j$NEE_new, "NEE"), agree(j$TA_old, j$TA_new, "TA"),
                                    agree(j$SWC_old, j$SWC_new, "SWC"))))
  row$night_both <- sum(j$night_ok_old & j$night_ok_new)
  row$night_only_old <- sum(j$night_ok_old & !j$night_ok_new)
  row$night_only_new <- sum(!j$night_ok_old & j$night_ok_new)
  row$night_mean_old <- mean(j$NEE_old[j$night_ok_old])
  row$night_mean_new <- mean(j$NEE_new[j$night_ok_new])
  row$night_diff_own <- row$night_mean_new - row$night_mean_old

  # the shuttle's observations against both releases
  shuttle <- tryCatch(read_fluxnet(site_id, "SHUTTLE"), error = function(e) NULL)
  row$shuttle_column <- NA; row$shuttle_n <- 0; row$shuttle_equals_old <- NA; row$shuttle_equals_new <- NA
  if (!is.null(shuttle)) {
    col <- if (!is.null(shuttle$NEE_VUT_REF) && any(shuttle$NEE_VUT_REF != -9999)) "NEE_VUT_REF" else "NEE_CUT_REF"
    start <- as.POSIXct(as.character(shuttle$TIMESTAMP_START), format = "%Y%m%d%H%M", tz = "UTC")
    dt <- as.numeric(stats::median(diff(start[1:1000])), units = "secs")
    s <- data.frame(time = start + dt / 2, NEE_shuttle = ifelse(shuttle[[col]] == -9999, NA, shuttle[[col]]),
                    obs = shuttle[[paste0(col, "_QC")]] %in% 0)
    k <- merge(j, s[s$obs & !is.na(s$NEE_shuttle), ], by = "time")
    k <- k[!is.na(k$NEE_old) & !is.na(k$NEE_new), ]
    row$shuttle_column <- col
    row$shuttle_n <- nrow(k)
    if (nrow(k)) {
      row$shuttle_equals_old <- mean(abs(k$NEE_shuttle - k$NEE_old) < TOL)
      row$shuttle_equals_new <- mean(abs(k$NEE_shuttle - k$NEE_new) < TOL)
    }
  }
  yearly <- do.call(rbind, lapply(sort(unique(year)), function(y) {
    m <- year == y
    cbind(data.frame(site_ID = site_id, year = y), as.data.frame(agree(j$NEE_old[m], j$NEE_new[m], "NEE")),
          night_only_old = sum(j$night_ok_old[m] & !j$night_ok_new[m]), night_only_new = sum(!j$night_ok_old[m] & j$night_ok_new[m]),
          night_both = sum(j$night_ok_old[m] & j$night_ok_new[m]))
  }))
  message(site_id, ": NEE identical ", round(row$NEE_identical, 3), ", shuttle = old ", round(row$shuttle_equals_old, 3),
          ", shuttle = new ", round(row$shuttle_equals_new, 3))
  list(summary = row, yearly = yearly)
}

results <- parallel::mclapply(sites, function(s) tryCatch(compare_site(s), error = function(e) {
  message(s, ": FAILED: ", conditionMessage(e))
  list(summary = data.frame(site_ID = s, error = conditionMessage(e)))
}), mc.cores = cores)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(dplyr::bind_rows(lapply(results, `[[`, "summary")), file.path(out_dir, "ameriflux_releases.csv"), row.names = FALSE)
utils::write.csv(dplyr::bind_rows(lapply(results, `[[`, "yearly")), file.path(out_dir, "ameriflux_releases_yearly.csv"), row.names = FALSE)
message("wrote ", out_dir)
