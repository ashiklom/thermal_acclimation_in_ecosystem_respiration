# The manuscript's flux records against the FLUXNET-archive shuttle's ONEFlux
# products, at every site the shuttle has (refactor-v2 stage S18; the
# analysis website's flux-products page).
#
# Streams of night-time NEE records, each selected as step 01 selects them:
#   PUB_VUT, PUB_CUT  FLUXNET-format sites: the published record the
#                     manuscript read (data/site_info.csv `source`,
#                     R/read-raw.R), NEE_VUT_REF or NEE_CUT_REF; selected
#                     with step 01's FLUXNET rule (air temperature present,
#                     NEE QC 0, NIGHT, -5 < NEE < 30)
#   MS                AmeriFlux sites: the manuscript's own record (AmeriFlux
#                     BASE, its own REddyProc u* threshold, step 01's night
#                     rule): step 01's ts_base and night_ok, from the main
#                     targets store
#   SHUT_VUT, SHUT_CUT  the shuttle's product, NEE_VUT_REF (where the
#                     product has it) or NEE_CUT_REF, selected with the
#                     FLUXNET rule
# Comparisons (reference, shuttle): FLUXNET-format sites PUB_VUT vs SHUT_VUT
# ("VUT") and PUB_CUT vs SHUT_CUT ("CUT"); AmeriFlux sites MS vs SHUT_VUT
# ("VUT") and MS vs SHUT_CUT ("CUT"). Records are matched on their midpoint
# time (TIMESTAMP_START + time step / 2, as step 01 keys them).
#
# Usage (on Bouchet, from a checkout; the main store and data-raw given):
#   Rscript scripts/compare-flux-products.R [--sites=A,B] [--store=_targets]
#       [--out=data-proc/flux-products] [--cores=8]
# Writes to --out:
#   summary.csv   per site and comparison: the products, overlapping years,
#                 the night records both select (n, mean of each, mean
#                 difference shuttle - reference, relative difference, r,
#                 slope of shuttle on reference), each stream's own
#                 selection over the overlapping years (n, mean, difference),
#                 daytime observations both have (n, r, slope: a scaling of
#                 the flux shows here), and a status
#   siteyear.csv  the same matched and own-selection differences per year
#   monthly.csv   per site and stream: monthly mean of the selected night
#                 records and their number

for (f in c("paths", "read-raw")) source(file.path("R", paste0(f, ".R")))

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit) else default
}
site_info <- utils::read.csv(file.path("data", "site_info.csv"))
sites <- if (nzchar(opt("sites", ""))) strsplit(opt("sites", ""), ",")[[1]] else site_info$site_ID
store <- opt("store", "_targets")
out_dir <- opt("out", path_proc("flux-products"))
cores <- as.integer(opt("cores", "8"))

#' Midpoint time of each record of a FLUXNET-format table, and its time step (s)
record_times <- function(tab) {
  start <- as.POSIXct(as.character(tab$TIMESTAMP_START), format = "%Y%m%d%H%M", tz = "UTC")
  dt <- as.numeric(stats::median(diff(start[seq_len(min(1000, length(start)))])), units = "secs")
  list(time = start + dt / 2, dt = dt)
}

#' The VUT and CUT streams of a FLUXNET-format table (missing values -9999)
fluxnet_streams <- function(tab, prefix) {
  used <- intersect(c("TA_F_MDS", "NIGHT", "NEE_VUT_REF", "NEE_VUT_REF_QC", "NEE_CUT_REF", "NEE_CUT_REF_QC"), names(tab))
  for (col in used) tab[[col]][tab[[col]] %in% -9999] <- NA
  tm <- record_times(tab)
  year <- as.POSIXlt(tm$time)$year + 1900L
  streams <- list()
  for (v in c("VUT", "CUT")) {
    col <- paste0("NEE_", v, "_REF")
    if (is.null(tab[[col]]) || all(is.na(tab[[col]]))) next
    nee <- tab[[col]]
    obs <- !is.na(nee) & tab[[paste0(col, "_QC")]] %in% 0
    sel <- obs & !is.na(tab$TA_F_MDS) & tab$NIGHT %in% 1 & nee > -5 & nee < 30
    streams[[paste0(prefix, "_", v)]] <- data.frame(time = tm$time, year = year, NEE = nee, sel = sel, day = obs & tab$NIGHT %in% 0)
  }
  attr(streams, "dt") <- tm$dt
  streams
}

#' Read a site's shuttle product, or NULL if it has none
read_shuttle <- function(site_id) {
  path <- tryCatch(raw_product_file(site_id, "SHUTTLE"), error = function(e) NULL)
  if (is.null(path)) return(NULL)
  tab <- read_fluxnet(site_id, "SHUTTLE")
  attr(tab, "file") <- basename(path)
  tab
}

#' The manuscript's stream at an AmeriFlux site, from step 01's ts_base
manuscript_stream <- function(site_id) {
  s1 <- targets::tar_read_raw(paste0("step01_", gsub("-", "_", site_id)), store = store)
  b <- s1$ts_base
  time <- as.POSIXct(format(b$TIMESTAMP, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  out <- list(MS = data.frame(time = time, year = as.POSIXlt(time)$year + 1900L, NEE = b$NEE,
                              sel = b$night_ok, day = !is.na(b$NEE) & b$daytime %in% TRUE))
  attr(out, "dt") <- s1$ts_params$dt_secs
  out
}

agree <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 10 || stats::sd(x) == 0) return(c(n = length(x), r = NA, slope = NA))
  c(n = length(x), r = stats::cor(x, y), slope = unname(stats::coef(stats::lm(y ~ x))[2]))
}

compare_pair <- function(a, b) {
  j <- merge(a, b[, setdiff(names(b), "year")], by = "time", suffixes = c("_a", "_b"))
  year <- j$year
  both <- j$sel_a & j$sel_b
  overlap <- sort(intersect(unique(a$year[a$sel]), unique(b$year[b$sel])))
  own_a <- a$NEE[a$sel & a$year %in% overlap]
  own_b <- b$NEE[b$sel & b$year %in% overlap]
  m <- agree(j$NEE_a[both], j$NEE_b[both])
  day <- j$day_a & j$day_b
  dm <- agree(j$NEE_a[day], j$NEE_b[day])
  summary <- data.frame(
    years = if (length(overlap)) paste(range(overlap), collapse = "-") else NA, n_years = length(overlap),
    n_both = sum(both), mean_ref = mean(j$NEE_a[both]), mean_shuttle = mean(j$NEE_b[both]),
    diff = mean(j$NEE_b[both] - j$NEE_a[both]), r = m[["r"]], slope = m[["slope"]],
    n_ref_own = length(own_a), n_shuttle_own = length(own_b), mean_ref_own = mean(own_a), mean_shuttle_own = mean(own_b),
    day_n = dm[["n"]], day_r = dm[["r"]], day_slope = dm[["slope"]])
  summary$rel_diff <- summary$diff / summary$mean_ref
  summary$diff_own <- summary$mean_shuttle_own - summary$mean_ref_own
  yearly <- do.call(rbind, lapply(overlap, function(y) {
    k <- both & year == y
    oa <- a$NEE[a$sel & a$year == y]
    ob <- b$NEE[b$sel & b$year == y]
    data.frame(year = y, n_both = sum(k), diff = if (any(k)) mean(j$NEE_b[k] - j$NEE_a[k]) else NA,
               mean_ref = if (any(k)) mean(j$NEE_a[k]) else NA, n_ref_own = length(oa), n_shuttle_own = length(ob),
               diff_own = if (length(oa) && length(ob)) mean(ob) - mean(oa) else NA)
  }))
  list(summary = summary, yearly = yearly)
}

monthly <- function(streams) {
  do.call(rbind, lapply(names(streams), function(s) {
    x <- streams[[s]][streams[[s]]$sel, ]
    if (!nrow(x)) return(NULL)
    month <- format(x$time, "%Y-%m")
    data.frame(stream = s, month = sort(unique(month)), nee = as.numeric(tapply(x$NEE, month, mean)),
               n = as.integer(tapply(x$NEE, month, length)))
  }))
}

compare_site <- function(site_id) {
  site <- site_info[site_info$site_ID == site_id, ]
  ameriflux <- site$source == "AmeriFlux_BASE"
  shuttle <- read_shuttle(site_id)
  ref <- if (ameriflux) manuscript_stream(site_id) else
    fluxnet_streams(read_fluxnet_record(site_id, site$source, path_original("SiteData")), "PUB")
  shut <- if (is.null(shuttle)) list() else fluxnet_streams(shuttle, "SHUT")
  base_row <- data.frame(site_ID = site_id, network = if (ameriflux) "AmeriFlux" else "FLUXNET-format",
                         reference_product = if (ameriflux) "AmeriFlux BASE (manuscript u* filter)" else site$source,
                         shuttle_file = if (is.null(shuttle)) NA else attr(shuttle, "file"),
                         dt_ref = attr(ref, "dt") / 60, dt_shuttle = if (is.null(shuttle)) NA else attr(shut, "dt") / 60)
  rows <- list(); years <- list()
  for (v in c("VUT", "CUT")) {
    a_name <- if (ameriflux) "MS" else paste0("PUB_", v)
    b_name <- paste0("SHUT_", v)
    row <- cbind(base_row, comparison = v, reference = a_name, shuttle = b_name)
    status <- if (is.null(shuttle)) "no shuttle product" else if (is.null(shut[[b_name]])) paste("shuttle product has no", v) else
      if (is.null(ref[[a_name]])) paste("published record has no", v) else
        if (!isTRUE(all.equal(attr(ref, "dt"), attr(shut, "dt")))) "time steps differ: own selections only" else "ok"
    if (is.null(ref[[a_name]]) || is.null(shut[[b_name]])) {
      rows[[v]] <- cbind(row, status = status)
      next
    }
    cmp <- compare_pair(ref[[a_name]], shut[[b_name]])
    rows[[v]] <- cbind(row, cmp$summary, status = status)
    if (!is.null(cmp$yearly)) years[[v]] <- cbind(site_ID = site_id, comparison = v, cmp$yearly)
  }
  message(site_id, ": ", paste(vapply(rows, function(r) r$status, ""), collapse = ", "))
  list(summary = dplyr::bind_rows(rows), yearly = dplyr::bind_rows(years),
       monthly = cbind(site_ID = site_id, monthly(c(ref, shut))))
}

results <- parallel::mclapply(sites, function(s) tryCatch(compare_site(s), error = function(e) {
  message(s, ": FAILED: ", conditionMessage(e))
  list(summary = data.frame(site_ID = s, status = paste("failed:", conditionMessage(e))))
}), mc.cores = cores)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
for (k in c("summary", "yearly", "monthly")) {
  tab <- dplyr::bind_rows(lapply(results, `[[`, k))
  utils::write.csv(tab, file.path(out_dir, paste0(if (k == "yearly") "siteyear" else k, ".csv")), row.names = FALSE)
}
message("wrote ", out_dir)
