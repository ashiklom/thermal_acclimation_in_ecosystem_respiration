# Daily means of every temperature column of every site, for the analysis
# website's site explorer: the soil-temperature columns (data-proc/ts-columns/
# <site>.parquet, R/ts-columns.R) and the air-temperature columns (the
# ta_candidate_values_<site> targets, R/ta-columns.R), each averaged over the
# whole day and over its nighttime records (step 01's `daytime` FALSE, the
# nighttime the fits use). A day's mean is NA where fewer than half of that
# period's records have a value.
#
# Reads only: the targets store (tar_read_raw, no tar_make) and data-proc/.
#
# Usage (from the repository root, after a full pipeline run):
#   Rscript scripts/export-temperature-daily.R [--out=data-proc/temperature_daily.parquet]
#
# Writes one long table: site_ID, date, period ("day" or "night") and one
# column per temperature column (NA where the site lacks it), rounded to 0.01 °C.

source(file.path("R", "paths.R"))
source(file.path("R", "io.R"))

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) > 0) sub(paste0("^--", name, "="), "", hit) else default
}
out <- opt("out", path_proc("temperature_daily.parquet"))

objects <- list.files(file.path(targets::tar_config_get("store"), "objects"))
sites <- sub("^ta_candidate_values_", "", grep("^ta_candidate_values_", objects, value = TRUE))
ts_dir <- path_proc("ts-columns")

daily <- function(x, date, keep) {
  dt <- data.table::as.data.table(x[keep, , drop = FALSE])
  cols <- names(dt)
  dt$date <- date[keep]
  dt[, lapply(.SD, function(v) if (sum(!is.na(v)) * 2 >= length(v)) round(mean(v, na.rm = TRUE), 2) else NA_real_),
     by = date, .SDcols = cols]
}

tables <- lapply(sites, function(target_site) {
  base <- targets::tar_read_raw(paste0("step01_", target_site))$ts_base
  site <- gsub("_", "-", target_site)
  ta <- targets::tar_read_raw(paste0("ta_candidate_values_", target_site))
  values <- as.data.frame(ta$values)
  stopifnot(nrow(values) == nrow(base))
  ts_path <- file.path(ts_dir, paste0(site, ".parquet"))
  if (file.exists(ts_path)) {
    ts <- read_table(ts_path)
    stopifnot(nrow(ts) == nrow(base), all(ts$TIMESTAMP == base$TIMESTAMP))
    ts_cols <- grep("^TS_", names(ts), value = TRUE)
    ts_cols <- setdiff(ts_cols[!grepl("_keep$", ts_cols)], c("TS_truth", "TS_real"))
    values <- cbind(ts[, ts_cols, drop = FALSE], values)
  }
  date <- as.Date(format(base$TIMESTAMP, "%Y-%m-%d"))
  night <- !is.na(base$daytime) & !base$daytime
  rows <- rbind(cbind(period = "day", daily(values, date, rep(TRUE, nrow(base)))),
                cbind(period = "night", daily(values, date, night)), fill = TRUE)
  rows$site_ID <- site
  message(site, ": ", nrow(rows), " rows, ", ncol(values), " columns")
  rows
})

result <- data.table::rbindlist(tables, use.names = TRUE, fill = TRUE)
data.table::setcolorder(result, c("site_ID", "date", "period"))
write_table(as.data.frame(result), out)
message("wrote ", out, ": ", nrow(result), " rows, ", length(sites), " sites")
