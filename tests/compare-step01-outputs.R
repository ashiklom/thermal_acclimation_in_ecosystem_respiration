# Compare step-01 outputs in a data-proc root against the published ones:
#
#   <root>/RespirationData/*.{parquet,csv}         vs ../original-data/RespirationData/
#   <root>/TS_RandomForest/*.{parquet,csv}         vs ../original-data/TS_RandomForest/
#   <root>/growing_season_feature_*.{parquet,csv}  vs data/growing_season_feature_*.csv
#
# Newer code writes parquet, older code CSV; the parquet file is used when
# both exist. Parquet tables are compared by value (types and full-precision
# doubles differ from the CSV text by design).
#
# Usage: Rscript tests/compare-step01-outputs.R [ROOT] [--out=summary.csv]
# ROOT defaults to data-proc. Byte-identical files are reported as such; for
# the others the file is read and compared column by column (dimensions,
# names, number of differing values, max absolute difference).

source(file.path("R", "paths.R"))
source(file.path("R", "io.R"))

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) > 0 && !startsWith(args[1], "--")) args[1] else "data-proc"
out_arg <- grep("^--out=", args, value = TRUE)
out <- if (length(out_arg) > 0) sub("^--out=", "", out_arg) else file.path(root, "compare-step01.csv")

pairs <- rbind(
  data.frame(
    kind = "RespirationData",
    new = file.path(root, "RespirationData", list.files(path_original("RespirationData"), pattern = "\\.csv$")),
    old = list.files(path_original("RespirationData"), pattern = "\\.csv$", full.names = TRUE)
  ),
  data.frame(
    kind = "TS_RandomForest",
    new = file.path(root, "TS_RandomForest", list.files(path_original("TS_RandomForest"), pattern = "\\.csv$")),
    old = list.files(path_original("TS_RandomForest"), pattern = "\\.csv$", full.names = TRUE)
  ),
  data.frame(
    kind = "growing_season_feature",
    new = file.path(root, c("growing_season_feature_EuropFlux.csv", "growing_season_feature_AmeriFlux.csv")),
    old = file.path("data", c("growing_season_feature_EuropFlux.csv", "growing_season_feature_AmeriFlux.csv"))
  )
)

compare_pair <- function(new, old) {
  parquet <- sub("\\.csv$", ".parquet", new)
  if (file.exists(parquet)) {
    new <- parquet
  } else if (!file.exists(new)) {
    return(data.frame(status = "missing", detail = ""))
  } else if (identical(unname(tools::md5sum(new)), unname(tools::md5sum(old)))) {
    return(data.frame(status = "identical", detail = ""))
  }
  a <- if (grepl("\\.parquet$", new)) read_table(new) else utils::read.csv(new)
  if ("TIMESTAMP" %in% names(a) && inherits(a$TIMESTAMP, "POSIXct")) a$TIMESTAMP <- format(a$TIMESTAMP, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  b <- utils::read.csv(old)
  if (!identical(names(a), names(b))) {
    return(data.frame(status = "columns", detail = paste0(
      "new-only: ", paste(setdiff(names(a), names(b)), collapse = " "),
      "; old-only: ", paste(setdiff(names(b), names(a)), collapse = " "),
      if (setequal(names(a), names(b))) "; same set, different order" else ""
    )))
  }
  if (nrow(a) != nrow(b)) {
    return(data.frame(status = "rows", detail = sprintf("new %d rows, old %d rows", nrow(a), nrow(b))))
  }
  diffs <- vapply(names(a), function(col) {
    x <- a[[col]]
    y <- b[[col]]
    if (is.numeric(x) && is.numeric(y)) {
      same <- (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & abs(x - y) <= 1e-9 * pmax(1, abs(y)))
      if (all(same)) return(NA_character_)
      sprintf("%s: %d values differ, max |d| = %.3g", col, sum(!same), max(abs(x - y), na.rm = TRUE))
    } else {
      same <- (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & as.character(x) == as.character(y))
      if (all(same)) return(NA_character_)
      sprintf("%s: %d values differ", col, sum(!same))
    }
  }, character(1))
  diffs <- diffs[!is.na(diffs)]
  if (length(diffs) == 0) return(data.frame(status = "equal-values", detail = "text differs, values equal"))
  data.frame(status = "values", detail = paste(diffs, collapse = "; "))
}

result <- do.call(rbind, lapply(seq_len(nrow(pairs)), function(i) {
  cbind(pairs[i, c("kind", "new")], compare_pair(pairs$new[i], pairs$old[i]))
}))
result$file <- basename(result$new)
result$new <- NULL
utils::write.csv(result, out, row.names = FALSE)
print(table(result$kind, result$status))
bad <- result[!result$status %in% c("identical", "equal-values"), ]
if (nrow(bad) > 0) {
  message(nrow(bad), " files differ (details in ", out, "):")
  print(utils::head(bad[, c("file", "status", "detail")], 40), right = FALSE)
}
