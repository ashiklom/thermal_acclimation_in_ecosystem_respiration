# How well step 03's random forests, fitted to the manuscript's sites only,
# predict the TAS of the sites added later (the site table's `added`): for
# each steps 03-04 table set of a saved run (results/<id>/step03-04/<set>/
# and step03-04-ts/<column>/), forests of each TAS type (direct, total,
# apparent) on ELEV, MATA, LAI, SOC and warm_rate, with driver_analysis()'s
# settings (mtry 1, nodesize 30, 500 trees), fitted to the manuscript's sites
# and evaluated on the added ones: r2 (1 - SSE/SST about the added sites'
# mean), correlation, RMSE, bias; and the sites' TAS distributions.
#
# Usage (from the repository root, after scripts/save-results.R):
#   Rscript scripts/new-sites-effect.R --id=<run id> [--site-info=data/site_info_new.csv]
# Writes results/<id>/new_sites_effect.csv (one row per table set and TAS type).

source(file.path("R", "pipeline.R"))
source(file.path("R", "step03-drivers.R"))

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) > 0) sub(paste0("^--", name, "="), "", hit) else default
}
id <- opt("id")
stopifnot(!is.null(id))
site_table <- read_site_info(opt("site-info", file.path("data", "site_info_new.csv")))
added <- site_table$site_ID[site_table$added != ""]

dirs <- c(list.dirs(file.path("results", id, "step03-04"), recursive = FALSE),
          list.dirs(file.path("results", id, "step03-04-ts"), recursive = FALSE))
types <- c(TAS_direct = "TAS", TAS_tot = "TAS_tot", TAS_app = "TAS_app")
predictors <- c("ELEV", "MATA", "LAI", "SOC", "warm_rate")
rows <- list()
for (d in dirs) {
  f <- file.path(d, "acclimation_data.csv")
  if (!file.exists(f)) next
  data <- driver_sites(utils::read.csv(f))
  data$TAS_app <- data$TAS_tot - data$TAS
  train <- data[!data$site_ID %in% added, ]
  test <- data[data$site_ID %in% added, ]
  for (type in names(types)) {
    y <- types[[type]]
    set.seed(985)
    rf <- randomForest::randomForest(x = train[, predictors], y = train[[y]], mtry = 1, nodesize = 30, ntree = 500)
    pred <- if (nrow(test)) stats::predict(rf, test[, predictors]) else numeric()
    obs <- test[[y]]
    rows[[length(rows) + 1]] <- data.frame(
      set = paste0(basename(dirname(d)), "/", basename(d)), type = type, n_original = nrow(train), n_added = nrow(test),
      r2 = if (length(obs) > 2) 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2) else NA_real_,
      r = if (length(obs) > 2) stats::cor(obs, pred) else NA_real_,
      rmse = if (length(obs)) sqrt(mean((obs - pred)^2)) else NA_real_,
      bias = if (length(obs)) mean(pred - obs) else NA_real_,
      oob_r2_original = utils::tail(rf$rsq, 1),
      mean_original = mean(train[[y]]), sd_original = stats::sd(train[[y]]),
      mean_added = if (length(obs)) mean(obs) else NA_real_, sd_added = if (length(obs) > 1) stats::sd(obs) else NA_real_
    )
  }
}
out <- do.call(rbind, rows)
path <- file.path("results", id, "new_sites_effect.csv")
utils::write.csv(out, path, row.names = FALSE)
message(nrow(out), " rows -> ", path)
