# Compare per-site step-02 results with a reference.
#
# Usage (from the repository root):
#   Rscript tests/compare-step02.R ROOT [--ref=published|OTHER_ROOT] [--threshold=1e-3]
#
# ROOT (and OTHER_ROOT) hold TAS_sites/<model>/<site>.rds from the step-02
# runners (e.g. tests/run-step02.R). The reference "published" is the tracked
# data/outcome_*.csv tables. For each model, prints a summary and writes
# ROOT/compare-<model>.csv (see R/compare-tas.R).

source(file.path("R", "compare-tas.R"))

args <- commandArgs(trailingOnly = TRUE)
root <- args[1]
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) > 0) sub(paste0("^--", name, "="), "", hit) else default
}
ref <- opt("ref", "published")
threshold <- as.numeric(opt("threshold", "1e-3"))
published <- list(
  total = c("data/outcome_temp.csv", "data/outcome_siteyear_temp.csv"),
  direct = c("data/outcome_temp_water_gpp.csv", "data/outcome_siteyear_temp_water_gpp.csv")
)

read_root <- function(dir, model) {
  files <- list.files(file.path(dir, "TAS_sites", model), pattern = "\\.rds$", full.names = TRUE)
  results <- lapply(files, readRDS)
  list(
    outcome = do.call(rbind, lapply(results, `[[`, "outcome")),
    siteyear = do.call(rbind, lapply(results, `[[`, "siteyear")),
    profile = unique(unlist(lapply(results, `[[`, "fit_profile")))
  )
}

for (model in c("total", "direct")) {
  new <- read_root(root, model)
  if (is.null(new$outcome)) next
  if (ref == "published") {
    r <- list(outcome = utils::read.csv(published[[model]][1]), siteyear = utils::read.csv(published[[model]][2]))
  } else {
    r <- read_root(ref, model)
  }
  r$outcome <- r$outcome[r$outcome$site_ID %in% new$outcome$site_ID, ]
  r$siteyear <- r$siteyear[r$siteyear$site_ID %in% new$outcome$site_ID, ]
  cmp <- compare_tas(r$outcome, new$outcome, r$siteyear, new$siteyear, threshold = threshold)
  utils::write.csv(cmp, file.path(root, paste0("compare-", model, ".csv")), row.names = FALSE)
  message(summarise_tas_comparison(cmp, paste(model, "vs", ref, if (!is.null(new$profile)) paste0("(", paste(new$profile, collapse = ","), " profile)") else "")))
  print(cmp[, intersect(c("site_ID", "TAS_ref", "TAS_new", "dTAS", "class_ref", "class_new", "n_siteyears_ref", "n_siteyears_new", "n_fitted_ref", "n_fitted_new"), names(cmp))], row.names = FALSE)
}
