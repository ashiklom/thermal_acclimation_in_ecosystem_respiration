# Save a run's TAS results in the layout the analysis website reads:
#
#   results/<id>/outcome_total.csv     outcome_direct.csv
#   results/<id>/siteyear_total.csv    siteyear_direct.csv
#   results/<id>/manifest.json         what produced them
#
# and, where the run wrote them, its TAS input summary and growing-season
# features (tas_inputs_summary.csv, growing_season_feature.csv), the
# soil-temperature column summary, quality, ranking and selection
# (ts_*.csv) and every column's TAS (ts_outcome_*.csv, ts_siteyear_*.csv),
# and its step 03-04 tables (step03-04/<fit set>/, step03-04-ts/<column>/).
#
# Usage (from the repository root):
#   Rscript scripts/save-results.R --from=DIR --id=ID --label=LABEL --stage=S9
#       --description=TEXT [--sha=GITSHA] [--data=TEXT] [--machine=TEXT]
#
# DIR holds TAS_sites/<model>/<site>.rds (step-02 runners, tests/run-step02.R)
# or the pipeline's combined outcome_*.csv tables.

source(file.path("R", "paths.R"))
source(file.path("R", "step02-tas.R"))

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) > 0) sub(paste0("^--", name, "="), "", hit) else default
}
from <- opt("from")
id <- opt("id")
stopifnot(!is.null(from), !is.null(id))
out <- file.path("results", id)
dir.create(out, recursive = TRUE, showWarnings = FALSE)

site_order <- utils::read.csv(file.path("data", "site_info.csv"))$site_ID
files_out <- c(total = "outcome_temp.csv", direct = "outcome_temp_water_gpp.csv")
siteyear_out <- c(total = "outcome_siteyear_temp.csv", direct = "outcome_siteyear_temp_water_gpp.csv")
profiles <- character()
sites <- character()
for (model in c("total", "direct")) {
  site_dir <- file.path(from, "TAS_sites", model)
  if (dir.exists(site_dir)) {
    files <- list.files(site_dir, pattern = "\\.rds$", full.names = TRUE)
    files <- files[order(match(sub("\\.rds$", "", basename(files)), site_order))]
    results <- lapply(files, readRDS)
    combined <- combine_tas_sites(results)
    profiles <- c(profiles, unlist(lapply(results, `[[`, "fit_profile")))
  } else {
    combined <- list(outcome = utils::read.csv(file.path(from, files_out[[model]])),
                     siteyear = utils::read.csv(file.path(from, siteyear_out[[model]])))
  }
  utils::write.csv(combined$outcome, file.path(out, paste0("outcome_", model, ".csv")), row.names = FALSE)
  utils::write.csv(combined$siteyear, file.path(out, paste0("siteyear_", model, ".csv")), row.names = FALSE)
  sites <- union(sites, combined$outcome$site_ID)
}

for (f in c("tas_inputs_summary.csv", "growing_season_feature.csv", "ts_columns_summary.csv", "ts_quality.csv",
            "ts_ranking.csv", "ts_methods_selected.csv", "ts_outcome_total.csv", "ts_outcome_direct.csv",
            "ts_siteyear_total.csv", "ts_siteyear_direct.csv", "ts_screen.csv", "ts_screen_yearly.csv",
            "ts_screen_windows.csv", "ts_screen_diurnal.csv", "ts_screen_validation.csv")) {
  if (file.exists(file.path(from, f))) file.copy(file.path(from, f), file.path(out, f), overwrite = TRUE)
}
for (d in c("step03-04", "step03-04-ts")) {
  if (dir.exists(file.path(from, d))) file.copy(file.path(from, d), out, recursive = TRUE, overwrite = TRUE)
}

manifest <- list(
  id = id,
  label = opt("label", id),
  stage = opt("stage", ""),
  description = opt("description", ""),
  git_sha = opt("sha", system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE)),
  fit_profile = if (length(profiles)) unique(profiles) else opt("profile", "full"),
  data = opt("data", "published step-01 outputs"),
  machine = opt("machine", Sys.info()[["nodename"]]),
  n_sites = length(sites),
  created = format(Sys.time(), "%Y-%m-%d %H:%M %Z")
)
jsonlite::write_json(manifest, file.path(out, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
message("saved ", length(sites), " sites to ", out)
