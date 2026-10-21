# Compare the 04_02 script with scenario_site() (R/step04-scenario.R) in dry
# run (tests/dry-run.R: every brms::brm() call recorded, not sampled) on a few
# sites: identical brm inputs (prior values to 1e-12) and identical output rows.
#
# Usage (from the repository root, with step-01 parquet tables in data-proc/):
#   Rscript tests/check-step04-dryrun.R [--sites=AU-Tum,US-Tw1,US-ICt]

args <- commandArgs(trailingOnly = TRUE)
hit <- grep("^--sites=", args, value = TRUE)
sites <- strsplit(if (length(hit)) sub("^--sites=", "", hit) else "AU-Tum,US-Tw1,US-ICt,DE-RuC,US-GLE", ",")[[1]]
repo <- normalizePath(".")
work <- file.path(repo, "data-proc", "checks", paste0("dryrun04-", format(Sys.time(), "%Y%m%d-%H%M%S")))
root <- file.path(work, "proc")
dir.create(file.path(root, "RespirationData"), recursive = TRUE)
for (s in sites) for (k in c("ac", "nightNEE")) {
  f <- paste0(s, "_", k, ".parquet")
  file.symlink(file.path(repo, "data-proc", "RespirationData", f), file.path(root, "RespirationData", f))
}
for (f in c("growing_season_feature_EuropFlux.parquet", "growing_season_feature_AmeriFlux.parquet")) {
  file.symlink(file.path(repo, "data-proc", f), file.path(root, f))
}
file.copy(file.path(repo, "data", "acclimation_data.csv"), root)
file.copy(file.path(repo, "data", "Tmin_month_ssp245_wc.csv"), root)

# the function version, as a script
driver <- file.path(work, "functions.R")
writeLines(c(
  'source(file.path("R", "paths.R")); source(file.path("R", "io.R")); source(file.path("R", "step02-tas.R")); source(file.path("R", "step04-scenario.R"))',
  'feature_gs <- read_growing_season_features()',
  'acclimation <- read.csv(path_proc("acclimation_data.csv"))',
  'tmin <- read.csv(path_proc("Tmin_month_ssp245_wc.csv"))',
  'files <- list.files(path = path_proc("RespirationData"), pattern = "_ac.parquet$")',
  'rows <- do.call(rbind, lapply(substring(files, 1, 6), function(s) scenario_site(s, read_step01_table(s, "ac"), read_step01_table(s, "nightNEE"),',
  '  feature_gs$gStart[feature_gs$site_ID == s], feature_gs$gEnd[feature_gs$site_ID == s], tmin[tmin$site_ID == s, ], acclimation$TAS_tot[acclimation$site_ID == s])))',
  'write.csv(scenario_data(acclimation, tmin, rows), path_proc("acclimation_data_future_ssp245.csv"), row.names = FALSE)'
), driver)

runs <- list(script = file.path(repo, "workflows", "04_02_thermal_response_effect_on_future_ecosystem_respiration.R"), functions = driver)
for (v in names(runs)) {
  status <- system(sprintf("cd %s && TAS_DRY_RUN=%s THERMAL_DATA_PROC=%s Rscript -e %s > %s 2>&1",
                           shQuote(repo), shQuote(file.path(work, v)), shQuote(root),
                           shQuote(sprintf("source('%s'); source('%s')", file.path(repo, "tests", "dry-run.R"), runs[[v]])),
                           shQuote(file.path(work, paste0(v, ".log")))))
  if (status != 0) stop(v, " failed; see ", work)
  file.rename(file.path(root, "acclimation_data_future_ssp245.csv"), file.path(work, paste0(v, "-output.csv")))
}

read_calls <- function(v) {
  calls <- do.call(rbind, lapply(list.files(file.path(work, v), full.names = TRUE), utils::read.delim, colClasses = "character"))
  calls[order(calls$site, as.integer(calls$seq)), setdiff(names(calls), c("seed", "priors"))]
}
prior_values <- function(v) {
  calls <- do.call(rbind, lapply(list.files(file.path(work, v), full.names = TRUE), utils::read.delim, colClasses = "character"))
  calls <- calls[order(calls$site, as.integer(calls$seq)), ]
  lapply(strsplit(calls$priors, ";"), function(e) { kv <- strsplit(e, "="); v <- as.numeric(vapply(kv, `[`, "", 2)); names(v) <- vapply(kv, `[`, "", 1); v[!startsWith(names(v), "sigma")] })
}
a <- read_calls("script"); b <- read_calls("functions")
rownames(a) <- rownames(b) <- NULL
pa <- prior_values("script"); pb <- prior_values("functions")
priors_ok <- length(pa) == length(pb) && all(mapply(function(x, y) identical(names(x), names(y)) && isTRUE(all.equal(x, y, tolerance = 1e-12)), pa, pb))
out_a <- utils::read.csv(file.path(work, "script-output.csv")); out_b <- utils::read.csv(file.path(work, "functions-output.csv"))
out_a <- out_a[out_a$site_ID %in% sites, ]; out_b <- out_b[out_b$site_ID %in% sites, ]
rownames(out_a) <- rownames(out_b) <- NULL
message("brm calls: ", nrow(a), " vs ", nrow(b), "; other fields identical: ", identical(a, b), "; prior values equal: ", priors_ok)
message("output rows identical: ", isTRUE(all.equal(out_a, out_b, tolerance = 1e-12)))
if (!identical(a, b) || !priors_ok || !isTRUE(all.equal(out_a, out_b, tolerance = 1e-12))) stop("04_02 transcription differs; see ", work)
message("OK: scenario_site() matches the 04_02 script for ", paste(sites, collapse = ", "))
