# Run workflow step 01 from the raw site data at a given git ref, into a
# scratch output root, for comparison with the published step-01 outputs
# (tests/compare-step01-outputs.R).
#
# Usage (from the repository root):
#   Rscript tests/run-step01.R REF ROOT [--steps=01_02a,01_02b] [--jobs=8]
#
# REF is a git ref, or "worktree" for the working tree. The scripts run from
# REF's source tree (git archive) with THERMAL_DATA_PROC=ROOT. Unless 01_01
# is among --steps, ROOT/TS_RandomForest links to the published random-forest
# soil temperatures: 01_01's random forests do not reproduce exactly on
# current package versions (see refactor-v2-log.md, S2), and 01_02 reads them.
# Steps written as one loop over sites run as a single process; per-site
# runner scripts (with a --sites option) are spread over --jobs processes.

stopifnot(requireNamespace("parallel"))

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 2)
ref <- args[1]
root <- normalizePath(args[2], mustWork = FALSE)
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) > 0) sub(paste0("^--", name, "="), "", hit) else default
}
steps <- strsplit(opt("steps", "01_02a,01_02b"), ",")[[1]]
jobs <- as.integer(opt("jobs", "8"))
scripts <- c(
  "01_01" = "workflows/01_01_estimate_soil_temperature_at_some_sites.R",
  "01_02a" = "workflows/01_02a_filter_high_quality_night_respiration_EuroFlux.R",
  "01_02b" = "workflows/01_02b_filter_high_quality_night_respiration_AmeriFlux.R"
)
stopifnot(all(steps %in% names(scripts)))

repo <- normalizePath(".")
original_data <- normalizePath(file.path("..", "original-data"))
dir.create(root, recursive = TRUE, showWarnings = FALSE)
src <- if (ref == "worktree") repo else {
  dir <- file.path(root, "src")
  dir.create(dir, showWarnings = FALSE)
  stopifnot(system(sprintf("git archive %s | tar -x -C %s", shQuote(ref), shQuote(dir))) == 0)
  dir
}
if (!"01_01" %in% steps && !file.exists(file.path(root, "TS_RandomForest"))) {
  file.symlink(file.path(original_data, "TS_RandomForest"), file.path(root, "TS_RandomForest"))
}

site_info <- utils::read.csv(file.path(src, "data", "site_info.csv"))
step_sites <- list(
  "01_01" = site_info$site_ID[site_info$estimate_Ts == "YES"],
  "01_02a" = site_info$site_ID[site_info$source != "AmeriFlux_BASE"],
  "01_02b" = site_info$site_ID[site_info$source == "AmeriFlux_BASE"]
)
run_step <- function(step) {
  script <- file.path(src, scripts[[step]])
  per_site <- any(grepl("--sites=", readLines(script), fixed = TRUE))
  site_sets <- if (per_site) {
    s <- step_sites[[step]]
    unname(split(s, rep_len(seq_len(min(jobs, length(s))), length(s))))
  } else {
    list(NULL)
  }
  status <- parallel::mclapply(seq_along(site_sets), function(i) {
    log <- file.path(root, sprintf("%s-%d.log", step, i))
    system(sprintf(
      "cd %s && THERMAL_DATA_PROC=%s THERMAL_ORIGINAL_DATA=%s Rscript %s %s > %s 2>&1",
      shQuote(src), shQuote(root), shQuote(original_data), shQuote(script),
      if (is.null(site_sets[[i]])) "" else shQuote(paste0("--sites=", paste(site_sets[[i]], collapse = ","))),
      shQuote(log)
    ))
  }, mc.cores = jobs)
  if (any(unlist(status) != 0)) stop(step, " failed; see ", root, "/", step, "-*.log")
  if (per_site && any(grepl("--combine-only", readLines(script), fixed = TRUE))) {
    status <- system(sprintf(
      "cd %s && THERMAL_DATA_PROC=%s THERMAL_ORIGINAL_DATA=%s Rscript %s --combine-only > %s 2>&1",
      shQuote(src), shQuote(root), shQuote(original_data), shQuote(script), shQuote(file.path(root, paste0(step, "-combine.log")))
    ))
    if (status != 0) stop(step, " --combine-only failed")
  }
  message(step, " done")
}
# 01_01 feeds 01_02a/b; 01_02a and 01_02b are independent of each other.
if ("01_01" %in% steps) run_step("01_01")
rest <- intersect(c("01_02a", "01_02b"), steps)
invisible(parallel::mclapply(rest, run_step, mc.cores = length(rest)))
message("step 01 outputs in ", root)
