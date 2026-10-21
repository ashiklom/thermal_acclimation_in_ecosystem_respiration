# Fit workflow step 02 (total and direct TAS) at a given git ref, for some or
# all sites, into a scratch output root.
#
# Usage (from the repository root):
#   Rscript tests/run-step02.R REF ROOT [--sites=A,B] [--models=total,direct]
#                              [--jobs=4] [--profile=full|fast] [--inputs=data-proc]
#
# REF is a git ref or "worktree". The step-02 scripts run from REF's source
# tree with THERMAL_DATA_PROC=ROOT; ROOT links the step-01 outputs from
# --inputs (default data-proc) and shares data-proc/stan-cache, so models are
# compiled once per machine. Both models are compiled first (--compile-only),
# then sites are fitted in --jobs parallel processes of 4 cores each. Results
# land in ROOT/TAS_sites/<model>/<site>.rds; compare them with
# tests/compare-step02.R.

stopifnot(requireNamespace("parallel"))

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 2)
ref <- args[1]
dir.create(args[2], recursive = TRUE, showWarnings = FALSE)
root <- normalizePath(args[2])
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) > 0) sub(paste0("^--", name, "="), "", hit) else default
}
repo <- normalizePath(".")
site_info <- utils::read.csv(file.path(repo, "data", "site_info.csv"))
sites <- strsplit(opt("sites", paste(site_info$site_ID, collapse = ",")), ",")[[1]]
stopifnot(all(sites %in% site_info$site_ID))
models <- strsplit(opt("models", "total,direct"), ",")[[1]]
jobs <- as.integer(opt("jobs", "4"))
profile <- opt("profile", "full")
inputs <- normalizePath(opt("inputs", "data-proc"))
scripts <- c(
  total = "workflows/02_01a_estimate_total_TAS_use_moving_window.R",
  direct = "workflows/02_01b_estimate_direct_TAS_use_moving_window.R"
)
original_data <- normalizePath(file.path("..", "original-data"))

src <- if (ref == "worktree") repo else {
  dir <- file.path(root, "src")
  dir.create(dir, showWarnings = FALSE)
  stopifnot(system(sprintf("git archive %s | tar -x -C %s", shQuote(ref), shQuote(dir))) == 0)
  dir
}
for (f in c("RespirationData", paste0("growing_season_feature_", rep(c("EuropFlux", "AmeriFlux"), 2), rep(c(".csv", ".parquet"), each = 2)))) {
  if (file.exists(file.path(inputs, f)) && !file.exists(file.path(root, f))) file.symlink(file.path(inputs, f), file.path(root, f))
}
dir.create(file.path(repo, "data-proc", "stan-cache"), recursive = TRUE, showWarnings = FALSE)
if (!file.exists(file.path(root, "stan-cache"))) {
  file.symlink(file.path(repo, "data-proc", "stan-cache"), file.path(root, "stan-cache"))
}

run <- function(script, extra, log) {
  system(sprintf(
    "cd %s && THERMAL_DATA_PROC=%s THERMAL_ORIGINAL_DATA=%s TAS_FIT_PROFILE=%s Rscript %s %s > %s 2>&1",
    shQuote(src), shQuote(root), shQuote(original_data), shQuote(profile),
    shQuote(file.path(src, script)), extra, shQuote(log)
  ))
}

prep <- "workflows/02_00_prepare_TAS_inputs.R"
if (file.exists(file.path(src, prep))) {
  chunks <- unname(split(sites, rep_len(seq_len(min(jobs * 4, length(sites))), length(sites))))
  prep_status <- parallel::mclapply(seq_along(chunks), function(i) {
    run(prep, shQuote(paste0("--sites=", paste(chunks[[i]], collapse = ","))), file.path(root, sprintf("prep-%d.log", i)))
  }, mc.cores = jobs * 4)
  if (any(unlist(prep_status) != 0)) stop("input preparation failed; see ", root, "/prep-*.log")
}

if (any(grepl("--compile-only", readLines(file.path(src, scripts[["total"]])), fixed = TRUE))) {
  stopifnot(run(scripts[["total"]], "--compile-only", file.path(root, "compile.log")) == 0)
}

tasks <- expand.grid(site = sites, model = models, stringsAsFactors = FALSE)
started <- Sys.time()
status <- parallel::mclapply(seq_len(nrow(tasks)), function(i) {
  t <- tasks[i, ]
  t0 <- Sys.time()
  rc <- run(scripts[[t$model]], shQuote(paste0("--sites=", t$site)), file.path(root, sprintf("%s-%s.log", t$model, t$site)))
  data.frame(site = t$site, model = t$model, status = rc,
             minutes = round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2))
}, mc.cores = jobs, mc.preschedule = FALSE)
timing <- do.call(rbind, status)
utils::write.csv(timing, file.path(root, "timing.csv"), row.names = FALSE)
print(timing)
message("total wall time: ", round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1), " min")
if (any(timing$status != 0)) stop("some fits failed; see logs in ", root)
