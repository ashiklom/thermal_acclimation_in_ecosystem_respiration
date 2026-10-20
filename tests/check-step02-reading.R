# Check that the data-reading part of workflow step 02 (02_01a and 02_01b)
# loads exactly the same objects as the same scripts at a reference git ref.
#
# Usage (from the repository root):
#   Rscript tests/check-step02-reading.R [REF] [--sites=US-Kon,FI-Sod]
#
# REF defaults to `v2-s00-base`, the published code. For REF the absolute
# `dir_rawdata` is pointed at ../original-data, so REF reads the published
# step-01 outputs (original-data/RespirationData and data/*feature*.csv),
# while the working tree reads whatever is in data-proc/.
#
# Each script is instrumented in a temporary copy: every loop iteration saves
# the data it has assembled (after the SWC filter, the ERA5 join and the
# TS ~ TA substitution, just before the daytime-NEE block) and skips to the
# next site; the script quits before writing any outputs. No model is fit.

stopifnot(requireNamespace("parallel"))

args <- commandArgs(trailingOnly = TRUE)
ref <- if (length(args) > 0 && !startsWith(args[1], "--")) args[1] else "v2-s00-base"
sites_arg <- grep("^--sites=", args, value = TRUE)
sites <- if (length(sites_arg) > 0) strsplit(sub("^--sites=", "", sites_arg), ",")[[1]] else character()

scripts <- c(
  total = "workflows/02_01a_estimate_total_TAS_use_moving_window.R",
  direct = "workflows/02_01b_estimate_direct_TAS_use_moving_window.R"
)
marker <- "# calculate daily daytime NEE and rolling average"
original_data <- normalizePath(file.path("..", "original-data"))
work <- tempfile("check-step02-")
dir.create(work)

instrument <- function(lines, out_dir) {
  at_marker <- which(trimws(lines) == marker)
  at_write <- which(grepl("^write\\.csv\\(outcome,", lines))
  stopifnot(length(at_marker) == 1, length(at_write) >= 1)
  capture <- c(
    "  saveRDS(list(",
    "    night = a_measure_night_complete, ac = ac,",
    "    site = site_info[id, ], feature_gs = feature_gs,",
    "    site_TS_issue = site_TS_issue",
    sprintf("  ), file.path(%s, paste0(name_site, '.rds')))", deparse(out_dir)),
    "  next"
  )
  lines <- append(lines, "quit(save = 'no')", after = at_write[1] - 1)
  lines <- append(lines, capture, after = at_marker - 1)
  if (length(sites) > 0) {
    loop <- which(grepl("^for \\(id in 1:nrow\\(site_info\\)\\)", lines))
    stopifnot(length(loop) == 1)
    lines[loop] <- sprintf(
      "for (id in which(site_info$site_ID %%in%% %s)) {",
      paste(deparse(sites), collapse = "")
    )
  }
  lines
}

prepare <- function(version, model) {
  path <- scripts[[model]]
  lines <- if (version == "ref") {
    system2("git", c("show", paste0(ref, ":", path)), stdout = TRUE)
  } else {
    readLines(path)
  }
  if (version == "ref") {
    lines <- sub("^dir_rawdata <- .*$", sprintf("dir_rawdata <- '%s'", original_data), lines)
  }
  out_dir <- file.path(work, version, model)
  dir.create(out_dir, recursive = TRUE)
  script <- file.path(work, paste0(version, "-", model, ".R"))
  writeLines(instrument(lines, out_dir), script)
  list(script = script, out_dir = out_dir, log = paste0(script, ".log"))
}

runs <- list()
for (version in c("ref", "head")) {
  for (model in names(scripts)) runs[[paste(version, model)]] <- prepare(version, model)
}

status <- parallel::mclapply(runs, function(r) {
  system2("Rscript", r$script, stdout = r$log, stderr = r$log)
}, mc.cores = length(runs))
failed <- names(runs)[unlist(status) != 0]
if (length(failed) > 0) {
  for (f in failed) message(f, " failed; log tail:\n", paste(utils::tail(readLines(runs[[f]]$log), 20), collapse = "\n"))
  stop("Instrumented script(s) failed: ", paste(failed, collapse = ", "))
}

n_bad <- 0
for (model in names(scripts)) {
  ref_files <- sort(list.files(runs[[paste("ref", model)]]$out_dir))
  head_files <- sort(list.files(runs[[paste("head", model)]]$out_dir))
  if (!identical(ref_files, head_files)) {
    message(model, ": different site sets")
    n_bad <- n_bad + 1
  }
  for (f in intersect(ref_files, head_files)) {
    a <- readRDS(file.path(runs[[paste("ref", model)]]$out_dir, f))
    b <- readRDS(file.path(runs[[paste("head", model)]]$out_dir, f))
    if (!identical(a, b)) {
      n_bad <- n_bad + 1
      message(model, " ", f, ": not identical (all.equal: ", paste(all.equal(a, b), collapse = "; "), ")")
    }
  }
  n_compared <- length(intersect(ref_files, head_files))
  message(model, ": compared ", n_compared, " sites")
  if (n_compared == 0 || (length(sites) > 0 && n_compared != length(sites))) {
    n_bad <- n_bad + 1
    message(model, ": expected ", if (length(sites) > 0) length(sites) else "> 0", " sites")
  }
}
if (n_bad > 0) stop(n_bad, " mismatches against ", ref)
message("OK: step-02 inputs identical to ", ref)
