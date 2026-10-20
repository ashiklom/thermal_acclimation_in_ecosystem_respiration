# Compare the step-02 fit-input fingerprints of the loop scripts 02_01a/b at
# a reference git ref with those of the working tree.
#
# Usage (from the repository root):
#   Rscript tests/check-step02-dryrun.R [REF] [--sites=US-Kon,FI-Sod]
#
# Both versions run in dry-run mode (R/dry-run.R from the working tree):
# every brms::brm() call is recorded instead of sampled. The check requires
# identical call logs (site, order, data hash, priors, sampler settings) and
# identical outcome tables. REF reads the published step-01 outputs from
# ../original-data and data/; the working tree reads data-proc/.

stopifnot(requireNamespace("parallel"))

args <- commandArgs(trailingOnly = TRUE)
ref <- if (length(args) > 0 && !startsWith(args[1], "--")) args[1] else "v2-s00-base"
sites_arg <- grep("^--sites=", args, value = TRUE)
sites <- if (length(sites_arg) > 0) strsplit(sub("^--sites=", "", sites_arg), ",")[[1]] else character()

scripts <- c(
  total = "workflows/02_01a_estimate_total_TAS_use_moving_window.R",
  direct = "workflows/02_01b_estimate_direct_TAS_use_moving_window.R"
)
original_data <- normalizePath(file.path("..", "original-data"))
work <- tempfile("check-dryrun-")
dir.create(work)

instrument <- function(lines) {
  anchor <- max(grep('^(rm\\(list ?= ?ls\\(\\)\\)|source\\(file\\.path\\("R", "paths\\.R"\\)\\))\\s*$', lines))
  stopifnot(is.finite(anchor))
  lines <- append(lines, 'source(file.path("R", "dry-run.R"))', after = anchor)
  if (length(sites) > 0) {
    loop <- grep("^for \\(id in 1:nrow\\(site_info\\)\\)", lines)
    stopifnot(length(loop) == 1)
    lines[loop] <- sprintf("for (id in which(site_info$site_ID %%in%% %s)) {", paste(deparse(sites), collapse = ""))
  }
  lines
}

prepare <- function(version, model) {
  path <- scripts[[model]]
  lines <- if (version == "ref") system2("git", c("show", paste0(ref, ":", path)), stdout = TRUE) else readLines(path)
  if (version == "ref") lines <- sub("^dir_rawdata <- .*$", sprintf("dir_rawdata <- '%s'", original_data), lines)
  script <- file.path(work, paste0(version, "-", model, ".R"))
  writeLines(instrument(lines), script)
  list(script = script, out_dir = file.path(work, version, model), log = paste0(script, ".log"))
}

runs <- list()
for (version in c("ref", "head")) for (model in names(scripts)) runs[[paste(version, model)]] <- prepare(version, model)

status <- parallel::mclapply(runs, function(r) {
  system2("Rscript", r$script, stdout = r$log, stderr = r$log, env = paste0("TAS_DRY_RUN=", r$out_dir))
}, mc.cores = length(runs))
failed <- names(runs)[unlist(status) != 0]
if (length(failed) > 0) {
  for (f in failed) message(f, " failed; log tail:\n", paste(utils::tail(readLines(runs[[f]]$log), 20), collapse = "\n"))
  stop("Instrumented script(s) failed: ", paste(failed, collapse = ", "))
}

n_bad <- 0
for (model in names(scripts)) {
  a_dir <- runs[[paste("ref", model)]]$out_dir
  b_dir <- runs[[paste("head", model)]]$out_dir
  files <- sort(union(list.files(a_dir), list.files(b_dir)))
  for (f in files) {
    a <- utils::read.delim(file.path(a_dir, f), sep = if (grepl("\\.tsv$", f)) "\t" else ",", colClasses = "character")
    b <- utils::read.delim(file.path(b_dir, f), sep = if (grepl("\\.tsv$", f)) "\t" else ",", colClasses = "character")
    if (!identical(a, b)) {
      n_bad <- n_bad + 1
      message(model, " ", f, ": DIFFERENT (", nrow(a), " vs ", nrow(b), " rows)")
      if (identical(dim(a), dim(b)) && identical(names(a), names(b))) {
        bad_rows <- which(rowSums(a != b, na.rm = TRUE) > 0 | (is.na(a) != is.na(b)) |> rowSums() > 0)
        print(utils::head(rbind(cbind(version = "ref", a[bad_rows, ]), cbind(version = "head", b[bad_rows, ])), 10))
      }
    } else {
      message(model, " ", f, ": identical (", nrow(a), " rows)")
    }
  }
}
saveRDS(runs, file.path(work, "runs.rds"))
message("Dry-run outputs in ", work)
if (n_bad > 0) stop(n_bad, " differences against ", ref)
message("OK: step-02 fit inputs identical to ", ref)
