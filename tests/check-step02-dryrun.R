# Compare the step-02 fit-input fingerprints of a reference git ref with
# those of the working tree.
#
# Usage (from the repository root):
#   Rscript tests/check-step02-dryrun.R [REF] [--sites=US-Kon,FI-Sod] [--jobs=8]
#
# Both versions run 02_01a and 02_01b in dry-run mode (R/dry-run.R from the
# working tree): every brms::brm() call is recorded instead of sampled. Each
# version runs from its own source tree (REF via `git archive`) against a
# scratch data-proc root whose step-01 inputs are symlinked from data-proc/,
# so nothing in data-proc/ is written. The check requires identical call logs
# (site, call order, data hash, priors, sampler settings) and identical
# per-site outcome tables. Prior values are compared numerically to a
# relative 1e-12 (prior means went from 15-significant-digit literals in the
# model code to full-precision Stan data); entries for the residual SD
# (sigma_*), which only exist once its default prior is passed explicitly,
# and seeds, when the reference passes none, are not compared.
#
# Scripts written as a loop over all sites (up to v2-s03) are restricted to
# --sites by rewriting the loop header; per-site runner scripts (from v2-s04)
# get --sites passed through and run in --jobs parallel processes.

stopifnot(requireNamespace("parallel"))

args <- commandArgs(trailingOnly = TRUE)
ref <- if (length(args) > 0 && !startsWith(args[1], "--")) args[1] else stop("give a reference git ref")
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) > 0) sub(paste0("^--", name, "="), "", hit) else default
}
jobs <- as.integer(opt("jobs", "8"))
site_info <- utils::read.csv(file.path("data", "site_info.csv"))
sites <- strsplit(opt("sites", paste(site_info$site_ID, collapse = ",")), ",")[[1]]
stopifnot(all(sites %in% site_info$site_ID))
sites <- site_info$site_ID[site_info$site_ID %in% sites]

models <- c(
  total = "workflows/02_01a_estimate_total_TAS_use_moving_window.R",
  direct = "workflows/02_01b_estimate_direct_TAS_use_moving_window.R"
)
outputs <- list(
  total = c("outcome_temp.csv", "outcome_siteyear_temp.csv"),
  direct = c("outcome_temp_water_gpp.csv", "outcome_siteyear_temp_water_gpp.csv")
)
repo <- normalizePath(".")
dry_run <- file.path(repo, "tests", "dry-run.R")
original_data <- normalizePath(file.path("..", "original-data"))
work <- file.path(repo, "data-proc", "checks", paste0("dryrun-", format(Sys.time(), "%Y%m%d-%H%M%S")))
dir.create(work, recursive = TRUE)

source_tree <- function(version) {
  if (version == "head") return(repo)
  dir <- file.path(work, "src-ref")
  dir.create(dir)
  status <- system(sprintf("git archive %s | tar -x -C %s", shQuote(ref), shQuote(dir)))
  stopifnot(status == 0)
  dir
}

# A scratch data-proc root holding links to the step-01 inputs.
proc_root <- function(version, model) {
  root <- file.path(work, version, model, "proc")
  dir.create(root, recursive = TRUE)
  for (f in c("RespirationData", "TS_RandomForest", "growing_season_feature_EuropFlux.csv", "growing_season_feature_AmeriFlux.csv")) {
    file.symlink(file.path(repo, "data-proc", f), file.path(root, f))
  }
  root
}

is_loop_script <- function(path) any(grepl("^for \\(id in 1:nrow\\(site_info\\)\\)", readLines(path)))

runs <- list()
for (version in c("ref", "head")) {
  src <- source_tree(version)
  for (model in names(models)) {
    script <- file.path(src, models[[model]])
    root <- proc_root(version, model)
    dry_dir <- file.path(work, version, model, "calls")
    if (is_loop_script(script)) {
      lines <- readLines(script)
      loop <- grep("^for \\(id in 1:nrow\\(site_info\\)\\)", lines)
      lines[loop] <- sprintf("for (id in which(site_info$site_ID %%in%% %s)) {", paste(deparse(sites), collapse = ""))
      script <- file.path(work, paste0(version, "-", model, ".R"))
      writeLines(lines, script)
      chunks <- list(NULL)
    } else {
      chunks <- unname(split(sites, rep_len(seq_len(min(jobs, length(sites))), length(sites))))
    }
    for (i in seq_along(chunks)) {
      runs[[length(runs) + 1]] <- list(
        version = version, model = model, src = src, script = script, root = root, dry_dir = dry_dir,
        sites = chunks[[i]], log = file.path(work, sprintf("%s-%s-%d.log", version, model, i))
      )
    }
  }
}

# Versions with a separate input-preparation step (02_00, from v2-s07) prepare
# each site's fit inputs in each scratch root first.
prep_runs <- list()
for (r in runs) {
  prep <- file.path(r$src, "workflows", "02_00_prepare_TAS_inputs.R")
  key <- r$root
  if (file.exists(prep) && is.null(prep_runs[[key]])) {
    prep_runs[[key]] <- lapply(unname(split(sites, rep_len(seq_len(min(jobs, length(sites))), length(sites)))), function(chunk) {
      list(src = r$src, root = r$root, script = prep, sites = chunk)
    })
  }
}
prep_runs <- unlist(prep_runs, recursive = FALSE)
if (length(prep_runs) > 0) {
  prep_status <- parallel::mclapply(seq_along(prep_runs), function(i) {
    r <- prep_runs[[i]]
    system(sprintf("cd %s && THERMAL_DATA_PROC=%s THERMAL_ORIGINAL_DATA=%s Rscript %s %s > %s 2>&1",
                   shQuote(r$src), shQuote(r$root), shQuote(original_data), shQuote(r$script),
                   shQuote(paste0("--sites=", paste(r$sites, collapse = ","))), shQuote(file.path(work, sprintf("prep-%d.log", i)))))
  }, mc.cores = jobs)
  if (any(unlist(prep_status) != 0)) stop("input preparation failed; see ", work, "/prep-*.log")
}

status <- parallel::mclapply(runs, function(r) {
  cmd <- sprintf(
    "cd %s && TAS_DRY_RUN=%s THERMAL_DATA_PROC=%s THERMAL_ORIGINAL_DATA=%s Rscript -e %s %s > %s 2>&1",
    shQuote(r$src), shQuote(r$dry_dir), shQuote(r$root), shQuote(original_data),
    shQuote(sprintf("source('%s'); source('%s')", dry_run, r$script)),
    if (is.null(r$sites)) "" else shQuote(paste0("--sites=", paste(r$sites, collapse = ","))),
    shQuote(r$log)
  )
  system(cmd)
}, mc.cores = jobs)
failed <- which(unlist(status) != 0)
if (length(failed) > 0) {
  for (i in failed) message(runs[[i]]$log, ":\n", paste(utils::tail(readLines(runs[[i]]$log), 15), collapse = "\n"))
  stop(length(failed), " dry-run process(es) failed")
}

read_calls <- function(dir) {
  files <- list.files(dir, pattern = "^brm-calls.*\\.tsv$", full.names = TRUE)
  calls <- do.call(rbind, lapply(files, utils::read.delim, colClasses = "character"))
  calls <- calls[order(match(calls$site, sites), as.integer(calls$seq)), ]
  rownames(calls) <- NULL
  calls
}

# Per-site outcome tables: from a loop script's combined CSVs, or from a
# runner's per-site results, written out the same way and re-read as text.
read_outcomes <- function(version, model) {
  root <- file.path(work, version, model, "proc")
  site_dir <- file.path(root, "TAS_sites", model)
  if (dir.exists(site_dir)) {
    env <- new.env()
    sys.source(file.path(source_tree_dirs[[version]], "R", "step02-tas.R"), envir = env)
    combined <- env$combine_tas_sites(lapply(file.path(site_dir, paste0(sites, ".rds")), readRDS))
    tables <- list(combined$outcome, combined$siteyear)
  } else {
    tables <- lapply(outputs[[model]], function(f) utils::read.csv(file.path(root, f)))
    # A loop restricted to some sites still indexes outcome rows by the site's
    # row in site_info, leaving all-NA rows for the sites it skipped.
    tables[[1]] <- tables[[1]][!is.na(tables[[1]]$site_ID), ]
  }
  lapply(tables, function(t) {
    f <- tempfile(fileext = ".csv")
    utils::write.csv(t, f, row.names = FALSE)
    utils::read.csv(f, colClasses = "character")
  })
}
source_tree_dirs <- list(ref = file.path(work, "src-ref"), head = repo)

n_bad <- 0
report <- function(label, a, b) {
  # columns added by the working tree (e.g. TAS_se) are reported, not compared
  added <- setdiff(names(b), names(a))
  if (length(added) > 0 && all(names(a) %in% names(b))) {
    message(label, ": new column(s) ", paste(added, collapse = ", "))
    b <- b[names(a)]
  }
  if (identical(a, b)) {
    message(label, ": identical (", nrow(a), " rows)")
    return(invisible())
  }
  n_bad <<- n_bad + 1
  message(label, ": DIFFERENT (", nrow(a), " vs ", nrow(b), " rows)")
  if (identical(dim(a), dim(b)) && identical(names(a), names(b))) {
    bad <- which(rowSums(as.matrix(a) != as.matrix(b) | is.na(a) != is.na(b), na.rm = TRUE) > 0)
    print(utils::head(rbind(cbind(version = "ref", a[bad, ]), cbind(version = "head", b[bad, ])), 10))
  }
}
parse_priors <- function(x) {
  lapply(strsplit(x, ";", fixed = TRUE), function(entries) {
    kv <- strsplit(entries, "=", fixed = TRUE)
    v <- stats::setNames(as.numeric(vapply(kv, `[`, "", 2)), vapply(kv, `[`, "", 1))
    v[!startsWith(names(v), "sigma_")]
  })
}
compare_calls <- function(label, a, b) {
  if (!identical(nrow(a), nrow(b))) return(report(label, a, b))
  pa <- parse_priors(a$priors)
  pb <- parse_priors(b$priors)
  close <- mapply(function(x, y) {
    identical(names(x), names(y)) && identical(is.na(x), is.na(y)) &&
      all(abs(x - y) <= 1e-12 * pmax(1, abs(x)), na.rm = TRUE)
  }, pa, pb)
  if (!all(close)) {
    n_bad <<- n_bad + 1
    message(label, ": prior values differ in ", sum(!close), " calls")
    print(utils::head(cbind(a[!close, c("site", "seq", "priors")], head_priors = b$priors[!close]), 5))
  }
  drop <- c("priors", if (all(is.na(a$seed) | a$seed == "NA")) "seed")
  report(paste(label, "(other columns)"), a[setdiff(names(a), drop)], b[setdiff(names(b), drop)])
}
for (model in names(models)) {
  compare_calls(paste(model, "brm calls"), read_calls(file.path(work, "ref", model, "calls")),
                read_calls(file.path(work, "head", model, "calls")))
  a <- read_outcomes("ref", model)
  b <- read_outcomes("head", model)
  report(paste(model, "outcome"), a[[1]], b[[1]])
  report(paste(model, "siteyear"), a[[2]], b[[2]])
}
message("Dry-run outputs in ", work)
if (n_bad > 0) stop(n_bad, " differences against ", ref)
message("OK: step-02 fit inputs and outcome tables identical to ", ref, " (", length(sites), " sites)")
