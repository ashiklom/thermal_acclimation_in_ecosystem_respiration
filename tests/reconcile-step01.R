#!/usr/bin/env Rscript
#
# Step-01 reconciliation against the manuscript's own outputs.
#
# `data-proc-original/growing_season_feature_{AmeriFlux,EuropFlux}.csv` are the
# growing-season features the original scripts produced for all 117 sites. They
# are the only site-level oracle that survived into this repo (the per-site
# `_ac.csv` / `_nightNEE.csv` files lived on an external drive), and they happen
# to summarise exactly the part of the pipeline that was rewritten rather than
# transcribed: gStart, gEnd, tStart, tEnd and the number of qualifying years.
#
# This reports a diff, it does not assert equality. Differences are *expected*:
# 27 sites changed data provenance when multi-release stitching was dropped, and
# the refactor deliberately fixed a handful of things. The point is to make every
# difference visible and attributable, so that a difference nobody can explain
# stands out. The `flag` column is a crude plausibility heuristic, not a verdict.
#
#   pixi run reconcile
#   Rscript tests/reconcile-step01.R --sites US-Kon,DE-Tha

suppressMessages({
  library(dplyr)
  library(targets)
})
tar_source()

# Representative sites: one row per code path that the rewrite touched.
DEFAULT_SITES <- c(
  "US-Kon", # AmeriFlux baseline; SWC_use = NO with a named SWC column; TS ~ TA site
  "US-Wkg", # AmeriFlux with measured soil water (the other SWC branch)
  "US-GLE", # AmeriFlux TS >= 2 C truncation, applied after the gap scan; gStart override
  "US-ICt", # Arctic tundra: low-light night filter, sparse gap thresholds, TS ~ TA
  "CH-Dav", # ICOS TS >= 2 C truncation, applied before the gap scan; predicted soil temp
  "DE-Tha", # ICOS baseline
  "NL-Loo", # ICOS with a gStart override and TS ~ TA
  "GF-Guy", # ICOS tropical: air temperature substituted for soil, gStart/gEnd pinned
  "AU-Tum", # TERN southern hemisphere: the wrapped growing year, gEnd override
  "US-Ha1", # AmeriFlux estimate_Ts: soil temperature regressed from air only
  "US-Los"  # AmeriFlux estimate_Ts: the random forest on air + net radiation
)

args <- commandArgs(trailingOnly = TRUE)
sites <- if (length(args) >= 2 && args[[1]] == "--sites") {
  trimws(unlist(strsplit(args[[2]], ",")))
} else {
  DEFAULT_SITES
}

oracle <- dplyr::bind_rows(
  read.csv(file.path("data-proc-original", "growing_season_feature_AmeriFlux.csv")),
  read.csv(file.path("data-proc-original", "growing_season_feature_EuropFlux.csv"))
)

# Failures we already know about and have documented, so that a genuinely new
# breakage is not lost among them.
KNOWN_GAPS <- c(
  "site-specific logic not implemented",
  "No download method"
)

# Not a code problem: the site simply has not been downloaded. Reported
# separately so that an incomplete `data-raw/` does not look like a regression.
MISSING_DATA <- c(
  "length(file_path) == 1 is not TRUE",
  "cannot open the connection",
  "does not exist"
)

# Exceeded the per-site budget below rather than failing outright.
TIMED_OUT <- c("reached elapsed time limit", "reached CPU time limit")

# One pathological site should not stall the whole run. This is a soft limit:
# `setTimeLimit()` only fires at R-level interrupt checks, so it will not
# interrupt a tight loop inside compiled code -- but REddyProc is mostly R, so
# in practice it does bite. US-GLE is the site that motivated this.
SITE_TIME_BUDGET_SECS <- 600

classify_error <- function(msg) {
  hit <- function(patterns) any(vapply(patterns, grepl, logical(1), x = msg, fixed = TRUE))
  if (hit(TIMED_OUT)) return("timed out")
  if (hit(MISSING_DATA)) return("no raw data")
  if (hit(KNOWN_GAPS)) return("blocked")
  "error"
}

rows <- list()
for (name_site in sites) {
  cat(sprintf("\n==== %s ====\n", name_site))
  t0 <- Sys.time()
  res <- tryCatch(
    {
      setTimeLimit(elapsed = SITE_TIME_BUDGET_SECS, transient = TRUE)
      on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
      suppressWarnings(suppressMessages(prep_nee_ac(get_site_info(name_site))))
    },
    error = function(e) structure(conditionMessage(e), class = "recon_error")
  )
  setTimeLimit(elapsed = Inf)
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  si <- get_site_info(name_site)
  ref <- oracle[oracle$site_ID == name_site, ]

  if (inherits(res, "recon_error")) {
    status <- classify_error(as.character(res))
    cat(sprintf("  %s: %s\n", toupper(status), as.character(res)))
    rows[[name_site]] <- tibble::tibble(
      site_ID = name_site, source = si$source, status = status,
      note = substr(as.character(res), 1, 90), secs = round(secs, 1)
    )
    next
  }

  f <- res$feature_gs
  cmp <- tibble::tibble(
    site_ID = name_site,
    source = si$source,
    status = "ok",
    gStart_orig = if (nrow(ref)) ref$gStart else NA_real_,
    gStart_new = f$gStart,
    gEnd_orig = if (nrow(ref)) ref$gEnd else NA_real_,
    gEnd_new = f$gEnd,
    tStart_orig = if (nrow(ref)) ref$tStart else NA_real_,
    tStart_new = f$tStart,
    tEnd_orig = if (nrow(ref)) ref$tEnd else NA_real_,
    tEnd_new = f$tEnd,
    nyear_orig = if (nrow(ref)) ref$nyear else NA_integer_,
    nyear_new = f$nyear,
    night_rows = nrow(res$nightNEE),
    ac_rows = nrow(res$ac),
    secs = round(secs, 1)
  ) |>
    dplyr::mutate(
      d_gStart = .data$gStart_new - .data$gStart_orig,
      d_gEnd = .data$gEnd_new - .data$gEnd_orig,
      d_nyear = .data$nyear_new - .data$nyear_orig,
      # Heuristic only: the growing season moving by more than a month, or the
      # usable record changing by more than three years, is worth a look.
      flag = dplyr::case_when(
        abs(.data$d_gStart) > 30 | abs(.data$d_gEnd) > 30 ~ "season shift",
        abs(.data$d_nyear) > 3 ~ "record length",
        TRUE ~ ""
      )
    )

  cat(sprintf(
    "  gStart %4s -> %-4s (%+d)   gEnd %4s -> %-4s (%+d)   nyear %3s -> %-3s (%+d)\n",
    cmp$gStart_orig, cmp$gStart_new, cmp$d_gStart,
    cmp$gEnd_orig, cmp$gEnd_new, cmp$d_gEnd,
    cmp$nyear_orig, cmp$nyear_new, cmp$d_nyear
  ))
  cat(sprintf(
    "  tStart %6.2f -> %-6.2f       tEnd %6.2f -> %-6.2f       night rows %d, ac rows %d  [%.0fs]%s\n",
    cmp$tStart_orig, cmp$tStart_new, cmp$tEnd_orig, cmp$tEnd_new,
    cmp$night_rows, cmp$ac_rows, cmp$secs,
    if (nzchar(cmp$flag)) paste0("  <-- ", cmp$flag) else ""
  ))
  rows[[name_site]] <- cmp
}

report <- dplyr::bind_rows(rows)
outdir <- file.path("data-proc", "reconciliation")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
outfile <- file.path(outdir, "step01-reconciliation.csv")
write.csv(report, outfile, row.names = FALSE)

cat("\n================ SUMMARY ================\n")
ok <- report |> dplyr::filter(.data$status == "ok")
if (nrow(ok)) {
  summary_cols <- c("site_ID", "source", "d_gStart", "d_gEnd", "d_nyear", "night_rows", "flag")
  print(
    as.data.frame(dplyr::select(ok, dplyr::any_of(summary_cols))),
    row.names = FALSE
  )
}
cat(sprintf(
  "\n  %d ok, %d timed out, %d missing raw data, %d blocked by a known gap, %d unexplained error\n",
  sum(report$status == "ok"), sum(report$status == "timed out"),
  sum(report$status == "no raw data"),
  sum(report$status == "blocked"), sum(report$status == "error")
))
for (st in c("timed out", "no raw data", "blocked", "error")) {
  bad <- report |> dplyr::filter(.data$status == st)
  if (nrow(bad)) {
    cat(sprintf("\n  %s:\n", st))
    notes <- if ("note" %in% names(bad)) bad$note else rep("", nrow(bad))
    for (i in seq_len(nrow(bad))) cat(sprintf("    %-8s %s\n", bad$site_ID[i], notes[i]))
  }
}
cat(sprintf("\n  written to %s\n", outfile))

# A known gap or an absent download is not a test failure. An unexplained error
# is, and so is a site that blew its time budget.
if (any(report$status %in% c("error", "timed out"))) quit(status = 1)
