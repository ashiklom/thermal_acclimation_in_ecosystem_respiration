# Writing the pipeline's results where the `workflows/` scripts look for them.
#
# Everything below uses `write.csv`, never `readr::write_csv`. `write_csv` emits
# ISO8601 timestamps (`1996-01-01T00:30:00Z`) which the downstream scripts, all
# of which use default-format `read.csv`, cannot parse. See docs/derived-columns.md.

DIR_ANALYSIS <- file.path("data-proc", "analysis")
DIR_FEATURES <- file.path("data-proc", "features")
DIR_RESPIRATION <- file.path("data-proc", "respiration")

# The manuscript's column order for outcome_siteyear_*.csv. Ours carries one
# extra column, `status`, appended rather than interleaved so that a diff
# against data-proc-original/ lines up column for column.
OUTCOME_SITEYEAR_COLS <- c(
  "site_ID", "growing_year", "window", "nobsv", "extend_days",
  "alpha", "beta", "C0", "Hs", "k2", "TS", "ERref", "lnRatio"
)

write_result_csv <- function(dat, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(dat, file = path, row.names = FALSE)
  path
}

# Site-level TAS, one row per site -- the counterpart to outcome_temp.csv.
#
# Sorted by site_ID, and that is load-bearing rather than tidiness.
# `02_02_compare_different_TAS.R:15-18` grafts the direct model's TAS onto the
# total model's table *by position*:
#
#     outcome$TAS <- outcome_temp_water_gpp$TAS
#
# There is no join. If the two files disagree on row order every site silently
# receives another site's number, and nothing downstream can detect it. Sorting
# both on the same key is what makes that safe.
collect_outcome <- function(...) {
  dplyr::bind_rows(lapply(list(...), `[[`, "outcome")) |>
    dplyr::arrange(.data$site_ID)
}

collect_outcome_siteyear <- function(...) {
  dplyr::bind_rows(lapply(list(...), `[[`, "outcome_siteyear")) |>
    dplyr::relocate(dplyr::any_of(OUTCOME_SITEYEAR_COLS)) |>
    dplyr::arrange(.data$site_ID, .data$window, .data$growing_year)
}

collect_settings <- function(...) {
  dplyr::bind_rows(lapply(list(...), `[[`, "settings")) |>
    dplyr::arrange(.data$site_ID, .data$model)
}

collect_window_skips <- function(...) {
  dplyr::bind_rows(lapply(list(...), `[[`, "window_skips"))
}

# Growing-season features, one row per site.
#
# This table had no producer anywhere in the repo. `prep_nee_ac()` computed
# `feature_gs` and returned it, `04_02` required the file, and the copy on disk
# held 8 sites from some earlier pass -- so `04_02` failed with `integer(0)`
# bounds for every other site. `load_growing_season_features()` checks only that
# the file exists, not that it covers anything.
collect_feature_gs <- function(...) {
  dplyr::bind_rows(lapply(list(...), `[[`, "feature_gs")) |>
    dplyr::arrange(.data$site_ID)
}

# Per-site half-hourly tables, which `03_01` and `04_02` find by globbing
# `data-proc/respiration/**/*_ac.csv` recursively.
#
# That glob is why this function has to take responsibility for the whole
# directory and not just its own sites: anything else left lying there is read
# as though this run had produced it. Files written by an older version of the
# pipeline carry a different set of columns, so they are identified by their
# header and removed. Files that match the current schema but belong to sites
# outside this run are left alone -- they may be a deliberate wider run -- but
# they are reported, because they will be picked up too.
write_respiration_all <- function(...) {
  parts <- list(...)
  written <- character()
  sites <- character()

  for (sd in parts) {
    site <- sd[["feature_gs"]][["site_ID"]]
    sites <- c(sites, site)
    outdir <- file.path(DIR_RESPIRATION, site)
    write_respiration_outputs(sd[["ac"]], sd[["nightNEE"]], outdir, site)
    written <- c(written, file.path(outdir, paste0(site, c("_ac.csv", "_nightNEE.csv"))))
  }

  expected_header <- names(parts[[1]][["ac"]])
  others <- setdiff(
    list.files(DIR_RESPIRATION, pattern = "_ac[.]csv$", full.names = TRUE, recursive = TRUE),
    written
  )
  for (path in others) {
    header <- names(utils::read.csv(path, nrows = 1))
    site <- sub("_ac[.]csv$", "", basename(path))
    if (identical(header, expected_header)) {
      message(
        "  respiration: keeping ", site, " from an earlier run -- it matches the ",
        "current schema, but note the downstream glob will read it too."
      )
    } else {
      message("  respiration: removing stale ", site, " (schema predates this pipeline)")
      unlink(dirname(path), recursive = TRUE)
    }
  }

  # Empty site directories are invisible to the loop above -- it walks files,
  # not directories -- and harmless to the downstream glob, which matches on
  # `_ac.csv`. Still worth clearing, so the directory listing is an honest
  # record of which sites the pipeline has produced.
  for (d in list.dirs(DIR_RESPIRATION, recursive = FALSE)) {
    if (length(list.files(d)) == 0) unlink(d, recursive = TRUE)
  }

  sort(written)
}
