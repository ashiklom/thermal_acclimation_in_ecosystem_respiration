# Writing the pipeline's results where the `workflows/` scripts look for them.
#
# Everything below uses `write.csv`, never `readr::write_csv`. `write_csv` emits
# ISO8601 timestamps (`1996-01-01T00:30:00Z`) which the downstream scripts, all
# of which use default-format `read.csv`, cannot parse.

#' Directory for the analysis result CSVs
DIR_ANALYSIS <- file.path("data-proc", "analysis")
#' Directory for the feature CSVs
DIR_FEATURES <- file.path("data-proc", "features")
#' Directory for the per-site half-hourly respiration tables
DIR_RESPIRATION <- file.path("data-proc", "respiration")

#' The manuscript's column order for outcome_siteyear_*.csv
#'
#' Ours appends `status`, so a diff against data-proc-original/ lines up.
OUTCOME_SITEYEAR_COLS <- c(
  "site_ID", "growing_year", "window", "nobsv", "extend_days",
  "alpha", "beta", "C0", "Hs", "k2", "TS", "ERref", "lnRatio"
)

#' Write a table to CSV, creating its directory
#'
#' @param dat Data frame to write.
#' @param path Path of the CSV.
#' @return Called for its side effect; returns `path`.
write_result_csv <- function(dat, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(dat, file = path, row.names = FALSE)
  path
}

#' Drop errored targets
#'
#' Under `error = "null"` an errored target reaches the collectors as NULL;
#' dropping it leaves a gap in the tables instead of failing them. If every
#' target errored, the bound table has no columns at all, which is why the
#' collectors below select with `any_of()`.
#'
#' @param ... Per-site target values, any of which may be NULL.
#' @return List of the non-NULL arguments.
built <- function(...) Filter(Negate(is.null), list(...))

#' Site-level TAS
#'
#' @param ... Per-site `fit_tas_site()` results; NULL ones (errored targets) are
#'   dropped.
#' @return Tibble, one row per site x recipe x model (outcome_temp*.csv).
collect_outcome <- function(...) {
  dplyr::bind_rows(lapply(built(...), `[[`, "outcome")) |>
    dplyr::arrange(dplyr::across(dplyr::any_of(c("site_ID", "recipe_id", "model"))))
}

#' Window-level fit results across sites
#'
#' @param ... Per-site `fit_tas_site()` results; NULL ones (errored targets) are
#'   dropped.
#' @return Tibble of `outcome_siteyear` rows: `site_ID`, `recipe_id`, `model`,
#'   then the `OUTCOME_SITEYEAR_COLS` in order, then the rest.
collect_outcome_siteyear <- function(...) {
  dplyr::bind_rows(lapply(built(...), `[[`, "outcome_siteyear")) |>
    dplyr::relocate(dplyr::any_of(c("site_ID", "recipe_id", "model"))) |>
    dplyr::relocate(dplyr::any_of(setdiff(OUTCOME_SITEYEAR_COLS, "site_ID")),
                    .after = dplyr::any_of(c("site_ID", "recipe_id", "model"))) |>
    dplyr::arrange(dplyr::across(dplyr::any_of(c("site_ID", "recipe_id", "model", "window", "growing_year"))))
}

#' Run settings across sites
#'
#' @param ... Per-site `fit_tas_site()` results; NULL ones (errored targets) are
#'   dropped.
#' @return Tibble of each fit's `settings`, one row per site x recipe x model.
collect_settings <- function(...) {
  dplyr::bind_rows(lapply(built(...), `[[`, "settings")) |>
    dplyr::arrange(dplyr::across(dplyr::any_of(c("site_ID", "recipe_id", "model"))))
}

#' The manuscript-layout subset `workflows/` reads
#'
#' The `original` recipe, one model, and none of the variant grid's columns.
#'
#' @param tbl A table from `collect_fit_tables()`.
#' @param model Model to keep, or `NULL` to keep every model (and the `model`
#'   column).
#' @param drop Columns to drop; `model` is added to them when `model` is given.
#' @return `tbl` filtered to the `original` recipe. Empty if `original` was not
#'   run; returned as is if it has no columns (every fit errored).
original_only <- function(tbl, model = NULL, drop = c("recipe_id", "fit_profile")) {
  if (!"recipe_id" %in% names(tbl)) return(tbl)
  out <- dplyr::filter(tbl, .data$recipe_id == "original")
  if (!is.null(model) && "model" %in% names(out)) {
    out <- dplyr::filter(out, .data$model == .env$model)
    drop <- c(drop, "model")
  }
  dplyr::select(out, -dplyr::any_of(drop))
}

#' Soil-temperature quality verdicts
#'
#' @param ... Per-site step-01 results (`prep_nee_ac()`); NULL ones (errored
#'   targets) are dropped.
#' @return Tibble, one row per site.
collect_ts_qc <- function(...) {
  dplyr::bind_rows(lapply(built(...), `[[`, "ts_qc")) |>
    dplyr::arrange(.data$site_ID)
}

#' What stage A did to each site's soil temperature
#'
#' @param ... Per-site step-01 results (`prep_nee_ac()`); NULL ones (errored
#'   targets) are dropped.
#' @return Tibble, one row per site.
collect_ts_provenance <- function(...) {
  dplyr::bind_rows(lapply(built(...), `[[`, "ts_provenance")) |>
    dplyr::arrange(.data$site_ID)
}

#' The blocked-CV table behind each site's fill choice
#'
#' @param ... Per-site `fill_soil_temp()` results; NULL ones (errored targets)
#'   are dropped.
#' @return Tibble, one row per method at each site, sorted by site and `rmse`;
#'   empty `site_ID` and `method` columns if no site has a CV table.
collect_fill_cv <- function(...) {
  parts <- Filter(function(f) !is.null(f[["cv"]]), built(...))
  if (!length(parts)) return(tibble::tibble(site_ID = character(), method = character()))
  dplyr::bind_rows(lapply(parts, `[[`, "cv")) |>
    dplyr::arrange(.data$site_ID, .data$rmse)
}

#' What each site's fill decided, including the sites where it could not
#'
#' @param ... Per-site `fill_soil_temp()` results; NULL ones (errored targets)
#'   are dropped.
#' @return Tibble, one row per site: `status`, `method`, `cv_rmse`,
#'   `degenerate`, `truth_synthetic`, `blocking` and `n_train`.
collect_fill_summary <- function(...) {
  dplyr::bind_rows(lapply(built(...), function(f) {
    tibble::tibble(
      site_ID = f[["site_ID"]],
      status = f[["status"]],
      method = f[["method"]] %||% NA_character_,
      cv_rmse = f[["cv_rmse"]] %||% NA_real_,
      degenerate = f[["degenerate"]] %||% NA,
      truth_synthetic = f[["truth_synthetic"]] %||% NA,
      blocking = f[["blocking"]] %||% NA_character_,
      n_train = f[["n_train"]] %||% NA_integer_
    )
  })) |>
    dplyr::arrange(.data$site_ID)
}

#' What a run with no skips writes
#'
#' So the CSV still has a header: `read.csv()` refuses an empty file.
WINDOW_SKIPS_EMPTY <- tibble::tibble(
  site_ID = character(), recipe_id = character(), model = character(), window = character(),
  window_start = numeric(), window_end = numeric(), reason = character(), detail = character()
)

#' Skipped windows across sites
#'
#' @param ... Per-site `fit_tas_site()` results; NULL ones (errored targets) are
#'   dropped.
#' @return Tibble, one row per skipped window with its `reason` and `detail`;
#'   `WINDOW_SKIPS_EMPTY` if there are none.
collect_window_skips <- function(...) {
  out <- dplyr::bind_rows(lapply(built(...), `[[`, "window_skips"))
  if (nrow(out)) out else WINDOW_SKIPS_EMPTY
}

#' Growing-season features
#'
#' @param ... Per-site step-01 results (`prep_nee_ac()`); NULL ones (errored
#'   targets) are dropped.
#' @return Tibble, one row per site (read by `04_02`).
collect_feature_gs <- function(...) {
  dplyr::bind_rows(lapply(built(...), `[[`, "feature_gs")) |>
    dplyr::arrange(.data$site_ID)
}

# ------------------------------------------------------------ pipeline groups
#
# `_targets.R` collects each kind of per-site result once, into a named list of
# tables, and writes them in two groups: what the run report reads and what
# the variant report reads. A group's file target changes only when one of its
# files does, so each report re-renders only for its own inputs.

#' Every fit, all recipes and models
#'
#' @param ... Per-site `fit_tas_site()` results; NULL ones (errored targets) are
#'   dropped.
#' @return `fit_tables`: a named list of tibbles, `outcome`, `siteyear`,
#'   `settings` and `window_skips`.
collect_fit_tables <- function(...) {
  list(
    outcome = collect_outcome(...),
    siteyear = collect_outcome_siteyear(...),
    settings = collect_settings(...),
    window_skips = collect_window_skips(...)
  )
}

#' Step 01 under the manuscript's prep
#'
#' @param ... Per-site step-01 results (`prep_nee_ac()`); NULL ones (errored
#'   targets) are dropped.
#' @return `site_tables`: a named list of tibbles, `feature_gs`, `ts_qc` and
#'   `ts_provenance`.
collect_site_tables <- function(...) {
  list(
    feature_gs = collect_feature_gs(...),
    ts_qc = collect_ts_qc(...),
    ts_provenance = collect_ts_provenance(...)
  )
}

#' The soil-temperature fill under the manuscript's prep
#'
#' @param ... Per-site `fill_soil_temp()` results; NULL ones (errored targets)
#'   are dropped.
#' @return `fill_tables`: a named list of tibbles, `fill_cv` and `fill_summary`.
collect_fill_tables <- function(...) {
  list(fill_cv = collect_fill_cv(...), fill_summary = collect_fill_summary(...))
}

#' Each table to `<dir>/<name>.csv`
#'
#' @param tables Named list of data frames.
#' @param dir Directory to write into.
#' @return Called for its side effect; returns the paths.
write_result_csvs <- function(tables, dir = DIR_ANALYSIS) {
  unname(vapply(names(tables), function(name) {
    write_result_csv(tables[[name]], file.path(dir, paste0(name, ".csv")))
  }, ""))
}

#' Write the run report's CSVs
#'
#' The manuscript layout `workflows/` reads -- the `original` recipe only --
#' plus what the run report reads beside it.
#'
#' @param fit_tables The `fit_tables` target, from `collect_fit_tables()`.
#' @param site_tables The `site_tables` target, from `collect_site_tables()`.
#' @param manuscript_paths Paths to the manuscript's site-year tables.
#' @return Called for its side effect; returns the paths written.
write_run_csvs <- function(fit_tables, site_tables, manuscript_paths = MANUSCRIPT_SITEYEAR_CSVS) {
  c(
    write_result_csvs(list(
      outcome_temp = original_only(fit_tables$outcome, "total"),
      outcome_temp_water_gpp = original_only(fit_tables$outcome, "direct"),
      outcome_siteyear_temp = original_only(fit_tables$siteyear, "total"),
      outcome_siteyear_temp_water_gpp = original_only(fit_tables$siteyear, "direct"),
      run_settings = original_only(fit_tables$settings),
      window_skips = original_only(fit_tables$window_skips),
      new_siteyears = collect_new_siteyears(fit_tables$siteyear, manuscript_paths)
    )),
    write_result_csvs(list(growing_season_features = site_tables$feature_gs), DIR_FEATURES)
  )
}

#' Write the variant report's CSVs
#'
#' The variant grid in full, and the per-site diagnostics the variant report
#' reads.
#'
#' @param fit_tables The `fit_tables` target, from `collect_fit_tables()`.
#' @param site_tables The `site_tables` target, from `collect_site_tables()`.
#' @param fill_tables The `fill_tables` target, from `collect_fill_tables()`.
#' @return Called for its side effect; returns the paths written.
write_variant_csvs <- function(fit_tables, site_tables, fill_tables) {
  write_result_csvs(list(
    variant_outcome = fit_tables$outcome,
    variant_siteyear = fit_tables$siteyear,
    variant_settings = fit_tables$settings,
    variant_window_skips = fit_tables$window_skips,
    ts_qc = site_tables$ts_qc,
    ts_provenance = site_tables$ts_provenance,
    fill_cv = fill_tables$fill_cv,
    fill_summary = fill_tables$fill_summary
  ))
}

#' Write every site's half-hourly respiration tables
#'
#' Per-site half-hourly tables, which `03_01` and `04_02` find by globbing
#' `data-proc/respiration/**/*_ac.csv` -- so this owns the whole directory:
#' files with an older schema are removed, and current-schema files from sites
#' outside this run are kept but reported, since the glob will read them.
#'
#' @param ... Per-site step-01 results (`prep_nee_ac()`); NULL ones (errored
#'   targets) are dropped.
#' @return Called for its side effect; returns the sorted paths of the
#'   `_ac.csv` and `_nightNEE.csv` files written.
write_respiration_all <- function(...) {
  parts <- built(...)
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

  # Clear empty site directories, so the listing shows what was produced.
  for (d in list.dirs(DIR_RESPIRATION, recursive = FALSE)) {
    if (length(list.files(d)) == 0) unlink(d, recursive = TRUE)
  }

  sort(written)
}

#' The manuscript's site-year tables
#'
#' What the site-level QC in site_info.csv (`year_removed`, gStart/gEnd, the gap
#' thresholds in `compute_gap_thresholds()`) was set by hand against.
MANUSCRIPT_SITEYEAR_CSVS <- file.path(
  "data-proc-original",
  c("outcome_siteyear_temp.csv", "outcome_siteyear_temp_water_gpp.csv")
)

#' Site-years fitted beyond the manuscript's record
#'
#' Site-years this run fitted that come after the manuscript's last year at the
#' site -- or at a site the manuscript did not have. These arrive with new data
#' releases and pass only the automatic checks, never the manual review the
#' manuscript's years got, so they are listed for someone to look at before the
#' results that include them are trusted. Years *inside* the manuscript's span
#' that differ from it are a different question, which `pixi run reconcile`
#' answers.
#'
#' @param siteyear_tbl The `siteyear` table from `collect_fit_tables()`.
#' @param manuscript_paths Paths to the manuscript's site-year tables.
#' @return Tibble, one row per new site-year with a fitted `ERref`: `site_ID`,
#'   `growing_year`, `manuscript_last_year` (NA at a site the manuscript did not
#'   have), `n_windows_fitted`, and the comma-joined `recipes` and `models`.
collect_new_siteyears <- function(siteyear_tbl, manuscript_paths = MANUSCRIPT_SITEYEAR_CSVS) {
  last <- dplyr::bind_rows(lapply(manuscript_paths, utils::read.csv)) |>
    dplyr::group_by(.data$site_ID) |>
    dplyr::summarise(manuscript_last_year = max(.data$growing_year), .groups = "drop")
  empty <- tibble::tibble(
    site_ID = character(), growing_year = integer(), manuscript_last_year = integer(),
    n_windows_fitted = integer(), recipes = character(), models = character()
  )
  if (is.null(siteyear_tbl) || !nrow(siteyear_tbl)) return(empty)
  fitted <- siteyear_tbl |> dplyr::filter(!is.na(.data$growing_year), !is.na(.data$ERref))
  if (!nrow(fitted)) return(empty)
  fitted |>
    dplyr::left_join(last, by = "site_ID") |>
    dplyr::filter(is.na(.data$manuscript_last_year) | .data$growing_year > .data$manuscript_last_year) |>
    dplyr::group_by(.data$site_ID, .data$growing_year, .data$manuscript_last_year) |>
    dplyr::summarise(
      n_windows_fitted = dplyr::n(),
      recipes = paste(sort(unique(.data$recipe_id)), collapse = ","),
      models = paste(sort(unique(.data$model)), collapse = ","),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$site_ID, .data$growing_year)
}
