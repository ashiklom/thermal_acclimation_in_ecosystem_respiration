# Writing the pipeline's results where the `workflows/` scripts look for them.
#
# Everything below uses `write.csv`, never `readr::write_csv`. `write_csv` emits
# ISO8601 timestamps (`1996-01-01T00:30:00Z`) which the downstream scripts, all
# of which use default-format `read.csv`, cannot parse.

DIR_ANALYSIS <- file.path("data-proc", "analysis")
DIR_FEATURES <- file.path("data-proc", "features")
DIR_RESPIRATION <- file.path("data-proc", "respiration")

# The manuscript's column order for outcome_siteyear_*.csv; ours appends
# `status`, so a diff against data-proc-original/ lines up.
OUTCOME_SITEYEAR_COLS <- c(
  "site_ID", "growing_year", "window", "nobsv", "extend_days",
  "alpha", "beta", "C0", "Hs", "k2", "TS", "ERref", "lnRatio"
)

write_result_csv <- function(dat, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(dat, file = path, row.names = FALSE)
  path
}

# Under `error = "null"` an errored target reaches the collectors as NULL;
# dropping it leaves a gap in the tables instead of failing them. If every
# target errored, the bound table has no columns at all, which is why the
# collectors below select with `any_of()`.
built <- function(...) Filter(Negate(is.null), list(...))

# Site-level TAS, one row per site x recipe x model (outcome_temp*.csv).
collect_outcome <- function(...) {
  dplyr::bind_rows(lapply(built(...), `[[`, "outcome")) |>
    dplyr::arrange(dplyr::across(dplyr::any_of(c("site_ID", "recipe_id", "model"))))
}

collect_outcome_siteyear <- function(...) {
  dplyr::bind_rows(lapply(built(...), `[[`, "outcome_siteyear")) |>
    dplyr::relocate(dplyr::any_of(c("site_ID", "recipe_id", "model"))) |>
    dplyr::relocate(dplyr::any_of(setdiff(OUTCOME_SITEYEAR_COLS, "site_ID")),
                    .after = dplyr::any_of(c("site_ID", "recipe_id", "model"))) |>
    dplyr::arrange(dplyr::across(dplyr::any_of(c("site_ID", "recipe_id", "model", "window", "growing_year"))))
}

collect_settings <- function(...) {
  dplyr::bind_rows(lapply(built(...), `[[`, "settings")) |>
    dplyr::arrange(dplyr::across(dplyr::any_of(c("site_ID", "recipe_id", "model"))))
}

# The manuscript-layout subset `workflows/` reads: the `original` recipe, one
# model, and none of the variant grid's columns. Empty if `original` was not
# run; returned as is if it has no columns (every fit errored).
original_only <- function(tbl, model = NULL, drop = c("recipe_id", "fit_profile")) {
  if (!"recipe_id" %in% names(tbl)) return(tbl)
  out <- dplyr::filter(tbl, .data$recipe_id == "original")
  if (!is.null(model) && "model" %in% names(out)) {
    out <- dplyr::filter(out, .data$model == .env$model)
    drop <- c(drop, "model")
  }
  dplyr::select(out, -dplyr::any_of(drop))
}

# Soil-temperature quality verdicts, one row per site.
collect_ts_qc <- function(...) {
  dplyr::bind_rows(lapply(built(...), `[[`, "ts_qc")) |>
    dplyr::arrange(.data$site_ID)
}

# What stage A did to each site's soil temperature: one row per site.
collect_ts_provenance <- function(...) {
  dplyr::bind_rows(lapply(built(...), `[[`, "ts_provenance")) |>
    dplyr::arrange(.data$site_ID)
}

# The blocked-CV table behind each site's fill choice: one row per method.
collect_fill_cv <- function(...) {
  parts <- Filter(function(f) !is.null(f[["cv"]]), built(...))
  if (!length(parts)) return(tibble::tibble(site_ID = character(), method = character()))
  dplyr::bind_rows(lapply(parts, `[[`, "cv")) |>
    dplyr::arrange(.data$site_ID, .data$rmse)
}

# What each site's fill decided, including the sites where it could not.
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

# A run with no skips still writes a header: `read.csv()` refuses an empty
# file.
WINDOW_SKIP_COLS <- c("site_ID", "recipe_id", "model", "window", "window_start",
                      "window_end", "reason", "detail")

collect_window_skips <- function(...) {
  out <- dplyr::bind_rows(lapply(built(...), `[[`, "window_skips"))
  if (!nrow(out)) {
    out <- tibble::tibble(
      site_ID = character(), recipe_id = character(), model = character(), window = character(),
      window_start = numeric(), window_end = numeric(), reason = character(), detail = character()
    )
  }
  out
}

# Growing-season features, one row per site (read by `04_02`).
collect_feature_gs <- function(...) {
  dplyr::bind_rows(lapply(built(...), `[[`, "feature_gs")) |>
    dplyr::arrange(.data$site_ID)
}

# Per-site half-hourly tables, which `03_01` and `04_02` find by globbing
# `data-proc/respiration/**/*_ac.csv` -- so this owns the whole directory:
# files with an older schema are removed, and current-schema files from sites
# outside this run are kept but reported, since the glob will read them.
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

# The manuscript's site-year tables: what the site-level QC in site_info.csv
# (`year_removed`, gStart/gEnd, the gap thresholds in
# `compute_gap_thresholds()`) was set by hand against.
MANUSCRIPT_SITEYEAR_CSVS <- file.path(
  "data-proc-original",
  c("outcome_siteyear_temp.csv", "outcome_siteyear_temp_water_gpp.csv")
)

# Site-years this run fitted that come after the manuscript's last year at the
# site -- or at a site the manuscript did not have. These arrive with new data
# releases and pass only the automatic checks, never the manual review the
# manuscript's years got, so they are listed for someone to look at before the
# results that include them are trusted. Years *inside* the manuscript's span
# that differ from it are a different question, which `pixi run reconcile`
# answers.
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
