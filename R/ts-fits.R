# The fit matrix (refactor-v2 stage S17): TAS fits, step-03 climate
# statistics and the step-04 scenario for every site and selected
# soil-temperature column (ts_methods_selected, R/ts-ranking.R), and steps 03
# and 04 for every column's set of TAS estimates.
#
# Columns often give identical inputs at a site (TS_reconstructed is
# TS_manuscript wherever step 02 does not regress; TS_original is
# TS_reconstructed wherever step 01 keeps the sensor as it is). Each per-site
# result is computed once per distinct input: a column whose input is the
# manuscript's reuses the manuscript's result (the pipeline's own tas_total,
# tas_direct, climate_stats and scenario targets), and a column whose input
# equals an earlier column's returns list(same_as = <that column>), resolved
# when the sites are combined (resolve_same_as()).

#' Hash of a column's TAS inputs
#'
#' Hash of a column's TAS inputs (NA if the column has none at the site).
#'
#' @param inputs ts_column_inputs()'s result, or NULL.
#' @return A string, or NA.
#' @noRd
ts_inputs_key <- function(inputs) {
  if (is.null(inputs)) NA_character_ else digest::digest(inputs)
}

#' A column's step-01 tables
#'
#' A column's step-01 tables (ts_column_tables()), for the step 03 climate statistics and the
#' step 04 scenario: step 01's own for TS_manuscript (the manuscript ran both on step 01's
#' tables, not on step 02's regressed soil temperature). NULL if the column has no nighttime
#' record.
#'
#' @param column Candidate column name.
#' @param step01 The site's step-01 list.
#' @param candidates ts_candidates()'s list.
#' @return list(ac, night, feature), or NULL.
#' @noRd
ts_step01_tables <- function(column, step01, candidates) {
  if (column == "TS_manuscript") {
    return(list(ac = step01$ac, night = step01$night, feature = step01$feature))
  }
  if (!any(step01$ts_base$night_ok & candidates$ok[[column]])) return(NULL)
  tables <- ts_column_tables(step01$ts_base, step01$ts_params, candidates$values[[column]], candidates$ok[[column]])
  if (nrow(tables$night) == 0) return(NULL)
  tables[c("ac", "night", "feature")]
}

#' Hash of a column's step-01 tables
#'
#' Hash of a column's step-01 tables (ac, night and the growing season), NA if it has none.
#'
#' @param tables ts_step01_tables()'s result, or NULL.
#' @return A string, or NA.
#' @noRd
ts_tables_key <- function(tables) {
  if (is.null(tables)) NA_character_ else digest::digest(list(tables$ac, tables$night, tables$feature[, c("gStart", "gEnd")]))
}

#' Where a column's result comes from
#'
#' Which result a column's result is: "manuscript" (the manuscript's), "none" (no input), the
#' name of an earlier column with the same input, or "" (its own). `keys` is a data frame of
#' column and key for all selected columns, in order.
#'
#' @param column Candidate column name.
#' @param keys A data frame of column and key.
#' @return A string.
#' @noRd
ts_result_source <- function(column, keys) {
  key <- keys$key[keys$column == column]
  if (is.na(key)) return("none")
  if (identical(key, keys$key[keys$column == "TS_manuscript"])) return("manuscript")
  first <- keys$column[match(key, keys$key)]
  if (first != column) first else ""
}

#' One column's TAS fit at a site
#'
#' One column's TAS fit at a site, unless shared (ts_result_source()).
#'
#' @param column Candidate column name.
#' @param model_type "direct" or "total".
#' @param keys A data frame of column and key (ts_inputs_key()) for all selected columns.
#' @param manuscript_fit The pipeline's own fit of this model at the site.
#' @param inputs_fn Function of no arguments returning the column's TAS inputs.
#' @param sampler Sampler settings (tas_fit_settings()).
#' @return fit_tas_target()'s result, the manuscript's fit, list(same_as = <column>), or NULL.
#' @noRd
ts_fit_column <- function(column, model_type, keys, manuscript_fit, inputs_fn, sampler) {
  source <- ts_result_source(column, keys)
  if (source == "none") return(NULL)
  if (source == "manuscript") return(manuscript_fit)
  if (source != "") return(list(same_as = source))
  fit_tas_target(inputs_fn(), model_type, sampler)
}

#' One column's step-01-table result at a site
#'
#' One column's step-01-table result at a site (climate statistics or scenario): fn(tables)
#' unless shared (ts_result_source()).
#'
#' @param column Candidate column name.
#' @param keys A data frame of column and key (ts_tables_key()) for all selected columns.
#' @param manuscript_result The pipeline's own result at the site.
#' @param tables_fn Function of no arguments returning the column's step-01 tables.
#' @param fn Function of the tables computing the result.
#' @return fn()'s result, the manuscript's result, list(same_as = <column>), or NULL.
#' @noRd
ts_tables_column <- function(column, keys, manuscript_result, tables_fn, fn) {
  source <- ts_result_source(column, keys)
  if (source == "none") return(NULL)
  if (source == "manuscript") return(manuscript_result)
  if (source != "") return(list(same_as = source))
  fn(tables_fn())
}

#' Resolve shared results
#'
#' Replace list(same_as = c) by column c's result, within one site's results (named by column).
#'
#' @param results One site's results, named by column.
#' @return `results` with the shared results filled in.
#' @noRd
resolve_same_as <- function(results) {
  lapply(results, function(r) if (is.list(r) && !is.null(r$same_as) && length(r) == 1) results[[r$same_as]] else r)
}

#' Combine the per-column TAS fits of all sites
#'
#' Combine every site's per-column TAS fits into one set of TAS estimates per column: a named
#' list (by column) of list(outcome, siteyear) as combine_tas_sites() returns. `sites`: per
#' site, the list of results in `columns` order.
#'
#' @param sites Per site, the list of results in `columns` order.
#' @param columns The selected column names.
#' @return A list named by column of list(outcome, siteyear).
#' @noRd
combine_ts_fits <- function(sites, columns) {
  sites <- lapply(sites, function(r) resolve_same_as(stats::setNames(r, columns)))
  stats::setNames(lapply(columns, function(column) {
    combine_tas_sites(Filter(Negate(is.null), lapply(sites, `[[`, column)))
  }), columns)
}

#' Combine the per-column rows of all sites
#'
#' Same for one-row-per-site results (climate statistics, scenario): a named list (by column) of
#' data frames.
#'
#' @param sites Per site, the list of results in `columns` order.
#' @param columns The selected column names.
#' @return A list named by column of data frames.
#' @noRd
combine_ts_rows <- function(sites, columns) {
  sites <- lapply(sites, function(r) resolve_same_as(stats::setNames(r, columns)))
  stats::setNames(lapply(columns, function(column) dplyr::bind_rows(lapply(sites, `[[`, column))), columns)
}

#' TAS estimates of every column in one table
#'
#' The TAS estimates of every column in one long table (column, then the outcome table's
#' columns).
#'
#' @param fit_sets combine_ts_fits()'s list.
#' @param which "outcome" or "siteyear": which table to combine.
#' @return A data frame.
#' @noRd
ts_outcome_table <- function(fit_sets, which = c("outcome", "siteyear")) {
  which <- match.arg(which)
  dplyr::bind_rows(lapply(names(fit_sets), function(column) {
    x <- fit_sets[[column]][[which]]
    if (is.null(x) || nrow(x) == 0) return(NULL)
    cbind(column = column, x)
  }))
}
