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

# Hash of a column's TAS inputs (NA if the column has none at the site).
ts_inputs_key <- function(inputs) {
  if (is.null(inputs)) NA_character_ else digest::digest(inputs)
}

# A column's step-01 tables (ts_column_tables()), for the step 03 climate
# statistics and the step 04 scenario: step 01's own for TS_manuscript (the
# manuscript ran both on step 01's tables, not on step 02's regressed soil
# temperature). NULL if the column has no nighttime record.
ts_step01_tables <- function(column, step01, candidates) {
  if (column == "TS_manuscript") {
    return(list(ac = step01$ac, night = step01$night, feature = step01$feature))
  }
  if (!any(step01$ts_base$night_ok & candidates$ok[[column]])) return(NULL)
  tables <- ts_column_tables(step01$ts_base, step01$ts_params, candidates$values[[column]], candidates$ok[[column]])
  if (nrow(tables$night) == 0) return(NULL)
  tables[c("ac", "night", "feature")]
}

ts_tables_key <- function(tables) {
  if (is.null(tables)) NA_character_ else digest::digest(list(tables$ac, tables$night, tables$feature[, c("gStart", "gEnd")]))
}

# Which result a column's result is: "manuscript" (the manuscript's), "none"
# (no input), the name of an earlier column with the same input, or "" (its
# own). `keys` is a data frame of column and key for all selected columns, in
# order.
ts_result_source <- function(column, keys) {
  key <- keys$key[keys$column == column]
  if (is.na(key)) return("none")
  if (identical(key, keys$key[keys$column == "TS_manuscript"])) return("manuscript")
  first <- keys$column[match(key, keys$key)]
  if (first != column) first else ""
}

# One column's TAS fit at a site.
ts_fit_column <- function(column, model_type, keys, manuscript_fit, inputs_fn, sampler) {
  source <- ts_result_source(column, keys)
  if (source == "none") return(NULL)
  if (source == "manuscript") return(manuscript_fit)
  if (source != "") return(list(same_as = source))
  fit_tas_target(inputs_fn(), model_type, sampler)
}

# One column's step-01-table result at a site (climate statistics or
# scenario): fn(tables) unless shared.
ts_tables_column <- function(column, keys, manuscript_result, tables_fn, fn) {
  source <- ts_result_source(column, keys)
  if (source == "none") return(NULL)
  if (source == "manuscript") return(manuscript_result)
  if (source != "") return(list(same_as = source))
  fn(tables_fn())
}

# Replace list(same_as = c) by column c's result, within one site's results
# (named by column).
resolve_same_as <- function(results) {
  lapply(results, function(r) if (is.list(r) && !is.null(r$same_as) && length(r) == 1) results[[r$same_as]] else r)
}

# Combine every site's per-column TAS fits into one set of TAS estimates per
# column: a named list (by column) of list(outcome, siteyear) as
# combine_tas_sites() returns. `sites`: per site, the list of results in
# `columns` order.
combine_ts_fits <- function(sites, columns) {
  sites <- lapply(sites, function(r) resolve_same_as(stats::setNames(r, columns)))
  stats::setNames(lapply(columns, function(column) {
    combine_tas_sites(Filter(Negate(is.null), lapply(sites, `[[`, column)))
  }), columns)
}

# Same for one-row-per-site results (climate statistics, scenario): a named
# list (by column) of data frames.
combine_ts_rows <- function(sites, columns) {
  sites <- lapply(sites, function(r) resolve_same_as(stats::setNames(r, columns)))
  stats::setNames(lapply(columns, function(column) dplyr::bind_rows(lapply(sites, `[[`, column))), columns)
}

# The TAS estimates of every column in one long table (column, then the
# outcome table's columns).
ts_outcome_table <- function(fit_sets, which = c("outcome", "siteyear")) {
  which <- match.arg(which)
  dplyr::bind_rows(lapply(names(fit_sets), function(column) {
    x <- fit_sets[[column]][[which]]
    if (is.null(x) || nrow(x) == 0) return(NULL)
    cbind(column = column, x)
  }))
}
