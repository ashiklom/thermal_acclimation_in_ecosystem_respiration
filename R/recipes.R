# Methodology recipes.
#
# A recipe is a named set of choices, one strategy per axis, that a pipeline
# run is made under. Sites x recipes is the pipeline's grid, so that the
# manuscript's logic and any number of alternatives to it are produced by the
# same code in the same run and can be compared row for row.
#
# How to add a recipe, strategy or axis: docs/recipes.md.

#' The recipe table
RECIPES_CSV <- file.path("data-core", "recipes.csv")

#' Every axis and the strategies it admits
#'
#' Order within an axis is not meaningful; the first entry is not a default.
#' Defaults live in `original_recipe()`, which is the one recipe that has to be
#' right.
RECIPE_AXES <- list(
  # Which soil-temperature column the model is fitted on.
  ts = c("site_info", "measured_or_lm", "measured_or_best_fill"),
  # The day-of-year span the moving windows are laid over. `detect_or_override` is the
  # manuscript: the detector's bounds, replaced by the site_info.csv literal
  # wherever one is declared (24 sites). `force_detect` is the detector alone.
  season = c("detect_or_override", "force_detect", "whole_year"),
  # Which population tStart/tEnd -- the window-skip gate -- are percentiles of,
  # when the selected column is the measured one. `climatology_uptake` is the
  # manuscript's: the detector's DOY climatology over the NEE-uptake days.
  ts_bounds_measured = c("climatology_uptake", "climatology", "halfhourly"),
  # The same, when the selected column is an estimate (`TS_linear`,
  # `TS_memfill`). `climatology_uptake` is not offered: it comes out of season
  # detection on the measured column and cannot be computed for any other.
  ts_bounds_estimated = c("climatology", "halfhourly"),
  # Which soil-water column the direct model uses.
  swc = c("site_info", "era5"),
  # How years are qualified. One strategy so far; see docs for `computed`.
  year_qc = c("site_info"),
  # Which soil-temperature column step 01 qualifies years on and screens.
  # `manuscript` is stage A as the manuscript had it -- repairs and
  # reconstructions included; `sensor` is the raw declared sensor wherever
  # the manuscript's arm would have left rows that are not a sensor reading.
  ts_qc = c("manuscript", "sensor")
)

#' The axes that change what step 01 produces
#'
#' Recipes that agree on these share one step-01 result; every other axis is
#' resolved in step 02.
RECIPE_PREP_AXES <- c("year_qc", "ts_qc")

#' The development sample of recipes
#'
#' By analogy with `DEV_SITES`: chosen to exercise every strategy that has its
#' own code path, not to be exhaustive.
#'   original    site_info ts, manuscript bounds (climatology_uptake/halfhourly),
#'               detected season -- the oracle
#'   memfill_hh  measured_or_best_fill ts (needs the per-site fill), halfhourly bounds
#'   noseason    whole_year season
#' `memfill_sensor` is not in the sample: it is the first recipe with its own
#' step 01, so it doubles a run's step-01 cost. THERMAL_RECIPES names it.
DEV_RECIPES <- c("original", "memfill_hh", "noseason")

#' Construct a recipe
#'
#' A plain named list -- no class: nothing dispatches on one, and it is written
#' literally into every fit's command. (Not S7 either: an S7 object does not
#' come back `identical()` from the qs2 store.) Validation runs at construction.
#'
#' The CSV's `description` column is for people (and the variant report, which
#' reads the CSV itself), not part of the recipe: `_targets.R` writes each
#' recipe into its fits' commands, so editing a description invalidates nothing.
#'
#' @param recipe_id Recipe ID: a single lower-case identifier, used in target
#'   names.
#' @param ts Strategy for the `ts` axis; one of `RECIPE_AXES$ts`.
#' @param season Strategy for the `season` axis; one of `RECIPE_AXES$season`.
#' @param ts_bounds_measured Strategy for the `ts_bounds_measured` axis; one of
#'   `RECIPE_AXES$ts_bounds_measured`.
#' @param ts_bounds_estimated Strategy for the `ts_bounds_estimated` axis; one of
#'   `RECIPE_AXES$ts_bounds_estimated`.
#' @param swc Strategy for the `swc` axis; one of `RECIPE_AXES$swc`.
#' @param year_qc Strategy for the `year_qc` axis; one of `RECIPE_AXES$year_qc`.
#' @param ts_qc Strategy for the `ts_qc` axis; one of `RECIPE_AXES$ts_qc`.
#' @return The recipe: a named list of `recipe_id` and one strategy per axis.
new_recipe <- function(recipe_id, ts, season, ts_bounds_measured, ts_bounds_estimated,
                       swc, year_qc, ts_qc) {
  r <- list(
    recipe_id = recipe_id, ts = ts, season = season,
    ts_bounds_measured = ts_bounds_measured, ts_bounds_estimated = ts_bounds_estimated,
    swc = swc, year_qc = year_qc, ts_qc = ts_qc
  )
  validate_recipe(r)
}

#' Check a recipe's ID and that every axis names a known strategy
#'
#' @param r A recipe, as built by `new_recipe()`.
#' @return `r`, unchanged. Errors on an invalid ID or strategy.
validate_recipe <- function(r) {
  stopifnot("a recipe is a list; pass get_recipe(id), not the id" = is.list(r))
  id <- r[["recipe_id"]]
  if (length(id) != 1 || is.na(id) || !grepl("^[a-z][a-z0-9_]*$", id)) {
    stop(
      "recipe_id must be a single lower-case identifier ([a-z][a-z0-9_]*), got ",
      if (length(id) == 1) shQuote(id) else paste0("length ", length(id)),
      ". It is used in target names."
    )
  }
  for (axis in names(RECIPE_AXES)) {
    v <- r[[axis]]
    if (length(v) != 1 || is.na(v) || !v %in% RECIPE_AXES[[axis]]) {
      stop(
        "Recipe ", shQuote(id), ": axis ", shQuote(axis), " is ",
        if (length(v) == 1) shQuote(v) else paste0("length ", length(v)),
        "; must be one of ", paste(shQuote(RECIPE_AXES[[axis]]), collapse = ", "), "."
      )
    }
  }
  r
}

#' The manuscript's logic
#'
#' The default wherever no recipe is passed.
#'
#' @return The `original` recipe.
original_recipe <- function() {
  new_recipe(
    "original",
    ts = "site_info", season = "detect_or_override",
    ts_bounds_measured = "climatology_uptake", ts_bounds_estimated = "halfhourly",
    swc = "site_info", year_qc = "site_info", ts_qc = "manuscript"
  )
}

#' Read and validate the recipe table
#'
#' @param path Path to recipes.csv.
#' @return A tibble of all-character columns: `recipe_id`, one column per
#'   `RECIPE_AXES` axis, and `description`. Errors if a column is missing, an ID
#'   is duplicated, `original` is absent, or any row is not a valid recipe.
read_recipes <- function(path = RECIPES_CSV) {
  cols <- readr::cols(.default = readr::col_character())
  dat <- readr::read_csv(path, col_types = cols, progress = FALSE)
  needed <- c("recipe_id", names(RECIPE_AXES), "description")
  absent <- setdiff(needed, names(dat))
  if (length(absent)) {
    stop(path, " lacks column(s) ", paste(shQuote(absent), collapse = ", "),
         ". Every axis in RECIPE_AXES needs a column.")
  }
  if (anyDuplicated(dat$recipe_id)) {
    stop(path, " has duplicate recipe_id: ",
         paste(shQuote(unique(dat$recipe_id[duplicated(dat$recipe_id)])), collapse = ", "))
  }
  if (!"original" %in% dat$recipe_id) {
    stop(path, " must contain the `original` recipe; it is the oracle every other one is compared to.")
  }
  # Validate every row now, so a typo fails at pipeline definition rather than
  # inside the one target that reaches it.
  for (i in seq_len(nrow(dat))) get_recipe(dat$recipe_id[[i]], dat)
  dat
}

#' Look up a recipe by ID
#'
#' @param recipe_id Recipe ID.
#' @param path Path to recipes.csv. `path` may be a path or an already-read
#'   table.
#' @return The recipe, as built by `new_recipe()`. Errors if `recipe_id` does not
#'   match exactly one row, or if the table's `original` row disagrees with
#'   `original_recipe()`.
get_recipe <- function(recipe_id, path = RECIPES_CSV) {
  dat <- if (is.data.frame(path)) path else {
    readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), progress = FALSE)
  }
  row <- dat[dat$recipe_id == recipe_id, , drop = FALSE]
  if (nrow(row) != 1) {
    stop("Recipe ", shQuote(recipe_id), " not found in recipes (",
         nrow(row), " matches). Available: ", paste(dat$recipe_id, collapse = ", "), ".")
  }
  r <- new_recipe(
    row$recipe_id, ts = row$ts, season = row$season,
    ts_bounds_measured = row$ts_bounds_measured, ts_bounds_estimated = row$ts_bounds_estimated,
    swc = row$swc, year_qc = row$year_qc, ts_qc = row$ts_qc
  )
  # The CSV's `original` row must agree with the code's definition, or the
  # oracle comparison is against the wrong thing.
  if (identical(recipe_id, "original")) {
    ref <- original_recipe()
    for (axis in names(RECIPE_AXES)) {
      if (!identical(r[[axis]], ref[[axis]])) {
        stop("recipes.csv defines `original` with ", axis, " = ", shQuote(r[[axis]]),
             " but original_recipe() says ", shQuote(ref[[axis]]), ". They must agree.")
      }
    }
  }
  r
}

#' The part of a recipe that step 01 sees
#'
#' Two recipes with the same prep key share one step-01 result per site, and
#' the key is the suffix of its target names
#' (`site_data_site_info_manuscript_<site>`), so it is joined with `_`.
#' Strategy names contain `_` too, so two different preps could in principle
#' share a key; `_targets.R` would then fail on a duplicate target name.
#'
#' @param recipe A recipe, or just its `RECIPE_PREP_AXES` entries.
#' @return A single string: the `RECIPE_PREP_AXES` strategies joined with `_`.
recipe_prep_key <- function(recipe) {
  paste(vapply(RECIPE_PREP_AXES, function(a) recipe[[a]], ""), collapse = "_")
}
#' The prep key of the `original` recipe
#'
#' @return A single string, `recipe_prep_key(original_recipe())`.
MANUSCRIPT_PREP_KEY <- function() recipe_prep_key(original_recipe())

#' Which recipes a pipeline run includes
#'
#' By analogy with `pipeline_sites()`.
#'
#' @param scope Which recipes:
#'   THERMAL_RECIPES=dev            the development sample (default)
#'   THERMAL_RECIPES=all            every row of recipes.csv
#'   THERMAL_RECIPES=original,memfill  an explicit list
#' @param path Path to recipes.csv.
#' @return Character vector of recipe IDs. Errors if `scope` names an unknown
#'   recipe.
pipeline_recipes <- function(scope = Sys.getenv("THERMAL_RECIPES", "dev"),
                             path = RECIPES_CSV) {
  available <- read_recipes(path)$recipe_id
  if (identical(scope, "all")) return(available)
  wanted <- if (identical(scope, "dev")) DEV_RECIPES else trimws(strsplit(scope, ",")[[1]])
  unknown <- setdiff(wanted, available)
  if (length(unknown)) {
    stop("THERMAL_RECIPES names recipe(s) not in ", path, ": ",
         paste(shQuote(unknown), collapse = ", "),
         ". Available: ", paste(available, collapse = ", "), ".")
  }
  wanted
}

#' Which models a run fits
#'
#' @param scope Which models:
#'   THERMAL_MODELS=total,direct (default) | total | direct
#' @return Character vector of `MODEL_TYPES` entries, without duplicates.
pipeline_models <- function(scope = Sys.getenv("THERMAL_MODELS", "total,direct")) {
  wanted <- trimws(strsplit(scope, ",")[[1]])
  bad <- setdiff(wanted, MODEL_TYPES)
  if (length(bad)) {
    stop("THERMAL_MODELS must name one or more of ", paste(shQuote(MODEL_TYPES), collapse = ", "),
         ", not ", paste(shQuote(bad), collapse = ", "))
  }
  unique(wanted)
}
