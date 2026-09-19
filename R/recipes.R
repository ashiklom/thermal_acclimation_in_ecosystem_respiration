# Methodology recipes.
#
# A recipe is a named set of choices, one strategy per axis, that a pipeline
# run is made under. Sites x recipes is the pipeline's grid, so that the
# manuscript's logic and any number of alternatives to it are produced by the
# same code in the same run and can be compared row for row.
#
# Adding a variant is one row in data-core/recipes.csv. Adding a *strategy*
# is one new branch in the matching `choose_*()` function in R/strategies.R
# and one entry in RECIPE_AXES below. Adding an *axis* is a new column in the
# CSV, a new entry here, and a new `choose_*()` function -- the compiler-ish
# checks in `new_recipe()` and `read_recipes()` will name every place that has
# not caught up.

RECIPES_CSV <- file.path("data-core", "recipes.csv")

# Every axis and the strategies it admits. Order within an axis is not
# meaningful; the first entry is not a default. Defaults live in
# `original_recipe()`, which is the one recipe that has to be right.
RECIPE_AXES <- list(
  # Which soil-temperature column the model is fitted on.
  ts = c("site_info", "screen_best", "memory_fill"),
  # The day-of-year span the 14-day windows tile.
  season = c("detect", "whole_year"),
  # Which population tStart/tEnd -- the window-skip gate -- are percentiles of.
  bounds = c("native", "climatology", "halfhourly"),
  # Which soil-water column the direct model uses.
  swc = c("site_info", "era5"),
  # How years are qualified. One strategy so far; see docs for `computed`.
  year_qc = c("site_info")
)

# The axes whose choice changes what step 01 (`prep_nee_ac()`) produces. Every
# other axis is resolved in step 02, so recipes that agree on these share one
# step-01 result. This is what keeps the grid affordable: step 01 costs
# 30-140 s a site, and step 02 produces every candidate column so that a fit
# can select rather than recompute.
RECIPE_PREP_AXES <- c("year_qc")

# The development sample of recipes, by analogy with `DEV_SITES`: chosen to
# exercise every strategy that has its own code path, not to be exhaustive.
#   original    site_info ts, native bounds, detected season -- the oracle
#   memfill_hh  memory_fill ts (needs the per-site fill), halfhourly bounds
#   noseason    whole_year season
DEV_RECIPES <- c("original", "memfill_hh", "noseason")

# A plain S3 list rather than an S7 class, deliberately. Recipes travel
# through targets' qs2 store and through `tar_map()` values; an S7 object does
# not come back `identical()` from qs2 (the class object is re-created), and a
# named list with a class attribute does. The validation an S7 class would have
# given lives in `validate_recipe()` and runs at construction.
new_recipe <- function(recipe_id, ts, season, bounds, swc, year_qc,
                       description = NA_character_) {
  r <- structure(
    list(
      recipe_id = recipe_id, ts = ts, season = season, bounds = bounds,
      swc = swc, year_qc = year_qc, description = description
    ),
    class = "recipe"
  )
  validate_recipe(r)
}

validate_recipe <- function(r) {
  stopifnot(inherits(r, "recipe"))
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

print.recipe <- function(x, ...) {
  cat("<recipe> ", x$recipe_id, "\n", sep = "")
  for (axis in names(RECIPE_AXES)) cat("  ", format(axis, width = 8), x[[axis]], "\n")
  if (!is.na(x$description)) cat("  ", x$description, "\n")
  invisible(x)
}

# The manuscript's logic, spelled out. `total_tas_site()` uses this when no
# recipe is passed, so every existing caller -- the oracle in
# tests/ts-swc-baseline.R above all -- keeps its meaning.
original_recipe <- function() {
  new_recipe(
    "original",
    ts = "site_info", season = "detect", bounds = "native", swc = "site_info",
    year_qc = "site_info",
    description = "Exactly the manuscript logic."
  )
}

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

# `path` may be a path or an already-read table. The pipeline hands in the
# `format = "file"` target for the CSV so that editing it invalidates exactly
# the fits whose recipe changed.
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
    row$recipe_id, ts = row$ts, season = row$season, bounds = row$bounds,
    swc = row$swc, year_qc = row$year_qc, description = row$description
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

# The part of a recipe that step 01 sees. Two recipes with the same prep key
# can share a `site_data` target.
recipe_prep_key <- function(recipe) {
  paste(vapply(RECIPE_PREP_AXES, function(a) recipe[[a]], ""), collapse = "+")
}

# Which recipes a pipeline run includes, by analogy with `pipeline_sites()`.
#   THERMAL_RECIPES=dev            the development sample (default)
#   THERMAL_RECIPES=all            every row of recipes.csv
#   THERMAL_RECIPES=original,memfill  an explicit list
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

# Which models a run fits.
#   THERMAL_MODELS=total,direct (default) | total | direct
pipeline_models <- function(scope = Sys.getenv("THERMAL_MODELS", "total,direct")) {
  wanted <- trimws(strsplit(scope, ",")[[1]])
  bad <- setdiff(wanted, c("total", "direct"))
  if (length(bad)) stop("THERMAL_MODELS must name `total` and/or `direct`, not ", paste(shQuote(bad), collapse = ", "))
  unique(wanted)
}
