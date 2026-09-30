# Methodology recipes.
#
# A recipe is a named set of choices, one strategy per axis, that a pipeline
# run is made under. Sites x recipes is the pipeline's grid, so that the
# manuscript's logic and any number of alternatives to it are produced by the
# same code in the same run and can be compared row for row.
#
# How to add a recipe, strategy or axis: docs/recipes.md.

RECIPES_CSV <- file.path("data-core", "recipes.csv")

# Every axis and the strategies it admits. Order within an axis is not
# meaningful; the first entry is not a default. Defaults live in
# `original_recipe()`, which is the one recipe that has to be right.
RECIPE_AXES <- list(
  # Which soil-temperature column the model is fitted on.
  ts = c("site_info", "screen_best", "memory_fill"),
  # The day-of-year span the 14-day windows tile. `detect_or_override` is the
  # manuscript: the detector's bounds, replaced by the site_info.csv literal
  # wherever one is declared (24 sites). `force_detect` is the detector alone.
  season = c("detect_or_override", "force_detect", "whole_year"),
  # Which population tStart/tEnd -- the window-skip gate -- are percentiles of.
  bounds = c("native", "climatology", "halfhourly"),
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

# The axes that change what step 01 produces. Recipes that agree on these
# share one step-01 result; every other axis is resolved in step 02.
RECIPE_PREP_AXES <- c("year_qc", "ts_qc")

# The development sample of recipes, by analogy with `DEV_SITES`: chosen to
# exercise every strategy that has its own code path, not to be exhaustive.
#   original    site_info ts, native bounds, detected season -- the oracle
#   memfill_hh  memory_fill ts (needs the per-site fill), halfhourly bounds
#   noseason    whole_year season
# `memfill_sensor` is not in the sample: it is the first recipe with its own
# step 01, so it doubles a run's step-01 cost. THERMAL_RECIPES names it.
DEV_RECIPES <- c("original", "memfill_hh", "noseason")

# A plain named list -- no class: nothing dispatches on one, and it is written
# literally into every fit's command. (Not S7 either: an S7 object does not
# come back `identical()` from the qs2 store.) Validation runs at construction.
#
# The CSV's `description` column is for people (and the variant report, which
# reads the CSV itself), not part of the recipe: `_targets.R` writes each
# recipe into its fits' commands, so editing a description invalidates nothing.
new_recipe <- function(recipe_id, ts, season, bounds, swc, year_qc, ts_qc) {
  r <- list(
    recipe_id = recipe_id, ts = ts, season = season, bounds = bounds,
    swc = swc, year_qc = year_qc, ts_qc = ts_qc
  )
  validate_recipe(r)
}

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

# The manuscript's logic; the default wherever no recipe is passed.
original_recipe <- function() {
  new_recipe(
    "original",
    ts = "site_info", season = "detect_or_override", bounds = "native", swc = "site_info",
    year_qc = "site_info", ts_qc = "manuscript"
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

# `path` may be a path or an already-read table.
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

# The part of a recipe that step 01 sees. Two recipes with the same prep key
# share one step-01 result per site, and the key is the suffix of its target
# names (`site_data_site_info_manuscript_<site>`), so it is joined with `_`.
# Strategy names contain `_` too, so two different preps could in principle
# share a key; `_targets.R` would then fail on a duplicate target name.
recipe_prep_key <- function(recipe) {
  paste(vapply(RECIPE_PREP_AXES, function(a) recipe[[a]], ""), collapse = "_")
}
MANUSCRIPT_PREP_KEY <- function() recipe_prep_key(original_recipe())

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
