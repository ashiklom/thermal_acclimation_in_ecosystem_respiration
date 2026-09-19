# Soil-temperature reconstruction methods, and the blocked cross-validation
# that scores them.
#
# Kept in `scripts/` rather than `R/` for now, deliberately: nothing in the
# targets pipeline calls it yet, and every edit to `R/` invalidates the
# step-01 caches these analyses depend on (70-140 s a site to rebuild, 66
# sites). It moves into `R/` when it replaces `fix_soil_temp()`.

suppressMessages({
  library(dplyr)
})

# ------------------------------------------------------------------ features
#
# The single largest deficiency of the methods already in the pipeline is that
# they are *instantaneous*: `TS ~ TA` and `randomForest(TS ~ TA + NETRAD)` map
# the air temperature at time t onto the soil temperature at time t, with no
# memory at all. Shallow soil temperature is a damped, phase-lagged integral of
# the surface forcing -- that is what the heat equation says and what the
# measured amplitude ratios confirm -- so an instantaneous map structurally
# cannot represent it. Whatever lag it appears to capture comes from the
# diurnal and seasonal cycles being correlated with themselves.
#
# Giving the model running means over 1, 3, 7 and 30 days supplies exactly the
# missing memory, at essentially no cost, using columns that are already there.
# The harmonics let it place a residual seasonal and diurnal offset.
#
# Running means are computed on a *daily* series completed over the full date
# range, not on half-hourly row offsets. A record with missing rows -- which
# every one of these has, because step 01 drops disqualified years -- would
# otherwise have a "30-day" mean spanning however many calendar days those
# 1440 rows happened to cover.
ts_fill_add_features <- function(dat) {
  need <- c("YEAR", "MONTH", "DAY", "DOY", "HOUR", "TA")
  absent <- setdiff(need, names(dat))
  if (length(absent)) stop("ts_fill_add_features needs ", paste(absent, collapse = ", "))

  dat$date <- as.Date(sprintf("%04d-%02d-%02d", dat$YEAR, dat$MONTH, dat$DAY))

  denan <- function(x) replace(x, is.nan(x), NA_real_)
  amp1 <- function(x) if (sum(!is.na(x)) >= 4) diff(range(x, na.rm = TRUE)) else NA_real_

  daily <- dat |>
    dplyr::summarise(
      TA_day = denan(mean(.data$TA, na.rm = TRUE)),
      TA_amp = amp1(.data$TA),
      .by = "date"
    ) |>
    dplyr::arrange(.data$date)

  full <- tibble::tibble(date = seq(min(daily$date), max(daily$date), by = "day"))
  daily <- dplyr::left_join(full, daily, by = "date")

  roll <- function(x, k) {
    denan(zoo::rollapply(
      x, width = k, FUN = function(v) mean(v, na.rm = TRUE),
      align = "right", fill = NA_real_, partial = TRUE
    ))
  }
  daily$TA_m3 <- roll(daily$TA_day, 3)
  daily$TA_m7 <- roll(daily$TA_day, 7)
  daily$TA_m30 <- roll(daily$TA_day, 30)
  # Yesterday's daily mean and amplitude: the most direct expression of the
  # one-day memory, and cheap.
  daily$TA_lag1 <- dplyr::lag(daily$TA_day)
  daily$TA_amp_lag1 <- dplyr::lag(daily$TA_amp)

  dat <- dplyr::left_join(
    dat,
    dplyr::select(daily, "date", "TA_day", "TA_lag1", "TA_amp_lag1",
                  "TA_m3", "TA_m7", "TA_m30"),
    by = "date"
  )

  th_d <- 2 * pi * dat$DOY / 365.25
  th_h <- 2 * pi * dat$HOUR / 24
  dat$doy_s1 <- sin(th_d);     dat$doy_c1 <- cos(th_d)
  dat$doy_s2 <- sin(2 * th_d); dat$doy_c2 <- cos(2 * th_d)
  dat$hr_s1 <- sin(th_h);      dat$hr_c1 <- cos(th_h)
  dat
}

TS_FILL_FEATURES_MEMORY <- c(
  "TA", "TA_day", "TA_lag1", "TA_amp_lag1", "TA_m3", "TA_m7", "TA_m30",
  "doy_s1", "doy_c1", "doy_s2", "doy_c2", "hr_s1", "hr_c1"
)

# ------------------------------------------------------------------- methods
#
# Each method is fit(train) -> model and predict(model, newdata) -> numeric of
# length nrow(newdata), NA wherever the predictors are not available.
#
# `lm_ta_pos` is the pipeline's current default, reproduced exactly: fit
# `TS ~ TA` on rows above freezing, predict everywhere. `rf_ta` and
# `rf_ta_netrad` are `predict_soil_temp()`'s two branches.
#
# NOTE on the random-forest implementation. `predict_soil_temp()` uses
# `randomForest`; these use `ranger`, which is the same algorithm and is
# already a project dependency, but is an order of magnitude faster and is
# what makes 66 sites x 6 methods x 3 blocking schemes affordable. The
# comparison here is between *methods*, and both branches of it use the same
# implementation, so the substitution does not favour either. It does mean
# these numbers are not bit-identical to what the pipeline would produce.

lm_method <- function(preds, fit_subset = NULL) {
  list(
    predictors = preds,
    fit = function(train) {
      if (!is.null(fit_subset)) train <- train[fit_subset(train), , drop = FALSE]
      stats::lm(stats::reformulate(preds, "TS"), data = train, na.action = stats::na.omit)
    },
    predict = function(mod, newdata) {
      as.numeric(stats::predict(mod, newdata = newdata, na.action = stats::na.pass))
    }
  )
}

rf_method <- function(preds, num_trees = 200) {
  list(
    predictors = preds,
    fit = function(train) {
      ok <- stats::complete.cases(train[, c("TS", preds), drop = FALSE])
      if (sum(ok) < 50) stop("fewer than 50 complete training rows")
      ranger::ranger(
        x = as.data.frame(train[ok, preds, drop = FALSE]),
        y = train$TS[ok],
        num.trees = num_trees,
        num.threads = 1,
        seed = 222
      )
    },
    predict = function(mod, newdata) {
      # ranger has no NA handling, so rows with an incomplete feature vector
      # get NA rather than being silently dropped -- which would misalign the
      # prediction with the rows it belongs to.
      out <- rep(NA_real_, nrow(newdata))
      ok <- stats::complete.cases(newdata[, mod$forest$independent.variable.names, drop = FALSE])
      if (any(ok)) {
        out[ok] <- stats::predict(
          mod, data = as.data.frame(newdata[ok, , drop = FALSE]), num.threads = 1
        )$predictions
      }
      out
    }
  )
}

ts_fill_methods <- function(have_netrad = FALSE) {
  m <- list(
    # The pipeline's current default, exactly: fitted above freezing only.
    lm_ta_pos = lm_method("TA", fit_subset = function(d) !is.na(d$TA) & d$TA > 0),
    lm_ta = lm_method("TA"),
    rf_ta = rf_method("TA"),
    # The cheap fix: same model class, plus the memory the physics requires.
    lm_memory = lm_method(TS_FILL_FEATURES_MEMORY),
    rf_memory = rf_method(TS_FILL_FEATURES_MEMORY)
  )
  if (have_netrad) {
    m$lm_ta_netrad <- lm_method(c("TA", "NETRAD"))
    m$rf_ta_netrad <- rf_method(c("TA", "NETRAD"))
    m$rf_memory_netrad <- rf_method(c(TS_FILL_FEATURES_MEMORY, "NETRAD"))
  }
  m
}

# ------------------------------------------------------------------- blocking
#
# `predict_soil_temp()` splits half-hourly rows 70/30 at *random*. Half-hourly
# soil temperature is autocorrelated on a scale of days, so a held-out row's
# immediate neighbours are in the training set and the reported skill measures
# interpolation between adjacent half-hours. The model is used to reconstruct
# months to years at a stretch. These schemes hold out contiguous blocks that
# resemble the gap actually being filled.
#
# `random` is kept in the list on purpose: running it alongside the blocked
# schemes is what turns "this split is optimistic" from an assertion into a
# measurement.

blocks_random <- function(dat, nfold = 5, seed = 222) {
  set.seed(seed)
  split(seq_len(nrow(dat)), sample(rep_len(seq_len(nfold), nrow(dat))))
}

blocks_year <- function(dat) {
  split(seq_len(nrow(dat)), dat$YEAR)
}

blocks_season <- function(dat) {
  # Contiguous three-month blocks within a year. Leaving out a whole season
  # asks the question the snow-covered and frozen periods actually pose: can
  # the model reach a regime it never saw in that year?
  split(seq_len(nrow(dat)), paste(dat$YEAR, (dat$MONTH - 1) %/% 3))
}

blocks_multiyear <- function(dat, k = 2) {
  yrs <- sort(unique(dat$YEAR))
  if (length(yrs) < 2 * k) return(blocks_year(dat))
  grp <- setNames(((seq_along(yrs) - 1) %/% k), yrs)
  split(seq_len(nrow(dat)), grp[as.character(dat$YEAR)])
}

TS_FILL_BLOCKINGS <- list(
  random = blocks_random,
  year = blocks_year,
  season = blocks_season,
  multiyear = function(dat) blocks_multiyear(dat, k = 2)
)

# --------------------------------------------------------------------- the CV
#
# Returns one out-of-fold prediction per row. Assembling the folds into a
# single complete series is the point: it is a simulation of "what if this
# site's soil temperature had had to be reconstructed", and it can then be
# scored with exactly the metrics used to score `TS_linear` against measured
# soil temperature, so the existing method is one row of the same table.
ts_fill_oof <- function(dat, method, blocks) {
  out <- rep(NA_real_, nrow(dat))
  for (idx in blocks) {
    train <- dat[-idx, , drop = FALSE]
    if (sum(!is.na(train$TS)) < 50) next
    mod <- tryCatch(method$fit(train), error = function(e) NULL)
    if (is.null(mod)) next
    p <- tryCatch(method$predict(mod, dat[idx, , drop = FALSE]), error = function(e) NULL)
    if (is.null(p) || length(p) != length(idx)) next
    out[idx] <- p
  }
  out
}
