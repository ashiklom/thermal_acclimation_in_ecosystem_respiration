# The TAS models' Stan programs must not depend on the data or on the prior
# values, so that each model compiles once. Builds the Stan code of the total
# and direct models for two sites with different data and prior means, and
# requires identical code.
#
# Usage (from the repository root, with step-01 outputs of two dev sites (DEV_SITES) in
# data-proc/, or THERMAL_DATA_PROC):
#   Rscript tests/test-stan-code.R

source(file.path("R", "paths.R"))
source(file.path("R", "io.R"))
source(file.path("R", "step02-tas.R"))

site_data <- function(name_site) {
  d <- read_step01_table(name_site, "nightNEE")
  # Only the code is compared, so placeholder covariates will do.
  d$NEE_daytime <- abs(stats::rnorm(nrow(d), 3, 1))
  d$SWC <- stats::runif(nrow(d), 10, 40)
  d[!is.na(d$TS) & !is.na(d$NEE), ]
}
models <- list(
  total = list(
    formula = brms::bf(NEE ~ exp(alpha * TS + beta*TS^2) * C0, alpha+beta+C0 ~ 1, nl = TRUE),
    priors = priors_temp
  ),
  direct = list(
    formula = brms::bf(NEE ~ exp(alpha * TS + beta*TS^2) * SWC / (Hs + SWC) * (C0 + NEE_daytime * k2),
                       alpha+beta+C0+Hs+k2 ~ 1, nl = TRUE),
    priors = priors_temp + priors_water + priors_gpp
  )
)
set.seed(1)
datasets <- list(site_data("US-Tw1"), site_data("AU-Tum")[1:500, ])
values <- list(prior_defaults, prior_defaults * c(1.3, 1, 0.7, 1, 1.1, 1, 2, 1, 0.4, 1))

for (model in names(models)) {
  m <- models[[model]]
  codes <- Map(function(data, pv) {
    brms::stancode(m$formula, data = data, prior = m$priors + prior_sigma,
                   stanvars = tas_stanvars(pv, m$priors, m$formula, data))
  }, datasets, values)
  scales <- vapply(datasets, function(d) default_sigma_scale(m$formula, d), numeric(1))
  stopifnot(identical(codes[[1]], codes[[2]]))
  message(model, ": Stan code identical across data and prior values (sigma scales ",
          paste(scales, collapse = ", "), ")")
}
