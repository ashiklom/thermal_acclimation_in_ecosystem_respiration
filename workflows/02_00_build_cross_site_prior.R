# Build persisted cross-site priors for the total ecosystem-respiration model.

library(librarian)
shelf(dplyr, gslnls, brms)
rm(list = ls())

args <- commandArgs(trailingOnly = TRUE)
site_arg <- args[grepl('^--sites=', args)]
positional_sites <- args[!grepl('^--', args)]
if (length(site_arg) > 1 || length(positional_sites) > 1) stop('Provide at most one comma-separated site list.')
requested_sites <- if (length(site_arg) == 1) trimws(unlist(strsplit(sub('^--sites=', '', site_arg), ','))) else if (length(positional_sites) == 1) trimws(unlist(strsplit(positional_sites, ','))) else NULL

site_info <- read.csv(file.path('data-core', 'site_info.csv'))
source(file.path('workflows', 'load_growing_season_features.R'))
feature_gs <- load_growing_season_features()
respiration_dir <- file.path('data-proc', 'respiration')
night_files <- list.files(respiration_dir, pattern = '_nightNEE\\.csv$', full.names = TRUE, recursive = TRUE)
night_file_for_site <- function(name_site) {
  night_files[basename(night_files) == paste0(name_site, '_nightNEE.csv')]
}
available_sites <- site_info$site_ID[vapply(site_info$site_ID, function(site) length(night_file_for_site(site)) == 1, logical(1))]
sites <- if (is.null(requested_sites)) available_sites else requested_sites
unknown_sites <- setdiff(sites, available_sites)
if (length(unknown_sites) > 0) stop('Missing respiration input for: ', paste(unknown_sites, collapse = ', '))
if (!length(sites)) stop('No respiration inputs are available.')
site_rows <- site_info[match(sites, site_info$site_ID), , drop = FALSE]
feature_rows <- feature_gs[match(sites, feature_gs$site_ID), , drop = FALSE]
site_rows$gStart <- feature_rows$gStart
site_rows$gEnd <- feature_rows$gEnd
missing_bounds <- sites[is.na(site_rows$gStart) | is.na(site_rows$gEnd)]
if (length(missing_bounds) > 0 && is.null(requested_sites)) {
  warning('Skipping sites without source-specific growing-season features: ', paste(missing_bounds, collapse = ', '))
  sites <- setdiff(sites, missing_bounds)
  site_rows <- site_rows[!site_rows$site_ID %in% missing_bounds, , drop = FALSE]
}
if (length(missing_bounds) > 0 && !is.null(requested_sites)) stop('Growing-season bounds are missing for: ', paste(missing_bounds, collapse = ', '))
if (!length(sites)) stop('No sites have source-specific growing-season features.')

baseline <- list(
  C0 = brms::prior('normal(2, 5)', nlpar = 'C0', lb = 0, ub = 10),
  alpha = brms::prior('normal(0.1, 1)', nlpar = 'alpha', lb = 0, ub = 0.2),
  beta = brms::prior('normal(-0.001, 0.1)', nlpar = 'beta', lb = -0.01, ub = 0)
)
frmu_nls <- NEE ~ exp(exp(alpha_ln) * TS - exp(beta_ln) * TS^2) * exp(C0_ln)
stprm <- c(C0_ln = 0.7, alpha_ln = -2.99, beta_ln = -6.9)
window_size <- 14
max_windows <- max(pmax(1, ceiling((site_rows$gEnd - site_rows$gStart + 1) / window_size)))

prior_rows <- vector('list', max_windows)
for (iwindow in seq_len(max_windows)) {
  pooled <- lapply(sites, function(name_site) {
    x <- read.csv(night_file_for_site(name_site))
    info <- site_rows[match(name_site, site_rows$site_ID), ]
    start <- info$gStart + window_size * (iwindow - 1)
    end <- min(info$gStart + window_size * iwindow, info$gEnd)
    if (start > info$gEnd || !all(c('DOY', 'TS', 'NEE') %in% names(x))) return(NULL)
    x %>% filter(between(DOY, start, end), is.finite(TS), is.finite(NEE)) %>% select(TS, NEE)
  }) %>% bind_rows()

  priors <- baseline
  if (nrow(pooled) >= 100) {
    fit <- try(gsl_nls(fn = frmu_nls, data = pooled, start = stprm), silent = TRUE)
    if (!inherits(fit, 'try-error')) {
      estimates <- coefficients(fit)
      priors$alpha <- brms::set_prior(paste0('normal(', min(exp(estimates['alpha_ln']), 0.2), ', 1)'), nlpar = 'alpha', lb = 0, ub = 0.2)
      priors$beta <- brms::set_prior(paste0('normal(', -min(exp(estimates['beta_ln']), 0.01), ', 0.1)'), nlpar = 'beta', lb = -0.01, ub = 0)
      priors$C0 <- brms::set_prior(paste0('normal(', min(exp(estimates['C0_ln']), 10), ', 5)'), nlpar = 'C0', lb = 0, ub = 10)
    }
  }
  prior_rows[[iwindow]] <- list(window = iwindow, nobs = nrow(pooled), priors = priors)
}

dir.create('data-proc/analysis', recursive = TRUE, showWarnings = FALSE)
saveRDS(list(model = 'total', window_size = window_size, sites = sites, priors = prior_rows), file.path('data-proc/analysis', 'cross_site_priors_total.rds'))
