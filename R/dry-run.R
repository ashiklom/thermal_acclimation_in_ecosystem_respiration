# Dry-run mode for the TAS fits: record every brms::brm() call instead of
# sampling.
#
# Activated by setting the environment variable TAS_DRY_RUN to an output
# directory and sourcing this file before the fits run. It replaces
# brms::brm(), brms::fixef() and the brms methods the fitting code uses with
# deterministic stand-ins:
#
# - brm() appends one row per call to a call log (below): the site,
#   the call's sequence number within the site, the number of rows, a hash of
#   the model data (only the columns the formula uses, row names dropped),
#   the numeric prior means and SDs, and the sampler settings.
# - fixef() returns the prior means, so prior updates downstream of a fit
#   stay deterministic.
# - fitted() returns the mean NEE of the fitted data (Est.Error 0), with or
#   without newdata, so reference respiration and the site-level regression
#   still run.
# - nuts_params() reports no divergences.
#
# Each R process writes its own log, <TAS_DRY_RUN>/brm-calls-<pid>.tsv, so
# sites can run in parallel. The call logs are the fit-input "fingerprint"
# used to show that a refactor leaves the fits' inputs unchanged without
# running any MCMC.

local({
  out_dir <- Sys.getenv("TAS_DRY_RUN")
  stopifnot(nzchar(out_dir))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  log_file <- file.path(out_dir, sprintf("brm-calls-%d.tsv", Sys.getpid()))
  state <- new.env()
  state$counter <- list()

  # Numeric means and SDs from a brmsprior data frame: either literal
  # "normal(m, s)" strings, or symbols resolved through stanvars.
  prior_values <- function(prior, stanvars) {
    sv <- list()
    if (!is.null(stanvars)) {
      for (v in stanvars) sv[[v$name]] <- v$sdata
    }
    resolve <- function(x) {
      x <- trimws(x)
      num <- suppressWarnings(as.numeric(x))
      if (!is.na(num)) return(num)
      if (!is.null(sv[[x]])) return(as.numeric(sv[[x]]))
      stop("dry run: cannot resolve prior argument '", x, "'")
    }
    rows <- prior[nzchar(prior$prior), ]
    vals <- lapply(seq_len(nrow(rows)), function(i) {
      args <- strsplit(sub("^[a-z_]+\\((.*)\\)$", "\\1", rows$prior[i]), ",")[[1]]
      name <- if (nzchar(rows$nlpar[i])) rows$nlpar[i] else rows$class[i]
      bounds <- suppressWarnings(as.numeric(c(rows$lb[i], rows$ub[i])))
      c(
        stats::setNames(vapply(args, resolve, numeric(1)), paste0(name, "_", seq_along(args))),
        stats::setNames(bounds, paste0(name, c("_lb", "_ub")))
      )
    })
    vals <- unlist(vals)
    vals[order(names(vals))]
  }

  fake_brm <- function(formula, data, prior = NULL, stanvars = NULL, iter = 2000,
                       chains = 4, control = list(), seed = NA, ...) {
    site <- tryCatch(dynGet("name_site", ifnotfound = NULL), error = function(e) NULL) %||%
      get0("name_site", envir = globalenv(), ifnotfound = NA_character_)
    site <- as.character(site)[1]
    key <- if (is.na(site)) "_" else site
    state$counter[[key]] <- (state$counter[[key]] %||% 0L) + 1L
    vars <- intersect(all.vars(formula$formula), names(data))
    model_data <- as.data.frame(data)[, sort(vars), drop = FALSE]
    rownames(model_data) <- NULL
    pv <- prior_values(prior, stanvars)
    record <- data.frame(
      site = site,
      seq = state$counter[[key]],
      nrow = nrow(model_data),
      data_hash = digest::digest(model_data, algo = "xxhash64"),
      priors = paste(sprintf("%s=%.17g", names(pv), pv), collapse = ";"),
      iter = iter,
      chains = chains,
      adapt_delta = control$adapt_delta %||% NA_real_,
      max_treedepth = control$max_treedepth %||% NA_real_,
      seed = if (is.null(seed)) NA else seed
    )
    utils::write.table(record, log_file, sep = "\t", row.names = FALSE, quote = FALSE,
                       append = file.exists(log_file), col.names = !file.exists(log_file))
    means <- pv[grepl("_1$", names(pv))]
    names(means) <- sub("_1$", "", names(means))
    means <- means[setdiff(names(means), "sigma")]
    structure(list(means = means, nee = mean(data$NEE, na.rm = TRUE), n = nrow(data)),
              class = "dry_run_fit")
  }

  utils::assignInNamespace("brm", fake_brm, ns = "brms")
  if ("package:brms" %in% search()) {
    env <- as.environment("package:brms")
    unlockBinding("brm", env)
    assign("brm", fake_brm, envir = env)
    lockBinding("brm", env)
  }

  # fixef(), nuts_params() and fitted() are S3 generics (from nlme, bayesplot
  # and stats); give them methods for the stand-in fit class.
  registerS3method("fixef", "dry_run_fit", function(object, ...) {
    est <- object$means
    m <- cbind(Estimate = est, Est.Error = 0, Q2.5 = est, Q97.5 = est)
    rownames(m) <- paste0(names(est), "_Intercept")
    m
  }, envir = asNamespace("nlme"))
  registerS3method("nuts_params", "dry_run_fit", function(object, ...) {
    data.frame(Chain = 1L, Iteration = 1L, Parameter = "divergent__", Value = 0)
  }, envir = asNamespace("bayesplot"))
  registerS3method("fitted", "dry_run_fit", function(object, newdata = NULL, ...) {
    n <- if (is.null(newdata)) object$n else nrow(newdata)
    cbind(Estimate = rep(object$nee, n), Est.Error = 0, Q2.5 = object$nee, Q97.5 = object$nee)
  }, envir = asNamespace("stats"))
})
