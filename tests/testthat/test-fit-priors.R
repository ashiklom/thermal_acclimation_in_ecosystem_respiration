# The priors reach Stan as data, so the Stan code -- and with it the compiled
# model cmdstanr caches by a hash of that code -- depends on neither the
# priors nor the data.

fake_night <- function(nee = c(1, 2, 3, 8)) {
  data.frame(NEE = nee, TS_final = seq(5, 20, length.out = length(nee)),
             SWC = 25, NEE_daytime = 1)
}

stan_code <- function(priors, data, direct) {
  brms::stancode(
    if (direct) BRM_FORMULA_DIRECT else BRM_FORMULA_TOTAL,
    data = data,
    prior = if (direct) BRM_PRIORS_DIRECT else BRM_PRIORS_TOTAL,
    stanvars = prior_stanvars(priors, data, direct)
  )
}

test_that("the Stan code is the same whatever the prior values and data", {
  for (direct in c(FALSE, TRUE)) {
    a <- default_priors(direct)
    b <- recentre_priors(a, c(alpha = 0.0734, C0 = 1.23456789))
    # The second response has a MAD well above 2.5, so brms's own default
    # sigma prior would differ between the two.
    expect_identical(
      stan_code(a, fake_night(), direct),
      stan_code(b, fake_night(c(1, 12, 30, 55, 90)), direct)
    )
  }
})

test_that("the sigma scale is brms's own default", {
  for (nee in list(c(1, 2, 3, 8), c(1, 12, 30, 55, 90), c(0.5, 0.6, NA, 20, 40))) {
    d <- fake_night(nee)
    for (direct in c(FALSE, TRUE)) {
      formula <- if (direct) BRM_FORMULA_DIRECT else BRM_FORMULA_TOTAL
      dp <- suppressWarnings(brms::default_prior(formula, data = d))  # the NA row
      brms_prior <- dp$prior[dp$class == "sigma"]
      expect_identical(brms_prior, sprintf("student_t(3, 0, %s)", default_sigma_scale(d, direct)))
    }
  }
})

test_that("the default priors are the manuscript's", {
  expect_equal(
    default_priors(),
    c(C0_mu = 2, C0_sd = 5, alpha_mu = 0.1, alpha_sd = 1, beta_mu = -0.001, beta_sd = 0.1)
  )
  expect_equal(default_priors(TRUE)[c("Hs_mu", "Hs_sd", "k2_mu", "k2_sd")],
               c(Hs_mu = 10, Hs_sd = 10, k2_mu = 0.5, k2_sd = 2))
})

test_that("re-centring moves the means and leaves the SDs", {
  p <- recentre_priors(default_priors(), c(alpha = 0.05, beta = -0.002))
  expect_equal(p[["alpha_mu"]], 0.05)
  expect_equal(p[["beta_mu"]], -0.002)
  kept <- c("alpha_sd", "beta_sd", "C0_mu", "C0_sd")
  expect_equal(p[kept], default_priors()[kept])
})

test_that("every value the templates name is supplied, and nothing else", {
  for (direct in c(FALSE, TRUE)) {
    prior <- if (direct) BRM_PRIORS_DIRECT else BRM_PRIORS_TOTAL
    named <- unique(unlist(regmatches(prior$prior, gregexpr("[A-Za-z0-9]+_(mu|sd|scale)", prior$prior))))
    supplied <- vapply(prior_stanvars(default_priors(direct), fake_night(), direct), `[[`, "", "name")
    expect_setequal(named, supplied)
  }
})
