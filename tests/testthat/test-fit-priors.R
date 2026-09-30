# The priors reach Stan as data, so the Stan code -- and with it the compiled
# model cmdstanr caches by a hash of that code -- does not depend on them.

fake_night <- function() {
  data.frame(NEE = c(1, 2, 3), TS_final = c(5, 10, 15), SWC = c(20, 25, 30),
             NEE_daytime = c(1, 1, 2))
}

test_that("the Stan code is the same whatever the prior values", {
  for (direct in c(FALSE, TRUE)) {
    formula <- if (direct) BRM_FORMULA_DIRECT else BRM_FORMULA_TOTAL
    prior <- if (direct) BRM_PRIORS_DIRECT else BRM_PRIORS_TOTAL
    a <- default_priors(direct)
    b <- recentre_priors(a, c(alpha = 0.0734, C0 = 1.23456789))
    code <- function(p) {
      brms::stancode(formula, data = fake_night(), prior = prior, stanvars = prior_stanvars(p))
    }
    expect_identical(code(a), code(b))
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
  expect_equal(p[c("alpha_sd", "beta_sd", "C0_mu", "C0_sd")], default_priors()[c("alpha_sd", "beta_sd", "C0_mu", "C0_sd")])
})

test_that("every value the templates name is supplied, and nothing else", {
  for (direct in c(FALSE, TRUE)) {
    prior <- if (direct) BRM_PRIORS_DIRECT else BRM_PRIORS_TOTAL
    named <- unique(unlist(regmatches(prior$prior, gregexpr("[A-Za-z0-9]+_(mu|sd)", prior$prior))))
    expect_setequal(named, names(default_priors(direct)))
  }
})
