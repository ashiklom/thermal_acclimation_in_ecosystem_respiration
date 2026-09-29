# Compare the total, direct and apparent (total - direct) thermal response
# strengths, overall and by vegetation and climate class. Prints the tests;
# writes nothing.
# Authors: Junna Wang, October, 2025

stopifnot(requireNamespace("dplyr"), requireNamespace("ggplot2"), requireNamespace("tidyr"))

outcome_temp <- read.csv(file.path("data-proc", "analysis", "outcome_temp.csv"))
outcome_temp_water_gpp <- read.csv(file.path("data-proc", "analysis", "outcome_temp_water_gpp.csv"))

# Join on site_ID: the two tables come from separate targets and can hold
# different sites. Inner, because everything below is a within-site contrast.
total_tas <- outcome_temp |>
  dplyr::select(site_ID, TAS_tot = TAS, TAS_totp = TASp)
direct_tas <- outcome_temp_water_gpp |>
  dplyr::select(site_ID, TAS, TASp)
outcome <- dplyr::inner_join(total_tas, direct_tas, by = "site_ID")

# Loud: the manuscript's tests are all on 117 sites.
dropped <- setdiff(union(total_tas$site_ID, direct_tas$site_ID), outcome$site_ID)
if (length(dropped)) {
  warning(
    length(dropped), " site(s) fitted under only one model and are excluded: ",
    paste(dropped, collapse = ", "), ". n = ", nrow(outcome), " sites.",
    call. = FALSE
  )
}

outcome$TAS_app <- outcome$TAS_tot - outcome$TAS

site_info <- read.csv(file.path("data-core", "site_info.csv"))
outcome <- outcome |>
  dplyr::left_join(site_info[, c("site_ID", "IGBP", "Climate_class")], by = "site_ID")

outcome_long <- outcome |>
  tidyr::pivot_longer(cols = c("TAS_tot", "TAS", "TAS_app"), names_to = "TAS_type", values_to = "TAS_value")

# comparison of different thermal response strengths
if (interactive()) {
  ggplot2::ggplot(outcome_long, ggplot2::aes(x = TAS_type, y = TAS_value)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_bw()
}

# One-sample t-test p-values of each TAS against zero, within each class of
# `group` that has at least three sites.
class_tests <- function(outcome, group) {
  p_value <- function(x) if (length(x) >= 3) t.test(x, mu = 0)$p.value else NA_real_
  outcome |>
    dplyr::summarise(
      n = dplyr::n(),
      total0 = p_value(TAS_tot),
      direct0 = p_value(TAS),
      app0 = p_value(TAS_app),
      .by = dplyr::all_of(group)
    )
}
df.TAS.IGBP <- class_tests(outcome, "IGBP")

t.test(outcome$TAS_tot, mu = 0)
range(outcome$TAS_tot)
sum(outcome$TAS_tot < 0)
sum(outcome$TAS_tot > 0)
sum(outcome$TAS_tot < 0 & outcome$TAS_totp < 0.05)
sum(outcome$TAS_tot > 0 & outcome$TAS_totp < 0.05)

t.test(outcome$TAS, mu = 0)
sum(outcome$TAS > 0 & outcome$TASp < 0.05)
sum(outcome$TAS < 0 & outcome$TASp < 0.05)

t.test(outcome$TAS_app, mu = 0)
sum(outcome$TAS_app < 0)

t.test(outcome$TAS_tot[outcome$IGBP %in% c("OSH", "SAV", "WSA")], mu = 0)
t.test(outcome$TAS_tot[outcome$IGBP %in% c("GRA", "ENF")], mu = 0)
t.test(outcome$TAS_tot[outcome$IGBP %in% c("WET")], mu = 0)
t.test(outcome$TAS_tot[outcome$IGBP %in% c("EBF", "DBF", "MF", "DNF", "CSH")], mu = 0)

# forests, shrublands and wetlands
t.test(outcome$TAS[outcome$IGBP %in% c("DBF", "EBF", "MF", "DNF", "CSH", "WET")], mu = 0)
t.test(outcome$TAS[outcome$IGBP %in% c("WET")], mu = 0)
t.test(outcome$TAS_app[outcome$IGBP %in% c("WET")], mu = 0)

df.TAS.climate <- class_tests(outcome, "Climate_class") |>
  dplyr::rename(climate = Climate_class)

t.test(outcome$TAS_tot[outcome$Climate_class %in% c("Csa", "Csb")], mu = 0)
t.test(outcome$TAS_tot[outcome$Climate_class %in% c("Bsh", "Bsk", "Bwk")], mu = 0)

# look at total response
t.test(outcome$TAS_tot[outcome$Climate_class %in% c("Cfa", "Cfb", "Cfc")], mu = 0)
t.test(outcome$TAS_tot[outcome$Climate_class %in% c("Dfa", "Dfb")], mu = 0)
t.test(outcome$TAS_tot[outcome$Climate_class %in% c("Dfc", "Dfd", "Dwc")], mu = 0)
t.test(outcome$TAS_tot[outcome$Climate_class %in% c("Cfa", "Cfb", "Cfc", "Dfa", "Dfb", "Dfc", "Dfd", "Dwc", "Csa", "Csb", "ET", "Af", "Am")], mu = 0)

# How the variance of total TAS splits between direct and apparent.
var(outcome$TAS_tot)
var(outcome$TAS_app)
var(outcome$TAS)
cov(outcome$TAS_app, outcome$TAS)
(var(outcome$TAS_app) + cov(outcome$TAS_app, outcome$TAS)) / var(outcome$TAS_tot)
(var(outcome$TAS) + cov(outcome$TAS_app, outcome$TAS)) / var(outcome$TAS_tot)

# Total thermal responses vary with climate and vegetation class; direct ones
# do not.
summary(lm(data = outcome, TAS_tot ~ IGBP))
summary(lm(data = outcome, TAS_tot ~ Climate_class))
summary(lm(data = outcome, TAS ~ IGBP))
summary(lm(data = outcome, TAS ~ Climate_class))
summary(lm(data = outcome, TAS_app ~ IGBP))
summary(lm(data = outcome, TAS_app ~ Climate_class))

if (interactive()) {
  print(
    ggplot2::ggplot(outcome_long, ggplot2::aes(x = Climate_class, y = TAS_value, col = TAS_type)) +
      ggplot2::geom_boxplot() +
      ggplot2::theme_bw()
  )
  print(
    outcome_long |>
      dplyr::mutate(IGBP = factor(IGBP, levels = c("OSH", "SAV", "WSA", "CSH", "DBF", "DNF", "EBF", "MF", "ENF", "GRA", "WET"))) |>
      ggplot2::ggplot(ggplot2::aes(x = IGBP, y = TAS_value, col = TAS_type)) +
      ggplot2::geom_boxplot() +
      ggplot2::theme_bw()
  )
}
