# Compare two sets of site-level TAS estimates (outcome_*.csv tables, and
# optionally the matching outcome_siteyear_*.csv tables).
#
# compare_tas() returns one row per site with the reference and new TAS, its
# change, whether the change is beyond `threshold`, the sign/significance
# class of each estimate (sig- / n.s. / sig+ at p < `alpha`) and how it moved,
# and, when site-year tables are given, structural differences (windows and
# site-years present, site-years with a fitted model).

#' Sign and significance class of TAS estimates
#'
#' "sig-" or "sig+" (by the sign of the estimate) where p < `alpha`, "n.s." otherwise, NA where
#' the estimate or its p-value is missing.
#'
#' @param tas TAS estimates.
#' @param p Their p-values.
#' @param alpha Significance level.
#' @return A character vector.
#' @noRd
tas_class <- function(tas, p, alpha = 0.05) {
  ifelse(is.na(tas) | is.na(p), NA_character_,
         ifelse(p >= alpha, "n.s.", ifelse(tas < 0, "sig-", "sig+")))
}

#' Severity of class transitions
#'
#' Order of severity for a class transition, most important first: 1 a significant sign flip
#' (sig- to sig+ or back), 2 another change between classes, 3 a class appearing or
#' disappearing (NA on one side only), 4 no change.
#'
#' @param from Classes before (tas_class()).
#' @param to Classes after.
#' @return An integer vector.
#' @noRd
transition_rank <- function(from, to) {
  flip <- !is.na(from) & !is.na(to) & ((from == "sig-" & to == "sig+") | (from == "sig+" & to == "sig-"))
  sig_change <- !is.na(from) & !is.na(to) & from != to & !flip
  ifelse(flip, 1L, ifelse(sig_change, 2L, ifelse(is.na(from) != is.na(to), 3L, 4L)))
}

#' Structure of a site-year table
#'
#' Per site: the number of distinct windows, of site-years, and of site-years with a fitted
#' model (non-NA alpha).
#'
#' @param siteyear An outcome_siteyear_*.csv table.
#' @return A data frame: site_ID, n_windows, n_siteyears, n_fitted.
#' @noRd
siteyear_structure <- function(siteyear) {
  siteyear |>
    dplyr::group_by(.data$site_ID) |>
    dplyr::summarise(
      n_windows = dplyr::n_distinct(.data$window),
      n_siteyears = dplyr::n(),
      n_fitted = sum(!is.na(.data$alpha)),
      .groups = "drop"
    )
}

#' Compare two sets of site-level TAS estimates
#'
#' One row per site with the reference and new TAS, its change, whether the change is beyond
#' `threshold`, the sign/significance class of each estimate (sig- / n.s. / sig+ at p < `alpha`)
#' and how it moved, and, when site-year tables are given, structural differences (windows and
#' site-years present, site-years with a fitted model).
#'
#' @param ref Reference outcome table (outcome_*.csv: site_ID, TAS, TASp).
#' @param new New outcome table.
#' @param ref_siteyear Reference outcome_siteyear_*.csv table, or NULL.
#' @param new_siteyear New outcome_siteyear_*.csv table, or NULL; structural differences need
#'   both.
#' @param threshold Absolute change in TAS that counts as a change.
#' @param alpha Significance level of the classes.
#' @return A data frame: site_ID, TAS_ref, TASp_ref, TAS_new, TASp_new, dTAS, changed,
#'   class_ref, class_new, transition ("" if none), transition_rank (transition_rank()), and
#'   with site-year tables n_windows, n_siteyears and n_fitted (_ref and _new) and
#'   structure_changed; sorted by transition_rank, then by decreasing |dTAS|.
#' @noRd
compare_tas <- function(ref, new, ref_siteyear = NULL, new_siteyear = NULL, threshold = 1e-3, alpha = 0.05) {
  out <- dplyr::full_join(
    ref |> dplyr::select("site_ID", TAS_ref = "TAS", TASp_ref = "TASp"),
    new |> dplyr::select("site_ID", TAS_new = "TAS", TASp_new = "TASp"),
    by = "site_ID"
  ) |>
    dplyr::mutate(
      dTAS = .data$TAS_new - .data$TAS_ref,
      changed = is.na(.data$TAS_ref) != is.na(.data$TAS_new) | (!is.na(.data$dTAS) & abs(.data$dTAS) >= threshold),
      class_ref = tas_class(.data$TAS_ref, .data$TASp_ref, alpha),
      class_new = tas_class(.data$TAS_new, .data$TASp_new, alpha),
      transition = ifelse(dplyr::coalesce(.data$class_ref, "NA") == dplyr::coalesce(.data$class_new, "NA"), "",
                          paste(dplyr::coalesce(.data$class_ref, "NA"), "->", dplyr::coalesce(.data$class_new, "NA"))),
      transition_rank = transition_rank(.data$class_ref, .data$class_new)
    )
  if (!is.null(ref_siteyear) && !is.null(new_siteyear)) {
    s_ref <- siteyear_structure(ref_siteyear)
    s_new <- siteyear_structure(new_siteyear)
    names(s_ref)[-1] <- paste0(names(s_ref)[-1], "_ref")
    names(s_new)[-1] <- paste0(names(s_new)[-1], "_new")
    out <- out |>
      dplyr::left_join(s_ref, by = "site_ID") |>
      dplyr::left_join(s_new, by = "site_ID") |>
      dplyr::mutate(structure_changed = .data$n_windows_ref != .data$n_windows_new |
                      .data$n_siteyears_ref != .data$n_siteyears_new |
                      .data$n_fitted_ref != .data$n_fitted_new)
  }
  out |> dplyr::arrange(.data$transition_rank, dplyr::desc(abs(.data$dTAS)))
}

#' A short text summary of compare_tas() output
#'
#' The number of sites, median and maximum |dTAS|, the sites changed beyond the threshold, the
#' class transitions, the significant sign flips and, if compared, the sites with structural
#' changes.
#'
#' @param cmp compare_tas() output.
#' @param label Prefix of the first line ("" for none).
#' @return A single string, lines separated by newlines.
#' @noRd
summarise_tas_comparison <- function(cmp, label = "") {
  lines <- c(
    sprintf("%s%d sites; |dTAS| median %.2g, max %.2g; %d changed beyond threshold",
            if (nzchar(label)) paste0(label, ": ") else "", nrow(cmp),
            stats::median(abs(cmp$dTAS), na.rm = TRUE), suppressWarnings(max(abs(cmp$dTAS), na.rm = TRUE)),
            sum(cmp$changed, na.rm = TRUE)),
    sprintf("  class transitions: %s", if (any(nzchar(cmp$transition))) paste(names(table(cmp$transition[nzchar(cmp$transition)])), table(cmp$transition[nzchar(cmp$transition)]), sep = " x", collapse = "; ") else "none"),
    sprintf("  significant sign flips: %s", if (any(cmp$transition_rank == 1)) paste(cmp$site_ID[cmp$transition_rank == 1], collapse = ", ") else "none")
  )
  if ("structure_changed" %in% names(cmp)) {
    lines <- c(lines, sprintf("  sites with structural changes: %d", sum(cmp$structure_changed, na.rm = TRUE)))
  }
  paste(lines, collapse = "\n")
}
