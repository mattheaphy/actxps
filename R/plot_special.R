#' Additional plotting functions for termination studies
#'
#' These functions create additional experience study plots that are not
#' available or difficult to produce using the [autoplot.exp_df()] function.
#'
#' @param object An object of class `exp_df` created by the function
#' [exp_stats()].
#' @param ... Additional arguments passed to [autoplot.exp_df()].
#' @param include_cred_adj If `TRUE`, credibility-weighted termination rates
#' will be plotted as well.
#' @param add_hline If `TRUE`, a blue dashed horizontal line will be drawn at
#' 100%.
#'
#' @details
#'
#' [plot_termination_rates()] - Create a plot of observed termination rates
#' and any expected termination rates attached to an `exp_df` object.
#'
#' [plot_actual_to_expected()] - Create a plot of actual-to-expected termination
#' rates attached to an `exp_df` object.
#'
#' @return a `ggplot` object
#'
#' @examples
#'
#' study_py <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
#' expected_table <- c(seq(0.005, 0.03, length.out = 10), 0.2, 0.15, rep(0.05, 3))
#'
#' study_py <- study_py |>
#'   mutate(expected_1 = expected_table[pol_yr],
#'          expected_2 = ifelse(inc_guar, 0.015, 0.03))
#'
#' exp_res <- study_py |> group_by(pol_yr) |>
#'   exp_stats(expected = c("expected_1", "expected_2"))
#'
#' plot_termination_rates(exp_res)
#'
#' plot_actual_to_expected(exp_res)
#'
#' @seealso [autoplot.exp_df()]
#'
#' @name plot_special
#' @rdname plot_special
#' @export
plot_termination_rates <- function(object, ..., include_cred_adj = FALSE) {

  verify_exp_df(object)
  if (include_cred_adj && (!attr(object, "xp_params")$credibility ||
                           is.null(attr(object, "expected")))) {
    cred_adj_warning()
  }

  .groups <- groups(object)
  piv_cols <- c("q_obs", attr(object, "expected"),
                if (include_cred_adj) paste0("adj_", attr(object, "expected")))

  object <- pivot_plot_special(object, piv_cols)

  attr(object, "groups") <- append(.groups, rlang::expr(Series), after = 1L)

  autoplot(object, y = Rate, ...)
}

#' @rdname plot_special
#' @export
plot_actual_to_expected <- function(object, ..., add_hline = TRUE) {

  verify_exp_df(object)

  piv_cols <- paste0("ae_", attr(object, "expected")) |>
    intersect(names(object))
  if (length(piv_cols) == 0) {
    rlang::abort(c(x = "The `exp_df` object does not have any actual-to-expected results available.",
                   i = "Hint: to add expected values, use the `expected` argument in `exp_stats()`"
    ))
  }

  .groups <- groups(object)

  object <- pivot_plot_special(object, piv_cols, values_to = "A/E ratio")

  attr(object, "groups") <- append(.groups, rlang::expr(Series), after = 1L)
  p <- autoplot(object, y = `A/E ratio`, ...)

  if (add_hline) {
    p <- p + ggplot2::geom_hline(yintercept = 1,
                                 linetype = 2, color = "#112599")
  }

  p

}

# this function is used to pivot `exp_df` or `trx_df` objects before they're
# passed to special plotting functions
#' @param object An `exp_df` or `trx_df` object
#' @param piv_cols A primary set of columns to pivot longer
#' @param extra_piv_cols A secondary set of pivot columns corresponding to the
#' upper and lower confidence interval limits of the primary set of columns.
#' These column names must all end in `_upper` or `_lower`.
#' @param values_to Name of the values column in the pivoted object.
#' @noRd
pivot_plot_special <- function(object, piv_cols, values_to = "Rate") {

  hold_class <- class(object)
  xp_params <- attr(object, "xp_params")
  piv_cols <- intersect(piv_cols, names(object))

  object <- if (!xp_params$conf_int) {
    object |>
      tidyr::pivot_longer(dplyr::all_of(piv_cols),
                          names_to = "Series",
                          values_to = values_to)
  } else {

    extra_piv_cols <- c(
      piv_cols |> paste0("_upper"),
      piv_cols |> paste0("_lower")
    ) |>
      intersect(names(object))

    object |>
      dplyr::rename_at(piv_cols, \(x) paste(x, values_to, sep = "_")) |>
      tidyr::pivot_longer(c(dplyr::all_of(piv_cols |>
                                            paste(values_to, sep = "_")),
                            dplyr::all_of(extra_piv_cols)),
                          names_to = c("Series", ".value"),
                          names_pattern =
                            paste0("^(",
                                   paste0(piv_cols, collapse = "|"),
                                   ")_(", values_to, "|upper|lower)")) |>
      dplyr::rename_at(c("lower", "upper"),
                       \(x) paste(values_to, x, sep = "_"))
  }

  class(object) <- hold_class
  attr(object, "xp_params") <- xp_params
  object

}

# This internal function provides a common warning that is used by multiple
# functions.
cred_adj_warning <- function() {
  rlang::warn(c("*" = "`object` has no credibility-weighted termination rates.",
                "i" = "Pass `credibility = TRUE` and one or more column names to `expected` when calling `exp_stats()` to calculate credibility-weighted termination rates."))
}
