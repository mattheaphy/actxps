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

  .groups <- groups(object)
  exp_params <- attr(object, "exp_params")
  piv_cols <- c("q_obs", attr(object, "expected"),
                if (include_cred_adj) paste0("adj_", attr(object, "expected"))) |>
    intersect(names(object))


  if (exp_params$conf_int) {

    extra_piv_cols <- c("q_obs_lower", "q_obs_upper")
    if (include_cred_adj) {
      extra_piv_cols <- c(extra_piv_cols,
                          paste0("adj_", attr(object, "expected"), "_lower"),
                          paste0("adj_", attr(object, "expected"), "_upper")) |>
        intersect(names(object))
    }

    object <- object |>
      dplyr::rename_at(piv_cols, \(x) paste0(x, "_Rate")) |>
      tidyr::pivot_longer(c(dplyr::all_of(piv_cols |> paste0("_Rate")),
                            dplyr::all_of(extra_piv_cols)),
                          names_to = c("Series", ".value"),
                          names_pattern = paste0("^(",
                                                 paste0(piv_cols, collapse = "|"),
                                                 ")_(Rate|upper|lower)")) |>
      rename(Rate_lower = lower, Rate_upper = upper)

  } else {

    object <- object |>
      tidyr::pivot_longer(dplyr::all_of(piv_cols),
                          names_to = "Series",
                          values_to = "Rate")

  }

  attr(object, "groups") <- append(.groups, rlang::expr(Series), after = 1L)
  attr(object, "exp_params") <- exp_params
  class(object) <- c("exp_df", class(object))
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
  exp_params <- attr(object, "exp_params")

  if (exp_params$conf_int) {

    extra_piv_cols <- c(paste0("ae_", attr(object, "expected"), "_lower"),
                        paste0("ae_", attr(object, "expected"), "_upper")) |>
      intersect(names(object))

    object <- object |>
      dplyr::rename_at(piv_cols, \(x) paste0(x, "_A/E ratio")) |>
      tidyr::pivot_longer(c(dplyr::all_of(piv_cols |> paste0("_A/E ratio")),
                            dplyr::all_of(extra_piv_cols)),
                          names_to = c("Series", ".value"),
                          names_pattern = paste0("^(",
                                                 paste0(piv_cols, collapse = "|"),
                                                 ")_(A/E ratio|upper|lower)")) |>
      rename(`A/E ratio_lower` = lower, `A/E ratio_upper` = upper)

  } else {

    object <- object |>
      tidyr::pivot_longer(dplyr::all_of(piv_cols),
                          names_to = "Series",
                          values_to = "A/E ratio")

  }

  attr(object, "groups") <- append(.groups, rlang::expr(Series), after = 1L)
  attr(object, "exp_params") <- exp_params
  class(object) <- c("exp_df", class(object))
  p <- autoplot(object, y = `A/E ratio`, ...)

  if (add_hline) {
    p <- p + ggplot2::geom_hline(yintercept = 1,
                                 linetype = 2, color = "#112599")
  }

  p

}
