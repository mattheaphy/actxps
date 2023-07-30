#' Plot observed and expected termination rates
#'
#' @param object An object of class `exp_df` created by the function
#' [exp_stats()].
#' @param ... Additional arguments passed to `autoplot.exp_df`.
#'
#' @details Create a plot of all observed and expected termination rates.
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
#' plot_termination_rates(exp_res)
#'
#' @name plot_special
#' @rdname plot_special
#' @export
plot_termination_rates <- function(object, ...) {

  verify_exp_df(object)

  .groups <- groups(object)
  object <- object |>
    tidyr::pivot_longer(c("q_obs", attr(object, "expected")) |>
                          intersect(colnames(object)),
                        names_to = "Series",
                        values_to = "Rate")
  attr(object, "groups") <- append(.groups, rlang::expr(Series), after = 1L)
  class(object) <- c("exp_df", class(object))
  autoplot(object, y = Rate, ...)
}
