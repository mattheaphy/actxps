#' Additional plotting functions for transaction studies
#'
#' These functions create additional experience study plots that are not
#' available or difficult to produce using the `[autoplot.trx_df()]` function.
#'
#' @param object An object of class `trx_df` created by the function
#' [trx_stats()].
#' @param ... Additional arguments passed to [autoplot.trx_df()].
#'
#' @details
#'
#' [plot_utilization_rates()] - Create a plot of transaction frequency and
#' severity. Frequency is represented by utilization rates (`trx_util`).
#' Severity is represented by transaction amounts as a percentage of one or
#' more other columns in the data (`{*}_w_trx`). All severity series begin with
#' the prefix "pct_of_" and end with the suffix "_w_trx". The suffix refers to
#' the fact that the denominator only includes records with a non-zero
#' transactions. Severity series are based on column names passed to the
#' `percent_of` argument in `trx_stats()`. If no "percentage of" columns exist
#' in `object`, this function will only plot utilization rates.
#'
#' @return a `ggplot` object
#'
#' @examples
#'
#' study_py <- expose_py(census_dat, "2019-12-31",
#'                       target_status = "Surrender") |>
#'   add_transactions(withdrawals) |>
#'   left_join(account_vals, by = c("pol_num", "pol_date_yr"))
#'
#' trx_res <- study_py |> group_by(pol_yr) |>
#'   trx_stats(percent_of = "av_anniv", combine_trx = TRUE)
#'
#' plot_utilization_rates(trx_res)
#'
#' @seealso [autoplot.trx_df()]
#'
#' @name plot_special_trx
#' @rdname plot_special_trx
#' @export
plot_utilization_rates <- function(object, ...) {

  verify_trx_df(object)

  .groups <- groups(object)
  piv_cols <- c("trx_util",
                paste0("pct_of_", attr(object, "percent_of"), "_w_trx")) |>
    intersect(names(object))

  object <- object |>
    tidyr::pivot_longer(dplyr::all_of(piv_cols),
                        names_to = "Series",
                        values_to = "Rate")

  # special logic to ensure that `Series` will not be used as an
  #   x or color variable
  if (length(.groups) == 0) {
    .groups <- rlang::parse_expr("All")
    object[["All"]] <- ""
  }
  if (length(.groups) == 1) {
    .groups <- append(.groups, rlang::parse_expr(".no_color"))
  }
  attr(object, "groups") <- c(.groups, rlang::expr(Series))
  class(object) <- c("trx_df", class(object))

  autoplot(object, y = Rate, scales = "free_y", ...)
}
