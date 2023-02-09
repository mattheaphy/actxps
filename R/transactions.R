#' Add transactions to an experience study
#'
#' @description Attach summarized transactions to a data frame with
#' exposure-level records.
#'
#' @details UPDATE ME
#'
#' @param .data a data frame with exposure-level records, ideally of type
#' `exposed_df`
#' @param trx_data a data frame containing transactions details
#'
#' @examples
#' expo <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
#' add_transactions(expo, withdrawals)
#'
#' @returns UPDATE ME
#'
#' @importFrom dplyr between
#'
#' @export
add_transactions <- function(.data, trx_data) {

  # what if .data isn't an exposed_df?
  date_cols <- attr(.data, "date_cols") |> rlang::parse_exprs()

  # select a subset of columns - change dat to .data
  date_lookup <- .data |>
    dplyr::select(pol_num, !!!date_cols)


  # add dates to transaction data
  trx_data <- dplyr::left_join(
    trx_data, date_lookup, multiple = "error",
    dplyr::join_by(pol_num,
                   between(trx_date, !!date_cols[[1]], !!date_cols[[2]]))) |>
    dplyr::mutate(trx_n = 1) |>
    tidyr::pivot_wider(
      names_from = trx_type,
      id_cols = c(pol_num, !!date_cols[[1]]),
      values_from = c(trx_amt, trx_n),
      values_fn = \(x) sum(x, na.rm = TRUE))

  # update exposed_df structure to document transaction types

  .data |>
    dplyr::left_join(trx_data, dplyr::join_by(pol_num, !!date_cols[[1]])) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("trx_"), \(x)
                                dplyr::coalesce(x, 0)))

}
