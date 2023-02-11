#' Add transactions to an experience study
#'
#' @description Attach summarized transactions to a data frame with
#' exposure-level records.
#'
#' @details UPDATE ME
#'
#' @param .data a data frame with exposure-level records of type
#' `exposed_df`. Use [as_exposed_df()] to convert a data frame to an
#' `exposed_df` object if necessary.
#' @param trx_data a data frame containing transactions details
#' @param col_pol_num name of the column in `trx_data` containing the policy
#' number
#' @param col_trx_date name of the column in `trx_data` containing the
#' transaction date
#' @param col_trx_type name of the column in `trx_data` containing the
#' transaction type
#' @param col_trx_amt name of the column in `trx_data` containing the
#' transaction amount
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
add_transactions <- function(.data, trx_data,
                             col_pol_num = "pol_num",
                             col_trx_date = "trx_date",
                             col_trx_type = "trx_type",
                             col_trx_amt = "trx_amt") {

  if(!is_exposed_df(.data)) {
    rlang::abort(c(x = "`.data` must be an `exposed_df` object.",
                   i = "Hint: Use `as_exposed_df()` to convert your data to the required format.."
    ))
  }

  date_cols <- attr(.data, "date_cols") |> rlang::parse_exprs()

  # select a minimum subset of columns
  date_lookup <- .data |>
    dplyr::select(pol_num, !!!date_cols)

  # add dates to transaction data and pivot / summarize to match the grain
  #   of exposure data
  trx_data <- trx_data |>
    # column renames
    dplyr::rename(pol_num = {{col_pol_num}},
                  trx_date = {{col_trx_date}},
                  trx_type = {{col_trx_type}},
                  trx_amt = {{col_trx_amt}}) |>
    # between join by transaction date falling within exposures windows
    dplyr::left_join(
      date_lookup, multiple = "error",
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
