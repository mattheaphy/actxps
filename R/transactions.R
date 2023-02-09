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
                   between(trx_date, !!date_cols[[1]], !!date_cols[[2]])))

  trx_data <- trx_data |>
    dplyr::group_by(pol_num, trx_type, !!date_cols[[1]]) |>
    dplyr::summarize(trx_n = dplyr::n(),
                     trx_amt = sum(trx_amt, na.rm = TRUE),
                     .groups = "drop")

  .data |>
    dplyr::left_join(trx_data, dplyr::join_by(pol_num, !!date_cols[[1]]))

}
