#' Add transactions to an experience study
#'
#' @description Attach summarized transactions to a data frame with
#' exposure-level records.
#'
#' @details This function attaches transactions to an `exposed_df` object.
#' Transactions are grouped and summarized such that the number of rows in
#' the `exposed_df` object does not change. Two columns are added to the output
#' for each transaction type. These columns have names of the pattern
#' `trx_n_{*}` (transaction counts) and `trx_amt_{*}` (transaction_amounts).
#'
#' Transactions are associated with the `exposed_df` object by matching
#' transactions dates with exposure dates ranges found in `exposed_df`.
#'
#' All columns containing dates must be in YYYY-MM-DD format.
#'
#' @param .data A data frame with exposure-level records with the class
#' `exposed_df`. Use [as_exposed_df()] to convert a data frame to an
#' `exposed_df` object if necessary.
#' @param trx_data A data frame containing transactions details. This data
#' frame must have columns for policy numbers, transaction dates, transaction
#' types, and transaction amounts.
#' @param col_pol_num Name of the column in `trx_data` containing the policy
#' number
#' @param col_trx_date Name of the column in `trx_data` containing the
#' transaction date
#' @param col_trx_type Name of the column in `trx_data` containing the
#' transaction type
#' @param col_trx_amt Name of the column in `trx_data` containing the
#' transaction amount
#'
#' @examples
#' expo <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
#' add_transactions(expo, withdrawals)
#'
#' @returns An `exposed_df` object with two new columns containing transaction
#' counts and amounts for each transaction type found in `trx_data`. The
#' `exposed_df`'s `trx_types` attributes will be updated to include the new
#' transaction types found in `trx_data.`
#'
#' @importFrom dplyr between
#'
#' @seealso [expose()], [as_exposed_df()]
#'
#' @export
add_transactions <- function(.data, trx_data,
                             col_pol_num = "pol_num",
                             col_trx_date = "trx_date",
                             col_trx_type = "trx_type",
                             col_trx_amt = "trx_amt") {

  verify_exposed_df(.data)

  if (!is.data.frame(trx_data)) {
    rlang::abort(c(x = "`trx_data` must be a data frame."))
  }

  date_cols <- attr(.data, "date_cols") |> rlang::parse_exprs()

  # select a minimum subset of columns
  date_lookup <- ungroup(.data) |>
    select(pol_num, !!!date_cols)

  # # column renames
  trx_data <- trx_data |>
    rename(pol_num = {{col_pol_num}},
           trx_date = {{col_trx_date}},
           trx_type = {{col_trx_type}},
           trx_amt = {{col_trx_amt}}) |>
    mutate(trx_date = .convert_date(trx_date))

  .check_missing_dates(trx_data$trx_date, "trx_date")

  # check for conflicting transaction types
  existing_trx_types <- attr(.data, "trx_types")
  new_trx_types <- unique(trx_data$trx_type)
  conflict_trx_types <- intersect(new_trx_types, existing_trx_types)
  if (length(conflict_trx_types) > 0) {
    rlang::abort(c(x = glue::glue("`trx_data` contains transaction types that have already been attached to `.data`: {paste(conflict_trx_types, collapse = ', ')}."),
                   i = "Update `trx_data` with unique transaction types."))
  }

  # add dates to transaction data
  trx_data <- trx_data |>
    # between-join by transaction date falling within exposures windows
    inner_join(
      date_lookup, relationship = "many-to-one",
      dplyr::join_by(pol_num,
                     between(trx_date, !!date_cols[[1]], !!date_cols[[2]])))

  # pivot / summarize to match the grain of exposure data
  trx_data <- trx_data |>
    mutate(trx_n = 1) |>
    tidyr::pivot_wider(
      names_from = trx_type,
      id_cols = c(pol_num, !!date_cols[[1]]),
      values_from = c(trx_amt, trx_n),
      values_fn = \(x) sum(x, na.rm = TRUE))

  # add new transaction types
  attr(.data, "trx_types") <- c(existing_trx_types, as.character(new_trx_types))

  # update exposed_df structure to document transaction types
  .data |>
    left_join(trx_data, dplyr::join_by(pol_num, !!date_cols[[1]])) |>
    mutate(dplyr::across(dplyr::starts_with("trx_"), \(x)
                         dplyr::coalesce(x, 0)))

}
