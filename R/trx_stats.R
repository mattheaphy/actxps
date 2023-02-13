#' Summarize transactions and utilization rates
#'
#' @description Create a summary data frame of transaction counts, amounts,
#' and utilization rates.
#'
#' @details UPDATE ME
#'
#' As a default, partial exposures are removed from `.data` before summarizing
#' results. This is done to avoid complexity associated with a lopsided skew
#' in the timing of transactions. For example, if transactions can occur on a
#' monthly basis or annually at the beginning of each policy year, partial
#' exposures may not be appropriate. If a policy had an exposure of 0.5 years
#' and was taking withdrawals annually at the beginning of the year, an
#' argument could be made that the exposure should instead be 1 complete year.
#' If the same policy was expected to take withdrawals 9 months into the year,
#' it's not clear if the exposure should be 0.5 years or 0.5 / 0.75 years.
#' To override this treatment, set `full_exposures_only` to `FALSE`.
#'
#' @param .data a data frame with exposure-level records of type
#' `exposed_df` with transaction data attached. If necessary, use
#' [as_exposed_df()] to convert a data frame to an `exposed_df` object, and use
#' [add_transactions()] to attach transactions to an `exposed_df` object.
#'
#' @param trx_types A character vector of transaction types to include in the
#' output. If none is provided, all available transaction types in `.data`
#' will be used.
#'
#' @param percent_of A optional character vector containing column names in `.data`
#' to use as denominators in the calculation of utilization rates or
#' actual-to-expected ratios.
#'
#' @param full_exposures_only If `TRUE` (default), partially exposed records will
#' be excluded from `data`.
#'
#' @param object an `trx_df` object
#' @param ... groups to retain after `summary()` is called
#'
#' @return UPDATE ME
#'
#' @examples
#' expo <- expose_py(census_dat, "2019-12-31", target_status = "Surrender") |>
#'   add_transactions(withdrawals)
#'
#' res <- expo |> dplyr::group_by(inc_guar) |> trx_stats()
#' res
#'
#' summary(res)
#'
#' @export
trx_stats <- function(.data,
                      trx_types,
                      percent_of = NULL,
                      full_exposures_only = TRUE) {

  verify_exposed_df(.data)

  # verify transaction types
  all_trx_types <- attr(.data, "trx_types")
  if(is.null(all_trx_types)) {
    rlang::abort(c(x = "No transactions have been attached to `.data`.",
                   i = "Add transaction data using `add_transactions()` before calling this function."))
  }

  if(missing(trx_types)) {
    trx_types <- all_trx_types
  } else {
    unmatched <- setdiff(trx_types, all_trx_types)
    if (length(unmatched) > 0) {
      rlang::abort(c(x = glue::glue("The following transactions do not exist in `.data`: {paste0(unmatched, collapse = ', ')}")))
    }
  }

  start_date <- attr(.data, "start_date")
  end_date <- attr(.data, "end_date")

  # remove partial exposures
  if(full_exposures_only) {
    .data <- dplyr::filter(.data, dplyr::near(exposure, 1))
  }

  .groups <- dplyr::groups(.data)

  trx_cols <- names(.data)[grepl("trx_(n|amt)_", names(.data))]
  trx_cols <- trx_cols[grepl(paste(trx_types, collapse = "|"), trx_cols)]

  .data <- .data |>
    dplyr::select(pol_num, exposure, !!!.groups,
                  dplyr::all_of(trx_cols), dplyr::all_of(percent_of)) |>
    tidyr::pivot_longer(dplyr::all_of(trx_cols),
                        names_to = c(".value", "trx_type"),
                        names_pattern = "^(trx_(?:amt|n))_(.*)$") |>
    dplyr::mutate(trx_flag = abs(trx_n) > 0)

  finish_trx_stats(.data, trx_types, percent_of,
                   .groups, start_date, end_date)

}

#' @export
print.trx_df <- function(x, ...) {

  cat("Transaction study results\n\n",
      "Groups:", paste(groups(x), collapse = ", "), "\n",
      "Study range:", as.character(attr(x, "start_date")), "to",
      as.character(attr(x, "end_date")), "\n",
      "Transaction types:", paste(attr(x, "trx_types"), collapse = ", "), "\n")
  if (!is.null(attr(x, "percent_of"))) {
    cat(" Transactions as % of:", paste(attr(x, "percent_of"), collapse = ", "), "\n")
  }
  if (is.null(attr(x, "wt"))) {
    cat("\n")
  } else {
    cat(" Weighted by:", attr(x, "wt"), "\n\n")
  }

  NextMethod()
}


#' @export
groups.trx_df <- function(x) {
  attr(x, "groups")
}

#' @export
#' @rdname trx_stats
summary.trx_df <- function(object, ...) {

  res <- dplyr::group_by(object, !!!rlang::enquos(...))

  .groups <- dplyr::groups(res)
  trx_types <- attr(object, "trx_types")
  start_date <- attr(object, "start_date")
  end_date <- attr(object, "end_date")
  percent_of <- attr(object, "percent_of")

  finish_trx_stats(res, trx_types, percent_of,
                   .groups, start_date, end_date)

}


# support functions -------------------------------------------------------


finish_trx_stats <- function(.data, trx_types, percent_of,
                             .groups, start_date, end_date) {

  if (!is.null(percent_of)) {
    pct_vals <- exp_form("sum({.col})",
                         "{.col}", percent_of)
    pct_form <- exp_form("trx_amt / {.col}",
                        "pct_of_{.col}", percent_of)
  } else {
    pct_vals <- pct_form <- percent_of <- NULL
  }

  res <- .data |>
    dplyr::group_by(trx_type, .add = TRUE) |>
    dplyr::summarize(trx_n = sum(trx_n),
                     trx_amt = sum(trx_amt),
                     trx_flag = sum(trx_flag),
                     exposure = sum(exposure),
                     avg_nz = trx_amt / trx_flag,
                     avg_nz_each = trx_amt / trx_n,
                     avg_all = trx_amt / exposure,
                     trx_freq  = trx_n / exposure,
                     trx_util = trx_flag / exposure,
                     !!!pct_vals,
                     !!!pct_form,
                     .groups = "drop")

  structure(res, class = c("trx_df", class(res)),
            groups = .groups, trx_types = trx_types,
            start_date = start_date,
            percent_of = percent_of,
            end_date = end_date)
}
