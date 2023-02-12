#' Summarize transactions and utilization rates
#'
#' @description Create a summary data frame of transaction counts, amounts,
#' and utilization rates.
#'
#' @details UPDATE ME
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
#' @param full_exposures If `TRUE` (default), partially exposed records will
#' be excluded from `data`.
#'
#' @param object an `trx_df` object
#' @param ... groups to retain after `summary()` is called
#'
#' @return UPDATE ME
#'
#' @examples
#' #UPDATE ME
#'
#' @export
trx_stats <- function(.data,
                      trx_types,
                      # TODO % of base amount,
                      #expected,
                      full_exposures = TRUE) {

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

  # remove partial exposures
  if(full_exposures) {
    .data <- dplyr::filter(.data, dplyr::near(exposure, 1))
  }

  .groups <- dplyr::groups(.data)

  # TODO KEEP???
  start_date <- attr(.data, "start_date")
  end_date <- attr(.data, "end_date")

  trx_cols <- names(.data)[grepl("trx_(n|amt)_", names(.data))]
  trx_cols <- trx_cols[grepl(paste(trx_types, collapse = "|"), trx_cols)]

  .data <- .data |>
    dplyr::select(pol_num, exposure, !!!.groups, dplyr::all_of(trx_cols)) |>
    tidyr::pivot_longer(dplyr::all_of(trx_cols),
                        names_to = c(".value", "trx_type"),
                        names_pattern = "^(trx_(?:amt|n))_(.*)$") |>
    dplyr::mutate(trx_flag = abs(trx_n) > 0)

  finish_trx_stats(.data, trx_types, #TODO pct_of, #expected,
                   .groups, start_date, end_date)

}

#' @export
print.trx_df <- function(x, ...) {
  # TODO
  cat("Transaction study results\n\n",
      "Groups:", paste(groups(x), collapse = ", "), "\n",
      "Study range:", as.character(attr(x, "start_date")), "to",
      as.character(attr(x, "end_date")), "\n",
      "Transaction types:", paste(attr(x, "trx_types"), collapse = ", "), "\n")
  if (!is.null(attr(x, "expected"))) {
    cat(" Expected values:", paste(attr(x, "expected"), collapse = ", "), "\n")
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
  # TODO expected <- attr(object, "expected")

  finish_trx_stats(res, trx_types, #TODO expected,
                   .groups, start_date, end_date)

}


# support functions -------------------------------------------------------


finish_trx_stats <- function(.data, trx_types, #TODO pct_of, #expected,
                             .groups, start_date, end_date) {

  # expected value formulas. these are already weighted if applicable
  # if (!missing(expected)) {
  #   ex_mean <- exp_form("weighted.mean({expected}, exposure)",
  #                       "{expected}", expected)
  #   ex_ae <- exp_form("q_obs / {expected}",
  #                     "ae_{expected}", expected)
  # } else {
  ex_ae <- ex_mean <- expected <- NULL
  # }

  res <- .data |>
    dplyr::summarize(trx_n = sum(trx_n),
                     trx_amt = sum(trx_amt),
                     trx_flag = sum(trx_flag),
                     exposure = sum(exposure),
                     avg_nz = trx_amt / trx_flag,
                     avg_nz_each = trx_amt / trx_n,
                     avg_all = trx_amt / exposure,
                     trx_freq  = trx_n / exposure,
                     trx_util = trx_flag / exposure,
                     !!!ex_ae,
                     .groups = "drop")

  structure(res, class = c("trx_df", class(res)),
            groups = .groups, trx_types = trx_types,
            start_date = start_date,
            # TODO expected = expected,
            end_date = end_date)
}
