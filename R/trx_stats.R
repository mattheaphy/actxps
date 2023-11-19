#' Summarize transactions and utilization rates
#'
#' @description Create a summary data frame of transaction counts, amounts,
#' and utilization rates.
#'
#' @details Unlike [exp_stats()], this function requires `data` to be an
#' `exposed_df` object.
#'
#' If `.data` is grouped, the resulting data frame will contain
#' one row per transaction type per group.
#'
#' Any number of transaction types can be passed to the `trx_types` argument,
#' however each transaction type **must** appear in the `trx_types` attribute of
#' `.data`. In addition, `trx_stats()` expects to see columns named `trx_n_{*}`
#' (for transaction counts) and `trx_amt_{*}` for (transaction amounts) for each
#' transaction type. To ensure `.data` is in the appropriate format, use the
#' functions [as_exposed_df()] to convert an existing data frame with
#' transactions or [add_transactions()] to attach transactions to an existing
#' `exposed_df` object.
#'
#' # "Percentage of" calculations
#'
#' The `percent_of` argument is optional. If provided, this argument must
#' be a character vector with values corresponding to columns in `.data`
#' containing values to use as denominators in the calculation of utilization
#' rates or actual-to-expected ratios. Example usage:
#'
#' - In a study of partial withdrawal transactions, if `percent_of` refers to
#'  account values, observed withdrawal rates can be determined.
#' - In a study of recurring claims, if `percent_of` refers to a column
#' containing a maximum benefit amount, utilization rates can be determined.
#'
#' # Confidence intervals
#'
#' If `conf_int` is set to `TRUE`, the output will contain lower and upper
#' confidence interval limits for the observed utilization rate and any
#' `percent_of` output columns. The confidence level is dictated
#' by `conf_level`.
#'
#' - Intervals for the utilization rate (`trx_util`) assume a binomial
#' distribution.
#' - Intervals for transactions as a percentage of another column with
#' non-zero transactions (`pct_of_{*}_w_trx`) are constructed using a normal
#' distribution
#' - Intervals for transactions as a percentage of another column
#' regardless of transaction utilization (`pct_of_{*}_all`) are calculated
#' assuming that the aggregate distribution is normal with a mean equal to
#' observed transactions and a variance equal to:
#'
#'     `Var(S) = E(N) * Var(X) + E(X)^2 * Var(N)`,
#'
#'     Where `S` is the aggregate transactions random variable, `X` is an individual
#' transaction amount assumed to follow a normal distribution, and `N` is a
#' binomial random variable for transaction utilization.
#'
#' # Default removal of partial exposures
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
#' # `summary()` Method
#'
#' Applying `summary()` to a `trx_df` object will re-summarize the
#' data while retaining any grouping variables passed to the "dots"
#' (`...`).
#'
#' @param .data A data frame with exposure-level records of type
#' `exposed_df` with transaction data attached. If necessary, use
#' [as_exposed_df()] to convert a data frame to an `exposed_df` object, and use
#' [add_transactions()] to attach transactions to an `exposed_df` object.
#'
#' @param trx_types A character vector of transaction types to include in the
#' output. If none is provided, all available transaction types in `.data`
#' will be used.
#'
#' @param percent_of A optional character vector containing column names in
#' `.data` to use as denominators in the calculation of utilization rates or
#' actual-to-expected ratios.
#'
#' @param combine_trx If `FALSE` (default), the results will contain output rows
#' for each transaction type. If `TRUE`, the results will contains aggregated
#' results across all transaction types.
#'
#' @param col_exposure Name of the column in `.data` containing exposures
#'
#' @param full_exposures_only If `TRUE` (default), partially exposed records will
#' be excluded from `data`.
#'
#' @param conf_int If `TRUE`, the output will include confidence intervals
#' around the observed utilization rate and any `percent_of` output columns.
#'
#' @param conf_level Confidence level for confidence intervals
#'
#' @param object A `trx_df` object
#' @param ... Groups to retain after `summary()` is called
#'
#' @return A tibble with class `trx_df`, `tbl_df`, `tbl`,
#' and `data.frame`. The results include columns for any grouping
#' variables and transaction types, plus the following:
#'
#' - `trx_n`: the number of unique transactions.
#' - `trx_amt`: total transaction amount
#' - `trx_flag`: the number of observation periods with non-zero transaction amounts.
#' - `exposure`: total exposures
#' - `avg_trx`: mean transaction amount (`trx_amt / trx_flag`)
#' - `avg_all`: mean transaction amount over all records (`trx_amt / exposure`)
#' - `trx_freq`: transaction frequency when a transaction occurs (`trx_n / trx_flag`)
#' - `trx_utilization`: transaction utilization per observation period (`trx_flag / exposure`)
#'
#' If `percent_of` is provided, the results will also include:
#'
#' - The sum of any columns passed to `percent_of` with non-zero transactions.
#' These columns include the suffix `_w_trx`.
#' - The sum of any columns passed to `percent_of`
#' - `pct_of_{*}_w_trx`: total transactions as a percentage of column
#' `{*}_w_trx`. In other words, total transactions divided by the sum of a
#' column including only records utilizing transactions.
#' - `pct_of_{*}_all`: total transactions as a percentage of column `{*}`. In
#' other words, total transactions divided by the sum of a column regardless
#' of whether or not transactions were utilized.
#'
#' If `conf_int` is set to `TRUE`, additional columns are added for lower and
#' upper confidence interval limits around the observed utilization rate and any
#' `percent_of` output columns. Confidence interval columns include the name
#' of the original output column suffixed by either `_lower` or `_upper`.
#' - If values are passed to `percent_of`, an additional column is created
#' containing the the sum of squared transaction amounts (`trx_amt_sq`).
#'
#' @examples
#' expo <- expose_py(census_dat, "2019-12-31", target_status = "Surrender") |>
#'   add_transactions(withdrawals)
#'
#' res <- expo |> group_by(inc_guar) |> trx_stats(percent_of = "premium")
#' res
#'
#' summary(res)
#'
#' expo |> group_by(inc_guar) |>
#'   trx_stats(percent_of = "premium", combine_trx = TRUE, conf_int = TRUE)
#'
#' @export
trx_stats <- function(.data,
                      trx_types,
                      percent_of = NULL,
                      combine_trx = FALSE,
                      col_exposure = "exposure",
                      full_exposures_only = TRUE,
                      conf_int = FALSE,
                      conf_level = 0.95) {

  verify_exposed_df(.data)

  # verify transaction types
  all_trx_types <- verify_get_trx_types(.data)

  if (missing(trx_types)) {
    trx_types <- all_trx_types
  } else {
    unmatched <- setdiff(trx_types, all_trx_types)
    if (length(unmatched) > 0) {
      rlang::abort(c(x = glue::glue("The following transactions do not exist in `.data`: {paste0(unmatched, collapse = ', ')}")))
    }
  }

  check_split_expose_basis(.data, col_exposure)

  start_date <- attr(.data, "start_date")
  end_date <- attr(.data, "end_date")

  .data <- .data |> rename(exposure = {{col_exposure}})

  # remove partial exposures
  if (full_exposures_only) {
    .data <- filter(.data, dplyr::near(exposure, 1))
  }

  .groups <- groups(.data)

  trx_cols <- names(.data)[grepl("trx_(n|amt)_", names(.data))]
  trx_cols <- trx_cols[grepl(paste(trx_types, collapse = "|"), trx_cols)]

  if (combine_trx) {
    trx_n_cols <- trx_cols[grepl("_n_", trx_cols)]
    trx_amt_cols <- trx_cols[grepl("_amt_", trx_cols)]
    .data <- .data |> mutate(
      trx_n_All = !!rlang::parse_expr(paste(trx_n_cols, collapse = "+")),
      trx_amt_All = !!rlang::parse_expr(paste(trx_amt_cols, collapse = "+")))
    trx_cols <- c("trx_n_All", "trx_amt_All")
  }

  pct_nz <- if (!is.null(percent_of)) {
    exp_form("{.col} * trx_flag", "{.col}_w_trx", percent_of)
  }

  .data <- .data |>
    select(pol_num, exposure, !!!.groups,
           dplyr::all_of(trx_cols), dplyr::all_of(percent_of)) |>
    tidyr::pivot_longer(dplyr::all_of(trx_cols),
                        names_to = c(".value", "trx_type"),
                        names_pattern = "^(trx_(?:amt|n))_(.*)$") |>
    mutate(trx_flag = abs(trx_n) > 0, !!!pct_nz,
           trx_amt_sq = if (conf_int) trx_amt ^ 2)

  finish_trx_stats(.data, trx_types, percent_of,
                   .groups, start_date, end_date,
                   conf_int, conf_level)

}

#' @export
print.trx_df <- function(x, ...) {

  cat("Transaction study results\n\n")
  if (length(groups(x)) > 0) {
    cat(" Groups:", paste(groups(x), collapse = ", "), "\n")
  }
  cat(" Study range:", as.character(attr(x, "start_date")), "to",
      as.character(attr(x, "end_date")), "\n",
      "Transaction types:", paste(attr(x, "trx_types"), collapse = ", "), "\n")
  if (!is.null(attr(x, "percent_of"))) {
    cat(" Transactions as % of:", paste(attr(x, "percent_of"), collapse = ", "), "\n")
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

  res <- group_by(object, !!!rlang::enquos(...))

  .groups <- groups(res)
  trx_types <- attr(object, "trx_types")
  start_date <- attr(object, "start_date")
  end_date <- attr(object, "end_date")
  percent_of <- attr(object, "percent_of")
  xp_params <- attr(object, "xp_params")

  finish_trx_stats(res, trx_types, percent_of,
                   .groups, start_date, end_date,
                   xp_params$conf_int, xp_params$conf_level)

}


# support functions -------------------------------------------------------


finish_trx_stats <- function(.data, trx_types, percent_of,
                             .groups, start_date, end_date,
                             conf_int, conf_level) {

  # "percent_of" formulas
  if (!is.null(percent_of)) {
    percent_of_nz <- paste0(percent_of, "_w_trx")
    pct_vals <- exp_form("sum({.col})", "{.col}", percent_of)
    pct_vals_trx <- exp_form("sum({.col})", "{.col}", percent_of_nz)
    pct_form_all <- exp_form("trx_amt / {.col}", "pct_of_{.col}_all",
                             percent_of)
    pct_form_trx <- exp_form("trx_amt / {.col}", "pct_of_{.col}",
                             percent_of_nz)
  } else {
    pct_vals <- pct_vals_trx <- pct_form_all <- pct_form_trx <- percent_of <- NULL
  }

  # confidence interval formulas
  if (conf_int) {

    p <- c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2)

    ci <- rlang::exprs(
      trx_util_lower = stats::qbinom(p[[1]], exposure, trx_util) / exposure,
      trx_util_upper = stats::qbinom(p[[2]], exposure, trx_util) / exposure
    )

    if (!is.null(percent_of)) {

      # standard deviations
      sds <- rlang::exprs(
        trx_amt_sq = sum(trx_amt_sq),
        sd_trx = (trx_amt_sq / trx_flag - avg_trx ^ 2) ^ 0.5,
        # For binomial N
        # Var(S) = n * p * (Var(X) + E(X)^2 * (1 - p))
        sd_all = (trx_flag * (
          sd_trx ^ 2 + avg_trx ^ 2 * (1 - trx_util))) ^ 0.5
      )

      # confidence intervals with transactions
      pct_lower <- exp_form("stats::qnorm(p[[1]], trx_amt,
                            sd_trx * sqrt(trx_flag)) / {.col}",
                            "pct_of_{.col}_lower", percent_of_nz)
      pct_upper <- exp_form("stats::qnorm(p[[2]], trx_amt,
                            sd_trx * sqrt(trx_flag)) / {.col}",
                            "pct_of_{.col}_upper", percent_of_nz)
      # confidence intervals across all records
      pct_lower_all <- exp_form(
        "stats::qnorm(p[[1]], trx_amt, sd_all) / {.col}",
        "pct_of_{.col}_all_lower",
        percent_of)
      pct_upper_all = exp_form(
        "stats::qnorm(p[[2]], trx_amt, sd_all) / {.col}",
        "pct_of_{.col}_all_upper",
        percent_of)
      ci <- append(ci, c(sds, pct_lower, pct_upper,
                         pct_lower_all, pct_upper_all))
    }

  } else {
    ci <- NULL
  }

  res <- .data |>
    group_by(trx_type, .add = TRUE) |>
    dplyr::summarize(trx_n = sum(trx_n),
                     trx_flag = sum(trx_flag),
                     trx_amt = sum(trx_amt),
                     exposure = sum(exposure),
                     avg_trx = trx_amt / trx_flag,
                     avg_all = trx_amt / exposure,
                     trx_freq = trx_n / trx_flag,
                     trx_util = trx_flag / exposure,
                     !!!pct_vals_trx,
                     !!!pct_vals,
                     !!!pct_form_trx,
                     !!!pct_form_all,
                     !!!ci,
                     .groups = "drop")

  if (conf_int && !is.null(percent_of)) {
    res <- res |>
      # drop interim columns
      select(-sd_trx, -sd_all) |>
      relocate(trx_amt_sq, .after = dplyr::last_col())
  }

  tibble::new_tibble(res,
                     class = "trx_df",
                     groups = .groups, trx_types = trx_types,
                     start_date = start_date,
                     percent_of = percent_of,
                     end_date = end_date,
                     xp_params = list(conf_level = conf_level,
                                      conf_int = conf_int))
}

verify_trx_df <- function(.data) {
  if (!inherits(.data, "trx_df")) {
    rlang::abort(c(x = glue::glue("`{deparse(substitute(.data))}` must be a `trx_df` object."),
                   i = "Hint: Use `trx_stats()` to create `trx_df` objects."
    ))
  }
}
