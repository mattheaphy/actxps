#' Transaction summary helper functions
#'
#' Convert aggregate transaction experience studies to the `trx_df` class.
#'
#' `is_trx_df()` will return `TRUE` if `x` is a `trx_df` object.
#'
#' `as_trx_df()` will coerce a data frame to a `trx_df` object if that
#' data frame has the required columns for transaction studies listed below.
#'
#' `as_trx_df()` is most useful for working with aggregate summaries of
#' experience that were not created by actxps where individual policy
#' information is not available. After converting the data to the `trx_df`
#' class, [summary()] can be used to summarize data by any grouping variables,
#' and [autoplot()] and [autotable()] are available for reporting.
#'
#' At a minimum, the following columns are required:
#'
#' - Transaction amounts (`trx_amt`)
#' - Transaction counts (`trx_n`)
#' - The number of exposure records with transactions (`trx_flag`). This number
#' is not necessarily equal to transaction counts. If multiple transactions
#' are allowed per exposure period, `trx_flag` will be less than `trx_n`.
#' - Exposures (`exposure`)
#'
#' If transaction amounts should be expressed as a percentage of another
#' variable (i.e. to calculate utilization rates or actual-to-expected ratios),
#' additional columns are required:
#'
#' - A denominator "percent of" column. For example, the sum of account values.
#' - A denominator "percent of" column for exposure records with transactions.
#' For example, the sum of account values across all records with non-zero
#' transaction amounts.
#'
#' If confidence intervals are desired and "percent of" columns are passed, an
#' additional column for the sum of squared transaction amounts (`trx_amt_sq`)
#' is also required.
#'
#' The names in parentheses above are expected column names. If the data
#' frame passed to `as_trx_df()` uses different column names, these can be
#' specified using the `col_*` arguments.
#'
#' `start_date`, and `end_date` are optional arguments that are
#' only used for printing the resulting `trx_df` object.
#'
#' Unlike [trx_stats()], `as_trx_df()` only permits a single transaction type and
#' a single `percent_of` column.
#'
#' @param x An object. For `as_trx_df()`, `x` must be a data frame.
#' @param col_trx_amt Optional. Name of the column in `x` containing transaction
#' amounts.
#' @param col_trx_n Optional. Name of the column in `x` containing transaction
#' counts.
#' @param col_trx_flag Optional. Name of the column in `x` containing the number
#' of exposure records with transactions.
#' @param col_exposure Optional. Name of the column in `x` containing exposures.
#' @param col_percent_of Optional. Name of the column in `x` containing a
#' numeric variable to use in "percent of" calculations.
#' @param col_percent_of_w_trx Optional. Name of the column in `x` containing a
#' numeric variable to use in "percent of" calculations with transactions.
#' @param col_trx_amt_sq  Optional and only required when `col_percent_of` is
#' passed and `conf_int` is `TRUE`. Name of the column in `x` containing squared
#' transaction amounts.
#' @param conf_int If `TRUE`, future calls to [summary()] will include
#' confidence intervals around the observed utilization rates and any
#' `percent_of` output columns.
#' @param conf_level Confidence level for confidence intervals
#' @inheritParams expose
#'
#' @return For `is_trx_df()`, a length-1 logical vector. For `as_trx_df()`,
#' a `trx_df` object.
#'
#' @seealso [trx_stats()] for information on how `trx_df` objects are typically
#' created from individual exposure records.
#'
#' @examples
#' # convert pre-aggregated experience into a trx_df object
#' dat <- as_trx_df(agg_sim_dat,
#'                  col_exposure = "n",
#'                  col_trx_amt = "wd",
#'                  col_trx_n = "wd_n",
#'                  col_trx_flag = "wd_flag",
#'                  col_percent_of = "av",
#'                  col_percent_of_w_trx = "av_w_wd",
#'                  col_trx_amt_sq = "wd_sq",
#'                  start_date = 2005, end_date = 2019,
#'                  conf_int = TRUE)
#' dat
#' is_trx_df(dat)
#'
#' # summary by policy year
#' summary(dat, pol_yr)
#'
#' @export
as_trx_df <- function(x,
                      col_trx_amt = "trx_amt",
                      col_trx_n = "trx_n",
                      col_trx_flag = "trx_flag",
                      col_exposure = "exposure",
                      col_percent_of = NULL,
                      col_percent_of_w_trx = NULL,
                      col_trx_amt_sq = "trx_amt_sq",
                      start_date = as.Date("1900-01-01"),
                      end_date = NULL,
                      conf_int = FALSE,
                      conf_level = 0.95) {

  if (is_trx_df(x)) return(x)

  if (!is.data.frame(x)) {
    rlang::abort("`x` must be a data frame.")
  }

  # column name alignment
  req_names <- c("exposure", "trx_amt", "trx_n", "trx_flag")
  if (!missing(col_exposure)) x <- x |> rename(exposure = {{col_exposure}})
  if (!missing(col_trx_amt)) x <- x |> rename(trx_amt = {{col_trx_amt}})
  if (!missing(col_trx_n)) x <- x |> rename(trx_n = {{col_trx_n}})
  if (!missing(col_trx_flag)) x <- x |> rename(trx_flag = {{col_trx_flag}})

  if (conf_int && !missing(col_percent_of)) {
    req_names <- c(req_names, "trx_amt_sq")
    if (!missing(col_trx_amt_sq)) x <- x |>
        rename(trx_amt_sq = {{col_trx_amt_sq}})
  }

  if (!missing(col_percent_of)) {
    req_names <- c(req_names, col_percent_of, paste0(col_percent_of, "_w_trx"))
  }
  if (!missing(col_percent_of_w_trx)) {
    if (missing(col_percent_of)) {
      rlang::abort("`col_percent_of_w_trx` was supplied without passing anything to `col_percent_of`")
    }
    pct_w_trx_name <- rlang::parse_expr(paste0(col_percent_of, "_w_trx"))
    x <- x |> rename(!!pct_w_trx_name := {{col_percent_of_w_trx}})
  }

  # check required columns
  verify_col_names(names(x), req_names)

  new_trx_df(x |> mutate(trx_type = col_trx_amt),
             .groups = list(),
             trx_types = col_trx_amt,
             start_date = start_date,
             percent_of = col_percent_of,
             end_date = end_date,
             conf_level = conf_level,
             conf_int = conf_int)
}

#' @export
#' @rdname as_trx_df
is_trx_df <- function(x) {
  inherits(x, "trx_df")
}
