#' Exposed data frame helper functions
#'
#' Test for and coerce to the `exposed_df` class.
#'
#' `is_exposed_df()` will return `TRUE` if `x` is an `exposed_df` object.
#'
#' `as_exposed_df()` will coerce a data frame to an `exposed_df` object if that
#' data frame has columns for policy numbers, statuses, exposures,
#' policy periods (for policy exposures only), and exposure start / end dates.
#'
#' @param x an object. For `as_exposed_df()`, `x` must be a data frame.
#' @param trx_types Optional. Character vector containing unique transaction
#' types that have been attached to `x`. For each value in `trx_types`,
#' `as_exposed_df` requires that columns exist in `x` named `trx_n_{*}` and
#' `trx_amt_{*}` containing transaction counts and amounts, respectively. The
#' prefixes "trx_n_" and "trx_amt_" can be overridden using the `col_trx_n_`
#' and `col_trx_amt_` arguments.
#' @param col_pol_num Optional. Name of the column in `x` containing the policy
#' number. The assumed default is "pol_num".
#' @param col_status Optional. Name of the column in `x` containing the policy
#' status. The assumed default is "status".
#' @param col_exposure Optional. Name of the column in `x` containing exposures.
#' The assumed default is "exposure".
#' @param col_pol_per Optional. Name of the column in `x` containing policy
#' exposure periods. Only necessary if `cal_expo` is `FALSE`. The assumed
#' default is either "pol_yr", "pol_qtr", "pol_mth", or "pol_wk" depending on
#' the value of `expo_length`.
#' @param cols_dates Optional. Names of the column in `x` containing exposure
#' start and end dates. Both date ranges are assumed to be exclusive. The
#' assumed default is of the form *A*_*B*. *A* is "cal" if `cal_expo` is `TRUE`
#' or "pol" otherwise. *B* is either "pol_yr", "pol_qtr", "pol_mth", or "pol_wk"
#' depending on the value of `expo_length`.
#' @param col_trx_n_ Optional. Prefix to use for columns containing transaction
#' counts.
#' @param col_trx_amt_ Optional. Prefix to use for columns containing transaction
#' amount.
#' @inheritParams expose
#'
#' @return For `is_exposed_df()`, a length-1 logical vector. For
#' `as_exposed_df()`, an `exposed_df` object.
#'
#' @export
is_exposed_df <- function(x) {
  inherits(x, "exposed_df")
}

#' @rdname is_exposed_df
#' @export
as_exposed_df <- function(x, end_date, start_date = as.Date("1900-01-01"),
                          target_status = NULL, cal_expo = FALSE,
                          expo_length = c("year", "quarter", "month", "week"),
                          trx_types = NULL,
                          col_pol_num,
                          col_status,
                          col_exposure,
                          col_pol_per,
                          cols_dates,
                          col_trx_n_ = "trx_n_",
                          col_trx_amt_ = "trx_amt_") {

  if(is_exposed_df(x)) return(x)

  expo_length <- rlang::arg_match(expo_length)

  if(!is.data.frame(x)) {
    rlang::abort("`x` must be a data frame.")
  }

  # column name alignment
  if(!missing(col_pol_num)) x <- x |> dplyr::rename(pol_num = {{col_pol_num}})
  if(!missing(col_status)) x <- x |> dplyr::rename(status = {{col_status}})
  if(!missing(col_exposure)) x <- x |> dplyr::rename(exposure = {{col_exposure}})

  # column name alignment - policy exposure periods
  exp_col_pol_per <- if (!cal_expo) {
    abbrev <- abbr_period(expo_length)
    paste0("pol_", abbrev)
  }
  if(!missing(col_pol_per) && !cal_expo) {
    x <- x |> dplyr::rename({{exp_col_pol_per}} := {{col_pol_per}})
  }

  # column name alignment - period start and end dates
  exp_cols_dates <- make_date_col_names(cal_expo, expo_length)
  if(!missing(cols_dates)) {

    if(length(cols_dates) != 2 && is.character(cols_dates)) {
      rlang::abort("`cols_dates` must be a length 2 character vector")
    }

    q_exp_cols_dates <- rlang::syms(exp_cols_dates)
    q_cols_dates <- rlang::syms(cols_dates)

    x <- x |> dplyr::rename(!!q_exp_cols_dates[[1]] := !!q_cols_dates[[1]],
                            !!q_exp_cols_dates[[2]] := !!q_cols_dates[[2]])
  }

  # check transaction types
  exp_cols_trx <- if(!is.null(trx_types)) {

    trx_renamer <- function(x) {
      x <- gsub(paste0("^", col_trx_n_), "trx_n_", x)
      gsub(paste0("^", col_trx_amt_), "trx_amt_", x)
    }

    x <- dplyr::rename_with(x, trx_renamer)

    trx_types <- unique(trx_types)
    exp_cols_trx <- outer(c("trx_n_", "trx_amt_"), trx_types, paste0) |>
      as.character()
  }

  # check required columns
  # pol_num, status, exposure, 2 date cols, policy period (policy expo only)
  unmatched <- c("pol_num", "status", "exposure",
                 exp_col_pol_per,
                 exp_cols_dates,
                 exp_cols_trx)
  unmatched <- setdiff(unmatched, names(x))

  if(length(unmatched) > 0) {
    rlang::abort(c(x = glue::glue("The following columns are missing from `x`: {paste(unmatched, collapse = ', ')}."),
                   i = "Hint: create these columns or use use the `cols_*` arguments to specify existing columns that should be mapped to these elements."))
  }

  new_exposed_df(x, end_date, start_date, target_status, cal_expo,
                 expo_length, trx_types)

}

# low-level class constructor
new_exposed_df <- function(x, end_date, start_date, target_status,
                           cal_expo, expo_length, trx_types = NULL) {


  date_cols <- make_date_col_names(cal_expo, expo_length)

  structure(x, class = c("exposed_df", class(x)),
            target_status = target_status,
            exposure_type = glue::glue("{if(cal_expo) 'calendar' else 'policy'}_{expo_length}"),
            start_date = start_date,
            end_date = end_date,
            date_cols = date_cols,
            trx_types = trx_types)

}

# helper for determining date columns
make_date_col_names <- function(cal_expo, expo_length) {
  abbrev <- abbr_period(expo_length)
  c(
    paste0(if (cal_expo) "cal_" else "pol_date_", abbrev),
    paste0(if (cal_expo) "cal_" else "pol_date_", abbrev, "_end")
  )
}
