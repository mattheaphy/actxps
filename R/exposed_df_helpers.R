#' Exposed data frame helper functions
#'
#' Test for and coerce to the `exposed_df` class.
#'
#' `is_exposed_df()` will return `TRUE` if `x` is an `exposed_df` object.
#'
#' `as_exposed_df()` will coerce a data frame to an `exposed_df` object if that
#' data frame has columns for policy numbers, statuses, exposures,
#' policy periods (for policy exposures only), and exposure start / end dates.
#' Optionally, if `x` has transaction counts and amounts by type, these can
#' be specified without calling [add_transactions()].
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
#' @param cols_dates Optional. Names of the columns in `x` containing exposure
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
#' @importFrom vctrs vec_ptype2 vec_cast
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
  if(!missing(col_pol_num)) x <- x |> rename(pol_num = {{col_pol_num}})
  if(!missing(col_status)) x <- x |> rename(status = {{col_status}})
  if(!missing(col_exposure)) x <- x |> rename(exposure = {{col_exposure}})

  # column name alignment - policy exposure periods
  exp_col_pol_per <- if (!cal_expo) {
    abbrev <- abbr_period(expo_length)
    paste0("pol_", abbrev)
  }
  if(!missing(col_pol_per) && !cal_expo) {
    x <- x |> rename({{exp_col_pol_per}} := {{col_pol_per}})
  }

  # column name alignment - period start and end dates
  exp_cols_dates <- make_date_col_names(cal_expo, expo_length)
  if(!missing(cols_dates)) {

    if(length(cols_dates) != 2 && is.character(cols_dates)) {
      rlang::abort("`cols_dates` must be a length 2 character vector")
    }

    q_exp_cols_dates <- rlang::syms(exp_cols_dates)
    q_cols_dates <- rlang::syms(cols_dates)

    x <- x |> rename(!!q_exp_cols_dates[[1]] := !!q_cols_dates[[1]],
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

  tibble::new_tibble(x,
                     class = "exposed_df",
                     target_status = target_status,
                     exposure_type = glue::glue("{if(cal_expo) 'calendar' else 'policy'}_{expo_length}") |>
                       as.character(),
                     start_date = start_date,
                     end_date = end_date,
                     date_cols = date_cols,
                     trx_types = trx_types)

}

#' @export
print.exposed_df <- function(x, ...) {
  cat("Exposure data\n\n",
      "Exposure type:", attr(x, "exposure_type"), "\n",
      "Target status:", paste(attr(x, "target_status"), collapse = ", "), "\n",
      "Study range:", as.character(attr(x, "start_date")), "to",
      as.character(attr(x, "end_date")))

  trx_types <- attr(x, "trx_types")
  if (!is.null(trx_types)) {
    cat("\n", "Transaction types:", paste(trx_types, collapse = ", "), "\n")
  }

  cat("\n\n")
  NextMethod()
}

#' @export
group_by.exposed_df <- function(.data, ..., .add, .drop) {
  x <- NextMethod()
  class(x) <- c("exposed_df", class(x))
  x
}

#' @export
ungroup.exposed_df <- function(x, ...) {
  res <- NextMethod()
  vec_cast(res, x)
}

#' @export
filter.exposed_df <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  x <- NextMethod()
  vec_cast(x, .data)
}

#' @export
arrange.exposed_df <- function(.data, ..., .by_group) {
  x <- NextMethod()
  vec_cast(x, .data)
}

#' @export
mutate.exposed_df <- function(.data, ...) {
  x <- NextMethod()
  if (dplyr::is_grouped_df(.data)) {
    g <- groups(.data)
    ptype <- vec_ptype2(ungroup(.data), x)
    vec_cast(x, ptype |> group_by(!!!g))
  } else {
    x
  }
}

#' @export
select.exposed_df <- function(.data, ...) {
  x <- NextMethod()
  if (dplyr::is_grouped_df(.data)) {
    g <- groups(.data)
    vec_cast(x, ungroup(.data)[, names(x)] |> group_by(!!!g))
  } else {
    x
  }
}

#' @export
slice.exposed_df <- function (.data, ..., .by = NULL, .preserve = FALSE)  {
  if (dplyr::is_grouped_df(.data)) {
    NextMethod(.by = NULL) |> vec_cast(.data)
  } else {
    NextMethod()
  }
}

#' @export
rename.exposed_df <- function(.data, ..., .by_group) {
  x <- NextMethod()
  if (dplyr::is_grouped_df(.data)) {
    g <- groups(.data)
    vec_cast(x, stats::setNames(ungroup(.data), names(x)) |> group_by(!!!g))
  } else {
    x
  }
}

#' @export
relocate.exposed_df <- function(.data, ..., .by_group) {
  x <- NextMethod()
  if (dplyr::is_grouped_df(.data)) {
    g <- groups(.data)
    vec_cast(x, ungroup(.data)[, names(x)] |> group_by(!!!g))
  } else {
    x
  }
}

# NULL coalesce function
`%||%` <- function(x, y) if(is.null(x)) y else x

has_compatible_expo <- function(x, y) {
  x <- attr(x, "exposure_type")
  y <- attr(y, "exposure_type")
  x <- x %||% y
  y <- y %||% x
  x == y
}

exposed_df_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {

  out <- vctrs::tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)

  if (!has_compatible_expo(x, y)) {
    vctrs::stop_incompatible_type(
      x,
      y,
      x_arg = x_arg,
      y_arg = y_arg,
      details = "Two different exposure period types cannot be combined."
    )
  }

  end_date <- max(as.Date(attr(x, "end_date")),
                  as.Date(attr(y, "end_date")))
  start_date <- min(as.Date(attr(x, "start_date")),
                    as.Date(attr(y, "start_date")))
  target_status <- union(attr(x, "target_status"), attr(y, "target_status"))
  trx_types <- union(attr(x, "trx_types"), attr(y, "trx_types"))

  expo_type <- attr(x, "exposure_type") %||% attr(y, "exposure_type")

  split_type <- strsplit(expo_type, "_")[[1]]
  cal_expo <- split_type[[1]] == "calendar"
  expo_length <- split_type[[2]]

  new_exposed_df(out, end_date, start_date, target_status, cal_expo,
                 expo_length, trx_types)
}


exposed_df_cast <- function(x, to, ..., x_arg = "", to_arg = "") {

  out <- vctrs::tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)

  if (!has_compatible_expo(x, to)) {
    vctrs::stop_incompatible_cast(
      x,
      to,
      x_arg = x_arg,
      to_arg = to_arg,
      details = "Two different exposure period types cannot be combined."
    )
  }

  end_date <- attr(to, "end_date")
  start_date <- attr(to, "start_date")
  target_status <- attr(to, "target_status")
  trx_types <- attr(to, "trx_types")

  expo_type <- attr(to, "exposure_type")
  split_type <- strsplit(expo_type, "_")[[1]]
  cal_expo <- split_type[[1]] == "calendar"
  expo_length <- split_type[[2]]

  new_exposed_df(out, end_date, start_date, target_status, cal_expo,
                 expo_length, trx_types)
}

# exposed_df | exposed_df
#' @export
vec_ptype2.exposed_df.exposed_df <- function(x, y, ...) {
  exposed_df_ptype2(x, y, ...)
}

#' @export
vec_cast.exposed_df.exposed_df <- function(x, to, ...) {
  exposed_df_cast(x, to, ...)
}

# exposed_df | tbl_df
#' @export
vec_ptype2.exposed_df.tbl_df <- function(x, y, ...) {
  exposed_df_ptype2(x, y, ...)
}

#' @export
vec_ptype2.tbl_df.exposed_df <- function(x, y, ...) {
  exposed_df_ptype2(x, y, ...)
}

#' @export
vec_cast.exposed_df.tbl_df <- function(x, to, ...) {
  exposed_df_cast(x, to, ...)
}

#' @export
vec_cast.tbl_df.exposed_df <- function(x, to, ...) {
  vctrs::tib_cast(x, to, ...)
}

# exposed_df | data.frame
#' @export
vec_ptype2.exposed_df.data.frame <- function(x, y, ...) {
  exposed_df_ptype2(x, y, ...)
}

#' @export
vec_ptype2.data.frame.exposed_df <- function(x, y, ...) {
  exposed_df_ptype2(x, y, ...)
}

#' @export
vec_cast.exposed_df.data.frame <- function(x, to, ...) {
  exposed_df_cast(x, to, ...)
}

#' @export
vec_cast.data.frame.exposed_df <- function(x, to, ...) {
  vctrs::df_cast(x, to, ...)
}


# exposed_df | grouped_df
#' @export
vec_ptype2.exposed_df.grouped_df <- function(x, y, ...) {
  g <- union(dplyr::group_vars(x), dplyr::group_vars(y))
  exposed_df_ptype2(x, y, ...) |>
    group_by(dplyr::across(dplyr::all_of(g)))
}

#' @export
vec_ptype2.grouped_df.exposed_df <- function(x, y, ...) {
  g <- union(dplyr::group_vars(x), dplyr::group_vars(y))
  exposed_df_ptype2(x, y, ...) |>
    group_by(dplyr::across(dplyr::all_of(g)))
}

#' @export
vec_cast.exposed_df.grouped_df <- function(x, to, ...) {
  exposed_df_cast(x, to, ...) |>
    group_by(!!!groups(to))
}

#' @export
vec_cast.grouped_df.exposed_df <- function(x, to, ...) {
  vctrs::tib_cast(x, to, ...) |>
    group_by(!!!groups(to))
}


# helper for determining date columns
make_date_col_names <- function(cal_expo, expo_length) {
  abbrev <- abbr_period(expo_length)
  c(
    paste0(if (cal_expo) "cal_" else "pol_date_", abbrev),
    paste0(if (cal_expo) "cal_" else "pol_date_", abbrev, "_end")
  )
}

verify_exposed_df <- function(.data) {
  if(!is_exposed_df(.data)) {
    rlang::abort(c(x = glue::glue("`{deparse(substitute(.data))}` must be an `exposed_df` object."),
                   i = "Hint: Use `as_exposed_df()` to convert your data to the required format."
    ))
  }
}

# verify transactions exist and return all unique transaction types.
# If no transactions and required = TRUE, an error is thrown. Otherwise NULL
# if returned
verify_get_trx_types <- function(.data, required = TRUE) {
  trx_types <- attr(.data, "trx_types")
  if(is.null(trx_types)) {
    if (required) {
      rlang::abort(c(x = "No transactions have been attached to `.data`.",
                     i = "Add transaction data using `add_transactions()` before calling this function."))
    }
    return(NULL)
  }
  trx_types
}
