#' Split calendar exposures by policy year
#'
#' @description Split calendar period exposures that cross a policy anniversary
#' into a pre-anniversary record and a post-anniversary record.
#'
#' After splitting the data, the resulting data frame will contain both calendar
#' exposures and policy year exposures. These columns will be named
#' `exposure_cal` and `exposure_pol`, respectively. Calendar exposures will be
#' in the original units passed to `expose_split()`. Policy exposures will
#' always be expressed in years.
#'
#' After splitting exposures, downstream functions like `exp_stats()` and
#' `exp_shiny()` will require clarification as to which exposure basis should
#' be used to summarize results.
#'
#' `is_split_exposed_df()` will return `TRUE` if `x` is a `split_exposed_df`
#' object.
#'
#' @details `.data` must be an `exposed_df` with calendar year, quarter, month,
#' or week exposure records. Calendar year exposures are created by the
#' functions [expose_cy()], [expose_cq()], [expose_cm()], or [expose_cw()], (or
#' [expose()] when `cal_expo = TRUE`).
#'
#' @param .data An `exposed_df` object with calendar period exposures.
#' @param x Any object
#'
#' @return For `expose_split()`, a tibble with class `split_exposed_df`,
#' `exposed_df`, `tbl_df`, `tbl`, and `data.frame`. The results include all
#' columns in `.data` except that `exposure` has been renamed to `exposure_cal`.
#' Additional columns include:
#'
#' - `exposure_pol` - policy year exposures
#' - `pol_yr` - policy year
#'
#' For `is_split_exposed_df()`, a length-1 logical vector.
#'
#' @examples
#' toy_census |> expose_cy("2022-12-31") |> expose_split()
#'
#' @seealso [expose()] for information on creating exposure records from census
#' data.
#'
#' @export
expose_split <- function(.data) {

  verify_exposed_df(.data)
  expo_type <- strsplit(attr(.data, "exposure_type"), "_")[[1]]

  if (expo_type[[1]] != "calendar") {
    rlang::abort(c(x = "`.data` must contain calendar exposures.",
                   i = "Hint: Try passing an `exposed_df` object that was created by `expose_cy()`, `expose_cq()`, `expose_cm()`, or `expose_cw()`."))
  }

  if (!is.null(attr(.data, "trx_types"))) {
    rlang::warn(c("!" = "`.data` has transactions attached. This will lead to duplication of transactions after exposures are split.",
                  "i" = "The appropriate order of operations is to call `add_transactions()` after `expose_split()`.")
    )
  }

  target_status <- attr(.data, "target_status")
  default_status <- attr(.data, "default_status")
  date_cols <- attr(.data, "date_cols") |> rlang::parse_exprs()
  start_date <- attr(.data, "start_date")
  end_date <- attr(.data, "end_date")
  expo_length <- expo_type[[2]]

  pol_frac <- function(x, start, end, y = start - 1) {
    as.integer(x - y) / as.integer(end - start + 1)
  }
  cal_frac <- cal_frac(expo_length)

  # clock::add_years with invalid pre-populated
  add_years <- \(x, n) clock::add_years(x, n, invalid = "previous")

  # time fractions
  # b = fraction from boy to cal_b
  #     - usually zero except for new contracts and a truncated start date
  # h = fraction from boy to anniv
  # v = fraction from boy to the earlier of termination and cal_e

  .data <- .data |>
    # temporary generic date column names
    rename(cal_b = !!date_cols[[1]],
           cal_e = !!date_cols[[2]]) |>
    mutate(
      pol_yr = clock::get_year(cal_b) - clock::get_year(issue_date),
      anniv = add_years(issue_date, pol_yr),
      split = between(anniv, cal_b, cal_e),
      cal_b = pmax(start_date, issue_date, cal_b),
      cal_e = pmin(end_date, cal_e),
      b = cal_frac(cal_b, 1),
      h = dplyr::if_else(split, cal_frac(anniv, 1), 0),
      v = dplyr::if_else(is.na(term_date), cal_e, term_date) |> cal_frac()
    )

  pre_anniv <- .data |>
    filter(split) |>
    mutate(piece = 1L,
           next_anniv = anniv,
           cal_e = pmin(end_date, next_anniv - 1),
           exposure = pmin(h, v) - b
    )

  post_anniv <- .data |>
    mutate(piece = 2L,
           cal_b = dplyr::if_else(split, pmax(anniv, start_date), cal_b),
           pol_yr = pol_yr + (cal_b >= anniv),
           exposure = v - pmax(h, b),
           next_anniv = add_years(issue_date, pol_yr)
    )

  .data <- dplyr::bind_rows(pre_anniv, post_anniv) |>
    filter(cal_b <= cal_e,
           is.na(term_date) | term_date >= cal_b) |>
    mutate(anniv = add_years(issue_date, pol_yr - 1L),
           term_date = dplyr::if_else(between(term_date, cal_b, cal_e),
                                      term_date, as.Date(NA)),
           status = dplyr::if_else(is.na(term_date),
                                   factor(default_status,
                                          levels = levels(.data$status)),
                                   status),
           claims = status %in% target_status,
           exposure_cal = dplyr::case_when(
             claims ~ dplyr::if_else(piece == 1 | cal_b == issue_date |
                                       cal_b == start_date,
                                     1, 1 - (h - b)),
             is.na(term_date) ~ exposure,
             piece == 1 ~ v - b,
             .default = v - pmax(h, b)
           ),
           exposure_pol = dplyr::if_else(
             claims,
             1 - pol_frac(cal_b - 1L,
                          anniv,
                          next_anniv - 1L),
             pol_frac(pmin(cal_e, term_date, na.rm = TRUE),
                      anniv,
                      next_anniv - 1L,
                      cal_b - 1L)
           )
    ) |>
    arrange(pol_num, cal_b, piece) |>
    select(-b, -h, -v, -split, -anniv, -next_anniv,
           -claims, -exposure, -piece) |>
    relocate(pol_yr, .after = cal_e) |>
    # restore date column names
    rename(!!date_cols[[1]] := cal_b,
           !!date_cols[[2]] := cal_e)

  # update exposure type and update class
  class(.data) <- c("split_exposed_df", class(.data))
  attr(.data, "exposure_type") <- paste0("split_", expo_length)

  .data

}

#' @rdname expose_split
#' @export
is_split_exposed_df <- function(x) {
  inherits(x, "split_exposed_df")
}


# This internal function sends an error if a `split_exposed_df` is passed
# without clarifying which exposure basis should be used.
check_split_expose_basis <- function(dat, col_exposure) {
  if (is_split_exposed_df(dat) &&
      !col_exposure %in% c("exposure_cal", "exposure_pol")) {
    rlang::abort(c(x = "A `split_exposed_df` was passed without clarifying which exposure basis should be used to summarize results.",
                   i = 'Pass "exposure_pol" to `col_exposure` for policy year exposures.',
                   i = 'Pass "exposure_cal" to `col_exposure` for calendar exposures.'))
  }
}
