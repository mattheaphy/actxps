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
#' @details `dat` must be an `exposed_df` with calendar year, quarter, month,
#' or week exposure records. Calendar year exposures are created by the
#' functions [expose_cy()], [expose_cq()], [expose_cm()], or [expose_cw()], (or
#' [expose()] when `cal_expo = TRUE`).
#'
#' @param dat An `exposed_df` object with calendar period exposures.
#'
#' @return For `expose_split()`, a tibble with class `split_exposed_df`,
#' `exposed_df`, `tbl_df`, `tbl`, and `data.frame`. The results include all
#' columns in `dat` except that `exposure` has been renamed to `exposure_cal`.
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
#' @seealso [expose()]
#'
#' @export
expose_split <- function(dat) {

  verify_exposed_df(dat)
  expo_type <- strsplit(attr(dat, "exposure_type"), "_")[[1]]

  if (expo_type[[1]] != "calendar") {
    rlang::abort(c(x = "`dat` must contain calendar exposures.",
                   i = "Hint: Try passing an `exposed_df` object that was created by `expose_cy()`, `expose_cq()`, `expose_cm()`, or `expose_cw()`."))
  }

  if (!is.null(attr(dat, "trx_types"))) {
    rlang::warn(c("!" = "`dat` has transactions attached. This will lead to duplication of transactions after exposures are split.",
                  "i" = "Try calling `add_transactions()` after calling `expose_split()` instead of beforehand.")
    )
  }

  target_status <- attr(dat, "target_status")
  default_status <- attr(dat, "default_status")
  date_cols <- attr(dat, "date_cols") |> rlang::parse_exprs()
  expo_length <- expo_type[[2]]

  pol_frac <- function(x, start, end, y) {
    if (missing(y)) {
      as.integer(x - start + 1) / as.integer(end - start + 1)
    } else {
      as.integer(x - y) / as.integer(end - start + 1)
    }

  }
  cal_frac <- cal_frac(expo_length)

  # time fractions
  # h = yearfrac from boy to anniv
  # v = yearfrac from boy to term

  dat <- dat |>
    # temporary generic date column names
    rename(cal_b = !!date_cols[[1]],
           cal_e = !!date_cols[[2]]) |>
    mutate(
      anniv = issue_date %m+%
        (lubridate::years(1) *
           (lubridate::year(cal_b) - lubridate::year(issue_date))),
      split = between(anniv, cal_b, cal_e),
      h = cal_frac(anniv, 1),
      v = cal_frac(term_date)
    )

  pre_anniv <- dat |>
    filter(split) |>
    mutate(piece = 1L,
           cal_b = pmax(issue_date, cal_b),
           cal_e = anniv - 1,
           exposure = h,
           exposure_pol = 1 - pol_frac(cal_b - 1L,
                                       anniv %m-% lubridate::years(1),
                                       anniv - 1L)
    )

  post_anniv <- dat |>
    mutate(piece = 2L,
           cal_b = dplyr::if_else(split, anniv, cal_b),
           exposure = dplyr::if_else(split, 1 - h, 1),
           anniv = dplyr::if_else(anniv > cal_e,
                                  anniv %m-% lubridate::years(1),
                                  anniv),
           exposure_pol = pol_frac(cal_e,
                                   anniv,
                                   anniv %m+% lubridate::years(1) - 1L,
                                   cal_b - 1L)
    )

  dat <- dplyr::bind_rows(pre_anniv, post_anniv) |>
    filter(cal_b <= cal_e,
           is.na(term_date) | term_date >= cal_b) |>
    mutate(term_date = dplyr::if_else(between(term_date, cal_b, cal_e),
                                      term_date, lubridate::NA_Date_),
           pol_yr = lubridate::year(anniv) - lubridate::year(issue_date) +
             piece - 1L,
           status = dplyr::if_else(is.na(term_date),
                                   factor(default_status,
                                          levels = levels(dat$status)),
                                   status),
           claims = status %in% target_status,
           exposure_cal = dplyr::case_when(
             claims ~ dplyr::if_else(piece == 1 | cal_b == issue_date,
                                     1, 1 - h),
             is.na(term_date) ~ exposure,
             piece == 1 ~ v,
             .default = v - h
           ),
           exposure_pol = dplyr::case_when(
             claims ~ dplyr::case_when(
               piece == 1 ~ exposure_pol,
               split ~ 1,
               TRUE ~ 1 - pol_frac(cal_b - 1L,
                                   anniv,
                                   anniv %m+% lubridate::years(1) - 1L)
             ),
             is.na(term_date) ~ exposure_pol,
             piece == 1 ~ pol_frac(term_date,
                                   anniv %m-% lubridate::years(1),
                                   anniv - 1L) - (1 - exposure_pol),
             TRUE ~ pol_frac(term_date,
                             anniv,
                             anniv %m+% lubridate::years(1) - 1L)
           )
    ) |>
    arrange(pol_num, cal_b, piece) |>
    select(-h, -v, -split, -anniv, -claims, -exposure, -piece) |>
    # restore date column names
    rename(!!date_cols[[1]] := cal_b,
           !!date_cols[[2]] := cal_e)

  # update exposure type and update class
  class(dat) <- c("split_exposed_df", class(dat))
  attr(dat, "exposure_type") <- paste0("split_", expo_length)

  dat

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
