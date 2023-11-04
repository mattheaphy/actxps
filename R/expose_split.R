#' Split calendar year exposures by policy year
#'
#' @description Convert a calendar year exposed data frame into a split exposed
#' data frame that divides each calendar year into two pieces: a pre-anniversary
#' record and a post-anniversary record.
#'
#' @details `dat` must be an `exposed_df` with calendar year exposure records.
#' Calendar year exposures are created by the function [expose_cy()] (or
#' [expose()] when `expo_length = "year"` and `cal_expo = TRUE`).
#'
#' @param dat An `exposed_df` object with calendar year exposures.
#'
#' @return A tibble with class `split_exposed_df`, `exposed_df`, `tbl_df`,
#' `tbl`, and `data.frame`. The results include all columns in `dat` except that
#' `exposure` has been renamed to `exposure_cal`. Additional columns include:
#'
#' - `exposure_pol` - policy year exposures
#' - `pol_yr` - policy year
#' - `piece` - a factor containing 2 levels: "pre_anniv" (pre-anniversary
#' records) and "post_anniv" (post-anniversary records)
#'
#' @examples
#' toy_census |> expose_cy("2022-12-31") |> expose_split()
#'
#' @seealso [expose()]
#'
#' @export
expose_split <- function(dat) {

  verify_exposed_df(dat)
  if (attr(dat, "exposure_type") != "calendar_year") {
    rlang::abort(c(x = "`dat` must contain calendar year exposures.",
                   i = "Hint: Try passing an `exposed_df` object that was created by `expose_cy()`."))
  }

  if (!is.null(attr(dat, "trx_types"))) {
    rlang::warn(c("!" = "`dat` has transactions attached. This will lead to duplication of transactions after exposures are split.",
                  "i" = "Try calling `add_transactions()` after calling `expose_split()` instead of beforehand.")
    )
  }

  cal_frac <- cal_frac("year")
  target_status <- attr(dat, "target_status")
  default_status <- attr(dat, "default_status")

  pol_frac <- function(x, start, end) {
    as.integer(x - start + 1) / as.integer(end - start + 1)
  }

  # time fractions
  # h = yearfrac from boy to anniv
  # v = yearfrac from boy to term

  dat <- dat |> mutate(
    anniv = issue_date %m+%
      (lubridate::years(1) *
         (lubridate::year(cal_yr) - lubridate::year(issue_date))),
    split = between(anniv, cal_yr, cal_yr_end),
    h = cal_frac(anniv, 1),
    v = cal_frac(term_date)
  )

  pre_anniv <- dat |>
    filter(split) |>
    mutate(piece = 1L,
           cal_yr = pmax(issue_date, cal_yr),
           cal_yr_end = anniv - 1,
           exposure = h,
           exposure_pol = 1 - pol_frac(cal_yr - 1L,
                                       anniv %m-% lubridate::years(1),
                                       anniv - 1L)
    )

  post_anniv <- dat |>
    mutate(piece = 2L,
           cal_yr = anniv,
           exposure = 1 - h,
           exposure_pol = pol_frac(cal_yr_end,
                                   anniv,
                                   anniv %m+% lubridate::years(1) - 1L))

  dat <- dplyr::bind_rows(pre_anniv, post_anniv) |>
    filter(cal_yr <= cal_yr_end,
           is.na(term_date) | term_date >= cal_yr) |>
    mutate(term_date = dplyr::if_else(between(term_date, cal_yr, cal_yr_end),
                                      term_date, lubridate::NA_Date_),
           pol_yr = lubridate::year(cal_yr) - lubridate::year(issue_date) +
             piece - 1L,
           status = dplyr::if_else(is.na(term_date),
                                   factor(default_status,
                                          levels = levels(dat$status)),
                                   status),
           claims = status %in% target_status,
           exposure_cal = dplyr::case_when(
             claims ~ dplyr::if_else(piece == 1 | cal_yr == issue_date,
                                     1, 1 - h),
             is.na(term_date) ~ exposure,
             piece == 1 ~ v,
             .default = v - h
           ),
           exposure_pol = dplyr::case_when(
             claims ~ dplyr::if_else(piece == 2, 1, exposure_pol),
             is.na(term_date) ~ exposure_pol,
             piece == 1 ~ pol_frac(term_date,
                                   anniv %m-% lubridate::years(1),
                                   anniv - 1L) - (1 - exposure_pol),
             .default = pol_frac(term_date,
                                 anniv,
                                 anniv %m+% lubridate::years(1) - 1L)
           ),
           piece = dplyr::if_else(piece == 2, "post_anniv", "pre_anniv") |>
             factor()
    ) |>
    arrange(pol_num, cal_yr, piece) |>
    select(-h, -v, -split, -anniv, -claims, -exposure)

  class(dat) <- c("split_exposed_df", class(dat))
  attr(dat, "exposure_type") <- "split_year"

  dat

}

# This internal function sends a warning if a `split_exposed_df` is passed
# without clarifying which exposure basis should be used.
check_warn_split_expose <- function(dat, col_exposure) {
  if (inherits(dat, "split_exposed_df") && col_exposure == "exposure") {
    rlang::abort(c(x = "A `split_exposed_df` was passed without clarifying whether policy or calendar year exposures should be used to summarize results.",
                   i = 'Pass "exposure_pol" (for policy year exposures) or "exposure_cal" (for calendar year exposures) to the `col_exposure` argument.'))
  }
}
