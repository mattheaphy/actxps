#' Create exposure records from census records
#'
#' @description Convert a data frame of census-level records to exposure-level
#' records.
#'
#' @details Census-level data refers to a data set wherein there is one row
#' per unique policy. Exposure-level data expands census-level data such that
#' there is one record per policy per observation period. Observation periods
#' could be any meaningful period of time such as a policy year, policy month,
#' calendar year, calendar quarter, calendar month, etc.
#'
#' \code{target_status} is used in the calculation of exposures. The annual
#' exposure method is applied, which allocates a full period of exposure for
#' any statuses in \code{target_status}. For all other statuses, new entrants
#' and exits are partially exposed based on the time elapsed in the observation
#' period. This method is consistent with the Balducci Hypothesis, which assumes
#' that the probability of termination is proportionate to the time elapsed
#' in the observation period. If the annual exposure method isn't desired,
#' \code{target_status} can be ignored. In this case, partial exposures are
#' always applied regardless of status.
#'
#' \code{default_status} is used to indicate the default active status that
#' should be used when exposure records are created. If left blank, then the
#' first status level will be assumed to be the default active status.
#'
#' @param .data a data frame with census-level records
#' @param end_date experience study end date
#' @param start_date experience study start date. Default value = 1900-01-01.
#' @param target_status character vector of target status values. Default value = \code{NULL.}
#' @param col_pol_num name of the column in \code{.data} containing the policy number
#' @param col_status name of the column in \code{.data} containing the policy status
#' @param col_issue_date name of the column in \code{.data} containing the issue date
#' @param col_term_date name of the column in \code{.data} containing the termination date
#' @param default_status optional scalar character representing the default active status code
#' @param cal_type exposure basis for calendar period studies
#'
#' @return A tibble with class \code{exposed_df}, \code{tbl_df}, \code{tbl},
#' and \code{data.frame}. The results include all existing columns in
#' \code{.data} plus new columns for exposures and observation periods.
#'
#' For policy year and policy month exposures, any calendar-based observation
#' periods represent the beginning of the policy year or policy month. For
#' example, using a policy year exposure basis, assume that for a particular
#' record the policy year (\code{pol_yr}) is 3 and the calendar year
#' (\code{cal_yr}) is 2022. This means that it was 2022 at the start of policy
#' year 3.
#'
#' @examples
#' toy_census |> expose("2020-12-31", target_status = "Surrender")
#'
#' \dontrun{
#' census_dat |>
#'     expose("2019-12-31", target_status = "Surrender")}

#'
#' @references https://www.soa.org/49378a/globalassets/assets/files/research/experience-study-calculations.pdf
#'
#' @export
expose <- function(.data,
                   end_date,
                   start_date = as.Date("1900-01-01"),
                   target_status = NULL,
                   col_pol_num = "pol_num",
                   col_status = "status",
                   col_issue_date = "issue_date",
                   col_term_date = "term_date",
                   default_status) {

  .data <- .data |>
    dplyr::rename(pol_num = {{col_pol_num}},
                  status = {{col_status}},
                  issue_date = {{col_issue_date}},
                  term_date = {{col_term_date}})

  if(!is.factor(.data$status)) .data$status <- factor(.data$status)

  if (missing(default_status)) {
    default_status <- factor(levels(.data$status)[[1]],
                             levels = levels(.data$status))
  } else {
    status_levels <- union(levels(.data$status), default_status)
    default_status <- factor(default_status,
                             levels = status_levels)
    levels(.data$status) <- status_levels
  }

  res <- .data |>
    dplyr::filter(issue_date < end_date,
                  is.na(term_date) | term_date > start_date) |>
    dplyr::mutate(
      term_date = dplyr::if_else(term_date > end_date, lubridate::NA_Date_, term_date),
      status = dplyr::if_else(is.na(term_date),default_status, status),
      last_date = pmin(term_date, end_date, na.rm = TRUE),
      tot_int = lubridate::interval(issue_date - 1, last_date),
      tot_yrs = tot_int / lubridate::years(1),
      pol_yr = ceiling(tot_yrs)) |>
    dplyr::slice(rep(dplyr::row_number(), pol_yr)) |>
    dplyr::group_by(pol_num) |>
    dplyr::mutate(
      last_yr = dplyr::row_number() == pol_yr,
      pol_yr = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      cal_yr = lubridate::year(issue_date) + pol_yr - 1,
      exposure = dplyr::if_else(last_yr & !status %in% target_status,
                                tot_yrs %% 1, 1),
      # exposure = 0 is possible if exactly 1 year has elapsed. replace these with 1's
      exposure = dplyr::if_else(exposure == 0, 1, exposure),
      status = dplyr::if_else(last_yr, status, default_status),
      term_date = dplyr::if_else(last_yr, term_date, lubridate::NA_Date_)
    ) |>
    dplyr::select(-last_yr, -last_date, -tot_yrs, -tot_int) |>
    dplyr::filter(cal_yr >= lubridate::year(start_date))

  structure(res, class = c("exposed_df", class(res)),
            target_status = target_status,
            exposure_type = "policy_year",
            start_date = start_date,
            end_date = end_date)

}



#' @rdname expose
#' @export
expose_cal <- function(.data,
                   end_date,
                   start_date = as.Date("1900-01-01"),
                   target_status = NULL,
                   cal_type = c("year", "quarter", "month", "week"),
                   col_pol_num = "pol_num",
                   col_status = "status",
                   col_issue_date = "issue_date",
                   col_term_date = "term_date",
                   default_status) {

  cal_type <- rlang::arg_match(cal_type)

  .data <- .data |>
    dplyr::rename(pol_num = {{col_pol_num}},
                  status = {{col_status}},
                  issue_date = {{col_issue_date}},
                  term_date = {{col_term_date}})

  if(!is.factor(.data$status)) .data$status <- factor(.data$status)

  if (missing(default_status)) {
    default_status <- factor(levels(.data$status)[[1]],
                             levels = levels(.data$status))
  } else {
    status_levels <- union(levels(.data$status), default_status)
    default_status <- factor(default_status,
                             levels = status_levels)
    levels(.data$status) <- status_levels
  }

  cal_frac <- switch(cal_type,
                     "year" = year_frac,
                     "quarter" = quarter_frac,
                     "month" = month_frac,
                     'week' = week_frac)

  cal_step <- switch(cal_type,
                     "year" = lubridate::years(1),
                     "quarter" = months(3),
                     "month" = months(1),
                     "week" = lubridate::days(7))

  res <- .data |>
    dplyr::filter(issue_date < end_date,
                  is.na(term_date) | term_date > start_date) |>
    dplyr::mutate(
      term_date = dplyr::if_else(term_date > end_date,
                                 lubridate::NA_Date_, term_date),
      status = dplyr::if_else(is.na(term_date),default_status, status),
      last_date = pmin(term_date, end_date, na.rm = TRUE), # same
      first_date = pmax(issue_date, start_date),
      cal_b = lubridate::floor_date(first_date, cal_type),
      tot_int = lubridate::interval(
        cal_b,
        lubridate::floor_date(last_date, cal_type)
      ),
      tot_per = tot_int / cal_step,
      rep_n = ceiling(tot_per) + 1) |>
    dplyr::slice(rep(dplyr::row_number(), rep_n)) |>
    dplyr::group_by(pol_num) |>
    dplyr::mutate(cal_per = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      last_per = cal_per == rep_n,
      first_per = cal_per == 1,
      cal_per = cal_b + cal_step * (cal_per - 1),
      exposure = dplyr::case_when(
        status %in% target_status ~ 1,
        first_per & last_per ~ cal_frac(last_date) - cal_frac(first_date, 1),
        first_per ~ 1 - cal_frac(first_date, 1),
        last_per ~ cal_frac(last_date),
        TRUE ~ 1
      ),
      status = dplyr::if_else(last_per, status, default_status),
      term_date = dplyr::if_else(last_per, term_date, lubridate::NA_Date_)
    ) |>
    dplyr::select(-rep_n, -first_date, -last_date, -first_per, -last_per,
                  -cal_b, -tot_int, -tot_per) |>
    dplyr::rename_with(
      .fn = function(x) {
        switch(cal_type,
               "year" = "cal_yr",
               "quarter" = "cal_qtr",
               "month" = "cal_mth",
               'week' = "cal_wk")
      },
      .cols = cal_per
    )

  structure(res, class = c("exposed_df", class(res)),
            target_status = target_status,
            exposure_type = paste0("calendar_", cal_type),
            start_date = start_date,
            end_date = end_date)

}

#' @rdname expose
#' @export
expose_cy <- function(...) {
  expose_cal(cal_type = "year", ...)
}

#' @rdname expose
#' @export
expose_cq <- function(...) {
  expose_cal(cal_type = "quarter", ...)
}

#' @rdname expose
#' @export
expose_cm <- function(...) {
  expose_cal(cal_type = "month", ...)
}

#' @rdname expose
#' @export
expose_cw <- function(...) {
  expose_cal(cal_type = "week", ...)
}



year_frac <- function(x, .offset = 0) {
  (lubridate::yday(x) - .offset) / (365 + lubridate::leap_year(x))
}

quarter_frac <- function(x, .offset = 0) {
  (lubridate::qday(x) - .offset) /
    lubridate::qday((lubridate::ceiling_date(x, "quarter") - 1))
}

month_frac <- function(x, .offset = 0) {
  (lubridate::mday(x) - .offset) /
    lubridate::mday((lubridate::ceiling_date(x, "month") - 1))
}

week_frac <- function(x, .offset = 0) {
  (lubridate::wday(x) - .offset) / 7
}
