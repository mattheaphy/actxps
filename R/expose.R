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
#' @param cal_expo set to TRUE for calendar year exposures. Otherwise policy year exposures are used.
#' @param expo_length exposure period length
#' @param col_pol_num name of the column in \code{.data} containing the policy number
#' @param col_status name of the column in \code{.data} containing the policy status
#' @param col_issue_date name of the column in \code{.data} containing the issue date
#' @param col_term_date name of the column in \code{.data} containing the termination date
#' @param default_status optional scalar character representing the default active status code
#' @param ... additional arguments passed to \code{expose_cal}
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
                   cal_expo = FALSE,
                   expo_length = c("year", "quarter", "month", "week"),
                   col_pol_num = "pol_num",
                   col_status = "status",
                   col_issue_date = "issue_date",
                   col_term_date = "term_date",
                   default_status) {

  # set up exposure period lengths
  expo_length <- rlang::arg_match(expo_length)
  expo_step <- switch(expo_length,
                      "year" = lubridate::years(1),
                      "quarter" = months(3),
                      "month" = months(1),
                      "week" = lubridate::days(7))

  # calendar fraction function
  if (cal_expo) {

    cal_frac <- switch(expo_length,
                       "year" = year_frac,
                       "quarter" = quarter_frac,
                       "month" = month_frac,
                       'week' = week_frac)

  }

  # column renames and name conflicts
  .data <- .data |>
    dplyr::rename(pol_num = {{col_pol_num}},
                  status = {{col_status}},
                  issue_date = {{col_issue_date}},
                  term_date = {{col_term_date}}) |>
    .name_conflict(if(cal_expo) "exposure" else c("pol_yr", "exposure", "cal_yr"))

  # set up statuses
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

  # pre-exposure updates
  res <- .data |>
    dplyr::filter(issue_date < end_date,
                  is.na(term_date) | term_date > start_date) |>
    dplyr::mutate(
      term_date = dplyr::if_else(term_date > end_date,
                                 lubridate::NA_Date_, term_date),
      status = dplyr::if_else(is.na(term_date),default_status, status),
      last_date = pmin(term_date, end_date, na.rm = TRUE))

  if (cal_expo) {
    res <- res |>
      dplyr::mutate(
        first_date = pmax(issue_date, start_date),
        cal_b = lubridate::floor_date(first_date, expo_length),
        tot_per = lubridate::interval(
          cal_b,
          lubridate::floor_date(last_date, expo_length)
        ) / expo_step,
        rep_n = ceiling(tot_per) + 1)
  } else {
    res <- res |>
      dplyr::mutate(
        tot_per = lubridate::interval(issue_date - 1, last_date) /
          lubridate::years(1),
        rep_n = ceiling(tot_per))
  }

  # apply exposures
  res <- res |>
    dplyr::slice(rep(dplyr::row_number(), rep_n)) |>
    dplyr::group_by(pol_num) |>
    dplyr::mutate(.time = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      last_per = .time == rep_n,
      status = dplyr::if_else(last_per, status, default_status),
      term_date = dplyr::if_else(last_per, term_date, lubridate::NA_Date_))

  if (cal_expo) {
    res <- res |>
      dplyr::mutate(first_per = .time == 1,
                    .time = cal_b + expo_step * (.time - 1),
                    exposure = dplyr::case_when(
                      status %in% target_status ~ 1,
                      first_per & last_per ~ cal_frac(last_date) - cal_frac(first_date, 1),
                      first_per ~ 1 - cal_frac(first_date, 1),
                      last_per ~ cal_frac(last_date),
                      TRUE ~ 1)
      ) |>
      dplyr::select(-rep_n, -first_date, -last_date, -first_per, -last_per,
                    -cal_b, -tot_per) |>
      dplyr::rename_with(
        .fn = function(x) {
          switch(expo_length,
                 "year" = "cal_yr",
                 "quarter" = "cal_qtr",
                 "month" = "cal_mth",
                 'week' = "cal_wk")
        },
        .cols = .time
      )
  } else {
    res <- res |>
      dplyr::mutate(
        cal_yr = lubridate::year(issue_date) + .time - 1,
        exposure = dplyr::if_else(last_per & !status %in% target_status,
                                  tot_per %% 1, 1),
        # exposure = 0 is possible if exactly 1 period has elapsed. replace these with 1's
        exposure = dplyr::if_else(exposure == 0, 1, exposure)
      ) |>
      dplyr::select(-last_per, -last_date, -tot_per, -rep_n) |>
      dplyr::filter(cal_yr >= lubridate::year(start_date)) |>
      dplyr::rename(pol_yr = .time)


  }

  # set up S3 object
  structure(res, class = c("exposed_df", class(res)),
            target_status = target_status,
            exposure_type = if(cal_expo) {
              paste0("calendar_", expo_length)
            } else {
              "policy_year"
            },
            start_date = start_date,
            end_date = end_date)
}


#' @rdname expose
#' @export
expose_py <- function(...) {
  expose(cal_expo = FALSE, ...)
}

#' @rdname expose
#' @export
expose_cy <- function(...) {
  expose(cal_expo = TRUE, expo_length = "year", ...)
}

#' @rdname expose
#' @export
expose_cq <- function(...) {
  expose(cal_expo = TRUE, expo_length = "quarter", ...)
}

#' @rdname expose
#' @export
expose_cm <- function(...) {
  expose(cal_expo = TRUE, expo_length = "month", ...)
}

#' @rdname expose
#' @export
expose_cw <- function(...) {
  expose(cal_expo = TRUE, expo_length = "week", ...)
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

# helper function to handle name conflicts
.name_conflict <- function(.data, x) {
  x <- x[x %in% names(.data)]
  .data[x] <- NULL
  if (length(x > 0)) {
    rlang::warn(c(x = glue::glue(".data contains the following conflicting columns that will be overridden: {paste(x, collapse = ', ')}. If you don't want this to happen, please rename these columns prior to calling the applicable expose function.")))
  }
  .data
}
