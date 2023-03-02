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
#' `target_status` is used in the calculation of exposures. The annual
#' exposure method is applied, which allocates a full period of exposure for
#' any statuses in `target_status`. For all other statuses, new entrants
#' and exits are partially exposed based on the time elapsed in the observation
#' period. This method is consistent with the Balducci Hypothesis, which assumes
#' that the probability of termination is proportionate to the time elapsed
#' in the observation period. If the annual exposure method isn't desired,
#' `target_status` can be ignored. In this case, partial exposures are
#' always applied regardless of status.
#'
#' `default_status` is used to indicate the default active status that
#' should be used when exposure records are created. If left blank, then the
#' first status level will be assumed to be the default active status.
#'
#' # Policy period and calendar period variations
#'
#' The functions `expose_py()`, `expose_pq()`, `expose_pm()`,
#' `expose_pw()`, `expose_cy()`, `expose_cq()`,
#' `expose_cm()`, `expose_cw()` are convenience functions for
#' specific implementations of `expose()`. The two characters after the
#' underscore describe the exposure type and exposure period, respectively.
#'
#' For exposures types:
#'
#' - `p` refers to policy years
#' - `c` refers to calendar years.
#'
#' For exposure periods:
#'
#' - `y` = years
#' - `q` = quarters
#' - `m` = months
#' - `w` = weeks.
#'
#' @param .data a data frame with census-level records
#' @param end_date experience study end date
#' @param start_date experience study start date. Default value = 1900-01-01.
#' @param target_status character vector of target status values. Default value = `NULL`.
#' @param cal_expo set to TRUE for calendar year exposures. Otherwise policy year exposures are assumed.
#' @param expo_length exposure period length
#' @param col_pol_num name of the column in `.data` containing the policy number
#' @param col_status name of the column in `.data` containing the policy status
#' @param col_issue_date name of the column in `.data` containing the issue date
#' @param col_term_date name of the column in `.data` containing the termination date
#' @param default_status optional scalar character representing the default active status code
#' @param ... arguments passed to `expose()`
#'
#' @return A tibble with class `exposed_df`, `tbl_df`, `tbl`,
#' and `data.frame`. The results include all existing columns in
#' `.data` plus new columns for exposures and observation periods. Observation
#' periods include counters for policy exposures, start dates, and end dates.
#' Both start dates and end dates are inclusive bounds.
#'
#' For policy year exposures, two observation period columns are returned.
#' Columns beginning with (`pol_`) are integer policy periods. Columns
#' beginning with (`pol_date_`) are calendar dates representing
#' anniversary dates, monthiversary dates, etc.
#'
#' @examples
#' toy_census |> expose("2020-12-31")
#'
#' census_dat |> expose_py("2019-12-31", target_status = "Surrender")
#'
#' @references Atkinson and McGarry (2016). Experience Study Calculations.
#' <https://www.soa.org/49378a/globalassets/assets/files/research/experience-study-calculations.pdf>
#'
#' @importFrom lubridate %m+%
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

  end_date <- as.Date(end_date)
  start_date <- as.Date(start_date)

  # helper functions
  rename_col <- function(x, prefix, suffix = "") {
    res <- abbr_period(expo_length)
    paste0(prefix, "_", res, suffix)
  }


  # set up exposure period lengths
  expo_length <- rlang::arg_match(expo_length)
  expo_step <- switch(expo_length,
                      "year" = lubridate::years(1),
                      "quarter" = months(3),
                      "month" = months(1),
                      "week" = lubridate::days(7))

  cal_frac <- switch(expo_length,
                     "year" = year_frac,
                     "quarter" = quarter_frac,
                     "month" = month_frac,
                     'week' = week_frac)

  # column renames and name conflicts
  .data <- .data |>
    rename(pol_num = {{col_pol_num}},
                  status = {{col_status}},
                  issue_date = {{col_issue_date}},
                  term_date = {{col_term_date}}) |>
    .expo_name_conflict(cal_expo, expo_length)

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
    filter(issue_date < end_date,
                  is.na(term_date) | term_date > start_date) |>
    mutate(
      term_date = dplyr::if_else(term_date > end_date,
                                 lubridate::NA_Date_, term_date),
      status = dplyr::if_else(is.na(term_date), default_status, status),
      last_date = pmin(term_date, end_date, na.rm = TRUE))

  if (cal_expo) {
    res <- res |>
      mutate(
        first_date = pmax(issue_date, start_date),
        cal_b = lubridate::floor_date(first_date, expo_length),
        tot_per = lubridate::interval(
          cal_b,
          lubridate::floor_date(last_date, expo_length)
        ) / expo_step,
        rep_n = ceiling(tot_per) + 1)
  } else {
    res <- res |>
      mutate(
        tot_per = lubridate::interval(issue_date - 1, last_date) / expo_step,
        rep_n = ceiling(tot_per))
  }

  # apply exposures
  res <- res |>
    dplyr::slice(rep(dplyr::row_number(), rep_n)) |>
    group_by(pol_num) |>
    mutate(.time = dplyr::row_number()) |>
    dplyr::ungroup() |>
    mutate(
      last_per = .time == rep_n,
      status = dplyr::if_else(last_per, status, default_status),
      term_date = dplyr::if_else(last_per, term_date, lubridate::NA_Date_))

  if (cal_expo) {
    res <- res |>
      mutate(first_per = .time == 1,
                    cal_e = cal_b %m+% (expo_step * .time) - 1,
                    cal_b = cal_b %m+% (expo_step * (.time - 1)),
                    exposure = dplyr::case_when(
                      status %in% target_status ~ 1,
                      first_per & last_per ~ cal_frac(last_date) - cal_frac(first_date, 1),
                      first_per ~ 1 - cal_frac(first_date, 1),
                      last_per ~ cal_frac(last_date),
                      TRUE ~ 1)
      ) |>
      dplyr::select(-rep_n, -first_date, -last_date, -first_per, -last_per,
                    -.time, -tot_per) |>
      relocate(cal_e, .after = cal_b) |>
      dplyr::rename_with(.fn = rename_col, .cols = cal_b, prefix = "cal") |>
      dplyr::rename_with(.fn = rename_col, .cols = cal_e, prefix = "cal",
                         suffix = "_end")
  } else {
    res <- res |>
      mutate(
        cal_b = issue_date %m+% (expo_step * (.time - 1)),
        cal_e = issue_date %m+% (expo_step * .time) - 1,
        exposure = dplyr::if_else(last_per & !status %in% target_status,
                                  tot_per %% 1, 1),
        # exposure = 0 is possible if exactly 1 period has elapsed. replace these with 1's
        exposure = dplyr::if_else(exposure == 0, 1, exposure)
      ) |>
      dplyr::select(-last_per, -last_date, -tot_per, -rep_n) |>
      filter(between(cal_b, start_date, end_date)) |>
      dplyr::rename_with(.fn = rename_col, .cols = .time, prefix = "pol") |>
      dplyr::rename_with(.fn = rename_col, .cols = cal_b, prefix = "pol_date") |>
      dplyr::rename_with(.fn = rename_col, .cols = cal_e, prefix = "pol_date",
                         suffix = "_end")


  }

  # set up S3 object
  new_exposed_df(res, end_date, start_date,
                 target_status, cal_expo, expo_length,
                 trx_types = NULL)

}


#' @rdname expose
#' @export
expose_py <- function(...) {
  expose(cal_expo = FALSE, expo_length = "year", ...)
}

#' @rdname expose
#' @export
expose_pq <- function(...) {
  expose(cal_expo = FALSE, expo_length = "quarter", ...)
}

#' @rdname expose
#' @export
expose_pm <- function(...) {
  expose(cal_expo = FALSE, expo_length = "month", ...)
}

#' @rdname expose
#' @export
expose_pw <- function(...) {
  expose(cal_expo = FALSE, expo_length = "week", ...)
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
.expo_name_conflict <- function(.data, cal_expo, expo_length) {

  abbrev <- abbr_period(expo_length)

  x <- c(
    "exposure",
    paste0(if (cal_expo) "cal_" else "pol_", abbrev),
    if (!cal_expo) paste0("pol_date_", abbrev),
    paste0(if (cal_expo) "cal_" else "pol_date_", abbrev, "_end")
  )

  x <- x[x %in% names(.data)]
  .data[x] <- NULL
  if (length(x > 0)) {
    rlang::warn(c(x = glue::glue("`.data` contains the following conflicting columns that will be overridden: {paste(x, collapse = ', ')}."),
                  i = "If you don't want this to happen, rename these columns prior to calling the applicable expose function."))
  }
  .data
}

# helper function - do not export
abbr_period <- function(x) {
  switch(x,
         "year" = "yr",
         "quarter" = "qtr",
         "month" = "mth",
         'week' = "wk")
}
