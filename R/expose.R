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
#' should be used when exposure records are created.
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
#' - `c` refers to calendar years
#'
#' For exposure periods:
#'
#' - `y` = years
#' - `q` = quarters
#' - `m` = months
#' - `w` = weeks
#'
#' All columns containing dates must be in YYYY-MM-DD format.
#'
#' @param .data A data frame with census-level records
#' @param end_date Experience study end date
#' @param start_date Experience study start date. Default value = 1900-01-01.
#' @param target_status Character vector of target status values. Default value
#'  = `NULL`.
#' @param cal_expo Set to TRUE for calendar year exposures. Otherwise policy
#' year exposures are assumed.
#' @param expo_length Exposure period length
#' @param col_pol_num Name of the column in `.data` containing the policy number
#' @param col_status Name of the column in `.data` containing the policy status
#' @param col_issue_date Name of the column in `.data` containing the issue date
#' @param col_term_date Name of the column in `.data` containing the termination
#' date
#' @param default_status Optional scalar character representing the default
#' active status code. If not provided, the most common status is assumed.
#' @param ... Arguments passed to `expose()`
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
#' @seealso [expose_split()] for information on splitting calendar year
#' exposures by policy year.
#'
#' @references Atkinson and McGarry (2016). Experience Study Calculations.
#' <https://www.soa.org/49378a/globalassets/assets/files/research/experience-study-calculations.pdf>
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
  cal_frac <- cal_frac(expo_length)
  cal_floor <- cal_floor(expo_length)
  add_period <- add_period(expo_length)

  n_term_dates <- sum(!is.na(.data[[col_term_date]]))

  # column renames and name conflicts
  .data <- .data |>
    rename(pol_num = {{col_pol_num}},
           status = {{col_status}},
           issue_date = {{col_issue_date}},
           term_date = {{col_term_date}}) |>
    .expo_name_conflict(cal_expo, expo_length) |>
    # convert to dates if needed
    mutate(dplyr::across(c(issue_date, term_date), .convert_date))

  .check_missing_dates(.data$issue_date, "issue_date")

  if (n_term_dates != sum(!is.na(.data$term_date))) {
    rlang::abort(c(
      "Bad termination date formats were detected.",
      i = "Make sure all dates are in YYYY-MM-DD format.")
    )
  }

  # set up statuses
  if (!is.factor(.data$status)) .data$status <- factor(.data$status)

  if (missing(default_status)) {
    default_status <- most_common(.data$status)
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
                                 as.Date(NA), term_date),
      status = dplyr::if_else(is.na(term_date), default_status, status),
      last_date = pmin(term_date, end_date, na.rm = TRUE))

  if (cal_expo) {
    res <- res |>
      mutate(
        first_date = pmax(issue_date, start_date),
        cal_b = cal_floor(first_date),
        rep_n = clock::date_count_between(cal_b, last_date,
                                          expo_length) + 1L)
  } else {
    res <- res |>
      mutate(
        rep_n = clock::date_count_between(issue_date, last_date,
                                          expo_length) + 1L)
  }

  # apply exposures
  res <- res |>
    slice(rep(dplyr::row_number(), rep_n)) |>
    group_by(pol_num) |>
    mutate(.time = dplyr::row_number()) |>
    ungroup() |>
    mutate(
      last_per = .time == rep_n,
      status = dplyr::if_else(last_per, status, default_status),
      term_date = dplyr::if_else(last_per, term_date, as.Date(NA)))

  if (cal_expo) {
    res <- res |>
      mutate(first_per = .time == 1,
             cal_e = add_period(cal_b, .time) - 1L,
             cal_b = add_period(cal_b, .time - 1L),
             exposure = dplyr::case_when(
               status %in% target_status ~ 1,
               first_per & last_per ~ cal_frac(last_date) -
                 cal_frac(first_date, 1),
               first_per ~ 1 - cal_frac(first_date, 1),
               last_per ~ cal_frac(last_date),
               TRUE ~ 1)
      ) |>
      select(-rep_n, -first_date, -last_date, -first_per, -last_per,
             -.time) |>
      relocate(cal_e, .after = cal_b) |>
      dplyr::rename_with(.fn = rename_col, .cols = cal_b, prefix = "cal") |>
      dplyr::rename_with(.fn = rename_col, .cols = cal_e, prefix = "cal",
                         suffix = "_end")
  } else {
    res <- res |>
      mutate(
        cal_b = add_period(issue_date, .time - 1L),
        cal_e = add_period(issue_date, .time) - 1L,
        exposure = dplyr::if_else(
          last_per & !status %in% target_status,
          as.integer((last_date - cal_b + 1))  /
            as.integer(cal_e - cal_b + 1),
          1),
        # exposure = 0 is possible if exactly 1 period has elapsed. replace these with 1's
        exposure = dplyr::if_else(exposure == 0, 1, exposure)
      ) |>
      select(-last_per, -last_date, -rep_n) |>
      filter(between(cal_b, start_date, end_date)) |>
      dplyr::rename_with(.fn = rename_col, .cols = .time, prefix = "pol") |>
      dplyr::rename_with(.fn = rename_col, .cols = cal_b, prefix = "pol_date") |>
      dplyr::rename_with(.fn = rename_col, .cols = cal_e, prefix = "pol_date",
                         suffix = "_end")


  }

  # set up S3 object
  new_exposed_df(res, end_date, start_date,
                 target_status, cal_expo, expo_length,
                 trx_types = NULL, default_status)

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

# helper functions for calendar year fractions - do not export
year_frac <- function(x, .offset = 0) {
  xday <- clock::as_year_day(x) |> clock::get_day()
  (xday - .offset) / (365 + clock::date_leap_year(x))
}

quarter_frac <- function(x, .offset = 0) {
  xday <- clock::as_year_quarter_day(x) |> clock::get_day()
  qdays <- (clock::date_group(x, "month", n = 3) |>
              clock::add_quarters(1, invalid = "previous") - 1L) |>
    clock::as_year_quarter_day() |>
    clock::get_day()
  (xday - .offset) / qdays
}

month_frac <- function(x, .offset = 0) {
  xday <- clock::get_day(x)
  mdays <- (clock::date_group(x, "month") |>
              clock::add_months(1, invalid = "previous") - 1L) |>
    clock::get_day()
  (xday - .offset) / mdays
}

week_frac <- function(x, .offset = 0) {
  (clock::as_year_week_day(x) |> clock::get_day() - .offset) / 7
}

cal_frac <- function(expo_length) {
  switch(expo_length,
         "year" = year_frac,
         "quarter" = quarter_frac,
         "month" = month_frac,
         'week' = week_frac)
}

# helper functions for calendar year flooring
year_floor <- function(x) {
  clock::date_group(x, "year")
}

quarter_floor <- function(x) {
  clock::date_group(x, "month", n = 3)
}

month_floor <- function(x) {
  clock::date_group(x, "month")
}

week_floor <- function(x) {
  sunday <- clock::weekday(clock::clock_weekdays$sunday)
  clock::date_shift(x, target = sunday, which = "previous")
}

cal_floor <- function(expo_length) {
  switch(expo_length,
         "year" = year_floor,
         "quarter" = quarter_floor,
         "month" = month_floor,
         'week' = week_floor)
}

# helper function for adding period
add_period <- function(expo_length) {
  fun <- switch(expo_length,
                "year" = clock::add_years,
                "quarter" = clock::add_quarters,
                "month" = clock::add_months,
                'week' = clock::add_weeks)
  if (expo_length == "week") return(fun)
  \(x, n) fun(x, n, invalid = "previous")
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

# determine the most common status
most_common <- function(x) {
  y <- table(x) |> sort(decreasing = TRUE) |> names()
  factor(y[[1]], levels(x))
}

is.Date <- function(x) {
  inherits(x, "Date")
}

.convert_date <- function(x) {
  if (is.Date(x)) return(x)
  clock::date_parse(x, format = "%Y-%m-%d")
}

.check_missing_dates <- function(x, name) {
  if (any(is.na(x))) {
    rlang::abort(c(
      glue::glue("Missing values are not allowed in the `{name}` column."),
      i = "Make sure all dates are in YYYY-MM-DD format.")
    )
  }
}
