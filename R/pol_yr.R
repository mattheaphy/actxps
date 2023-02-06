#' Calculate policy durations
#'
#' @description Given a vector of dates and a vector of issue dates, calculate
#' policy years, quarters, months, or weeks.
#'
#' @details This function assumes the first day of each policy year is the
#' anniversary date (or issue date in the first year). The last day of each
#' policy year is the day before the next anniversary date. Analogous rules
#' are used for policy quarters, policy months, and policy weeks.
#'
#' @param x A vector of dates
#' @param issue_date A vector of issue dates
#'
#' @return An integer vector
#'
#' @examples
#' pol_yr(as.Date("2021-02-28") + 0:2, "2020-02-29")
#'
#' pol_mth(as.Date("2021-02-28") + 0:2, "2020-02-29")
#'
#' @name pol_yr

pol_interval <- function(x, issue_date, expo_length) {
  ceiling(lubridate::interval(as.Date(issue_date) - 1, x) / expo_length)
}

#' @rdname pol_yr
#' @export
pol_yr <- function(x, issue_date) {
  pol_interval(x, issue_date, lubridate::years(1))
}

#' @rdname pol_yr
#' @export
pol_qtr <- function(x, issue_date) {
  pol_interval(x, issue_date, months(3))
}

#' @rdname pol_yr
#' @export
pol_mth <- function(x, issue_date) {
  pol_interval(x, issue_date, months(1))
}

#' @rdname pol_yr
#' @export
pol_wk <- function(x, issue_date) {
  pol_interval(x, issue_date, lubridate::days(7))
}
