#' Calculate policy duration
#'
#' @description Given a vector of dates and a vector of issue dates, calculate
#' policy years, quarters, months, weeks, or other durations.
#'
#' @details These functions assume the first day of each policy year is the
#' anniversary date (or issue date in the first year). The last day of each
#' policy year is the day before the next anniversary date. Analogous rules
#' are used for policy quarters, policy months, and policy weeks.
#'
#' The [pol_interval()] function can be used to determine any arbitrary
#' duration passed to the `dur_length` argument.
#'
#' @param x A vector of dates
#' @param issue_date A vector of issue dates
#' @param dur_length Duration length
#'
#' @return An integer vector
#'
#' @examples
#' pol_yr(as.Date("2021-02-28") + 0:2, "2020-02-29")
#'
#' pol_mth(as.Date("2021-02-28") + 0:2, "2020-02-29")
#'
#' @name pol_yr
#' @rdname pol_yr
#' @export
pol_yr <- function(x, issue_date) {
  pol_interval(x, issue_date, "year")
}

#' @rdname pol_yr
#' @export
pol_qtr <- function(x, issue_date) {
  pol_interval(x, issue_date, "quarter")
}

#' @rdname pol_yr
#' @export
pol_mth <- function(x, issue_date) {
  pol_interval(x, issue_date, "month")
}

#' @rdname pol_yr
#' @export
pol_wk <- function(x, issue_date) {
  pol_interval(x, issue_date, "week")
}

#' @rdname pol_yr
#' @export
pol_interval <- function(x, issue_date,
                         dur_length = c("year", "quarter", "month", "week")) {
  dur_length <- rlang::arg_match(dur_length)
  clock::date_count_between(as.Date(issue_date), as.Date(x), dur_length) + 1L
}

