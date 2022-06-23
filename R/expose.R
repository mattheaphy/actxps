#' Create exposure records from census records
#'
#' @description update me
#'
#' @details add details here
#'
#' @param .data a data frame with census-level records
#' @param end_date experience study end date
#' @param start_date experience study start date
#' @param target_status character vector of target status values
#' @param col_pol_num name of the column in \code{.data} containing the policy number
#' @param col_status name of the column in \code{.data} containing the policy status
#' @param col_issue_date name of the column in \code{.data} containing the issue date
#' @param col_term_date name of the column in \code{.data} containing the termination date
#'
#' @return something useful
#'
#' @import rlang
#'
#' @examples
#' toy_census |> expose("2020-12-31", target_status = "Surrender")
#'
#' @export
expose <- function(.data,
                   end_date,
                   start_date,
                   target_status = "None",
                   col_pol_num = "pol_num",
                   col_status = "status",
                   col_issue_date = "issue_date",
                   col_term_date = "term_date") {

  .data <- .data |>
    dplyr::rename(pol_num = {{col_pol_num}},
           status = {{col_status}},
           issue_date = {{col_issue_date}},
           term_date = {{col_term_date}})

  if(missing(start_date)) start_date <- as.Date("1900-01-01")

  if(!is.factor(.data$status)) .data$status <- factor(.data$status)

  default_status <- factor("Active", levels = levels(.data$status))

  res <- .data |>
    dplyr::filter(issue_date < end_date,
                  is.na(term_date) | term_date > start_date) |>
    dplyr::mutate(
      last_date = pmin(term_date, end_date, na.rm = TRUE),
      pol_yr = lubridate::interval(issue_date, last_date) %/%
                    lubridate::years(1) + 1) |>
    dplyr::slice(rep(dplyr::row_number(), pol_yr)) |>
    dplyr::group_by(pol_num) |>
    dplyr::mutate(
      last_yr = dplyr::row_number() == pol_yr,
      pol_yr = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      exposure =
        ifelse(last_yr & !status %in% target_status,
               (lubridate::interval(issue_date, last_date) /
                  lubridate::years(1)) %% 1, 1),
      status = dplyr::if_else(last_yr, status, default_status),
      term_date = dplyr::if_else(last_yr, term_date, lubridate::NA_Date_)
    ) |>
    dplyr::select(-last_yr, -last_date)

  structure(res, class = c("exposed_df", class(res)),
            target_status = target_status,
            exposure_type = "policy_year",
            start_date = start_date,
            end_date = end_date)

}
