#' Create exposoure records from census records
#'
#' @description update me
#'
#' @details add details here
#'
#' @param dat a data frame with census-level records
#'
#' @return something useful
#'
#' @import rlang
#'
#' @examples 1
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
    rename(pol_num = {{col_pol_num}},
           status = {{col_status}},
           issue_date = {{col_issue_date}},
           term_date = {{col_term_date}})

  if(missing(start_date)) start_date <- as.Date("1900-01-01")

  if(!is.factor(.data$status)) .data$status <- factor(.data$status)

  default_status <- factor("Active", levels = levels(.data$status))

  .data |>
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
}
