#' Summarize experience study records
#'
#' @description update me
#'
#' @details add more details here
#'
#' If \code{target_status} isn't provided, all status values except the first level will be used. This method
#' implicitly assumes that the first level is a default active status.
#'
#' @param .data a data frame with exposure-level records
#' @param expected a character vector of expected values
#' @param target_status a character vector of target status values
#'
#' @return something useful
#'
#' @import rlang
#'
#' @examples 1
#'
#' @export
exp_stats <- function(.data, expected, target_status) {

  .groups <- dplyr::groups(.data)

  if (missing(target_status)) {
    target_status <- levels(.data$status)[-1]
  }

  if (!missing(expected)) {
    ex_mean <- glue::glue("mean({expected})") |>
      set_names(expected) |>
      parse_exprs()

    ex_ae <- glue::glue("{expected} / q_obs") |>
      set_names(glue::glue("ae_{expected}")) |>
      parse_exprs()
  } else {
    ex_ae <- ex_mean <- NULL
  }

  res <- .data |>
    dplyr::mutate(term = status %in% target_status) |>
    dplyr::summarize(claims = sum(term),
              exposures = dplyr::n(),
              q_obs = mean(term),
              !!!ex_mean,
              !!!ex_ae,
              .groups = "drop")

  structure(res, class = c("exp_df", class(res)),
            groups = .groups)

}
