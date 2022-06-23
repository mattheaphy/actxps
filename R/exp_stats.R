#' Summarize experience study records
#'
#' @description update me
#'
#' @details add more details here
#'
#' If \code{target_status} isn't provided, \code{exp_stats} will use the same
#' target status from \code{.data} if it is an \code{exposed_df} object. If
#' \code{.data} is not an \code{exposed_df} object, all status values except
#' the first level will be assumed. This will produce a warning message.
#'
#' @param .data a data frame with exposure-level records, ideally of type \code{exposed_df}
#' @param target_status a character vector of target status values
#' @param expected a character vector of expected values
#' @param col_exposure name of the column in \code{.data} containing exposures
#' @param col_status name of the column in \code{.data} containing the policy status
#'
#' @return something useful
#'
#' @import rlang
#'
#' @examples 1
#'
#' @export
exp_stats <- function(.data, target_status = attr(.data, "target_status"),
                      expected, col_exposure = "exposure",
                      col_status = "status") {

  .groups <- dplyr::groups(.data)

  if (is.null(target_status) | target_status == "None") {
    target_status <- levels(.data$status)[-1]
    warn(c(x = "No target status was provided.",
           i = glue::glue("{paste(target_status, collapse = ', ')} was assumed.")))
  }

  if (!missing(expected)) {
    ex_mean <- glue::glue("weighted.mean({expected}, exposure)") |>
      set_names(expected) |>
      parse_exprs()

    ex_ae <- glue::glue("q_obs / {expected}") |>
      set_names(glue::glue("ae_{expected}")) |>
      parse_exprs()
  } else {
    ex_ae <- ex_mean <- NULL
  }


  .data <- .data |>
    dplyr::rename(exposure = {{col_exposure}},
                  status = {{col_status}})

  res <- .data |>
    dplyr::mutate(term = status %in% target_status) |>
    dplyr::summarize(claims = sum(term),
              exposure = sum(exposure),
              q_obs = claims / exposure,
              !!!ex_mean,
              !!!ex_ae,
              .groups = "drop")

  structure(res, class = c("exp_df", class(res)),
            groups = .groups, target_status = target_status)

}
