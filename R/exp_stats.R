#' Summarize experience study records
#'
#' @description Create a summary data frame of experience for a given target
#' status.
#'
#' @details If \code{.data} is grouped, the resulting data frame will contain
#' one row per group.
#'
#' If \code{target_status} isn't provided, \code{exp_stats} will use the same
#' target status from \code{.data} if it has the class \code{exposed_df}.
#' Otherwise, \code{.data} is not an \code{exposed_df} object, all status
#' values except the first level will be assumed. This will produce a
#' warning message.
#'
#' The \code{expected} argument is optional. If provided, this argument must
#' be a character vector with values corresponding to columns in \code{.data}
#' containing expected experience. More than one expected basis can be provided.
#'
#' @param .data a data frame with exposure-level records, ideally of type \code{exposed_df}
#' @param target_status a character vector of target status values
#' @param expected a character vector of expected values
#' @param col_exposure name of the column in \code{.data} containing exposures
#' @param col_status name of the column in \code{.data} containing the policy status
#'
#' @return A tibble with class \code{exp_df}, \code{tbl_df}, \code{tbl},
#' and \code{data.frame}. The results include columns for any grouping
#' variables, claims, exposures, and observed decrement rates.
#' If any values are passed to \code{expected}, additional columns will be
#' added for expected decrements and actual-to-expected ratios.
#'
#' @examples
#' toy_census |> expose("2020-12-31", target_status = "Surrender") |>
#'     exp_stats()
#'
#' \dontrun{
#' census_dat |>
#'     expose("2019-12-31", target_status = "Surrender") |>
#'     group_by(pol_yr) |>
#'     exp_stats()}
#'
#' @export
exp_stats <- function(.data, target_status = attr(.data, "target_status"),
                      expected, col_exposure = "exposure",
                      col_status = "status") {

  .groups <- dplyr::groups(.data)

  if (is.null(target_status) | target_status == "None") {
    target_status <- levels(.data$status)[-1]
    rlang::warn(c(x = "No target status was provided.",
           i = glue::glue("{paste(target_status, collapse = ', ')} was assumed.")))
  }

  if (!missing(expected)) {
    ex_mean <- glue::glue("weighted.mean({expected}, exposure)") |>
      purrr::set_names(expected) |>
      rlang::parse_exprs()

    ex_ae <- glue::glue("q_obs / {expected}") |>
      purrr::set_names(glue::glue("ae_{expected}")) |>
      rlang::parse_exprs()
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
