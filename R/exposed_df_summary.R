#' Summarize experience study records
#'
#' Create a summary data frame of termination experience for a given target
#' status.
#'
#' @details
#' Calling [summary()] on an `exposed_df` object will summarize results using
#' [exp_stats()]. See [exp_stats()] for more information.
#'
#' @param object A data frame with exposure-level records
#'
#' @param ... Additional arguments passed to [exp_stats()]
#'
#' @return A tibble with class `exp_df`, `tbl_df`, `tbl`,
#' and `data.frame`.
#'
#' @examples
#' toy_census |> expose("2022-12-31", target_status = "Surrender") |>
#'     summary()
#'
#' @seealso [exp_stats()]
#'
#' @export
summary.exposed_df <- function(object, ...) {
  exp_stats(object, ...)
}
