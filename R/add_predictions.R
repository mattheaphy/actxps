#' Add predictions to a data frame
#'
#' @description Attach predicted values from a model to a data frame with
#' exposure-level records.
#'
#' @details This function attaches predictions from a model to a data frame
#' that preferably has the class `exposed_df`. The `model` argument must be
#' a model object that has an S3 method for the [predict()] function. This
#' method must have new data for predictions as the second argument.
#'
#' The `col_expected` argument is optional.
#'
#' - If `NULL`, names from the result of [predict()] will be used. If there are
#'  no names, a default name of "expected" is assumed. In the event that
#'  [predict()] returns multiple values, the default name will be suffixed by
#'  "_x", where x = 1 to the number of values returned.
#' - If a value is passed, it must be a character vector of same length as
#'   the result of [predict()]
#'
#' @param .data A data frame, preferably with the class `exposed_df`
#' @param model A model object that has an S3 method for [predict()]
#' @param ... Additional arguments passed to [predict()]
#' @param col_expected `NULL` or a character vector containing column names for
#' each value returned by [predict()]
#'
#' @examples
#' expo <- expose_py(census_dat, "2019-12-31") |>
#'   mutate(surrender = status == "Surrender")
#' mod <- glm(surrender ~ inc_guar + pol_yr, expo, family = 'binomial')
#' add_predictions(expo, mod, type = 'response')
#'
#' @returns A data frame or `exposed_df` object with one of more new columns
#' containing predictions.
#'
#' @export
add_predictions <- function(.data, model, ..., col_expected = NULL) {

  preds <- stats::predict(model, .data, ...)

  # assign names
  if (is.null(col_expected)) {

    if (is.null(colnames(preds))) {

      n <- ncol(preds)
      if (is.null(n) || n == 1) {
        col_expected <- "expected"
      } else {
        col_expected <- paste("expected", 1:n, sep = "_")
      }

    } else {
      col_expected <- colnames(preds)
    }

  }

  # convert to a data frame if necessary
  if (!is.data.frame(preds)) {
    preds <- as.data.frame(preds)
  }

  list(.data, purrr::set_names(preds, col_expected)) |>
    purrr::list_cbind()

}
