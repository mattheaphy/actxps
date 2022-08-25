#' Exposed data frame helper functions
#'
#' Test for and coerce to the `exposed_df` class.
#'
#' These are behind-the-scenes functions that will generally not be called
#' by users.
#'
#' `is_exposed_df()` will return `TRUE` if `x` is an `exposed_df` object.
#'
#' `as_exposed_df()` will coerce a data frame to an `exposed_df` object.
#'
#' @param x an object. `x` must be a data frame when calling `as_exposed_df()`
#' @inheritParams expose
#'
#' @return For `is_exposed_df()`, a length-1 logical vector. For
#' `as_exposed_df()`, an `exposed_df` object.
#'
#' @export
is_exposed_df <- function(x) {
  "exposed_df" %in% class(x)
}

#' @rdname is_exposed_df
#' @export
as_exposed_df <- function(x, end_date, start_date = as.Date("1900-01-01"),
                          target_status = NULL, cal_expo = FALSE,
                          expo_length = "year") {

  if(!is.data.frame(x)) {
    rlang::abort("`x` must be a data frame.")
  }

  structure(x, class = c("exposed_df", class(x)),
            target_status = target_status,
            exposure_type = glue::glue("{if(cal_expo) 'calendar' else 'policy'}_{expo_length}"),
            start_date = start_date,
            end_date = end_date)

}
