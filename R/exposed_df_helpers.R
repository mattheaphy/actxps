#' Exposed data frame helper functions
#'
#' Test for and coerce to the `exposed_df` class.
#'
#' These are behind-the-scenes functions that will generally not be called
#' by users.
#'
#' @param x an object. `x` must be a data frame when calling `as_exposed_df`
#' @inheritParams expose
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
