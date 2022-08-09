#' Create exposure records in a `recipes` step
#'
#' `step_expose` creates a *specification* of a recipe step that will convert
#' a data frame of census-level records to exposure-level records.
#'
#' Policy year exposures are calculated as a default. To switch to calendar
#' exposures or another exposure length, use pass the appropriate arguments to
#' the `options` parameter.
#'
#' @inheritParams recipes::step_center
#' @inheritParams expose
#' @param options A named list of additional arguments passed to `expose()`.
#'
#' @examples
#'
#' \dontrun{}
#' expo_rec <- recipes::recipe(status ~ ., toy_census) |>
#'   step_expose(end_date = "2022-12-31", target_status = "Surrender",
#'               options = list(expo_length = "month")) |>
#'   prep()
#'
#' recipes::juice()
#'
#' @seealso
#' [expose()]
#'
#' @export
step_expose <- function(recipe,
                        ...,
                        role = NA,
                        trained = FALSE,
                        end_date,
                        start_date = as.Date("1900-01-01"),
                        target_status = NULL,
                        options = list(
                          cal_expo = FALSE,
                          expo_length = "year"),
                        skip = TRUE,
                        id = recipes::rand_id("expose")) {

  if (length(rlang::enquos(...)) > 0) {
    rlang::warn("No variable selectors are used for step_expose.")
  }

  if (!"cal_expo" %in% names(options)) options$cal_expo <- FALSE
  if (!"expo_length" %in% names(options)) options$expo_length <- "year"

  recipes::add_step(
    recipe,
    step_expose_new(
      terms = rlang::enquos(...),
      role = role,
      trained = trained,
      end_date = end_date,
      start_date = start_date,
      target_status = target_status,
      options = options,
      skip = skip,
      id = id
    )
  )

}

step_expose_new <- function(terms, role, trained, end_date, start_date,
                            target_status, options, skip, id) {

  recipes::step(
    subclass = "expose",
    terms = terms,
    role = role,
    trained = trained,
    end_date = end_date,
    start_date = start_date,
    target_status = target_status,
    options = options,
    skip = skip,
    id = id
  )

}

#' @export
prep.step_expose <- function(x, training, info = NULL, ...) {

  step_expose_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    end_date = x$end_date,
    start_date = x$start_date,
    target_status = x$target_status,
    options = x$options,
    skip = x$skip,
    id = x$id
  )

}

#' @export
bake.step_expose <- function(object, new_data, ...) {

  rlang::exec("expose",
              new_data,
              object$end_date,
              object$start_date,
              object$target_status,
              !!!object$options) |>
    dplyr::select(-pol_num)

}

#' @export
print.step_expose <- function(x, width = max(20, options()$width - 30), ...) {

  title <- glue::glue("Exposed data based on {if (x$options$cal_expo) 'calendar' else 'policy'} {x$options$expo_length}s{if(!is.null(x$target_status)) paste(' for target status', paste(x$target_status, collapse = ', ')) else ''} ")

  recipes::print_step(
    untr_obj = NULL,
    tr_obj = NULL,
    trained = x$trained,
    title = title,
    width = width
  )

  invisible(x)

}
