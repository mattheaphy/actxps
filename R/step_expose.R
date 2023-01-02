#' Create exposure records in a `recipes` step
#'
#' `step_expose()` creates a *specification* of a recipe step that will convert
#' a data frame of census-level records to exposure-level records.
#'
#' Policy year exposures are calculated as a default. To switch to calendar
#' exposures or another exposure length, use pass the appropriate arguments to
#' the `options` parameter.
#'
#' Policy numbers are dropped as a default whenever the recipe is baked. This
#' is done to prevent unintentional errors when the model formula includes
#' all variables (`y ~ .`). If policy numbers are required for any reason
#' (mixed effect models, identification, etc.), set `drop_pol_num` to `FALSE`.
#'
#' @inheritParams recipes::step_center
#' @inheritParams expose
#' @param options A named list of additional arguments passed to [expose()].
#' @param drop_pol_num Whether the `pol_num` column produced by [expose()]
#' should be dropped. Defaults to `TRUE`.
#'
#' @return An updated version of `recipe` with the new expose step added to the
#' sequence of any existing operations. For the `tidy` method, a `tibble` with
#' the columns `exposure_type`, `target_status`, `start_date`, and `end_date`.
#'
#' @examples
#'
#' expo_rec <- recipes::recipe(status ~ ., toy_census) |>
#'   step_expose(end_date = "2022-12-31", target_status = "Surrender",
#'               options = list(expo_length = "month")) |>
#'   prep()
#'
#' recipes::juice(expo_rec)
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
                        drop_pol_num = TRUE,
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
      drop_pol_num = drop_pol_num,
      skip = skip,
      id = id
    )
  )

}

step_expose_new <- function(terms, role, trained, end_date, start_date,
                            target_status, options, drop_pol_num, skip, id) {

  recipes::step(
    subclass = "expose",
    terms = terms,
    role = role,
    trained = trained,
    end_date = end_date,
    start_date = start_date,
    target_status = target_status,
    options = options,
    drop_pol_num = drop_pol_num,
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
    drop_pol_num = x$drop_pol_num,
    skip = x$skip,
    id = x$id
  )

}

#' @export
bake.step_expose <- function(object, new_data, ...) {

  new_data <- rlang::exec("expose",
              new_data,
              object$end_date,
              object$start_date,
              object$target_status,
              !!!object$options)

  if (object$drop_pol_num) {
    new_data |> dplyr::select(-pol_num)
  } else {
    new_data
  }

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

#' @export
tidy.step_expose <- function(x, ...) {
  tibble::tibble(
    exposure_type = paste(
      if (x$options$cal_expo) "calendar" else "policy",
      x$options$expo_length,
      sep = "_"
    ),
    target_status = x$target_status,
    start_date = x$start_date,
    end_date = x$end_date
  )
}
