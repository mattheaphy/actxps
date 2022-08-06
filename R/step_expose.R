#' Create exposure records in a `recipes` step
#'
#' `step_expose` creates a *specification* of a recipe step that will convert
#' a data frame of census-level records to exposure-level records.
#'
#'
#' @inheritParams recipes::step_center
#' @inheritParams expose
#' @param col_names A named list of column names passed to the `col_pol_num`,
#' `col_status`, `col_issue_date`, and `col_term_date` arguments of `expose()`.
#'
#' @examples
#'
#' 1
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
                        cal_expo = FALSE,
                        expo_length = "year",
                        col_names = list(
                          col_pol_num = "pol_num",
                          col_status = "status",
                          col_issue_date = "issue_date",
                          col_term_date = "term_date"),
                        default_status,
                        skip = TRUE,
                        id = recipes::rand_id("expose")) {

  recipes::add_step(
    recipe,
    step_expose_new(
      terms = rlang::enquos(...),
      role = role,
      trained = trained,
      end_date = end_date,
      start_date = start_date,
      target_status = target_status,
      cal_expo = cal_expo,
      expo_length = expo_length,
      col_names = col_names,
      default_status = default_status,
      skip = skip,
      id = id
    )
  )

}

step_expose_new <- function(terms, role, trained, end_date, start_date,
                            target_status, cal_expo, expo_length, col_names,
                            default_status, skip, id) {

  recipes::step(
    subclass = "expose",
    terms = terms,
    role = role,
    trained = trained,
    end_date = end_date,
    start_date = start_date,
    target_status = target_status,
    cal_expo = cal_expo,
    expo_length = expo_length,
    col_names = col_names,
    default_status = default_status,
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
    cal_expo = x$cal_expo,
    expo_length = x$expo_length,
    col_names = x$col_names,
    default_status = x$default_status,
    skip = x$skip,
    id = x$id
  )

}

#' @export
bake.step_expose <- function(object, new_data, ...) {

  expose(new_data,
         object$end_date,
         object$start_date,
         object$target_status,
         object$cal_expo,
         object$expo_length,
         object$col_names$col_pol_num,
         object$col_names$col_status,
         object$col_names$col_issue_date,
         object$col_names$col_term_date,
         object$default_status) |>
    dplyr::select(-pol_num)

}
