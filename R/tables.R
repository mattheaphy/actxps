#' Tabular experience study summary
#'
#' @description \code{autotable} is a generic function used to create a table
#' from an object of a particular class. Tables are constructed using the
#' \code{gt} package.
#'
#' \code{autotable.exp_df} is used to convert experience study results to a
#' presentation-friendly format.

#'
#' @param object An object of class \code{exp_df} usually created by the
#' function \code{exp_stats()}.
#' @param fontsize Font size percentage multiplier.
#' @param ... Not used
#'
#'
#' @return a \code{gt} object
#'
#' @export
autotable <- function(object, ...) {
  UseMethod("autotable")
}

#' @rdname autotable
#' @export
autotable.exp_df <- function(object, fontsize = 100, ...) {

  object |>
    gt::gt() |>
    gt::fmt_number(c(claims, exposure), decimals = 0) |>
    gt::fmt_percent(q_obs, decimals = 1) |>
    gt::fmt_percent(dplyr::starts_with("ae_"), decimals = 1) |>
    gt::tab_options(table.font.size = gt::pct(fontsize),
                    row.striping.include_table_body = TRUE) |>
    gt::tab_style(list(gt::cell_text(weight = "bold")),
                  locations = gt::cells_column_labels())

}
