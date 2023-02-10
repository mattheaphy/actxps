#' Tabular experience study summary
#'
#' @description `autotable()` is a generic function used to create a table
#' from an object of a particular class. Tables are constructed using the
#' `gt` package.
#'
#' `autotable.exp_df()` is used to convert experience study results to a
#' presentation-friendly format.

#'
#' @param object An object of class `exp_df` usually created by the
#' function [exp_stats()].
#' @param fontsize Font size percentage multiplier.
#' @param decimals Number of decimals to display for percentages
#' @param colorful If `TRUE`, color will be added to the the observed
#' decrement rate and actual-to-expected columns.
#' @param color_q_obs Color palette used for the observed decrement rate.
#' @param color_ae_ Color palette used for actual-to-expected rates.
#' @param rename_cols An optional list consisting of key-value pairs. This
#' can be used to relabel columns on the output table. Names are column names
#' in `object` and values are new labels. See [gt::cols_label()] for
#' more information.
#' @param ... Additional arguments passed to [gt::gt()].
#'
#' @details
#'
#' See [paletteer::paletteer_d()]'s `palette` argument for usage of
#' the `color_q_obs` and `color_ae_` arguments.
#'
#' @return a `gt` object
#'
#' @importFrom rlang :=
#'
#' @export
autotable <- function(object, ...) {
  UseMethod("autotable")
}

#' @rdname autotable
#' @export
autotable.exp_df <- function(object, fontsize = 100, decimals = 1,
                             colorful = TRUE,
                             color_q_obs = "RColorBrewer::GnBu",
                             color_ae_ = "RColorBrewer::RdBu",
                             rename_cols = rlang::list2(...),
                             ...) {

  rlang::check_installed("RColorBrewer")

  expected <- attr(object, "expected")
  target_status <- attr(object, "target_status")
  wt <- attr(object, "wt")
  cred <- attr(object, "exp_params")$credibility

  tab <- object |>
    dplyr::select(-dplyr::starts_with(".weight")) |>
    gt::gt(...) |>
    gt::fmt_number(c(claims, exposure), decimals = 0) |>
    gt::fmt_percent(c(q_obs,
                      dplyr::starts_with("ae_"),
                      dplyr::starts_with("adj_"),
                      dplyr::any_of("credibility"),
                      expected),
                    decimals = decimals) |>
    gt::tab_options(table.font.size = gt::pct(fontsize),
                    row.striping.include_table_body = TRUE) |>
    gt::tab_style(list(gt::cell_text(weight = "bold")),
                  locations = gt::cells_column_labels()) |>
    gt::cols_label(q_obs = gt::md("*q<sup>obs</sup>*"),
                   claims = "Claims",
                   exposure = "Exposures") |>
    gt::cols_label(.list = rename_cols) |>
    gt::tab_header(title = "Experience Study Results",
                   subtitle = glue::glue("Target status{ifelse(length(target_status) > 1,'es','')}: {paste(target_status, collapse = ', ')}")) |>
    gt::tab_source_note(glue::glue("Study range: {as.character(attr(object, 'start_date'))} to {as.character(attr(object, 'end_date'))}"))

  if (length(wt) > 0) {
    tab <- tab |>
      gt::tab_source_note(glue::glue("Results weighted by `{wt}`") |> gt::md()) |>
      gt::cols_label(n_claims = "# Claims")
  } else {
    tab <- tab |> gt::cols_hide(n_claims)
  }


  for (i in expected) {
    tab <- tab |> span_expected(i, cred)
  }

  if (cred) {
    tab <- tab |>
      gt::cols_label(credibility = gt::md("*Z<sup>cred</sup>*"))
  }

  if (length(expected > 0)) {
    tab <- tab |>
      gt::tab_style(list(gt::cell_text(weight = "bold")),
                    locations = gt::cells_column_spanners())
  }
  if (colorful) {
    tab <- tab |>
      gt::data_color(
        columns = q_obs,
        colors = scales::col_numeric(
          palette = paletteer::paletteer_d(palette = color_q_obs) |>
            as.character(),
          domain = NULL
        )
      ) |>
      gt::data_color(
        columns = dplyr::starts_with("ae_"),
        colors = scales::col_numeric(
          palette = paletteer::paletteer_d(palette = color_ae_) |>
            as.character(),
          domain = NULL,
          reverse = TRUE
        )
      )
  }

  tab

}

span_expected <- function(tab, ex, cred) {

  force(ex)
  tab <- tab |>
    gt::tab_spanner(glue::glue("`{ex}`") |> gt::md(),
                    c(ex, paste0("ae_", ex),
                      if (cred) paste0("adj_", ex))) |>
    gt::cols_label(!!rlang::enquo(ex) := gt::md("*q<sup>exp</sup>*"),
                   !!rlang::sym(paste0("ae_", ex)) := gt::md("*A/E*"))

  if (!cred) return(tab)

  tab |> gt::cols_label(
    !!rlang::sym(paste0("adj_", ex)) := gt::md("*q<sup>adj</sup>*"))

}
