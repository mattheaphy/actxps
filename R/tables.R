#' Tabular experience study summary
#'
#' @description `autotable()` is a generic function used to create a table
#' from an object of a particular class. Tables are constructed using the
#' `gt` package.
#'
#' `autotable.exp_df()` is used to convert experience study results to a
#' presentation-friendly format.
#'
#' `autotable.trx_df()` is used to convert transaction study results to a
#' presentation-friendly format.
#'
#' @param object An object of class `exp_df` usually created by the
#' function [exp_stats()] or an object of class `trx_df` created by the
#' [trx_stats()] function.
#' @param fontsize Font size percentage multiplier.
#' @param decimals Number of decimals to display for percentages
#' @param colorful If `TRUE`, color will be added to the the observed
#' decrement rate and actual-to-expected columns for termination studies, and
#' the utilization rate and "percentage of" columns for transaction studies.
#' @param color_q_obs Color palette used for the observed decrement rate.
#' @param color_ae_ Color palette used for actual-to-expected rates.
#' @param color_util Color palette used for utilization rates.
#' @param color_pct_of Color palette used for "percentage of" columns.
#' @param rename_cols An optional list consisting of key-value pairs. This
#' can be used to relabel columns on the output table. This parameter is most
#' useful for renaming grouping variables that will appear under their original
#' variable names if left unchanged. See [gt::cols_label()] for more
#' information.
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
    select(-dplyr::starts_with(".weight")) |>
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

    domain_ae <- if (length(expected > 0)) {
      object |>
        select(dplyr::starts_with('ae_')) |>
        range(na.rm = TRUE)
    }

    tab <- tab |>
      gt::data_color(
        columns = q_obs,
        fn = scales::col_numeric(
          palette = paletteer::paletteer_d(palette = color_q_obs) |>
            as.character(),
          domain = NULL
        )
      ) |>
      gt::data_color(
        columns = dplyr::starts_with("ae_"),
        fn = scales::col_numeric(
          palette = paletteer::paletteer_d(palette = color_ae_) |>
            as.character(),
          domain = domain_ae,
          reverse = TRUE
        )
      )
  }

  tab

}

#' @rdname autotable
#' @export
autotable.trx_df <- function(object, fontsize = 100, decimals = 1,
                             colorful = TRUE,
                             color_util = "RColorBrewer::GnBu",
                             color_pct_of = "RColorBrewer::RdBu",
                             rename_cols = rlang::list2(...),
                             ...) {

  rlang::check_installed("RColorBrewer")

  percent_of <- attr(object, "percent_of")
  trx_types <- attr(object, "trx_types")

  # remove unnecessary columns
  if (!is.null(percent_of)) {
    object <- object |>
      select(-dplyr::all_of(percent_of),
             -dplyr::all_of(paste0(percent_of, "_w_trx")))
  }

  tab <- object |>
    select(-exposure) |>
    arrange(trx_type) |>
    gt::gt(groupname_col = "trx_type") |>
    gt::fmt_number(c(trx_n, trx_amt, trx_flag, avg_trx, avg_all),
                   decimals = 0) |>
    gt::fmt_number(trx_freq, decimals = 1) |>
    gt::fmt_percent(c(trx_util, dplyr::starts_with("pct_of_")),
                    decimals = decimals) |>
    gt::sub_missing() |>
    gt::tab_options(table.font.size = gt::pct(fontsize),
                    row.striping.include_table_body = TRUE) |>
    gt::tab_style(list(gt::cell_text(weight = "bold")),
                  locations = gt::cells_column_labels()) |>
    gt::tab_spanner(gt::md("**Counts**"), c("trx_n", "trx_flag")) |>
    gt::tab_spanner(gt::md("**Averages**"), c("avg_trx", "avg_all")) |>
    gt::cols_label(trx_n = "Total",
                   trx_flag = "Periods",
                   trx_amt = "Amount",
                   avg_trx = gt::md("*w/ trx*"),
                   avg_all = gt::md("*all*"),
                   trx_freq = "Frequency",
                   trx_util = "Utilization") |>
    gt::cols_label(.list = rename_cols) |>
    gt::tab_header(title = "Transaction Study Results",
                   subtitle = glue::glue("Transaction type{ifelse(length(trx_types) > 1,'s','')}: {paste(trx_types, collapse = ', ')}")) |>
    gt::tab_source_note(glue::glue("Study range: {as.character(attr(object, 'start_date'))} to {as.character(attr(object, 'end_date'))}"))

  for (i in percent_of) {
    tab <- tab |> span_percent_of(i)
  }

  if (colorful) {

    domain_pct <- if (!is.null(percent_of)) {
      object |>
        select(dplyr::starts_with('pct_of')) |>
        range(na.rm = TRUE)
    }

    tab <- tab |>
      gt::data_color(
        columns = trx_util,
        fn = scales::col_numeric(
          palette = paletteer::paletteer_d(palette = color_util) |>
            as.character(),
          domain = NULL
        )
      ) |>
      gt::data_color(
        columns = dplyr::starts_with("pct_of"),
        fn = scales::col_numeric(
          palette = paletteer::paletteer_d(palette = color_pct_of) |>
            as.character(),
          domain = domain_pct,
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

span_percent_of <- function(tab, pct_of) {

  pct_names <- paste0("pct_of_", pct_of, c("_w_trx", "_all"))

  tab <- tab |>
    gt::tab_spanner(glue::glue("**% of {pct_of}**") |> gt::md(),
                    pct_names) |>
    gt::cols_label(!!rlang::sym(pct_names[[1]]) := gt::md("*w/ trx*"),
                   !!rlang::sym(pct_names[[2]]) := gt::md("*all*"))

}
