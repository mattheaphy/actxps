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
#' termination rate and actual-to-expected columns for termination studies, and
#' the utilization rate and "percentage of" columns for transaction studies.
#' @param color_q_obs Color palette used for the observed termination rate.
#' @param color_ae_ Color palette used for actual-to-expected rates.
#' @param color_util Color palette used for utilization rates.
#' @param color_pct_of Color palette used for "percentage of" columns.
#' @param rename_cols An optional list consisting of key-value pairs. This
#' can be used to relabel columns on the output table. This parameter is most
#' useful for renaming grouping variables that will appear under their original
#' variable names if left unchanged. See [gt::cols_label()] for more
#' information.
#' @param conf_int_show If `TRUE` confidence intervals will be displayed
#' assuming they are available on `object`.
#' @param ... Additional arguments passed to [gt::gt()].
#'
#' @details
#'
#' The `color_q_obs`, `color_ae_`, `color_util`, and `color_pct_of` arguments
#' must be strings referencing a discrete color palette available in the
#' `paletteer` package. Palettes must be in the form "package::palette".
#' For a full list of available palettes, see [paletteer::palettes_d_names].
#'
#' @return a `gt` object
#'
#' @examples
#'
#' if (interactive()) {
#'   study_py <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
#'   expected_table <- c(seq(0.005, 0.03, length.out = 10), 0.2, 0.15, rep(0.05, 3))
#'
#'   study_py <- study_py |>
#'     mutate(expected_1 = expected_table[pol_yr],
#'            expected_2 = ifelse(inc_guar, 0.015, 0.03)) |>
#'     add_transactions(withdrawals) |>
#'     left_join(account_vals, by = c("pol_num", "pol_date_yr"))
#'
#'   exp_res <- study_py |> group_by(pol_yr) |>
#'     exp_stats(expected = c("expected_1", "expected_2"), credibility = TRUE,
#'               conf_int = TRUE)
#'   autotable(exp_res)
#'
#'   trx_res <- study_py |> group_by(pol_yr) |>
#'     trx_stats(percent_of = "av_anniv", conf_int = TRUE)
#'   autotable(trx_res)
#' }
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
                             conf_int_show = FALSE,
                             ...) {

  rlang::check_installed("RColorBrewer")

  expected <- attr(object, "expected")
  target_status <- attr(object, "target_status")
  wt <- attr(object, "wt")
  cred <- attr(object, "xp_params")$credibility
  conf_int <- attr(object, "xp_params")$conf_int


  if (conf_int_show && !conf_int) {
    conf_int_warning()
  } else if (conf_int && !conf_int_show) {
    object <- object |>
      select(-dplyr::ends_with("_lower"),
             -dplyr::ends_with("_upper"))
  }
  conf_int <- conf_int_show && conf_int

  tab <- object |>
    select(-dplyr::starts_with(".weight")) |>
    gt::gt(...) |>
    gt::fmt_number(c(claims, exposure), decimals = 0) |>
    gt::fmt_percent(c(q_obs,
                      dplyr::ends_with("_lower"),
                      dplyr::ends_with("_upper"),
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
    gt::tab_source_note(glue::glue("Study range: {as.character(attr(object, 'start_date'))} to {as.character(attr(object, 'end_date'))}")) |>
    gt::cols_hide(n_exposure)

  if (length(wt) > 0) {
    tab <- tab |>
      gt::tab_source_note(glue::glue("Results weighted by `{wt}`") |> gt::md()) |>
      gt::cols_label(n_claims = "# Claims")
  } else {
    tab <- tab |> gt::cols_hide(n_claims)
  }

  # merge confidence intervals into a single range column
  if (conf_int) {
    tab <- tab |>
      gt::cols_merge_range(q_obs_lower, q_obs_upper) |>
      gt::cols_label(q_obs_lower = gt::md("*q<sup>obs</sup> CI*"))
    for (i in expected) {
      tab <- tab |>
        gt::cols_merge_range(paste0("ae_", i, "_lower"),
                             paste0("ae_", i, "_upper"))
      if (cred) {
        tab <- tab |>
          gt::cols_merge_range(paste0("adj_", i, "_lower"),
                               paste0("adj_", i, "_upper"))
      }
    }
  }

  for (i in expected) {
    tab <- tab |> span_expected(i, cred, conf_int)
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

    ae_cols <- paste0("ae_", expected)
    domain_ae <- if (length(expected > 0)) {
      object |>
        select(dplyr::any_of(ae_cols)) |>
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
        columns = dplyr::any_of(ae_cols),
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
                             conf_int_show = FALSE,
                             ...) {

  rlang::check_installed("RColorBrewer")

  percent_of <- attr(object, "percent_of")
  trx_types <- attr(object, "trx_types")
  conf_int <- attr(object, "xp_params")$conf_int

  if (conf_int_show && !conf_int) {
    conf_int_warning()
  } else if (conf_int && !conf_int_show) {
    object <- object |>
      select(-dplyr::ends_with("_lower"),
             -dplyr::ends_with("_upper"))
  }
  conf_int <- conf_int_show && conf_int

  # remove unnecessary columns
  if (!is.null(percent_of)) {
    object <- object |>
      select(-dplyr::all_of(percent_of),
             -dplyr::all_of(paste0(percent_of, "_w_trx")))
  }

  tab <- object |>
    select(-exposure, -dplyr::any_of("trx_amt_sq")) |>
    arrange(trx_type) |>
    gt::gt(groupname_col = "trx_type") |>
    gt::fmt_number(c(trx_n, trx_amt, trx_flag, avg_trx, avg_all),
                   decimals = 0) |>
    gt::fmt_number(trx_freq, decimals = 1) |>
    gt::fmt_percent(c(dplyr::starts_with("trx_util"),
                      dplyr::starts_with("pct_of_")),
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

  # merge confidence intervals into a single range column
  if (conf_int) {
    tab <- tab |>
      gt::cols_merge_range(trx_util_lower, trx_util_upper) |>
      gt::tab_spanner(gt::md("**Utilization**"), c(trx_util, trx_util_lower)) |>
      gt::cols_label(trx_util = gt::md("*Rate*"),
                     trx_util_lower = gt::md("*CI*"))
    for (i in percent_of) {
      tab <- tab |>
        gt::cols_merge_range(paste0("pct_of_", i, "_w_trx_lower"),
                             paste0("pct_of_", i, "_w_trx_upper")) |>
        gt::cols_merge_range(paste0("pct_of_", i, "_all_lower"),
                             paste0("pct_of_", i, "_all_upper"))
    }
  }

  for (i in percent_of) {
    tab <- tab |> span_percent_of(i, conf_int)
  }

  if (colorful) {

    pct_of_cols <- c(paste0("pct_of_", percent_of, "_w_trx"),
                     paste0("pct_of_", percent_of, "_all"))
    domain_pct <- if (!is.null(percent_of)) {
      object |>
        select(dplyr::any_of(pct_of_cols)) |>
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
        columns = dplyr::any_of(pct_of_cols),
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


span_expected <- function(tab, ex, cred, conf_int) {

  force(ex)

  ae <- paste0("ae_", ex)
  ae_ci <- paste0(ae, "_lower")
  adj <- paste0("adj_", ex)
  adj_ci <- paste0("adj_", ex, "_lower")

  tab <- tab |>
    gt::tab_spanner(glue::glue("`{ex}`") |> gt::md(),
                    c(ex, ae,
                      if (cred) adj,
                      if (conf_int) c(
                        ae_ci,
                        if(cred) adj_ci
                      ))) |>
    gt::cols_label(!!rlang::enquo(ex) := gt::md("*q<sup>exp</sup>*"),
                   !!rlang::sym(ae) := gt::md("*A/E*"))

  if (cred) {
    tab <- tab |> gt::cols_label(
      !!rlang::sym(adj) := gt::md("*q<sup>adj</sup>*"))
  }

  if (conf_int) {
    tab <- tab |> gt::cols_label(
      !!rlang::sym(ae_ci) :=
        gt::md("*A/E CI*")) |>
      gt::cols_move(ae_ci, after = ae)
    if (cred) {
      tab <- tab |> gt::cols_label(
        !!rlang::sym(adj_ci) :=
          gt::md("*q<sup>adj</sup> CI*"))
    }
  }

  tab

}

span_percent_of <- function(tab, pct_of, conf_int) {

  pct_names <- paste0("pct_of_", pct_of, c("_w_trx", "_all"))
  if (conf_int) {
    pct_names <- c(pct_names,
                   paste0(pct_names, "_lower"))
  }

  tab <- tab |>
    gt::tab_spanner(glue::glue("**% of {pct_of}**") |> gt::md(),
                    pct_names) |>
    gt::cols_label(!!rlang::sym(pct_names[[1]]) := gt::md("*w/ trx*"),
                   !!rlang::sym(pct_names[[2]]) := gt::md("*all*"))

  if (conf_int) {
    tab <- tab |>
      gt::cols_label(!!rlang::sym(pct_names[[3]]) := gt::md("*w/ trx CI*"),
                     !!rlang::sym(pct_names[[4]]) := gt::md("*all CI*")) |>
      gt::cols_move(pct_names[[3]], pct_names[[1]])
  }

  tab

}
