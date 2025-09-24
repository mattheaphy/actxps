#' Plot experience study results
#'
#' @param object An object of class `exp_df` created by the
#' function [exp_stats()] or an object of class `trx_df` created by the function
#' [trx_stats()].
#' @param ... Faceting variables passed to [ggplot2::facet_wrap()].
#' @param x An unquoted column name in `object` or expression to use as the `x`
#' variable.
#' @param y An unquoted column name in `object` or expression to use as the
#' `y` variable. If unspecified, `y` will default to the observed termination
#' rate (`q_obs`) for `exp_df` objects and the observed utilization rate
#' (`trx_util`) for `trx_df` objects.
#' @param color An unquoted column name in `object` or expression to use as the
#' `color` and `fill` variables.
#' @param mapping Aesthetic mapping passed to [ggplot2::ggplot()]. NOTE: If
#' `mapping` is supplied, the `x`, `y`, and `color` arguments will be ignored.
#' @param second_axis Logical. If `TRUE`, the variable specified by
#' `second_y` (default = exposure) is plotted on a second
#' y-axis using an area geometry.
#' @param second_y An unquoted column name in `object` to use as the `y`
#' variable on the second y-axis. If unspecified, this will default to
#' `exposure`.
#' @param scales The `scales` argument passed to [ggplot2::facet_wrap()].
#' @param geoms Type of geometry. If "lines" is passed, the plot will
#' display lines and points. If "bars", the plot will display bars. If "points",
#' the plot will display points only.
#' @param y_labels Label function passed to [ggplot2::scale_y_continuous()].
#' @param second_y_labels Same as `y_labels`, but for the second y-axis.
#' @param y_log10 If `TRUE`, the y-axes are plotted on a log-10 scale.
#' @param conf_int_bars If `TRUE`, confidence interval error bars are included
#' in the plot. For `exp_df` objects, this option is available for termination
#' rates and actual-to-expected ratios. For `trx_df` objects, this option is
#' available for utilization rates and any `pct_of` columns.
#'
#' @details If no aesthetic map is supplied, the plot will use the first
#' grouping variable in `object` on the x axis and `q_obs` on the y
#' axis. In addition, the second grouping variable in `object` will be
#' used for color and fill.
#'
#' If no faceting variables are supplied, the plot will use grouping
#' variables 3 and up as facets. These variables are passed into
#' [ggplot2::facet_wrap()]. Specific to `trx_df` objects, transaction
#' type (`trx_type`) will also be added as a faceting variable.
#'
#' @return a `ggplot` object
#'
#' @examples
#'
#' study_py <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
#'
#' study_py <- study_py |>
#'   add_transactions(withdrawals)
#'
#' exp_res <- study_py |> group_by(pol_yr) |> exp_stats()
#' autoplot(exp_res)
#'
#' trx_res <- study_py |> group_by(pol_yr) |> trx_stats()
#' autoplot(trx_res)
#'
#' @seealso [plot_termination_rates()], [plot_actual_to_expected()]
#'
#' @name autoplot_exp
#' @rdname autoplot_exp
#' @export
autoplot.exp_df <- function(
  object,
  ...,
  x = NULL,
  y = NULL,
  color = NULL,
  mapping,
  second_axis = FALSE,
  second_y = NULL,
  scales = "fixed",
  geoms = c("lines", "bars", "points"),
  y_labels = scales::label_percent(accuracy = 0.1),
  second_y_labels = scales::label_comma(
    accuracy = 1
  ),
  y_log10 = FALSE,
  conf_int_bars = FALSE
) {
  y <- rlang::enexpr(y)
  y <- if (is.null(y)) rlang::expr(q_obs) else y

  second_y <- rlang::enexpr(second_y)
  second_y <- if (is.null(second_y)) {
    rlang::expr(exposure)
  } else {
    second_y
  }

  plot_experience(
    object,
    rlang::enexpr(x),
    y,
    rlang::enexpr(color),
    mapping,
    second_axis,
    second_y,
    scales,
    geoms,
    y_labels,
    second_y_labels,
    rlang::enquos(...),
    y_log10,
    conf_int_bars
  )
}

#' @rdname autoplot_exp
#' @export
autoplot.trx_df <- function(
  object,
  ...,
  x = NULL,
  y = NULL,
  color = NULL,
  mapping,
  second_axis = FALSE,
  second_y = NULL,
  scales = "fixed",
  geoms = c("lines", "bars", "points"),
  y_labels = scales::label_percent(accuracy = 0.1),
  second_y_labels = scales::label_comma(
    accuracy = 1
  ),
  y_log10 = FALSE,
  conf_int_bars = FALSE
) {
  y <- rlang::enexpr(y)
  y <- if (is.null(y)) rlang::expr(trx_util) else y

  second_y <- rlang::enexpr(second_y)
  second_y <- if (is.null(second_y)) {
    rlang::expr(exposure)
  } else {
    second_y
  }

  facets <- rlang::enquos(...)
  if (length(facets) == 0) {
    facets <- c(rlang::expr(trx_type), groups(object)[-(1:2)])
  }

  plot_experience(
    object,
    rlang::enexpr(x),
    y,
    rlang::enexpr(color),
    mapping,
    second_axis,
    second_y,
    scales,
    geoms,
    y_labels,
    second_y_labels,
    facets,
    y_log10,
    conf_int_bars
  )
}

plot_experience <- function(
  object,
  x = NULL,
  y = NULL,
  color = NULL,
  mapping,
  second_axis = FALSE,
  second_y = NULL,
  scales = "fixed",
  geoms = c("lines", "bars", "points"),
  y_labels = scales::label_percent(accuracy = 0.1),
  second_y_labels = scales::label_comma(accuracy = 1),
  facets,
  y_log10,
  conf_int_bars = FALSE
) {
  .groups <- groups(object)
  if (length(.groups) == 0) {
    .groups <- list(rlang::parse_expr("All"))
    object[["All"]] <- ""
  }

  auto_aes <- function(.var, default) {
    if (length(.var) == 0) {
      if (length(.groups) < default) NULL else .groups[[default]]
    } else {
      .var
    }
  }

  geoms <- match.arg(geoms)

  # set up aesthetics
  if (missing(mapping)) {
    x <- auto_aes(x, 1)
    color <- auto_aes(color, 2)
    if (!is.null(color) && as.character(color) == ".no_color") {
      color <- NULL
    }
    mapping <- ggplot2::aes(
      !!x,
      !!y,
      color = !!color,
      fill = !!color,
      group = !!color
    )
  }

  if (length(facets) == 0) {
    facets <- .groups[-(1:2)]
    if (length(facets) == 0) facets <- NULL
  }

  y_trans <- if (y_log10) {
    "log10"
  } else {
    "identity"
  }

  p <- ggplot2::ggplot(object, mapping)

  if (second_axis) {
    adj <- max(object |> dplyr::pull(!!second_y), na.rm = TRUE) /
      max(object |> dplyr::pull(!!mapping$y), na.rm = TRUE)
    # important - re-assign color. If `mapping` was passed, it's currently NULL
    color <- p$mapping$colour
    if (y_log10) {
      geom.fun <- function(...) {
        ggplot2::geom_ribbon(
          ggplot2::aes(
            ymax = !!second_y,
            group = if (is.null(color)) 1 else !!color
          ),
          ...,
          ymin = -Inf
        )
      }
    } else {
      geom.fun <- function(...) {
        ggplot2::geom_area(
          ggplot2::aes(
            y = !!second_y,
            group = if (is.null(color)) 1 else !!color
          ),
          ...
        )
      }
    }
    p <- p +
      geom.fun(
        data = object |>
          mutate(
            !!second_y := !!second_y /
              adj
          ),
        alpha = 0.2,
        position = "identity"
      ) +
      ggplot2::scale_y_continuous(
        sec.axis = ggplot2::sec_axis(
          ~ . * adj,
          labels = second_y_labels,
          name = as.character(second_y)
        ),
        labels = y_labels,
        trans = y_trans
      )
  } else {
    p <- p +
      ggplot2::scale_y_continuous(
        labels = y_labels,
        trans = y_trans
      )
  }

  if (geoms == "lines") {
    p <- p + ggplot2::geom_point() + ggplot2::geom_line()
  } else if (geoms == "points") {
    p <- p + ggplot2::geom_point()
  } else {
    p <- p + ggplot2::geom_col(position = "dodge")
  }

  if (conf_int_bars) {
    conf_int <- attr(object, "xp_params")$conf_int
    y_chr <- rlang::as_name(p$mapping$y)
    y_min_max <- paste0(y_chr, c("_upper", "_lower"))

    if (is.null(conf_int) || !conf_int) {
      conf_int_warning()
    } else {
      if (all(y_min_max %in% names(object))) {
        y_min_max <- rlang::syms(y_min_max)
        p <- p +
          ggplot2::geom_errorbar(ggplot2::aes(
            ymin = !!y_min_max[[1]],
            ymax = !!y_min_max[[2]]
          ))
      } else {
        rlang::warn(c(
          "Confidence intervals are not available for the selected y-variable."
        ))
      }
    }
  }

  if (is.null(facets)) {
    return(p)
  }
  p + ggplot2::facet_wrap(ggplot2::vars(!!!facets), scales = scales)
}

# This internal function provides a common warning that is used by multiple
# functions.
conf_int_warning <- function() {
  rlang::warn(c(
    "*" = "`object` has no confidence intervals.",
    "i" = "Pass `conf_int = TRUE` to `exp_stats()` or `trx_stats()` to calculate confidence intervals."
  ))
}
