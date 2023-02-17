#' Plot experience study results
#'
#' @param object An object of class `exp_df` usually created by the
#' function [exp_stats()].
#' @param ... Faceting variables passed to [ggplot2::facet_wrap()].
#' @param x An unquoted column name in `object` or expression to use as the `x`
#' variable.
#' @param y An unquoted column name in `object` or expression to use as the
#' `y` variable. If unspecified, `y` will default to the observed termination
#' (`q_obs`) for `exp_df` objects and the observed utilization rate
#' (`trx_util`) for `trx_df` objects.
#' @param color An unquoted column name in `object` or expression to use as the
#' `color` and `fill` variables.
#' @param mapping Aesthetic mapping passed to [ggplot2::ggplot()]. NOTE: If
#' `mapping` is supplied, the `x`, `y`, and `color` arguments will be ignored.
#' @param scales The `scales` argument passed to [ggplot2::facet_wrap()].
#' @param geoms Type of geometry. If "points" is passed, the plot will
#' display lines and points. If "bars", the plot will display bars.
#' @param y_labels Label function passed to [ggplot2::scale_y_continuous()].
#'
#' @details If no aesthetic map is supplied, the plot will use the first
#' grouping variable in `object` on the x axis and `q_obs` on the y
#' axis. In addition, the second grouping variable in `object` will be
#' used for color and fill.
#'
#' If no faceting variables are supplied, the plot will use grouping
#' variables 3 and up as facets. These variables are passed into
#' [ggplot2::facet_wrap()].
#'
#' @return a `ggplot` object
#'
#' @export
autoplot.exp_df <- function(object, ..., x = NULL, y = NULL, color = NULL,
                            mapping, scales = "fixed",
                            geoms = c("lines", "bars"),
                            y_labels = scales::label_percent(accuracy = 0.1)) {

  y <- rlang::enexpr(y)
  y <- if (is.null(y)) rlang::expr(q_obs) else y

  plot_experience(object, rlang::enexpr(x), y,
                  rlang::enexpr(color), mapping, scales, geoms,
                  y_labels, rlang::enquos(...))
}

#' @export
autoplot.trx_df <- function(object, ..., x = NULL, y = NULL, color = NULL,
                            mapping, scales = "fixed",
                            geoms = c("lines", "bars"),
                            y_labels = scales::label_percent(accuracy = 0.1)) {

  y <- rlang::enexpr(y)
  y <- if (is.null(y)) rlang::expr(trx_util) else y

  plot_experience(object, rlang::enexpr(x), y,
                  rlang::enexpr(color), mapping, scales, geoms,
                  y_labels, rlang::enquos(...))
}

plot_experience <- function(
    object, x = NULL, y = NULL, color = NULL,
    mapping, scales = "fixed",
    geoms = c("lines", "bars"),
    y_labels = scales::label_percent(accuracy = 0.1),
    facets) {

  .groups <- groups(object)
  if(length(.groups) == 0) {
    .groups <- list(rlang::parse_expr("All"))
    object[["All"]] <- ""
  }

  auto_aes <- function(.var, default) {
    if(length(.var) == 0) {
      if (length(.groups) < default) NULL else .groups[[default]]
    } else {
      .var
    }
  }

  geoms <- match.arg(geoms)

  # set up aesthetics
  if(missing(mapping)) {
    x <- auto_aes(x, 1)
    color <- auto_aes(color, 2)
    mapping <- ggplot2::aes(!!x, !!y, color = !!color,
                            fill = !!color, group = !!color)
  }

  if(length(facets) == 0) {
    facets <- .groups[-(1:2)]
    if (length(facets) == 0) facets <- NULL
  }

  p <- ggplot2::ggplot(object, mapping) +
    ggplot2::scale_y_continuous(labels = y_labels)

  if (geoms == "lines") {
    p <- p + ggplot2::geom_point() + ggplot2::geom_line()
  } else {
    p <- p + ggplot2::geom_col(position = "dodge")
  }


  if (is.null(facets)) return(p)
  p + ggplot2::facet_wrap(ggplot2::vars(!!!facets), scales = scales)

}
