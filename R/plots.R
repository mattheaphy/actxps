#' Plot experience study results
#'
#' @param object An object of class `exp_df` usually created by the
#' function `exp_stats()`.
#' @param ... Faceting variables passed to `facet_wrap()`.
#' @param mapping Aesthetic mapping passed to `ggplot()`.
#' @param scales The `scales` argument passed to `facet_wrap()`.
#' @param geoms Type of geometry. If "points" is passed, the plot will
#' display lines and points. If "bars", the plot will display bars.
#'
#' @details If no aesthetic map is supplied, the plot will use the first
#' grouping variable in `object` on the x axis and `q_obs` on the y
#' axis. In addition, the second grouping variable in `object` will be
#' used for color and fill.
#'
#' If no faceting variables are supplied, the plot will use all grouping
#' variables 3+ as facets passed into `facet_wrap()`.
#'
#' @return a `ggplot` object
#'
#' @export
autoplot.exp_df <- function(object, ..., mapping, scales = "fixed",
                            geoms = c("lines", "bars")) {

  .groups <- groups(object)
  if(length(.groups) == 0) {
    .groups <- list(rlang::parse_expr("All"))
    object[["All"]] <- ""
  }

  defaultNULL <- function(x) if (length(.groups) < x) NULL else .groups[[x]]

  geoms <- match.arg(geoms)

  # set up aesthetics
  if(missing(mapping)) {
    x <- .groups[[1]]
    color <- defaultNULL(2)
    fill <- defaultNULL(2)
    mapping <- ggplot2::aes(!!x, q_obs, color = !!color)
  }

  if(missing(...)) {
    facet <- .groups[-(1:2)]
    if (length(facet) == 0) facet <- NULL
  } else {
    facet <- rlang::enquos(...)
  }

  p <- ggplot2::ggplot(object, mapping) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1))

  if (geoms == "lines") {
    p <- p + ggplot2::geom_point() + ggplot2::geom_line()
  } else {
    p <- p + ggplot2::geom_col(position = "dodge")
  }


  if (is.null(facet)) return(p)
  p + ggplot2::facet_wrap(ggplot2::vars(!!!facet), scales = scales)

}
