#' Plot experience study results
#'
#' @param object An object of class \code{exp_df} usually created by the
#' function \code{exp_stats}.
#' @param ... Not currently used.
#'
#' @export
autoplot.exp_df <- function(object, ...) {

  .groups <- attr(object, "groups")

  # set up aesthetics
  x <- .groups[[1]]
  defaultNULL <- function(x) if (length(.groups) < x) NULL else .groups[[x]]
  color <- defaultNULL(2)
  fill <- defaultNULL(2)
  facet <- defaultNULL(3)

  p <- ggplot2::ggplot(object, ggplot2::aes(!!x, q_obs, color = !!color)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1))

  if (is.null(facet)) return(p)
  p + ggplot2::facet_wrap(ggplot2::vars(!!facet))

}
