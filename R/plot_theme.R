#' Plotting theme for vignettes
#'
#' This is an internal function used to set a plotting theme in vignettes
#' and articles.
#'
#' @return No return value. This function is called for its side effects.
#' @noRd
set_actxps_plot_theme <- function() {

  ggplot2::theme_set(ggplot2::theme_light() +
                       ggplot2::theme(
                         strip.background = ggplot2::element_rect(
                           fill = "#1367D4")
                       )
  )
  if (rlang::is_installed("thematic")) {
    thematic::thematic_on(
      qualitative = c("#1367D4", "#7515EB", "#EB15E4", "#1AC4F2",
                               "#1FF2C1", "#C6E531", "#FFA13D", "#FF7647"))
  }
}
