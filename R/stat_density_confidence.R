
#' Normal Theory Based Confidence Envelope for Density
#'
#' This function creates a normal theory based confidence envelope for the empirical density. The idea came from
#' functions written by Bowman and Azzalini.
#'
#' @format NULL
#' @inheritParams StatDensityConfidence
#'
#' @section Aesthetics:
#' These stat uses  `geom_ribbon()` so support the same aesthetics: `alpha`, `colour`, `fill`,
#'     `linetype`, `group`, and `size`.
#'
#' @export

stat_density_confidence <- function(mapping = NULL, data = NULL, geom = "ribbon",
                                   position = "identity", na.rm = TRUE,
                                   inherit.aes = TRUE, h = NULL, fill = "skyblue",
                                   fade = FALSE, model = "none", ...) {

    ggplot2::layer(
      stat = StatDensityConfidence,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        h = h,
        fill = fill,
        model = model,
        fade = fade,
        #alpha = alpha,
        ...)
  )
}



