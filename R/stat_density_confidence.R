#' Normal Theory Based Confidence Envelope for Density
#'
#' This function creates a normal theory based confidence envelope for the empirical density. The idea came from
#' functions written by Bowman and Azzalini.
#'
#' @format NULL
#' @usage NULL
#' @param mapping Default list of aesthetic mappings to use for plot.
#' @param data The data to be displayed in this layer.
#' @param h A normal kernel function is used and `h` is its standard deviation. If this parameter is
#'     omitted, a normal optimal smoothing parameter is used.
#' @param fill Fill color for the confidence envelope. The default is `fill="skyblue"`
#' @param model The model to draw the confidence envelope for. The default is `model="none"` which creates the
#'     confidence envelope from the data. Using `model="normal"` creates the confidence envelope based on a normal distribution.
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @section Aesthetics:
#' These stat uses  `geom_ribbon()` so support the same aesthetics: `alpha`, `colour`, `fill`,
#'     `linetype`, `group`, and `size`.
#' @examples
#' ggplot(mtcars, aes(x = mpg)) +
#'   stat_density_confidence(model = "normal") +
#'   geom_density()
#' @export

stat_density_confidence <- function(
  mapping = NULL,
  data = NULL,
  h = NULL,
  fill = "skyblue",
  model = "none",
  ...
) {
  ggplot2::layer(
    stat = StatDensityConfidence,
    data = data,
    mapping = mapping,
    geom = "ribbon",
    position = "identity",
    inherit.aes = TRUE,
    params = list(
      na.rm = TRUE,
      h = h,
      fill = fill,
      model = model,
      #alpha = alpha,
      ...
    )
  )
}
