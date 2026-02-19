#' Bootstrapped Confidence Envelope for Density
#'
#' This function creates a confidence envelope for the empirical density by bootstrapping from the data.
#' Transparency is used to indicate the level of uncertainty.
#'
#' @format NULL
#' @usage NULL
#' @inheritParams StatDensityWatercolor
#'
#' @section Aesthetics:
#' These stat uses  `geom_line()` so support the same aesthetics: `alpha`, `colour`, `group`,
#'     `linetype` and `size`.
#' @export
stat_density_watercolor <- function(
  mapping = NULL,
  data = NULL,
  geom = "line",
  position = "identity",
  na.rm = TRUE,
  inherit.aes = TRUE,
  k = 1000,
  alpha = 0.03,
  color = "#1D91C0",
  model = "none",
  ...
) {
  ggplot2::layer(
    stat = StatDensityWatercolor,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      k = k,
      alpha = alpha,
      color = color,
      model = model,
      ...
    )
  )
}
