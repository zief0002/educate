#' Bootstrapped Confidence Envelope for Density
#'
#' This function creates a confidence envelope for the empirical density by bootstrapping from the data.
#' Transparency is used to indicate the level of uncertainty.
#'
#' @format NULL
#' @usage NULL
#' @param mapping Default list of aesthetic mappings to use for plot.
#' @param data The data to be displayed in this layer.
#' @param k Number of bootstrapped smoothers. The default is 1000.
#' @param color Color for the bootstrapped densities that make up the confidence envelope. The
#'     default is `color="#1D91C0"`
#' @param alpha Transparency level for the paths that make up the bootstrapped fitted lines. This may
#'    need to be adjusted if the argument `k=` is changed. The default value is `alpha=0.03`.
#' @param model The model to bootstrap from. The default is `model="none"` which bootstraps from the data.
#'     Using `model="normal"` draws repeated samples from a normal distribution with parameters based on the data.
#'
#' @section Aesthetics:
#' These stat uses  `geom_line()` so support the same aesthetics: `alpha`, `colour`, `group`,
#'     `linetype` and `size`.
#' @export
stat_density_watercolor <- function(
  data = NULL,
  mapping = NULL,
  k = 1000,
  color = "#1D91C0",
  alpha = 0.03,
  model = "none",
  ...
) {
  ggplot2::layer(
    stat = StatDensityWatercolor,
    data = data,
    mapping = mapping,
    geom = "line",
    position = "identity",
    inherit.aes = TRUE,
    params = list(
      na.rm = TRUE,
      k = k,
      alpha = alpha,
      color = color,
      model = model,
      ...
    )
  )
}
