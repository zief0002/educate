
#' Confidence Envelope for Residuals
#'
#' This function creates a normal theory based confidence envelope for the model residuals.
#'
#' @format NULL
#' @usage NULL
#' @inheritParams StatResidualConfidence
#'
#' @section Aesthetics:
#' These stat uses  `geom_ribbon()` so support the same aesthetics: `alpha`, `colour`, `fill`,
#'     `linetype`, `group`, and `size`.
#'
#' @export

stat_residual_confidence <- function(mapping = NULL, data = NULL, geom = "ribbon",
                                   position = "identity", na.rm = TRUE,
                                   inherit.aes = TRUE, fill = "skyblue",
                                   model = "none", ...) {

    ggplot2::layer(
      stat = StatResidualConfidence,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        fill = fill,
        model = model,
        #alpha = alpha,
        ...)
  )
}


