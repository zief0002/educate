#' Confidence Envelope for Residuals Helper Function
#'
#' This function creates a normal theory based confidence envelope for the model residuals.
#'
#' @param model Name of fitted lm model.
#' @param type Type of residual, either raw or standardized. The default is `type="standardized"`
#' @param fill Fill color for the confidence envelope. The default is `fill="skyblue"`
#' @usage NULL
#'
#' @export



StatResidualConfidence <- ggplot2::ggproto("StatResidualConfidence", ggplot2::Stat,

 # Required aesthetics
 required_aes = c("x"),
 default_aes = ggplot2::aes(ymin = stat(lower_limit), ymax = stat(upper_limit), group = stat(group)),


# Computations
compute_group = function(data, scales,
                         model,
                         type = "standardized",
                         fill = "skyblue",
                         alpha = 0.3, ...) {


  # Compute SEs for raw and standardized residuals
  yhat = predict(model)



  if(type == "raw") {
    y = resid(model)
    resid.se = predict(model, se.fit = TRUE)$se.fit
    upper_limit <-  2 * resid.se
    lower_limit <- -2 * resid.se
  } else{
    y = rstandard(model)
    std.resid.se = 1 / (resid(model) / rstandard(model)) * predict(model, se.fit = TRUE)$se.fit
    upper_limit <- 2 * std.resid.se
    lower_limit <- -2 * std.resid.se
  }



  # Output data for plotting
  data.frame(
    x = yhat,
    y = y,
    lower_limit = lower_limit,
    upper_limit = upper_limit,
    group = 1,
    alpha = alpha
  )


  }
)


