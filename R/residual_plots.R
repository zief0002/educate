#' Residual Plots for Linear Models
#'
#' @param model Logical indicating whether the model-level partitioning (TRUE) or term-level partitioning (FALSE; default) should be used
#' @param type Character string indicating the type of residual plot to return. The default is `type="b"` (both). You can also
#'             use `type="d"` (density) or `type="s"` (scatterplot).
#'
#' @return One or more plots of the standardized residuals. In the density plot, a confidence envelope of reasonable values 
#'         based on the normality assumption being true is produced along with the empirical density of the residuals. 
#'         In the scatterplot of the standardized residuals versus the fitted values, a confidence envelope of reasonable values
#'         based on the mean residual = 0 (linearity) is produced along with the loess smoother based on the empirical data.
#' @export
#'
#' @examples
#' lm.1 = lm(mpg ~ 1 + hp + wt, data = mtcars)
#' residual_plots(lm.1, type = "b")
#'
residual_plots = function(model, type = "b"){
  
  # Get residuals and fitted values
  aug_lm = broom::augment(model)
  
  # Create residual plot (normality)
  p1 = ggplot(data = aug_lm, aes(x =.std.resid)) +
    stat_density_confidence(model = "normal") +
    geom_density() +
    theme_light() +
    xlab("Standardized residuals") +
    ylab("Probability density")
  
  # Create residual plot (linearity, homoskedasticity)
  p2 = ggplot(data = aug_lm, aes(x =.fitted, y = .std.resid)) +
    geom_smooth(method = "lm", linewidth = 0) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +
    theme_light() +
    xlab("Fitted values") +
    ylab("Standardized residuals")
  
  # Return plots
  if(type == "d"){ 
    return(p1)
  } else if(type == "s") {
    return(p2)
  } else if (type == "b") {
    return(p1 | p2)
  } else{
    message(strwrap(paste0(crayon::blue("Use "), crayon::yellow('type="d" '), 
                           crayon::blue("for density plot of the standardized residuals, "),
                           crayon::yellow('type="s" '), 
                           crayon::blue("for a scatterplot of the standardized residuals versus the fitted values, or "),
                           crayon::yellow('type="b" '), crayon::blue("for both plots.")), 
                    prefix = " ", initial = ""))
    }
  
}