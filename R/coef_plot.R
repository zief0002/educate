#' Visually Weighted Coefficient Plot
#'
#' This function creates a visually weighted coefficient plot
#'
#' @param x lm object
#' @param labels Vector of coefficient labels
#' @param intercept Logical indicating whether the intercept should be displayed
#' @return A ggplot object giving the coefficient plot
#' @export
coef_plot <- function(x, labels = names(coef(x))[start:k], intercept = FALSE, ...) {

  coe = summary(x)$coefficients[ , 1] # extract coefficients
  cse = summary(x)$coefficients[ , 2] # standard errors
  k = length(coe)                     # Number of coefficients
  start = 2

  if(isTRUE(intercept)) start = 1


  #return(start)

  list_length = k - start + 1

  # Create empty list to store random results
  my_list = vector("list", list_length)

  # Draw and store random coefficients
  for(i in start:k) {
    my_list[[i]] <- data.frame(
      estimate = rnorm(mean = coe[i], sd = cse[i], n = 1000),
      term =  names(coef(x))[i]
    )

    my_list[[i]] = dplyr::mutate(my_list[[i]],
        dens_color = dplyr::case_when(
          estimate >= coe[i] - 0.25 * cse[i] & estimate <= coe[i] + 0.25 * cse[i] ~ "#2C7FB8",

          estimate  < coe[i] + 1.00 * cse[i] & estimate  > coe[i] + 0.25 * cse[i] ~ "#41B6C4",
          estimate  > coe[i] - 1.00 * cse[i] & estimate  < coe[i] - 0.25 * cse[i] ~ "#41B6C4",

          estimate  < coe[i] + 2.00 * cse[i] & estimate  > coe[i] + 1.00 * cse[i] ~ "#A1DAB4",
          estimate  > coe[i] - 2.00 * cse[i] & estimate  < coe[i] - 1.00 * cse[i] ~ "#A1DAB4",

          TRUE ~ "#FFFFCC"
        )
      )
  }

  # Store results in dataframe
  df = do.call("rbind", my_list)

  # Get empirical estimates for coefficients
  emp_est = data.frame(estimate = coe[start:k], term = names(coef(x))[start:k])

  # Create plot
  p = ggplot2::ggplot(data = df, ggplot2::aes(x = estimate, y = term, color = I(dens_color), fill = I(dens_color))) +
    ggplot2::geom_point(alpha = 0.1, shape = 15) +
    ggplot2::geom_point(data = emp_est, shape = 21, color = "#253494", fill = "#253494", size = 2) +
    ggplot2::guides(color = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    ) +
    ggplot2::scale_y_discrete(
      name = "Coefficients",
      labels = labels) +
    ggplot2::xlab("Estimate")

  return(p)
}

