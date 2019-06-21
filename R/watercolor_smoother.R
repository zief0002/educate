#' Watercolor Smoother
#'
#' This function creates a visually regression smoother, also called a watercolor plot. The initial idea
#' came from Solomon Hsiang.
#'
#' @param data Dataframe
#' @param x A column name in the dataframe. This should be unquoted.
#' @param y A column name in the dataframe. This should be unquoted.
#' @param k Number of bootstrapped smoothers. The default is 1000.
#' @param method Fitting function for the bootstrapped fitted lines. The default is `method="loess"`.
#'   The underlying syntax uses `stat_smooth()` so any method that works with that function should work.
#' @param color Color for the bootstrapped fitted lines that make up the confidence envelope. The
#'     default is `color="#1D91C0"`
#' @param alpha Transparency level for the paths that make up the bootstrapped fitted lines. This may
#'    need to be adjusted if the argument `k=` is changed. The default value is `alpha=0.03`.
#' @param emp_line_color Color of the fitted line for the empirical data.
#'     The default is `emp_line_color="black"`.
#' @param emp_line_size Size of the fitted line for the empirical data. The default value is
#'   `emp_line_size=0.5`.
#' @param ...: Further parameters passed to the fitting function, in the case of loess, for example,
#'             "span = .9", or "family = 'symmetric'"
#'
#' @return A ggplot object giving the plot of the bootstrapped fitted lines and the fitted line for
#'   the empirical data
#'
#' @importFrom dplyr %>%
#' @export


watercolor_smoother <- function(data, x, y, k = 1000, method = "loess",
                          color = "#1D91C0", alpha = 0.03,
                          emp_line_color = "black", emp_line_size = 0.5,
                          ...) {


  X = eval(substitute(x), data) # Get variable inputted for x
  Y = eval(substitute(y), data) # Get variable inputted for y

  # Tests
  if(!is.numeric(X)) return("x is non-numeric") # Check that input is numeric
  if(!is.numeric(Y)) return("y is non-numeric")
  if(length(X) == 0) return("x has no data")    # Check that input is not empty
  if(length(Y) == 0) return("y has no data")

  # if (bw == TRUE) {
  #   palette = colorRampPalette(c("#EEEEEE", "#999999", "#333333"), bias = 2)(20)
  # }

  message("Computing boostrapped smoothers ...")
  flush.console()

  # Bootstrap data
  boot.regression = data.frame(bs = 1:k) %>%
    dplyr::group_by(bs) %>%
    dplyr::mutate(
      data = list(data %>% dplyr::sample_frac(size = 1, replace = TRUE))
    ) %>%
    tidyr::unnest()


  # Select only variables being used and rename for use in ggplot
  boot.regression = boot.regression[ , c("bs", deparse(substitute(x)), deparse(substitute(y)))]
  names(boot.regression)[2] = "X"
  names(boot.regression)[3] = "Y"


  # Plot of the bootstrapped loess lines
  p = ggplot2::ggplot(data = boot.regression, ggplot2::aes(x = X, y = Y, group = bs)) +
        ggplot2::stat_smooth(color = color, size = 0.5, alpha = alpha, se = FALSE,
                             method = method, geom = "line", ...) +
        ggplot2::stat_smooth(data = data, ggplot2::aes(x = eval(X), y = eval(Y)),
                             group = 1, color = emp_line_color, size = emp_line_size, se = FALSE,
                             method = method, geom = "line") +
        ggplot2::xlab(deparse(substitute(x))) +
        ggplot2::ylab(deparse(substitute(y))) +
        ggplot2::theme_classic()

  message("Building ggplot figure ...")
  flush.console()

  return(p)
}

#
#   obs_data = data.frame(X = sort(unique(data[ , deparse(X)])))
#
#
#   ##### LM
#   boot.regression = boot.regression %>%
#     #dplyr::group_by(bs) %>%
#     dplyr::do(boot_fit = lm(Y ~ X, data = .)) %>%
#     mutate(
#       X = list(obs_data),
#       fitted = list(predict(boot_fit, newdata = obs_data))
#     ) %>%
#     dplyr::select(bs, X, fitted) %>%
#     tidyr::unnest()
#
#   ##### LOESS
#   boot.regression = boot.regression %>%
#     dplyr::group_by(bs) %>%
#     mutate(
#       loess_fit = loess(Y ~ X, data = .),
#       X = list(obs_data)
#      # fitted = list(predict(boot_fit, newdata = obs_data))
#     ) %>%
#     dplyr::select(bs, X, fitted) %>%
#     tidyr::unnest()
#
#
#
#   ggplot2::ggplot(data = boot.regression, ggplot2::aes(x = X, y = fitted, group = bs)) +
#     ggplot2::geom_path(color = color, size = 0.5, alpha = alpha) +
#     ggplot2::xlab(deparse(X)) +
#     ggplot2::ylab(deparse(Y)) +
#     ggplot2::theme_classic()
#
#
#   boot.regression = boot.regression %>%
#     group_by(X) %>%
#     mutate(
#       dens.scaled = abs(fitted - median(fitted)),
#       dens.scaled2 = dens.scaled / max(dens.scaled)
#     ) %>%
#     ungroup()
#
#
#   ggplot2::ggplot(data = boot.regression, ggplot2::aes(x = X, y = fitted, group = bs)) +
#     ggplot2::geom_path(ggplot2::aes(color = dens.scaled2), size = 0.5) +
#     ggplot2::scale_color_gradientn("dens.scaled2", colors = rev(RColorBrewer::brewer.pal(9, "YlGnBu"))) +
#     ggplot2::scale_alpha_continuous(range = c(0.001, 1)) +
#     ggplot2::xlab(deparse(X)) +
#     ggplot2::ylab(deparse(Y)) +
#     ggplot2::theme_classic() +
#     ggplot2::guides(color = FALSE, alpha = FALSE) +
#     ggplot2::geom_smooth(data = data, ggplot2::aes(x = eval(X), y = eval(Y)), se = FALSE,
#                          color = "white", group = 1, method = "lm", size = 0.5)
#
#
#
#
#
#
