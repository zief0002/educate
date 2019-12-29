#' Density with Confidence Envelope
#'
#' This function creates a normal theory based confidence envelope for the empirical density.
#' Transparency is used to indicate the level of uncertainty.
#'
#' @inheritParams ggplot2::stat_identity
#' @param h A normal kernel function is used and `h` is its standard deviation. If this parameter is
#'     omitted, a normal optimal smoothing parameter is used.
#' @param fill Fill color for the confidence envelope. The default is `color="skyblue"`
#' @param fade Should the confidence enevelope have more transparency for values farther from the
#'      empirical density? The default is `fade=FALSE`.
#' @param model The model to bootstrap from. The default is `model="none"` which bootstraps from the data.
#'     Using `model="normal"` draws repeated samples from a normal distribution with parameters based on the
#'     ML estimates from the data.
#'
#' @importFrom dplyr %>%
#'
#' @export



StatDensityConfidence <- ggplot2::ggproto("StatDensityConfidence", ggplot2::Stat,

  # Required aesthetics
  required_aes = c("x"),
  default_aes = ggplot2::aes(ymin = stat(lower_limit), ymax = stat(upper_limit), group = stat(group)),


  # Computations
  compute_group = function(data, scales, h = NULL, na.rm = TRUE,
                           fill = "skyblue", fade = FALSE,
                           model = "none", ...) {

    if (length(unique(data$x)) < 2) {
      # Not enough data to perform fit
      return(new_data_frame())
    }

    # Compute smoothing parameter
    if(is.null(h)){
      h = (4 / (3*length(data$x))) ^ (1/5) * sd(data$x)
    }

    # Compute points at which to evaluate density
    #eval_range = c(min(data$x) - diff(range(data$x)) / 4, max(data$x) + diff(range(data$x)) / 4)
    eval_range = c(min(data$x), max(data$x))
    eval_points = seq(from = eval_range[1], to = eval_range[2], length.out = 100)

    # Compute SD for kernel
    SD = sqrt(h^2 + var(data$x))


    if(model == "normal") {

      y = dnorm(eval_points, mean(data$x), SD)

    } else{

      y = density(data$x, bw = h, n = 100, from = eval_range[1], to = eval_range[2])$y

    }

    # Compute confidence limits
    se <- sqrt(dnorm(0, sd = sqrt(2)*h) / (4 * length(data$x) * h))
    upper <- sqrt(y) + 2 * se
    lower <- pmax(sqrt(y) - 2 * se, 0)
    upper <- upper^2
    lower <- lower^2
    est_se <- rep(se, length(eval_points))
    est_upper <- upper
    est_lower <- lower

    if(fade) {

      my_data = data.frame(
        x = eval_points,
        y = y,
        ll_10 = est_lower,
        ul_10 = est_upper
      ) %>%
        mutate(
          ll_1 = y - (ul_10 - y) / 10,
          ul_1 = y + (ul_10 - y) / 10,
          ll_2 = y - (ul_10 - y) / 10 * 2,
          ul_2 = y + (ul_10 - y) / 10 * 2,
          ll_3 = y - (ul_10 - y) / 10 * 3,
          ul_3 = y + (ul_10 - y) / 10 * 3,
          ll_4 = y - (ul_10 - y) / 10 * 4,
          ul_4 = y + (ul_10 - y) / 10 * 4,
          ll_5 = y - (ul_10 - y) / 10 * 5,
          ul_5 = y + (ul_10 - y) / 10 * 5,
          ll_6 = y - (ul_10 - y) / 10 * 6,
          ul_6 = y + (ul_10 - y) / 10 * 6,
          ll_7 = y - (ul_10 - y) / 10 * 7,
          ul_7 = y + (ul_10 - y) / 10 * 7,
          ll_8 = y - (ul_10 - y) / 10 * 8,
          ul_8 = y + (ul_10 - y) / 10 * 8,
          ll_9 = y - (ul_10 - y) / 10 * 9,
          ul_9 = y + (ul_10 - y) / 10 * 9
        ) %>%
        tidyr::pivot_longer(
          cols = ll_10:ul_9,
          names_to = c(".value", "group"),
          names_sep = "_"
        ) %>%
        rename(lower_limit = ll, upper_limit = ul)

      my_alpha = seq(from = 0.5, to = 0.1, length.out = 10)
      my_data$alpha = I(my_alpha[as.numeric(my_data$group)])


    } else{

      # Output data for plotting
      my_data = data.frame(
        x = eval_points,
        y = y,
        lower_limit = est_lower,
        upper_limit = est_upper,
        group = 1,
        alpha = 0.5
      )

    }

    my_data

    }

  )



#' Stat Method for Density with Confidence Envelope
#'
#' This function creates a confidence enevelope for the empirical density. The idea came from
#' functions written by Bowman and Azzalini.
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @inheritParams StatDensityConfidence
#'
#' @section Aesthetics:
#' These stat uses  `geom_ribbon()` so support the
#' same aesthetics: `alpha`, `colour`, `linetype` and
#' `size`.
#'
#' @importFrom dplyr %>%
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


