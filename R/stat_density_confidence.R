#' Normal Theory Based Confidence Envelope for Density
#'
#' This function creates a normal theory based confidence envelope for the empirical density.
#' Transparency is used to indicate the level of uncertainty.
#'
#' @inheritParams ggplot2::stat_identity
#' @param h A normal kernel function is used and `h` is its standard deviation. If this parameter is
#'     omitted, a normal optimal smoothing parameter is used.
#' @param fill Fill color for the confidence envelope. The default is `color="skyblue"`
#' @param model The model to draw the confidence envelope for. The default is `model="none"` which creates the confidence envelope
#'              from the data. Using `model="normal"` creates the confidence envelope based on a normal distribution.
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
      h = (4 / (3*length(data$x))) ^ (1/5) * sd(data$x, na.rm = TRUE)
    }

    # Compute points at which to evaluate density
    #eval_range = c(min(data$x) - diff(range(data$x)) / 4, max(data$x) + diff(range(data$x)) / 4)
    eval_range = c(min(data$x, na.rm = TRUE), max(data$x, na.rm = TRUE))
    eval_points = seq(from = eval_range[1], to = eval_range[2], length.out = 100)

    # Compute SD for kernel
    SD = sqrt(h^2 + var(data$x, na.rm = TRUE))


    if(model == "normal") {

      y = dnorm(eval_points, mean(data$x, na.rm = TRUE), SD)

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

    # Output data for plotting
    data.frame(
      x = eval_points,
      y = y,
      lower_limit = est_lower,
      upper_limit = est_upper,
      group = 1,
      alpha = 0.5
    )


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


