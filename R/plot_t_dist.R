#' Plot t-Distribution and Shaded p-Value
#'
#' @param x t.test() or t_test() object
#' @param shade_p_value Logical value indicating whether to shade the area under the curve
#' corresponding to the p-value. Default is TRUE.
#'
#' @return Plot of the t-distribution. If shade_p_value=TRUE, the area under the curve
#' corresponding to the p-value will also be shaded.
#' @importFrom ggplot2 ggplot aes stat_function theme_bw xlab ylab stat_function ggtitle geom_segment
#' @export
#'
#' @examples
#' my_t = t.test(cars$speed, mu = 16, alternative = "less")
#' plot_t_dist(my_t)
#'
plot_t_dist = function(x, shade_p_value = TRUE) {
  df = x$parameter[[1]] #Get df
  abs_t_value = abs(x$statistic[[1]])
  tail_limit = ceiling(abs_t_value) #Get how many SE to go in each direction
  p = if (x$p.value < .001) {
    formatC(x$p.value, format = "e", digits = 2)
  } else {
    round(x$p.value, 3)
  }

  # Reset tail limits to get better t-distribution if |t| < 5
  if (tail_limit < 5) {
    tail_limit = 5
  }

  # Initial plot
  p1 = ggplot(data = data.frame(x = c(-tail_limit, tail_limit)), aes(x = x)) +
    stat_function(
      fun = dt,
      args = list(df = df),
      geom = "line"
    ) +
    theme_bw() +
    xlab("t") +
    ylab("Density") +
    ggtitle(paste0("t(", df, ") = ", round(x$statistic, 2), ", p = ", p))

  if (shade_p_value) {
    # Different shading/vertical lines depending on alternative hyp.

    if (x$alternative == "less") {
      # Shade to the left
      p1 = p1 +
        stat_function(
          fun = dt,
          args = list(df = df),
          geom = "line",
          xlim = c(-tail_limit, x$statistic),
          color = "#ff2d21",
        ) +
        stat_function(
          fun = dt,
          args = list(df = df),
          geom = "area",
          xlim = c(-tail_limit, x$statistic),
          fill = "#ff2d21",
          alpha = 0.3
        ) +
        geom_segment(
          x = x$statistic,
          xend = x$statistic,
          y = 0,
          yend = dt(0, df = df),
          color = "#ff2d21"
        )
    } else if (x$alternative == "greater") {
      # Shade to the right
      p1 = p1 +
        stat_function(
          fun = dt,
          args = list(df = df),
          geom = "line",
          xlim = c(x$statistic, tail_limit),
          color = "#ff2d21"
        ) +
        stat_function(
          fun = dt,
          args = list(df = df),
          geom = "area",
          xlim = c(x$statistic, tail_limit),
          fill = "#ff2d21",
          alpha = 0.3
        ) +
        geom_segment(
          x = x$statistic,
          xend = x$statistic,
          y = 0,
          yend = dt(0, df = df),
          color = "#ff2d21"
        )
    } else {
      # Shade to the left and right
      p1 = p1 +
        stat_function(
          fun = dt,
          args = list(df = df),
          geom = "line",
          xlim = c(-tail_limit, -abs_t_value),
          color = "#ff2d21",
        ) +
        stat_function(
          fun = dt,
          args = list(df = df),
          geom = "line",
          xlim = c(abs_t_value, tail_limit),
          color = "#ff2d21"
        ) +
        stat_function(
          fun = dt,
          args = list(df = df),
          geom = "area",
          xlim = c(-tail_limit, -abs_t_value),
          fill = "#ff2d21",
          alpha = 0.3
        ) +
        stat_function(
          fun = dt,
          args = list(df = df),
          geom = "area",
          xlim = c(abs_t_value, tail_limit),
          fill = "#ff2d21",
          alpha = 0.3
        ) +
        geom_segment(
          x = -abs_t_value,
          xend = -abs_t_value,
          y = 0,
          yend = dt(0, df = df),
          color = "#ff2d21"
        ) +
        geom_segment(
          x = abs_t_value,
          xend = abs_t_value,
          y = 0,
          yend = dt(0, df = df),
          color = "#ff2d21"
        )
    }
  }

  return(p1)
}
