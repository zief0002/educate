#' Plot z-Distribution and Shaded p-Value
#'
#' @param x prop.test() or prop_test() object
#' @param shade_p_value Logical value indicating whether to shade the area under the curve
#' corresponding to the p-value. Default is TRUE.
#'
#' @return Plot of the z-distribution. If shade_p_value=TRUE, the area under the curve
#' corresponding to the p-value will also be shaded.
#' @importFrom ggplot2 ggplot aes stat_function theme_bw xlab ylab stat_function ggtitle geom_segment
#' @export
#'
#' @examples
#' heads <- rbinom(1, size = 100, prob = .5)
#' my_z = prop.test(heads, 100)
#' plot_z_dist(my_z)
#'
plot_z_dist = function(x, shade_p_value = TRUE) {
  if (
    x$method %in%
      c(
        "1-sample proportions test with continuity correction",
        "1-sample proportions test without continuity correction"
      )
  ) {
    if (x$estimate < x$null.value) {
      z_value = -1 * sqrt(x$statistic[[1]])
    } else {
      z_value = sqrt(x$statistic[[1]])
    }
  } else {
    if (x$estimate[[1]] < x$estimate[[2]]) {
      z_value = -1 * sqrt(x$statistic[[1]])
    } else {
      z_value = sqrt(x$statistic[[1]])
    }
  }

  abs_z_value = abs(z_value)
  tail_limit = ceiling(abs_z_value) #Get how many SE to go in each direction
  p = if (x$p.value < .001) {
    formatC(x$p.value, format = "e", digits = 2)
  } else {
    round(x$p.value, 3)
  }

  # Reset tail limits to get better z-distribution if |z| < 5
  if (tail_limit < 5) {
    tail_limit = 5
  }

  # Initial plot
  p1 = ggplot(data = data.frame(x = c(-tail_limit, tail_limit)), aes(x = x)) +
    stat_function(
      fun = dnorm,
      geom = "line"
    ) +
    theme_bw() +
    xlab("z") +
    ylab("Density") +
    ggtitle(paste0("z = ", round(z_value, 2), ", p = ", p))

  if (shade_p_value) {
    # Different shading/vertical lines depending on alternative hyp.

    if (x$alternative == "less") {
      # Shade to the left
      p1 = p1 +
        stat_function(
          fun = dnorm,
          geom = "line",
          xlim = c(-tail_limit, z_value),
          color = "#ff2d21",
        ) +
        stat_function(
          fun = dnorm,
          geom = "area",
          xlim = c(-tail_limit, z_value),
          fill = "#ff2d21",
          alpha = 0.3
        ) +
        geom_segment(
          x = z_value,
          xend = z_value,
          y = 0,
          yend = dnorm(0),
          color = "#ff2d21"
        )
    } else if (x$alternative == "greater") {
      # Shade to the right
      p1 = p1 +
        stat_function(
          fun = dnorm,
          geom = "line",
          xlim = c(z_value, tail_limit),
          color = "#ff2d21"
        ) +
        stat_function(
          fun = dnorm,
          geom = "area",
          xlim = c(z_value, tail_limit),
          fill = "#ff2d21",
          alpha = 0.3
        ) +
        geom_segment(
          x = z_value,
          xend = z_value,
          y = 0,
          yend = dnorm(0),
          color = "#ff2d21"
        )
    } else {
      # Shade to the left and right
      p1 = p1 +
        stat_function(
          fun = dnorm,
          geom = "line",
          xlim = c(-tail_limit, -abs_z_value),
          color = "#ff2d21",
        ) +
        stat_function(
          fun = dnorm,
          geom = "line",
          xlim = c(abs_z_value, tail_limit),
          color = "#ff2d21"
        ) +
        stat_function(
          fun = dnorm,
          geom = "area",
          xlim = c(-tail_limit, -abs_z_value),
          fill = "#ff2d21",
          alpha = 0.3
        ) +
        stat_function(
          fun = dnorm,
          geom = "area",
          xlim = c(abs_z_value, tail_limit),
          fill = "#ff2d21",
          alpha = 0.3
        ) +
        geom_segment(
          x = -abs_z_value,
          xend = -abs_z_value,
          y = 0,
          yend = dnorm(0),
          color = "#ff2d21"
        ) +
        geom_segment(
          x = abs_z_value,
          xend = abs_z_value,
          y = 0,
          yend = dnorm(0),
          color = "#ff2d21"
        )
    }
  }

  return(p1)
}
