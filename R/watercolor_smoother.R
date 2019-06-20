#' Watercolor Smoother
#'
#' This function creates a visually regression smoother, also called a Watercolor plot. The initial idea
#' came from Solomon Hsiang. Felix Schonbrodt provided inital syntax based on Solomon's idea, and
#' with additional inputfrom many blog commenters. He licensed this with a FreeBSD License (see syntax).
#'
#' @param B Number of bootstrapped smoothers. The default is 1000.
#' @param shade Logical value indicating whether to plot the shaded confidence region. The default is
#'              TRUE.
#' @param shade.alpha Numercial value indicating the degree to which the confidence enevelope shading
#'                    should fade out at the edges. This reduces the alpha value. (0 = no alpha decrease,
#'                     0.1 = medium alpha decrease, 0.5 = strong alpha decrease)
#' @param spag Logical value indicating whether the bootstrapped spaghetti lines should be plotted.
#'             The default is FALSE.
#' @param spag.color Color of bootstrapped spaghetti lines.
#' @param mweight Logical value indicating whether the median smoother should be visually weighted. The
#'                default is TRUE.
#' @param show.lm Logical value indicating whether the linear regresison line should be plotted.
#'                The default is FALSE.
#' @param show.CI Logical value indicating whether the 95% limits on the parametric confidence
#'                envelope should be plotted. Default is FALSE.
#' @param show.median Logical value indicating whether the median smoother should be plotted. The default
#'                    is TRUE.
#' @param median.col Color of the median smoother.
#' @param show.points Logical value indicating whether to plot the observed data. The default
#'                    is FALSE.
#' @param shape Shape of points. The default is 21 (open circles). This is only relevant if show.points=TRUE.
#' @param method the fitting function for the spaghettis; default: loess
#' @param bw Logical value indicating whether to use a black-and-white palette. The default is FALSE.
#' @param slices Number of slices in x and y direction for the shaded region. Higher numbers make a
#'               smoother plot, but takes longer to draw. I would not go beyond 500.
#' @param palette Color palette for the shading in the confidence envelope. This defaults to the 9-color
#'                YlGnBu palette from \link{RColorBrewer}.
#' @param ylim Set y-limit to restrict the range of the watercoloring. The default is NULL.
#' @param quantize: Character string indicating the type of shading on the confidence envelope. The default
#'                  is "continuous", which uses a continuous color gradient across the entire range of
#'                  y. Setting this to "SD" we get three color regions for 1, 2, and 3 SD
#'                  (an idea of John Mashey)
#' @param add: Logical value indicating whether to return a new ggplot. The default is FALSE.
#'             If add = TRUE, only the elements are returned, which can be added to another ggplot
#'             (with the '+' operator)
#' @param ...: Further parameters passed to the fitting function, in the case of loess, for example,
#'             "span = .9", or "family = 'symmetric'"
#'
#' @return A ggplot object giving the watercolor plot
#'
#' @importFrom plyr .
#' @importFrom dplyr %>%
#' @export

# Copyright 2012 Felix Schönbrodt
# All rights reserved.
#
# FreeBSD License
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER `AS IS'' AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# The views and conclusions contained in the software and documentation
# are those of the authors and should not be interpreted as representing
# official policies, either expressed or implied, of the copyright
# holder.

# Version history:
# 0.1: original code
# 0.1.1: changed license to FreeBSD; re-established compability to ggplot2 (new version 0.9.2)

##




watercolor_smoother <- function(formula, data, title = "", B = 1000, shade = TRUE,
                                  shade.alpha = 0.1, spag = FALSE, spag.color = "darkblue",
                                  mweight = TRUE, show.lm = FALSE, show.median = TRUE,
                                  median.col = "white", show.points = FALSE, shape = 21,
                                  show.CI = FALSE, method = loess,
                                  bw = FALSE, slices = 200,
                                  palette = RColorBrewer::brewer.pal(9, "YlGnBu"),
                                  ylim = NULL, quantize = "continuous",  add = FALSE, ...) {


  IV = all.vars(formula)[2]
  DV = all.vars(formula)[1]
  data = as.data.frame(data)
  data = na.omit(data[order(data[, IV]), c(IV, DV)])

  if (bw == TRUE) {
    palette = colorRampPalette(c("#EEEEEE", "#999999", "#333333"), bias = 2)(20)
  }

  print("Computing boostrapped smoothers ...")
  newx = data.frame(seq(min(data[ , IV]), max(data[ , IV]), length = slices))
  colnames(newx) = IV
  l0.boot = matrix(NA, nrow = nrow(newx), ncol = B)

  l0 = method(formula, data)
  for (i in 1:B) {
    data2 = data[sample(nrow(data), replace = TRUE), ]
    data2 = data2[order(data2[ , IV]), ]
    if (class(l0) == "loess") {
      m1 = method(formula, data2,
                  control = loess.control(surface = "interpolate",
                                          statistics = "approximate",
                                          trace.hat = "approximate")
                  )
    } else {
      m1 = method(formula, data2)
    }
    l0.boot[ , i] = predict(m1, newdata = newx)
  }

  # compute median and CI limits of bootstrap

  CI.boot <- plyr::adply(l0.boot, 1, function(x) quantile(x, prob = c(.025, .5, .975, pnorm(c(-3, -2, -1, 0, 1, 2, 3))), na.rm = TRUE))[ , -1]
  colnames(CI.boot)[1:10] = c("LL", "M", "UL", paste0("SD", 1:7))
  CI.boot$x = newx[ , 1]
  CI.boot$width = CI.boot$UL - CI.boot$LL

  # scale the CI width to the range 0 to 1 and flip it (bigger numbers = narrower CI)
  CI.boot$w2 = (CI.boot$width - min(CI.boot$width))
  CI.boot$w3 = 1-(CI.boot$w2 / max(CI.boot$w2))


  # convert bootstrapped spaghettis to long format
  b2 <- reshape2::melt(l0.boot)
  b2$x = newx[ , 1]
  colnames(b2) = c("index", "B", "value", "x")

  # Construct ggplot
  # All plot elements are constructed as a list, so they can be added to an existing ggplot

  # if add == FALSE: provide the basic ggplot object
  p0 = ggplot2::ggplot(data, ggplot2::aes_string(x = IV, y = DV)) + ggplot2::theme_bw()

  # initialize elements with NULL (if they are defined, they are overwritten with something meaningful)
  gg.tiles  = NULL
  gg.poly   = NULL
  gg.spag   = NULL
  gg.median = NULL
  gg.CI1    = NULL
  gg.CI2    = NULL
  gg.lm     = NULL
  gg.points = NULL
  gg.title  = NULL

  if (shade == TRUE) {
    quantize = match.arg(quantize, c("continuous", "SD"))
    if (quantize == "continuous") {
      print("Computing density estimates for each vertical cut ...")
      flush.console()

      if (is.null(ylim)) {
        min_value = min(min(l0.boot, na.rm = TRUE), min(data[ , DV], na.rm = TRUE))
        max_value = max(max(l0.boot, na.rm = TRUE), max(data[ , DV], na.rm = TRUE))
        ylim = c(min_value, max_value)
      }

      # vertical cross-sectional density estimate
      d2 = plyr::ddply(b2[ , c("x", "value")], .(x), function(df) {
        res <- data.frame(density(df$value, na.rm = TRUE, n = slices,
                                  from = ylim[1], to = ylim[2])[c("x", "y")])
        #res = data.frame(density(df$value, na.rm = TRUE, n = slices)[c("x", "y")])
        colnames(res) = c("y", "dens")
        return(res)
      }, .progress = "text")

      maxdens = max(d2$dens)
      mindens = min(d2$dens)
      d2$dens.scaled = (d2$dens - mindens) / maxdens

      ## Tile approach
      d2$alpha.factor = d2$dens.scaled ^ shade.alpha
      gg.tiles =  list(ggplot2::geom_tile(data = d2, ggplot2::aes(x = x, y = y, fill = dens.scaled,
                                                                  alpha = alpha.factor)),
                       ggplot2::scale_fill_gradientn("dens.scaled", colours = palette),
                       ggplot2::scale_alpha_continuous(range = c(0.001, 1)))
    }
    if (quantize == "SD") {
      ## Polygon approach

      SDs = reshape2::melt(CI.boot[ , c("x", paste0("SD", 1:7))], id.vars="x")
      count = 0
      d3 = data.frame()
      col = c(1, 2, 3, 3, 2, 1)
      for (i in 1:6) {
        seg1 = SDs[SDs$variable == paste0("SD", i), ]
        seg2 = SDs[SDs$variable == paste0("SD", i+1), ]
        seg = rbind(seg1, seg2[nrow(seg2):1, ])
        seg$group = count
        seg$col = col[i]
        count = count + 1
        d3 = rbind(d3, seg)
      }

      gg.poly =  list(ggplot2::geom_polygon(data = d3, ggplot2::aes(x = x, y = value, color = NULL,
                                                           fill = col, group = group)),
                      ggplot2::scale_fill_gradientn("dens.scaled", colours = palette,
                                                    values = seq(-1, 3, 1)))
    }
  }

  print("Build ggplot figure ...")
  flush.console()


  if (spag == TRUE) {
    gg.spag =  ggplot2::geom_path(data = b2, ggplot2::aes(x = x, y = value, group = B),
                                  size = 0.7, alpha = 10/B, color = spag.color)
  }

  if (show.median == TRUE) {
    if (mweight == TRUE) {
      gg.median = ggplot2::geom_path(data = CI.boot, ggplot2::aes(x = x, y = M, alpha = w3^3),
                                      size = 0.6, linejoin = "mitre", color = median.col)
    } else {
      gg.median = ggplot2::geom_path(data = CI.boot, ggplot2::aes(x = x, y = M), size = 0.6,
                                     linejoin = "mitre", color = median.col)
    }
  }

  # Confidence limits
  if (show.CI == TRUE) {
    gg.CI1 = ggplot2::geom_path(data = CI.boot, ggplot2::aes(x = x, y = UL), size = 1, color = "red")
    gg.CI2 = ggplot2::geom_path(data = CI.boot, ggplot2::aes(x = x, y = LL), size = 1, color = "red")
  }

  # plain linear regression line
  if (show.lm == TRUE) {
    gg.lm = ggplot2::geom_smooth(method = "lm", color = "darkgreen", se = FALSE)
    }

  if (show.points == TRUE) {
    gg.points = ggplot2::geom_point(data = data, ggplot2::aes_string(x = IV, y = DV), size = 1,
                                  shape = shape, fill = "white", color = "black")
  }

  if (title != "") {
    gg.title = ggplot2::theme(title = title)
  }


  gg.elements = list(gg.tiles, gg.poly, gg.spag, gg.median, gg.CI1, gg.CI2, gg.lm, gg.points,
                     gg.title, ggplot2::theme(legend.position = "none"))

  if (add == FALSE) {
    return(p0 + gg.elements)
  } else {
    return(gg.elements)
  }
}
