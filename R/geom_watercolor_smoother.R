geom_watercolor_smoother <- function(mapping = NULL, data = NULL,
                           stat = "smooth", position = NULL,
                           method = "auto",
                           na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  vars <- all.vars(formula)
  response <- vars[1]
  factor <- vars[2]
  mymap <- aes_string(x=factor,y=response)
  fun_med <- function(x) {
    return(data.frame(y = median(x), label = round(median(x), 3)))
  }
  position <- position_dodge(width)
  l1 <- layer(data = data, mapping = mymap, stat = StatBoxplot,
              geom = "errorbar", position = position, show.legend = show.legend,
              inherit.aes = inherit.aes, params = list(na.rm = na.rm,
                                                       coef = coef, width = width, ...))
  l2 <- layer(data = data, mapping = mymap, stat = stat, geom = GeomBoxplot,
              position = position, show.legend = show.legend, inherit.aes = inherit.aes,
              params = list(outlier.colour = outlier.colour, outlier.shape = outlier.shape,
                            outlier.size = outlier.size, outlier.stroke = outlier.stroke,
                            notch = notch, notchwidth = notchwidth, varwidth = varwidth,
                            na.rm = na.rm, ...))
  l3 <- layer(data = data, mapping = mymap, stat = StatSummary,
              geom = "label", position = position, show.legend = show.legend,
              inherit.aes = inherit.aes, params = list(fun.data = fun_med,
                                                       fun.y = fun.y, fun.ymax = fun.ymax, fun.ymin = fun.ymin,
                                                       fun.args = fun.args, na.rm=na.rm,family=font,size=fsize/3,vjust=-0.1,...))
  return(list(l1,l2,l3))
}
