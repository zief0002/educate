#' Watercolor Bootstrapped Smoother
#'
#' This function creates a visually regression smoother, also called a watercolor plot. The initial idea
#' came from Solomon Hsiang.
#'
#' @inheritParams ggplot2::stat_smooth
#' @inheritParams ggplot2::stat_identity
#' @param k Number of bootstrapped smoothers. The default is 700.
#' @param color Color for the bootstrapped fitted lines that make up the confidence envelope. The
#'     default is `color="#1D91C0"`
#' @param alpha Transparency level for the paths that make up the bootstrapped fitted lines. This may
#'    need to be adjusted if the argument `k=` is changed. The default value is `alpha=0.06`.
#' @param method Smoothing method (function) to use, accepts either a character vector,
#'   e.g. `"auto"`, `"lm"`, `"glm"`, `"gam"`, `"loess"` or a function, e.g.
#'   `MASS::rlm` or `mgcv::gam`, `stats::lm`, or `stats::loess`.
#'
#'   For `method = "auto"` the smoothing method is chosen based on the
#'   size of the largest group (across all panels). [stats::loess()] is
#'   used for less than 1,000 observations; otherwise [mgcv::gam()] is
#'   used with `formula = y ~ s(x, bs = "cs")`. Somewhat anecdotally,
#'   `loess` gives a better appearance, but is \eqn{O(N^{2})}{O(N^2)} in memory,
#'   so does not work for larger datasets.
#'
#'   If you have fewer than 1,000 observations but want to use the same `gam()`
#'   model that `method = "auto"` would use, then set
#'   `method = "gam", formula = y ~ s(x, bs = "cs")`.
#' @param formula Formula to use in smoothing function. The default is `formula = y ~ x`,
#' @param fullrange Should the fit span the full range of the plot, or just
#'   the data? The default is `fullrange=FALSE`.
#' @param span Controls the amount of smoothing for the default loess smoother.
#'   Smaller numbers produce wigglier lines, larger numbers produce smoother
#'   lines.
#' @param n Number of points at which to evaluate smoother.
#' @param method.args List of additional arguments passed on to the modelling
#'   function defined by `method`.
#' @importFrom dplyr %>%
#' @export



StatWatercolorSmoother <- ggplot2::ggproto("StatWatercolorSmoother", ggplot2::Stat,

  # Required aesthetics
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(group = stat(boot_sample)),

  setup_params = function(data, params) {
    if (identical(params$method, "auto")) {
      # Use loess for small datasets, gam with a cubic regression basis for
      # larger. Based on size of the _largest_ group to avoid bad memory
      # behaviour of loess
      max_group <- max(table(interaction(data$group, data$PANEL, drop = TRUE)))

      if (max_group < 1000) {
        params$method <- "loess"
      } else {
        params$method <- "gam"
        params$formula <- y ~ s(x, bs = "cs")
      }
      message("`geom_smooth()` using method = '", params$method,
              "' and formula '", deparse(params$formula), "'")
    }
    if (identical(params$method, "gam")) {
      params$method <- mgcv::gam
    }

    params
  },

  # Computations
  compute_group = function(data, scales, params, k = 700, formula = y ~ x, method = "auto",
                           n = 80, span = 0.75, fullrange = FALSE, na.rm = FALSE,
                           xseq = NULL,method.args = list(),
                           alpha = 0.06, color = "#1D91C0", ...) {

    if (length(unique(data$x)) < 2) {
      # Not enough data to perform fit
      return(new_data_frame())
    }

    if (is.null(data$weight)) data$weight <- 1

    if (is.null(xseq)) {
      if (is.integer(data$x)) {
        if (fullrange) {
          xseq <- scales$x$dimension()
        } else {
          xseq <- sort(unique(data$x))
        }
      } else {
        if (fullrange) {
          range <- scales$x$dimension()
        } else {
          range <- range(data$x, na.rm = TRUE)
        }
        xseq <- seq(range[1], range[2], length.out = n)
      }
    }
    # Special case span because it's the most commonly used model argument
    if (identical(method, "loess")) {
      method.args$span <- span
    }


    d1 = data.frame(
      x = data$x,
      y = data$y,
      weight = data$weight
    )

    if (is.character(method)) method <- match.fun(method)

    # Helper function to compute predicted values
    helper_func = function(d){
      base.args <- list(quote(formula), data = quote(d), weights = quote(weight))
      model <- do.call(method, c(base.args, method.args))
      predict(model, newdata = d)
    }


      # Bootstap
      boot_strap = data.frame(group = 1:k) %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(
          d = list(d1 %>% dplyr::sample_frac(size = 1, replace = TRUE)),
          yhat = purrr::map(.x = d, .f = ~helper_func(.x))
        ) %>%
        tidyr::unnest()


    data.frame(
      x = boot_strap$x,
      y = boot_strap$yhat,
      boot_sample = boot_strap$group
    )



    }
)



#' Stat Method for Watercolor Bootstrapped Smoother
#'
#' This function creates a visually regression smoother, also called a watercolor plot. The initial idea
#' came from Solomon Hsiang.
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @inheritParams StatWatercolorSmoother
#'
#' @section Aesthetics:
#' These stat uses  `geom_line()` so support the
#' same aesthetics: `alpha`, `colour`, `linetype` and
#' `size`.
#'
#' @importFrom dplyr %>%
#' @export
stat_watercolor_smooth <- function(mapping = NULL, data = NULL, geom = "line",
                                   position = "identity", na.rm = FALSE, show.legend = NA,
                                   inherit.aes = TRUE, k = 700, formula = y ~ x, method = "auto",
                                   n = 80, span = 0.75, fullrange = FALSE,
                                   xseq = NULL, method.args = list(),
                                   alpha = 0.06, color = "#1D91C0",...) {

    ggplot2::layer(
      stat = StatWatercolorSmoother,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        k = k,
        formula = formula,
        method = method,
        n = n,
        fullrange = fullrange,
        method.args = method.args,
        span = span,
        alpha = alpha,
        color = color,
        ...)
  )
}


