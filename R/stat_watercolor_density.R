#' Watercolor Bootstrapped Density
#'
#' This function creates a confidence envelope for the empirical density by bootstrapping from the data.
#' Transparency is used to indicate the level of uncertainty.
#'
#' @inheritParams ggplot2::stat_identity
#' @param k Number of bootstrapped smoothers. The default is 1000.
#' @param color Color for the bootstrapped fitted lines that make up the confidence envelope. The
#'     default is `color="#1D91C0"`
#' @param alpha Transparency level for the paths that make up the bootstrapped fitted lines. This may
#'    need to be adjusted if the argument `k=` is changed. The default value is `alpha=0.03`.
#' @param model The model to bootstrap from. The default is `model="none"` which bootstraps from the data.
#'     Using `model="normal"` draws repeated samples from a normal distribution with parameters based on the
#'     ML estimates from the data.
#'
#' @importFrom dplyr %>%
#'
#' @export



StatWatercolorDensity <- ggplot2::ggproto("StatWatercolorDensity", ggplot2::Stat,

  # Required aesthetics
  required_aes = c("x"),
  default_aes = ggplot2::aes(group = stat(boot_sample)),


  # Computations
  compute_group = function(data, scales, k = 1000, na.rm = TRUE,
                           alpha = 0.03, color = "#1D91C0",
                           model = "none", ...) {

    if (length(unique(data$x)) < 2) {
      # Not enough data to perform fit
      return(new_data_frame())
    }

    # Compute bounds on data
    lower_bound = min(data$x, na.rm = TRUE)
    upper_bound = max(data$x, na.rm = TRUE)

    d1 = data.frame(
      x = data$x
    )

    if(model == "normal") {

      # Get parameter estimates
      mu_hat = MASS::fitdistr(na.omit(data$x), "normal")$estimate[[1]]
      sigma_hat = MASS::fitdistr(na.omit(data$x), "normal")$estimate[[2]]
      n = length(data$x)

      # Function to compute densities
      helper_func = function(d){
        broom::tidy(density(d, from = lower_bound, to = upper_bound, n = 128, na.rm = TRUE))
      }

      # Compute densities of bootstrap samples
      message("Computing boostrapped smoothers ...")
      flush.console()

      densities_within = data.frame(group = 1:k) %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(
          d = list(rnorm(n, mu_hat, sigma_hat)),
          yhat = purrr::map(.x = d, .f = ~helper_func(.x))
        ) %>%
        dplyr::select(-d) %>%
        tidyr::unnest() %>%
        dplyr::ungroup()

    } else{

      # Function to compute densities
      helper_func = function(d){
        broom::tidy(density(d$x, from = lower_bound, to = upper_bound, n = 128, na.rm = TRUE))
        }

      # Bootstap
      message("Computing boostrapped smoothers ...")
      flush.console()

      densities_within = data.frame(group = 1:k) %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(
          d = list(d1 %>% dplyr::sample_frac(size = 1, replace = TRUE)),
          yhat = purrr::map(.x = d, .f = ~helper_func(.x))
        ) %>%
        dplyr::select(-d) %>%
        tidyr::unnest() %>%
        dplyr::ungroup()

      # Compute limits for conditional densities for better color gradient
      M = mean(densities_within$y)
      SD = sd(densities_within$y)
      low_limit = M - 3 * SD
      upp_limit = M + 3 * SD

      # Filter out extremes for better coloring
      densities_within = densities_within %>%
        dplyr::filter(y > low_limit, y < upp_limit)
    }

    # Output data for plotting
    data.frame(
      x = densities_within$x,
      y = densities_within$y,
      boot_sample = densities_within$group
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
#' @inheritParams StatWatercolorDensity
#'
#' @section Aesthetics:
#' These stat uses  `geom_line()` so support the
#' same aesthetics: `alpha`, `colour`, `linetype` and
#' `size`.
#'
#' @importFrom dplyr %>%
#' @export
stat_watercolor_density <- function(mapping = NULL, data = NULL, geom = "line",
                                   position = "identity", na.rm = TRUE,
                                   inherit.aes = TRUE, k = 1000,
                                   alpha = 0.03, color = "#1D91C0",
                                   model = "none", ...) {

    ggplot2::layer(
      stat = StatWatercolorDensity,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        k = k,
        alpha = alpha,
        color = color,
        model = model,
        ...)
  )
}


