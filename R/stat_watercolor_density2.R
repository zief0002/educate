#' Watercolor Bootstrapped Density (Multicolor)
#'
#' This function creates a confidence envelope for the empirical density by bootstrapping from the data.
#' Transparency is used to indicate the level of uncertainty.
#'
#' @inheritParams ggplot2::stat_identity
#' @param k Number of bootstrapped smoothers. The default is 1000.
#' @param alpha Transparency level for the paths that make up the bootstrapped fitted lines. This may
#'    need to be adjusted if the argument `k=` is changed. The default value is `alpha=0.03`.
#' @param model The model to bootstrap from. The default is `model="none"` which bootstraps from the data.
#'     Using `model="normal"` draws repeated samples from a normal distribution with parameters based on the
#'     ML estimates from the data.
#'
#' @importFrom dplyr %>%
#'
#' @export



StatWatercolorDensity2 <- ggplot2::ggproto("StatWatercolorDensity", ggplot2::Stat,

      # Required aesthetics
      required_aes = c("x"),
      default_aes = ggplot2::aes(y = stat(y), group = stat(group), color = stat(color)),


      # Computations
      compute_group = function(data, scales, k = 1000, na.rm = TRUE,
                               alpha = 0.03, model = "none",
                               ...) {
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

          ###################################################################
          # model = "normal"
          ###################################################################

          # Get parameter estimates
          mu_hat = MASS::fitdistr(na.omit(data$x), "normal")$estimate[[1]]
          sigma_hat = MASS::fitdistr(na.omit(data$x), "normal")$estimate[[2]]
          n = length(data$x)

          # Compute densities of bootstrap samples
          message("Computing boostrapped smoothers ...")
          flush.console()

          densities_within = data.frame(group = 1:k) %>%
            dplyr::group_by(group) %>%
            dplyr::mutate(
              d = list(data %>% dplyr::sample_frac(size = 1, replace = TRUE))
            ) %>%
            tidyr::unnest() %>%
            dplyr::do(broom::tidy(density(.$x, from = lower_bound, to = upper_bound, n = 128, na.rm = TRUE))) %>%
            dplyr::ungroup() %>%
            dplyr::group_by(x) %>%
            dplyr::mutate(
              norm_dens = dnorm(x, mean = mu_hat, sd = sigma_hat),
              dens.scaled = abs(y - norm_dens),
              dens.scaled2 = dens.scaled / (max(dens.scaled) - min(dens.scaled))
            ) %>%
            dplyr::ungroup()

          } else{

            ###################################################################
            # model = "none"
            ###################################################################

            # Bootstap
            message("Computing boostrapped smoothers ...")
            flush.console()

            densities_within = data.frame(group = 1:k) %>%
              dplyr::group_by(group) %>%
              dplyr::mutate(
                d = list(d1 %>% dplyr::sample_frac(size = 1, replace = TRUE))
              ) %>%
              tidyr::unnest() %>%
              dplyr::group_by(group) %>%
              dplyr::do(broom::tidy(density(.$x, from = lower_bound, to = upper_bound, n = 128, na.rm = TRUE))) %>%
              dplyr::ungroup()

            # Get empirical densities
            my_dens = data.frame(
              x        = density(data$x, from = lower_bound, to = upper_bound, n = 128, na.rm = TRUE)$x,
              emp_dens = density(data$x, from = lower_bound, to = upper_bound, n = 128, na.rm = TRUE)$y
            )

            # Compute limits for conditional densities for better color gradient
            M = mean(densities_within$y)
            SD = sd(densities_within$y)
            low_limit = M - 3 * SD
            upp_limit = M + 3 * SD

            # Filter out extremes for better coloring
            densities_within = densities_within %>%
              dplyr::left_join(my_dens, by = "x") %>%
              dplyr::filter(y > low_limit, y < upp_limit) %>%
              dplyr::group_by(x) %>%
              dplyr::mutate(
                dens.scaled = abs(y - emp_dens),
                dens.scaled2 = dens.scaled / (max(dens.scaled) - min(dens.scaled))
              ) %>%
              dplyr::ungroup()

          }



        # Output data for plotting
        data.frame(
          x = densities_within$x,
          y = densities_within$y,
          group = densities_within$group,
          color = densities_within$dens.scaled2
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
#' @inheritParams StatWatercolorDensity2
#'
#' @section Aesthetics:
#' These stat uses  `geom_line()` so support the
#' same aesthetics: `alpha`, `colour`, `linetype` and
#' `size`.
#'
#' @importFrom dplyr %>%
#' @export
stat_watercolor_density2 <- function(mapping = NULL, data = NULL, geom = "line",
                                    position = "identity", na.rm = TRUE,
                                    inherit.aes = TRUE, k = 1000,
                                    alpha = 0.03,
                                    model = "none", ...) {

  ggplot2::layer(
    stat = StatWatercolorDensity2,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      k = k,
      alpha = alpha,
      model = model,
      ...)
  )
}


