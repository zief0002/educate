#' Bootstrapped Confidence Envelope for Density
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
#' @export



StatDensityWatercolor <- ggplot2::ggproto("StatWatercolorDensity", ggplot2::Stat,

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
      mu_hat = mean(data$x, na.rm = TRUE) #MASS::fitdistr(na.omit(data$x), "normal")$estimate[[1]]
      sigma_hat = sd(data$x, na.rm = TRUE) #MASS::fitdistr(na.omit(data$x), "normal")$estimate[[2]]
      n = length(data$x)

      # Function to compute densities
      helper_func = function(d){
        broom::tidy(density(d, from = lower_bound, to = upper_bound, n = 128, na.rm = TRUE))
      }

      # Print message
      message("Boostrapping densities ...")
      flush.console()

      # Set up empty list
      temp = vector(mode = "list", length = k)

      # Bootstrap densities
      for(i in 1:k){
        boot_x = rnorm(n, mu_hat, sigma_hat)
        d = density(boot_x, from = lower_bound, to = upper_bound, n = 128, na.rm = TRUE)
        temp[[i]] = data.frame(
          x = d$x,
          y = d$y,
          group = i
        )
      }

      # Combine list into dataframe
      densities_within = do.call(rbind, temp)

    } else{

      # Print message
      message("Boostrapping densities ...")
      flush.console()

      # Create empty list
      temp = vector(mode = "list", length = k)

      # Bootsrap densities
      for(i in 1:k){
        boot_x = sample(d1$x, replace = TRUE)
        d = density(boot_x, from = lower_bound, to = upper_bound, n = 128, na.rm = TRUE)
        temp[[i]] = data.frame(
          x = d$x,
          y = d$y,
          group = i
        )
      }

      # Combine list into data frame
      densities_within = do.call(rbind, temp)


      # Compute limits for conditional densities for better color gradient
      M = mean(densities_within$y)
      SD = sd(densities_within$y)
      low_limit = M - 3 * SD
      upp_limit = M + 3 * SD

      # Filter out extremes for better coloring
      densities_within = densities_within[densities_within$y > low_limit & densities_within$y < upp_limit, ]

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
#' @inheritParams StatDensityWatercolor
#'
#' @section Aesthetics:
#' These stat uses  `geom_line()` so support the
#' same aesthetics: `alpha`, `colour`, `linetype` and
#' `size`.
#'
#' @importFrom dplyr %>%
#' @export
stat_density_watercolor <- function(mapping = NULL, data = NULL, geom = "line",
                                   position = "identity", na.rm = TRUE,
                                   inherit.aes = TRUE, k = 1000,
                                   alpha = 0.03, color = "#1D91C0",
                                   model = "none", ...) {

    ggplot2::layer(
      stat = StatDensityWatercolor,
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


