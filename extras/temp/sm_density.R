#' Density plot with Bootstrapped Confidence Envelope
#'
#' This function creates a visually weighted coefficient plot
#'
#' @param data Dataframe
#' @param x Column name (unquoted)
#' @param k Number of bootstrap samples
#' @param boot_envelope Logical value to indicate whether the bootstrap confidence envelope should be displayed
#' @param model Character string indicating the type of bootstrapped confidence envelope to display;
#' @param rug_color Color for the rug plot
#' @param envelope_fill Fill color for the bootstrapped confidence envelope
#' @return A ggplot object giving the density plot
#' @export

sm_density = function(data, x, k = 1000, boot_envelope = TRUE, model = "none",
                      rug_color = "black", envelope_fill = "skyblue", ...){

  var  = eval(substitute(x), data) # Get variable
  var2 = substitute(.$x)           # Get variable for density()

  # Tests
  if(!is.numeric(var)) return("x is non-numeric") # Check that input is numeric
  if(length(var) == 0) return("x has no data")

  if(model == "none"){

    if(boot_envelope){

      range = max(var, na.rm = TRUE) - min(var, na.rm = TRUE)
      lower_bound = min(var, na.rm = TRUE) - 0.2 * range
      upper_bound = max(var, na.rm = TRUE) + 0.2 * range

      # Compute densities of bootstrap samples
      densities_within = data.frame(bs = 1:k) %>%
        dplyr::group_by(bs) %>%
        dplyr::mutate(
          data = list(data %>% dplyr::sample_frac(size = 1, replace = TRUE))
        ) %>%
        tidyr::unnest() %>%
        dplyr::group_by(bs) %>%
        dplyr::do(broom::tidy(density(eval(var2), from = lower_bound, to = upper_bound, n = 128, na.rm = TRUE))) %>%
        dplyr::ungroup()

      names(densities_within)[2] = "X"
      names(densities_within)[3] = "dens"

      densities_qtiles = densities_within %>%
        dplyr::group_by(X) %>%
        dplyr::summarise(
          q05 = quantile(dens, 0.025),
          q50 = quantile(dens, 0.500),
          q95 = quantile(dens, 0.975)
        )

      p = ggplot2::ggplot(data = densities_qtiles, ggplot2::aes(x = X)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = q05, ymax = q95), alpha = 0.5, fill = envelope_fill) +
        ggplot2::stat_density(data = data, ggplot2::aes(x = var), geom = "line") +
        ggplot2::geom_point(data = data, ggplot2::aes(x = var), y = 0, shape = "|", size = 2, color = rug_color) +
        ggplot2::xlab(deparse(substitute(x))) +
        ggplot2::ylab("Probability density") +
        ggplot2::theme_classic()

      } else{

        # Create plot
        p = ggplot2::ggplot(data = data, ggplot2::aes(x = var)) +
          ggplot2::stat_density(geom = "line") +
          ggplot2::geom_point(data = data, ggplot2::aes(x = var), y = 0, shape = "|", size = 2, color = rug_color) +
          ggplot2::xlab(deparse(substitute(x))) +
          ggplot2::ylab("Probability density") +
          ggplot2::theme_classic()
      }
  }

  if(model == "normal") {

    # Get parameter estimates
    mu_hat = MASS::fitdistr(na.omit(var), "normal")$estimate[[1]]
    sigma_hat = MASS::fitdistr(na.omit(var), "normal")$estimate[[2]]
    n = length(var)
    lower_bound = mu_hat - 4 * sigma_hat
    upper_bound = mu_hat + 4 * sigma_hat

    # Compute densities of bootstrap samples
    densities_within = data.frame(bs = 1:k) %>%
      dplyr::group_by(bs) %>%
      dplyr::mutate(
        data = list(rnorm(n, mu_hat, sigma_hat))
      ) %>%
      tidyr::unnest(cols = c(data)) %>%
      dplyr::group_by(bs) %>%
      dplyr::do(broom::tidy(density(.$data, from = lower_bound, to = upper_bound, n = 128, na.rm = TRUE))) %>%
      dplyr::ungroup()

    names(densities_within)[2] = "X"
    names(densities_within)[3] = "dens"

    densities_qtiles = densities_within %>%
      dplyr::group_by(X) %>%
      dplyr::summarise(
        q05 = quantile(dens, 0.025),
        q50 = quantile(dens, 0.500),
        q95 = quantile(dens, 0.975)
      )

    # Create plot
    p = ggplot2::ggplot(data = densities_qtiles, ggplot2::aes(x = X)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = q05, ymax = q95), alpha = 0.5, fill = envelope_fill) +
      ggplot2::stat_density(data = data, ggplot2::aes(x = var), geom = "line") +
      ggplot2::geom_point(data = data, ggplot2::aes(x = var), y = 0, shape = "|", size = 2, color = rug_color) +
      ggplot2::xlab(deparse(substitute(x))) +
      ggplot2::ylab("Probability density") +
      ggplot2::theme_classic()
  }

  print(p)

}

# sm_density(keith, homework) + theme_bw() + xlab("Time spent on homework")
# sm_density(city, gender)
# sm_density(city, income, boot_envelope = FALSE)
# sm_density(city, seniority, envelope_fill = "skyblue") + theme_bw()
#
# sm_density(keith, homework, model = "normal") + theme_bw()
# sm_density(city, income, model = "normal", envelope_fill = "skyblue", rug_color = "red")




##########################################


