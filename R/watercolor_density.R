#' Density Plot with Bootstrapped Confidence Envelope
#'
#' This function creates an empirical density plot along with options to also include
#' a visually weighted confidence envelope.
#'
#' @param data Dataframe
#' @param x A column name in the dataframe. This should be unquoted.
#' @param k Number of bootstrap samples. The default value is 1000.
#' @param boot_envelope Logical value to indicate whether the bootstrap confidence envelope should be displayed.
#'                      The default value is TRUE.
#' @param model Character string indicating the type of bootstrapped confidence envelope to display.
#'              Its default value is "none". If it is set to "normal" then a parametric bootstrap
#'              confidence envelope based on random draws from a normal distribution with ML estimated
#'              parameters will be generated.
#' @param rug_color Color for the rug plot
#' @param envelope_fill Fill color for the monocolor bootstrapped confidence envelope.
#' @param alpha Transparency level for the paths that make up the bootstrapped confidence envelope. This may
#'              need to be adjusted if the argument k= is changed.
#' @param emp_dens_color Color of the line for the density plot of the empirical data. The default is "black".
#' @param emp_dens_size Size of the line for the density plot of the empirical data. The default value is 0.5.
#' @param multicolor Logical value indicating whether the confidence envelope should be multicolored.
#'                   The default value is FALSE which produces a monocolored envelope.
#' @param palette Color palette used if \code{multicolor=TRUE}. This defaults to the 9-color YlGnBu palette
#'                from \link{RColorBrewer}.
#'
#' @return A ggplot object giving the density plot
#'
#' @importFrom dplyr %>%
#'
#' @export

watercolor_density = function(data, x, k = 1000, boot_envelope = TRUE, model = "none",
                      rug_color = "black", envelope_fill = "#7FCDBB", alpha = 0.03,
                      emp_dens_color = "black", emp_dens_size = 0.5, multicolor = FALSE,
                      palette = RColorBrewer::brewer.pal(9, "YlGnBu"), ...){

  var  = eval(substitute(x), data) # Get variable
  var2 = substitute(.$x)           # Get variable for density()

  # Tests
  if(!is.numeric(var)) return("x is non-numeric") # Check that input is numeric
  if(length(var) == 0) return("x has no data")

  if(model == "none"){

    if(boot_envelope){

      lower_bound = min(var, na.rm = TRUE)
      upper_bound = max(var, na.rm = TRUE)

      # Compute densities of bootstrap samples
      message("Computing boostrapped smoothers ...")
      flush.console()

      densities_within = data.frame(bs = 1:k) %>%
        dplyr::group_by(bs) %>%
        dplyr::mutate(
          data = list(data %>% dplyr::sample_frac(size = 1, replace = TRUE))
        ) %>%
        tidyr::unnest() %>%
        dplyr::group_by(bs) %>%
        dplyr::do(broom::tidy(density(eval(var2), from = lower_bound, to = upper_bound, n = 128, na.rm = TRUE))) %>%
        dplyr::ungroup()

      # Rename variables
      names(densities_within)[2] = "X"
      names(densities_within)[3] = "dens"

      # Compute limits for conditional densities for better color gradient
      M = mean(densities_within$dens)
      SD = sd(densities_within$dens)
      low_limit = M - 3 * SD
      upp_limit = M + 3 * SD

      if(multicolor){

        # Get empirical densities
        my_dens = data.frame(
          X = density(eval(var), from = min(var, na.rm = TRUE), to = max(var, na.rm = TRUE), n = 128, na.rm = TRUE)$x,
          emp_dens = density(eval(var), from = min(var, na.rm = TRUE), to = max(var, na.rm = TRUE), n = 128, na.rm = TRUE)$y
        )

        # Join with empirical density and create color gradient levels
        densities_within = densities_within %>%
          dplyr::left_join(my_dens, by = "X") %>%
          dplyr::filter(dens > low_limit, dens < upp_limit) %>%
          dplyr::group_by(X) %>%
          dplyr::mutate(
            dens.scaled = abs(dens - emp_dens),
            dens.scaled2 = dens.scaled / (max(dens.scaled) - min(dens.scaled))
          ) %>%
          dplyr::ungroup()

        # Create multicolored watercolor density plot
        p = ggplot2::ggplot(data = densities_within, ggplot2::aes(x = X)) +
          ggplot2::geom_path(ggplot2::aes(group = bs, y = dens, color = dens.scaled2)) +
          ggplot2::scale_color_gradientn("dens.scaled", colors = rev(palette)) +
          ggplot2::scale_alpha_continuous(range = c(0.001, 1)) +
          ggplot2::stat_density(data = data, ggplot2::aes(x = var), geom = "line", color = emp_dens_color, size = 0.5) +
          ggplot2::theme_classic() +
          ggplot2::guides(color = FALSE, alpha = FALSE)

      } else{

        # Filter out extremes for better plotting
        densities_within = densities_within %>%
          dplyr::filter(dens > low_limit, dens < upp_limit)

        # Create monocolored watercolor density plot
        p = ggplot2::ggplot(data = densities_within, ggplot2::aes(x = X)) +
          ggplot2::geom_path(ggplot2::aes(group = bs, y = dens), color = envelope_fill, alpha = alpha) +
          ggplot2::stat_density(data = data, ggplot2::aes(x = var), geom = "line", color = emp_dens_color, size = emp_dens_size) +
          ggplot2::geom_point(data = data, ggplot2::aes(x = var), y = 0, shape = "|", size = 2, color = rug_color) +
          ggplot2::xlab(deparse(substitute(x))) +
          ggplot2::ylab("Probability density") +
          ggplot2::theme_classic()
      }
    } else{

      # Create density plot with no enevelope
      p = ggplot2::ggplot(data = data, ggplot2::aes(x = var)) +
        ggplot2::stat_density(geom = "line", color = emp_dens_color, size = emp_dens_size) +
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
    message("Computing boostrapped smoothers ...")
    flush.console()

    densities_within = data.frame(bs = 1:k) %>%
      dplyr::group_by(bs) %>%
      dplyr::mutate(
        data = list(rnorm(n, mu_hat, sigma_hat))
      ) %>%
      tidyr::unnest() %>%
      dplyr::group_by(bs) %>%
      dplyr::do(broom::tidy(density(.$data, from = lower_bound, to = upper_bound, n = 128, na.rm = TRUE))) %>%
      dplyr::ungroup()

    names(densities_within)[2] = "X"
    names(densities_within)[3] = "dens"

    if(multicolor){

      densities_within = densities_within %>%
        dplyr::group_by(X) %>%
        dplyr::mutate(
          norm_dens = dnorm(X, mean = mu_hat, sd = sigma_hat),
          dens.scaled = abs(dens - norm_dens),
          dens.scaled2 = dens.scaled / (max(dens.scaled) - min(dens.scaled))
        ) %>%
        dplyr::ungroup()

      # Create multicolored watercolor normal density plot
      p = ggplot2::ggplot(data = densities_within, ggplot2::aes(x = X)) +
        ggplot2::geom_path(ggplot2::aes(group = bs, y = dens, color = dens.scaled2)) +
        ggplot2::scale_color_gradientn("dens.scaled", colors = rev(palette)) +
        ggplot2::scale_alpha_continuous(range = c(0.001, 1)) +
        ggplot2::stat_density(data = data, ggplot2::aes(x = var), geom = "line", color = emp_dens_color, size = 0.5) +
        ggplot2::theme_classic() +
        ggplot2::guides(color = FALSE, alpha = FALSE)


    } else{

      # Create monocolored watercolor normal density plot
      p = ggplot2::ggplot(data = densities_within, ggplot2::aes(x = X)) +
        geom_path(aes(group = bs, y = dens), color = envelope_fill, alpha = alpha) +
        ggplot2::stat_density(data = data, ggplot2::aes(x = var), geom = "line", color = emp_dens_color, size = emp_dens_size) +
        ggplot2::geom_point(data = data, ggplot2::aes(x = var), y = 0, shape = "|", size = 2, color = rug_color) +
        ggplot2::xlab(deparse(substitute(x))) +
        ggplot2::ylab("Probability density") +
        ggplot2::theme_classic()
      }
  }
  message("Build ggplot figure ...")
  flush.console()

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


