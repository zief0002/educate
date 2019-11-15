# educate::sm_density(mtcars,x = mpg)
#
#
# d3 = sm.density(mtcars$mpg, model = "Normal")
#
# mu_hat = MASS::fitdistr(na.omit(mtcars$mpg), "normal")$estimate[[1]]
# sigma_hat = MASS::fitdistr(na.omit(mtcars$mpg), "normal")$estimate[[2]]
# sigma_hat = sd(mtcars$mpg)
# n = length(data$x)
#
#
# kde = density(mtcars$mpg, n = 1024)
# kde = density(rnorm(n = 10000, mean = mu_hat, sd = sigma_hat))
# names(d)
#
# # R(K) for a normal
# Rk <- 1 / (2 * sqrt(pi))
#
# # Selected bandwidth
# h <- kde$bw
# h = d3$h
#
#
# n = length(d3$eval.points)
#
# # Estimate the variance
# var_kde_hat <- kde$y * Rk / (n * h)
# var_kde_hat <- y * Rk / (n * h)
#
# # True expectation and variance (because the density is a normal)
# E_kde <- dnorm(x = kde$x, mean = mu, sd = sqrt(sigma^2 + h^2))
# var_kde <- (dnorm(kde$x, mean = mu, sd = sqrt(h^2 / 2 + sigma^2)) /
#               (2 * sqrt(pi) * h) - E_kde^2) / n
#
# # CI with estimated variance
# alpha <- 0.05
# z_alpha <- qnorm(1 - alpha/2)
# ci_low_1 <- kde$y - z_alpha * sqrt(var_kde_hat)
# ci_up_1 <- kde$y + z_alpha * sqrt(var_kde_hat)
#
# ci_low_1 <- y - z_alpha * sqrt(var_kde_hat)
# ci_up_1 <- y + z_alpha * sqrt(var_kde_hat)
#
#
# lines(kde$x, kde$y, col="blue")
#
# lines(kde$x, ci_up_1, col = "red")
#
#
# # CI with known variance
# ci_low_2 <- kde$y - z_alpha * sqrt(var_kde)
# ci_up_2 <- kde$y + z_alpha * sqrt(var_kde)
#
#
# y = dnorm(d3$eval.points, mean = mu_hat, sd = sigma_hat)
#
# lines(d3$eval.points, y, col = "blue")
# lines(d3$eval.points, ci_up_1, col = "red", lty = "dashed")
