




mu_hat = MASS::fitdistr(na.omit(mtcars$mpg), "normal")$estimate[[1]]
sigma_hat = MASS::fitdistr(na.omit(mtcars$mpg), "normal")$estimate[[2]]
sigma_hat = sd(mtcars$mpg)
n = length(data$x)


#kde = density(mtcars$mpg, n = 100)


x = seq(from = min(mtcars$mpg), to = max(mtcars$mpg), length.out = 100)
y = dnorm(x, mean = mu_hat, sd = sigma_hat)

# R(K) for a normal
Rk <- 1 / (2 * sqrt(pi))

# Selected bandwidth
h <- kde$bw
h = d3$h


n = length(x)

# Estimate the variance
# var_kde_hat <- y * Rk / (n * h)
# var_kde_hat <- y * Rk / (n * h)

# True expectation and variance (because the density is a normal)
E_kde <- dnorm(x = x, mean = mu_hat, sd = sqrt(sigma_hat^2 + h^2))
var_kde <- (dnorm(x, mean = mu_hat, sd = sqrt(h^2 / 2 + sigma_hat^2)) /
              (2 * sqrt(pi) * h) - E_kde^2) / n

# CI with estimated variance
alpha <- 0.05
z_alpha <- qnorm(1 - alpha/2)
# ci_low_1 <- y - z_alpha * sqrt(var_kde_hat)
# ci_up_1 <- y + z_alpha * sqrt(var_kde_hat)


# CI with known variance
ci_low_2 <- y - z_alpha * sqrt(var_kde)
ci_up_2 <- y + z_alpha * sqrt(var_kde)


y = dnorm(d3$eval.points, mean = mu_hat, sd = sigma_hat)

sm::sm.density(mtcars$mpg, model = "Normal")

lines(x, y, col = "blue")
lines(x, ci_up_1, col = "purple", lty = "dashed")
lines(x, ci_up_2, col = "red", lty = "dashed")
lines(x, ci_low_2, col = "red", lty = "dashed")
