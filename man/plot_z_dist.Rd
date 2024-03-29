% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_z_dist.R
\name{plot_z_dist}
\alias{plot_z_dist}
\title{Plot z-Distribution and Shaded p-Value}
\usage{
plot_z_dist(x, shade_p_value = TRUE)
}
\arguments{
\item{x}{prop.test() or prop_test() object}

\item{shade_p_value}{Logical value indicating whether to shade the area under the curve
corresponding to the p-value. Default is TRUE.}
}
\value{
Plot of the z-distribution. If shade_p_value=TRUE, the area under the curve
corresponding to the p-value will also be shaded.
}
\description{
Plot z-Distribution and Shaded p-Value
}
\examples{
heads <- rbinom(1, size = 100, prob = .5)
my_z = prop.test(heads, 100)
plot_z_dist(my_z)

}
