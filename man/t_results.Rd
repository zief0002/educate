% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/t_results.R
\name{t_results}
\alias{t_results}
\title{Print t-Test Results}
\usage{
t_results(x)
}
\arguments{
\item{x}{t.test() or t_test() object}
}
\value{
Printed output of the t-test results including test type, hypotheses, t-value, df, and p-value.
}
\description{
Print t-Test Results
}
\examples{
my_t = t.test(cars$speed, mu = 16, alternative = "less")
t_results(my_t)

}
