#' Confidence Interval for Cohen's h
#'
#' @param p1 Proportion for Sample 1
#' @param n1 Sample size for Sample 1
#' @param n1 Proportion for Sample 2
#' @param n2 Sample size for Sample 2
#' @param conf.level Confidence level, default is 0.95
#'
#' @return Lower and upper limits on confidence interval.
#' @export
#'
#' @examples
#' ci_cohens_h(p1 = 0.174, n1 = 27, p2 = .111, n2 = 25, conf.level = 0.95)
#'
ci_cohens_h = function(p1, p2, n1, n2, conf.level = 0.95 ){
  x1 = asin(sign(p1) * sqrt(abs(p1)))
  x2 = asin(sign(p2) * sqrt(abs(p2)))
  h = x1 - x2
  se = sqrt(0.25 * (1 / n1 + 1 / n2 ))
  moe = qnorm(1 - (1 - conf.level) / 2) * se
  h_low = (h - moe)*2
  h_upp = (h + moe)*2

  cat(
    "\n", rep("-", 50), "\n", paste0(conf.level*100, "% Confidence Interval"), "\n", rep("-", 50), "\n",
    "\nCohen's h = ", h,
    "\n\nLower Limit = ", h_low,
    "\nUpper Limit = ", h_upp,
    "\n\n", rep("-", 50), "\n",
    sep = "")
}
