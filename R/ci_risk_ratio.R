#' Confidence Interval for Risk Ratio
#'
#' @param p1 Proportion for Sample 1
#' @param n1 Sample size for Sample 1
#' @param p2 Proportion for Sample 2
#' @param n2 Sample size for Sample 2
#' @param conf.level Confidence level, default is 0.95
#'
#' @return Lower and upper limits on confidence interval.
#' @export
#'
#' @examples
#' ci_risk_ratio(p1 = 0.174, n1 = 27, p2 = .111, n2 = 25, conf.level = 0.95)
#'
ci_risk_ratio = function(p1, n1, p2, n2, conf.level = 0.95) {
  RR = p1 / p2
  z_star = (1 - conf.level) / 2 + conf.level
  LL = log(RR) - z_star * sqrt((1 / p1 - 1) / n1 + (1 / p2 - 1) / n2)
  UL = log(RR) + z_star * sqrt((1 / p1 - 1) / n1 + (1 / p2 - 1) / n2)

  cat(
    "\n",
    rep("-", 50),
    "\n",
    paste0(conf.level * 100, "% Confidence Interval"),
    "\n",
    rep("-", 50),
    "\n",
    "\nRisk Ratio = ",
    RR,
    "\n\nLower Limit = ",
    exp(LL),
    "\nUpper Limit = ",
    exp(UL),
    "\n\n",
    rep("-", 50),
    "\n",
    sep = ""
  )
}
