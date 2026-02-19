#' Print t-Test Results
#'
#' @param x t.test() or t_test() object
#'
#' @return Printed output of the t-test results including test type, hypotheses, t-value, df, and p-value.
#' @export
#'
#' @examples
#' my_t = t.test(cars$speed, mu = 16, alternative = "less")
#' t_results(my_t)
#'
t_results = function(x) {
  inequality_sign = switch(
    x$alternative,
    "less" = "<",
    "greater" = ">",
    "two.sided" = "\u2260"
  )

  if (x$method == "One Sample t-test") {
    cat(
      "\n",
      rep("-", 50),
      "\n",
      x$method,
      "\n",
      rep("-", 50),
      "\n",
      "\nH[0]: mu = ",
      x$null.value,
      "\nH[A]: mu ",
      inequality_sign,
      " ",
      x$null.value,
      "\n\nt(",
      x$parameter,
      ") = ",
      x$statistic,
      "\np = ",
      x$p.value,
      "\n\n",
      rep("-", 50),
      "\n",
      sep = ""
    )
  } else {
    # Extract group names
    group1 = gsub(
      x = names(x$estimate)[1],
      pattern = "mean in group ",
      replacement = ""
    )
    group2 = gsub(
      x = names(x$estimate)[2],
      pattern = "mean in group ",
      replacement = ""
    )

    cat(
      "\n",
      rep("-", 50),
      "\n",
      x$method,
      "\n",
      rep("-", 50),
      "\n",
      "\nH[0]: mu_[",
      group1,
      "] = mu_[",
      group2,
      "]",
      "\nH[A]: mu_[",
      group1,
      "] ",
      inequality_sign,
      " mu_[",
      group2,
      "]",
      "\n\nt(",
      x$parameter,
      ") = ",
      x$statistic,
      "\np = ",
      x$p.value,
      "\n\n",
      rep("-", 50),
      "\n",
      sep = ""
    )
  }
}
