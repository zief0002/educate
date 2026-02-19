#' Print z-Test Results
#'
#' @param x prop.test() or prop_test() object
#'
#' @return Printed output of the z-test results including test type, hypotheses, z-value, and p-value.
#' @importFrom mosaic prop_test
#' @export
#'
#' @examples
#' library(mosaic)
#' my_z = prop_test(x = faithful$eruptions > 3, alternative = "less")
#' z_results(my_z)
#'
z_results = function(x) {
  inequality_sign = switch(
    x$alternative,
    "less" = "<",
    "greater" = ">",
    "two.sided" = "\u2260"
  )

  if (
    x$method %in%
      c(
        "1-sample proportions test with continuity correction",
        "1-sample proportions test without continuity correction"
      )
  ) {
    if (x$estimate < x$null.value) {
      z_value = -1 * sqrt(x$statistic[[1]])
    } else {
      z_value = sqrt(x$statistic[[1]])
    }

    cat(
      "\n",
      rep("-", 50),
      "\n",
      x$method,
      "\n",
      rep("-", 50),
      "\n",
      "\nH[0]: pi = ",
      x$null.value,
      "\nH[A]: pi ",
      inequality_sign,
      " ",
      x$null.value,
      "\nz = ",
      z_value,
      "\np = ",
      x$p.value,
      "\n\n",
      rep("-", 50),
      "\n",
      sep = ""
    )
  } else {
    if (x$estimate[[1]] < x$estimate[[2]]) {
      z_value = -1 * sqrt(x$statistic[[1]])
    } else {
      z_value = -sqrt(x$statistic[[1]])
    }

    cat(
      "\n",
      rep("-", 50),
      "\n",
      x$method,
      "\n",
      rep("-", 50),
      "\n",
      "\nH[0]: pi_1 = pi_2",
      "\nH[A]: pi_1 ",
      inequality_sign,
      " p1_2",
      "\nz = ",
      z_value,
      "\np = ",
      x$p.value,
      "\n\n",
      rep("-", 50),
      "\n",
      sep = ""
    )
  }
}
