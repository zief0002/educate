#' Print z-Test Results
#'
#' @param x prop.test() or prop_test() object
#'
#' @return Printed output of the z-test results including test type, hypotheses, z-value, and p-value.
#' @export
#'
#' @examples
#' my_z = prop.test(faithful$eruptions > 3, alternative = "less")
#' z_results(my_z)
#'
z_results = function(x){

  inequality_sign = switch(x$alternative, "less"="<", "greater"=">", "two.sided"="≠")

  if(x$estimate < x$null.value){z_value = -1*sqrt(x$statistic[[1]])} else{z_value = -sqrt(x$statistic[[1]])}

  cat(
    "\n", rep("-", 50), "\n", x$method, "\n", rep("-", 50), "\n",
    "\nH[0]: pi = ", x$null.value,
    "\nH[A]: pi ", inequality_sign, " ", x$null.value,
    "\nz = ", z_value,
    "\np = ", x$p.value,
    "\n\n", rep("-", 50), "\n",
    sep = "")

}
