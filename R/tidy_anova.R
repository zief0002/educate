#' Tidy a(n) ANOVA table
#'
#' @param x an object containing the results returned by a model fitted with lm().
#' @param model Logical indicating whether the model-level partitioning (TRUE) or term-level partitioning (FALSE; default) should be used
#'
#' @return A data.frame with columns for the source of variation, degrees of freedom, Sum of Squares, Mean Squares, F-statistics, and p-values
#' @export
#'
#' @examples
#' lm.1 = lm(mpg ~ 1 + hp + wt, data = mtcars)
#' tidy_anova(lm.1, model = TRUE)
tidy_anova = function(x, model = FALSE){
  co <- stats::coef(x)
  av = anova(x)

  # Model partitioning
  if(model == TRUE){
    k = length(av$Df)

    # Set up columns for source, df, and sums of squares
    ret <- data.frame(
      Source = c("Model", "Residuals"),
      df = c(sum(av$Df[-k]), av$Df[k]),
      SS = c(sum(av$'Sum Sq'[-k]), av$'Sum Sq'[k]),
      stringsAsFactors = FALSE
    )

    # Add column for squares
    ret$MS = ret$SS / ret$df

    # Compute F- and p-value
    f.value = ret$MS[1] / ret$MS[2]
    p.value = 1- pf(f.value, df1 = ret$df[1], df2 = ret$df[2])

    # Add column for F-statistic
    ret$F = c(f.value, NA)

    # Add column for p-value
    ret$p = c(p.value, NA)

  } else{

  # Term-level partitioning
  ret <- data.frame(
    Source = c(names(co)[-1], "Residuals"),
    df = av$Df,
    SS = av$'Sum Sq',
    MS = av$'Mean Sq',
    F = av$'F value',
    p = av$'Pr(>F)',
    stringsAsFactors = FALSE
    )

  }

  return(ret)

}




