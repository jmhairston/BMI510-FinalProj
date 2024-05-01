#' Calculate Min Sample Size for a t-Test
#'
#' This function calculates the minimum sample size needed for a one-sample or two-sample t-test
#' on the provided data to achieve a power of 80% and alpha of 0.05.
#' @param x1 A numeric vector of preliminary data for the one-sample or the first sample in a two-sample t-test.
#' @param x2 An optional numeric vector of preliminary data for the second sample in a two-sample t-test.
#' @return The minimum sample size required to achieve the specified power and significance level.
#' @importFrom pwr pwr.t.test
#' @export
#' @examples
#' # One-sample t-test
#' data1 = rnorm(10, mean = 5)
#' minimumN(data1)
#'
#' # Two-sample t-test
#' data2 = rnorm(10, mean = 5.5)
#' minimumN(data1, data2)
minimumN = function(x1, x2 = NULL) {
  require(pwr)
  
  # Define the test parameters
  alpha = 0.05
  power = 0.8
  d = if (is.null(x2)) {
    # One-sample
    (mean(x1) - 0) / sd(x1)
  } else {
    # Two-sample
    (mean(x1) - mean(x2)) / sqrt((sd(x1)^2 + sd(x2)^2) / 2)
  }
  
  # Perform power analysis
  pwr_result = pwr::pwr.t.test(d = d,
                                sig.level = alpha,
                                power = power,
                                type = if (is.null(x2)) "one.sample" else "two.sample",
                                alternative = "two.sided")
  
  return(ceiling(pwr_result$n))
}
