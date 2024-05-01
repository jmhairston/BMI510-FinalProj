#' Calculate the maximizing parameter p for Bernoulli log-likelihood
#'
#' This function performs a grid search to find the parameter p that maximizes the
#' log-likelihood of a Bernoulli-distributed dataset. It assumes p varies between 0 and 1,
#' and increments by 0.001 for each step in the search.
#'
#' @param data Numeric vector containing Bernoulli trial outcomes (0s and 1s).
#'
#' @return A list containing the maximum log-likelihood and the corresponding value of p.
#' @export
#' @examples
#' data <- c(1, 0, 0, 0, 1, 1, 1)
#' logLikBernoulli(data)
logLikBernoulli = function(data) {
  p_values = seq(0, 1, by = 0.001)
  log_likelihoods = sapply(p_values, function(p) {
    sum(dbinom(data, size = 1, prob = p, log = TRUE))
  })
  max_index = which.max(log_likelihoods)
  list(max_log_likelihood = log_likelihoods[max_index], p = p_values[max_index])
}
