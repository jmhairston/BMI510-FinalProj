#' Reverse Scaling of a Vector
#'
#' This function takes a scaled vector and reverses the scaling using the original mean and standard deviation.
#' @param x A numeric vector that has been scaled.
#' @param original_mean The mean used to center the data before scaling.
#' @param original_sd The standard deviation used to scale the data.
#' @return A numeric vector with the scaling reversed.
#' @export
#' @examples
#' original_vector = c(1, 2, 3, 4, 5)
#' scaled_vector = scale(original_vector)
#' unscaled_vector = unscale(scaled_vector, mean(original_vector), sd(original_vector))
#' all.equal(original_vector, unscaled_vector)  # Should return TRUE
unscale = function(x, original_mean, original_sd) {
  x * original_sd + original_mean
}
