#' Principal Component Approximation
#'
#' This function performs PCA on data and reconstructs it using the specified number of principal components.
#' The reconstructed data is then rescaled and centered to match the original data.
#' @param x A matrix or data frame whose approximation is required.
#' @param npc The number of principal components to use for the approximation.
#' @return A matrix or data frame of the reconstructed data.
#' @export
#' @examples
#' data_matrix = matrix(rnorm(100), ncol = 10)
#' approx_data = pcApprox(data_matrix, npc = 2)
#' dim(approx_data)  # Should match the original data dimensions
pcApprox = function(x, npc) {
  # Perform PCA on the scaled data
  pca_res = prcomp(x, scale. = TRUE)

  # Use only the first npc principal components for the approximation
  approx = pca_res$x[, 1:npc] %*% t(pca_res$rotation[, 1:npc])

  # Rescale and recenter
  approx = scale(approx, center = -pca_res$center, scale = FALSE)
  approx = scale(approx, center = FALSE, scale = 1 / pca_res$scale)

  return(approx)
}
