#' Transform triangular correlation matrix
#'
#' Transforms a correlation matrix on triangular form to a correlation
#' matrix on full form.
#'
#' @param x matrix. A correlation matrix on triangular form.
#'
#' @return matrix. Correlation matrix on full from.
#' @export
#'
#' @examples
trans_triangular_corr <- function(x) {
  # Input checks
  if (!is_triangular(x)) {
    stop("x is not a triangular matrix")
  }
  if (!is_corr_matrix(x, type = "triangular")) {
    stop("x not a correlation matrix")
  }

  x <- x + t(x)
  diag(x) <- rep(1, length(diag(x)))
  return(x)
}
