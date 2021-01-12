#' Project real symmetric matrix onto the set of positive semidefinite matrices.
#'
#' Projects a real symmetric matrix onto the set of positive semidefinite
#' matrices, by using eigendecomposition, ie X = QDQ*, where Q is the matrix of
#' eigenvectors and D is the diagonal matrix containing the eigenvalues. The
#' projection is done by replacing any negative eigenvalues in D by zero, and
#' then reassembling the matrix.
#'
#' @param x a real symmetric matrix
#'
#' @return a matrix
#' @export
#'
#' @examples
#' x <- diag(c(-1, 0))
#' project_psd(x)
project_psd <- function(x) {
  # Input checks
  if (!is.matrix(x)) {
    stop("x not a matrix")
  }
  if (!isSymmetric(x)) {
    stop("x not symmetric")
  }
  if(!is.numeric(x)) {
    stop("x must have numeric values")
  }

  # Find eigenvectors and eigenvalues
  matrix_decomp <- eigen(x, symmetric = TRUE)
  Q <- matrix_decomp$vectors
  # Construct diagonal matrix with eigenvalues on diagonal. nrow argument
  # is manually specified to make sure weird cases like x = -1 correctly
  # makes a 1x1 matrix with value -1, instead of trying to create a diagnoal
  # matrix with nrow = -1.
  D <- diag(matrix_decomp$values, nrow = length(matrix_decomp$values))
  # Replace negative eigenvalues and reassemble matrix
  D[D < 0] <- 0
  return((Q %*% D) %*% t(Q))
}
