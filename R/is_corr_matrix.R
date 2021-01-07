#' Tests if object is a correlation matrix
#'
#' Tests if an object is a valid correlation matrix, ie if the input object is
#' symmetric positive semidefinite matrix with unit diagonal.
#'
#' The function will terminate with an error if the input object is a not a
#' square matrix with real values. For convenience, the input matrix can also
#' be given in lower/upper triangular form by specifying
#' \code{type = "triangular"}.
#'
#' @param matrix square matrix with real values
#' @param type "full" or "triangular". See details.
#'
#' @return boolean
#' @export
#'
#' @examples
#' matrix <- diag(c(1, 1, 1))
#' is_corr_matrix(matrix)
#' matrix[upper.tri(matrix)] <- 0.5
#' is_corr_matrix(matrix)
#' is_corr_matrix(matrix, type = "triangular")
is_corr_matrix <- function(matrix, type = c("full", "triangular")) {

  # Input checks
  type <- match.arg(type)
  if (!is.matrix(matrix)) {
    stop("Input not a matrix")
  }
  if (!nrow(matrix) == ncol(matrix)) {
    stop("matrix is not square")
  }
  if (!is.numeric(matrix)) {
    stop("matrix must have real values")
  }
  if (type == "triangular") {
    # Check that the matrix is actually on either upper or lower triangular
    # form

    # Length of vector with upper/lower matrix entries is the same since matrix
    # is guarantied to be symmetric at this point.
    low <- matrix[lower.tri(matrix)]
    up <- matrix[upper.tri(matrix)]
    len <- length(low)
    # If lower triangular zeroes, and upper is not, or vice versa, the matrix
    # is on triangular form.
    if (!(isTRUE(all.equal(low, rep(0, len))) != isTRUE(all.equal(up, rep(0, len))))){
      stop("type = \"triangular\", but matrix is not on triangular form")
    }
  }

  # Check unit diagonal
  if (!isTRUE(all.equal(rep(1, length(diag(matrix))), diag(matrix)))) {
    return(FALSE)
  }

  # Convert lower/upper triangular matrix to full matrix
  if (type == "triangular") {
    matrix <- matrix + t(matrix)
    diag(matrix) <- rep(1, length(diag(matrix)))
  }

  # Check if symmetric
  if (!isSymmetric(matrix)) {
    return(FALSE)
  }

  # Check if matrix is positive semidefinite. This is equivalent to checking
  # if the determinant is larger than or equal to zero.
  if (!det(matrix) >= 0) {
    return(FALSE)
  }

  # If all checks have been passed we return the value TRUE
  return(TRUE)
}
