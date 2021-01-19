#' Tests if object is a correlation matrix
#'
#' Tests if an object is a valid correlation matrix, ie if the input object is
#' symmetric positive semidefinite matrix with unit diagonal.
#'
#' The function will terminate with an error if the input object is a not a
#' square matrix with numeric values. For convenience, the input matrix can also
#' be given in lower/upper triangular form by specifying
#' \code{type = "triangular"}.
#'
#' @param x square matrix with real values
#' @param type "full" or "triangular". See details.
#'
#' @return boolean
#' @export
#'
#' @examples
#' x <- diag(c(1, 1, 1))
#' is_corr_matrix(x)
#' x[upper.tri(x)] <- 0.5
#' is_corr_matrix(x)
#' is_corr_matrix(x, type = "triangular")
is_corr_matrix <- function(x, type = c("full", "triangular")) {

  # Input checks
  type <- match.arg(type)
  if (!is.matrix(x)) {
    stop("x not a matrix")
  }
  if (!nrow(x) == ncol(x)) {
    stop("x not a square matrix")
  }
  if (!is.numeric(x)) {
    stop("x must have numeric values")
  }
  if (type == "triangular") {
    # Check that the matrix is actually on either upper or lower triangular
    # form
    if (!is_triangular(x)){
      stop("type = \"triangular\", but matrix is not on triangular form")
    }
  }

  # Check unit diagonal
  if (!isTRUE(all.equal(rep(1, length(diag(x))), diag(x)))) {
    return(FALSE)
  }

  # Convert lower/upper triangular matrix to full matrix
  if (type == "triangular") {
    x <- x + t(x)
    diag(x) <- rep(1, length(diag(x)))
  }

  # Check if symmetric
  if (!isSymmetric(x)) {
    return(FALSE)
  }

  # Check if matrix is positive semidefinite. This is equivalent to checking
  # if the determinant is larger than or equal to zero.
  if (!det(x) >= 0) {
    return(FALSE)
  }

  # If all checks have been passed we return the value TRUE
  return(TRUE)
}
