#' Project square matrix onto set of matrices with unit diagonal.
#'
#' Projects a square matrix onto the set of matrices with unit diagonal, by
#' simply replacing the diagonal elements with ones.
#'
#' @param matrix a square matrix
#'
#' @return matrix
#' @export
#'
#' @examples
#' matrix <- diag(c(0, 0))
#' project_unit_diag(matrix)
project_unit_diag <- function(matrix) {

  # Input checks
  if (!is.matrix(matrix)) {
    stop("Input not a matrix")
  }
  if (nrow(matrix) != ncol(matrix)) {
    stop("Matrix is not square")
  }
  if(!is.numeric(matrix)) {
    stop("Matrix must have real values")
  }

  diag(matrix) <- 1
  return(matrix)
}
