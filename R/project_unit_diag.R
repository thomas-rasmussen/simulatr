#' Project matrix onto set of matrices with unit diagonal.
#'
#' Projects a numeric square matrix onto the set of matrices with unit diagonal,
#' by simply replacing the diagonal elements with ones.
#'
#' @param x a square matrix
#'
#' @return matrix
#' @export
#'
#' @examples
#' x <- diag(c(0, 0))
#' project_unit_diag(x)
project_unit_diag <- function(x) {

  # Input checks
  if (!is.matrix(x)) {
    stop("x not a matrix")
  }
  if (nrow(x) != ncol(x)) {
    stop("x not a square matrix")
  }
  if(!is.numeric(x)) {
    stop("x must have numeric values")
  }

  diag(x) <- 1
  return(x)
}
