#' Check if triangular matrix
#'
#' Checks if square matrix is a triangular matrix.
#'
#' A triangular matrix can be either a lower triangular matrix or an upper
#' triangular matrix. By default (\code{type = "any"}) the function tests
#' if the square matrix is a either lower or upper triangular. If
#' \code{type = "lower"} or \code{type = "upper"} is specified,
#' testing if the matrix is specifically lower or upper triangular is
#' possible.
#'
#' @param x square matrix
#' @param type String. Type of triangular matrix that is being tested.
#' See details.
#'
#' @return boolean
#' @export
#'
#' @examples
#' x <- matrix(c(1, 0, 1, 1), nrow = 2, ncol = 2)
#' is_triangular(x)
is_triangular <- function(x, type = c("any", "lower", "upper")) {
  # Input checks
  type <- match.arg(type)
  if (!is.matrix(x)) {
    stop("x not a matrix")
  }
  if (nrow(x) != ncol(x)) {
    stop("x not a square matrix")
  }
  if (!is.numeric(x)) {
    stop("x must have numeric values")
  }

  # If upper triangle is zeroes the matrix is a lower triangular matrix.
  lower <- isTRUE(all.equal(x[upper.tri(x)], rep(0, length(x[upper.tri(x)]))))

  # If lower triangle is zeroes the matrix is a upper triangular matrix.
  upper <- isTRUE(all.equal(x[lower.tri(x)], rep(0, length(x[lower.tri(x)]))))

  if (type == "any" & (lower | upper)) {
    return(TRUE)
  } else if (type == "lower" & lower) {
    return(TRUE)
  } else if (type == "upper" & upper) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
