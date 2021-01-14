#' Nearest correlation matrix
#'
#' Finds the nearest correlation matrix given a symmetric matrix with numeric
#' values.
#'
#' Finding the nearest correlation matrix is done by iteratively alternating
#' between projecting \code{x} onto the set of positive semidefinite matrices
#' and the set of matrices with unit diagonal. This iterative process is
#' guarantied to converge to a matrix with unit diagonal that is also positive
#' semidefinite, ie a correlation matrix. See references for further details.
#'
#' Note that the return object should be tested to assert whether or not a
#' nearest correlation matrix was actually found, since it is not a guaranty
#' that the default maximum number of iterations is sufficient in all cases.
#'
#' For convenience, the input matrix can be given in lower/upper triangular form
#' by specifying \code{type = "triangular"}.
#'
#' @param x a matrix
#' @param type "full" or "triangular". See details.
#' @param verbose boolean. Should info for each iteration be printed to the log?
#' @param max_ite positive integer. Maximal number of iteration used to try to
#' find nearest correlation matrix.
#' @param conv_cri positive numeric value. Convergence criteria applied when
#' comparing infinite norms to determine convergence of algorithm.
#'
#' @return matrix
#' @export
#' @references
#' Higham, N.J. (1988). Computing a nearest symmetric positive semidefinite matrix.
#'
#' Higham. N.J. (2002). Computing the nearest correlation matrix - a problem from finance.
#'
#' Wicklin, R. (2013). Simulating data with SAS.
#'
#' @examples
#' x <- matrix(c(1, 0.3, 0.9, 0.3, 1, 0.9, 0.9, 0.9, 1), nrow = 3, ncol = 3)
#' nearest_corr_matrix(x)
nearest_corr_matrix <- function(x,
                                type = c("full", "triangular"),
                                verbose = FALSE,
                                max_ite = 100L,
                                conv_cri = 1e-8
) {

  # Input checks
  type <- match.arg(type)
  if (!is.matrix(x)) {
    stop("x not a matrix")
  }
  if (!nrow(x) == ncol(x)) {
    stop("x not a square matrix")
  }
  if(!is.numeric(x)) {
    stop("x must have numeric values")
  }
  if (type == "triangular") {
    # Check that the matrix is actually on either upper or lower triangular
    # form

    # Length of vector with upper/lower matrix entries is the same since matrix
    # is guarantied to be square at this point.
    low <- x[lower.tri(x)]
    up <- x[upper.tri(x)]
    len <- length(low)
    # If lower triangular zeroes, and upper is not, or vice versa, the matrix
    # is on triangular form.
    if (!(isTRUE(all.equal(low, rep(0, len))) != isTRUE(all.equal(up, rep(0, len))))){
      stop("type = \"triangular\", but matrix is not on triangular form")
    }
  }
  # Convert lower/upper triangular matrix to full matrix
  if (type == "triangular") {
    x <- x + t(x)
    diag(x) <- rep(1, length(diag(x)))
  }
  if (!isSymmetric(x)) {
    stop("x not symmetric")
  }
  if (!(identical(verbose, TRUE) | identical(verbose, FALSE))){
    stop("verbose not boolean")
  }
  if (!(is.numeric(max_ite) && is_integer(max_ite) && max_ite > 0)){
    stop("max_ite must be a positive integer")
  }
  if (!is.numeric(conv_cri) | conv_cri <= 0) {
    stop("conv_cri must be a positive numeric value")
  }

  # Initialize parameters
  x_old <- x
  y_old <- x
  ds <- 0
  max_delta <- 1
  ite <- 0

  while (ite <= max_ite & max_delta > conv_cri) {
    r <- y_old - ds
    x_new <- project_psd(r)
    ds <- x_new - r
    y_new <- project_unit_diag(x_new)

    # Calculate matrix infinity norms as measures
    # of how much matrices have changed
    dx <- max(rowSums(abs(x_new - x_old)))
    dy <- max(rowSums(abs(y_new - y_old)))
    dxy <- max(rowSums(abs(y_new - x_new)))
    max_delta <- max(dx, dy, dxy)

    # Update matrices and iteration counter
    ite <- ite + 1
    x_old <- x_new
    y_old <- y_new

    if (isTRUE(verbose)) {
      print(paste0("ite ", ite, ": max_delta = ", max_delta))
    }
  }

  return(x_new)
}
