test_that("invalid input triggers errors", {
  # x is not a matrix
  x <- "not a matrix"
  expect_error(project_psd(x), "x not a matrix")
  # x is not symmetric
  x <- matrix(c(1, 2))
  expect_error(project_psd(x), "x not symmetric")
  x <- matrix(c(1, 0.2, -0.2, 1), nrow = 2, ncol = 2)
  expect_error(project_psd(x), "x not symmetric")
  # matrix does not have numeric values. If non-hermitian complex matrix,
  # the matrix is not considered symmetric, and will throw an associated
  # error
  x <- matrix(complex(real = 1, imaginary = 1))
  expect_error(project_psd(x), "x not symmetric")
  x <- matrix(complex(real = 1, imaginary = 0))
  expect_error(project_psd(x), "x must have numeric values")
})

test_that("return object is a matrix", {
  x <- matrix(1)
  expect_equal(is.matrix(project_psd(x)), TRUE)
  x <- diag(c(0, 0))
  expect_equal(is.matrix(project_psd(x)), TRUE)
  x <- matrix(c(-0.5, 2, 2, 10), nrow = 2, ncol = 2)
  expect_equal(is.matrix(project_psd(x)), TRUE)
})

# Since a matrix is postive semidefinte if, and only, if its determinant is
# non-negative, we can assess positive semidefiniteness by calculating
# the determinant of the matrix.

test_that("positive semidefinte input matrix is returned unaltered", {
  x <- matrix(1)
  # det(x) = 1
  expect_equal(project_psd(x), x)
  x <- diag(c(1, 1))
  # det(x) = 1
  expect_equal(project_psd(x), x)
  x <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)
  # det(x) = 0.75
  expect_equal(project_psd(x), x)
})

test_that("return matrix is positive semidefinite", {
  x <- matrix(-1)
  expect_equal(det(project_psd(x)) >= 0, TRUE)
  x <- matrix(c(-1, -0.5, -0.5, -1), nrow = 2, ncol = 2)
  expect_equal(det(project_psd(x)) >= 0, TRUE)
})
