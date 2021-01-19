test_that("invalid input triggers errors", {
  # x is not a matrix
  x <- "not a matrix"
  expect_error(is_corr_matrix(x), "x not a matrix")
  # x is not square
  x <- matrix(c(1, 2))
  expect_error(is_corr_matrix(x), "x not a square matrix")
  # matrix does not have real values
  x <- matrix(complex(real = 1, imaginary = 1))
  expect_error(is_corr_matrix(x), "x must have numeric values")
})

test_that("matrix with non-unit diagnoal returns FALSE", {
  x <- matrix(2)
  expect_equal(is_corr_matrix(x), FALSE)
  x <- matrix(c(2, 0, 0, 1), nrow = 2, ncol = 2)
  expect_equal(is_corr_matrix(x), FALSE)
})

test_that("asymmetric matrix returns FALSE", {
  x <- matrix(c(1, 0.5, 0.6, 1), nrow = 2, ncol = 2)
  expect_equal(is_corr_matrix(x), FALSE)
})

test_that("non-positive semidefinite matrix returns FALSE", {
  x <- matrix(c(1, 2, 2, 1), nrow = 2, ncol = 2)
  # det(x) = -3
  expect_equal(is_corr_matrix(x), FALSE)
})

test_that("full correlation matrices returns TRUE", {
  x <- matrix(1)
  expect_equal(is_corr_matrix(x), TRUE)
  x <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  expect_equal(is_corr_matrix(x), TRUE)
  x <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)
  expect_equal(is_corr_matrix(x), TRUE)
})

test_that("correlation matrices on triangular form are handled properly", {
  # True triangular matrix
  x <- matrix(c(1, 0, 0, 0.5, 1, 0, 0.5, 0.5, 1), nrow = 3, ncol = 3)
  expect_equal(is_corr_matrix(x), FALSE)
  expect_equal(is_corr_matrix(x, type = "triangular"), TRUE)

  #  x falsely specified as triangular
  x <- matrix(c(1, 0.5, 0, 0.5, 1, 0, 0.5, 0.5, 1), nrow = 3, ncol = 3)
  expect_error(is_corr_matrix(x, type = "triangular"), "matrix is not on triangular form")

  # diagonal correlation matrices
  x <- matrix(1)
  expect_error(is_corr_matrix(x), NA)
  expect_error(is_corr_matrix(x, type = "triangular"), NA)
  x <- diag(c(1, 1))
  expect_error(is_corr_matrix(x), NA)
  expect_error(is_corr_matrix(x, type = "triangular"), NA)
  })


