test_that("invalid input triggers errors", {
  # matrix is not a matrix
  matrix <- "not a matrix"
  expect_error(is_corr_matrix(matrix), "Input not a matrix")
  # matrix is not square
  matrix <- matrix(c(1, 2))
  expect_error(is_corr_matrix(matrix), "matrix is not square")
  # matrix does not have real values
  matrix <- matrix(complex(real = 1, imaginary = 1))
  expect_error(is_corr_matrix(matrix), "matrix must have real values")
})

test_that("matrix with non-unit diagnoal returns FALSE", {
  matrix <- matrix(2)
  expect_equal(is_corr_matrix(matrix), FALSE)
  matrix <- matrix(c(2, 0, 0, 1), nrow = 2, ncol = 2)
  expect_equal(is_corr_matrix(matrix), FALSE)
})

test_that("asymmetric matrix returns FALSE", {
  matrix <- matrix(c(1, 0.5, 0.6, 1), nrow = 2, ncol = 2)
  expect_equal(is_corr_matrix(matrix), FALSE)
})

test_that("non-positive semidefinite matrix returns FALSE", {
  matrix <- matrix(c(1, 2, 2, 1), nrow = 2, ncol = 2)
  # det(matrix) = -3
  expect_equal(is_corr_matrix(matrix), FALSE)
})

test_that("full correlation matrices returns TRUE", {
  matrix <- matrix(1)
  expect_equal(is_corr_matrix(matrix), TRUE)
  matrix <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  expect_equal(is_corr_matrix(matrix), TRUE)
  matrix <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)
  expect_equal(is_corr_matrix(matrix), TRUE)
})

test_that("correlation matrices on triangular form are handled properly", {
  # True triangular matrix
  matrix <- matrix(c(1, 0, 0, 0.5, 1, 0, 0.5, 0.5, 1), nrow = 3, ncol = 3)
  expect_equal(is_corr_matrix(matrix), FALSE)
  expect_equal(is_corr_matrix(matrix, type = "triangular"), TRUE)

  #  matrix falsely specified as triangular
  matrix <- matrix(1)
  expect_error(is_corr_matrix(matrix, type = "triangular"), "matrix is not on triangular form")
  matrix <- matrix(c(1, 0.5, 0, 0.5, 1, 0, 0.5, 0.5, 1), nrow = 3, ncol = 3)
  expect_error(is_corr_matrix(matrix, type = "triangular"), "matrix is not on triangular form")
})
