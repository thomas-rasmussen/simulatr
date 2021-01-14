test_that("invalid input triggers errors", {
  # x is not a matrix
  x <- "not a matrix"
  expect_error(is_corr_matrix(x), "x not a matrix")
  x <- 1
  expect_error(nearest_corr_matrix(x), "x not a matrix")
  # x is not square
  x <- matrix(c(1, 2))
  expect_error(nearest_corr_matrix(x), "x not a square matrix")
  # matrix does not have real values
  x <- matrix(complex(real = 1, imaginary = 1))
  expect_error(nearest_corr_matrix(x), "x must have numeric values")
  # x not symmetric
  x <- matrix(c(1, 0, 0.5, 1), nrow = 2, ncol = 2)
  expect_error(nearest_corr_matrix(x), "x not symmetric")
  # verbose not boolean
  x <- matrix(1)
  expect_error(nearest_corr_matrix(x, verbose = "true"), "verbose not boolean")
  # max_ite not positive integer
  x <- matrix(1)
  expect_error(
    nearest_corr_matrix(x, max_ite = "1"),
    "max_ite must be a positive integer"
  )
  expect_error(
    nearest_corr_matrix(x, max_ite = 0),
    "max_ite must be a positive integer"
  )
  expect_error(
    nearest_corr_matrix(x, max_ite = -1),
    "max_ite must be a positive integer"
  )
  expect_error(
    nearest_corr_matrix(x, max_ite = 0.5),
    "max_ite must be a positive integer"
  )
  # conv_cri not positive
  x <- matrix(1)
  expect_error(
    nearest_corr_matrix(x, conv_cri = "1"),
    "conv_cri must be a positive numeric value"
  )
  expect_error(
    nearest_corr_matrix(x, conv_cri = -1),
    "conv_cri must be a positive numeric value"
  )
  expect_error(
    nearest_corr_matrix(x, conv_cri = 0),
    "conv_cri must be a positive numeric value"
  )
})

test_that("return object is a correlation matrix", {
  x <- matrix(0)
  expect_equal(is_corr_matrix(nearest_corr_matrix(x)), TRUE)
  x <- matrix(c(0.5, 0.5, 0.5, 0.5), nrow = 2, ncol = 2)
  expect_equal(is_corr_matrix(nearest_corr_matrix(x)), TRUE)
  x <- matrix(c(1, 0.3, 0.9, 0.3, 1, 0.9, 0.9, 0.9, 1), nrow = 3, ncol = 3)
  expect_equal(is_corr_matrix(nearest_corr_matrix(x)), TRUE)
})

test_that("input correlation matrices are unaltered", {
  x <- matrix(1)
  expect_equal(nearest_corr_matrix(x), x)
  x <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)
})
