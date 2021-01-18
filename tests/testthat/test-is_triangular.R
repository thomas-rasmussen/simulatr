test_that("invalid input triggers errors", {
  # x is not a matrix
  x <- "not a matrix"
  expect_error(is_triangular(x), "x not a matrix")
  x <- 1
  expect_error(is_triangular(x), "x not a matrix")
  # x is not square
  x <- matrix(c(1, 2))
  expect_error(is_triangular(x), "x not a square matrix")
  # matrix does not have real values
  x <- matrix(complex(real = 1, imaginary = 1))
  expect_error(is_corr_matrix(x), "x must have numeric values")
})

test_that("non-triangular matrices return FALSE", {
  x <- matrix(c(1, 1, 1, 1), nrow = 2, ncol = 2)
  expect_equal(is_triangular(x), FALSE)
  expect_equal(is_triangular(x, type = "any"), FALSE)
  expect_equal(is_triangular(x, type = "lower"), FALSE)
  expect_equal(is_triangular(x, type = "upper"), FALSE)
})

test_that("type argument works as intended",  {
  # lower triangular matrix tests
  x <- matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2)
  expect_equal(is_triangular(x), TRUE)
  expect_equal(is_triangular(x, type = "any"), TRUE)
  expect_equal(is_triangular(x, type = "lower"), TRUE)
  expect_equal(is_triangular(x, type = "upper"), FALSE)
  # upper triangular matrix tests
  x <- matrix(c(1, 0, 1, 1), nrow = 2, ncol = 2)
  expect_equal(is_triangular(x), TRUE)
  expect_equal(is_triangular(x, type = "any"), TRUE)
  expect_equal(is_triangular(x, type = "lower"), FALSE)
  expect_equal(is_triangular(x, type = "upper"), TRUE)
  # diagonal matrix tests
  x <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  expect_equal(is_triangular(x), TRUE)
  expect_equal(is_triangular(x, type = "any"), TRUE)
  expect_equal(is_triangular(x, type = "lower"), TRUE)
  expect_equal(is_triangular(x, type = "upper"), TRUE)
})
