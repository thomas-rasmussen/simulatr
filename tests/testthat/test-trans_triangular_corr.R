test_that("invalid input triggers errors", {
  # x not a correlation matrix
  x <- matrix(0)
  expect_error(trans_triangular_corr(x), "x not a correlation matrix")
  # x not on triangular form
  x <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)
  expect_error(trans_triangular_corr(x), "x is not a triangular matrix")
})

test_that("triangular correlation matrix correctly transformed",{
  x <- matrix(c(1, 0, 0.5, 1), nrow = 2, ncol = 2)
  expect_equal(
    trans_triangular_corr(x),
    matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)
  )
  x <- matrix(c(1, 0.5, 0, 1), nrow = 2, ncol = 2)
  expect_equal(
    trans_triangular_corr(x),
    matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)
  )
})

test_that("diagnoal correlation matrices are returned unaltered",{
  x <- matrix(1)
  expect_equal(trans_triangular_corr(x), x)
  x <- diag(c(1, 1))
  expect_equal(trans_triangular_corr(x), x)
})
