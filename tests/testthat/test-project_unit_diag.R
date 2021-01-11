test_that("invalid input triggers errors", {
  # x is not a matrix
  x <- "not a matrix"
  expect_error(project_unit_diag(x), "x not a matrix")
  # x is not square
  x <- matrix(c(1, 2))
  expect_error(project_unit_diag(x), "x not a square matrix")
  # x does not have real values
  x <- matrix(complex(real = 1, imaginary = 1))
  expect_error(project_unit_diag(x), "x must have numeric values")
})

test_that("return object is a matrix", {
  x <- matrix(1)
  expect_equal(class(project_unit_diag(x))[1], "matrix")
  x <- diag(c(0, 0))
  expect_equal(class(project_unit_diag(x))[1], "matrix")
})

test_that("projections are correct", {
  x <- matrix(0)
  expect_equal(project_unit_diag(x), matrix(1))
  x <- matrix(c(0, 0.5, -0.6, 2), nrow = 2, ncol = 2)
  expect_equal(project_unit_diag(x), matrix(c(1, 0.5, -0.6, 1), nrow = 2, ncol = 2))
})
