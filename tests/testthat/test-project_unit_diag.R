test_that("invalid input triggers errors", {
  # matrix is not a matrix
  matrix <- "not a matrix"
  expect_error(project_unit_diag(matrix), "Input not a matrix")
  # matrix is not square
  matrix <- matrix(c(1, 2))
  expect_error(project_unit_diag(matrix), "Matrix is not square")
  # matrix does not have real values
  matrix <- matrix(complex(real = 1, imaginary = 1))
  expect_error(project_unit_diag(matrix), "Matrix must have real values")
})

test_that("return object is a matrix", {
  matrix <- matrix(1)
  expect_equal(class(project_unit_diag(matrix))[1], "matrix")
  matrix <- diag(c(0, 0))
  expect_equal(class(project_unit_diag(matrix))[1], "matrix")
})

test_that("projections are correct", {
  matrix <- matrix(0)
  expect_equal(project_unit_diag(matrix), matrix(1))
  matrix <- matrix(c(0, 0.5, -0.6, 2), nrow = 2, ncol = 2)
  expect_equal(project_unit_diag(matrix), matrix(c(1, 0.5, -0.6, 1), nrow = 2, ncol = 2))
})
