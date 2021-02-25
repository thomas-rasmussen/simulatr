test_that("is_integer works", {
  expect_equal(is_integer(1), TRUE)
  expect_equal(is_integer(1L), TRUE)
  expect_equal(is_integer(1.0), TRUE)
  expect_equal(is_integer(-1), TRUE)
  expect_equal(is_integer(-1.5), FALSE)
  expect_equal(is_integer(FALSE), TRUE)
  expect_error(is_integer("1"), "non-numeric argument to mathematical function")
  expect_equal(is_integer(c(TRUE, -4, 2.0, 6.6)), c(TRUE, TRUE, TRUE, FALSE))
})

test_that("is_valid_variable_name works", {
  expect_equal(is_valid_variable_name("valid_name"), TRUE)
  expect_equal(is_valid_variable_name(var <- "var"), TRUE)
  expect_equal(is_valid_variable_name("_invalid"), FALSE)
  expect_equal(is_valid_variable_name(".valid"), TRUE)
  expect_equal(is_valid_variable_name("..valid"), TRUE)
  expect_equal(is_valid_variable_name("if"), FALSE)
  expect_equal(is_valid_variable_name("while"), FALSE)
  expect_equal(is_valid_variable_name("..."), FALSE)
  expect_equal(is_valid_variable_name("...", allow_reserved = FALSE), FALSE)
  expect_equal(is_valid_variable_name("...", allow_reserved = TRUE), TRUE)
  expect_equal(is_valid_variable_name(c("..1", "..2")), c(FALSE, FALSE))
  expect_equal(is_valid_variable_name(c("var", "var")), c(TRUE, TRUE))
  expect_equal(
    is_valid_variable_name(c("var", "var"), unique = TRUE), c(TRUE, FALSE)
  )
  expect_equal(
    is_valid_variable_name(c("valid", "_invalid", "while", "...")),
    c(TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("is_empty_dt works", {
  expect_equal(is_empty_dt(data.table()), TRUE)
  expect_error(is_empty_dt(list()), "`x` is not a data.table")
  expect_error(is_empty_dt(data.frame()), "`x` is not a data.table")
  x <- data.table(var = 1)
  no_rows <- x[0, ]
  expect_equal(is_empty_dt(data.table(x)), FALSE)
  expect_equal(is_empty_dt(data.table(no_rows)), FALSE)
})
