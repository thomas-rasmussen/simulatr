test_that("returns data.table", {
  test1 <- data.table(
    var = rnorm(1)
  )
  test2 <- simulate_indicator(
    x = test1,
    var_name = "var2",
    lp = c(var = 1)
  )
  expect_equal(is.data.table(test2), TRUE)
})

