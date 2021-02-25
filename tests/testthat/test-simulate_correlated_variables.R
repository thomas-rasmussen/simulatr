test_that("return data.table", {
  test1 <- simulate_correlated_variables(
    n = 1, var_def = list(var = list(qfun = "qnorm"))
  )
  expect_equal(class(test1), c("data.table", "data.frame"))
})


