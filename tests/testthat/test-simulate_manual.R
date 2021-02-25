test_that("returns data.table", {
  test1 <- simulate_manual(n = 10,
                           var = "1:n")
  test2 <- simulate_manual(n = 10)
  expect_equal(is.data.table(test1), TRUE)
  expect_equal(is.data.table(test2), TRUE)
})

test_that("basic use works as intended", {
  set.seed(1)
  test1 <- simulate_manual(n = 3,
                          var1 = "1:n",
                          var2 = "var1 + 1",
                          var3 = "floor(rnorm(n = n))")
  expect_equal(test1[["var1"]], c(1, 2, 3))
  expect_equal(test1[["var2"]], c(2, 3, 4))
  expect_equal(test1[["var3"]], c(-1, 0, -1))
})
