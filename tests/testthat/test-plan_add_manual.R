test_that("returns simulatr_plan object", {
  test1 <- plan_add_manual(x = plan_init())
  expect_equal(is_simulatr_plan(test1), TRUE)
})

test_that("works", {
  expect_error(
    plan_add_manual(
      x = plan_init(plan_size = 10),
      var1 = "1:..n",
      var2 = "var1 + 1",
      var3 = "rnorm(n = ..n)",
      var4 = "rnorm(n = n)"
    ),
    NA)
})
