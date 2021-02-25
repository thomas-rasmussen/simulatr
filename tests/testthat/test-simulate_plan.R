test_that("return data.table", {
  test1 <- plan_add_corr_vars(
    plan_init(plan_size = 10),
    var_def = list(var = list(qfun = "qnorm"))
  )

  test2 <- simulate_plan(x = test1, n_dataset = 2, n_obs = 2)

  expect_equal(class(test2), c("data.table", "data.frame"))
})
