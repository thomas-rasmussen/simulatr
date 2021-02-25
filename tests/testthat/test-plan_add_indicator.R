test_that("returns simulatr_plan object", {
  test1 <- plan_add_manual(x = plan_init(1), var = 1)
  test2 <- plan_add_indicator(
    x = test1,
    var_name = "var2",
    lp = c(var = 0.5)
  )

  expect_equal(is_simulatr_plan(test2), TRUE)
  expect_error(test2 <- validate_simulatr_plan(test2), NA)
})

test_that("dev tests", {

  test1 <- plan_add_corr_vars(
    x = plan_init(plan_size = 100),
    var_def = list(
      var1 = list(qfun = "qnorm"),
      var2 = list(qfun = "qbinom", size = 1, prob = 0.5 )
    )
  )
  test2 <- plan_add_indicator(
    x = test1,
    var_name = "var3",
    lp = c(var1 = 0.5, var2 = 0.5),
    target_prop = 0.2
  )

})
