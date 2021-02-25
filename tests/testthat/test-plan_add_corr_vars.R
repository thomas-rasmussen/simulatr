
test_that("return simulatr_plan object", {
  test1 <- plan_add_corr_vars(
    x = plan_init(plan_size = 10L),
    var_def = list(var = list(qfun = "qnorm"))
  )
  expect_equal(class(test1), "simulatr_plan")

  test2 <- plan_add_corr_vars(
    x = test1,
    var_def = list(
      var2 = list(qfun = "qnorm", mean = 10, sd = 11),
      var3 = list(qfun = "qnorm", mean = -10, sd = 1)
    ),
    plan_label = "test2"
  )
  expect_equal(class(test2), "simulatr_plan")
})

# List of variables to simulate. Each element of the list is the specification
# of a variable to be simulated. The name of the list element is the variable
# name. Each list element must be a list itself of length two, with a list element
# named "fun" specifying the inverse CDF (quantile) function for the distribution to be
# sampled from, eg qnorm for sampling from the normal distribution. The other list
# element must be named "par", and is a list of named parameters passed to the
# invervese CDF function.

