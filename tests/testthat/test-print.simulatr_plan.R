test_that("manual tests", {
  test1 <- plan_add_corr_vars(
    x = plan_init(plan_size = 1e3L),
    var_def = list(
      cov1 = list(qfun = "qnorm"),
      cov2 = list(qfun = "qnorm"),
      cov3 = list(qfun = "qnorm")
    ),
    plan_label = "covariates"
  )

  test2 <- plan_add_indicator(
    x = test1,
    var_name = "exp",
    lp = c(cov1 = 0.5),
    plan_label = "exposure"
  )

  # print(test2)
})
