test_that("multiplication works", {
  plan1 <- plan_add_manual(
    x = plan_init(1e2L),
    var1 = "rnorm(n = n)",
    var2 = "rbinom(n = ..n, size = 1, prob = 0.5)"
  )
  plan2 <- plan_add_indicator(
    x = plan1,
    var_name = "exp",
    lp = c(var1 = 0.5, var2 = 0.5)
  )

  plan3 <- plan_outcome_bin(
    x = plan2,
    out_name = "out",
    exp_var = "exp",
    exp_effect = 1.5
  )
})
