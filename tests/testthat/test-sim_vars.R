test_that("returns data.table", {
  set.seed(1)
  test1 <- sim_vars(
    n = 1L,
    var_spec = list(
      cont_var = list(fun = qnorm, par = list(mean = 5, sd = 10))
    )
  )
  expect_equal(class(test1)[1], "data.table")
})


# test1 <- sim_vars(
#   n = 1e3L,
#   var_spec = list(
#     cont_var = list(fun = qnorm, par = list(mean = 5, sd = 10)),
#     bin_var = list(fun = extraDistr::qbern, par = list(prob = 0.2)),
#     cat_var = list(fun = extraDistr::qcat, par = list(prob = c(0.2, 0.5, 0.3)))
#   ),
#   n_dataset = 2L,
#   dataset_var = "ds",
#   obs_var = "id",
#   seed = 2
# )

# print(object.size(test1), units = "Mb")
#
# cor(copy(test1)[, `:=`(ds = NULL, id = NULL)])
# test1[,
#   .(cont_var_mean = mean(cont_var),
#     cont_var_sd = sd(cont_var),
#     cat_var_pct_1 = sum(cat_var == 1) / length(cat_var),
#     cat_var_pct_2 = sum(cat_var == 2) / length(cat_var),
#     cat_var_pct_3 = sum(cat_var == 3) / length(cat_var)
#   ),
#   by = .(ds)
# ]


# List of variables to simulate. Each element of the list is the specification
# of a variable to be simulated. The name of the list element is the variable
# name. Each list element must be a list itself of length two, with a list element
# named "fun" specifying the inverse CDF (quantile) function for the distribution to be
# sampled from, eg qnorm for sampling from the normal distribution. The other list
# element must be named "par", and is a list of named parameters passed to the
# invervese CDF function.

