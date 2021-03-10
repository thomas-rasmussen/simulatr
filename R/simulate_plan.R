simulate_plan <- function(x, n_dataset, n_obs) {

    x <- validate_simulatr_plan(x)

    if (!is_integer(n_dataset) | n_dataset < 0) {
      stop("`n_dataset` is not a positive integer")
    }
    if (!is_integer(n_obs) | n_obs < 0) {
      stop("`n_obs` is not a positive integer")
    }

    models <- x[["models"]]
    dat <- data.table(NULL)


    # go through each element and simulate data. Can only handle
    # simulate_correlated_variables right now, but can hopefully easily be
    # expanded as new plan_xxx functions are implemented
    for (i in models) {
      if (i[["simulate_fct_name"]] == "simulate_correlated_variables") {
        i_par <- i[["simulate_fct_par"]]
        i_dat <- simulate_correlated_variables(
          n = n_obs * n_dataset,
          var_def = i_par[["var_def"]],
          corr_matrix = i_par[["corr_matrix"]]
        )
      }
      dat<- cbind(dat, i_dat)
    }

    # add dataset variable
    dat[, ("dataset") := rep(1:n_dataset, each = n_obs)]

    dat
}


