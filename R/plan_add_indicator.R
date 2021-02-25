

# Simulates an indicator variable and adds it to the input data set data.
# The indicator variable is simulated based on a specified
# logistic regression model that can include variables in the input data,
# specified as the linear predictor minus the intercept
# term. Using an iterative bisection approach, the intercept term in the model
# is set to induce the target proportion of observations with the indicator.

# Intended to be used to find parameters that can be used to simulate
# data where the specified associations have been induced, hence the name.

# Intended to be used to simulate an eg exposure or missingness indicator
# that is associated with values of other variables in the data,
# while controlling the proportion of observations with the indicator.

# the out object should be a list, where the first element is a data.table
# with the input data + the simulated indicator, and the second object should
# hold info on estimated parameters / models that can be used to apply the
# model to other data. This way the simulated data can be directly used or the estimated
# parameter can be used to simulate more data later.

# Interaction terms and functional forms can only be used if handled with
# plan_add-manual(). Maybe this functionality is good enough?

plan_add_indicator <- function(x,
                              var_name,
                              lp,
                              target_prop = NULL,
                              conv_cri = 1e-5,
                              max_ite = 20,
                              plan_label = NULL,
                              verbose = FALSE,
                              seed = Sys.time()) {

  #### Input checks ####

  if (missing(x)) {
    stop("`x` must be specified")
  }
  x <- validate_simulatr_plan(x)

  if (!is.null(target_prop)) {
    if (!is.numeric(target_prop)) {
      stop("`target_prop` is not numeric")
    }
    if (!length(target_prop) == 1) {
      stop("`target_prop` must have length 1")
    }
    if (!(target_prop > 0 & target_prop < 1)) {
      stop("`target_prop` must be larger than 0 and smaller than 1)")
    }
  }


  if (is.null(plan_label)) {
    plan_label <- "plan_add_indicator"
  }

  x_dat <- x[["data"]]

  #### Find initial induced proportion ####

  .beta0 <- 0

  # Reset stream to produce the same random numbers
  set.seed(seed)

  x_dat <- simulate_indicator(
    x = x_dat,
    var_name = var_name,
    lp = lp,
    .beta0 = .beta0
  )

  ind_prop <- mean(x_dat[[var_name]])

  beta0_min <- NA
  beta0_max <- NA

  # Initialize variables
  if (is.null(target_prop)) {
    beta0_min <- 0
    beta0_max <- 0
  } else if (ind_prop < target_prop) {
    beta0_min <- .beta0
  } else if (ind_prop > target_prop) {
    beta0_max <- .beta0
  } else if (ind_prop == target_prop) {
    beta0_min <- .beta0
    beta0_max <- .beta0
  }


  #### Find upper and lower bound for beta0 ####

  if (verbose) {
    print("Finding minimum and maximum beta0")
    print(paste0("Target proportion: ", target_prop))
  }

  cnt <- 0
  init_prop <- ind_prop

  while(is.na(beta0_min) | is.na(beta0_max)) {
    cnt <- cnt + 1

    # Update values
    if (is.na(beta0_max)) {
      .beta0 <- 2**cnt
    } else if (is.na(beta0_min)) {
      .beta0 <- -2**cnt
    }

    # Reset stream to produce the same random numbers
    set.seed(seed)

    x_dat <- simulate_indicator(
      x = x_dat,
      var_name = var_name,
      lp = lp,
      .beta0 = .beta0
    )

    ind_prop <- mean(x_dat[[var_name]])

    # Determine if min / max found
    if (init_prop < target_prop & ind_prop >= target_prop) {
      beta0_max <- .beta0
    }
    if (init_prop > target_prop & ind_prop <= target_prop) {
      beta0_min <-.beta0
    }

    if (verbose) {
      print(paste0("Iteration: ", cnt))
      print(paste0("beta0_min: ", beta0_min))
      print(paste0("beta0_min: ", beta0_max))
    }

    if (cnt > max_ite) {
      warning(paste0("Min/max beta0 bounds not found in ", max_ite, " iterations."))
      break
    }

  }

  #### Iterative bisection to find beta0 inducing target_prop ####

  cnt <- 0
  if (verbose) {
    print("Estimate beta0 inducing target proportion")
  }
  if (!is.null(target_prop)) {
    while(abs(ind_prop - target_prop) > conv_cri) {
      cnt <- cnt + 1

      # Reset stream to produce the same random numbers
      set.seed(seed)

      .beta0 <- beta0_min + (beta0_max - beta0_min) / 2
      x_dat <- simulate_indicator(
        x = x_dat,
        var_name = var_name,
        lp = lp,
        .beta0 = .beta0
      )

      ind_prop <- mean(x_dat[[var_name]])

      # Update min/max values
      if (ind_prop < target_prop) {
        beta0_min <- .beta0
      } else if (ind_prop > target_prop) {
        beta0_max <- .beta0
      } else if (ind_prop == target_prop) {
        beta0_min <- .beta0
        beta0_max <- .beta0
      }

      if (verbose) {
        print(paste0("ite ", cnt, ": beta0 = ", .beta0, ", induced proportion = ", ind_prop))
      }

      if (cnt > max_ite) {
        warning(paste0("beta0 inducing target proportion not found in ", max_ite, " iterations."))
        break
      }
    }
  }


  #### Make return object ####

  x[["data"]] <- x_dat

  model <- list(
    simulate_fct_name = "simulate_indicator",
    simulate_fct_par = list(
      lp = lp,
      .beta0 = .beta0,
      target_prop = target_prop,
      conv_cri = conv_cri,
      max_ite = max_ite,
      induced_prop = ind_prop,
      beta0_min = beta0_min,
      beta0_max = beta0_max
    ),
    simulate_vars = var_name,
    uses_vars = names(lp)
  )
  x[["models"]] <- append(x[["models"]], list(..model = model))

  index <- grep("..model", names(x[["models"]]), fixed = TRUE)
  names(x[["models"]])[index] <- plan_label

  x
}
