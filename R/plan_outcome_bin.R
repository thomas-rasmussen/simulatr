plan_outcome_bin <- function(x,
                             out_name,
                             exp_var,
                             exp_effect,
                             effect_estimator = c("risk_difference"),
                             effect_type = c("conditional", "marginal"),
                             plan_label = NULL) {

  #### Input checks ####

  x <- validate_simulatr_plan(x)

  if (missing(out_name)) {
    stop("`out_name` not  specified. Must be a character string")
  }
  if (!is.character(out_name)) {
    stop("`out_name` must be character vector")
  }
  if (length(out_name) != 1) {
    stop("`out_name` must have length 1")
  }
  if (out_name %in% names(x[["data"]])) {
    stop("`out_name` is already used as a variable name")
  }

  if (missing(exp_var)) {
    stop("`exp_var` is not specified.\nMust be a binary variable in the input plan")
  }
  if (!is.character(exp_var)) {
    stop("`exp_var` must be a character string")
  }
  if (length(exp_var) != 1) {
    stop("`exp_var` must have length 1")
  }
  if (!(exp_var %in% names(x[["data"]]))) {
    stop("`exp_var` is not a variable in the input plan")
  }
  if (!is.numeric(x[["data"]][[exp_var]])) {
    stop("`exp_var` must be a numeric variable")
  }
  if (!(all(unique(x[["data"]][[exp_var]]) %in% c(0, 1)))) {
    stop("`exp_var` can only take the values 0 and 1")
  }

  if (missing(exp_effect)) {
    stop("`exp_effect` is not specified. Must be a numeric value")
  }
  if (!is.numeric(exp_effect)) {
    stop("`exp_effect` must be a numeric value")
  }
  if (length(exp_effect) != 1) {
    stop("`exp_effect` must have length 1")
  }

  effect_estimator <- match.arg(effect_estimator)
  effect_type <- match.arg(effect_type)

  if (is.null(plan_label)) {
    plan_label <- "plan_outcome_bin"
  }



  #### do stuff ####






  #### Estimate induced proportion of observations with the outcome ####



  #### TODO ####

  # It is impossible to control % with outcome (through alpha0) and the treatment
  # effect (through beta) at the same time? Tweaking one will clearly
  # affect the other? The primary goal is to induce a disired treatment effect
  # will just have to make due with estimating the % with outcome and add it to
  # the plan model info?

  # Should there be a function for each estimator?

  #### return updated plan ####

  x
}
