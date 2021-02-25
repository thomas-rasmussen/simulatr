plan_add_manual <- function(x,
                            ...,
                            plan_label = character()) {

  x <- validate_simulatr_plan(x)
  ..n <- attr(x, "plan_size")

  dat <- simulate_manual(n = ..n, ...)

  # Bind new variables to plan
  x[["data"]] <- cbind(x[["data"]], dat)

  if (identical(plan_label, character())) {
    plan_label <- "simulate_manual"
  }

  # Add info to model list
  parms <- list(
    simulate_fct_name = "simulate_manual",
    simulate_fct_par = list(...),
    simulate_vars = names(list(...)),
    uses_vars = character(0)
  )

  x[["models"]] <- append(x[["models"]], list(..plan_label = parms))

  index <- grep("..plan_label", names(x[["models"]]), fixed = TRUE)
  names(x[["models"]])[index] <- plan_label

  x
}
