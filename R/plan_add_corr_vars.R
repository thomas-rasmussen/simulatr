#' Simulate correlated variables
#'
#' Simulate variables with with provided correlations. Correlation are only
#' approximately if one or more discrete variables are simulated.
#'
#' TODO:
#' 1) Figure out where the inspiration for this approach comes from, so that
#' proper references can be provided. Does not seem like Austin did this in any
#' of his propensity score simulation studies?
#' 2) Expand details on induced correlations, eg that we are using pearsons
#' correlation coefficient. Look a the math and figure out why correlations
#' are preserved when transforming to other continuous distirbutions, but
#' correlations are watered down when transforming to discrete distributions.
#' Wgat happens if other correlation coefficients are used, eg spearman?
#'
#' @param x simulatr_plan object
#' @param var_def todo
#' @param corr_matrix todo
#' @param plan_label todo
#'
#' @return a simulatr_plan object
#' @export
#' @import data.table
#'
#' @examples 2 + 2
plan_add_corr_vars <- function(x,
                     var_def,
                     corr_matrix = NULL,
                     plan_label = NULL) {

  x <- validate_simulatr_plan(x)

  # If no correlation matrix specified set to identity matrix, ie make
  # variables uncorrelated
  if (is.null(corr_matrix)) {
    corr_matrix <- diag(length(var_def))
  }

  dat <- simulate_correlated_variables(
    n = attr(x, "plan_size"),
    var_def = var_def,
    corr_matrix = corr_matrix
  )

  # This needs to be genenralized to check if name is already used
  if (is.null(plan_label)) {
    plan_label <- "plan_add_corr_vars"
  }

  # Bind new variables to plan
  x[["data"]] <- cbind(x[["data"]], dat)

  # Add info to model list
  parms <- list(
    simulate_fct_name = "simulate_correlated_variables",
    simulate_fct_par = list(
      var_def = var_def,
      corr_matrix = corr_matrix
    ),
    simulate_vars = names(var_def),
    uses_vars = character(0)
  )

  x[["models"]] <- append(x[["models"]], list(..plan_label = parms))

  index <- grep("..plan_label", names(x[["models"]]), fixed = TRUE)
  names(x[["models"]])[index] <- plan_label

  return(x)
}
