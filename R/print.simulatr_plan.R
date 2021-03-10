#' Title
#'
#' @param x object
#' @param ... eh
#'
#' @return print
#' @export
#'
#' @examples 2 + 2
print.simulatr_plan <- function(x, ...) {
  model_names <- names(x$models)
  cat(paste0("Simulation plan\n"))
  for (i in model_names) {
    sim_vars <- x$models[[i]]$simulate_vars
    cat(paste0(i, ":\n"))
    for (j in sim_vars) {
      cat(paste0("- ", j, "\n"))
    }
  }

}
