validate_simulatr_plan <- function(x) {
  if (class(x) != "simulatr_plan") {
    stop(
    "`x` does not have class \"simulatr\"",
    call. = FALSE
    )
  }
  if (typeof(x) != "list") {
    stop(
      "`x` does not have typeof \"list\"",
      call. = FALSE
    )
  }
  if (length(x) != 2) {
    stop(
      "`x` does not have length 2",
      call. = FALSE
    )
  }
  if (names(x[1]) != "data") {
    stop(
      "`x[1]` is not named \"data\"",
      call. = FALSE
    )
  }
  if (names(x[2]) != "models") {
    stop(
      "`x[2]` is not named \"models\"",
      call. = FALSE
    )
  }
  if (class(x[[1]])[1] != "data.table") {
    stop(
      "`x[[1]]` does not have class \"data.table\"",
      call. = FALSE
    )
  }
  if (typeof(x[[1]]) != "list") {
    stop(
      "`x[[1]]` does not have typeof \"list\"",
      call. = FALSE
    )
  }
  if (class(x[[2]]) != "list") {
    stop(
      "`x[[2]]` does not have class \"list\"",
      call. = FALSE
    )
  }
  if (typeof(x[[2]]) != "list") {
    stop(
      "`x[[2]]` does not have typeof \"list\"",
      call. = FALSE
    )
  }

  # TODO
  # - variables simulated from "models" needs to be in "data"
  # - variables simulated in models[i], cannot be used to simulate variables in
  #   models[i-1].

  # - models needs to have a substructure that is the same
  #   - function_call: sim(ulate?)_xxx function used to simulate model[i]
  #     function_name
  #   - function_parameters: parameters used in sim(ulate)_xxx?
  #   - use_variables: list of variables used in function/parameters (impossible to parse from call?)
  #   - make_variables:list of variables that is simulated with model[i]
  #   -
  #   -
  x
}
