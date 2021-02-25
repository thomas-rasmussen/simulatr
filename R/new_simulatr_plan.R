new_simulatr_plan <- function(x = list(data = data.table(), models = list()),
                         plan_size = integer()) {
  stopifnot(is.list(x), is_integer(plan_size))
  structure(x, class = "simulatr_plan", plan_size = plan_size)
}
