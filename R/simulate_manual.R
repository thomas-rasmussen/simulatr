simulate_manual <- function(n,
                            ...) {

  def <- list(...)
  dat <- data.table()
  ..n <- n

  for (i in names(def)) {
    i_def <- def[[i]]
    dat <- dat[, (i) := eval(parse(text = i_def))]
  }

  dat
}
