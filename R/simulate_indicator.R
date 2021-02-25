simulate_indicator <- function(x,
                               var_name,
                               lp,
                               .beta0 = 0) {

  if (missing(x)) {
    stop("`x` must be specified")
  }
  if (!is.data.table(x)) {
    stop("`x` must be a data.table")
  }

  if (missing(var_name)) {
    stop("`var_name` must be specified")
  }
  if (!is.character(var_name)) {
    stop("`var_name` must be a character variable")
  }
  if (!is.character(var_name)) {
    stop("`var_name` must be a character variable")
  }

  if (!is.numeric(lp)) {
    stop("`lp` must be a numeric vector")
  }
  if (is.null(names(lp))) {
    stop("`lp` must be a named vector")
  }
  if ("" %in% names(lp)) {
    stop("`lp` has one or more unnamed elements")
  }

  lp_text <- paste0("-", .beta0)
  for (i in names(lp)) {
    i_var <- names(lp)[names(lp) == i]
    i_beta <- lp[names(lp) == i]
    lp_text <- paste0(lp_text, "-", i_beta, "*", i_var)
  }

  x[, (var_name) := rbinom(nrow(x), 1, 1 / (1 + exp((eval(parse(text = lp_text))))))]

  x
}
