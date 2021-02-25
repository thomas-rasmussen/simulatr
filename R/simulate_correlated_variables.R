simulate_correlated_variables <- function(n,
                                          var_def,
                                          corr_matrix = NULL) {

  #### Input checks ####

  if (!(is_integer(n) && n > 0)) {
    stop("n is not a positive integer")
  }

  # Check that var_def has correct structure
  if (!is.list(var_def)) {
    stop("`var_def` not a list")
  }
  length_var_def <- length(var_def)
  if (length_var_def == 0) {
    stop("`var_def` is empty")
  }
  for (i in var_def) {
    # Implement more detailed checks here later
  }

  # If no correlation matrix specified set to identity matrix, ie make
  # variables uncorrelated
  if (is.null(corr_matrix)) {
    corr_matrix <- diag(length(var_def))
  }

  # Check that corr_matrix is a valid correlation matrix
  if (!is_corr_matrix(corr_matrix)) {
    stop("corr_matrix is not a valid correlation matrix")
  }


  # Draw from multivariate normal distribution with the specified
  # correlation matrix.
  dat <- data.table::as.data.table(mvtnorm::rmvnorm(
    n = n,
    mean = rep(0, length(var_def)),
    sigma = corr_matrix
  ))

  # Rename column names
  var_names <- names(var_def)
  default_names <- paste0("V", 1:length(var_def))
  data.table::setnames(dat, old = default_names, new = var_names)

  # Transform each column to uniform distribution using normal distribution cdf
  dat[, (var_names) := lapply(.SD, stats::pnorm, mean = 0, sd = 1), .SDcols = var_names]

  # Transform to different distributions using inverse distribution transformation.
  for (i in var_names) {
    i_names <- names(var_def[[i]])
    i_qfct <- eval(parse(text = var_def[[i]][["qfun"]]))
    i_par <- var_def[[i]][i_names[i_names != "qfun"]]
    dat[, (i) := do.call(i_qfct, c(list(dat[[i]]), i_par))]
  }

  return(dat)
}


