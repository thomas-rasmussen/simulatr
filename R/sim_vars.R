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
#' @param n todo
#' @param var_spec todo
#' @param corr_matrix todo
#' @param dataset_var todo
#' @param n_dataset todo
#' @param obs_var todo
#'
#' @return a data.table
#' @export
#' @import data.table
#'
#' @examples 2 + 2
sim_vars <- function(n,
                     var_spec,
                     corr_matrix = diag(length(var_spec)),
                     dataset_var = NULL,
                     n_dataset = 1L,
                     obs_var = NULL) {

  var_names <- names(var_spec)

  # Input checks
  if (!is.integer(n)) {
    stop("n is not an integer")
  }

  if (!is.list(var_spec)) {
    stop("var_spec is not a list")
  }
  if (!all(nzchar(var_names))) {
    stop("var_spec has one or more unnamed elements")
  }
  for (i in var_names) {
    if (!is.list(var_spec[[i]])) {
      stop("One or more elements of var_spec is not a list")
    }
  }
  for (i in var_names) {
    if (!length(var_spec[[i]]) == 2) {
      stop("One or more elements of var_spec does not have length 2")
    }
  }
  for (i in var_names) {
    if (!all(names(var_spec[[i]]) %in% c("fun", "par"))) {
      stop('One or more list elements of var_spec has named elements that is not "fun" or "par"')
    }
  }
  for (i in var_names) {
    if (!is.list(var_spec[["cont_var"]][["par"]])) {
      stop('One or more "par" list elements is not a list')
    }
  }

  # Check that corr_matrix is a valid correlation matrix
  if (!isTRUE(all.equal(diag(corr_matrix), rep(1, length(var_spec)))) |
      !isSymmetric(corr_matrix) |
      det(corr_matrix) < 0) {
    stop("corr_matrix is not a valid correlation matrix")
  }

  if (!is.null(dataset_var) &
      (!is.character(dataset_var) | !length(dataset_var) == 1)) {
    stop("dataset_var is not a character vector of length 1")
  }

  if (!is.null(obs_var) &
      (!is.character(obs_var) | !length(obs_var) == 1)) {
    stop("obs_var is not a character vector of length 1")
  }

  if (!is.integer(n_dataset)) {
    stop("n_dataset is not an integer")
  }

  # Draw from multivariate normal distribution with the specified
  # correlation matrix.
  dat <- data.table::as.data.table(mvtnorm::rmvnorm(
    n = n * n_dataset,
    mean = rep(0, length(var_names)),
    sigma = corr_matrix
  ))

  # Add identification variables
  if (!is.null(dataset_var)) {
    dat[, (dataset_var) := rep(1:n_dataset, each = n)]
  }
  if (!is.null(obs_var)) {
    dat[, (obs_var) := rep.int(1:n, n_dataset)]
  }
  data.table::setcolorder(dat, c(dataset_var, obs_var))

  # Rename column names
  default_names <- paste0("V", 1:length(var_spec))
  data.table::setnames(dat, old = default_names, new = var_names)

  # Transform each column to uniform distribution using normal distribution cdf
  dat[, (var_names) := lapply(.SD, stats::pnorm, mean = 0, sd = 1), .SDcols = var_names]

  # Transform to different distributions using inverse distribution transformation.
  # If the specified distributions are continuous, the specified correlation
  # structure is preserved. If one or more discrete distributions are specified,
  # the specified correlations are not exactly preserved.
  for (i in var_names) {
    i_fct <- var_spec[[i]][["fun"]]
    i_par <- var_spec[[i]][["par"]]
    dat[, (i) := do.call(i_fct, c(list(dat[[i]]), i_par))]
  }

  return(dat)
}
