# Helper functions

# Check if integer number
is_integer <- function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}

# Check if valid variable name. Modified version of function described at:
# https://www.r-bloggers.com/2011/07/testing-for-valid-variable-names/
is_valid_variable_name <- function(x, allow_reserved = FALSE, unique = FALSE)
{
  # Initialize return object
  ok <- rep.int(TRUE, length(x))

  # Check name not too long. This depends on the used version of R
  max_name_length <- if(getRversion() < "2.13.0") 256L else 10000L
  # Check if reserved variable i.e.an ellipsis or two dots then a number
  if(!allow_reserved)
  {
    ok[x == "..."] <- FALSE
    ok[grepl("^\\.{2}[[:digit:]]+$", x)] <- FALSE
  }
  # Use make.names() to check if valid name by checking if make.names()
  # changes the variable name (because it's not valid)
  ok[x != make.names(x, unique = unique)] <- FALSE
  ok
}

is_empty_dt <- function(x) {
  if (!is.data.table(x)) {
    stop("`x` is not a data.table")
  }
  if (ncol(x) == 0 & nrow(x) == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
