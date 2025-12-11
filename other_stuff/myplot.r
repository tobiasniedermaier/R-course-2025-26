
##### Initial version:

myplot <- function(x=NULL, y=NULL, ...){
if(any(is.na(x))){
 warning("NA or NaN values detected in x variable.")
}

if(any(is.na(y))){
 warning("NA or NaN values detected in y variable.")
}
 plot(x, y, ...)
}

##### Improved version:

myplot <- function(x, y = NULL, ...) {
  # x needs to be specified
  if (missing(x)) {
    stop("Argument 'x' needs to be specified")
  }

  # check NA/NaN in x
  if (anyNA(x)) {
    warning("NA or NaN values detected in 'x'.")
  }

  # Only check y if provided at all
  if (!is.null(y)) {
    if (anyNA(y)) {
      warning("NA or NaN values detected in 'y'.")
    }

    # Enforce same length for x and y
    if (length(x) != length(y)) {
      stop("'x' and 'y' must have the same length.")
    }
  }

  # actual Plot
  plot(x, y, ...)
}
