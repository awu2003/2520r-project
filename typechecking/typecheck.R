type_check <- function(value, expected_type) {
  # input validation
  if (missing(value) || is.null(value)) {
    stop("Error: 'value' is missing or NULL. Please provide a valid value to check.")
  }
  if (missing(expected_type) || is.null(expected_type)) {
    stop("Error: 'expected_type' is missing or NULL. Please provide a valid expected type.")
  }

  # store type to check
  if (is.list(expected_type)) {
    if (expected_type$composite == "vector") {
      type <- expected_type$type
      length <- expected_type$length
    }
    else if (expected_type$composite == "matrix") {
      type <- expected_type$type
      n_row <- expected_type$length[1]
      n_col <- expected_type$length[2]
      length <- n_row * n_col
    }
  }
  else {
    type <- expected_type
    length <- NULL
  }

  # check for vector list matching
  if (is.vector(value) && !is.scalar(value) && !is.list(expected_type)) {
    stop(
      "Error: 'value' is a vector, but 'expected_type' is not a list. Use a list to specify type and length for vectors."
    )
  }
  
  # check for matrix list matching
  if (is.matrix(value) && !is.list(expected_type)) {
    stop(
      "Error: 'value' is a matrix, but 'expected_type' is not a list. Use a list to specify type and dimensions for matrices."
    )
  }
  
  # check length if vector is expected
  if (is.vector(value) && !is.scalar(value) && !is.na(length) && length(value) != length) {
    stop(sprintf(
      "Error: Length mismatch. Expected length %d, but got %d.",
      length,
      length(value)
    ))
  }
  
  # check dims if matrix is expected
  if (is.matrix(value)) {
    if (!is.na(n_row) && nrow(value) != n_row) {
      stop(sprintf(
        "Error: Length mismatch. Expected %d rows, but got %d.",
        n_row,
        nrow(value)
      ))
    }
    if (!is.na(n_col) && ncol(value) != n_col) {
      stop(sprintf(
        "Error: Length mismatch. Expected %d cols, but got %d.",
        n_col,
        ncol(value)
      ))
    }
  }

  # check type
  if (is.matrix(value) || (is.vector(value) && !is.scalar(value))) { # composite case
    for (i in seq_along(value)) {
      if (!is_type(value[i], type)) {
        # change reference to "numeric" type
        if (class(value[i]) == "numeric") {
          arg_type <- "double"
        }
        else {
          arg_type <- class(value[i])
        }
        stop(sprintf(
          "Error: Type mismatch at index '%d'. Expected '%s', but got '%s'.",
          i,
          type,
          arg_type
        ))
      }
    }
  }
  else {
    if (!is_type(value, type)) {
      # change reference to "numeric" type
      if (class(value) == "numeric") {
        arg_type <- "double"
      }
      else {
        arg_type <- class(value)
      }
      stop(sprintf(
        "Error: Type mismatch. Expected '%s', but got '%s'.",
        type,
        arg_type
      ))
    }
  }

  return(TRUE)
}


# helper to check if object is a scalar
is.scalar <- function(x) {
  return(length(x) == 1)
}


# helper to check if item is of provided type, can be modified to accommodate additional types
is_type <- function(value, type) {
  switch(
    type,
    double = is.double(value),
    integer = is.integer(value),
    logical = is.logical(value),
    character = is.character(value),
    factor = is.factor(value),
    stop(paste("Unknown type:", type))
  )
}
