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
    type <- expected_type$type
    length <- expected_type$length
  } else {
    type <- expected_type
    length <- NULL
  }

  # check for list matching
  if (!is.scalar(value) && !is.list(expected_type)) {
    stop(
      "Error: 'value' is a vector, but 'expected_type' is not a list. Use a list to specify type and length for vectors."
    )
  }

  # check type
  if (!is.scalar(value)) { # vector case
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

  # check length if vector is expected
  if (!is.null(length) && length(value) != length) {
    stop(sprintf(
      "Error: Length mismatch. Expected length %d, but got %d.",
      length,
      length(value)
    ))
  }

  return(TRUE)
}


# helper to check something is a scalar
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
