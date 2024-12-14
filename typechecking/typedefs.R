#
# scalar assignment functions
#

`double<-` <- function(x, value, env = parent.frame()) {
  # check type of value to assign
  type_check(value, 'double')
  
  # if object not in env, break
  name <- deparse(substitute(x))
  
  if (!exists(name, envir = env)) {
    assign(name, value, envir = env)
    message("Object declared. Initializing the value.")
  }
  else if (is.null(x)) {
    message("Object declared. Initializing the value.")
  }
  else {
    message("Object exists. Updating the value.")
  }
  
  existing_value <- get(name, envir = env)
  
  # check type of existing object
  if (typeof(existing_value) != typeof(value) &&
      !is.null(existing_value)) {
    stop(
      sprintf(
        "Error: Type mismatch. Object type is '%s', can not assign as double type.",
        typeof(existing_value)
      )
    )
  }
  
  # check if existing object is a vector
  if (length(existing_value) > 1) {
    stop(sprintf("Error: Object is a vector, cannot assign as a scalar"))
  }
  return(value)
}

`integer<-` <- function(x, value, env = parent.frame()) {
  # check type of value to assign
  type_check(value, 'integer')
  
  # if object not in env, break
  name <- deparse(substitute(x))
  
  if (!exists(name, envir = env)) {
    assign(name, value, envir = env)
    message("Object declared. Initializing the value.")
  }
  else if (is.null(x)) {
    message("Object declared. Initializing the value.")
  }
  else {
    message("Object exists. Updating the value.")
  }
  
  existing_value <- get(name, envir = env)
  
  # check type of existing object
  if (typeof(existing_value) != typeof(value) &&
      !is.null(existing_value)) {
    stop(
      sprintf(
        "Error: Type mismatch. Object type is '%s', can not assign as integer type.",
        typeof(existing_value)
      )
    )
  }
  
  # check if existing object is a vector
  if (length(existing_value) > 1) {
    stop(sprintf("Error: Object is a vector, cannot assign as a scalar"))
  }
  return(value)
}

`logical<-` <- function(x, value, env = parent.frame()) {
  # check type of value to assign
  type_check(value, 'logical')
  
  # if object not in env, break
  name <- deparse(substitute(x))
  
  if (!exists(name, envir = env)) {
    assign(name, value, envir = env)
    message("Object declared. Initializing the value.")
  }
  else if (is.null(x)) {
    message("Object declared. Initializing the value.")
  }
  else {
    message("Object exists. Updating the value.")
  }
  
  existing_value <- get(name, envir = env)
  
  # check type of existing object
  if (typeof(existing_value) != typeof(value) &&
      !is.null(existing_value)) {
    stop(
      sprintf(
        "Error: Type mismatch. Object type is '%s', can not assign as logical type.",
        typeof(existing_value)
      )
    )
  }
  
  # check if existing object is a vector
  if (length(existing_value) > 1) {
    stop(sprintf("Error: Object is a vector, cannot assign as a scalar"))
  }
  return(value)
}

`character<-` <- function(x, value, env = parent.frame()) {
  # check type of value to assign
  type_check(value, 'character')
  
  # if object not in env, break
  name <- deparse(substitute(x))
  
  if (!exists(name, envir = env)) {
    assign(name, value, envir = env)
    message("Object declared. Initializing the value.")
  }
  else if (is.null(x)) {
    message("Object declared. Initializing the value.")
  }
  else {
    message("Object exists. Updating the value.")
  }
  
  existing_value <- get(name, envir = env)
  
  # check type of existing object
  if (typeof(existing_value) != typeof(value) &&
      !is.null(existing_value)) {
    stop(
      sprintf(
        "Error: Type mismatch. Object type is '%s', can not assign as character type.",
        typeof(existing_value)
      )
    )
  }
  
  # check if existing object is a vector
  if (length(existing_value) > 1) {
    stop(sprintf("Error: Object is a vector, cannot assign as a scalar"))
  }
  return(value)
}

`factor<-` <- function(x, value, env = parent.frame()) {
  # check type of value to assign
  type_check(value, 'factor')
  
  # if object not in env, break
  name <- deparse(substitute(x))
  
  if (!exists(name, envir = env)) {
    assign(name, value, envir = env)
    message("Object declared. Initializing the value.")
  }
  else if (is.null(x)) {
    message("Object declared. Initializing the value.")
  }
  else {
    message("Object exists. Updating the value.")
  }
  
  existing_value <- get(name, envir = env)
  
  # check type of existing object
  if (typeof(existing_value) != typeof(value) &&
      !is.null(existing_value)) {
    stop(
      sprintf(
        "Error: Type mismatch. Object type is '%s', can not assign as factor type.",
        typeof(existing_value)
      )
    )
  }
  
  # check if existing object is a vector
  if (length(existing_value) > 1) {
    stop(sprintf("Error: Object is a vector, cannot assign as a scalar"))
  }
  return(value)
}

#
# vector assignment functions
#

`vec_double<-` <- function(x, length, value, env = parent.frame()) {
  # check type of value to assign
  type_check(value, vector_type("double", length))
  
  # if object not in env, break
  name <- deparse(substitute(x))
  existing_value <- NULL
  
  if (!exists(name, envir = env)) {
    assign(name, value, envir = env)
    message("Object declared. Initializing the value.")
    existing_value <- get(name, envir = env)
  }
  else {
    existing_value <- get(name, envir = env)
    
    if (is.null(x)) {
      message("Object declared. Initializing the value.")
    }
    else {
      check_length(existing_value, length)
      message("Object exists. Updating the value.")
    }
  }
  
  # check type of existing object
  if (typeof(existing_value) != typeof(value) &&
      !is.null(existing_value)) {
    stop(
      sprintf(
        "Error: Type mismatch. Object type is '%s', can not assign as double type.",
        typeof(existing_value)
      )
    )
  }
  
  return(value)
}

`vec_integer<-` <- function(x, length, value, env = parent.frame()) {
  # check type of value to assign
  type_check(value, vector_type("integer", length))
  
  # if object not in env, break
  name <- deparse(substitute(x))
  
  if (!exists(name, envir = env)) {
    stop("Error: object does not exist. Please initialize object first.")
  }
  
  existing_value <- get(name, envir = env)
  
  if (is.null(x)) {
    print("Object declared. Initializing the value.")
  }
  else {
    check_length(existing_value, length)
    print("Object exists. Updating the value.")
  }
  
  # check type of existing object
  if (typeof(existing_value) != typeof(value) &&
      !is.null(existing_value)) {
    stop(
      sprintf(
        "Error: Type mismatch. Object type is '%s', can not assign as integer type.",
        typeof(existing_value)
      )
    )
  }
  
  return(value)
}

`vec_logical<-` <- function(x, length, value, env = parent.frame()) {
  # check type of value to assign
  type_check(value, vector_type("logical", length))
  
  # if object not in env, break
  name <- deparse(substitute(x))
  
  if (!exists(name, envir = env)) {
    stop("Error: object does not exist. Please initialize object first.")
  }
  
  existing_value <- get(name, envir = env)
  
  if (is.null(x)) {
    print("Object declared. Initializing the value.")
  }
  else {
    check_length(existing_value, length)
    print("Object exists. Updating the value.")
  }
  
  # check type of existing object
  if (typeof(existing_value) != typeof(value) &&
      !is.null(existing_value)) {
    stop(
      sprintf(
        "Error: Type mismatch. Object type is '%s', can not assign as logical type.",
        typeof(existing_value)
      )
    )
  }
  
  # check length of existing object
  if (length(existing_value) != length) {
    stop(sprintf(
      "Error: Length mismatch. Expected length %d, but got %d.",
      length,
      length(existing_value)
    ))
  }
  
  return(value)
}

`vec_character<-` <- function(x, length, value, env = parent.frame()) {
  # check type of value to assign
  type_check(value, vector_type("character", length))
  
  # if object not in env, break
  name <- deparse(substitute(x))
  
  if (!exists(name, envir = env)) {
    stop("Error: object does not exist. Please initialize object first.")
  }
  
  existing_value <- get(name, envir = env)
  
  if (is.null(x)) {
    print("Object declared. Initializing the value.")
  }
  else {
    check_length(existing_value, length)
    print("Object exists. Updating the value.")
  }
  
  # check type of existing object
  if (typeof(existing_value) != typeof(value) &&
      !is.null(existing_value)) {
    stop(
      sprintf(
        "Error: Type mismatch. Object type is '%s', can not assign as character type.",
        typeof(existing_value)
      )
    )
  }
  
  return(value)
}

`vec_factor<-` <- function(x, length, value, env = parent.frame()) {
  # check type of value to assign
  type_check(value, vector_type("factor", length))
  
  # if object not in env, break
  name <- deparse(substitute(x))
  
  if (!exists(name, envir = env)) {
    stop("Error: object does not exist. Please initialize object first.")
  }
  
  existing_value <- get(name, envir = env)
  
  if (is.null(x)) {
    print("Object declared. Initializing the value.")
  }
  else {
    check_length(existing_value, length)
    print("Object exists. Updating the value.")
  }
  
  # check type of existing object
  if (typeof(existing_value) != typeof(value) &&
      !is.null(existing_value)) {
    stop(
      sprintf(
        "Error: Type mismatch. Object type is '%s', can not assign as factor type.",
        typeof(existing_value)
      )
    )
  }
  
  return(value)
}

#
# matrix assignment functions
#

`mat_double<-` <- function(x, length, width, value, env = parent.frame()) {
  # check type of value to assign
  type_check(value, matrix_type("double", length, width))
  
  # if object not in env, break
  name <- deparse(substitute(x))
  
  if (!exists(name, envir = env)) {
    stop("Error: object does not exist. Please initialize object first.")
  }
  
  existing_value <- get(name, envir = env)
  
  if (is.null(x)) {
    print("Object declared. Initializing the value.")
  }
  else {
    check_matrix_size(existing_value, length, width)
    print("Object exists. Updating the value.")
  }
  
  # check type of existing object
  if (typeof(existing_value) != typeof(value) &&
      !is.null(existing_value)) {
    stop(
      sprintf(
        "Error: Type mismatch. Object type is '%s', can not assign as double type.",
        typeof(existing_value)
      )
    )
  }
  
  return(value)
}

`mat_integer<-` <- function(x, length, width, value, env = parent.frame()) {
  # check type of value to assign
  type_check(value, matrix_type("integer", length, width))
  
  # if object not in env, break
  name <- deparse(substitute(x))
  
  if (!exists(name, envir = env)) {
    stop("Error: object does not exist. Please initialize object first.")
  }
  
  existing_value <- get(name, envir = env)
  
  if (is.null(x)) {
    print("Object declared. Initializing the value.")
  }
  else {
    check_matrix_size(existing_value, length, width)
    print("Object exists. Updating the value.")
  }
  
  # check type of existing object
  if (typeof(existing_value) != typeof(value) &&
      !is.null(existing_value)) {
    stop(
      sprintf(
        "Error: Type mismatch. Object type is '%s', can not assign as integer type.",
        typeof(existing_value)
      )
    )
  }
  
  return(value)
}

`mat_logical<-` <- function(x, length, width, value, env = parent.frame()) {
  # check type of value to assign
  type_check(value, matrix_type("logical", length, width))
  
  # if object not in env, break
  name <- deparse(substitute(x))
  
  if (!exists(name, envir = env)) {
    stop("Error: object does not exist. Please initialize object first.")
  }
  
  existing_value <- get(name, envir = env)
  
  if (is.null(x)) {
    print("Object declared. Initializing the value.")
  }
  else {
    check_matrix_size(existing_value, length, width)
    print("Object exists. Updating the value.")
  }
  
  # check type of existing object
  if (typeof(existing_value) != typeof(value) &&
      !is.null(existing_value)) {
    stop(
      sprintf(
        "Error: Type mismatch. Object type is '%s', can not assign as logical type.",
        typeof(existing_value)
      )
    )
  }
  
  return(value)
}

`mat_character<-` <- function(x, length, width, value, env = parent.frame()) {
  # check type of value to assign
  type_check(value, matrix_type("character", length, width))
  
  # if object not in env, break
  name <- deparse(substitute(x))
  
  if (!exists(name, envir = env)) {
    stop("Error: object does not exist. Please initialize object first.")
  }
  
  existing_value <- get(name, envir = env)
  
  if (is.null(x)) {
    print("Object declared. Initializing the value.")
  }
  else {
    check_matrix_size(existing_value, length, width)
    print("Object exists. Updating the value.")
  }
  
  # check type of existing object
  if (typeof(existing_value) != typeof(value) &&
      !is.null(existing_value)) {
    stop(
      sprintf(
        "Error: Type mismatch. Object type is '%s', can not assign as character type.",
        typeof(existing_value)
      )
    )
  }
  
  return(value)
}

`mat_factor<-` <- function(x, length, width, value, env = parent.frame()) {
  # check type of value to assign
  type_check(value, matrix_type("factor", length, width))
  
  # if object not in env, break
  name <- deparse(substitute(x))
  
  if (!exists(name, envir = env)) {
    stop("Error: object does not exist. Please initialize object first.")
  }
  
  existing_value <- get(name, envir = env)
  
  if (is.null(x)) {
    print("Object declared. Initializing the value.")
  }
  else {
    check_matrix_size(existing_value, length, width)
    print("Object exists. Updating the value.")
  }
  
  # check type of existing object
  if (typeof(existing_value) != typeof(value) &&
      !is.null(existing_value)) {
    stop(
      sprintf(
        "Error: Type mismatch. Object type is '%s', can not assign as factor type.",
        typeof(existing_value)
      )
    )
  }
  
  return(value)
}

# helper function to deal with length mismatches
check_length <- function(existing_value, length) {
  if (length(existing_value) != length) {
    stop(sprintf(
      "Error: Length mismatch. Object is of length %d, but input is length %d.",
      length(existing_value),
      length
    ))
  }
}

# helper function to deal with length mismatches
check_matrix_size <- function(existing_value, nrow, ncol) {
  if (nrow(existing_value) != nrow) {
    stop(sprintf(
      "Error: Length mismatch. Object has %d rows, but input has %d rows.",
      nrow(existing_value),
      nrow
    ))
  }
  
  if (ncol(existing_value) != ncol) {
    stop(sprintf(
      "Error: Length mismatch. Object has %d cols, but input has %d cols.",
      ncol(existing_value),
      ncol
    ))
  }
}

# helper function to generate vector types
vector_type <- function(type, length) {
  return(list(type = type, length = length, composite="vector"))
}

# helper function to generate matrix types
matrix_type <- function(type, length, width) {
  return(list(type = type, length = c(length, width), composite="matrix"))
}
