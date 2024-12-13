typed_add <- function(type_string) {
  annotate(function(...) {
    args <- list(...)
    return(Reduce(`+`, args))
  }, list("...", type_string), type_string)
}

typed_add_scalar <- function(type_string, vec_size) {
  annotate(
    function(vec, scalar) {
      return(vec + scalar)
    }, list(vector_type(type_string, vec_size), type_string), vector_type(type_string, vec_size)
  )
}

typed_multiply <- function(type_string) {
  annotate(function(...) {
    args <- list(...)
    return(Reduce(`*`, args))
  }, list("...", type_string), type_string)
}

typed_multiply_scalar <- function(type_string, vec_size) {
  annotate(
    function(vec, scalar) {
      return(vec * scalar)
    }, list(vector_type(type_string, vec_size), type_string), vector_type(type_string, vec_size)
  )
}

typed_subtract <- function(type_string) {
  annotate(function(a, b) {
    return(a - b)
  }, list("...", type_string), type_string)
}

typed_subtract_scalar <- function(type_string, vec_size) {
  annotate(
    function(vec, scalar) {
      return(vec - scalar)
    }, list(vector_type(type_string, vec_size), type_string), vector_type(type_string, vec_size)
  )
}

typed_divide <- function(type_string) {
  annotate(function(a, b) {
    return(a / b)
  }, list("...", type_string), type_string)
}

typed_divide_scalar <- function(type_string, vec_size) {
  annotate(
    function(vec, scalar) {
      return(vec / scalar)
    }, list(vector_type(type_string, vec_size), type_string), vector_type(type_string, vec_size)
  )
}