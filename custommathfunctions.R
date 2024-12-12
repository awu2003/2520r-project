typed_add <- function(type_string) {
  annotate(function(...) {
    args <- list(...)
    return(Reduce(`+`, args))
  }, c("...", type_string), type_string)
}

typed_add_scalar <- function(type_string, vec_size) {
  annotate(
    function(vec, scalar) {
      return(vec + scalar)
    }, c(vector_type(type_string, vec_size), vector_type(type_string, vec_size))
  )
}

typed_multiply <- function(type_string) {
  annotate(function(...) {
    args <- list(...)
    return(Reduce(`*`, args))
  }, c("...", type_string), type_string)
}

typed_subtract <- function(type_string) {
  annotate(function(a, b) {
    return(a - b)
  }, c("...", type_string), type_string)
}

typed_divide <- function(type_string) {
  annotate(function(a, b) {
    return(a / b)
  }, c("...", type_string), type_string)
}