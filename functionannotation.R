#
# annotates functions for typechecking inputs and outputs
#
annotate <- function(func, input_types, output_type) {
  function(...) {
    args <- list(...)
    # if dots in args
    if (has_dots_argument(func)) {
      dots_index <- which(input_types == "...")
      
      # check arguments before dots
      if (dots_index > 1) {
        for (i in 1:(dots_index - 1)) {
          type_check(args[[i]], input_types[i])
        }
      }
      
      # check dots arguments if type is specified after dots
      if (length(input_types) > dots_index) {
        dots_type <- input_types[dots_index + 1]
        
        if (dots_index > 1) {
          dots_args <- args[-(1:(dots_index - 1))]
        } else {
          dots_args <- args
        }
        
        # check each dots argument against the specified type
        for (arg in dots_args) {
          type_check(arg, dots_type)
        }
      }
    # no dots in args
    } else {
      for (i in 1:length(args)) {
        type_check(args[[i]], input_types[i])
      }
    }
    
    result <- do.call(func, args)
    
    # check output type
    if (is.null(output_type)) {
      if (!is.null(typeof(result))) {
        stop("Error: output is not NULL")
      }
    }
    if (length(output_type) > 1 || output_type != "any") {
      type_check(result, output_type)
    }
    
    return(result)
  }
}


# helper to determine if function uses dots operator
has_dots_argument <- function(func) {
  formal_args <- formals(func)
  return(any(names(formal_args) == "..."))
}
