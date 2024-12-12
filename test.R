source("typecheck.R")
source("typedefs.R")
source("functionannotation.R")
source("custommathfunctions.R")

# BASIC TYPE CHECKING

# double(x) <- 1 # fails since x has not been initialized yet
x <- NULL # should initialize all variables with NULL before assignment
double(x) <- 1
# integer(x) <- 2 # fails since 2 is a double, not an integer
# integer(x) <- 2L # fails since x has been already initialized as a double

y <- NULL
vec_double(y, 3) <- c(1, 2, 3)
# vec_double(y, 3) <- c(1, 2) # fails since vector is of length 2, not 3
# vec_double(y, 2) <- c(1, 2) # fails since y was already initialized with length 3

z <- NULL
logical(z) <- TRUE
# integer(z) <- 0L # fails since z was already initialized as logical type

# demonstration of both scalar and vector factors
single_level <- NULL
character(single_level) <- "apple"
fruit_factor <- NULL
factor(fruit_factor) <-
  factor(single_level, levels = c("apple", "banana", "orange"))

multiple_levels <- NULL
vec_character(multiple_levels, 4) <-
  c("apple", "banana", "apple", "orange")
fruit_factor_list <- NULL
vec_factor(fruit_factor_list, 4) <-
  factor(
    multiple_levels,
    levels = c("apple", "banana", "orange"),
    labels = c("A", "B", "C")
  )
# fails since fruit_factor_list was already initialized with size 4
# vec_factor(fruit_factor_list, 3) <-
#   factor(
#     c("orange", "apple", "orange"),
#     levels = c("apple", "banana", "orange"),
#     labels = c("A", "B", "C")
#   )

# works with built in functions!
normal <- NULL
vec_normal <- NULL
double(normal) <- rnorm(1,0,1)
vec_double(vec_normal, 5) <- rnorm(5,0,1)
# vec_double(vec_normal, 5) <- rnorm(3,0,1) # fails because of length mismatch

# type checking for functions
return_odd <- annotate(
  function(x) {
    is_odd <- NULL
    # print(is_odd)
    logical(is_odd) <- (x %% 2 == 1)
    return(is_odd)
  }, list("double"), "logical"
)

return_vector <- annotate(
  function(x, y) {
    # return_vec <- NULL
    vec_double(return_vec, 2) <- c(x, y)
    return(return_vec)
  }, list('double', 'double'), list(type = "double", length = 2)
)

res <- return_vector(1, 2)

hi <- FALSE
logical(hi) <- TRUE

logical(is_odd) <- 'hihi'

add_integer_vec <- typed_multiply_scalar("integer", 3)

add_integer_vec(c(1L,2L,111L), 11L)

