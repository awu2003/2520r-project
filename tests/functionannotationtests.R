# FUNCTION DEMONSTRATIONS

check_if_odd <- annotate(function(x) {
  is_odd <- NULL
  logical(is_odd) <- (x %% 2 != 0)
  return(is_odd)
}, list("integer"), "logical")

check_if_odd_wrong <- annotate(function(x) {
  is_odd <- NULL
  logical(is_odd) <- x %% 2 # mistake
  return(is_odd)
}, list("integer"), "logical")

check_if_odd_also_wrong <- annotate(function(x) {
  is_odd <- x %% 2
  return(is_odd) # mistake
}, list("integer"), "logical")


res <- NULL
logical(res) <- check_if_odd(2L)
stopifnot(res == FALSE)

res_wrong <- NULL
# fails since check_if_odd_wrong has an internal typechecking failure
expect_stop(logical(res_wrong) <- check_if_odd_wrong(1L))

res_also_wrong <- NULL
# fails since output is of type double, not logical
expect_stop(logical(res_also_wrong) <- check_if_odd_also_wrong(1L))

greater_or_less <- annotate(function(a, b) {
  if (a > b) {
    return("a is larger")
  }
  else if (a < b) {
    return("b is larger")
  }
  else {
    return("a is equal to b")
  }
}, list("double", "double"), "character")

message <- NULL
character(message) <- greater_or_less(1, 2)
stopifnot(message == "b is larger")

return_second_arg <- annotate(function(vec) {
  return(vec[2])
}, list(vector_type("integer", 3)), "integer")

elem <- NULL
integer(elem) <- return_second_arg(c(1L, 2L, 3L))
stopifnot(elem == 2L)

# fails because argument is of length 2, not 3
expect_stop(integer(elem) <- return_second_arg(c(1L, 2L)))


# demonstration of annotations on an existing function in R
typed_rnorm <- annotate(
  \(n, mean, var) rnorm(n,mean,var),
  list("integer", "double", "double"),
  vector_type("double", NA)
)

a <- NULL
vec_double(a, NA) <- typed_rnorm(3L, 0, 1)
expect_stop(vec_double(a, NA) <- typed_rnorm(3, 0, 1)) # incorrect type for n

### LIMITATIONS

# R has a lot of strange side effects
# if you don't specify a return type, it just spits out whatever's on the last line
# ideally future work would enforce a NULL return type

print_side_effect <- annotate(function(message) {
  print(message)
}, list("character"), "character")

printed <- NULL
character(printed) <- print_side_effect("hello world!")
stopifnot(printed == "hello world!") # returns message