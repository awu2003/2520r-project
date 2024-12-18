# BASIC TYPE CHECKING

expect_stop(double(x) <-
              1) # fails since x has not been initialized yet

x <-
  NULL # should initialize all variables with NULL before assignment
double(x) <- 1
stopifnot(x == 1)
expect_stop(integer(x) <-
              2) # fails since 2 is a double, not an integer
expect_stop(integer(x) <-
              2L) # fails since x has been already initialized as a double

y <- NULL
vec_double(y, 3) <- c(1, 2, 3)
stopifnot(identical(y, c(1, 2, 3)))
expect_stop(vec_double(y, 3) <-
              c(1, 2)) # fails since vector is of length 2, not 3
expect_stop(vec_double(y, 2) <-
              c(1, 2)) # fails since y was already initialized with length 3

z <- NULL
logical(z) <- TRUE
stopifnot(z == TRUE)
expect_stop(integer(z) <-
              0L) # fails since z was already initialized as logical type

# demonstration of both scalar and vector factors
single_level <- NULL
character(single_level) <- "apple"
stopifnot(single_level == "apple")

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
expect_stop(vec_factor(fruit_factor_list, 3) <-
              factor(
                c("orange", "apple", "orange"),
                levels = c("apple", "banana", "orange"),
                labels = c("A", "B", "C")
              ))

# works with built in functions!
normal <- NULL
vec_normal <- NULL
double(normal) <- rnorm(1, 0, 1)
vec_double(vec_normal, 5) <- rnorm(5, 0, 1)
expect_stop(vec_double(vec_normal, 5) <-
              rnorm(3, 0, 1)) # fails because of length mismatch

# demonstration of ability to typecheck without enforcing lengths
v1 <- NULL
vec_logical(v1, NA) <- c(TRUE, TRUE, FALSE)
expect_stop(vec_double(v1, NA) <- c(1L, 2L))
expect_stop(vec_double(v1, NA) <- c(1, 2))
vec_logical(v1, NA) <- c(FALSE, TRUE)

# demonstration that you can typecheck individual components of vectors
v2 <- NULL
vec_character(v2) <- c("1", "2", "3")
character(v2[2]) <- "two"
expect_stop(double(v2[1]) <- 1) # fails because vector element is a character
stopifnot(identical(v2, c("1", "two", "3")))

### LIMITATIONS
# must use assignment wrapper in order to force typechecking
a <- NULL
integer(a) <- 100L
a <- "hello"
stopifnot(a == "hello")

a <- NULL
integer(a) <- 100L
expect_stop(integer(a) <- "hello")
stopifnot(a == 100L)