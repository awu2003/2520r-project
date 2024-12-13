## MATH TESTS
typed_add_ints <- typed_add("integer")

res <- typed_add_ints(1L, 2L, 3L)
stopifnot(res == 6L)
res <- typed_add_ints(1L, 2L, 3L, 4L)
stopifnot(res == 10L)

typed_multiply_vec_doubles <-
  typed_multiply(vector_type('double', 3))

res <- typed_multiply_vec_doubles(c(1, 2, 3), c(2, 3, 4))
stopifnot(identical(res, c(2, 6, 12)))
# fails since vector inputs are of length 2, not 3
expect_stop(res <- typed_multiply_vec_doubles(c(1, 2), c(2, 3)))

typed_subtract_scalar_double <- typed_subtract_scalar("double", 4)

res <- typed_subtract_scalar_double(c(1, 2, 3, 4), 2)
stopifnot(identical(res, c(-1, 0, 1, 2)))
# fails since vector input is of length 2, not 4
expect_stop(res <- typed_subtract_scalar_double(c(1, 2), 3))
# fails since scalar input is a vector
expect_stop(res <-
              typed_subtract_scalar_double(c(1, 2, 3, 4), c(1, 2)))
