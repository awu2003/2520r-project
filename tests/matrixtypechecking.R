# MATRIX TYPE CHECKING

X <- NULL
mat_double(X, 2, 3) <- matrix(c(1,2,3,4,5,6), 2, 3)
# fails because of dim mismatch
expect_stop(mat_double(X, 2, 3) <- matrix(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE), 3, 2))
# fails because of type mismatch
expect_stop(mat_double(X, 2, 3) <- matrix(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE), 2, 3))

Y <- NULL
mat_integer(Y, 3, 2) <- matrix(c(1L,2L,3L,4L,5L,6L), 3, 2)
Z <- NULL
mat_double(Z, 2, 2) <- matrix(c(1,2,3,4), 2, 2)

matrix_mult_typed_specific <- annotate(
  function(A,B) {
    return (A %*% B)
  },
  list(matrix_type("double", 2, 3), matrix_type("integer", 3, 2)),
  matrix_type("double", 2, 2)
)

matrix_mult_typed_specific(X, Y)
expect_stop(matrix_mult_typed_specific(X, Z)) # fails because Y has wrong dims


matrix_mult_typed_abstract <- annotate(
  function(A,B) {
    return (A %*% B)
  },
  list(matrix_type("integer", NA, 2), matrix_type("double", 2, NA)),
  matrix_type("double", NA, NA)
)

matrix_mult_typed_abstract(Y, Z)
expect_stop(matrix_mult_typed_abstract(X, Y)) # fails because both Y and Z have wrong dims


matrix_mult_typed <- annotate(
  function(A,B) {
    return (A %*% B)
  },
  list(matrix_type("double", NA, NA), matrix_type("double", NA, NA)),
  matrix_type("double", NA, NA)
)

matrix_mult_typed(Z, X)
expect_stop(matrix_mult_typed(X, Y)) # fails because Y is of integer type

# demonstration of dimension-flexible typechecking
# fails because existing object has length 2
expect_stop(mat_double(Z, NA, 3) <-  matrix(c(1,2,3,4,-1,-3), 2, 3))
mat_double(Z, 2, NA) <-  matrix(c(1,2,3,4,-1,-3), 2, 3)
mat_double(Z, NA, NA) <-  matrix(c(1,2,3,4,-1,-3,2,4,7), 3, 3)
# fials because input is integer, not double
expect_stop(mat_double(Z, NA, NA) <-  matrix(c(1L, 2L, -1L, -2L), 2, 2))