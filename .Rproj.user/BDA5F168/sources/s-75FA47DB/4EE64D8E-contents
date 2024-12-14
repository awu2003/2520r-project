# MATRIX TYPE CHECKING

X <- NULL
mat_double(X, 2, 3) <- matrix(c(1,2,3,4,5,6), 2, 3)
Y <- NULL
mat_integer(Y, 3, 2) <- matrix(c(1L,2L,3L,4L,5L,6L), 3, 2)
Z <- NULL
mat_double(Z, 2, 2) <- matrix(c(1,2,3,4), 2, 2)

matrix_mult_typed <- annotate(
  function(X,Y) {
    # return (X %*% Y)
    return(Z)
  },
  list(matrix_type("double", 2, 3), matrix_type("integer", 3, 2)),
  matrix_type("double", 2, 3)
)

matrix_mult_typed(X, Y)
