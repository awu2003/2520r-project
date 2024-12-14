`%add%` <- annotate(function(left, right) {
  # Example operation: Add the two inputs
  result <- left + right
  return(result)
}, list(vector_type("double", NA), vector_type("double", NA)), vector_type("double", 2))

c(1,2, 3) %add% c(1,2)


