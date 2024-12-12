expect_stop <- function(expr) {
  tryCatch({
    eval(expr)
    stop("Expected an error, but the code did not stop.")
  }, error = function(e) {
    cat("Caught expected error:\n", e$message, "\n")
  })
}
