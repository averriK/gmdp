#' @noRd
#'
#'



.find <- function(V, X) {
  # Ensure V is sorted
  V <- sort(unique(V))
  X <- sort(unique(X))
  # Initialize the result vector
  results <- numeric(length(X))

  # Process each element in X
  for (i in seq_along(X)) {
    x <- X[i]

    # Check if x is exactly in V
    if (x %in% V) {
      results[i] <- x
    } else {
      # Find the closest indices before and after x
      idx_before <- max(which(V < x), na.rm = TRUE)
      idx_after <- min(which(V > x), na.rm = TRUE)

      # Handle edge cases when x is outside the range of V
      if (length(idx_before) == 0) { # x is less than all elements in V
        idx_before <- idx_after
      }
      if (length(idx_after) == 0) { # x is more than all elements in V
        idx_after <- idx_before
      }

      # Fetch the closest values
      # Choose the closer one or both if x is equally distant from V[idx_before] and V[idx_after]
      if (abs(V[idx_before] - x) < abs(V[idx_after] - x)) {
        results[i] <- V[idx_before]
      } else if (abs(V[idx_before] - x) > abs(V[idx_after] - x)) {
        results[i] <- V[idx_after]
      } else {
        # If both are equally close, return the average or any other logic you want
        results[i] <- mean(c(V[idx_before], V[idx_after]))
      }
    }
  }

  return(results)
}
