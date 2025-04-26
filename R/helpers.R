
##############################################################################
# Helper internal parsing functions:
##############################################################################
#' @noRd

.extractQuantileFromHeader <- function(line) {
  if (grepl("kind='mean'", line)) return("mean")
  mt <- regexpr("kind='quantile-([0-9\\.]+)'", line)
  if (mt>0) {
    val <- regmatches(line, mt)
    qval <- sub("kind='quantile-", "", val)
    qval <- sub("'", "", qval)
    return(suppressWarnings(as.numeric(qval)))
  }
  return(NA)
}
#' @noRd

.extractInvestigationTime <- function(line) {
  mt <- regexpr("investigation_time=([0-9\\.]+)", line)
  if (mt>0) {
    val <- regmatches(line, mt)
    num <- sub("investigation_time=", "", val)
    return(as.numeric(num))
  }
  return(NA_real_)
}

#' @noRd

.extractTnFromHeader <- function(line) {
  if (grepl("imt='PGA'", line)) return(0)
  if (grepl("imt='PGV'", line)) return(-1)
  mt <- regexpr("imt='SA\\(([0-9\\.]+)\\)'", line)
  if (mt>0) {
    val <- regmatches(line, mt)
    num <- sub("imt='SA\\(", "", val)
    num <- sub("\\)'", "", num)
    return(as.numeric(num))
  }
  return(NA_real_)
}

#' @noRd

.safeParseUHS <- function(col_labels) {
  n <- length(col_labels)
  POE <- numeric(n)
  Tn  <- numeric(n)
  POE[] <- NA_real_
  Tn[]  <- NA_real_

  for (i in seq_len(n)) {
    clb <- col_labels[i]
    parts <- strsplit(clb, "~", fixed=TRUE)[[1]]
    if (length(parts)<2) next
    prefix <- suppressWarnings(as.numeric(parts[1]))
    if (is.na(prefix) || prefix>=1) next
    suffix <- parts[2]
    tn_val <- NA_real_
    if (grepl("PGA", suffix, ignore.case=TRUE)) {
      tn_val <- 0
    } else {
      mt <- regexpr("SA\\(([0-9\\.]+)\\)", suffix)
      if (mt>0) {
        val2 <- regmatches(suffix, mt)
        val2 <- sub("^SA\\(", "", val2)
        val2 <- sub("\\)$", "", val2)
        tn_val <- suppressWarnings(as.numeric(val2))
      }
    }
    if (!is.na(tn_val)) {
      POE[i] <- prefix
      Tn[i]  <- tn_val
    }
  }
  list(POE, Tn)
}

#' @noRd

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


