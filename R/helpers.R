
# Helper internal parsing functions:
#' @noRd
interpolateHazCurve <- function(DF, POE_star_vec, ITe) {
  # Sort by POE ascending
  DF <- DF[order(POE)]
  minPOE <- min(DF$POE)
  maxPOE <- max(DF$POE)

  results <- numeric(length(POE_star_vec))

  for (i in seq_along(POE_star_vec)) {
    poe_star <- POE_star_vec[i]
    if (poe_star <= minPOE) {
      # clamp to min
      results[i] <- DF$Sa[1]
    } else if (poe_star >= maxPOE) {
      # clamp to max
      results[i] <- DF$Sa[.N]
    } else {
      idx_high <- which(DF$POE >= poe_star)[1]
      idx_low  <- idx_high - 1

      x1 <- DF$POE[idx_low]
      x2 <- DF$POE[idx_high]
      y1 <- DF$Sa[idx_low]
      y2 <- DF$Sa[idx_high]
      frac <- (poe_star - x1) / (x2 - x1)

      val_log   <- log(y1) + frac * (log(y2) - log(y1))
      results[i] <- exp(val_log)
    }
  }
  return(results)
}

#' @noRd
checkTargetPOE <- function(poe_target,
                           poe_imported,
                           investigation_time,
                           tolTR = 0) {
  # Convert the user-specified POEs into integer TR
  TR_target <- round(-investigation_time / log(1 - poe_target))

  # Convert the OQ data's POEs into unique integer TR
  TR_imported <- round(-investigation_time / log(1 - poe_imported))
  TR_imported <- unique(TR_imported)  # single step to ensure uniqueness

  # For each TR_target, check if there's a matching TR_imported (± tolTR)
  pass_check <- sapply(TR_target, function(trt) {
    any(abs(TR_imported - trt) <= tolTR)
  })

  if (all(pass_check)) {
    message(sprintf(
      "checkTargetPOE: All target TRs match OQ data (tolTR=%d).", tolTR
    ))
  } else {
    failTR <- TR_target[!pass_check]
    stop(sprintf(
      "checkTargetPOE: The following target TRs not found in OQ data (tolTR=%d): %s.\n",
      tolTR, paste(failTR, collapse=", ")
    ))
  }

  # Optionally return a small summary (invisibly)
  invisible(
    data.table::data.table(
      POE_target = poe_target,
      TR_target  = TR_target,
      pass_check = pass_check
    )
  )
}

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


